library(tidycensus); library(tidyverse); library(patchwork); library(scales)



#' What this simple model is going to do:
#' takes the number of people in DC, grouped by household composition
#' lets the model user make different assumptions about the population growth rate broken out by household composition, with maybe some preset values based on actual historical rates
#' lets the user make different assumptions about what share of each household type will occupy each different kind of housing type
#' Then shows the user how many of each kind of housing type would be needed in the future, based on those assumptions
#' 
#' the model is more of a scenario runner, frankly

source("api_keys.R")


my_vars     <- c("TYPEHUGQ", "RELSHIPP", "AGEP", "TEN", "BLD", "BDSP")

pums <- get_pums(
  variables = my_vars,
  state = "DC", survey = "acs5", year = 2024, key = CENSUS_KEY,
  recode = T
)

years_to_fetch = c(2012, 2019, 2024)
for (y in years_to_fetch) {
  if (y <= 2012) {my_vars_all <- c("BDSP", "RELP")} else {
    my_vars_all <- c("BDSP", "RELSHIPP")
  }
  
  temp <- get_pums(
    variables = my_vars_all,
    state = "DC", survey = "acs5", year = y, key = CENSUS_KEY,
  ) %>% 
    mutate(
      YEAR = y,
      SPORDER = as.character(SPORDER),
      BDSP    = as.character(BDSP)
      )
  
  if (y == min(years_to_fetch)) {
    pums_all <- temp
  } else {
    pums_all <- bind_rows(pums_all, temp)
  }
}



#' RELSHIPP — relationship to householder. This is the key that tells you who's the householder (code 20) and how everyone else in the household relates to them (spouse, child, roommate, parent, etc.).
#' AGEP — age.
#' BLD — units in structure (detached single-family, attached row house, small/large multifamily, mobile home, etc.) — this is a household-level variable.
#' BDSP — number of bedrooms — also household-level.
#' TEN — tenure (own/rent) — household-level

vars <- pums_variables |>
  filter(year == 2024, survey == "acs5")

for (v in c("WGTP", "PWGTP", my_vars)) {
  print(vars |> filter(var_code == v) |> distinct(var_code, var_label, val_min, val_max, val_label))
}



# ____________________________________________
#'
#' CLEAN DATA
# ____________________________________________

# order the label factors by their underlying numeric code, not alphabetically
pums <- pums %>%
  mutate(
    TEN_label  = fct_reorder(TEN_label,  as.numeric(TEN)),
  )

pums <- pums %>%
  mutate(
    BLD_label = case_when(
      BLD == "01" ~ "Mobile home or trailer",
      BLD == "02" ~ "Detached single-family",
      BLD == "03" ~ "Attached single-family (row house)",
      BLD == "04" ~ "2 apartments",
      BLD == "05" ~ "3-4 apartments",
      BLD == "06" ~ "5-9 apartments",
      BLD == "07" ~ "10-19 apartments",
      BLD == "08" ~ "20-49 apartments",
      BLD == "09" ~ "50+ apartments",
      BLD == "10" ~ "Boat, RV, van, etc.",
      TRUE ~ NA_character_
    ) %>% fct_reorder(as.numeric(BLD)),
    BDSP_bin = case_when(
      BDSP == 0 ~ "Studio",
      BDSP == 1 ~ "1",
      BDSP == 2 ~ "2",
      BDSP == 3 ~ "3",
      BDSP >= 4 ~ "4+",
      TRUE ~ NA_character_
    ) %>% fct_relevel("Studio", "1", "2", "3", "4+")
  )

pums_hh <- pums %>% filter(RELSHIPP == "20")   # one row per household unit



# ____________________________________________
#'
#' CREATE SIMPLIFIED DATASETS
# ____________________________________________

df <- 
  pums %>%
  filter(TYPEHUGQ == 1) %>%  # filter out group quarters, which have HH weights of zero
  group_by(SERIALNO)    %>%  # group by household
  summarise(
    WGTP  = first(WGTP),
    n_ppl = n(),
    n_brs = first(BDSP)
  ) %>%
  ungroup()

df_xt <- 
  xtabs(WGTP ~ n_ppl + n_brs, data = df) %>% 
  data.frame(.) %>%
  mutate(
    n_ppl = as.numeric(as.character(n_ppl)),
    n_brs = as.numeric(as.character(n_brs))
    ) %>%
  mutate(
    n_ppl = if_else(n_ppl >=5, 5, n_ppl),
    n_brs = if_else(n_brs >=5, 5, n_brs)
    ) %>%
  group_by(n_ppl, n_brs) %>%
  summarise(Freq = sum(Freq), .groups = "drop") %>%
  ungroup()


df_all <- 
  pums_all %>%
  filter(WGTP > 0)          %>%  # filter out group quarters, which have HH weights of zero
  group_by(YEAR, SERIALNO)  %>%  # group by household
  summarise(
    WGTP  = first(WGTP),
    n_ppl = n(),
    n_brs = first(BDSP)
  ) %>%
  ungroup()


df_list <- list()
for (y in years_to_fetch) {
  df_list[[as.character(y)]] <- 
    xtabs(WGTP ~ n_ppl + n_brs, data = filter(df_all, YEAR==y)) %>% 
    data.frame(.) %>%
    mutate(
      n_ppl = as.numeric(as.character(n_ppl)),
      n_brs = as.numeric(as.character(n_brs))
    ) %>%
    group_by(n_ppl, n_brs) %>%
    summarise(Freq = sum(Freq), .groups = "drop") %>%
    ungroup()
}

# saveRDS(df_list, file = "data/my_list.rds")
# TODO: delete this line?

 
df_all_wide <- imap_dfr(df_list, ~ mutate(.x, year = .y)) %>%
  pivot_wider(id_cols = c(n_ppl, n_brs), names_from = year,
              values_from = Freq, names_prefix = "Freq_")

df_all_long <- imap_dfr(df_list, ~ mutate(.x, year = .y)) %>%
  mutate(
    n_ppl_label = if_else(n_ppl == 5, "5+", as.character(n_ppl)) %>%
      fct_relevel("1", "2", "3", "4", "5+"),
    n_brs_label = if_else(n_brs == 5, "5+", as.character(n_brs)) %>%
      fct_relevel("0", "1", "2", "3", "4", "5+"),
    year = factor(year, levels = c("2012", "2019", "2024"))
  )

df_growth <- df_all_wide %>%
  mutate(
    cagr_2012_2019 = (Freq_2019 / Freq_2012)^(1 / (2019 - 2012)) - 1,
    cagr_2019_2024 = (Freq_2024 / Freq_2019)^(1 / (2024 - 2019)) - 1
  )


# ____________________________________________
#'
#' MAKE 2024 PLOTS
# ____________________________________________

temp <-
  ggplot(df_xt %>%
         group_by(n_ppl) %>%
         summarise(Freq = sum(Freq, na.rm=T)) %>%
         ungroup(), 
       aes(x=factor(n_ppl), y=Freq)
       ) + 
  geom_bar(stat="identity", fill='steelblue') +
  scale_y_continuous(labels = comma) +
  scale_x_discrete(labels = c("5" = "5+")) +
  ylab("") + xlab("Household size") + ggtitle("Number of households by HH size, 2024 5-year ACS") +
  theme_minimal() 

ggsave("images/hh_size_hist.png", plot = temp, width = 7, height = 4, dpi = 300)



temp <-
  ggplot(df_xt, aes(y = factor(n_brs), x = factor(n_ppl), fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = comma(Freq)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue", labels = comma) +
  labs(y = "Bedrooms", x = "Household size", fill = "Number of\nHouseholds",
       title = "Number of DC households by HH size and dwelling bedroom count\n2024 5-year ACS") +
  scale_x_discrete(labels = c("5" = "5+")) +
  scale_y_discrete(labels = c("5" = "5+")) +
  guides(fill = "none") + # Removes the fill legend
  theme_minimal() +
  labs(caption = "Note: Each cell shows the number of households of that size, living in a dwelling with that number of bedrooms.")

ggsave("images/heatmap_counts.png", plot = temp, width = 7, height = 4, dpi = 300)


df_pct <- df_xt %>%
  group_by(n_ppl) %>%
  mutate(pct = Freq / sum(Freq)) %>%
  ungroup()

temp <-
  ggplot(df_pct, aes(y = factor(n_brs), x = factor(n_ppl), fill = pct)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(100*pct), "%"), size = 3)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(y = "Bedrooms", x = "Household size", fill = "Number of\nHouseholds",
       title = "Percent of households of each HH size in each size dwelling unit\n2024 5-year ACS") +
  scale_x_discrete(labels = c("5" = "5+")) +
  scale_y_discrete(labels = c("5" = "5+")) +
  guides(fill = "none", label="none", size="none") + # Removes the fill legend
  theme_minimal() +
  labs(caption = "Note: columns sum to ~100%. So a cell value of 46% in column 1 means that 46% of 1-person HH live in 1BR units.")

ggsave("images/heatmap_pcts.png", plot = temp, width = 7, height = 4, dpi = 300)




p_age <- ggplot(filter(pums, TYPEHUGQ == "1"), aes(x = AGEP, weight = PWGTP)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "steelblue") +
  labs(title = "Age", x = "", y = "")

hh_ten <- ggplot(pums_hh, 
                 aes(x = TEN_label, weight = WGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Owned with mortgage or loan (include home equity loans)" = "Owned with loan",
               "Occupied without payment of rent" = "Occ. w/o rent payment")
  ) +
  labs(title = "Tenure", x = NULL, y = "Weighted count")

hh_bld <- ggplot(pums_hh, aes(x = BLD_label, weight = WGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Units in structure", x = NULL, y = "Weighted count") +
  scale_x_discrete(
    labels = c("Attached single-family (row house)" = "Attached single-family")
  )

hh_bds <- ggplot(pums_hh, aes(x = BDSP_bin, weight = WGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Bedrooms", x = NULL, y = "Weighted count")


p_ten <- ggplot(filter(pums, TYPEHUGQ == "1"), 
                aes(x = TEN_label, weight = PWGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  scale_x_discrete(
    labels = c("Owned with mortgage or loan (include home equity loans)" = "Owned with loan",
               "Occupied without payment of rent" = "Occ. w/o rent payment")
  ) +
  labs(title = "Tenure", x = NULL, y = "Weighted count")


p_bld <- ggplot(filter(pums, TYPEHUGQ == "1"), aes(x = BLD_label, weight = PWGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Units in structure", x = NULL, y = "Weighted count") +
  scale_x_discrete(
    labels = c("Attached single-family (row house)" = "Attached single-family")
  )

p_bds <- ggplot(filter(pums, TYPEHUGQ == "1"), aes(x = BDSP_bin, weight = PWGTP)) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  labs(title = "Bedrooms", x = NULL, y = "Weighted count")

p_age <- p_age + scale_y_continuous(labels = comma)
p_ten <- p_ten + scale_y_continuous(labels = comma)
p_bld <- p_bld + scale_y_continuous(labels = comma)
p_bds <- p_bds + scale_y_continuous(labels = comma)
hh_ten <- hh_ten + scale_y_continuous(labels = comma)
hh_bld <- hh_bld + scale_y_continuous(labels = comma)
hh_bds <- hh_bds + scale_y_continuous(labels = comma)

desc_chart <- p_age / (p_ten + p_bld + p_bds) + 
  plot_annotation(caption = "Charts show people in households only (no group quarters residents).\nSource: 2024 5-year ACS PUMS.",
                  title = "Descriptive statistics, 2024 5-year ACS",
                  theme = theme(
                    plot.caption = element_text(hjust = 0)
                    )
                  ) 

ggsave("images/descriptive_chart.png", plot = desc_chart, width = 10, height = 9, dpi = 300)


# ____________________________________________
#'
#' MAKE CHANGE OVER TIME PLOTS
# ____________________________________________

group_high_vals <- function(df, n_ppl_cap, n_brs_cap) {
  df %>%
    mutate(
      n_ppl = if_else(n_ppl >= n_ppl_cap, n_ppl_cap, n_ppl),
      n_brs = if_else(n_brs >= n_brs_cap, n_brs_cap, n_brs)
    ) %>%
    group_by(year, n_ppl, n_brs) %>%
    summarise(
      Freq        = sum(Freq),
      n_ppl_label = ifelse(n_ppl >= n_ppl_cap, paste0(n_ppl_cap, "+"), as.character(n_ppl)),
      n_brs_label = ifelse(n_brs >= n_brs_cap, paste0(n_brs_cap, "+"), as.character(n_brs)),
      .groups = "drop") %>%
    ungroup() %>%
    distinct()
}


temp <-
  ggplot(group_high_vals(df_all_long, 5, 5), aes(x = year, y = Freq)) +
  geom_col(fill='steelblue') +
  facet_wrap(~ n_ppl_label, nrow = 1) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(name = "Bedrooms") +
  labs(x = "Household size, Year", y = "Number of households",
       title = "DC households by size, 2012–2024") +
  theme_minimal()

ggsave("images/all_years_hhsize_chart.png", plot = temp, width = 10, height = 6, dpi = 300)

df_all_long_pct <- group_high_vals(df_all_long, 5, 5) %>%
  group_by(year, n_ppl_label) %>%
  mutate(pct = Freq / sum(Freq)) %>%
  ungroup()

temp <-
  ggplot(df_all_long_pct, aes(x = year, y = pct, fill = n_brs_label)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = if_else(pct > 0.05, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5), size = 2.5, color = "white"
  ) +
  facet_wrap(~ n_ppl_label, nrow = 1) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(name = "Bedrooms") +
  labs(x = NULL, y = "Share of households",
       title = "Bedroom mix by household size, 2012–2024") +
  theme_minimal()

ggsave("images/all_years_hhsize_pct_chart.png", plot = temp, width = 10, height = 6, dpi = 300)


df_all_long_pct <- group_high_vals(df_all_long, 5, 5) %>%
  group_by(year, n_brs_label) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = Freq / sum(Freq)) %>%
  ungroup()

temp <-
  ggplot(df_all_long_pct, aes(x = year, y = pct, fill = n_brs_label)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = if_else(pct > 0.05, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5), size = 2.5, color = "white"
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(name = "Bedrooms") +
  labs(x = NULL, y = "Share of households",
       title = "Bedroom mix, 2012–2024") +
  theme_minimal()

ggsave("images/brs_pct_chart.png", plot = temp, width = 10, height = 6, dpi = 300)


df_all_long_pct <- group_high_vals(df_all_long, 5, 5) %>%
  group_by(year, n_ppl_label) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(pct = Freq / sum(Freq)) %>%
  ungroup()

temp <-
  ggplot(df_all_long_pct, aes(x = year, y = pct, fill = n_ppl_label)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = if_else(pct > 0.05, percent(pct, accuracy = 1), "")),
    position = position_stack(vjust = 0.5), size = 2.5, color = "white"
  ) +
  scale_y_continuous(labels = percent) +
  scale_fill_viridis_d(name = "HH size") +
  labs(x = NULL, y = "Share of households",
       title = "Household size, 2012–2024") +
  theme_minimal()

ggsave("images/hh_pct_chart.png", plot = temp, width = 10, height = 6, dpi = 300)








