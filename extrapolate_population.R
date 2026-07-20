library(tidycensus); library(tidyverse); library(patchwork); library(scales)
library(Hmisc); library(tidyverse); library(dplyr); library(zoo); library(ipumsr)

# source("api_keys.R")
# 
# years_to_fetch = seq(2012, 2024, 1)
# 
# for (y in years_to_fetch) {
#   
#   if (y < 2019) {
#     grab_vars = c("RELP", "BDSP")
#   } else {
#     grab_vars = c("RELSHIPP", "BDSP")
#   }
#   
#   temp <- get_pums(
#     variables = grab_vars,
#     state = "DC", survey = "acs5", year = y, key = CENSUS_KEY,
#   ) %>% 
#     mutate(
#       YEAR = y,
#       SPORDER = as.character(SPORDER),
#       BDSP    = as.character(BDSP)
#     )
#   
#   if (y == min(years_to_fetch)) {
#     pums_all <- temp
#   } else {
#     pums_all <- bind_rows(pums_all, temp)
#   }
# }
# 
# saveRDS(pums_all, file = "data/all_pums_data.rds")



# DC historic population data

# extract_def <- define_extract_agg(
#   collection = "nhgis",
#   description = "DC total population, table A00, 1790-present",
#   time_series_tables = tst_spec("A00", geog_levels = "state")
# )
# 
# submitted <- submit_extract(extract_def, api_key = IPUMS_KEY)
# submitted <- wait_for_extract(submitted, api_key = IPUMS_KEY)
# files     <- download_extract(extract = submitted, download_dir = "data/", api_key = IPUMS_KEY)
# 
# dc_historical_pop <-
#   read_nhgis(files, verbose = F) %>%
#   filter(STATE == "District of Columbia")
# 
# # Extract labels and fallback to the original name if a column has no label
# new_names <- sapply(dc_historical_pop, function(x) {
#   lbl <- substr(attr(x, "label"), 1, 4)
#   if (is.null(lbl)) return(attributes(x)$name)
#   return(lbl)
# })
# 
# # Assign the extracted labels as the new column names
# names(dc_historical_pop) <- new_names
# 
# # remove unneeded vars
# dc_historical_pop <-
#   select(dc_historical_pop, -c("GIS ", "NHGI", "FIPS"), -NHGI) %>%
#   pivot_longer(cols=everything(), names_to = "Year", values_to = "Population") %>%
#   filter(!is.na(Population))
# 
# dc_historical_pop <- rbind(dc_historical_pop, list(2024, 681294))
# dc_historical_pop$Year <- as.numeric(dc_historical_pop$Year)
# 
# dc_historical_pop <- dc_historical_pop %>%
#   mutate(`Percent change` = (Population - lag(Population)) / lag(Population) * 100)
# 
# saveRDS(dc_historical_pop, file = "data/historical_population.rds")


dc_historical_pop <- readRDS("data/historical_population.rds")


a <- ggplot(dc_historical_pop, aes(x=Year, y=Population / 1000, group=1)) +
  geom_point() + geom_line() +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(y="", x="", title="DC population (thousands)")

b <- ggplot(dc_historical_pop, aes(x=Year, y=`Percent change`, group=1)) +
  geom_point() + geom_line() +
  geom_hline(aes(yintercept=0), color="grey") +
  theme_minimal() +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(y="", x="", 
       title="DC population, % growth between subsequent census snapshots", 
       caption="Final dot shows 2024 5-year ACS data. Source: IPUMS NHGIS, Census Bureau") +
  theme(plot.caption = element_text(hjust = 0))

a / b

ggsave(filename = "images/historic_pop.png", plot = a / b)

pa <- 
  readRDS("data/all_pums_data.rds") %>% 
  mutate(n_brs = as.numeric(as.character(BDSP))) %>%
  group_by(SERIALNO) %>%
  mutate(n_ppl = n()) %>%
  ungroup() %>%
  mutate(
    hh_head_ind = case_when(
      (RELSHIPP == "20") | (RELP == "0") ~ 1,
      !is.na(RELSHIPP) | !is.na(RELP)    ~ 0, 
      .default = NA
    )
    )

pop_wo_gq <-
  pa %>%
  filter(WGTP > 0) %>%
  group_by(YEAR) %>%
  dplyr::summarise(n_ppl = sum(PWGTP)) 

pop_w_gq <-
  pa %>%
  group_by(YEAR) %>%
  dplyr::summarise(n_ppl = sum(PWGTP)) 


beg_year = 2024
end_year = 2030
decline_factor = 0.001

# middle estimate: linear extrapolation
pop_proj     <- Hmisc::approxExtrap(x = pop_w_gq$YEAR, y = pop_w_gq$n_ppl, xout = seq(beg_year, end_year, 1))

# lower estimate: population declines at decline_factor % per year
for (i in seq(beg_year, end_year, 1)) {
  if (i == beg_year) {pop_proj_l = pop_w_gq$n_ppl[pop_w_gq$YEAR==beg_year]} else {
    last_val = pop_proj_l[length(pop_proj_l)]
    pop_proj_l <- c(pop_proj_l, last_val*(1-decline_factor))
  }
}

# high estimate: average annual population growth rate between 2012 and 2020
# get historic growth rates:
grs = c()
for (i in seq(2013, 2020, 1)) {
  growth_pct = (pop_w_gq$n_ppl[pop_w_gq$YEAR==i] - pop_w_gq$n_ppl[pop_w_gq$YEAR==i-1]) / pop_w_gq$n_ppl[pop_w_gq$YEAR==i-1]
  grs = c(grs, growth_pct)
}
mean_gr = mean(grs)
# create extrapolation:
for (i in seq(beg_year, end_year, 1)) {
  if (i == beg_year) {pop_proj_h = pop_w_gq$n_ppl[pop_w_gq$YEAR==beg_year]} else {
    last_val = pop_proj_h[length(pop_proj_h)]
    pop_proj_h <- c(pop_proj_h, last_val*(1+mean_gr))
  }
}

people_to_hh_ratio_2024 = 
  pa %>% filter(WGTP > 0 & YEAR==2024) %>% summarise(s=sum(PWGTP)) %>% pull(s) / 
  pa %>% filter(WGTP > 0 & hh_head_ind==1 & YEAR==2024) %>% summarise(s=sum(WGTP)) %>% pull(s)

jlg_goal = 72e3 * people_to_hh_ratio_2024

what_ifs_pops <-
  data.frame(
  YEAR = seq(beg_year, end_year, 1),
  low_proj = pop_proj_l[1:length(pop_proj_l)],
  pop_proj = pop_proj$y,
  hig_proj = pop_proj_h[1:length(pop_proj_h)],
  jlg_goal = zoo::na.approx(c(pop_proj$y[1], rep(NA, length(pop_proj$y)-2), pop_proj$y[1]+jlg_goal))
  )

pal <- c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")

lwd = .8

a <- 
  ggplot() + 
  geom_line(data = pop_w_gq, aes(x=YEAR, y=n_ppl / 1000), linewidth=lwd) +
  # geom_line(data=dc_historical_pop, aes(x = Year, y=Population)) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=low_proj / 1000), linetype = "dashed", color=pal[4], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=pop_proj / 1000), linetype = "dashed", color=pal[3], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=hig_proj / 1000), linetype = "dashed", color=pal[2], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=jlg_goal / 1000), linetype = "dashed", color=pal[1], linewidth=lwd) +
  theme_minimal() +
  labs(x="", y="Number of residents", title="DC population (thousands)",
       caption = paste0("Dashed lines, from the top down:\n",
                        "Janeese Lewis-George's 72,000 housing unit goal achieved by end of period (Applies 2024 ratio of housing units to population).\n",
                        "Population grows at average annual rate observed during 2012-2020 period.\n",
                        "Population increases linearly after 2023-2024.\n",
                        "Population declines at 0.1%/year.")) +
  scale_x_continuous(breaks = seq(2012, 2030, by = 2)) +
  scale_y_continuous(breaks = seq(600, 830, by = 20), labels = scales::label_comma()) +
  theme(
    # Left-align the text layout (0 = left, 0.5 = center, 1 = right)
    plot.caption = element_text(hjust = 0)
    # 
    # # Optional: Align to the edge of the entire plot instead of the panel grid
    # plot.caption.position = "plot" 
  )


lwd = .8
inset_plot <-
  ggplot() + 
  geom_line(data=dc_historical_pop, aes(x = Year, y=Population)) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=low_proj), linetype = "solid", color=pal[4], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=pop_proj), linetype = "solid", color=pal[3], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=hig_proj), linetype = "solid", color=pal[2], linewidth=lwd) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=jlg_goal), linetype = "solid", color=pal[1], linewidth=lwd) +
  geom_rect(aes(xmin = 2010, xmax = 2035, ymin = 600e3, ymax = 850e3), 
            fill = NA, color = "darkred", linewidth = 1) +
  theme_void() +
  labs(x="", y="") +
  scale_y_continuous(labels = scales::label_comma()) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#FCFBF4"),
    plot.margin = margin(0, 0, 0, 0, "pt")
  )

a + inset_element(inset_plot, left = 0.05, bottom = 0.6, right = 0.5, top = .95)

ggsave(filename = "images/projections.png", 
       plot = a + inset_element(inset_plot, left = 0.05, bottom = 0.6, right = 0.5, top = .95), 
       width = 9, height = 7
         )


br_counts <-
  pa %>%
  filter(hh_head_ind == 1)              %>%
  group_by(YEAR, n_brs)                 %>%
  summarise(weighted_count = sum(WGTP)) %>%
  ungroup()                             %>%
  group_by(YEAR)                        %>%
  mutate(n_brs = pmin(n_brs, 5))        %>%
  group_by(YEAR, n_brs) %>%
  summarise(weighted_count = sum(weighted_count), .groups = "drop") %>%
  ungroup() %>%
  filter(YEAR == beg_year) %>%
  mutate(proportion = weighted_count / sum(weighted_count))


low_final <- what_ifs_pops$low_proj[nrow(what_ifs_pops)]
mid_final <- what_ifs_pops$pop_proj[nrow(what_ifs_pops)]
hig_final <- what_ifs_pops$hig_proj[nrow(what_ifs_pops)]
jlg_final <- what_ifs_pops$jlg_goal[nrow(what_ifs_pops)]

actual_pop = what_ifs_pops$pop_proj[1]

br_counts[[paste0("low_est_", end_year)]] = round(low_final / actual_pop * br_counts$weighted_count)
br_counts[[paste0("mid_est_", end_year)]] = round(mid_final / actual_pop * br_counts$weighted_count)
br_counts[[paste0("hig_est_", end_year)]] = round(hig_final / actual_pop * br_counts$weighted_count)
br_counts[[paste0("jlg_est_", end_year)]] = round(jlg_final / actual_pop * br_counts$weighted_count)

br_counts_plot <-
  br_counts %>%
  select(-YEAR, -proportion) %>%
  pivot_longer(cols = -n_brs) %>%
  mutate(
    name = case_when(
      name == "weighted_count" ~ "2024 actual",
      name == paste0("low_est_", end_year)   ~ paste0(end_year, ", low est."),
      name == paste0("mid_est_", end_year)   ~ paste0(end_year, ", middle est."),
      name == paste0("hig_est_", end_year)   ~ paste0(end_year, ", high est."),
      name == paste0("jlg_est_", end_year)   ~ "Potential outcome if\nJLG housing goal achieved"
    )
  ) %>%
  mutate(
    name = factor(name, levels = c(paste0(end_year, ", low est."), 
                                   "2024 actual",  
                                   paste0(end_year, ", middle est."),  
                                   paste0(end_year, ", high est."),
                                   "Potential outcome if\nJLG housing goal achieved"
                                   )
                  )
  ) %>%
  ggplot() +
  geom_bar(aes(x=n_brs, y=value / 1000, fill=name), stat="identity", position="dodge") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(values =   rev(c("#440154FF", "#3B528BFF", "#21908CFF","darkgrey", "#5DC863FF"))) +
  labs(x="Number of bedrooms", y="Count (thousands)", fill="Scenario", 
       title="Estimated bedroom counts by scenario",
       caption="Chart based on data from occupied units.\nSource: 2024 5-year ACS") +
  theme(plot.caption = element_text(hjust = 0))

br_counts_plot

ggsave(filename = "images/projections_br_counts.png", 
       plot = br_counts_plot, 
       width = 7, height = 4
)
