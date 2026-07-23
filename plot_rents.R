library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(forcats)
library(stringr)
library(scales)
library(sf)
library(tigris)
library(ggrepel)

source("api_keys.R")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set_ipums_api_key(IPUMS_KEY, save = TRUE)  # once per machine




dc_msa_counties <- tribble(
  ~state, ~county,
  "DC", "001",
  "MD", "009", "MD", "017", "MD", "021", "MD", "031", "MD", "033",
  "VA", "013", "VA", "043", "VA", "047", "VA", "059", "VA", "061",
  "VA", "107", "VA", "153", "VA", "157", "VA", "177", "VA", "179",
  "VA", "187", "VA", "510", "VA", "600", "VA", "610", "VA", "630",
  "VA", "683", "VA", "685",
  "WV", "037"
)

my_vars <- c(
  studio           = "B25031_002",
  br1              = "B25031_003",
  br2              = "B25031_004",
  br3              = "B25031_005",
  br4              = "B25031_006",
  br5plus          = "B25031_007",
  median_hh_income = "B19013_001"
)

pull_counties <- function(yr) {
  dc_msa_counties %>%
    distinct(state) %>%
    pull(state) %>%
    map_dfr(function(st) {
      get_acs(
        geography = "county",
        state     = st,
        county    = dc_msa_counties %>% filter(state == st) %>% pull(county),
        variables = my_vars,
        survey    = "acs5", 
        year      = yr,
        key = CENSUS_KEY
      )
    }) %>%
    mutate(vintage = yr)
}

both <- bind_rows(pull_counties(2015), pull_counties(2024))

# wide, one row per county-vintage
both_wide <- both %>%
  select(GEOID, NAME, variable, estimate, vintage) %>%
  pivot_wider(names_from = variable, values_from = estimate)



msa_income <- map_dfr(c(2015, 2024), function(yr) {
  get_acs(
    geography = "cbsa",
    variables = c(median_hh_income = "B19013_001"),
    survey    = "acs5",
    year      = yr, 
    key = CENSUS_KEY
  ) %>%
    filter(str_detect(NAME, "Washington-Arlington-Alexandria")) %>%
    mutate(vintage = yr)
})


both_wide <-
  dplyr::left_join(
    x = both_wide, 
    y = msa_income %>% select(vintage, estimate) %>% rename(msa_hh_income = estimate), 
    by = "vintage")


br_cols <- c("studio", "br1", "br2", "br3", "br4", "br5plus")

df <- both_wide %>%
  mutate(across(all_of(br_cols),
                ~ .x / (msa_hh_income*.5 / 12),
                .names = "pct_{.col}"))


plot_dat <- df %>%
  select(NAME, vintage, starts_with("pct_")) %>%
  pivot_longer(starts_with("pct_"), names_to = "bedrooms",
               values_to = "pct", names_prefix = "pct_") %>%
  mutate(
    bedrooms = factor(bedrooms, levels = br_cols,
                      labels = c("Studio", "1BR", "2BR", "3BR", "4BR", "5BR+")),
    county   = str_remove(NAME, ",.*"),
    state    = str_extract(NAME, "(?<=, ).*"),
    state    = if_else(state == "District of Columbia", "DC",
                       state.abb[match(state, state.name)])
  ) %>%
  pivot_wider(names_from = vintage, values_from = pct, names_prefix = "y") %>%
  mutate(county_label = paste0(county, " (", state, ")"))

county_order <- plot_dat %>%
  filter(bedrooms == "2BR") %>%
  distinct(county_label, y2024) %>%
  arrange(y2024) %>%
  pull(county_label)

plot_dat <- plot_dat %>%
  mutate(county_label = factor(county_label, levels = county_order))

dc_y <- which(county_order == "District of Columbia (DC)")

a <- ggplot(plot_dat, aes(y = county_label)) +
  annotate("rect", ymin = dc_y - 0.5, ymax = dc_y + 0.5,
           xmin = -Inf, xmax = Inf, fill = "grey90") +
  geom_segment(aes(x = y2015, xend = y2024, yend = county_label), color = "grey70") +
  geom_point(aes(x = y2015), color = "steelblue", size = 1.5) +
  geom_point(aes(x = y2024), color = "firebrick", size = 1.5) +
  geom_vline(xintercept = 0.30, linetype = "dashed", color = "grey40") +
  facet_wrap(~ bedrooms, nrow = 1) +
  scale_x_continuous(labels = percent) +
  labs(x = "Rent as % of MSA monthly median income", y = NULL,
       title = "Rent burden relative to 50% MSA median income, 2011–2015 vs 2020–2024",
       caption = "Source: 5-year American Community Survey"
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(0, "pt")) +
  theme(plot.caption = element_text(hjust = 0))

a 
ggsave(filename = "images/county_rent_burden.png", plot = a, width = 14, height = 9)










county_order <- plot_dat %>%
  filter(bedrooms == "2BR") %>%
  mutate(delta = y2024 - y2015) %>%
  arrange(delta) %>%
  pull(county)

b <- plot_dat %>%
  mutate(delta  = y2024 - y2015,
         county = factor(county, levels = county_order)) %>%
  filter(!is.na(delta)) %>%
  ggplot(aes(x = delta, y = county, fill = delta > 0)) +
  geom_col() +
  facet_wrap(~ bedrooms, nrow = 1) +
  scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c("steelblue", "firebrick"), guide = "none") +
  theme_minimal() +
  labs(caption = "Source: 5-year American Community Survey", y="", x="", 
       title="Percent change in rent burden relative to 50% MSA median income, 2011–2015 vs 2020–2024") +
  theme(plot.caption = element_text(hjust = 0))

b

ggsave(filename = "images/county_rent_burden_change.png", plot = b, width = 14, height = 9)




# --- burden metric: 2BR rent as % of 50% of MSA median monthly income ---
target_geoids <- dc_msa_counties %>%
  mutate(GEOID = paste0(
    case_when(state == "DC" ~ "11", state == "MD" ~ "24",
              state == "VA" ~ "51", state == "WV" ~ "54"),
    county
  )) %>%
  pull(GEOID)

county_geo <- dc_msa_counties %>%
  distinct(state) %>% pull(state) %>%
  map_dfr(~ counties(state = .x, cb = TRUE, year = 2024)) %>%
  filter(GEOID %in% target_geoids) %>%
  select(GEOID, geometry) %>%
  st_transform(5070)


burden <- df %>%
  mutate(burden = br2 / (0.5 * msa_hh_income / 12)) %>%
  pivot_wider(id_cols = GEOID, names_from = vintage,
              values_from = burden, names_prefix = "b") %>%
  mutate(delta_burden = b2024 - b2015)   # change in percentage points

map_dat <- county_geo %>%
  st_transform(3857) %>%
  left_join(burden, by = "GEOID") %>%
  mutate(centroid = st_centroid(geometry))

coords <- st_coordinates(map_dat$centroid)
map_dat$cx <- coords[, 1]
map_dat$cy <- coords[, 2]


county_names <- df %>% distinct(GEOID, NAME) %>%
  mutate(short_name = str_remove(NAME, ",.*")) %>%
  mutate(short_name = str_remove(short_name, " County")) %>%
  mutate(short_name = str_remove(short_name, " city"))


# --- arrows scaled to the burden change ---
arrow_scale <- 250000   # meters per 1.0 (=100pp) change — tune after first render
label_drop  <- 3000     # meters below centroid for the text label

map_dat <- map_dat %>% left_join(county_names, by = "GEOID")
map_dat <- map_dat %>%
  mutate(
    angle   = if_else(delta_burden >= 0, pi / 2, -pi / 2),
    radius  = abs(delta_burden) * arrow_scale,
    label_y = if_else(delta_burden >= 0, cy - label_drop, cy + label_drop)
  ) %>%
  mutate(short_name = ifelse(is.na(angle), NA, short_name)) %>%
  filter(NAME != "Spotsylvania County, Virginia") %>%
  mutate(label_y = ifelse(NAME == "Fairfax County, Virginia", label_y - 5000, label_y),
         label_y = ifelse(NAME == "Fairfax County, Virginia", label_y - 5000, label_y)) %>%
  mutate(cy = ifelse(NAME == "Fairfax County, Virginia", cy - 5000, cy),
         cy = ifelse(NAME == "Fairfax County, Virginia", cy - 5000, cy)) %>%
  # mutate(label_y = ifelse(NAME == "Alexandria city, Virginia", label_y - 3000, label_y),
  #        label_y = ifelse(NAME == "Alexandria city, Virginia", label_y - 3000, label_y)) %>%
  mutate(cy = ifelse(NAME == "Arlington County, Virginia", cy + 3000, cy),
         cy = ifelse(NAME == "Arlington County, Virginia", cy + 3000, cy)) %>%
    mutate(label_y = ifelse(NAME == "Arlington County, Virginia", label_y + 3000, label_y),
         label_y = ifelse(NAME == "Arlington County, Virginia", label_y + 3000, label_y)) %>%
  mutate(short_name = ifelse(short_name == "District of Columbia", "DC", short_name))

c <-
  ggplot() +
  geom_sf(data = map_dat, fill = "grey80", color = "white") +
  geom_spoke(data = map_dat,
             aes(x = cx, y = cy, angle = angle, radius = radius, color = delta_burden),
             arrow = arrow(length = unit(0.12, "inches")), linewidth = 1) +
  geom_text(data = map_dat,
            aes(  x = cx
                , y = label_y
                , label = paste0(short_name, "\n", percent(delta_burden, accuracy = 0.1))
                ),
            size = 2.3) +
  scale_color_gradient2(low = "steelblue", mid = "grey80", high = "firebrick",
                        midpoint = 0, labels = percent, name = "Change in rent burden") +
  labs(title = "Change in 2BR rent as share of 50% of MSA median income, 2015 5-year ACS to 2024 5-year ACS",
       caption = "Source: 5-year ACS") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0))


c

ggsave(filename = "images/county_rent_map.png", plot = c, width = 14, height = 9)





to_plot <-
  df %>%
  mutate(short_name = str_remove(NAME, ",.*")) %>%
  mutate(short_name = str_remove(short_name, " County")) %>%
  mutate(short_name = str_remove(short_name, " city")) %>%
  mutate(short_name = ifelse(short_name == "District of Columbia", "DC", short_name)) %>%
  mutate(br2 = ifelse(vintage == 2015, br2*1.26, br2))  # adjust for inflation


d <- 
  ggplot(to_plot %>%
         filter(short_name %in% c(
           "DC", "Alexandria", "Arlington", "Prince George's", "Montgomery"
         ))
       , aes(x=short_name, y=br2, fill = as.factor(vintage))) +
  geom_bar(stat = "identity", position='dodge') +
  theme_minimal() +
  labs(
    x = "", y = "", fill = "", title = "Inflation adjusted median rent for a 2BR",
    caption = "Source: 5-year ACS"
  ) +
  scale_y_continuous(labels = label_dollar()) +
  theme(plot.caption = element_text(hjust = 0))

d

ggsave(filename = "images/nearby_county_rents.png", plot = d, width = 14, height = 9)






























puma_dc_2024 <- get_acs(
  geography = "public use microdata area",
  state     = "DC",
  variables = my_vars,
  survey    = "acs5",
  year      = 2024,
  key       = CENSUS_KEY
) %>%
  mutate(vintage = 2024)

puma_dc_2015 <- get_acs(
  geography = "public use microdata area",
  state     = "DC",
  variables = my_vars,
  survey    = "acs5",
  year      = 2015,
  key       = CENSUS_KEY
) %>%
  mutate(vintage = 2015)

pumas <- 
  bind_rows(puma_dc_2015, puma_dc_2024) %>%
  select(GEOID, NAME, variable, estimate, vintage) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  dplyr::left_join(
    y = msa_income %>% select(vintage, estimate) %>% rename(msa_hh_income = estimate), 
    by = "vintage") %>%
  mutate(across(all_of(br_cols),
                ~ .x / (msa_hh_income*.5 / 12),
                .names = "pct_{.col}"))

setdiff(puma_dc_2015$GEOID, puma_dc_2024$GEOID)
setdiff(puma_dc_2024$GEOID, puma_dc_2015$GEOID)

puma_geo_2024 <- pumas(state = "DC", cb = TRUE, year = 2020)
puma_geo_2015 <- pumas(state = "DC", cb = TRUE, year = 2019)


puma_shp <-
  bind_rows(
      puma_geo_2015 %>% mutate(GEOID = GEOID10, vintage=2015)
    , puma_geo_2024 %>% mutate(GEOID = GEOID20, vintage=2024)
    ) %>%
  select(-GEOID10, -GEOID20, -ends_with("10"), -ends_with("20")) %>%
  left_join(pumas, by=c("GEOID","vintage")) %>%
  mutate(across(all_of(br_cols),
                ~ .x * ifelse(vintage==2015, 1.26, 1),
                .names = "inf_adj_{.col}"))

e <- ggplot(data = puma_shp) +
  geom_sf(aes(fill = inf_adj_br2), color = "white") +
  geom_sf_label(aes(label = scales::dollar(inf_adj_br2, accuracy = 1)),
                size = 2.6, fontface = "bold", fill = "white", alpha = 0.85,
                label.size = 0) +
  scale_fill_viridis_c(option = "mako") +
  labs(x="", y='', fill="2022 $", title="Inflation adjusted rents within DC",
       caption="Source: 5-year ACS") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0)) +
  facet_grid(~vintage)


ggsave(filename = "images/rents_by_puma.png", plot = e, width = 8, height = 6)
















