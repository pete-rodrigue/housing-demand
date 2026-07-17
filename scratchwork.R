library(tidycensus); library(tidyverse); library(patchwork); library(scales)
library(Hmisc); library(tidyverse); library(dplyr); library(zoo)

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

pop <-
  pa %>%
  filter(WGTP > 0) %>%
  group_by(YEAR) %>%
  dplyr::summarise(n_ppl = sum(PWGTP)) 


beg_year = 2024
end_year = 2030
decline_factor = 0.001

# middle estimate: linear extrapolation
pop_proj     <- Hmisc::approxExtrap(x = pop$YEAR, y = pop$n_ppl, xout = seq(beg_year, end_year, 1))


# lower estimate: population declines at decline_factor % per year
for (i in seq(beg_year, end_year, 1)) {
  if (i == beg_year) {pop_proj_l = pop$n_ppl[pop$YEAR==beg_year]} else {
    last_val = pop_proj_l[length(pop_proj_l)]
    pop_proj_l <- c(pop_proj_l, last_val*(1-decline_factor))
  }
}

grs = c()
for (i in seq(2013, 2020, 1)) {
  growth_pct = (pop$n_ppl[pop$YEAR==i] - pop$n_ppl[pop$YEAR==i-1]) / pop$n_ppl[pop$YEAR==i-1]
  grs = c(grs, growth_pct)
}


# high estimate: average annual population growth rate between 2012 and 2020
mean_gr = mean(grs)

for (i in seq(beg_year, end_year, 1)) {
  if (i == beg_year) {pop_proj_h = pop$n_ppl[pop$YEAR==beg_year]} else {
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

ggplot(data = pop, aes(x=YEAR, y=n_ppl)) + 
  geom_line() + 
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=low_proj), linetype = "dashed", color=pal[4]) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=pop_proj), linetype = "dashed", color=pal[3]) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=hig_proj), linetype = "dashed", color=pal[2]) +
  geom_line(data=what_ifs_pops, aes(x=YEAR, y=jlg_goal), linetype = "dashed", color=pal[1]) +
  theme_minimal() +
  labs(x="", y="Number of residents", title="DC non-group quarters population (5-year ACS waves)",
       caption = paste0("Dashed lines, from the top down:\n",
                        "Incoming Mayor's 72,000 housing unit goal achieved by end of period (Applies 2024 ratio of housing units to population).\n",
                        "Population grows at average annual rate observed during 2012-2020 period.\n",
                        "Population increases linearly after 2023-2024.\n",
                        "Population declines at 0.1%/year.")) +
  scale_x_continuous(breaks = seq(2012, 2030, by = 1)) + 
  scale_y_continuous(breaks = seq(550e3, 800e3, by = 20e3), labels = scales::label_comma()) +
  theme(
    # Left-align the text layout (0 = left, 0.5 = center, 1 = right)
    plot.caption = element_text(hjust = 0), 
    # 
    # # Optional: Align to the edge of the entire plot instead of the panel grid
    # plot.caption.position = "plot" 
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

br_counts %>%
  select(-YEAR, -proportion) %>%
  pivot_longer(cols = -n_brs) %>%
  mutate(
    name = case_when(
      name == "weighted_count" ~ "2024 actual",
      name == paste0("low_est_", end_year)   ~ paste0(end_year, ", low est."),
      name == paste0("mid_est_", end_year)   ~ paste0(end_year, ", middle est."),
      name == paste0("hig_est_", end_year)   ~ paste0(end_year, ", high est."),
      name == paste0("jlg_est_", end_year)   ~ "Incoming Mayor's housing goal"
    )
  ) %>%
  mutate(
    name = factor(name, levels = c(paste0(end_year, ", low est."), 
                                   "2024 actual",  
                                   paste0(end_year, ", middle est."),  
                                   paste0(end_year, ", high est."),
                                   "Incoming Mayor's housing goal"
                                   )
                  )
  ) %>%
  ggplot() +
  geom_bar(aes(x=n_brs, y=value, fill=name), stat="identity", position="dodge") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 5, by = 1)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x="Number of bedrooms", y="Count", fill="Scenario")

