# Load in the pre-processed datatables
source(here("code", "03b_prep_read_datatable.R"))

# Summaries for descriptive section of paper ----

UPR_total_country <- sdg_data |> 
  filter(iso3 %in% c(analysis_countries)) |> 
  filter(cycle != "Cycle 4") |> 
  group_by(state_under_review) |> 
  summarise(
    UPR_total = n(),
    UPR_health = sum(health_related != "Other"),
    UPR_GBV = sum(GBV != "Other"),
    UPR_CAH = sum(CAH != "Other"),
    UPR_health_systems = sum(health_systems != "Other"),
    UPR_sexual_health = sum(sexual_health != "Other"),
    UPR_maternal = sum(Maternal_health != "Other")
  ) |> 
  ungroup() |> 
  mutate(
    perc_health = UPR_health/UPR_total*100,
    perc_GBV = UPR_GBV/UPR_total*100,
    perc_CAH = UPR_CAH/UPR_total*100,
    perc_health_systems = UPR_health_systems/UPR_total*100,
    perc_sexual_health = UPR_sexual_health/UPR_total*100,
    perc_maternal = UPR_maternal/UPR_total*100
  )

summary(UPR_total_country)

UPR_total <- sdg_data |> 
  filter(iso3 %in% c(analysis_countries)) |> 
  filter(cycle != "Cycle 4") |> 
  summarise(
    UPR_total = n(),
    UPR_health = sum(health_related != "Other"),
    maternal_health = sum(Maternal_health != "Other")
  )|> 
  mutate(cycle = "Total")

UPR_total_cycle <- sdg_data |> 
  filter(iso3 %in% c(analysis_countries)) |> 
  filter(cycle != "Cycle 4") |> 
  group_by(cycle) |> 
  summarise(UPR_total = n(),UPR_health = sum(health_related != "Other"), maternal_health = sum(Maternal_health != "Other")) |> 
  ungroup()

UPR_total_combined <- bind_rows(UPR_total_cycle, UPR_total) |> 
  mutate(health_perc = sprintf("%.2f", UPR_health / UPR_total *100))
print(UPR_total_combined)


# Plot the individual countries' trajectories and overall trends
dat_table_long |> 
  mutate(recs_cat_Maternal_health = factor(case_when(
    recs_cat_Maternal_health == "0" ~ "0-3",
    recs_cat_Maternal_health == "1-3" ~ "0-3",
    recs_cat_Maternal_health == "4+" ~ "4+"
  ), levels = c("0-3", "4+"))) |> 
  # filter(!is.na(GDP_2005_cat)) |> 
  filter(year >=0) |>
  # filter(!YEAR %in% c(2020, 2021)) |>
  ggplot(aes(x = YEAR, y = MMR_long_log))+
  geom_line(aes(group = country_name
                , color = recs_cat_Maternal_health
  ), alpha = 0.4)+
  geom_smooth(
    aes(
      color = recs_cat_Maternal_health
    )
    # , method = "lm"
  )+
  scale_x_continuous(
    # breaks = c(-8, -5, 0, 5, 10, 15), 
    expand = c(0,0)) +
  theme_classic()

dat_table_long |> 
  mutate(recs_cat_Maternal_health = factor(case_when(
    recs_cat_Maternal_health == "0" ~ "0-3",
    recs_cat_Maternal_health == "1-3" ~ "0-3",
    recs_cat_Maternal_health == "4+" ~ "4+"
  ), levels = c("0-3", "4+"))) |> 
  ggplot(aes(x = YEAR, y = MMR_long_log))+
  geom_line(aes(group = country_name
                , color = recs_cat_Maternal_health
  ), alpha = 0.4)+
  geom_smooth(
    aes(
      color = recs_cat_Maternal_health
    )
    # , method = "lm"
  )+
  scale_x_continuous(
    # breaks = c(-8, -5, 0, 5, 10, 15), 
    expand = c(0,0)) +
  # geom_smooth(aes(color = FCS_count_cat))+
  # facet_wrap(.~recs_cat_Maternal_health)+
  facet_wrap(.~spline_years_2, scales = "free_x", space = "free_x")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 30))
