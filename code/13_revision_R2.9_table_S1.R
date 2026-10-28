# =============================================================================
# 13_revision_R2.9_table_S1.R
#
# QUESTION  Reviewer 2, point 9: how many recommendations were there per theme in total?
#
# ANALYSIS  Recommendations per theme, UPR cycles 1-3, for all UN Member States and for the
#           analysed countries (Table S1).
#           Note: Maternal_health (the paper's exposure) = maternal_health WITHOUT the
#           abortion-related recommendations (defined in 03b), hence two maternal-health lines.
#
# OUTPUT    output/revision/13_table_S1.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

all_themes <- union("Maternal_health",
                    names(sdg_data)[map_lgl(sdg_data, \(x) is.factor(x) && "Other" %in% levels(x))])

table_s1 <- sdg_data |>
  filter(cycle %in% c("Cycle 1", "Cycle 2", "Cycle 3")) |>
  mutate(in_analysis = iso3 %in% unique(dat_main$COUNTRY)) |>
  summarise(across(all_of(all_themes),
                   list(all_countries = \(x) sum(x != "Other", na.rm = TRUE),
                        analysis      = \(x) sum(x != "Other" & in_analysis, na.rm = TRUE)))) |>
  pivot_longer(everything(), names_to = c("variable", ".value"),
               names_pattern = "(.*)_(all_countries|analysis)$") |>
  left_join(theme_labels, by = "variable") |>
  mutate(theme_label = case_when(
    variable == "Maternal_health" ~ "Maternal health, excluding abortion (exposure used in the paper)",
    variable == "maternal_health" ~ "Maternal health, including abortion-related",
    .default = coalesce(theme_label, variable))) |>
  arrange(desc(all_countries))

print(table_s1, n = Inf)
write_csv(table_s1, file.path(out_dir, "13_table_S1.csv"))

# Check: must equal the sum of the exposure used in the models
sum(dat_main |> distinct(country_name, n_tot_Maternal_health) |> pull(n_tot_Maternal_health))
