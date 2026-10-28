# =============================================================================
# 07_revision_main_results.R
#
# QUESTION  What are the paper's main results on the corrected data?
#           (missing recommendations added for 6 countries, duplicates removed)
#           These are the reference for every other analysis.
#
# ANALYSIS  The multivariable models of 04a-04c, for all nine themes:
#           outcome ~ year * n_tot_<theme> + (1 + year | country_name) + GDP_long_c + FCS_count_cat
#           Reported: year:n_tot_<theme> = % change per year per additional recommendation
#           (MMR: % change in MMR; SBA, CPR: % change in the odds).
#
# OUTPUT    output/revision/07_main_results.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

main_results <- run_over_themes(dat_main) |>
  mutate(analysis = "Main model")

write_csv(main_results, file.path(out_dir, "07_main_results.csv"))

# Table: one row per theme, one column per outcome
main_results |>
  mutate(effect = fmt_eff(estimate_pct, conf.low_pct, conf.high_pct, p.value)) |>
  select(theme = theme_label, outcome, effect) |>
  pivot_wider(names_from = outcome, values_from = effect) |>
  print(n = Inf, width = Inf)
