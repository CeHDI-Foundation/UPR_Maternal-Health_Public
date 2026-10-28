# =============================================================================
# 18_revision_summary.R
#
# QUESTION  Overall: do the main results hold across the analyses requested by the reviewers?
#
# ANALYSIS  Collects the maternal-health estimates from the results of scripts 07-17
#           (run those first) into one table: one row per check, one column per outcome.
#           Missing files are skipped.
#
# OUTPUT    output/revision/18_summary_maternal_health.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

read_res <- function(file) {
  path <- file.path(out_dir, file)
  if (!file.exists(path)) { message("Not found (run the script first): ", file); return(NULL) }
  read_csv(path, show_col_types = FALSE)
}

# One row per check: file, filter, label
checks <- list(
  list("07_main_results.csv",     \(d) d,                                                  "Main result (corrected data)"),
  list("08_supported_only.csv",   \(d) filter(d, analysis == "Supported only"),             "R2.2 Supported recommendations only"),
  list("10_nonlinearity.csv",     \(d) filter(d, analysis == "Curved trend (spline)"),      "R2.5 Curved common trend"),
  list("10_nonlinearity.csv",     \(d) filter(d, analysis == "Excluding 2020-2021"),        "R2.5 Excluding 2020-2021"),
  list("11_confounders.csv",      \(d) filter(d, analysis == "+ all three"),                "R2.6/R3.3 + constitution, CEDAW, ICESCR"),
  list("12_sba_min3.csv",         \(d) filter(d, str_starts(analysis, ">=")),               "R2.7 SBA, countries with >= 3 observations"),
  list("14_before_after.csv",     \(d) filter(d, model == "E Neg. control own pre-8y"),     "R3.1a Negative control: 8 years before review (~0)"),
  list("14_before_after.csv",     \(d) filter(d, model == "C Before/after (main)", quantity == "before"), "R3.1a Before first review (~0)"),
  list("14_before_after.csv",     \(d) filter(d, model == "C Before/after (main)", quantity == "change"), "R3.1a Change at first review"),
  list("14_before_after.csv",     \(d) filter(d, model == "C Before/after (main)", quantity == "after"),  "R3.1a After first review"),
  list("15_windows.csv",          \(d) filter(d, str_starts(analysis, "Pre")),              "R3.1a Window: before first review (placebo, ~0)"),
  list("15_windows.csv",          \(d) filter(d, analysis == "Post: T to T+4"),             "R3.1a Window: T to T+4"),
  list("15_windows_post_vs_pre.csv", \(d) filter(d, analysis == "Difference: Post: T to T+4 vs pre"), "R3.1a Window: difference T to T+4 vs pre"),
  list("16_time_varying.csv",     \(d) filter(d, analysis == "Curved trend, lag 1"),        "R3.1b Cumulative count, lag 1 (shift in level)"),
  list("17_reviews_lag.csv",      \(d) filter(d, str_starts(quantity, "lambda"), analysis == "L = 2"), "R3.1 Each review: lambda, before first review (~0)"),
  list("17_reviews_lag.csv",      \(d) filter(d, str_starts(quantity, "theta"),  analysis == "L = 2"), "R3.1 Each review: theta, from 2 years after")
)

summary_mh <- map_dfr(checks, \(ch) {
  d <- read_res(ch[[1]])
  if (is.null(d)) return(NULL)
  ch[[2]](d) |>
    filter(variable == "Maternal_health") |>
    transmute(check = ch[[3]], outcome = as.character(outcome),
              effect = fmt_eff(estimate_pct, conf.low_pct, conf.high_pct, p.value))
}) |>
  mutate(check = factor(check, levels = map_chr(checks, 3) |> unique())) |>
  pivot_wider(names_from = outcome, values_from = effect, values_fill = "-") |>
  arrange(check) |>
  select(check, any_of(names(outcome_vars)))

print(summary_mh, n = Inf, width = Inf)
write_csv(summary_mh, file.path(out_dir, "18_summary_maternal_health.csv"))
