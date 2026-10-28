# =============================================================================
# 14_revision_R3.1a_before_after.R
#
# QUESTION  Reviewer 3, point 1(a): were countries with more recommendations already
#           improving faster BEFORE the UPR (e.g. 2005-2008)?
#
# ANALYSIS  Mixed models with time counted from each country's first review
#           (year_upr: negative before, 0 = review year; t_post = years since the review).
#           Per additional recommendation:
#             before = slope difference before the review           (should be ~0)
#             change = change in that difference at the review      ("UPR signal")
#             after  = slope difference after the review (before + change)
#
#           A  Main model, 2005-2023 (calendar year)
#           B1 Negative control, 2005-2007 (no country reviewed yet)
#           B2 Negative control, 2000-2007
#           E  Negative control, each country's own 8 years before its first review
#           C  Before/after, from 8 years before the first review   (main)
#           D  Before/after, 2005-2023
#
#           Model C:  y = b0 + b1 r + b2 p + b3 n + b4 (r x n) + b5 (p x n) + GDP + FCS
#                         + u0i + u1i r + u2i p + e,   r = year_upr, p = t_post
#                     before = b4, change = b5, after = b4 + b5
#
# OUTPUT    output/revision/14_before_after.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

dat_seg <- dat_table_long |> mutate(t_post = pmax(year_upr, 0))   # all years from 2000

seg_specs <- tribble(
  ~model,                       ~keep,                                         ~time,               ~quantities,
  "A Main 2005-2023",           \(d) filter(d, YEAR >= 2005),                  "year",              list(before = "year:{th}"),
  "B1 Neg. control 2005-07",    \(d) filter(d, between(YEAR, 2005, 2007)),     "year",              list(before = "year:{th}"),
  "B2 Neg. control 2000-07",    \(d) filter(d, between(YEAR, 2000, 2007)),     "year",              list(before = "year:{th}"),
  "E Neg. control own pre-8y",  \(d) filter(d, year_upr >= -8, year_upr < 0),  "year_upr",          list(before = "year_upr:{th}"),
  "C Before/after (main)",      \(d) filter(d, year_upr >= -8),                "year_upr + t_post", list(before = "year_upr:{th}",
                                                                                                         change = "t_post:{th}",
                                                                                                         after  = "year_upr:{th} + t_post:{th}"),
  "D Before/after 2005-2023",   \(d) filter(d, YEAR >= 2005),                  "year_upr + t_post", list(before = "year_upr:{th}",
                                                                                                         change = "t_post:{th}",
                                                                                                         after  = "year_upr:{th} + t_post:{th}")
)

seg_results <- expand_grid(outcome = names(outcome_vars), theme = themes_sens, seg_specs) |>
  mutate(res = pmap(list(outcome, theme, keep, time, quantities), possibly(\(o, th, keep, tm, qs) {
    m <- fit_mlm(formula_mv(outcome_vars[[o]], th, time = tm), keep(dat_seg))
    imap_dfr(qs, \(expr, label) lincom(m, str_replace_all(expr, fixed("{th}"), th)) |>
               mutate(quantity = label, .before = 1))
  }, otherwise = NULL, quiet = FALSE))) |>
  select(outcome, theme, model, res) |>
  unnest(res) |>
  add_labels() |>
  mutate(analysis = model)

write_csv(seg_results, file.path(out_dir, "14_before_after.csv"))

# One table per theme: models in rows, before / change / after in columns
for (th in themes_sens) {
  seg_results |>
    filter(theme == th) |>
    mutate(effect = fmt_eff(estimate_pct, conf.low_pct, conf.high_pct, p.value)) |>
    select(theme_label, outcome, model, quantity, effect) |>
    pivot_wider(names_from = quantity, values_from = effect, values_fill = "") |>
    print(n = Inf, width = Inf)
}

seg_results |>
  filter(model == "C Before/after (main)") |>
  mutate(quantity = factor(quantity, levels = c("before", "change", "after"))) |>
  plot_compare("quantity", "Model C: before vs after the first review")
