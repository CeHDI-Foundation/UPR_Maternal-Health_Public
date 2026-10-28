# =============================================================================
# 16_revision_R3.1b_time_varying.R
#
# QUESTION  Reviewer 3, point 1(b): the exposure should vary over time. In the paper, each
#           country has one fixed total (cycles 1-3) for every year, including recommendations
#           it had not yet received.
#
# ANALYSIS  Cumulative count of recommendations received so far: 0 before the first review,
#           then cycle 1, cycle 1+2, cycle 1+2+3, each counted from L years after its review
#           (L = 0, 1, 3).
#             y = b0 + f(year) + theta C_it(L) + GDP + FCS + u0i + u1i year + e
#           theta = % difference in the outcome per additional recommendation already received
#           (a shift in level relative to the country's own trend, not a change per year).
#           f(year) = straight line, or natural cubic spline (curved trend): with a straight
#           line, theta can pick up curvature of the common trend.
#
# OUTPUT    output/revision/16_time_varying.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

themes_tv <- str_remove(themes_sens, "n_tot_")

# Running total per country and review
cum_by_review <- recs_by_review |>
  group_by(iso3) |>
  mutate(across(all_of(themes_tv), cumsum, .names = "cum_{.col}")) |>
  ungroup() |>
  select(iso3, review_year, starts_with("cum_"))

# Check: the total after the last review equals the total used in the paper (0 rows expected)
cum_by_review |>
  group_by(iso3) |> slice_max(review_year, n = 1, with_ties = FALSE) |> ungroup() |>
  inner_join(dat_main |> distinct(COUNTRY, n_tot_Maternal_health), join_by(iso3 == COUNTRY)) |>
  filter(cum_Maternal_health != n_tot_Maternal_health)

# Running total reached by each country-year (counted from review year + lag)
add_cumulative <- function(data, lag) {
  lookup <- cum_by_review |> mutate(from_year = review_year + lag) |> select(-review_year)
  data |>
    left_join(lookup, join_by(COUNTRY == iso3, closest(YEAR >= from_year))) |>
    mutate(across(starts_with("cum_"), \(x) coalesce(as.numeric(x), 0))) |>
    select(-from_year)
}

trends <- c(`Straight trend` = "year", `Curved trend` = "ns(year, df = 3)")

tv_results <- expand_grid(trend = names(trends), lag = c(0, 1, 3),
                          outcome = names(outcome_vars), theme = themes_tv) |>
  mutate(res = pmap(list(trend, lag, outcome, theme), possibly(\(tr, lg, o, th) {
    f <- as.formula(paste0(outcome_vars[[o]], " ~ ", trends[[tr]], " + cum_", th,
                           " + (1 + year | country_name) + GDP_long_c + FCS_count_cat"))
    tidy_term(fit_mlm(f, add_cumulative(dat_main, lg)), paste0("cum_", th))
  }, otherwise = NULL, quiet = FALSE))) |>
  unnest(res) |>
  add_labels() |>
  mutate(analysis = paste0(trend, ", lag ", lag))

write_csv(tv_results, file.path(out_dir, "16_time_varying.csv"))

# % difference in the outcome per additional recommendation already received
show_wide(tv_results |> filter(trend == "Straight trend"), "analysis")
show_wide(tv_results |> filter(trend == "Curved trend"), "analysis")
