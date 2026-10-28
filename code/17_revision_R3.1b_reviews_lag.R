# =============================================================================
# 17_revision_R3.1b_reviews_lag.R
#
# QUESTION  Reviewer 3, point 1 (a and b together): placing each review (cycles 1-3) at its
#           exact year and allowing time for implementation, does the trajectory change after
#           countries receive recommendations, and did it already differ before their first review?
#
# ANALYSIS  For country i with reviews k at years T_ik, receiving n_ik recommendations:
#             E_it(L) = sum_k n_ik x max(YEAR - T_ik - L, 0)     ("recommendation-years")
#             pre_it  = min(YEAR - T_i1, 0)                      (years before the first review)
#             post_it = max(YEAR - T_i1 - L, 0)                  (years after first review + lag)
#
#             y = b0 + f(year) + b1 pre + b2 post + b3 N + lambda (N x pre) + theta E(L)
#                 + GDP + FCS + u0i + u1i year + e
#
#           lambda = slope difference BEFORE the first review per (future) recommendation (~0)
#           theta  = change in slope per recommendation, from L years after the review that
#                    issued it.  L = 0, 1, 2, 3.  Both: % change per year per recommendation.
#           Window: from 8 years before the first review.  f(year) = natural cubic spline.
#
# OUTPUT    output/revision/17_reviews_lag.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

trend <- "ns(year, df = 3)"   # common trend ("year" for a straight line)
lags  <- 0:3
themes_ry <- str_remove(themes_sens, "n_tot_")

# Recommendation-years: sum over the reviews already "active" of n_k x years since (T_k + lag)
add_rec_years <- function(data, lag) {
  exposure <- data |>
    distinct(COUNTRY, YEAR) |>
    inner_join(recs_by_review |> select(iso3, review_year, all_of(themes_ry)),
               join_by(COUNTRY == iso3), relationship = "many-to-many") |>
    mutate(across(all_of(themes_ry), \(n) n * pmax(YEAR - (review_year + lag), 0))) |>
    group_by(COUNTRY, YEAR) |>
    summarise(across(all_of(themes_ry), sum), .groups = "drop") |>
    rename_with(\(x) paste0("ry_", x), all_of(themes_ry))
  data |>
    left_join(exposure, join_by(COUNTRY, YEAR)) |>
    mutate(across(starts_with("ry_"), \(x) coalesce(x, 0)))
}

make_lag_data <- function(lag) {
  dat_table_long |>
    filter(year_upr >= -8) |>
    mutate(pre = pmin(year_upr, 0), post_any = pmax(year_upr - lag, 0)) |>
    add_rec_years(lag) |>
    droplevels()
}

# Check on one country (Nigeria): reviews, recommendations and exposure by year, L = 2
recs_by_review |> filter(iso3 == "NGA") |> select(cycle, review_year, Maternal_health)
make_lag_data(2) |> filter(COUNTRY == "NGA") |> select(YEAR, year_upr, pre, post_any, ry_Maternal_health) |> print(n = Inf)

# Models ----------------------------------------------------------------------------------------
ry_results <- expand_grid(lag = lags, outcome = names(outcome_vars), theme = themes_ry) |>
  mutate(res = pmap(list(lag, outcome, theme), possibly(\(lg, o, th) {
    f <- as.formula(paste0(
      outcome_vars[[o]], " ~ ", trend, " + pre + post_any",
      " + n_tot_", th, " + n_tot_", th, ":pre",      # lambda
      " + ry_", th,                                  # theta
      " + GDP_long_c + FCS_count_cat + (1 + year | country_name)"
    ))
    m <- fit_mlm(f, make_lag_data(lg))
    bind_rows(
      tidy_term(m, paste0("n_tot_", th, ":pre")) |> mutate(quantity = "lambda: before first review"),
      tidy_term(m, paste0("ry_", th))            |> mutate(quantity = "theta: after each review")
    )
  }, otherwise = NULL, quiet = FALSE))) |>
  unnest(res) |>
  add_labels() |>
  mutate(analysis = paste0("L = ", lag))

write_csv(ry_results, file.path(out_dir, "17_reviews_lag.csv"))

show_wide(ry_results |> filter(str_starts(quantity, "lambda")), "analysis")
show_wide(ry_results |> filter(str_starts(quantity, "theta")),  "analysis")
