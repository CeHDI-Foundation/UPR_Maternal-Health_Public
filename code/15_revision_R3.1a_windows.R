# =============================================================================
# 15_revision_R3.1a_windows.R
#
# QUESTION  Reviewer 3, point 1(a): were countries with more recommendations already
#           improving faster BEFORE the UPR, and does the association appear AFTER it?
#           (also part of 3.1(b): only recommendations already received are counted)
#
# ANALYSIS  Change in each outcome over windows relative to each country's FIRST review (T),
#           regressed on the recommendations received at that review (cycle 1), adjusting for
#           the starting level, GDP at the start, FCS, constitution, CEDAW, ICESCR and the
#           review year. One row per country -> OLS.
#             Pre (placebo): T-3 -> T      (no recommendation received yet: should be ~0)
#             Post:          T   -> T+h    h = 1 to 4 years, ending before the second review
#           Changes are per year (change / window length): % change per year per additional
#           recommendation, as in the paper (SBA, CPR: in the odds).
#           Formal test: association after - before (stacked regression, SE clustered by country).
#
#           Window model:  (y_i,T+b - y_i,T+a) / (b - a) = a0 + beta n1_i + g1 y_i,T+a
#                          + g2 log GDP_i,T+a + g3 FCS_i + g4 Const_i,T+a + g5 CEDAW_i,T+a
#                          + g6 ICESCR_i,T+a + d_T + e_i
#
#           Confounders are measured at the START of each window (year T+a), not later:
#             CEDAW, ICESCR  = 1 if ratified in or before T+a
#             Constitution   = health provision / no specific provision, known only for the
#                              constitution in force in 2024 -> countries whose current
#                              constitution was adopted after T+a get their own category
#                              ("Adopted after window start"): their content at T+a is unknown.
#           Measuring them later (e.g. at the end of the window, or in 2024) would adjust for
#           changes that the UPR itself may have caused (it often recommends ratifying treaties).
#
# OUTPUT    output/revision/15_windows.csv
#           output/revision/15_windows_post_vs_pre.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

# Settings -----------------------------------------------------------------------------------
pre_start <- -3     # start of the placebo window (data start in 2000, so up to -8 is possible)
windows <- tribble(
  ~window,                                        ~a,        ~b,
  paste0("Pre (placebo): T", pre_start, " to T"),  pre_start,  0,
  "Post: T to T+1",                                0,          1,
  "Post: T to T+2",                                0,          2,
  "Post: T to T+3",                                0,          3,
  "Post: T to T+4",                                0,          4
)
pre_window <- windows$window[1]

# Outcomes and how far (years) to look for the nearest observation (SBA = surveys)
outcomes <- tribble(
  ~outcome, ~var,                            ~scale,  ~tol,
  "MMR",    "MMR_long_log",                  "log",    0,
  "SBA",    "skilled_birth_long_p_squeezed", "logit",  2,
  "CPR",    "CPR_any_long_p",                "logit",  0
)
themes <- str_remove(themes_sens, "n_tot_")

# Check: years between the first and second review (post windows must end before the second)
dat_table_long |>
  distinct(country_name, year_cycle_1, year_cycle_2) |>
  count(years_between_reviews = year_cycle_2 - year_cycle_1)

# Country data: recommendations at the first review + confounders -------------------------------
recs_c1 <- recs_by_review |>
  filter(cycle == "Cycle 1") |>
  select(iso3, all_of(themes)) |>
  rename_with(\(x) paste0("n1_", x), all_of(themes))

countries <- dat_table_long |>
  distinct(COUNTRY, country_name, FCS_count_cat, year_cycle_1, year_cycle_2) |>
  left_join(recs_c1, join_by(COUNTRY == iso3)) |>
  left_join(load_confounders() |> select(iso3, const_health, const_yr_adopt, cedaw_year, icescr_year),
            join_by(COUNTRY == iso3)) |>
  mutate(across(starts_with("n1_"), \(x) coalesce(x, 0L)))

countries |> count(n1_Maternal_health)   # distribution of the dose

# Value of a variable at year T + r (closest observation within `tol` years)
value_at <- function(var, r, tol) {
  dat_table_long |>
    filter(!is.na(.data[[var]]), !is.na(year_upr), abs(year_upr - r) <= tol) |>
    mutate(dist = abs(year_upr - r)) |>
    group_by(COUNTRY) |>
    slice_min(dist, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(COUNTRY, value = all_of(var), year_used = YEAR)
}

# One row per country: start value, change per year, GDP at the start, country data
make_window <- function(var, scale, tol, a, b) {
  tf <- if (scale == "logit") qlogis else identity
  start <- value_at(var, a, tol) |> rename(y_start = value, year_start = year_used)
  end   <- value_at(var, b, tol) |> rename(y_end = value,   year_end = year_used)
  gdp   <- value_at("GDP_long", a, 1) |> transmute(COUNTRY, log_gdp_start = log(value))
  inner_join(start, end, by = "COUNTRY") |>
    filter(year_end > year_start) |>
    mutate(y_start = tf(y_start), y_end = tf(y_end),
           delta_py = (y_end - y_start) / (year_end - year_start)) |>
    left_join(gdp, by = "COUNTRY") |>
    left_join(countries, by = "COUNTRY") |>
    filter(b <= 0 | is.na(year_cycle_2) | year_end <= year_cycle_2) |>   # before the second review
    add_confounders_at(a)
}

# Confounders as they were at the start of the window (year T + a)
add_confounders_at <- function(d, a) {
  d |>
    mutate(
      start_yr     = year_cycle_1 + a,
      cedaw_start  = as.integer(!is.na(cedaw_year)  & cedaw_year  <= start_yr),
      icescr_start = as.integer(!is.na(icescr_year) & icescr_year <= start_yr),
      const_start  = case_when(
        is.na(const_health) | is.na(const_yr_adopt) ~ NA_character_,
        const_yr_adopt > start_yr                    ~ "Adopted after window start",
        const_health == 1                            ~ "Health provision",
        .default                                     = "No specific provision") |>
        factor(levels = c("No specific provision", "Health provision", "Adopted after window start"))
    )
}

# Check: confounder distribution at the start of each window (MMR windows)
windows |>
  mutate(tab = map2(a, b, \(a, b) make_window("MMR_long_log", "log", 0, a, b) |>
                      count(const_start, cedaw_start, icescr_start))) |>
  select(window, tab) |>
  unnest(tab) |>
  print(n = Inf)

covariates <- " + y_start + log_gdp_start + FCS_count_cat + const_start + cedaw_start + icescr_start + factor(year_cycle_1)"

# 1. One regression per window ------------------------------------------------------------------
win_results <- expand_grid(outcomes, windows, theme = themes) |>
  mutate(res = pmap(list(var, scale, tol, a, b, theme), possibly(\(v, s, tl, a, b, th) {
    d <- make_window(v, s, tl, a, b)
    m <- lm(as.formula(paste0("delta_py ~ n1_", th, covariates)), data = d)
    tidy(m, conf.int = TRUE) |>
      filter(term == paste0("n1_", th)) |>
      transmute(estimate, std.error, p.value,
                estimate_pct = pct(estimate), conf.low_pct = pct(conf.low), conf.high_pct = pct(conf.high),
                n_obs = nobs(m))
  }, otherwise = NULL, quiet = FALSE))) |>
  select(outcome, window, theme, res) |>
  unnest(res) |>
  add_labels() |>
  mutate(analysis = factor(window, levels = windows$window))

write_csv(win_results, file.path(out_dir, "15_windows.csv"))
show_wide(win_results, "analysis")

# 2. Formal test: after - before -------------------------------------------------------------------
# Each country contributes a pre row and a post row; all terms may differ between periods.
test_results <- expand_grid(outcomes, post = windows$window[-1], theme = themes) |>
  mutate(res = pmap(list(var, scale, tol, post, theme), possibly(\(v, s, tl, pw, th) {
    w_pre  <- windows |> filter(window == pre_window)
    w_post <- windows |> filter(window == pw)
    pre_d  <- make_window(v, s, tl, w_pre$a,  w_pre$b)  |> mutate(period = "Pre")
    post_d <- make_window(v, s, tl, w_post$a, w_post$b) |> mutate(period = "Post")
    d <- bind_rows(pre_d, post_d) |>
      filter(COUNTRY %in% intersect(pre_d$COUNTRY, post_d$COUNTRY)) |>
      mutate(period = factor(period, levels = c("Pre", "Post")))
    m  <- lm(as.formula(paste0("delta_py ~ (n1_", th, covariates, ") * period")), data = d)
    vc <- vcovCL(m, cluster = ~ COUNTRY)
    term <- paste0("n1_", th, ":periodPost")
    ct <- coeftest(m, vcov. = vc)[term, ]
    ci <- coefci(m, vcov. = vc)[term, ]
    tibble(estimate = ct[["Estimate"]], p.value = ct[["Pr(>|t|)"]],
           estimate_pct = pct(ct[["Estimate"]]), conf.low_pct = pct(ci[[1]]), conf.high_pct = pct(ci[[2]]),
           n_obs = n_distinct(d$COUNTRY))
  }, otherwise = NULL, quiet = FALSE))) |>
  select(outcome, post, theme, res) |>
  unnest(res) |>
  add_labels() |>
  mutate(analysis = factor(paste0("Difference: ", post, " vs pre"),
                           levels = paste0("Difference: ", windows$window[-1], " vs pre")))

write_csv(test_results, file.path(out_dir, "15_windows_post_vs_pre.csv"))
show_wide(test_results, "analysis")

# Plot ---------------------------------------------------------------------------------------------
plot_compare(win_results |> mutate(analysis = fct_rev(analysis)), "analysis",
             xlab = "% change per year per additional recommendation at the first review [95% CI]")