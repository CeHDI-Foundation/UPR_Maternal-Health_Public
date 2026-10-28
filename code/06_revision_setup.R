# =============================================================================
# 06_revision_setup.R
# Shared setup for the revision analyses (scripts 07 to 18). Not run on its own:
# every analysis script starts with source(here("code", "06_revision_setup.R")).
#
# Overview of the revision scripts
#   07  Main results on the corrected data (reference for all comparisons)
#   08  R2.2  Supported vs noted recommendations
#   09  R2.4  Model details and example trajectories (observed vs fitted)
#   10  R2.5  Non-linear trajectories (plots, residuals, curved trend, COVID-19 years)
#   11  R2.6 / R3.3  Confounding and alternative pathways (constitution, CEDAW, ICESCR)
#   12  R2.7  Missing data (coverage, SBA with >= 3 observations)
#   13  R2.9  Total recommendations per theme (Table S1)
#   14  R3.1a Before/after the first review, with negative controls (mixed models)
#   15  R3.1a Change over windows around the first review (placebo vs post, OLS)
#   16  R3.1b Time-varying cumulative count of recommendations, with lags
#   17  R3.1b Each review at its exact date, with a lag, and a pre-review test
#   18  Summary table (maternal health) from the results of 07-17
#
# Every script writes its results to output/revision/ as <script number>_<name>.csv.
# SBA: the original coding of the data is used (100% set to 99%), pending Anshu's answer.
# =============================================================================

pacman::p_load(
  tidyverse, broom, broom.mixed, here,
  lme4, lmerTest, glmmTMB, splines,
  countrycode, readxl, janitor, rvest,
  sandwich, lmtest
)

# Data ------------------------------------------------------------------------------
# Defines sdg_data, dat_table, dat_table_long, theme_medians, theme_vars, vars_included,
# prefix ("n_tot_") and theme_labels. Make sure 03b points to the latest data files.
source(here("code", "03b_prep_read_datatable.R"))

# The map columns (sf) are not used and break lme4/ggplot2 in recent R versions
dat_table_long <- dat_table_long |> select(-where(\(x) inherits(x, "sfc")))

# Analysis data of the paper: 2005-2023 (year 0 = 2005)
dat_main <- dat_table_long |> filter(year >= 0) |> droplevels()

# Outcomes (as in 04a-04c)
outcome_vars <- c(
  MMR = "MMR_long_log",                    # lmer, log scale
  SBA = "skilled_birth_long_p_squeezed",   # glmmTMB beta, logit scale (original coding)
  CPR = "CPR_any_long_p"                   # glmmTMB beta, logit scale
)

# Themes for the sensitivity analyses (set to theme_vars to run all nine themes)
themes_sens <- paste0("n_tot_", c("Maternal_health", "women", "health_systems"))

out_dir <- here("output", "revision")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# Model helpers -----------------------------------------------------------------------

# Multivariable formula of the paper (formula_mv in 04a-04c).
# `time` = time variable(s) (e.g. "year", "year_upr + t_post"); `extra` = added terms.
formula_mv <- function(outcome_var, theme, time = "year", extra = "") {
  as.formula(paste0(
    outcome_var, " ~ (", time, ")*", theme,
    " + (1 + ", time, " | country_name)",
    " + GDP_long_c + FCS_count_cat",
    extra
  ))
}

# lmer for MMR (04a), beta glmmTMB for SBA and CPR (04b, 04c)
fit_mlm <- function(formula, data) {
  data <- droplevels(data)
  if (all.vars(formula)[1] == "MMR_long_log") {
    lmerTest::lmer(formula, data = data, REML = FALSE, control = lmerControl(optimizer = "bobyqa"))
  } else {
    glmmTMB(formula, data = data, REML = FALSE, family = beta_family(link = "logit"))
  }
}

pct <- \(x) (exp(x) - 1) * 100   # log / logit coefficient -> % change (in the odds for SBA, CPR)

# One coefficient, as in 04a-04c (broom.mixed::tidy). The name is matched in any order
# ("year:n_tot_X" = "n_tot_X:year"). Stops if the term is not in the model.
tidy_term <- function(model, term_name) {
  wanted <- str_split_1(term_name, ":")
  out <- broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) |>
    filter(map_lgl(str_split(term, ":"), \(x) setequal(x, wanted)))
  if (nrow(out) != 1) stop("Term not found in the model: ", term_name)
  out |>
    transmute(estimate, std.error, p.value,
              estimate_pct = pct(estimate), conf.low_pct = pct(conf.low), conf.high_pct = pct(conf.high),
              n_obs = nobs(model), aic = AIC(model))
}

# A coefficient OR a sum of coefficients ("a + b"), with the SE of the sum:
# Var(a + b) = Var(a) + Var(b) + 2 Cov(a, b)
lincom <- function(model, expr) {
  if (inherits(model, "glmmTMB")) {
    est <- fixef(model)$cond; V <- vcov(model)$cond
  } else {
    est <- fixef(model); V <- as.matrix(vcov(model))
  }
  find_term <- function(term) {
    parts <- sort(strsplit(term, ":", fixed = TRUE)[[1]])
    hit <- names(est)[map_lgl(strsplit(names(est), ":", fixed = TRUE), \(x) identical(sort(x), parts))]
    if (length(hit) != 1) stop("Term not found: ", term)
    hit
  }
  L <- setNames(rep(0, length(est)), names(est))
  for (t in strsplit(expr, " + ", fixed = TRUE)[[1]]) L[find_term(t)] <- 1
  b  <- sum(L * est)
  se <- sqrt(drop(t(L) %*% V[names(L), names(L)] %*% L))
  tibble(estimate = b, std.error = se, p.value = 2 * pnorm(-abs(b / se)),
         estimate_pct = pct(b), conf.low_pct = pct(b - 1.96 * se), conf.high_pct = pct(b + 1.96 * se),
         n_obs = nobs(model))
}

# Fit the same model for every outcome x theme and extract one term
# make_formula(outcome_var, theme) -> formula; term_text + theme = term to extract
run_over_themes <- function(data, make_formula = formula_mv, themes = theme_vars,
                            outcomes = outcome_vars, term_text = "year:") {
  expand_grid(outcome = names(outcomes), theme = themes) |>
    mutate(res = map2(outcome, theme, possibly(\(o, th) {
      m <- fit_mlm(make_formula(outcomes[[o]], th), data)
      tidy_term(m, paste0(term_text, th))
    }, otherwise = NULL, quiet = FALSE))) |>
    unnest(res) |>
    add_labels()
}

# Readable theme names (theme_labels.R) + `variable` = theme without prefix
add_labels <- function(df) {
  df |>
    mutate(variable = str_remove(theme, "^n_(tot|supported)_|^ry_|^cum_|^n1_")) |>
    left_join(theme_labels, by = "variable") |>
    mutate(theme_label = case_when(variable == "health_related" ~ "Health-related (overall)",
                                   .default = coalesce(theme_label, variable)),
           outcome = factor(outcome, levels = names(outcome_vars)))
}


# Display helpers ----------------------------------------------------------------------

fmt_eff <- function(pct, low, high, p) {
  z <- \(x) ifelse(abs(x) < 0.005, 0, x)   # avoids "-0.00%"
  sprintf("%.2f%% [%.2f, %.2f]%s", z(pct), z(low), z(high), ifelse(p < 0.05, " *", ""))
}

# Wide table: one row per theme x outcome, one column per value of `col_var`
show_wide <- function(df, col_var) {
  df |>
    mutate(effect = fmt_eff(estimate_pct, conf.low_pct, conf.high_pct, p.value)) |>
    select(theme = theme_label, outcome, all_of(col_var), effect) |>
    pivot_wider(names_from = all_of(col_var), values_from = effect) |>
    arrange(outcome, theme) |>
    print(n = Inf, width = Inf)
}

# Estimates and CIs per theme, one colour per value of `col_var`
plot_compare <- function(df, col_var, title = NULL,
                         xlab = "% change per year per additional recommendation [95% CI]") {
  df |>
    ggplot(aes(x = estimate_pct, y = theme_label, colour = factor(.data[[col_var]]))) +
    geom_vline(xintercept = 0, linetype = 2, colour = "grey40") +
    geom_pointrange(aes(xmin = conf.low_pct, xmax = conf.high_pct),
                    position = position_dodge(width = 0.6), size = 0.25) +
    facet_wrap(~ outcome, scales = "free_x") +
    labs(x = xlab, y = NULL, colour = NULL, title = title) +
    theme_classic() +
    theme(legend.position = "bottom")
}


# UPR reviews ---------------------------------------------------------------------------

# One row per country x review (cycles 1-3): review year and recommendations per theme
sessions <- read_csv(here("data", "upr_sessions.csv"), show_col_types = FALSE)

recs_by_review <- sdg_data |>
  filter(cycle %in% c("Cycle 1", "Cycle 2", "Cycle 3"), !is.na(iso3)) |>
  left_join(sessions, join_by(upr_session_number == session)) |>
  group_by(iso3, cycle) |>
  summarise(review_year = min(year, na.rm = TRUE),
            across(all_of(vars_included), \(x) sum(x != "Other", na.rm = TRUE)),
            .groups = "drop") |>
  arrange(iso3, review_year)


# Country-level confounders (R2.6 / R3.3) ---------------------------------------------------
# const_health : constitution explicitly provides for health protection (right to health,
#                public health or medical care), any strength = 1; "No specific provision" = 0
#                (WORLD Policy Analysis Center, as of June 2024)
# cedaw_2005   : CEDAW ratified/acceded by 2005 (OHCHR export, an HTML table saved as .xls)
# icescr_2005  : ICESCR ratified/acceded by 2005 (OHCHR)
load_confounders <- function() {
  constitution <- readRDS(here("data", "API_data", "constitutions.rds")) |>
    transmute(iso3, const_yr_adopt,
              const_health = case_when(
                is.na(const_anyhealth) ~ NA_integer_,
                as.character(const_anyhealth) %in% c("No specific provision", "1") ~ 0L,
                .default = 1L))

  cedaw <- read_html(here("data", "Ratifications_CEDAW.xls")) |>
    html_element("table") |>
    html_table() |>
    select(1:5) |>
    set_names(c("country", "treaty_description", "treaty", "signature", "ratification")) |>
    filter(treaty == "CEDAW") |>
    transmute(iso3 = countrycode(country, "country.name", "iso3c", warn = FALSE,
                                 custom_match = c("Naoero" = "NRU")),
              cedaw_year = as.integer(str_extract(ratification, "\\d{4}")),
              cedaw_2005 = as.integer(!is.na(cedaw_year) & cedaw_year <= 2005))

  icescr <- read_excel(here("data", "UnderlyingData_ICESCR_OHCHR_23_04_2026.xls"), skip = 1) |>
    clean_names() |>
    transmute(iso3 = countrycode(country, "country.name", "iso3c", warn = FALSE),
              icescr_year = lubridate::year(date_of_ratification_accession),
              icescr_2005 = as.integer(!is.na(icescr_year) & icescr_year <= 2005)) |>
    filter(!is.na(iso3))

  constitution |>
    full_join(cedaw, by = "iso3") |>
    full_join(icescr, by = "iso3") |>
    filter(!is.na(iso3))
}
