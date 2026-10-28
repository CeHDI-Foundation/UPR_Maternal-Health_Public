# =============================================================================
# 10_revision_R2.5_nonlinearity.R
#
# QUESTION  Reviewer 2, point 5: a linear model may not reflect the actual trajectories
#           (e.g. gradual, then accelerated improvement); spline-based models could be considered.
#
# ANALYSIS  1. Trajectories on the model scale (log MMR, logit SBA/CPR): straight line vs free curve.
#           2. Residuals of the maternal-health models over time (a U or arch shape = missed curve).
#           3. Common trend modelled as a natural cubic spline of year (3 df, knots ~2011 and 2017),
#              keeping the year x recommendations term unchanged; fit compared by AIC.
#           4. Main model without 2020-2021 (COVID-19 spike in MMR).
#
# OUTPUT    output/revision/10_trajectories_model_scale.png
#           output/revision/10_residuals.png
#           output/revision/10_nonlinearity.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

# 1. Trajectories on the model scale -----------------------------------------------------------
p_traj <- dat_main |>
  transmute(country_name, YEAR,
            `MMR (log)`   = MMR_long_log,
            `SBA (logit)` = qlogis(skilled_birth_long_p_squeezed),
            `CPR (logit)` = qlogis(CPR_any_long_p)) |>
  pivot_longer(-c(country_name, YEAR), names_to = "outcome", values_to = "value") |>
  drop_na(value) |>
  mutate(outcome = factor(outcome, levels = c("MMR (log)", "SBA (logit)", "CPR (logit)"))) |>
  ggplot(aes(YEAR, value)) +
  geom_line(aes(group = country_name), colour = "grey70", alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, colour = "#D55E00", linewidth = 1) +              # free curve
  geom_smooth(method = "lm", se = FALSE, colour = "#0072B2", linetype = 2, linewidth = 1) +   # straight line
  facet_wrap(~ outcome, scales = "free_y") +
  labs(x = NULL, y = "Value on the model scale") +
  theme_classic()

print(p_traj)
ggsave(file.path(out_dir, "10_trajectories_model_scale.png"), p_traj, width = 10, height = 4, dpi = 300)

# 2. Residuals over time ------------------------------------------------------------------------
resid_dat <- imap_dfr(outcome_vars, \(ov, o) {
  m <- fit_mlm(formula_mv(ov, "n_tot_Maternal_health"), dat_main)
  tibble(outcome = o, YEAR = model.frame(m)$year + 2005, resid = residuals(m, type = "pearson"))
})

p_resid <- resid_dat |>
  mutate(outcome = factor(outcome, levels = names(outcome_vars))) |>
  ggplot(aes(YEAR, resid)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_point(alpha = 0.15, size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, colour = "#D55E00") +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(x = NULL, y = "Pearson residual") +
  theme_classic()

print(p_resid)
ggsave(file.path(out_dir, "10_residuals.png"), p_resid, width = 10, height = 4, dpi = 300)

# 3. Curved common trend and 4. without 2020-2021 -----------------------------------------------
formula_ns <- function(outcome_var, theme) {
  as.formula(paste0(outcome_var, " ~ ns(year, df = 3) + ", theme, " + year:", theme,
                    " + (1 + year | country_name) + GDP_long_c + FCS_count_cat"))
}

r25 <- bind_rows(
  run_over_themes(dat_main, themes = themes_sens) |> mutate(analysis = "Main (straight line)"),
  run_over_themes(dat_main, make_formula = formula_ns, themes = themes_sens) |> mutate(analysis = "Curved trend (spline)"),
  run_over_themes(dat_main |> filter(!YEAR %in% 2020:2021), themes = themes_sens) |> mutate(analysis = "Excluding 2020-2021")
)

write_csv(r25, file.path(out_dir, "10_nonlinearity.csv"))

show_wide(r25, "analysis")
plot_compare(r25, "analysis")

# Model fit (lower AIC = better)
r25 |>
  filter(analysis != "Excluding 2020-2021") |>
  select(theme_label, outcome, analysis, aic) |>
  pivot_wider(names_from = analysis, values_from = aic) |>
  mutate(difference = `Curved trend (spline)` - `Main (straight line)`) |>
  arrange(outcome) |>
  print(n = Inf, width = Inf)
