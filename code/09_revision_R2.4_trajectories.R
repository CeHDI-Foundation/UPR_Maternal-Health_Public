# =============================================================================
# 09_revision_R2.4_trajectories.R
#
# QUESTION  Reviewer 2, point 4: what do the models estimate for individual countries?
#           (model details and examples of trajectories)
#
# ANALYSIS  For six example countries: observed MMR, SBA and CPR (points) and the
#           trajectories fitted by the maternal-health models (lines).
#
#           Model (country i, year t, t = 0 in 2005):
#             MMR:      log(MMR_it) = b0 + b1 year_t + b2 n_i + b3 (year_t x n_i)
#                                     + g1 GDP_it + g2 FCS_i + u0i + u1i year_t + e_it
#             SBA, CPR: y_it ~ Beta(mu_it, phi), logit(mu_it) = same linear predictor (no e_it)
#           n_i = recommendations of the theme in cycles 1-3; (u0i, u1i) = country's own
#           level and slope; b3 = coefficient reported in the paper.
#
# OUTPUT    output/revision/09_trajectories_observed_vs_fitted.png
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

examples <- c("Nigeria", "Bangladesh", "Haiti", "Lesotho", "Cambodia", "Bolivia (Plurinational State of)")

traj <- imap_dfr(outcome_vars, \(ov, o) {
  d <- dat_main |>
    drop_na(all_of(c(ov, "GDP_long_c", "FCS_count_cat", "n_tot_Maternal_health"))) |>   # rows used by the model
    droplevels()
  m <- fit_mlm(formula_mv(ov, "n_tot_Maternal_health"), d)
  d |> transmute(outcome = o, country_name, YEAR, observed = .data[[ov]], fitted = fitted(m))
}) |>
  # back to the original scale: MMR per 100,000; SBA and CPR in %
  mutate(
    across(c(observed, fitted), \(x) if_else(outcome == "MMR", exp(x), 100 * x)),
    outcome = factor(outcome, levels = names(outcome_vars),
                     labels = c("MMR (per 100,000)", "SBA (%)", "CPR (%)"))
  )

fig_traj <- traj |>
  filter(country_name %in% examples) |>
  ggplot(aes(x = YEAR)) +
  geom_point(aes(y = observed), colour = "grey40", size = 1.2) +
  geom_line(aes(y = fitted), colour = "#0072B2", linewidth = 0.8) +
  facet_grid(outcome ~ country_name, scales = "free_y", switch = "y",
             labeller = labeller(country_name = label_wrap_gen(15))) +
  labs(x = NULL, y = NULL,
       caption = "Points: observed. Lines: fitted by the mixed-effects models (maternal-health recommendations).") +
  theme_classic() +
  theme(strip.placement = "outside", axis.text.x = element_text(angle = 45, hjust = 1))

print(fig_traj)
ggsave(file.path(out_dir, "09_trajectories_observed_vs_fitted.png"), fig_traj, width = 11, height = 7, dpi = 300)
