pacman::p_load(
  tidyverse,
  broom,
  here,
  scales,
  lme4,
  glmmTMB, # Beta regression is specifically designed to handle bounded continuous data (fractions strictly between 0 and 1)
  lmerTest,
  report,
  patchwork
)

# Load in the pre-processed datatables
source(here("code", "03b_prep_read_datatable.R"))

# Run the model for Maternal health recommendations
MLM_CPR_MH <- glmmTMB::glmmTMB(
  CPR_any_long_p ~ 
    # `year` variable is centered at 2005 = year 0 (same for all countries)
    year*n_tot_Maternal_health_c +
    
    # `year_upr` variable is centered to year of first UPR session = 0 (varies by country)
    # year_upr*n_tot_Maternal_health_c + # Single linear slope
    
    # We considered a segmented analysis with a single knot at year_upr = 0 (pre-upr trends and post-review trends)
    # These models offer more robust analyses (especially as they include pre-baseline trajectories on a separate slope, but for now the simpler model was deemed more digestible for policy-makers)
    # ns( # Natural cubic spline was considered, but model fit was better with a linear spline (and also easier to interpret)
    # lspline( # Linear spline
    #   year_upr, knots = c(0)
    # )*n_tot_Maternal_health_c +
    
    # Covariates
    GDP_long_c +
    FCS_count_cat +
    # government_effectiveness +
    # political_stability +
    
    (1 + year | country_name),
  # (1 + year_upr | country_name),
  
  # (1 +
  #    # ns(
  #    lspline(
  #      year_upr, knots = c(0)) | country_name),
  data = dat_table_long |> 
    filter (year >= 0) |> # use this when using the `year` variable
    # filter(year_upr >= -8) |> # use this when using the `year_upr` variable
    mutate(
      n_tot_Maternal_health_c = n_tot_Maternal_health-
        theme_medians |> pull(eval("n_tot_Maternal_health"))
    ) |> 
    droplevels()
  , REML = FALSE
  # , control = ctrl
  , family = beta_family(link = "logit")
)

summary(MLM_CPR_MH)
# paste0(round((exp(0.0021439 )-1)*100,2),"%") # calculate the % change from raw estimates
performance::check_model(MLM_CPR_MH, verbose = TRUE)

# Visualise the model predictions ----
p_cpr <- plot_predictions(
  MLM_CPR_MH, 
  condition = list(
    # Using a sequence ensures the lines are perfectly drawn 
    year = seq(0,18, by=1),
    # year_upr = seq(-8, 15, by = 1),
    n_tot_Maternal_health_c = c(-2, 2)
  ),
  re.form = NA, # Still works perfectly with glmmTMB to get population averages
  vcov = TRUE,
  draw = FALSE
) |> 
  mutate(n_tot_Maternal_health = factor(as.numeric(as.character(n_tot_Maternal_health_c)) + 
                                          theme_medians |> pull(eval("n_tot_Maternal_health"))))

plot_cpr <- p_cpr |> 
  ggplot(aes(
    x = year+2005,
    # x = year_upr, 
    y = estimate)) +
  
  # Confidence intervals
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = n_tot_Maternal_health), 
              alpha = 0.15, color = NA) +
  
  # Trajectory lines
  geom_line(aes(color = n_tot_Maternal_health), linewidth = 1.2) +
  
  # Automatically format the 0-1 proportions into percentages (e.g., 0.80 -> 80%)
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(
    # breaks = c(-8, -5, 0, 5, 10, 15), 
                     expand = c(0,0)) +
  labs(
    title = "Modelled trajectories of Contraceptive Prevalence Rate",
    y = "Estimated CPR (%)",
    # x = "Years since a country's first UPR Working Group review",
    x = NULL,
    color = "UPR recommendations\nrelated to maternal health",
    fill = "UPR recommendations\nrelated to maternal health"
  ) +
  # geom_vline(xintercept = 0, linetype = 2, alpha = 0.4) +
  
  theme_classic() +
  theme(
    # NOTE: Because Skilled Birth Attendance generally goes UP over time, 
    # the lines will likely end in the top-right corner. 
    # I moved the legend to the bottom-right so it doesn't block the data.
    legend.position = c(0.95, 0.05), 
    legend.justification = c("right", "bottom"),
    legend.background = element_blank()
  ) +
  # Optional: Explicit colorblind-friendly colors
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_fill_manual(values = c("#D55E00", "#0072B2"));plot_cpr

ggsave(plot = plot_cpr, "spline_plot_CPR.svg", width = 6.5, height =5)

plot_3 <- (plot_mmr+
             labs(title = "(a) Modelled trajectories of MMR")+
             theme(axis.title.x = element_blank(), 
                   title = element_text(size = 9),
                   axis.title.y = element_text(size = 11))) / (
               plot_sba+
                 labs(title = "(b) Modelled trajectories of Skilled Birth Attendance")+
                 theme(legend.position = "none", 
                       axis.title.x = element_blank(),
                       title = element_text(size = 9),
                       axis.title.y = element_text(size = 11))) / (
                         plot_cpr+             
                           labs(title = "(c) Modelled trajectories of Contraceptive Prevalence Rate")+
                           theme(legend.position = "none",
                                 title = element_text(size = 9),
                                 axis.title.y = element_text(size = 11),
                                 axis.title.x = element_text(size = 10)))
ggsave(plot = plot_3, "plot_3.png", width = 4.6, height = 10, dpi = 500)

# 1. Prepare Data & Run Regressions ---------------------------------------
outcome_var <- "CPR_any_long_p"

forest_data_MLM_CPR <- map_dfr(theme_vars, function(theme) {
  
  dat <- dat_table_long |> 
    filter(year >= 0) |> 
    # filter(year_upr >= -8) |> 
    droplevels()
  formula_uv <- as.formula(paste0(outcome_var, 
                                  " ~ year*",
                                  # "~ lspline(year_upr, knots = c(0))*",
                                  theme,
                                  " + (1 + year |country_name)"))
                                  # " + (1 + lspline(year_upr, knots = c(0)) |country_name)"))
  
  formula_mv <- as.formula(paste0(outcome_var, 
                                  " ~ year*",
                                  # "~ lspline(year_upr, knots = c(0))*",
                                  theme,
                                  " + (1 + year |country_name)"
                                  # " + (1 + lspline(year_upr, knots = c(0)) |country_name)"
                                  
                                  # covariates
                                  , "+ GDP_long_c"
                                  , "+ FCS_count_cat"
                                  # , "+ government_effectiveness"
                                  # , "+ political_stability"
  ))
  
  # Run regression
  model_uv <- glmmTMB(
    formula_uv, 
    data = dat
    , REML = FALSE
    , family = beta_family(link = "logit")
  )
  
  model_mv <- glmmTMB(
    formula_mv, 
    data = dat
    , REML = FALSE
    , family = beta_family(link = "logit")
  )
  
  term_text = "year:"
  # term_text = "lspline(year_upr, knots = c(0))2:"
  
  # Extract stats
  tidy_uv <- broom.mixed::tidy(model_uv, conf.int = TRUE
  ) %>%
    filter(
      term == paste0(term_text,theme)
    ) |> 
    mutate(
      term2 = term,
      term = str_remove(term, fixed(paste0(term_text, prefix))), #%>% str_replace_all("_", " ") %>% str_to_title(),
      is_significant = p.value < 0.05,
      p_symbol = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01 ~ "**",
                           p.value < 0.05 ~ "*",
                           .default = ""),
      # TRANSFORM TO PERCENT CHANGE
      estimate_pct = (exp(estimate) - 1) * 100,
      conf.low_pct = (exp(conf.low) - 1) * 100,
      conf.high_pct = (exp(conf.high) - 1) * 100
    ) |> 
    left_join(theme_labels, join_by(term == variable)) |> 
    mutate(theme_label = case_when(term == "health_related" ~ "Health-related (overall)", .default = theme_label)) |> 
    select(-term) |>
    rename(term = theme_label) |>
    mutate(
      label_text = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high),
      label_text_p = sprintf("%.2f [%.2f, %.2f]", estimate_pct, conf.low_pct, conf.high_pct)
    ) |> 
    mutate(model = "Univariable")
  
  tidy_mv <- broom.mixed::tidy(model_mv, conf.int = TRUE
  ) %>%
    filter(term == paste0(term_text,theme)) %>%
    mutate(
      term2 = term,
      term = str_remove(term, fixed(paste0(term_text, prefix))), #%>% str_replace_all("_", " ") %>% str_to_title(),
      is_significant = p.value < 0.05,
      p_symbol = case_when(p.value < 0.001 ~ "***",
                           p.value < 0.01 ~ "**",
                           p.value < 0.05 ~ "*",
                           .default = ""),
      # TRANSFORM TO PERCENT CHANGE
      estimate_pct = (exp(estimate) - 1) * 100,
      conf.low_pct = (exp(conf.low) - 1) * 100,
      conf.high_pct = (exp(conf.high) - 1) * 100
    ) |> 
    left_join(theme_labels, join_by(term == variable)) |> 
    mutate(theme_label = case_when(term == "health_related" ~ "Health-related (overall)", .default = theme_label)) |> 
    select(-term) |>
    rename(term = theme_label) |> 
    mutate(
      label_text = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high),
      label_text_p = sprintf("%.2f [%.2f, %.2f]", estimate_pct, conf.low_pct, conf.high_pct)
    ) |> 
    mutate(model = "Multivariable")
  
  estimates_uv <- tidy_uv |> 
    select(term, label_text, label_text_p) |> 
    rename(label_text_uv = label_text,
           label_text_uv_p = label_text_p)
  estimates_mv <- tidy_mv |> 
    select(term, label_text, label_text_p) |> 
    rename(label_text_mv = label_text,
           label_text_mv_p = label_text_p)
  
  bind_rows(tidy_uv, tidy_mv) |> 
    mutate(model= fct_relevel(model, "Univariable")) |> 
    left_join(estimates_uv) |> 
    left_join(estimates_mv)
  
})

# 2. Format Data for Plotting ---------------------------------------------
forest_plot_data_MLM_CPR <- forest_data_MLM_CPR %>%
  # arrange(fct_rev(model), estimate) %>% # Sort by coefficient (most negative at top)
  mutate(
    outcome = outcome_var,
    # term = fct_inorder(term),
    
    # Define colors
    color_code = ifelse(is_significant, "#D55E00", "grey60"),
    sig_label = ifelse(is_significant, "Significant", "Not Significant")
  )

# Calculate range to determine where to place the text column
max_ci <- max(forest_plot_data_MLM_CPR$conf.high_pct)
min_ci <- min(forest_plot_data_MLM_CPR$conf.low_pct)
range_ci <- max_ci - min_ci

# Position text column at 15% past the max CI value
text_position <- max_ci + (range_ci * 0.05)
text_position_2 <- max_ci + (range_ci * 0.3)

# 3. Generate Enhanced Forest Plot ----------------------------------------
p_forest_enhanced_CPR_MLM <- forest_plot_data_MLM_CPR |> 
  # filter(model == "Univariable") |> 
  mutate(model = fct_rev(model)) |> 
  ggplot(aes(y = term, color = is_significant, group = model)) +
  
  # Add Vertical Line at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30", alpha = 1) +
  
  # Add Error Bars & Points
  geom_errorbarh(aes(
    # xmin = conf.low, xmax = conf.high, 
    xmin = conf.low_pct, xmax = conf.high_pct, 
    color = sig_label
    # , linetype = model
  ),
  height = 0.3, size = 0.5
  , position = position_dodge(width = 0.5)
  ) +
  geom_point(aes(
    # x = estimate, 
    x = estimate_pct, 
    color = sig_label, shape = model), size = 1.6
    , position = position_dodge(width = 0.5)
  ) +
  
  # # Add Text Column (Estimate [95% CI])
  # geom_text(aes(x = text_position, label = label_text_uv),
  #           hjust = 0, size = 3.5, fontface = "plain", color = "black") +
  # 
  # # Add Column Header
  # annotate("text", x = text_position, y = length(theme_vars) + 1.2,
  #          label = "     Univariable\nestimate [95% CI]", fontface = "bold", hjust = 0, size = 3.5) +
  # 
  # # # Add Text Column (Estimate [95% CI])
  # geom_text(aes(x = text_position_2, label = label_text_mv),
  #           hjust = 0, size = 3.5, fontface = "plain", color = "black") +
  # 
  # # Add Column Header
  # annotate("text", x = text_position_2, y = length(theme_vars) + 1.2,
  #          label = "  Multivariable\nestimate [95% CI]", fontface = "bold", hjust = 0, size = 3.5) +
  
  # Colors
  scale_color_manual(values = c("Not Significant" = "grey60", "Significant" = "#D55E00")) +
  scale_shape_manual(values = c("Univariable" = 16, "Multivariable" = 15))+
  guides(color = "none")+
  
  # Scales
  scale_x_continuous(
    expand = expansion(mult = c(0.01, 0.01)) # Expand right side to fit text
  ) +
  # 
  # # Scales
  # scale_y_discrete(
  #   expand = expansion(mult = c(0.02, 0.1)) # Expand right side to fit text
  # ) +
  
  coord_cartesian(
    clip = "off")+
  
  # Labels
  labs(
    title = "% change in odds of CPR,\nper additional recommendation"
    # title = "Association between # of recommendations and change in MMR (2005-2023)\n"
    # \nCoefficients indicate the log-change in MMR for each additional recommendation\n(Countries with a baseline MMR < 50 were excluded from this analysis)
    ,
    # subtitle = "Regression Coefficients (controlling for Baseline MMR & GDP)",
    # x = "Regression coefficients (multivariable models control for MMR in 2005, GDP in 2005,\nand FCS status in 2025)",
    x = "Regression coefficients",
    # x = "Regression coefficients (multivariable models control for\nGDP in 2005 and FCS status in 2025)",
    # x = "Regression Coefficients (univariable models)",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  # Theme
  theme_classic()+
  guides(shape=guide_legend(reverse=T))+
  theme(
    plot.title = element_text(
      # face = "bold", 
      # size = 11, 
      vjust = 0.5,
      hjust = 0.5
    ),
    # plot.title = element_blank(),
    panel.grid = element_blank(), 
    plot.title.position = "plot",
    axis.text.y = element_text(size = 10, color = "black"),
    # legend.position = "top",
    # legend.position = "none",
    legend.position = "inside",
    legend.position.inside = c(0.01, 0.98), legend.justification = c(0,1),
    legend.text = element_text(size = 8),
    legend.background = element_rect(color = "grey50", fill = "white"),
    legend.key.spacing = unit(0.01, "cm"),
    legend.margin = margin(0.5,1,1,1),
    # legend.spacing.x = unit(0.01, "cm"),
    panel.grid.major.x = element_line(linetype = "dotted", color = "grey80"),
    # Remove right border to make text look like a table extension
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# 4. Save -----------------------------------------------------------------
print(p_forest_enhanced_CPR_MLM)
# Univariable
annot1 <- forest_plot_data_MLM_CPR |> 
  filter(model == "Univariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  # Add Text Column (Estimate [95% CI])
  geom_text(aes(x = 0, label = paste0(label_text_uv_p, p_symbol)),
            hjust = 0.5, size = 3.5, fontface = "plain") +
  labs(title = "Univariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )#;annot1

# Multivariable
annot2 <-  forest_plot_data_MLM_CPR |> 
  filter(model == "Multivariable") |> 
  ggplot(aes(y = term, color = sig_label))+
  geom_text(aes(x = 0, label = paste0(label_text_mv_p, p_symbol)),
            hjust = 0.5, size = 3.5, fontface = "plain")+
  labs(title = "Multivariable\nEstimate [95% CI]")+
  scale_x_continuous(
    expand = expansion(mult = c(0, 0)) # Expand right side to fit text
  ) +
  scale_color_manual(values = c("Not Significant" = "grey0", "Significant" = "#D55E00")) +
  coord_cartesian(
    clip = "off")+
  theme_void()+
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )#;annot2

p_forest_enhanced_CPR_MLM+annot1+annot2+plot_layout(widths = c(1.7,1.3,1.3))

ggsave(here("output", "publication_figures", "forest_plot_recs_by_CPR_MLM.svg"), 
       # plot = p_forest_enhanced_CPR_MLM, 
       width = 8, height = 7, dpi = 300)