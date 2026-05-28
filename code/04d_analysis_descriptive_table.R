table_vars <- c(
  # "MMR_2005",
  # HDI_2023, 
  # GDP_2023, 
  "WHO_region",
  "WBIncome_2005", 
  "WBIncome_2023", 
  # "FCS_status",
  # "FCS_2023",
  "FCS_count_cat",
  "government_effectiveness_baseline_cat",
  "political_stability_baseline_cat",
  "abortion_law",
  
  "mmr_cat5_2005",
  "UHC_RMNCH_cat_2005",
  # "UHC_RMNCH_cat_2023",
  "institutional_birth_cat",
  "skilled_birth_cat",
  "family_planning_cat",
  "anc4_cat"
  
  # "UHC_RMNCH",
  # "institutional_birth",
  # "skilled_birth",
  # "family_planning",
  # "anc4"
)

dat_table_minimal <- dat_table |> droplevels()
#   
# 
# 
# qqnorm(dat_table_minimal$MMR_log_change)
# qqline(dat_table_minimal$MMR_log_change)
# shapiro.test(dat_table_minimal$MMR_log_change)
# model1 <- lm(log(MMR_2023) ~ n_tot_maternal_health
#             + log(MMR_2005)
#             + log(GDP_2005)
#             + FCS_cat
#             # +literacy_long
#             , data = dat_table_minimal)
# anova(model1, model2)
# summary(model)
# shapiro.test(residuals(model))
# performance::check_model(model)
# dat_table_minimal |> 
#   ggplot(aes(x = log(MMR_2005), y = log(MMR_2023)))+
#   geom_point()+
#   geom_smooth()
# 
# qqnorm(dat_table_minimal$skilled_birth_change)
# qqline(dat_table_minimal$skilled_birth_change)
# shapiro.test(dat_table_minimal$skilled_birth_change)
# model <- lm(log(skilled_birth_max) ~ n_tot_maternal_health
#             + log(skilled_birth_min)
#             + log(GDP_2005)
#             + FCS_cat
#             , data = dat_table_minimal)
# summary(model)
# shapiro.test(residuals(model))
# performance::check_model(model)
# dat_table_minimal |> 
#   ggplot(aes(x = skilled_birth_min, y = skilled_birth_change))+
#   geom_point()+
#   geom_smooth()
# 
# 
# qqnorm(dat_table_minimal$CPR_any_change)
# qqline(dat_table_minimal$CPR_any_change)
# shapiro.test(dat_table_minimal$CPR_any_change)
# model <- lm(CPR_any_2023 ~ n_tot_maternal_health
#             + CPR_any_2005
#             # + log(CPR_any_2005)
#             + log(GDP_2005)
#             + FCS_cat
#             # +literacy_long
#             , data = dat_table_minimal)
# shapiro.test(residuals(model))
# performance::check_model(model)

t_mmr_baseline <- dat_table_minimal |> 
  select(
    MMR_2005,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = MMR_2005
  ) |> modify_header(list(
    "stat_0" ~ "**Baseline**"
  ));t_mmr_baseline #|> add_p(test = everything() ~ "kruskal.test")

t_mmr_change <- dat_table_minimal |> 
  select(
    MMR_change,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = MMR_change
  )  |> modify_header(list(
    "stat_0" ~ "**Change**"
  ))#|> add_p(test = everything() ~ "kruskal.test")

t_SBA_baseline <- dat_table_minimal |> 
  select(
    skilled_birth_min,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = skilled_birth_min
  ) |> modify_header(list(
    "stat_0" ~ "**Baseline**"
  ));t_SBA_baseline #|> add_p(test = everything() ~ "kruskal.test")

t_SBA_change <- dat_table_minimal |> 
  select(
    skilled_birth_change,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = skilled_birth_change
  )  |> modify_header(list(
    "stat_0" ~ "**Change**"
  )) #|> add_p(test = everything() ~ "kruskal.test")

t_CPR_baseline <- dat_table_minimal |> 
  select(
    CPR_any_2005,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = CPR_any_2005
  ) |> modify_header(list(
    "stat_0" ~ "**Baseline**"
  ))

t_CPR_change <- dat_table_minimal |> 
  select(
    CPR_any_change,
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_continuous(
    variable = CPR_any_change
  )  |> modify_header(list(
    "stat_0" ~ "**Change**"
  ))#|>  add_p(test = everything() ~ "kruskal.test")


t1 <- dat_table_minimal |> 
  select(
    starts_with("recs_median_"),
    all_of(table_vars)
  ) |> 
  select(-contains(vars_not_included, ignore.case = FALSE)) |> 
  mutate(overall_row = "Overall sample:") |> 
  relocate(overall_row) |> 
  tbl_summary(
    missing_text = "Missing", # how missing values should display
  )

t_all <- gtsummary::tbl_merge(tbls = list(t1,
                                          t_mmr_baseline,
                                          t_mmr_change,
                                          t_SBA_baseline,
                                          t_SBA_change,
                                          t_CPR_baseline,
                                          t_CPR_change
                                          )) |> 
  modify_spanning_header(list(
    c("stat_0_2", "stat_0_3"
      # , "p.value_2", "p.value_3", "p.value_4"
    ) ~ "**MMR**",
    c("stat_0_4", "stat_0_5"
      # , "p.value_2", "p.value_3", "p.value_4"
    ) ~ "**SBA**",
    c("stat_0_6", "stat_0_7"
      # , "p.value_2", "p.value_3", "p.value_4"
    ) ~ "**CPR**",
    # c("stat_0_2", "stat_0_3", "stat_0_4"
    #   # , "p.value_2", "p.value_3", "p.value_4"
    #   ) ~ "**Change scores, Median (Q1, Q3)**",
    "stat_0_1" ~ "**Overall**"
  )) |> 
  # modify_header(list(
  #   "stat_0_2" ~ "**MMR**",
  #   "stat_0_3" ~ "**SBA**",
  #   "stat_0_4" ~ "**CPR**"
  # )) |> 
  # modify_footnote_header(
  #   footnote = "Median (Q1, Q3)",
  #   columns = c("stat_0_2", "stat_0_3", "stat_0_4"),
  #   replace = TRUE
  # ) |>
  # modify_footnote_header(
  #   footnote = "Kruskal-Wallis rank sum test",
  #   columns = c("p.value_2", "p.value_3", "p.value_4"),
  #   replace = TRUE
  # ) |> 
  remove_footnote_header() |> 
  modify_footnote_spanning_header(footnote = "n (%) ; % may not add up to 100% due to rounding", 
                                  columns = c("stat_0_1")) |> 
  modify_footnote_spanning_header(footnote = "Maternal Mortality Ratio (maternal deaths per 100,000 live births), 2005 to 2023; Median (Q1, Q3)", 
                                  columns = c("stat_0_2", "stat_0_3")) |> 
  modify_footnote_spanning_header(footnote = "Skilled Birth Attendance (%), baseline and latest measure vary by country; Median (Q1, Q3)", 
                                  columns = c("stat_0_4", "stat_0_5")) |> 
  modify_footnote_spanning_header(footnote = "Contraceptive Prevalence Rate (%), 2005 to 2023; Median (Q1, Q3)", 
                                  columns = c("stat_0_6", "stat_0_7"))
  # modify_footnote_header(
  #   footnote = "Change in Maternal Mortality Ratio, 2005 to 2023; Median (Q1, Q3)",
  #   columns = c("stat_0_2"),
  #   replace = TRUE
  # ) |> 
  # modify_footnote_header(
  #   footnote = "Change in Skilled Birth Attendance, baseline to latest measure (data availability varies by country); Median (Q1, Q3)",
  #   columns = c("stat_0_3"),
  #   replace = TRUE
  # ) |> 
  # modify_footnote_header(
  #   footnote = "Change in Contraceptive Prevalence Rate, 2005 to 2023; Median (Q1, Q3)",
  #   columns = c("stat_0_4"),
  #   replace = TRUE
  # );t_all

# show_header_names(t_all)

first_theme <- t_all$table_body |> 
  # filter(!str_detect(variable, "abortion_maternal")) |> 
  filter(!(variable == "overall_row" & row_type == "label")) |> 
  # filter(!str_detect(variable, "abortion_maternal")) |> 
  rowid_to_column() |> 
  filter(str_detect(variable, "WHO_region")& row_type == "label") |> pull(rowid)

t_all$table_body <- t_all$table_body |>
  filter(!str_detect(variable, "abortion_maternal")) |> 
  filter(!(variable == "overall_row" & row_type == "label")) |> 
  mutate(row_type = case_when(variable == "overall_row" ~ "label", 
                              .default = row_type)) |> 
  mutate(stat_0_1 = case_when(variable == "overall_row" ~ NA, .default = stat_0_1)) |> 
  mutate(variable = str_remove(variable, "recs_median_")) |>
  left_join(theme_labels) |>
  left_join(clean_labels) |> 
  mutate(label = case_when(
    row_type != "label" ~ label,
    label == "recs_median_health_related" ~ "Health-related (any)",
    !is.na(theme_label) ~ theme_label,
    !is.na(clean_name) ~ clean_name,
    .default = label
  )) |> 
  add_row(label = "UPR\nRecommendations (n)", row_type = "label", .before = first_theme[1]) #|> 
  # add_row(label = "Contextual factors", row_type = "label", .before = first_theme[2]+1)


a <- t_all$table_body |>  
  rowid_to_column() |> 
  filter(row_type == "label")  ; groupnames <- (a$rowid)

table_all_final <- t_all |> 
  as_flex_table() |> 
  bg(i = groupnames, part = "body", bg = "#EFEFEF") |>
  bg(i = c(
    first_theme[1]
           # , first_theme[2]+1
    ), 
     part = "body", bg = "black") |>
  color(i = c(
    first_theme[1]
    # , first_theme[2]+1
    ), part = "body", color = "white") |> 
  bold(i = groupnames, j = 1, bold = TRUE, part = "body") |>
  vline(j = c(2, 4, 6), border = fp_border_default(color = "grey")) |>
  fontsize(size = 8, part = "all") |> 
  fontsize(size = 7, part = "footer") |>
  # fontsize(i =1, size = 10, part = "body") |> 
  padding(padding.top = 0.5, padding.bottom = 0.5, part = "body") |>
  padding(padding.top = 0.3, padding.bottom = 0.3, part = "footer") |>
  # add_footer_row(values = "'Fewer recommendations' indicates that a State received fewer than the median number received by all States for the given theme. 'More recommendations' indicates that a State received more than or equal to that median number.",
  #                colwidths = 6) |> 
  valign() |>
  width(j = 1, width = 3.8, unit = "cm") |>
  width(j = 2, width = 2, unit = "cm") |>
  width(j = c(3:4), width = 2.5, unit = "cm") |>
  # width(j = c(4,6,8), width = 2, unit = "cm") |> # p_values
  width(j = c(5), width = 1.9, unit = "cm") |> 
  width(j = c(6:8), width = 1.8, unit = "cm"); table_all_final

save_as_docx(
  "UPR recommendations by change scores for the outcomes of interest" = table_all_final, path = here("output", "publication_figures", paste0(format(Sys.time(), "%Y-%m-%d-%H%M_"),"Characteristics_change_scores.docx")))
