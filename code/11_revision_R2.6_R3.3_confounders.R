# =============================================================================
# 11_revision_R2.6_R3.3_confounders.R
#
# QUESTION  Reviewer 2, point 6 and Reviewer 3, point 3: could other factors or other human
#           rights pathways (treaty ratification, constitutional provisions) explain the association?
#
# ANALYSIS  Each country-level variable Z is allowed to change both the level and the SLOPE
#           of the outcome (year x Z), since only a variable linked to the rate of change can
#           confound the year x recommendations association:
#             outcome ~ year * n + Z + year:Z + GDP + FCS + (1 + year | country)
#           Z = constitutional provision for health, CEDAW by 2005, ICESCR by 2005, then all three.
#           All models use the same countries (complete data on the three variables).
#
# OUTPUT    output/revision/11_confounders_distribution.csv
#           output/revision/11_confounders.csv
# =============================================================================

library(here)
source(here("code", "06_revision_setup.R"))

confounders <- load_confounders()

dat_conf <- dat_main |>
  left_join(confounders |> select(iso3, const_health, const_yr_adopt, cedaw_year, cedaw_2005,
                                  icescr_year, icescr_2005),
            join_by(COUNTRY == iso3))

# Distribution in the analysed countries --------------------------------------------------------
country_conf <- dat_conf |>
  distinct(COUNTRY, country_name, const_health, const_yr_adopt, cedaw_year, cedaw_2005, icescr_year, icescr_2005)

conf_distribution <- country_conf |>
  summarise(across(c(const_health, cedaw_2005, icescr_2005),
                   list(yes = \(x) sum(x == 1, na.rm = TRUE), no = \(x) sum(x == 0, na.rm = TRUE),
                        missing = \(x) sum(is.na(x))))) |>
  pivot_longer(everything(), names_to = c("variable", ".value"), names_pattern = "(.*)_(yes|no|missing)$")

print(conf_distribution)
write_csv(conf_distribution, file.path(out_dir, "11_confounders_distribution.csv"))

# Countries not ratified by 2005; constitutions adopted after the UPR started
country_conf |> filter(cedaw_2005 == 0)  |> select(country_name, cedaw_year)
country_conf |> filter(icescr_2005 == 0) |> select(country_name, icescr_year) |> arrange(icescr_year)
country_conf |> count(const_health, adopted_2008_or_later = const_yr_adopt >= 2008)

# Models -------------------------------------------------------------------------------------------
conf_specs <- tribble(
  ~analysis,                           ~extra,
  "Main model",                        "",
  "+ constitution",                    " + year*const_health",
  "+ CEDAW",                           " + year*cedaw_2005",
  "+ ICESCR",                          " + year*icescr_2005",
  "+ all three",                       " + year*const_health + year*cedaw_2005 + year*icescr_2005"
)

dat_conf_cc <- dat_conf |> drop_na(const_health, cedaw_2005, icescr_2005)   # same countries for all

conf_results <- conf_specs |>
  mutate(res = map(extra, \(ex) run_over_themes(
    dat_conf_cc, make_formula = \(o, th) formula_mv(o, th, extra = ex), themes = themes_sens
  ))) |>
  select(analysis, res) |>
  unnest(res) |>
  mutate(analysis = factor(analysis, levels = conf_specs$analysis))

write_csv(conf_results, file.path(out_dir, "11_confounders.csv"))

show_wide(conf_results, "analysis")
plot_compare(conf_results, "analysis")
