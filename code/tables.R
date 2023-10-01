library(tidyverse)
library(gt)
library(gtExtras)
library(glue)
library(gtsummary)


# table 1 -----------------------------------------------------------------

tbl_signal_features <-
  tibble(
    Name = c("Lying", "Activity", "Time", "Thigh-SDacc", "Thigh-Inclination", "Hip-SDacc", "Hip-Inclination"),
    Description = c(
      "Posture based on thigh and back",
      "Activity type classification",
      "Time categorized into four-hour windows",
      "Thigh longitudinal acceleration SD",
      "Thigh device inclination angle",
      "Hip longitudinal acceleration SD",
      "Hip device inclination angle"
    ),
    Values = c(
      "1: lying, -1: not lying",
      "1: Standing, moving, 0: Sitting, -1: Other",
      "[−1,0.2) with step size Δ=0.2",
      "-1: No movement",
      "Range: −180 to 180 degrees",
      "-1: No movement",
      "Range: −180 to 180 degrees"
    )
  ) %>%
  gt() %>%
  # tab_header(
  #   title = "Signal features for the detection of in-bed periods in Audacity."
  # ) %>%
  cols_label(
    Name = "Feature",
    Description = "Description",
    Values = "Values"
  )


# table 2 -----------------------------------------------------------------

tbl_man_describe <-
  tibble(
    Population = c(rep("Children", 3), rep("Adults", 7)),
    Characteristic = c(
      "N", "Gender (% female)", "Age (years)",
      "N", "Gender (% female)", "Age (years)", "ISCE",
      "0–3 (%)", "4–6 (%)", "7–8 (%)"
    ),
    Value = c(
      "14", "28.6", "9 (7–10)",
      "19", "57.9", "42 (39–46)", "",
      "36.8", "47.4", "15.8"
    )
  ) %>%
  gt(groupname_col = "Population") %>%
  cols_label(Value = "")
# table 3 -----------------------------------------------------------------

tbl_icc_zm_man <-
  tibble(
    Action = c(rep("To bed", 4), rep("Out of bed", 4)),
    Timepoint = c(
      rep("Baseline (n = 94 Nights)", 2), rep("Follow-Up (n = 54 Nights)", 2),
      rep("Baseline (n = 94 Nights)", 2), rep("Follow-Up (n = 54 Nights)", 2)
    ),
    Round = c(
      "Round 1", "Round 2", "Round 1", "Round 2",
      "Round 1", "Round 2", "Round 1", "Round 2"
    ),
    ICC = c(
      "0.98", "0.98", "0.96", "0.95",
      "0.98", "0.98", "0.98", "0.97"
    ),
    `95% CI` = c(
      "(0.98; 0.99)", "(0.96; 0.98)", "(0.94; 0.98)", "(0.92; 0.97)",
      "(0.97; 0.99)", "(0.96; 0.98)", "(0.97; 0.99)", "(0.95; 0.98)"
    )
  ) %>%
  unite("ICC (95% CI)", c(ICC, "95% CI"), sep = " ", remove = TRUE) %>% 
  pivot_wider(names_from = c("Timepoint", "Round"), values_from = "ICC (95% CI)") %>% 
  gt() %>% 
  tab_spanner(label = "Baseline (N = 94)", columns = 2:3) %>%
  tab_spanner(label = "Followup (N = 54)", columns = 4:5) %>% 
  # tab_spanner(label = "Round 1", columns = c(2, 4)) %>%
  # tab_spanner(label = "Round 2", columns = c(3, 5)) %>%
  cols_label(Action = "",
             "Baseline (n = 94 Nights)_Round 1" = "Round 1",
             "Baseline (n = 94 Nights)_Round 2" = "Round 2",
             "Follow-Up (n = 54 Nights)_Round 1" = "Round 1",
             "Follow-Up (n = 54 Nights)_Round 2" = "Round 2")


# table 4 -----------------------------------------------------------------

tbl_icc_self_zm <-
  tibble(
    Measurement = c("To bed", "Out of bed"),
    "Baseline (N = 94)" = c("0.98 (0.98; 0.99)", "0.98 (0.97; 0.99)"),
    "Followup (N = 54)" = c("0.96 (0.94; 0.98)", "0.98 (0.96; 0.99)")
  ) %>%
  gt() %>%
  cols_label(Measurement = "") 
  # tab_spanner(label = "Baseline (N = 94)", columns = 2) %>%
  # tab_spanner(label = "Followup (N = 54)", columns = 3)


# table 5 -----------------------------------------------------------------

tbl_icc_man_man <-
  tibble(
    Metric = rep(c("To bed", "Out of bed"), times = 4),
    Phase = c(rep("Baseline (N = 110)", times = 4), rep("Follow-Up (N = 62)", times = 4)),
    Round = rep(rep(c("Round 1", "Round 2"), each = 2), times = 2),
    "ICC (95% CI)" = c(
      "0.91 (0.88; 0.94)", "0.93 (0.9; 0.95)", "0.92 (0.89; 0.94)", "0.97 (0.96; 0.98)",
      "0.94 (0.9; 0.96)", "0.97 (0.96; 0.98)", "0.97 (0.95; 0.98)", "0.98 (0.98; 0.99)"
    )
  ) %>%
  pivot_wider(names_from = c("Phase", "Metric"), values_from = "ICC (95% CI)") %>% 
  gt() %>%
  tab_spanner(label = "Baseline (N = 110)", columns = 2:3) %>%
  tab_spanner(label = "Followup (N = 62)", columns = 4:5) %>% 
  cols_label(Round = "",
             "Baseline (N = 110)_To bed" = "To Bed",
             "Baseline (N = 110)_Out of bed" = "Out of Bed",
             "Follow-Up (N = 62)_To bed" = "To Bed",
             "Follow-Up (N = 62)_Out of bed" = "Out of Bed")


# table 6 -----------------------------------------------------------------

tbl_icc_test_retest <-
  tibble(
    Rater = rep(c("Rater 1", "Rater 2", "Rater 3"), each = 4),
    Phase = rep(rep(c("Baseline", "Follow-Up"), each = 2), 3),
    Metric = rep(c("To Bed", "Out of Bed"), 6),
    ICC = c(
      0.91, 0.98, 0.96, 1.00,
      0.97, 0.91, 0.91, 0.99,
      0.91, 0.96, 0.98, 0.98
    ),
    CI_Lower = c(
      0.87, 0.98, 0.94, 0.99,
      0.96, 0.87, 0.86, 0.98,
      0.87, 0.94, 0.97, 0.97
    ),
    CI_Upper = c(
      0.94, 0.99, 0.98, 1.00,
      0.98, 0.94, 0.95, 0.99,
      0.94, 0.97, 0.99, 0.99
    )
  ) %>%
  mutate(
    ICC = glue::glue("{ICC} ({CI_Lower}; {CI_Upper})")
  ) %>%
  select(-CI_Lower, -CI_Upper) %>%
  pivot_wider(names_from = c(Metric, Phase), values_from = ICC) %>%
  gt(groupname_col = c("")) %>%
  cols_label(
    Rater = "",
    "To Bed_Baseline" = "To Bed",
    "Out of Bed_Baseline" = "Out of Bed",
    "To Bed_Follow-Up" = "To Bed",
    "Out of Bed_Follow-Up" = "Out of Bed"
  ) %>%
  tab_spanner(label = "Baseline (N = 110)", columns = 2:3) %>%
  tab_spanner(label = "Followup (N = 62)", columns = 4:5)


# table 7 -----------------------------------------------------------------

tbl_7 <-
  tibble(
    Method = rep(c("Manual, round 1", "Manual, round 2", "Self-report"), 4),
    Phase_Metric = rep(c("Baseline, to bed", "Baseline, out of bed", "Follow-up, to bed", "Follow-up, out of bed"), each = 3),
    n = c(rep(94, 6), rep(54, 6)),
    Bias = c(3.02, 0.48, 1.23, 0.53, 0.98, -2.79, -6.08, -0.4, 0.77, 4.95, 2.57, 0.56),
    CI_Lower_Bias = c(-0.44, -2.42, -1.57, -2.34, -1.47, -5.26, -11.34, -5.3, -4.08, 0.65, -0.76, -3.62),
    CI_Upper_Bias = c(6.47, 3.39, 4.03, 3.4, 3.43, -0.32, -0.83, 4.51, 5.62, 9.25, 5.89, 4.74),
    Upper_LOA = c(-30.04, -27.3, -25.56, -26.9, -22.49, -26.45, -43.81, -35.6, -34.06, -25.95, -21.3, -29.45),
    CI_Lower_Upper_LOA = c(-35.96, -32.28, -30.37, -31.82, -26.7, -30.69, -52.84, -44.03, -42.4, -33.35, -27.02, -36.64),
    CI_Upper_Upper_LOA = c(-24.12, -22.32, -20.76, -21.99, -18.28, -22.21, -34.77, -27.17, -25.72, -18.55, -15.59, -22.26),
    Lower_LOA = c(36.07, 28.27, 28.02, 27.96, 24.45, 20.87, 31.64, 34.8, 35.59, 35.85, 26.44, 30.57),
    CI_Lower_Lower_LOA = c(30.15, 23.29, 23.21, 23.05, 20.24, 16.63, 22.61, 26.37, 27.25, 28.45, 20.72, 23.39),
    CI_Upper_Lower_LOA = c(42, 33.24, 32.82, 32.88, 28.66, 25.11, 40.67, 43.23, 43.93, 43.25, 32.15, 37.76)
  ) %>%
  mutate(
    Bias = glue("{Bias} ({CI_Lower_Bias}; {CI_Upper_Bias})"),
    Lower_LOA = glue("{Lower_LOA} ({CI_Lower_Lower_LOA}; {CI_Upper_Lower_LOA})"),
    Upper_LOA = glue("{Upper_LOA} ({CI_Lower_Upper_LOA};{CI_Upper_Upper_LOA})"),
    Phase_Metric = glue("{Phase_Metric}, n = {n}")
  ) %>%
  select(Method, Phase_Metric, Bias, Lower_LOA, Upper_LOA) %>%
  gt(groupname_col = "Phase_Metric") %>% 
  cols_label(
    Bias = "Bias  (95% CI)",
    Lower_LOA = "Lower LOA (95% CI)",
    Upper_LOA = "Upper LOA (95% CI)"
  )


# table 8 -----------------------------------------------------------------

tbl_8 <- 
  tibble(
  Predictor = c("Weekday", "time_day", "Location", "macc_x", "macc_y", "macc_z", "sdacc_x", "sdacc_y", "sdacc_z", "Sdmax", "Incl", "Temp"),
  Description = c(
    "Day of week ([1:7])",
    "Time of day (milliseconds)",
    "Device wear location: 0 = thigh, 1 = hip",
    "Moving average of the z axis acceleration",
    "Moving average of the y axis acceleration",
    "Moving average of the z axis acceleration",
    "Moving average of the standard deviation on the x axis acceleration",
    "Moving average of the standard deviation of the y axis acceleration",
    "Moving average of the standard deviation of the z axis acceleration",
    "Maximum standard deviation",
    "Inclination angle of the device in relation to the direction of the gravitational force",
    "Surface skin temperature (degrees Celsius)"
  )
) %>% 
  gt()


# table 9 -----------------------------------------------------------------

nw_thigh_hip <- read_rds("data/nw_episodes_thigh_hip.rds")
nw_wrist <- read_rds("data/nw_episodes_wrist.rds")

tbl_9 <-
  nw_thigh_hip %>%
  bind_rows(nw_wrist) %>%
  mutate(
    n = (n * 10),
    duration = time_length(n, unit = "minute"),
    group_duration = if_else(duration >= 60, "\u226560 minutes", "<60 minutes")
  ) %>%
  group_by(location, group_duration) %>%
  summarise(
    mean = mean(duration),
    duration_total = sum(duration) / 60 
  ) %>%
  mutate(duration_prop = round((duration_total / sum(duration_total)) * 100, digits = 1),
         across(mean:duration_total, ~round(.x, digits = 0))) %>% 
  group_by(group_duration) %>%
  gt() %>%
  # fmt_number(columns = mean:duration_total, decimals = 0) %>%
  # fmt_percent(duration_prop, decimals = 1) %>%
  # tab_header(
  #   title = "Overview of non-wear episodes",
  #   subtitle = "Grouped in short and long non-wear episodes"
  # ) %>%
  # tab_footnote(
  #   footnote = "Aggregated in minutes",
  #   locations = cells_column_labels(columns = 3:4)
  # ) %>%
  # tab_footnote(footnote = "Proportion of total non-wear time by wear location",
  #              locations = cells_column_labels(5)) %>% 
  cols_label(
    location = "Wear location",
    mean = "Mean (min)",
    duration_total = "Cumulated (hrs)",
    duration_prop = "Proportion (%)") 


# table 10 ----------------------------------------------------------------

#!/usr/bin/env Rscript

stats <-
  read_csv("data/tbl_overview_sleep_summaries.csv")

# Create a formatted table to display the metrics, grouped by epoch length
tbl_10 <-
  stats %>% 
  # pivot_wider(names_from = row, values_from = raw_stat:median_10_stat2) %>%
  # mutate(raw_stat2_sleep_count_and_ratio = raw_stat2_sleep_count_and_ratio * 100,
  #        median_5_stat2_sleep_count_and_ratio = median_5_stat2_sleep_count_and_ratio * 100,
  #        median_10_stat2_sleep_count_and_ratio = median_5_stat2_sleep_count_and_ratio * 100)
  transmute(
    row = row,
    "Raw ZM Predictions" = glue("{round(raw_stat, 1)} ({round(raw_stat2, 1)})"),
    "5-Min Median" = glue("{round(median_5_stat, 1)} ({round(median_5_stat2, 1)})"),
    "10-Min Median" = glue("{round(median_10_stat, 1)} ({round(median_10_stat2, 1)})")
  ) %>%
  pivot_longer(-row) %>%
  pivot_wider(names_from = row, values_from = value) %>%
  gt() %>%
  cols_label(
    name = "", spt = "SPT (hrs)", tst = "TST (hrs)", se = "SE (%)", 
    lps = "LPS (min)", waso = "WASO (min)", no = "Awakenings (N)"
  ) %>%
  cols_align(align = "right", columns = -name)


# table 11 ----------------------------------------------------------------

metrics <-
  read_csv("~/projects/sleep_study/data/processed/in_bed_metrics.csv") %>% 
  bind_rows(read_csv("~/projects/sleep_study/data/processed/biLSTM_in_bed_performance_metrics.csv") %>% 
              filter(type == "raw") %>% 
              select(-type))

# Create a formatted table to display the metrics, grouped by epoch length
tbl_11 <-
  metrics %>%
  mutate(
    .estimate = .estimate * 100,
    .metric = factor(.metric,
                     levels = c("f_meas", "accuracy", "sensitivity", "precision", "specificity"),
                     labels = c("F1 Score (%)", "Accuracy (%)", "Sensitivity (%)", "Precision (%)", "Specificity (%)")
    ),
    model = case_when(model == "decision_tree" ~ "Decision Tree",
                      model == "log_reg" ~ "Logistic Regression",
                      model == "mlp" ~ "Feed-Forward Neural Net",
                      model == "xgboost" ~ "XGBoost",
                      TRUE ~ model),
    across(where(is.numeric), ~ round(.x, digits = 1))
  ) %>%
  select(-.estimator) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  gt() %>%
  cols_label(model = "") 
  # fmt_number(decimals = 1) 


# table 12 ----------------------------------------------------------------

all_metrics_30 <-
  read_csv("~/projects/sleep_study/data/processed/all_metrics_sleep_30.csv") %>% 
  bind_rows(
    read_csv("~/projects/sleep_study/data/processed/biLSTM_sleep_performance_metrics.csv") %>% 
      rename(group = type)) %>% 
  mutate(
    group = case_when(group == "raw" ~ "Raw ZM Predictions",
                      group == "median_5" ~ "5-Min Median",
                      group == "median_10" ~ "10-Min Median")
  )

tbl_12 <-
  all_metrics_30 %>%
  select(-.estimator) %>%
  mutate(
    .estimate = .estimate * 100,
    .metric = factor(.metric,
                     levels = c("f_meas", "precision", "npv", "sensitivity", "specificity"),
                     labels = c("F1 Score", "Precision", "NPV", "Sensitivity", "Specificity")
    ),
    across(where(is.numeric), ~ round(.x, digits = 1))
  ) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  rename_with(.cols = "F1 Score":"Specificity", ~ paste(.x, "(%)")) %>% 
  gt(groupname_col = "group") |>
  # fmt_number(everything(), decimals = 1) %>% 
  tab_options(table.font.names = "ibm") |>
  cols_width(everything() ~ px(140)) |>
  cols_align(align = "left", columns = 1:2) %>%
  cols_align(align = "right", columns = 3:7) %>% 
  cols_label(model = "") %>% 
  gt_theme_guardian() %>%
  opt_table_font(font = list(
    google_font(name = "Montserrat")
  )) %>% 
  tab_options(
    table.font.size = px(12)
  ) 


# table 13 ----------------------------------------------------------------


stats <-
  read_csv("data/all_boostrap_ba_cis.csv") %>%
  mutate(
    variable = str_remove(variable, "diff_")
  ) %>%
  left_join(read_csv("data/all_cors.csv"),
            by = c("type", "model", "variable" = "correlation")
  )

tbl_13 <-
  stats %>%
  mutate(
    model = factor(model, levels = c(
      "decision_tree", "logistic_regression",
      "neural_network", "xgboost", "biLSTM"
    ))
  ) %>%
  arrange(model, desc(type)) %>%
  mutate(
    across(bias:loa_lower_ci_upper, ~ round(.x, 1)),
    across(estimate:upper.ci, ~ round(.x, 2))
  ) %>%
  transmute(
    type = type, model = model, variable = variable,
    bias = glue("{bias} ({bias_ci_lower};{bias_ci_upper})"),
    lower_loa = glue("{loa_lower} ({loa_lower_ci_lower};{loa_lower_ci_upper})"),
    upper_loa = glue("{loa_upper} ({loa_upper_ci_lower};{loa_upper_ci_upper})"),
    pearson = glue("{estimate} ({lower.ci};{upper.ci})")
  ) %>%
  mutate(
    type = case_when(
      type == "raw" ~ "Raw ZM Predictions",
      type == "median5" ~ "5-Min Median",
      type == "median10" ~ "10-Min Median"
    ),
    model = case_when(
      model == "decision_tree" ~ "Decision Tree",
      model == "logistic_regression" ~ "Logistic Regression",
      model == "neural_network" ~ "Feed-Forward Neural Net",
      model == "xgboost" ~ "XGboost",
      TRUE ~ model
    ),
    variable = str_to_upper(variable),
    variable = case_when(
      variable == "SPT" ~ "SPT (min)",
      variable == "TST" ~ "TST (min)",
      variable == "SE" ~ "SE (%)",
      variable == "LPS" ~ "LPS (min)",
      variable == "WASO" ~ "WASO (min)"
    )
  ) %>%
  gt(groupname_col = c("type", "variable")) %>%
  cols_label(
    bias = "Bias (95% CI)", lower_loa = "lower LOA (95% CI)",
    upper_loa = "upper LOA (95% CI)", pearson = md("Pearson, _r_ (95% CI)"),
    model = ""
  ) %>%
  cols_align(align = "right", columns = bias:pearson) %>%
  cols_width(variable ~ px(200)) %>%
  fmt_markdown(pearson)


### short table ###
short_tbl_ba_cor <-
  stats %>%
  mutate(
    model = factor(model, levels = c(
      "decision_tree", "logistic_regression",
      "neural_network", "xgboost", "biLSTM"
    ))
  ) %>%
  arrange(model, desc(type)) %>%
  mutate(
    across(bias:loa_lower_ci_upper, ~ round(.x, 1)),
    across(estimate:upper.ci, ~ round(.x, 2))
  ) %>%
  transmute(
    type = type, model = model, variable = variable,
    bias = glue("{bias} ({bias_ci_lower};{bias_ci_upper})"),
    lower_loa = glue("{loa_lower} ({loa_lower_ci_lower};{loa_lower_ci_upper})"),
    upper_loa = glue("{loa_upper} ({loa_upper_ci_lower};{loa_upper_ci_upper})"),
    pearson = glue("{estimate} ({lower.ci};{upper.ci})")
  ) %>%
  mutate(
    type = case_when(
      type == "raw" ~ "Raw ZM Predictions",
      type == "median5" ~ "5-Min Median",
      type == "median10" ~ "10-Min Median"
    ),
    model = case_when(
      model == "decision_tree" ~ "Decision Tree",
      model == "logistic_regression" ~ "Logistic Regression",
      model == "neural_network" ~ "Feed-Forward Neural Net",
      model == "xgboost" ~ "XGboost",
      TRUE ~ model
    ),
    variable = str_to_upper(variable),
    variable = case_when(
      variable == "SPT" ~ "SPT (min)",
      variable == "TST" ~ "TST (min)",
      variable == "SE" ~ "SE (%)",
      variable == "LPS" ~ "LPS (min)",
      variable == "WASO" ~ "WASO (min)"
    )
  ) %>%
  filter(type == "5-Min Median") %>%
  gt(groupname_col = c("type", "model")) %>%
  cols_label(
    bias = "Bias (95% CI)", lower_loa = "Lower LOA (95% CI)",
    upper_loa = "Upper LOA (95% CI)", pearson = md("Pearson, _r_ (95% CI)"),
    variable = ""
  ) %>%
  cols_align(align = "right", columns = bias:pearson) %>%
  cols_width(variable ~ px(200)) %>%
  fmt_markdown(pearson)

