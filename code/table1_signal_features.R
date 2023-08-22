library(tidyverse)
library(gt)
library(gtExtras)
library(glue)


# table 1 -----------------------------------------------------------------

tbl_signal_features <-
  tibble(
    Name = c("Lying", "Activity", "Time", "Thigh-SDacc", "Thigh-Inclination", "Hip-SDacc", "Hip-Inclination"),
    Description = c(
      "Classification based on thigh and back",
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
      "Time categorized throughout 24 h cycle",
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
    Name = "Name",
    Description = "Description",
    Values = "Values"
  )


# table 2 -----------------------------------------------------------------

tbl_man_describe <-
  tibble(
    Population = c(rep("Children", 3), rep("Adults", 7)),
    Characteristic = c(
      "N", "Gender (% female)", "Age (years)",
      "N", "Gender (% female)", "Age (years)", "Education Level",
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
  gt(groupname_col = c("Timepoint")) %>%
  cols_label(Round = "")


# table 4 -----------------------------------------------------------------

tbl_icc_self_zm <-
  tibble(
    Measurement = c("To bed", "Out of bed"),
    `ICC (95% CI)` = c("0.98 (0.98; 0.99)", "0.98 (0.97; 0.99)"),
    ` ICC (95% CI)` = c("0.96 (0.94; 0.98)", "0.98 (0.96; 0.99)")
  ) %>%
  gt() %>%
  cols_label(Measurement = "") %>%
  tab_spanner(label = "Baseline (N = 94)", columns = 2) %>%
  tab_spanner(label = "Followup (N = 54)", columns = 3)


# table 5 -----------------------------------------------------------------

tbl_icc_man_man <- 
  tibble(
  Metric = rep(c("To bed", "Out of bed"), times = 4),
  Phase = c(rep("Baseline", times = 4), rep("Follow-Up", times = 4)),
  Round = rep(rep(c("Round 1", "Round 2"), each = 2), times = 2),
  "ICC (95% CI)" = c("0.91 (0.88; 0.94)", "0.93 (0.9; 0.95)", "0.92 (0.89; 0.94)", "0.97 (0.96; 0.98)", 
          "0.94 (0.9; 0.96)", "0.97 (0.96; 0.98)", "0.97 (0.95; 0.98)", "0.98 (0.98; 0.99)")
) %>% 
  gt(groupname_col = "Phase") %>% 
  cols_label(Metric = "")


# table 6 -----------------------------------------------------------------

tbl_icc_test_retest <- 
  tibble(
  Rater = rep(c("Rater 1", "Rater 2", "Rater 3"), each=4),
  
  Phase = rep(rep(c("Baseline", "Follow-Up"), each=2), 3),
  
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
  ) %>% select(-CI_Lower, -CI_Upper) %>% 
  pivot_wider(names_from = c(Metric, Phase), values_from = ICC) %>% 
  gt(groupname_col = c("")) %>%
  cols_label(Rater = "", 
             "To Bed_Baseline" = "To Bed\nICC (CI 95%)",
             "Out of Bed_Baseline" = "Out of Bed\nICC (CI 95%)",
             "To Bed_Follow-Up" = "To Bed\nICC (CI 95%)",
             "Out of Bed_Follow-Up" = "Out of Bed\nICC (CI 95%)") %>% 
  tab_spanner(label = "Baseline (N = 110)", columns = 2:3) %>%
  tab_spanner(label = "Followup (N = 62)", columns = 4:5)


# table 7 -----------------------------------------------------------------

tbl_7 <- 
  tibble(
  Method = rep(c("Manual, round 1", "Manual, round 2", "Self-report"), 4),
  Phase_Metric = rep(c("Baseline, to bed", "Baseline, out of bed", "Follow-up, to bed", "Follow-up, out of bed"), each=3),
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
  gt(groupname_col = "Phase_Metric")

