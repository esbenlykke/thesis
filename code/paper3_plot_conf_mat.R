library(tidyverse)
library(tidymodels)
library(arrow)
library(patchwork)
library(ggtext)
library(cvms)
library(showtext)

showtext_auto()
font_add_google("Montserrat", "Montserrat")

model_names <- c("decision_tree", "logistic_regression", "neural_net", "xgboost")

data <-
  list.files("~/projects/sleep_study/data/data_for_modelling/chained_classifiers/sleep_predictions/", full.names = TRUE) %>%
  map(~ read_parquet(.x) %>% mutate(across(contains("sleep"), as_factor))) %>%
  set_names(model_names)

# Function to create all confusion matrices as a list of plots
create_confusion_matrix_plot <- function(data_list, truth_col, estimate_col,
                                         font_row_percentages = font(size = 1.8),
                                         font_col_percentages = font(1.8),
                                         font_counts = font(size = 1.8),
                                         font_normalized = font(size = 1.7),
                                         arrow_size = 1,
                                         add_counts = FALSE,
                                         arrow_nudge_from_text = .08,
                                         palette = "Purples",
                                         add_arrows = FALSE) {
  plot_list <- map(data_list, ~ {
    data <- .x %>%
      mutate(across(c(truth_col, estimate_col), as_factor))

    conf_mat <- table(target = data[[truth_col]], pred = data[[estimate_col]]) %>%
      as_tibble() %>%
      mutate(
        target = if_else(target == 1, "Sleep", "Wake"),
        pred = if_else(pred == 1, "Sleep", "Wake")
      )

    plot <- plot_confusion_matrix(conf_mat,
      target_col = "target",
      prediction_col = "pred",
      counts_col = "n",
      font_row_percentages = font_row_percentages,
      font_col_percentages = font_col_percentages,
      font_counts = font_counts,
      font_normalized = font_normalized,
      arrow_size = arrow_size,
      arrow_nudge_from_text = arrow_nudge_from_text,
      palette = palette,
      add_counts = add_counts,
      add_arrows = add_arrows,
      tile_border_color = "black"
    ) +
      scale_fill_gradient(low = "white", high = "#4DBBD5") +
      theme(
        text = element_text(size = 6, family = "Montserrat")
      )

    return(plot)
  })

  return(plot_list)
}

truth_estimate_pairs <- list(
  list("truth" = "sleep", "estimate" = "sleep_raw_pred_class"),
  list("truth" = "sleep_median5", "estimate" = "sleep_median_5_pred_class"),
  list("truth" = "sleep_median10", "estimate" = "sleep_median_10_pred_class")
)

plots <-
  map(truth_estimate_pairs, ~ create_confusion_matrix_plot(data, .x$truth, .x$estimate))

# Function to create confusion matrix and plot for the biLSTM
create_confusion_matrix_plot_lstm <- function(target, preds) {
  table(target = target, preds = preds) %>%
    as_tibble() %>%
    mutate(
      target = case_when(
        target == 0 ~ "In-Bed\nAwake",
        target == 1 ~ "Out-Bed\nAwake",
        target == 2 ~ "In-Bed\nAsleep"
      ),
      preds = case_when(
        preds == 0 ~ "In-Bed\nAwake",
        preds == 1 ~ "Out-Bed\nAwake",
        preds == 2 ~ "In-Bed\nAsleep"
      )
    ) %>%
    plot_confusion_matrix(
      target_col = "target", prediction_col = "preds", counts_col = "n",
      font_row_percentages = font(size = 2),
      font_col_percentages = font(size = 2),
      font_counts = font(size = 1.7),
      font_normalized = font(size = 1.7),
      arrow_size = 1,
      add_counts = FALSE,
      arrow_nudge_from_text = .08,
      palette = "Purples",
      add_arrows = FALSE,
      tile_border_color = "black"
    ) +
    scale_fill_gradient(low = "white", high = "#4DBBD5") +
    theme(
      text = element_text(size = 6, family = "Montserrat")
    )
}

# Load the data
lstm_preds <-
  read_parquet("~/projects/sleep_study/data/data_for_modelling/lstm/predictions/lstm_multiclass_preds.parquet")

# Generate confusion matrix plots
conf_mat_lstm_raw <-
  create_confusion_matrix_plot_lstm(lstm_preds$score_simple, lstm_preds$predicted_class_raw) +
  labs(title = "Raw")
conf_mat_lstm_median5 <-
  create_confusion_matrix_plot_lstm(lstm_preds$score_simple_median_5, lstm_preds$predicted_class_median_5) +
  labs(title = "5-Minute Median")
conf_mat_lstm_median10 <-
  create_confusion_matrix_plot_lstm(lstm_preds$score_simple_median_10, lstm_preds$predicted_class_median_10) +
  labs(title = "10-Minute Median")

raw <-
  wrap_plots(plots[[1]][[1]], plots[[1]][[2]], plots[[1]][[3]], plots[[1]][[4]], nrow = 1) &
    plot_annotation(
      title = "Raw",
      tag_levels = "i",
      theme =
        theme(title = element_text(size = 8, family = "Montserrat"),
              plot.title = element_text(size = 8),
              axis.title = element_text(size = 7),
              axis.text = element_text(size = 6),
              plot.tag = element_text(size = 8))
    )

median5 <-
  wrap_plots(plots[[2]][[1]], plots[[2]][[2]], plots[[2]][[3]], plots[[2]][[4]], nrow = 1) &
    plot_annotation(
      title = "5-Minute Median",
      tag_levels = "i",
      theme =
        theme(title = element_text(size = 8, family = "Montserrat"),
              plot.title = element_text(size = 8),
              axis.title = element_text(size = 7),
              axis.text = element_text(size = 6),
              plot.tag = element_text(size = 8))
    )

median10 <-
  wrap_plots(plots[[3]][[1]], plots[[3]][[2]], plots[[3]][[3]], plots[[3]][[4]], nrow = 1) &
    plot_annotation(
      title = "10-Minute Median",
      tag_levels = "i",
      theme =
        theme(title = element_text(size = 8, family = "Montserrat"),
              plot.title = element_text(size = 8),
              axis.title = element_text(size = 7),
              axis.text = element_text(size = 6),
              plot.tag = element_text(size = 8))
    ) 

wrap_elements(raw) / wrap_elements(median5) / wrap_elements(median10)


ggsave("~/projects/thesis/figures/all_binary_conf_mats.pdf", height = 13, width = 13, units = "cm", dpi = 600)


wrap_plots(conf_mat_lstm_raw, conf_mat_lstm_median5, conf_mat_lstm_median10)

ggsave("~/projects/thesis/figures/all_multiclass_conf_mats.pdf", height = 5, width = 13, units = "cm", dpi = 600)
