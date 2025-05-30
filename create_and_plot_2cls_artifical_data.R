############### Libraries ##############################
library(mixOmics)
library(ggplot2)
library(cowplot)
library(ggpubr)

############### Constants ##############################
# Data generation parameters
SAMPLE_SIZES <- c(n1 = 40, n2 = 40)
MEANS <- c(m1 = 4, m2 = 8)
STANDARD_DEVIATIONS <- c(s1 = 1, s2 = 1)
TOTAL_SAMPLES <- sum(SAMPLE_SIZES)
CLASS_LABELS <- c(1, 2)
PROJECTION_TYPE <- "PLS-DA"  # Default projection type
SEED <- 12
show_labels <- FALSE

############### Data Generation Functions ##############################
generate_variable_A <- function() {
  set.seed(SEED)
  c(rnorm(SAMPLE_SIZES[1], MEANS[1], STANDARD_DEVIATIONS[1]),
    rnorm(SAMPLE_SIZES[2], MEANS[2], STANDARD_DEVIATIONS[2]))
}

generate_variable_B <- function() {
  set.seed(SEED)
  c(rnorm(SAMPLE_SIZES[1], 0.5*MEANS[2], 0.9*STANDARD_DEVIATIONS[2]),
    rnorm(SAMPLE_SIZES[2], 0.3*MEANS[1], 0.9*STANDARD_DEVIATIONS[1]))
}

generate_variables_C_and_D <- function() {
  set.seed(SEED)
  seq_values <- seq(from = 0, to = 10, length.out = 500)
  variable_C <- sample(x = seq_values, size = TOTAL_SAMPLES, prob = seq_values/max(seq_values))
  variable_D <- jitter(sample(x = seq_values, size = TOTAL_SAMPLES, prob = sample(seq_values/max(seq_values))))
  list(C = variable_C, D = variable_D)
}

generate_variables_E_and_G <- function() {
  set.seed(SEED)
  variable_E <- c(runif(SAMPLE_SIZES[1], min(jitter(rep(0.2*MEANS[1],100))), max(jitter(rep(2*MEANS[1],100)))),
                  runif(SAMPLE_SIZES[2], min(jitter(rep(0.2*MEANS[2],100))), max(jitter(rep(2*MEANS[2],100)))))
  variable_G <- c(runif(SAMPLE_SIZES[1], min(jitter(rep(0.5*MEANS[1],100))), max(jitter(rep(1.5*MEANS[1],100)))),
                  runif(SAMPLE_SIZES[2], min(jitter(rep(0.5*MEANS[2],100))), max(jitter(rep(1.5*MEANS[2],100)))))
  list(E = variable_E, G = variable_G)
}

generate_variables_H_and_I <- function() {
  set.seed(SEED)
  variable_H <- runif(TOTAL_SAMPLES, 0, 4)
  variable_I <- jitter(1.5 * variable_H)
  list(H = variable_H, I = variable_I)
}

create_synthetic_dataset_2cls <- function() {
  class_vector <- as.factor(rep(CLASS_LABELS, SAMPLE_SIZES))
  cd_vars <- generate_variables_C_and_D()
  eg_vars <- generate_variables_E_and_G()
  hi_vars <- generate_variables_H_and_I()

  data.frame(
    class = class_vector,
    A = generate_variable_A(),
    B = generate_variable_B(),
    C = cd_vars$C,
    D = cd_vars$D,
    E = eg_vars$E,
    G = eg_vars$G,
    H = hi_vars$H,
    I = hi_vars$I
  )
}

############### Plotting Functions ##############################
create_boxplot <- function(data_frame) {
  data_long <- reshape2::melt(data_frame)
  ggplot(data_long, aes(x = variable, y = value, color = class, fill = class)) +
    geom_boxplot(outliers = FALSE, fill = NA) +
    geom_point(position = position_dodge(width = 0.75), size = 3) +
    facet_wrap(~variable, scales =  "free", nrow = 1) +
    theme_light() +
    theme(
      legend.position = c(.8, .2), legend.direction = "vertical",
      legend.background = element_rect(colour = "transparent", fill = ggplot2::alpha("white", 0.2)),
      strip.background = element_rect(fill = "cornsilk"), strip.text = element_text(colour = "black"),
      axis.text.y = element_blank(), axis.ticks.y = element_blank()
    ) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    stat_compare_means(mapping=aes(label=after_stat(p.adj)))
}

perform_projection_analysis <- function(data_frame, projection_method = PROJECTION_TYPE) {
  switch(projection_method,
         "PLS-DA" = mixOmics::plsda(X = data_frame[,-1], Y = data_frame$class, scale = TRUE, ncomp = 2),
         "PCA" = mixOmics::pca(X = data_frame[,-1], scale = TRUE),
         mixOmics::plsda(X = data_frame[,-1], Y = data_frame$class, scale = TRUE))
}

############### Main Analysis ##############################
synthetic_dataset_2cls <- create_synthetic_dataset_2cls()

boxplot_result <- create_boxplot(synthetic_dataset_2cls)
print(boxplot_result)

projection_result <- perform_projection_analysis(data_frame = synthetic_dataset_2cls, projection_method = PROJECTION_TYPE)
projection_plot_data <- mixOmics::plotIndiv(projection_result, ellipse = FALSE, legend = TRUE, style = "graphics")
plot_dataframe <- projection_plot_data$df

# Note: create_projection_plots function needs to be sourced from create_projection_plots.R
analysis_results <- create_projection_plots(data = plot_dataframe, class_column = "group")

# Create combined plot
final_plot_list <- list(
  analysis_results$ellipse_plot + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind(),
  analysis_results$voronoi_plot + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind()
)

combined_visualization_2 <- cowplot::plot_grid(plotlist = final_plot_list, labels = "AUTO")
print(combined_visualization_2)

ggsave(
  filename = paste0("combined_visualization_2_artificial_", PROJECTION_TYPE, ".svg"),
  plot = combined_visualization_2, width = 12, height = 6
)