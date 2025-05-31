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
SCENARIOS <- c("No_errors", "With_errors")
PROJECTION_TYPE <- "PLS-DA"  # Default projection type
SEED <- 12
ERRORCOUNT <- 3
show_labels <- FALSE

source("create_projection_plots.R")

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

swap_random_pairs <- function(vec, ERRORCOUNT) {
  tab <- table(vec)
  uv <- names(tab)
  if (length(uv) < 2) return(vec)
  ind1 <- which(vec == uv[1])
  ind2 <- which(vec == uv[2])
  swap_n <- min(length(ind1), length(ind2), ERRORCOUNT)
  if (swap_n == 0) return(vec)
  s1 <- sample(ind1, swap_n)
  s2 <- sample(ind2, swap_n)
  temp <- vec[s1]
  vec[s1] <- vec[s2]
  vec[s2] <- temp
  return(vec)
}

process_scenario <- function(scenario_name, base_dataset) {
  current_dataset <- base_dataset

  if (scenario_name == "With_errors") {
    set.seed(12345)
    current_dataset[,1] <- swap_random_pairs(current_dataset[,1], ERRORCOUNT)
  }

  boxplot_result <- create_boxplot(current_dataset)
  print(boxplot_result)

  projection_result <- perform_projection_analysis(current_dataset, PROJECTION_TYPE)
  projection_plot_data <- mixOmics::plotIndiv(projection_result, ellipse = FALSE, legend = TRUE, style = "graphics")
  plot_dataframe <- projection_plot_data$df

  projection_plots <- create_projection_plots(data = plot_dataframe, class_column = "group")

  list(
    projection_plot_ellipses = projection_plots$ellipse_plot + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind() +
      labs(title = paste0(scenario_name, ": ", projection_method, " projection"),  subtitle = "Confidence ellipses for prior classes"),
    projection_plot_voronoi = projection_plots$voronoi_plot + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind() +
      labs(title = paste0(scenario_name, ": ", projection_method, " projection"),  subtitle = "Voronoi tesselation for prior classes"),
    voronoi_plot_plus_ellipse = projection_plots$voronoi_plot_plus_ellipse + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind() +
      labs(title = paste0(scenario_name, ": ", projection_method, " projection"),  subtitle = "Voronoi tesselation and confidence ellipses for prior classes"),
    boxplot_df_actual = boxplot_result + ggthemes::scale_fill_colorblind() + ggthemes::scale_color_colorblind() +
      labs(title = paste0(scenario_name, ": ", "boxplots"))
  )
}


############### Main Analysis ##############################
synthetic_dataset_2cls <- create_synthetic_dataset_2cls()

analysis_results <- lapply(SCENARIOS, function(scenario) {
  process_scenario(scenario, synthetic_dataset_2cls)
})
names(analysis_results) <- SCENARIOS

# Create combined plot
final_plot_list <- list(
  analysis_results$No_errors$projection_plot_ellipses  ,
  analysis_results$No_errors$projection_plot_voronoi,
  analysis_results$No_errors$voronoi_plot_plus_ellipse ,
  analysis_results$With_errors$projection_plot_ellipses,
  analysis_results$With_errors$projection_plot_voronoi ,
  analysis_results$With_errors$voronoi_plot_plus_ellipse
)

# combined_visualization_6a <- cowplot::plot_grid(plotlist = final_plot_list, labels = "AUTO")
# print(combined_visualization_6a)

combined_visualization_4a <- cowplot::plot_grid(plotlist = final_plot_list[c(1,2,4,5)], labels = "AUTO")
print(combined_visualization_4a)

ggsave(
  filename = paste0("combined_visualization_4_artificial_noerror_error", Projection, ".svg"),
  plot = combined_visualization_4a, width = 12, height = 12
)
