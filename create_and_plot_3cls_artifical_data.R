############### Libraries ##############################
library(mixOmics)
library(ggplot2)
library(cowplot)
library(ggpubr)

############### Constants ##############################
# Data generation parameters
SAMPLE_SIZES <- c(n1 = 20, n2 = 40, n3 = 15)
MEANS <- c(m1 = 4, m2 = 6, m3 = 8)
STANDARD_DEVIATIONS <- c(s1 = 2, s2 = 4, s3 = 3)
TOTAL_SAMPLES <- sum(SAMPLE_SIZES)
CLASS_LABELS <- c(1, 2, 3)
SCENARIOS <- c("Significant", "Non_significant")
PROJECTION_TYPE <- "PLS-DA"  # Default projection type
SEED <- 12
show_labels <- FALSE

############### Data Generation Functions ##############################
generate_variable_A <- function() {
  set.seed(SEED)
  c(rnorm(SAMPLE_SIZES[1], MEANS[1], STANDARD_DEVIATIONS[1]),
    rnorm(SAMPLE_SIZES[2], MEANS[2], STANDARD_DEVIATIONS[2]),
    rnorm(SAMPLE_SIZES[3], MEANS[3], STANDARD_DEVIATIONS[1]))
}

generate_variable_B <- function() {
  set.seed(SEED)
  c(rnorm(SAMPLE_SIZES[1], 0.5*MEANS[1], 2*STANDARD_DEVIATIONS[1]),
    rnorm(SAMPLE_SIZES[2], 0.3*MEANS[2], 3*STANDARD_DEVIATIONS[2]),
    rnorm(SAMPLE_SIZES[3], 0.8*MEANS[3], 1.5*STANDARD_DEVIATIONS[3]))
}

generate_variables_C_and_D <- function() {
  set.seed(SEED)
  uniform_values <- runif(n = 500, min = 0, max = 10)
  variable_C <- sample(x = uniform_values, size = TOTAL_SAMPLES, prob = uniform_values/max(uniform_values))
  variable_D <- jitter(sample(x = uniform_values, size = TOTAL_SAMPLES, prob = sample(uniform_values/max(uniform_values))))
  list(C = variable_C, D = variable_D)
}

generate_variables_E_and_G <- function() {
  set.seed(SEED)
  variable_E <- c(runif(SAMPLE_SIZES[1], 0, 4), runif(SAMPLE_SIZES[2], 2, 7), runif(SAMPLE_SIZES[3], 5, 7))
  variable_G <- c(runif(SAMPLE_SIZES[1], 0, 7), runif(SAMPLE_SIZES[2], 3, 9), runif(SAMPLE_SIZES[3], 3, 8))
  list(E = variable_E, G = variable_G)
}

generate_variables_H_and_I <- function() {
  set.seed(SEED)
  variable_H <- runif(TOTAL_SAMPLES, 0, 4)
  variable_I <- jitter(1.5 * variable_H)
  list(H = variable_H, I = variable_I)
}

create_synthetic_dataset <- function() {
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
get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

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

compute_voronoi_diagram <- function(x_coords, y_coords, class_groups, bounding_box = NULL) {
  requireNamespace("deldir")

  if (is.null(bounding_box)) {
    coordinate_range <- function(values) range(values, na.rm = TRUE)
    bounding_box <- c(coordinate_range(x_coords), coordinate_range(y_coords))
  } else {
    bounding_box <- bounding_box
  }

  tessellation <- deldir::deldir(x_coords, y_coords, rw = bounding_box)
  tiles <- deldir::tile.list(tessellation)

  do.call(rbind, lapply(seq_along(tiles), function(i) {
    data.frame(
      x = tiles[[i]]$x,
      y = tiles[[i]]$y,
      id = i,
      class = class_groups[i]
    )
  }))
}

create_projection_plots <- function(projection_data, scenario_name, projection_method) {
  projection_plot_data <- mixOmics::plotIndiv(projection_data, ellipse = FALSE, legend = TRUE, style = "graphics")
  plot_dataframe <- projection_plot_data$df

  # Ellipse plot
  ellipse_plot <- ggplot(data = plot_dataframe, aes(x = x, y = y, color = group, fill = group, shape = group)) +
    geom_point() +
    stat_ellipse(geom = "polygon", alpha = .1) +
    theme_light() +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme(legend.position = c(.1, .1), legend.background = element_rect(fill = ggplot2::alpha("white", 0.2))) +
    labs(title = paste0(scenario_name, ": ", projection_method, " projection"), x = "Dim1", y = "Dim2", color = "Class", shape = NULL, FILL = NULL) +
    guides(shape = "none", fill = "none") +
    geom_vline(xintercept = 0, color = "grey20", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "grey20", linetype = "dashed")

  if(show_labels)
    ellipse_plot <- ellipse_plot +
    ggrepel::geom_text_repel(aes(x = x, y = y, color = group, fontface = 2, label = rownames(plot_dataframe)), max.overlaps = Inf)

  # Voronoi plot
  voronoi_data <- compute_voronoi_diagram(plot_dataframe$x, plot_dataframe$y, plot_dataframe$group, bounding_box = unlist(get_plot_limits(ellipse_plot)))

  voronoi_plot <- ggplot() +
    geom_polygon(data = voronoi_data, aes(x = x, y = y, group = id, fill = class), alpha = 0.3, color = NA, show.legend = FALSE) +
    geom_point(data = plot_dataframe, aes(x = x, y = y, color = group), size = 2) +
    ggthemes::scale_fill_colorblind() +
    ggthemes::scale_color_colorblind() +
    theme_light() +
    labs(title = paste0(scenario_name, ": ", projection_method, " projection"), x = "Dim1", y = "Dim2", color = "Class", fill = NULL, shape = NULL, group = NULL) +
    theme(legend.position = c(.1, .1), legend.background = element_rect(fill = ggplot2::alpha("white", 0.2)))

  if(show_labels)
    voronoi_plot <- voronoi_plot +
    ggrepel::geom_text_repel(data = plot_dataframe, aes(x = x, y = y, color = group, fontface = 2, label = rownames(plot_dataframe)), max.overlaps = Inf)

  voronoi_plot_plus_ellipse <- voronoi_plot +
    stat_ellipse(data = plot_dataframe, aes(x = x, y = y, color = group, fill = group), geom = "polygon", alpha = .2, show.legend = FALSE)

  list(ellipse_plot = ellipse_plot, voronoi_plot = voronoi_plot, voronoi_plot_plus_ellipse = voronoi_plot_plus_ellipse)
}

process_scenario <- function(scenario_name, base_dataset) {
  current_dataset <- base_dataset

  if (scenario_name == "Non_significant") {
    set.seed(123)
    current_dataset[,-1] <- apply(current_dataset[,-1], 2, sample)
  }

  boxplot_result <- create_boxplot(current_dataset)
  print(boxplot_result)

  projection_result <- perform_projection_analysis(current_dataset, PROJECTION_TYPE)
  projection_plots <- create_projection_plots(projection_result, scenario_name, PROJECTION_TYPE)

  list(
    projection_plot_ellipses = projection_plots$ellipse_plot,
    projection_plot_voronoi = projection_plots$voronoi_plot,
    voronoi_plot_plus_ellipse = projection_plots$voronoi_plot_plus_ellipse,
    boxplot_df_actual = boxplot_result
  )
}

############### Main Analysis ##############################
synthetic_dataset <- create_synthetic_dataset()

analysis_results <- lapply(SCENARIOS, function(scenario) {
  process_scenario(scenario, synthetic_dataset)
})
names(analysis_results) <- SCENARIOS

# Create combined plot
final_plot_list <- list(
  analysis_results$Significant$projection_plot_ellipses,
  analysis_results$Significant$projection_plot_voronoi,
  analysis_results$Significant$voronoi_plot_plus_ellipse,
  analysis_results$Non_significant$projection_plot_ellipses,
  analysis_results$Non_significant$projection_plot_voronoi,
  analysis_results$Non_significant$voronoi_plot_plus_ellipse
)

combined_visualization_6 <- cowplot::plot_grid(plotlist = final_plot_list, labels = "AUTO")
print(combined_visualization_6)

combined_visualization_4 <- cowplot::plot_grid(plotlist = final_plot_list[c(1,2,4,5)], labels = "AUTO")

ggsave(
  filename = paste0("combined_visualization_artificial_", Projection, ".svg"),
  plot = combined_visualization, width = 18, height = 12
)

ggsave(
  filename = paste0("combined_visualization_artificial_", Projection, ".svg"),
  plot = combined_visualization, width = 12, height = 12
)

# Create combined plot
raw_data_plot_list <- list(
  analysis_results$Significant$boxplot_df_actual,
  analysis_results$Non_significant$boxplot_df_actual
)

Data_pvals<-  cowplot::plot_grid(plotlist = raw_data_plot_list, ncol = 1, labels = "AUTO")
print(Data_pvals)
ggsave(
  filename = paste0("raw_data_artificial", ".svg"),
  plot = Data_pvals, width = 18, height = 8
)

