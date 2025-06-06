############### Read the COVID data ##############################
library(stringr)

covid_metaboanalyst_pca_score <- read.csv("covid_metabolomics_pca_score.csv", row.names = 1)
covid_metaboanalyst_pca_score$covid <- ifelse(str_detect(rownames(covid_metaboanalyst_pca_score), "Pt"), 1,
                ifelse(str_detect(rownames(covid_metaboanalyst_pca_score), "HC"), 0, NA))

############### Parameters ##############################
projection_method <- "PCA"

############### Create projection plots ##############################
set.seed(42)
covid_tests_data_complete_plot <- create_projection_plots(
  data = covid_metaboanalyst_pca_score,
  class_column = "covid",
  case_labels = rownames(covid_metaboanalyst_pca_score),
  show_labels = FALSE
)

# Create individual plots
covid_ellipse_plot <- covid_tests_data_complete_plot$ellipse_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Confidence ellipses for prior classes"
  )

covid_voronoi_plot <- covid_tests_data_complete_plot$voronoi_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation for prior classes"
  )

covid_voronoi_ellipse_plot <- covid_tests_data_complete_plot$voronoi_plot_plus_ellipse +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation and confidence ellipses for prior classes"
  )

# Additional contour plot because of the MetaboAnalyst default

# Helper function to get plot limits
get_plot_limits <- function(plot) {
  gb <- ggplot2::ggplot_build(plot)
  list(
    xmin = gb$layout$panel_params[[1]]$x.range[1],
    xmax = gb$layout$panel_params[[1]]$x.range[2],
    ymin = gb$layout$panel_params[[1]]$y.range[1],
    ymax = gb$layout$panel_params[[1]]$y.range[2]
  )
}

# 2D density contour plot
covid_voronoi_contour_plot <- 
  ggplot(data = covid_metaboanalyst_pca_score, aes(x = PC1, y = PC2, color = as.factor(covid))) +
  geom_point() +
  stat_density_2d(show.legend = FALSE, linewidth = .7) + 
  theme_light() +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind()+
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "2D density contour plot",
    color = "COVID-19"
  ) +
  xlim(unlist(get_plot_limits(covid_ellipse_plot))[1:2]) +
  ylim(unlist(get_plot_limits(covid_ellipse_plot))[3:4]) +
  theme(legend.position = "bottom")
  
# Combine plots
covid_combined_visualization_3 <- cowplot::plot_grid(
  covid_ellipse_plot,
  covid_voronoi_plot,
  covid_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 1
)
print(covid_combined_visualization_3)
ggsave(
  filename = "covid_combined_visualization_3.svg",
  plot = covid_combined_visualization_3,
  width = 18,
  height = 6
)

# Combine plots 4
covid_combined_visualization_4 <- cowplot::plot_grid(
  covid_ellipse_plot,
  covid_voronoi_plot,
  covid_voronoi_ellipse_plot,
  covid_voronoi_contour_plot,
  labels = "AUTO",
  nrow = 2
)
print(covid_combined_visualization_4)

ggsave(
  filename = "covid_combined_visualization_4.svg",
  plot = covid_combined_visualization_4,
  width = 12,
  height = 13
)