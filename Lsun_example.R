# Load and prepare Lsun data
lsun_data <- read.csv("Lsun_OriginalData.csv", row.names = 1)
names(lsun_data) <- c("class", "x", "y")

# Basic scatter plot with original classes
plot(
  lsun_data[, 2] ~ lsun_data[, 3],
  col = lsun_data$class
)

# Perform k-means clustering
set.seed(42)
k2 <- kmeans(
  scale(lsun_data[, 2:3]),
  centers = 3,
  nstart = 100
)

lsun_data$kmeans_cluster <- k2$cluster

# Plot with k-means clusters
plot(
  lsun_data[, 2] ~ lsun_data[, 3],
  col = k2$cluster
)

# Create Voronoi plots
lsun_orig_classes_plot <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "class",
  add_grid_lines = FALSE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun original classes",
    subtitle = "Points and cells colored as prior classes"
  )

lsun_kmeans_plot <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "class",
  alternative_class_column = "kmeans_cluster",
  add_grid_lines = FALSE,
  fill_voronoi = "alternative"
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun k-means clusters",
    subtitle = "Points colored as prior classes, cells as clusters"
  )

lsun_kmeans_plot_variant2 <- create_voronoi_plot(
  data = lsun_data,
  coordinate_columns = c("x", "y"),
  class_column = "kmeans_cluster",
  alternative_class_column = "class",
  add_grid_lines = FALSE
) +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = "FCPS Lsun k-means clusters",
    subtitle = "Points and cells colored as clusters"
  )


# Combine plots
lsun_combined <- cowplot::plot_grid(
  lsun_orig_classes_plot,
  lsun_kmeans_plot,
  lsun_kmeans_plot_variant2,
  labels = "AUTO",
  nrow = 1
)
print(lsun_combined)

# Save combined plot
ggsave(
  filename = "Lsun_combined.svg",
  plot = lsun_combined,
  width = 18,
  height = 6
)
