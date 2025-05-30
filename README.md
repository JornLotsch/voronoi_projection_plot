# voronoi_projection_plot

A lightweight R code snippet for creating Voronoi tessellation visualizations of 2D projected data, particularly useful for displaying results from dimensionality reduction techniques.

## Overview

ProjectionViz provides an intuitive way to visualize class separation in projected data using Voronoi diagrams. Voronoi tessellation divides the plot space into regions based on proximity to data points, offering an elegant visualization of class boundaries and decision regions that is particularly effective for understanding clustering and classification results.

## Features

- **Voronoi Tessellation Visualization**: Creates proximity-based regions showing natural class boundaries
- **Dual Classification Support**: Allows separate classifications for point colors and Voronoi region fills
- **Flexible Input Handling**: Works with various data formats and handles missing class labels gracefully
- **Customizable Aesthetics**: Full control over colors, transparency, point sizes, and styling
- **Label Support**: Optional case labeling with intelligent overlap avoidance
- **Publication Ready**: Clean, professional plots suitable for academic and business presentations

## Installation

You can install ProjectionViz directly from GitHub:
```r
devtools::install_github("JornLotsch/voronoi_projection_plot")
```
## Dependencies

The package requires:
- `ggplot2` (for plotting)
- `deldir` (for Voronoi tessellation)
- `ggrepel` (optional, for smart label positioning)

## Quick Start

### Create sample projected data
```r
set.seed(123)

projected_data <- data.frame(
  PC1 = rnorm(150, mean = rep(c(-2, 0, 2), each = 50)),
  PC2 = rnorm(150, mean = rep(c(1, -1, 0), each = 50))
)
```
### Add class labels
```r
classes <- rep(c("Group A", "Group B", "Group C"), each = 50)
```

### Create Voronoi visualization
```r
plot <- create_voronoi_plot(
  data = projected_data,
  class_column = classes,
  title = "PCA Projection with Voronoi Tessellation",
  coord_names = c("PC1", "PC2")
)
```
### Advanced Usage with Dual Classifications

### Create alternative classification
```r
alt_classes <- rep(c("Type X", "Type Y"), length.out = 150)
```
### Use different classifications for points and regions
```r
plot <- create_voronoi_plot(
  data = projected_data,
  class_column = classes,
  alternative_class_column = alt_classes,
  color_points = "alternative",     # Points colored by alternative classification
  fill_voronoi = "primary",         # Regions filled by primary classification
  title = "Dual Classification Voronoi Plot"
)
``` 

### Specifying Coordinate Columns

```r
# When your data has numeric class columns that might interfere
data_with_numeric_classes <- data.frame(
  class_id = c(1, 2, 3, 1, 2, 3),  # numeric class column
  PC1 = rnorm(6),
  PC2 = rnorm(6),
  other_numeric = runif(6)
)

# Explicitly specify which columns to use as coordinates
plot <- create_voronoi_plot(
  data = data_with_numeric_classes,
  coordinate_columns = c("PC1", "PC2"),  # Specify coordinate columns
  class_column = "class_id",
  title = "Explicit Coordinate Specification"
)
```


## Main Function: `create_voronoi_plot()`

### Call
```r
result <-
  create_projection_plots(
    data,
    class_column = NULL,
    alternative_class_column = NULL,
    coordinate_columns = NULL,
    case_labels = NULL,
    coord_names = c("Dim1", "Dim2"),
    title = NULL,
    show_labels = FALSE,
    ellipse_alpha = 0.1,
    voronoi_alpha = 0.3,
    point_size = 2,
    legend_position = c(0.1, 0.1),
    color_palette = NULL,
    add_grid_lines = TRUE,
    color_points = "primary",
    fill_voronoi = "primary"
  )
```

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `data` | data.frame | - | Data with ≥2 numeric columns for coordinates |
| `class_column` | character/vector | NULL | Column name or vector of class labels |
| `alternative_class_column` | character/vector | NULL | Alternative column name or vector of class labels |
| `coordinate_columns` | character vector | NULL | Column names to use as coordinates (if NULL, uses first 2 numeric columns) |
| `case_labels` | character vector | NULL | Individual case labels (uses row numbers if NULL) |
| `coord_names` | character vector | c("Dim1", "Dim2") | Names for coordinate axes |
| `title` | character | NULL | Plot title |
| `show_labels` | logical | FALSE | Whether to show case labels |
| `voronoi_alpha` | numeric | 0.3 | Transparency of Voronoi regions (0-1) |
| `point_size` | numeric | 2 | Size of data points |
| `legend_position` | character/numeric | c(0.1, 0.1) | Legend position |
| `color_palette` | function/character | NULL | Custom color palette |
| `add_grid_lines` | logical | TRUE | Whether to add origin grid lines |
| `color_points` | character | "primary" | Which classification to use for point colors ("primary" or "alternative") |
| `fill_voronoi` | character | "primary" | Which classification to use for Voronoi fills ("primary" or "alternative") |


## Examples

### Basic Visualization
<img src="./example_plot.svg">

## Citation
Lötsch, J. and A. Ultsch (2024). "Comparative assessment of projection and clustering method combinations in the analysis of biomedical data." Informatics in Medicine Unlocked 50: 101573. 
https://www.sciencedirect.com/science/article/pii/S2352914824001291
