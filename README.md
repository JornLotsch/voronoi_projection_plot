# voronoi_projection_plot

A lightweight R code snippet for creating Voronoi tessellation visualizations of 2D projected data, particularly useful for displaying results from dimensionality reduction techniques.

## Overview

VoronoiBiomedPlot provides an intuitive way to visualize class separation in projected data using Voronoi diagrams. Voronoi tessellation divides the plot space into regions based on proximity to data points, offering an elegant visualization of class boundaries and decision regions that is particularly effective for understanding clustering and classification results.

## Features

- **Voronoi tessellation visualization**: Creates proximity-based regions showing natural class boundaries
- **Dual classification support**: Allows separate classifications for point colors and Voronoi region fills
- **Customizable aesthetics**: Full control over colors, transparency, point sizes, and styling
- **Label Support**: Optional case labeling with intelligent overlap avoidance
- **Publication ready**: Clean, professional plots suitable for academic and business presentations

## Installation

You can download this code to you local hard drive and run it from there. 
An R library "VoronoiBiomedPlot" will be available and then mentioned here.

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
### Advanced usage with dual classifications

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

### Specifying coordinate columns

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


## Main function 1: `create_voronoi_plot()`

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
    legend_position = "bottom",
    color_palette = NULL,
    add_grid_lines = FALSE,
    color_points = "primary",
    fill_voronoi = "primary",
    label_fontface = "plain",
    label_size = 3.88
  )
```

### Output

| Output | Type | Description |
| --- | --- | --- |
| `result` | list | A list containing 3 named ggplot objects: |
| `result$scatter_plot` | ggplot | Standard scatter plot of the projected data |
| `result$voronoi_plot` | ggplot | Voronoi tessellation plot with data points |
| `result$combined_plot` | ggplot | Combined visualization with additional features |


## Main function 2: `create_voronoi_plot()`

### Call
```r
result <-
  create_voronoi_plot(
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
    legend_position = "bottom",
    color_palette = NULL,
    add_grid_lines = FALSE,
    color_points = "primary",
    fill_voronoi = "primary",
    label_fontface = "plain",
    label_size = 3.88
  )
```

#### Output

| Output | Type | Description |
| --- | --- | --- |
| `result` | ggplot | A single ggplot object containing the Voronoi tessellation visualization with data points overlaid |


### Parameters
| Parameter | Type | Default | Description                                                                |
|-----------|------|---------|----------------------------------------------------------------------------|
| `data` | data.frame | - | Data with ≥2 numeric columns for coordinates                               |
| `class_column` | character/vector | NULL | Column name or vector of class labels                                      |
| `alternative_class_column` | character/vector | NULL | Alternative column name or vector of class labels                          |
| `coordinate_columns` | character vector | NULL | Column names to use as coordinates (if NULL, uses first 2 numeric columns) |
| `case_labels` | character vector | NULL | Individual case labels (uses row numbers if NULL)                          |
| `coord_names` | character vector | c("Dim1", "Dim2") | Names for coordinate axes                                                  |
| `title` | character | NULL | Plot title                                                                 |
| `show_labels` | logical | FALSE | Whether to show case labels                                                |
| `ellipse_alpha` | numeric | 0.1 | Transparency of ellipse regions (0-1)                                      |
| `voronoi_alpha` | numeric | 0.3 | Transparency of Voronoi regions (0-1)                                      |
| `point_size` | numeric | 2 | Size of data points                                                        |
| `legend_position` | character/numeric | "bottom" | Legend position                                                            |
| `color_palette` | function/character | NULL | Custom color palette                                                       |
| `add_grid_lines` | logical | FALSE | Whether to add origin grid lines                                           |
| `color_points` | character | "primary" | Which classification to use for point colors ("primary" or "alternative")  |
| `fill_voronoi` | character | "primary" | Which classification to use for Voronoi fills ("primary" or "alternative") |
| `label_fontface` | character | "plain" | Font face for case labels ("plain", "bold", "italic", "bold.italic")       |
| `label_size` | numeric | 3.88 | Size of case labels                                                        |

## Examples

### Basic Visualization
<img src="./example_plot.svg">

## License

GPL-3

## Citation

Lötsch J and Kringel D. Voronoi tessellation as a complementary alternative to confidence ellipses for visualizing data projection and clustering results. 2025 (in preparation) 



