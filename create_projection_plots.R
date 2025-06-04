#' Create Projection Visualization Plots
#'
#' Creates three types of visualization plots for 2D projected data: ellipse plots,
#' Voronoi diagram plots, and combined ellipse-Voronoi plots. The function is designed
#' to visualize class separation in dimensionally reduced data.
#'
#' @param data A data frame containing projected data. Must have at least 2 numeric columns.
#'   If more than 2 columns are provided, the first 2 are used as coordinates.
#' @param class_column Character string specifying the column name containing class labels,
#'   or a vector of class labels. If NULL, all observations are treated as a single class.
#'   Default: NULL.
#' @param alternative_class_column Character string specifying the column name containing
#'   alternative class labels, or a vector of alternative class labels. If NULL, uses
#'   class_column. Default: NULL.
#' @param coordinate_columns Character vector of length 2 specifying the column names
#'   to use as coordinates. If NULL, uses the first two numeric columns. Default: NULL.
#' @param case_labels Character vector of case labels for individual observations.
#'   If NULL, row numbers are used. Default: NULL.
#' @param coord_names Character vector of length 2 specifying names for the coordinate axes.
#'   Default: c("Dim1", "Dim2").
#' @param title Character string for plot title. If NULL, no title is added. Default: NULL.
#' @param show_labels Logical indicating whether to show case labels on plots. Default: FALSE.
#' @param ellipse_alpha Numeric value (0-1) for ellipse transparency. Default: 0.1.
#' @param voronoi_alpha Numeric value (0-1) for Voronoi polygon transparency. Default: 0.3.
#' @param point_size Numeric value for point size. Default: 2.
#' @param legend_position Character string or numeric vector specifying legend position.
#'   Default: c(0.1, 0.1).
#' @param color_palette Function or character vector for color palette. If NULL, uses
#'   ggplot2 default colors. Default: NULL.
#' @param add_grid_lines Logical indicating whether to add dashed grid lines at origin.
#'   Default: TRUE.
#' @param color_points Character string specifying which classification to use for point colors.
#'   Either "primary" (uses class_column) or "alternative" (uses alternative_class_column).
#'   Default: "primary".
#' @param fill_voronoi Character string specifying which classification to use for Voronoi fill.
#'   Either "primary" (uses class_column) or "alternative" (uses alternative_class_column).
#'   Default: "primary".
#'
#' @return A list containing three ggplot objects:
#'   \item{ellipse_plot}{Plot with confidence ellipses for each class}
#'   \item{voronoi_plot}{Plot with Voronoi tessellation regions}
#'   \item{voronoi_plot_plus_ellipse}{Combined plot with both Voronoi regions and ellipses}
#'
#' @details The function creates visualizations for 2D projected data, particularly useful
#'   for displaying results from dimensionality reduction techniques like PCA, PLS-DA, or t-SNE.
#'
#'   Voronoi tessellation divides the plot space into regions based on proximity to data points,
#'   providing an intuitive visualization of class boundaries and decision regions.
#'
#'   Confidence ellipses show the distribution spread and correlation structure within each class.
#'
#' @examples
#' # Basic usage with projected data
#' projected_data <- data.frame(x = rnorm(100), y = rnorm(100))
#' class_labels <- rep(c("A", "B", "C"), length.out = 100)
#' plots <- create_projection_plots(projected_data, class_column = class_labels)
#'
#' # With custom parameters
#' plots <- create_projection_plots(
#'   data = projected_data,
#'   class_column = class_labels,
#'   title = "PCA Projection",
#'   coord_names = c("PC1", "PC2"),
#'   show_labels = TRUE,
#'   point_size = 3
#' )
#'
#' # Using alternative classification for point colors and Voronoi fill
#' alt_labels <- rep(c("X", "Y"), length.out = 100)
#' plots <- create_projection_plots(
#'   data = projected_data,
#'   class_column = class_labels,
#'   alternative_class_column = alt_labels,
#'   color_points = "alternative",
#'   fill_voronoi = "alternative"
#' )
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_polygon stat_ellipse theme_light
#' @importFrom ggplot2 labs theme element_rect alpha guides geom_vline geom_hline
#' @importFrom deldir deldir tile.list
#' @importFrom ggrepel geom_text_repel
#' @export
create_projection_plots <- function(data,
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
                                    fill_voronoi = "primary") {

  # Input validation
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }

  if (ncol(data) < 2) {
    stop("'data' must have at least 2 columns for coordinates")
  }

  if (!color_points %in% c("primary", "alternative")) {
    stop("'color_points' must be either 'primary' or 'alternative'")
  }

  if (!fill_voronoi %in% c("primary", "alternative")) {
    stop("'fill_voronoi' must be either 'primary' or 'alternative'")
  }

  # Extract coordinates
  if (is.null(coordinate_columns)) {
    # Original logic: first two numeric columns
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) < 2) {
      stop("'data' must have at least 2 numeric columns")
    }
    coord_cols <- names(data)[numeric_cols][1:2]
  } else {
    # User-specified coordinate columns
    if (length(coordinate_columns) != 2) {
      stop("'coordinate_columns' must specify exactly 2 column names")
    }
    if (!all(coordinate_columns %in% names(data))) {
      missing_cols <- coordinate_columns[!coordinate_columns %in% names(data)]
      stop(paste("Coordinate columns not found in data:", paste(missing_cols, collapse = ", ")))
    }
    if (!all(sapply(data[coordinate_columns], is.numeric))) {
      stop("Specified coordinate columns must be numeric")
    }
    coord_cols <- coordinate_columns
  }

  # Prepare plot dataframe
  plot_dataframe <- data.frame(
    x = data[[coord_cols[1]]],
    y = data[[coord_cols[2]]]
  )

  # Helper function to process class column
  process_class_column <- function(class_col, col_name) {
    if (is.null(class_col)) {
      factor(rep(1, nrow(data)))
    } else if (is.character(class_col) && length(class_col) == 1) {
      # class_col is a column name
      if (!class_col %in% names(data)) {
        stop(paste("Column", class_col, "not found in data"))
      }
      as.factor(data[[class_col]])
    } else {
      # class_col is a vector of class labels
      if (length(class_col) != nrow(data)) {
        stop(paste("Length of", col_name, "must match number of rows in 'data'"))
      }
      as.factor(class_col)
    }
  }

  # Handle primary class column
  plot_dataframe$group_primary <- process_class_column(class_column, "'class_column'")

  # Handle alternative class column
  if (is.null(alternative_class_column)) {
    plot_dataframe$group_alternative <- plot_dataframe$group_primary
  } else {
    plot_dataframe$group_alternative <- process_class_column(alternative_class_column, "'alternative_class_column'")
  }

  # Set the active grouping variables based on parameters
  plot_dataframe$group_color <- if (color_points == "primary") {
    plot_dataframe$group_primary
  } else {
    plot_dataframe$group_alternative
  }

  plot_dataframe$group_fill <- if (fill_voronoi == "primary") {
    plot_dataframe$group_primary
  } else {
    plot_dataframe$group_alternative
  }

  # Handle case labels
  if (is.null(case_labels)) {
    plot_dataframe$labels <- as.character(seq_len(nrow(data)))
  } else {
    if (length(case_labels) != nrow(data)) {
      stop("Length of 'case_labels' must match number of rows in 'data'")
    }
    plot_dataframe$labels <- as.character(case_labels)
  }

  # Set row names for consistency
  rownames(plot_dataframe) <- plot_dataframe$labels

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

  # Helper function to compute Voronoi diagram
  compute_voronoi_diagram <- function(x_coords, y_coords, class_groups, bounding_box = NULL) {
    if (!requireNamespace("deldir", quietly = TRUE)) {
      stop("Package 'deldir' is required for Voronoi diagrams. Please install it.")
    }

    if (is.null(bounding_box)) {
      coordinate_range <- function(values) range(values, na.rm = TRUE)
      bounding_box <- c(coordinate_range(x_coords), coordinate_range(y_coords))
    }

    tessellation <- deldir::deldir(x_coords, y_coords, rw = bounding_box)
    tiles <- deldir::tile.list(tessellation)

    do.call(rbind, lapply(seq_along(tiles), function(i) {
      data.frame(
        x = tiles[[i]]$x,
        y = tiles[[i]]$y,
        id = i,
        class = class_groups[i],
        stringsAsFactors = FALSE
      )
    }))
  }

  # Create base plot for ellipse plot (uses primary classification for ellipses and coloring based on color_points)
  create_ellipse_base_plot <- function() {
    p <- ggplot2::ggplot(data = plot_dataframe,
                         ggplot2::aes(x = x, y = y, color = group_color, fill = group_primary, shape = group_color))

    # Apply color palette if specified
    if (!is.null(color_palette)) {
      if (is.function(color_palette)) {
        p <- p + color_palette()
      } else if (is.character(color_palette)) {
        p <- p + ggplot2::scale_color_manual(values = color_palette) +
          ggplot2::scale_fill_manual(values = color_palette)
      }
    }

    p <- p + ggplot2::theme_light() +
      ggplot2::theme(
        legend.position = legend_position,
        legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.2))
      ) +
      ggplot2::labs(
        title = title,
        x = coord_names[1],
        y = coord_names[2],
        color = "Class",
        fill = "Class",
        shape = "Class"
      )

    # Add grid lines if requested
    if (add_grid_lines) {
      p <- p + ggplot2::geom_vline(xintercept = 0, color = "grey20", linetype = "dashed") +
        ggplot2::geom_hline(yintercept = 0, color = "grey20", linetype = "dashed")
    }

    return(p)
  }

  # Create ellipse plot
  ellipse_plot <- create_ellipse_base_plot() +
    ggplot2::geom_point(size = point_size) +
    ggplot2::stat_ellipse(geom = "polygon", alpha = ellipse_alpha) +
    ggplot2::guides(shape = "none", fill = "none")

  # Add labels if requested
  if (show_labels && requireNamespace("ggrepel", quietly = TRUE)) {
    ellipse_plot <- ellipse_plot +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = labels),
        fontface = "plain",
        size = 3,
        max.overlaps = Inf,
        show.legend = FALSE
      )
  }

  # Create Voronoi plot
  voronoi_data <- compute_voronoi_diagram(
    plot_dataframe$x,
    plot_dataframe$y,
    plot_dataframe$group_fill,
    bounding_box = unlist(get_plot_limits(ellipse_plot))
  )

  voronoi_plot <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = voronoi_data,
      ggplot2::aes(x = x, y = y, group = id, fill = class),
      alpha = voronoi_alpha,
      color = NA,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      data = plot_dataframe,
      ggplot2::aes(x = x, y = y, color = group_color),
      size = point_size
    ) +
    ggplot2::theme_light() +
    ggplot2::labs(
      title = title,
      x = coord_names[1],
      y = coord_names[2],
      color = "Class"
    ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.background = ggplot2::element_rect(fill = ggplot2::alpha("white", 0.2))
    )

  # Apply color palette to Voronoi plot if specified
  if (!is.null(color_palette)) {
    if (is.function(color_palette)) {
      voronoi_plot <- voronoi_plot + color_palette()
    } else if (is.character(color_palette)) {
      voronoi_plot <- voronoi_plot +
        ggplot2::scale_color_manual(values = color_palette) +
        ggplot2::scale_fill_manual(values = color_palette)
    }
  }

  # Add grid lines to Voronoi plot if requested
  if (add_grid_lines) {
    voronoi_plot <- voronoi_plot +
      ggplot2::geom_vline(xintercept = 0, color = "grey20", linetype = "dashed") +
      ggplot2::geom_hline(yintercept = 0, color = "grey20", linetype = "dashed")
  }

  # Add labels to Voronoi plot if requested
  if (show_labels && requireNamespace("ggrepel", quietly = TRUE)) {
    voronoi_plot <- voronoi_plot +
      ggrepel::geom_text_repel(
        data = plot_dataframe,
        ggplot2::aes(x = x, y = y, color = group_color, label = labels),
        fontface = "plain",
        size = 3,
        max.overlaps = Inf,
        show.legend = FALSE
      )
  }

  # Create combined plot
  voronoi_plot_plus_ellipse <- voronoi_plot +
    ggplot2::stat_ellipse(
      data = plot_dataframe,
      ggplot2::aes(x = x, y = y, color = group_color, fill = group_primary),
      geom = "polygon",
      alpha = ellipse_alpha,
      show.legend = FALSE
    )

  # Return list of plots
  return(list(
    ellipse_plot = ellipse_plot,
    voronoi_plot = voronoi_plot,
    voronoi_plot_plus_ellipse = voronoi_plot_plus_ellipse
  ))
}
