# # Original data from Study 4 (QST)
# qst_pain_models <- data.frame(
#   readxl::read_excel(
#     "/home/joern/Dokumente/QSTSchmerzmodelle/09Originale/Daten_Exp_pain_QST.xlsx",
#     sheet = "DatenAnalysiert"
#   )
# )
#
# rownames(qst_pain_models) <- qst_pain_models$Probanden_Nr
#
# qst_pain_models_orig <- qst_pain_models
#
# # Align variables to "High number = low sensitivity"
# # N/cm2 convert to kilopascal: Factor = 10
# pain_tests_to_invert <- c(
#   "TSACold", "CO2VAS", "LaserVAS", "CDT", "CPT",
#   "MPS", "WUR", "VDT", "DMA"
# )
# newton_to_kpa <- c("PressureThr", "PressureTol")
#
# qst_pain_models[, names(qst_pain_models) %in% pain_tests_to_invert] <-
#   lapply(
#     qst_pain_models[, names(qst_pain_models) %in% pain_tests_to_invert],
#     function(x) { -x }
#   )
#
# qst_pain_models[, names(qst_pain_models) %in% newton_to_kpa] <-
#   lapply(
#     qst_pain_models[, names(qst_pain_models) %in% newton_to_kpa],
#     function(x) { 10 * x }
#   )
#
# pain_tests_names <- c(
#   "PressureThr", "PressureTol", "TSACold", "ElectricThr", "ElectricTol",
#   "Co2Thr", "CO2VAS", "LaserThr", "LaserVAS",
#   "CDT", "WDT", "TSL", "CPT", "HPT", "PPT", "MPT", "MPS", "WUR", "MDT"
# )
#
# # Extract only pain tests, remove demographics etc.
# pain_tests <- subset(qst_pain_models, select = pain_tests_names)
# dim(pain_tests)
#
# pain_tests_to_log_names <- pain_tests_names
# pain_tests_log <- pain_tests
# pain_tests_log[, names(pain_tests_log) %in% pain_tests_to_log_names] <-
#   lapply(
#     pain_tests_log[, names(pain_tests_log) %in% pain_tests_to_log_names],
#     function(x, mi = min(x, na.rm = TRUE)) {
#       log10(x - mi + 1)
#     }
#   )
#
# pain_tests_data <- cbind.data.frame(
#   class = qst_pain_models_orig$Geschlecht_N_m1,
#   pain_tests_log
# )
# pain_tests_data_complete <- na.omit(pain_tests_data)
# dim(pain_tests_data_complete)
#
# write.csv(
#   x = pain_tests_data_complete,
#   file = paste0("QSTpainEJP", ".csv")
# )

# Read the pain data
# pain_data_transformed <- read.csv("/home/joern/Aktuell/QST_DIB/DataSetPublished/qst_pain_data_transformed.csv", row.names = 1)
# pain_metadata <- read.csv("/home/joern/Aktuell/QST_DIB/DataSetPublished/qst_pain_metadata.csv", row.names = 1)
# pain_tests_data_complete <- cbind.data.frame(class = pain_metadata$Sex_m1, pain_data_transformed)
# rownames(pain_tests_data_complete) <- rownames(pain_data_transformed)
# pain_tests_data_complete <- na.omit(pain_tests_data_complete)
# dim(pain_tests_data_complete)
#
# write.csv(
#   x = pain_tests_data_complete,
#   file = paste0("QSTpainEJP", ".csv")
# )


############################## Start here ##########################################################
# Read the pain data

pain_tests_data_complete <- read.csv(file = paste0("QSTpainEJP", ".csv"), row.names = 1)
dim(pain_tests_data_complete)
table(pain_tests_data_complete$class)

#### Project and plot the data ####

source("create_projection_plots.R")

proj_pain_tests_data_complete <- perform_projection_analysis(
  data_frame = pain_tests_data_complete,
  projection_method = "PLS-DA"
)

proj_pain_tests_data_complete_data <- mixOmics::plotIndiv(
  proj_pain_tests_data_complete,
  ellipse = FALSE,
  legend = TRUE,
  style = "graphics"
)

pain_tests_data_complete_plot <- create_projection_plots(
  data = proj_pain_tests_data_complete_data$df,
  class_column = "group",
  case_labels = rownames(pain_tests_data_complete),
  show_labels = TRUE
)

# Create individual plots
pain_ellipse_plot <- pain_tests_data_complete_plot$ellipse_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Confidence ellipses for prior classes"
  )

pain_voronoi_plot <- pain_tests_data_complete_plot$voronoi_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation for prior classes"
  )

pain_voronoi_ellipse_plot <- pain_tests_data_complete_plot$voronoi_plot_plus_ellipse +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation and confidence ellipses for prior classes"
  )

# Combine plots
pain_combined_visualization_6 <- cowplot::plot_grid(
  pain_ellipse_plot,
  pain_voronoi_plot,
  pain_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 1
)
print(pain_combined_visualization_6)

ggsave(
  filename = "pain_combined_visualization_6.svg",
  plot = pain_combined_visualization_6,
  width = 18,
  height = 6
)

## PCA and Ward
library(ggfortify)
df <- pain_tests_data_complete[,-1]
pca_res <- prcomp(df, scale. = TRUE)
autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, variance_percentage = TRUE)

# PCA
nPC <- sum(pca_res$sdev^2 > 1)

# Ward's Method
pain_ward <- hclust(dist(pca_res$x[,1:nPC]), method = "ward.D2")
plot(pain_ward)
ward_clusters <- cutree(pain_ward, 2)

wilcox.p <- -log10(apply(pain_tests_data_complete[,-1], 2, function(x) wilcox.test(x ~ ward_clusters)$p.value))
barplot(sort(wilcox.p), las = 2)
pain_tests_data_complete_clusters <- cbind.data.frame(
  cluster = ward_clusters,
  class = pain_tests_data_complete$class,
  pca_res$x[,1:nPC]
)

create_projection_plots(
  data = pain_tests_data_complete_clusters,
  class_column = "cluster",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(pain_tests_data_complete_clusters),
  alternative_class_column = "class",
  show_labels = TRUE, fill_voronoi = "alternative"
)
