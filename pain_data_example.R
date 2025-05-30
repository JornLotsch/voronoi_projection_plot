# Original data from Study 4 (QST)
qst_pain_models <- data.frame(
  readxl::read_excel(
    "/home/joern/Dokumente/QSTSchmerzmodelle/09Originale/Daten_Exp_pain_QST.xlsx",
    sheet = "DatenAnalysiert"
  )
)
qst_pain_models_orig <- qst_pain_models

# Align variables to "High number = low sensitivity"
# N/cm2 convert to kilopascal: Factor = 10
pain_tests_to_invert <- c(
  "TSACold", "CO2VAS", "LaserVAS", "CDT", "CPT",
  "MPS", "WUR", "VDT", "DMA"
)
newton_to_kpa <- c("PressureThr", "PressureTol")

qst_pain_models[, names(qst_pain_models) %in% pain_tests_to_invert] <-
  lapply(
    qst_pain_models[, names(qst_pain_models) %in% pain_tests_to_invert],
    function(x) { -x }
  )

qst_pain_models[, names(qst_pain_models) %in% newton_to_kpa] <-
  lapply(
    qst_pain_models[, names(qst_pain_models) %in% newton_to_kpa],
    function(x) { 10 * x }
  )

pain_tests_names <- c(
  "PressureThr", "PressureTol", "TSACold", "ElectricThr", "ElectricTol",
  "Co2Thr", "CO2VAS", "LaserThr", "LaserVAS",
  "CDT", "WDT", "TSL", "CPT", "HPT", "PPT", "MPT", "MPS", "WUR", "MDT"
)

# Extract only pain tests, remove demographics etc.
pain_tests <- subset(qst_pain_models, select = pain_tests_names)
dim(pain_tests)

pain_tests_to_log_names <- pain_tests_names
pain_tests_log <- pain_tests
pain_tests_log[, names(pain_tests_log) %in% pain_tests_to_log_names] <-
  lapply(
    pain_tests_log[, names(pain_tests_log) %in% pain_tests_to_log_names],
    function(x, mi = min(x, na.rm = TRUE)) {
      log10(x - mi + 1)
    }
  )

pain_tests_data <- cbind.data.frame(
  class = qst_pain_models_orig$Geschlecht_N_m1,
  pain_tests_log
)
pain_tests_data_complete <- na.omit(pain_tests_data)
dim(pain_tests_data_complete)

write.csv(
  x = pain_tests_data_complete,
  file = paste0("QSTpainEJP", ".csv")
)

#### Project and plot the data ####
source("/home/joern/Aktuell/ProjectionsBiomed/voronoi_projection_plot/voronoi_projection_plot/create_projection_plots.R")

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
  show_labels = TRUE
)

# Create combined plot
pain_plot_list <- list(
  pain_tests_data_complete_plot$ellipse_plot +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind(),
  pain_tests_data_complete_plot$voronoi_plot +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind(),
  pain_tests_data_complete_plot$voronoi_plot_plus_ellipse +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind()
)

pain_combined_visualization_6 <- cowplot::plot_grid(
  plotlist = pain_plot_list,
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