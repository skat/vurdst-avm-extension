library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(rstudioapi)
library(ggrastr)
library(cowplot)
devtools::load_all("~/projekter/develop/pakker/plotDK/")
setwd("~/vurdst-avm-extension/")


if (!dir.exists("Paper_figures")) {
  dir.create("Paper_figures", recursive = TRUE)
  message(sprintf("Created folder: Paper_figures"))
} else {
  message(sprintf("Folder already exists: Paper_figures"))
}


axis_size <- 12
legend_title_size <- 14
legend_text_size <- 12


### Figures main paper ###

## Figure 1 ##
dataset <- read.csv2("dataset.csv")

dataset_municipalities <- dataset %>%
                          group_by(municipality) %>%
                          mutate(mean_sales_price = mean(sales_price)) %>%
                          select(municipality_type, municipality, mean_sales_price) %>%
                          unique()

dataset_municipalities <- dataset_municipalities %>%
                          mutate(municipality = tolower(municipality)) %>%
                          rename(kommune = municipality)


# (a) Plot of municipality types
municipality_type_plot <- plotDK(data = dataset_municipalities, id = "kommune", value = "municipality_type", niveau = "kommune") +
                          geom_path() +
                          scale_fill_brewer(name = "Municipality Type", palette = "Set2") +
                          labs(fill = "Municipality Type") +
                          theme(legend.position = c(0.775, 0.8),
                                legend.title = element_text(size = legend_title_size, margin = margin(b = 5)),  # moves title up from the color bar
                                legend.text = element_text(size = legend_text_size),
                                legend.title.align = 0.5)

# (b) Density distribution of sales prices colored by municipality type
sales_price_density_plot <- ggplot(dataset, aes(x = sales_price / 1e6, color = municipality_type, fill = municipality_type)) +
                            geom_density(alpha = 0.3) +
                            scale_color_brewer(palette = "Set2", name = "Municipality Type") +
                            scale_fill_brewer(palette = "Set2", name = "Municipality Type") +
                            labs(x = "Sales price [million DKK]", y = "Density") +
                            theme(legend.position = c(0.8, 0.8),
                                  legend.title = element_text(size = legend_title_size, margin = margin(b = 5)),
                                  legend.text = element_text(size = legend_text_size),
                                  axis.text = element_text(size = axis_size),
                                  legend.title.align = 0.5)

# (c) Plot of average sales price by municipality
mean_sales_price_histogram <- ggplot(dataset_municipalities, aes(x = mean_sales_price / 1e6, fill = municipality_type)) +
                              geom_histogram(binwidth = 0.1, color = "black", position = "stack", alpha = 0.85) +
                              scale_fill_brewer(name = "Municipality type", palette = "Set2") +
                              labs(x = "Mean sales price [million DKK]", y = "Number of municipalities") +
                              theme(legend.position = c(0.8, 0.8),
                                    legend.title = element_text(size = legend_title_size, margin = margin(b = 5)),
                                    legend.text = element_text(size = legend_text_size),
                                    axis.text = element_text(size = axis_size),
                                    legend.title.align = 0.5)

#combine everything
sales_price_density_plot <- sales_price_density_plot + theme(legend.position = "none")
mean_sales_price_histogram <- mean_sales_price_histogram + theme(legend.position = "none")

right_column <- sales_price_density_plot / mean_sales_price_histogram  # stacked with "/"

# Combine with (a) on the left
Figure1 <- municipality_type_plot | right_column  # side-by-side with "|"

# Add annotation tags
Figure1 <- Figure1 +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
           plot_layout(ncol = 2, widths = c(1.5, 1))



ggplot2::ggsave("Paper_figures/Figure1.pdf", Figure1, width = 9.5, height = 6)


## Figure 2 ## This is edited manually

dataset_municipalities <- dataset_municipalities %>% filter(kommune != "bornholm")

municipality_type_plot <- plotDK(data = dataset_municipalities, id = "kommune", value = "municipality_type", niveau = "kommune") +
                          geom_path() +
                          scale_fill_brewer(name = "Municipality Type", palette = "Set2") +
                          labs(fill = "Municipality Type") +
                          theme(legend.position = c(0.775, 0.8),
                                legend.title = element_text(size = legend_title_size, margin = margin(b = 5)),  # moves title up from the color bar
                                legend.text = element_text(size = legend_text_size),
                                legend.title.align = 0.5)

Figure2 <- municipality_type_plot + theme(legend.position = "none")#fjern legend


ggplot2::ggsave("Paper_figures/Figure2.pdf", Figure2, width = 6, height = 6)



## Figure 3 ##
plot_spatial_smooths <- readRDS("saved_files/plot_spatial_smooths.rds")

# 1 = effect of model 1
Fig3a <- plot_spatial_smooths[[1]]

# Zoomed-in version of the same plot (keeps fixed aspect)
Fig3b <- plot_spatial_smooths[[1]] +
         coord_cartesian(xlim = c(690000, 740000), ylim = c(6150000, 6200000)) +
         guides(fill = "none")


# 2 = SE of model 1
Fig3c <- plot_spatial_smooths[[2]]

# 3 = effect of model 2 (example)
Fig3d <- plot_spatial_smooths[[9]]

#wrap plots
plots <- list(Fig3a, Fig3b, Fig3c, Fig3d)

Figure3 <- wrap_plots(plots, ncol = 2) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
           theme(legend.position = c(0.7, 0.8),
                 legend.text = element_text(size = legend_text_size),
                 legend.title = element_text(size = legend_title_size),
                 legend.box = "horizontal",
                 legend.justification = "center")


ggplot2::ggsave("Paper_figures/Figure3.pdf", Figure3, width = 8.2, height = 6)
ggsave("Paper_figures/Figure3.png", Figure3, width = 8.2, height = 6, dpi = 300)



## Figure 4 ##
plot_smooths <- readRDS("saved_files/plot_smooths.rds")

smooths <- c("sales_date_numeric", "year_built", "living_space", "coast_distance", "ocean_view", "train_station_distance")

selected_plots <- plot_smooths[smooths]

#strip titles

selected_plots <- lapply(selected_plots, function(p) {
  p + labs(title = NULL)
})

Figure4 <- wrap_plots(selected_plots, ncol = 3) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 legend.text = element_text(size = legend_text_size),
                 legend.title = element_text(size = legend_title_size),
                 axis.text = element_text(size = axis_size),
                 legend.box = "horizontal",
                 legend.justification = "center")


ggplot2::ggsave("Paper_figures/Figure4.pdf", Figure4, width = 12.5, height = 6)



## Figure 5 ##
plot_factors <- readRDS("saved_files/plot_factors.rds")

factors <- c("municipality_type", "house_type", "roof_type", "heating_type")

selected_plots <- plot_factors[factors]

#strip titles

selected_plots <- lapply(selected_plots, function(p) {
  p + labs(title = NULL)
})

Figure5 <- wrap_plots(selected_plots, ncol = 2) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
           plot_layout(guides = "collect") &
           theme(legend.position = "bottom",
                 legend.text = element_text(size = legend_text_size),
                 legend.title = element_text(size = legend_title_size),
                 axis.text = element_text(size = axis_size),
                 legend.box = "horizontal",
                 legend.justification = "center")


ggplot2::ggsave("Paper_figures/Figure5.pdf", Figure5, width = 13, height = 9)


## Figure 6 ##
plot_test_residuals_log <- readRDS("saved_files/plot_test_residuals_log.rds")
plot_test_residuals_percent <- readRDS("saved_files/plot_test_residuals_percent.rds")
plot_test_residuals <- readRDS("saved_files/plot_test_residuals.rds")
plot_log_errors <- readRDS("~/vurdst-avm-extension/saved_files/plot_log_errors.rds")
plot_percentage_errors <- readRDS("~/vurdst-avm-extension/saved_files/plot_percentage_errors.rds")
plot_errors <- readRDS("~/vurdst-avm-extension/saved_files/plot_errors.rds")

plot_test_residuals_log <- plot_test_residuals_log+ guides(color = "none") + theme(legend.position = c(0.5, 0.1))
plot_test_residuals_percent <- plot_test_residuals_percent + guides(color = "none") + theme(legend.position = c(0.5, 0.1))
plot_test_residuals <- plot_test_residuals + guides(color = "none") + theme(legend.position = c(0.5, 0.1))
plot_log_errors <- plot_log_errors + guides(color = "none", linetype = guide_legend(title = NULL)) + theme(legend.position = c(0.45, 0.7))
plot_percentage_errors <- plot_percentage_errors + guides(color = "none", linetype = guide_legend(title = NULL)) + theme(legend.position = c(0.45, 0.25))
plot_errors <- plot_errors + guides(color = "none", linetype = guide_legend(title = NULL)) + theme(legend.position = c(0.4, 0.7))


plots <- list(plot_test_residuals_log,
              plot_test_residuals_percent,
              plot_test_residuals,
              plot_log_errors,
              plot_percentage_errors,
              plot_errors)

Figure6 <- wrap_plots(plots, ncol = 3) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
           theme(legend.text = element_text(size = legend_text_size),
                 legend.title = element_text(size = legend_title_size),
                 axis.text = element_text(size = axis_size))

ggplot2::ggsave("Paper_figures/Figure6.pdf", Figure6, width = 12, height = 6)


## Figure 7 ##
plot_uncertainties <- readRDS("saved_files/plot_uncertainties.rds")
plot_outliers_geographically <- readRDS("saved_files/plot_outliers_geographically.rds")

plot_uncertainties <- plot_uncertainties + guides(shape = "none", color = "none") + theme(axis.text = element_text(size = axis_size))
plot_outliers_geographically <- plot_outliers_geographically + xlim(440000, 750000) + theme(legend.position = c(1.0, 0.8))

Figure7 <- plot_uncertainties + plot_outliers_geographically +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
           plot_layout(widths = c(1, 1.3))

ggplot2::ggsave("Paper_figures/Figure7.pdf", Figure7, width = 12, height = 6)


## Figure 8 ##
plot_appraise <- readRDS("saved_files/plot_appraise.rds")
plots <- as.list(plot_appraise)

# Modify only the 4th plot (Observed vs fitted)
plots[[4]] <- plots[[4]] +
              scale_x_continuous(labels = function(x) x/1e6,
                                 name   = "Fitted sales price [million DKK]") +
              scale_y_continuous(labels = function(y) y/1e6,
                                 name   = "Sales price [million DKK]")

Figure8 <- (wrap_plots(plots) & labs(title = NULL, subtitle = NULL, caption = NULL)) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
           theme(axis.text = element_text(size = axis_size))

ggplot2::ggsave("Paper_figures/Figure8.pdf", Figure8, width = 12, height = 8)


## Figure 9 ##
plot_basis <- readRDS("saved_files/plot_basis.rds")
plot_distribution <- readRDS("saved_files/plot_distribution.rds")

plot_basis <- plot_basis +
              ylim(0, 0.3) +
              theme(legend.position = c(0.5, 0.2),
                    legend.direction = "horizontal",
                    legend.box = "vertical",
                    legend.background = element_rect(fill = "white", color = "gray90")) +
              labs(color = NULL, linetype = NULL)

plots <- list(plot_basis, plot_distribution)

Figure9 <- wrap_plots(plots, ncol = 2) +
           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
           theme(axis.text = element_text(size = axis_size),
                 legend.text = element_text(size = legend_text_size),
                 legend.title = element_text(size = legend_title_size))

ggplot2::ggsave("Paper_figures/Figure9.pdf", Figure9, width = 12, height = 5)



## Figure 10 ##
concurvity_heatmap_est <- readRDS("saved_files/concurvity_heatmap_est.rds")

ggplot2::ggsave("Paper_figures/Figure10.pdf", concurvity_heatmap_est$gtable, width = 12, height = 8)




### Figures SI ###

## Figure 1 ##
histograms_continuous <- readRDS("saved_files/histograms_continuous.rds")

Figure1_SI <- wrap_plots(histograms_continuous, ncol = 3) +
              theme(legend.text = element_text(size = legend_text_size),
                    legend.title = element_text(size = legend_title_size),
                    axis.text = element_text(size = axis_size))

ggplot2::ggsave("Paper_figures/Figure1_SI.pdf", Figure1_SI, width = 15, height = 25)


## Figure 2 ##
histograms_categorical <- readRDS("saved_files/histograms_categorical.rds")

Figure2_SI <- wrap_plots(histograms_categorical, ncol = 3) +
              plot_layout(guides = "collect") &
              theme(legend.text = element_text(size = legend_text_size),
                    legend.title = element_text(size = legend_title_size),
                    axis.text = element_text(size = axis_size))

ggplot2::ggsave("Paper_figures/Figure2_SI.pdf", Figure2_SI, width = 15, height = 25)


## Figure 3 (all spatial smooths) ##

plot_spatial_smooths <- readRDS("saved_files/plot_spatial_smooths.rds")

Figure3_SI <- wrap_plots(plot_spatial_smooths, ncol = 3) &
              theme(legend.position = c(0.7, 0.8),
                    legend.text = element_text(size = legend_text_size),
                    legend.title = element_text(size = legend_title_size),
                    legend.box = "horizontal",
                    legend.justification = "center")

ggplot2::ggsave("Paper_figures/Figure3_SI.pdf", Figure3_SI, width = 15, height = 12)


## Figure 4 (all other smooths effects) ##
plot_smooths <- readRDS("saved_files/plot_smooths.rds")

Figure4_SI <- wrap_plots(plot_smooths, ncol = 2) +
              plot_layout(guides = "collect") &
              theme(legend.position = "bottom",
                    legend.text = element_text(size = legend_text_size),
                    legend.title = element_text(size = legend_title_size),
                    legend.box = "horizontal",
                    legend.justification = "center")

ggplot2::ggsave("Paper_figures/Figure4_SI.pdf", Figure4_SI, width = 15, height = 25)

## Figure 5 (all factor effects) ##
plot_factors <- readRDS("saved_files/plot_factors.rds")

Figure5_SI <- wrap_plots(plot_factors, ncol = 2) +
              plot_layout(guides = "collect") &
              theme(legend.position = "bottom",
                    legend.text = element_text(size = legend_text_size),
                    legend.title = element_text(size = legend_title_size),
                    legend.box = "horizontal",
                    legend.justification = "center")

ggplot2::ggsave("Paper_figures/Figure5_SI.pdf", Figure5_SI, width = 15, height = 15)


## Figure 6 (all concurvity measures) ##
concurvity_heatmaps <- readRDS("saved_files/concurvity_heatmaps.rds")

Figure6_SI <- ggdraw() +
              draw_plot(concurvity_heatmaps, 0, 0, 1, 1) +
              draw_label("(a)", x = 0.02, y = 0.994, hjust = 0, vjust = 1, size = 14, color = "white") +
              draw_label("(b)", x = 0.02, y = 0.694, hjust = 0, vjust = 1, size = 14, color = "white") +
              draw_label("(c)", x = 0.02, y = 0.392, hjust = 0, vjust = 1, size = 14, color = "white")

ggplot2::ggsave("Paper_figures/Figure6_SI.pdf", Figure6_SI, width = 12, height = 22)




### Extra figures


#plot_market_noise <- readRDS("saved_files/plot_market_noise.rds")

#plot_market_noise <- plot_market_noise + guides(color = "none")


#Figure9 <- plot_market_noise + plot_market_noise +
#           plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
#           theme(legend.position = c(0.85, 0.95),
#                 legend.text = element_text(size = legend_text_size),
#                 legend.title = element_text(size = legend_title_size),
#                 axis.text = element_text(size = axis_size))


#ggplot2::ggsave("Paper_figures/Figure9.pdf", Figure9, width = 12, height = 5)


## Figure 10 ##
#plot_learning_curves <- readRDS("/data/users/AGM/Paper_v8/saved_files/plot_learning_curves.rds")
#plot_basis <- readRDS("/data/users/AGM/Paper_v8/saved_files/plot_basis.rds")

#plot_learning_curves <- plot_learning_curves +
#                        ylim(0.1, 0.3) +
#                        guides(color = "none") +
#                        guides(linetype = "none")

#plot_basis <- plot_basis + ylim(0.1, 0.3)


#Figure10 <- plot_basis + plot_learning_curves +
#            plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
#            plot_layout(ncol = 2, guides = "collect") &
#            theme(legend.position = "bottom",
#                  legend.text = element_text(size = legend_text_size),
#                  legend.title = element_text(size = legend_title_size),
#                  legend.box = "horizontal",
#                  legend.justification = "center")

#ggplot2::ggsave("Paper_figures/Figure10.pdf", Figure10, width = 12, height = 6)



## Figure 11 ##
#plot_clusters <- readRDS("saved_files/plot_clusters.rds")
#plot_clusters_geographically <- readRDS("saved_files/plot_clusters_geographically.rds")
#plot_interplay <- readRDS("saved_files/plot_interplay.rds")


#plot_clusters <- plot_clusters + guides(color = "none") + theme(axis.text = element_text(size = axis_size))
#plot_clusters_geographically <- plot_clusters_geographically + guides(color = "none")
#plot_interplay <- plot_interplay + theme(axis.text = element_text(size = axis_size))

#original boundaries
#min_x <- 440000
#max_x <- 900000
#min_y <- 6050000
#max_y <- 6410000

#plot_clusters_geographically <- plot_clusters_geographically + xlim(440000, 750000) #cut Bornholm fra
#plot_clusters_geographically_zoom <- plot_clusters_geographically + xlim(690000, 740000) + ylim(6150000, 6200000)


#Figure11 <- plot_clusters + plot_interplay + plot_clusters_geographically + plot_clusters_geographically_zoom +
#            plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") +
#            plot_layout(ncol = 2, guides = "collect", widths = c(2, 1), heights = c(1, 2)) &
#            theme(legend.position = "bottom",
#                  legend.text = element_text(size = legend_text_size),
#                  legend.title = element_text(size = legend_title_size),
#                  legend.box = "horizontal",
#                  legend.justification = "center")

#ggplot2::ggsave("Paper_figures/Figure11.pdf", Figure11, width = 12, height = 12)


