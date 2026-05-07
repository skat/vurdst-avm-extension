library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(mgcv)
library(gratia)
library(knitr)
library(patchwork)
library(lubridate)
library(future)
library(future.apply)
library(rlang)
library(stringr)
library(furrr)
library(xgboost)
library(caret)
library(Hmisc)
library(kableExtra)
library(nabor)
library(purrr)
library(ggrastr)
library(ggridges)
library(gridExtra)
library(forcats)
library(pheatmap)
library(viridis)
library(cowplot)
library(scico)
library(scales)

lot_sales <- read.csv2("~/analyse_grundmodeller/lot_sales.csv")
dataset <- readRDS("~/vurdst-avm-extension/dataset.rds")
models <- readRDS("~/vurdst-avm-extension/saved_files_lot_sales/models.rds")
models_lot_sales <- readRDS("~/vurdst-avm-extension/saved_files_lot_sales/models_lot_sales.rds")
models_lot_sales <- models_lot_sales[1:4]
N <- 4

assign_cv_folds <- function(data, N, seed = 123) {
  set.seed(seed)
  data %>% mutate(validation_set = sample(rep(1:N, length.out = n())),
                  .rand = runif(n()))
}


#lot sales
dataset_lot_sales_cv <- assign_cv_folds(lot_sales, N = N, seed = 123)

dataset_lot_sales_cv_standardized <- dataset_lot_sales_cv
dataset_lot_sales_cv_standardized$year_built <- 1970
dataset_lot_sales_cv_standardized$year_rebuilt <- 1970
dataset_lot_sales_cv_standardized$living_space <- 140
dataset_lot_sales_cv_standardized$lot_size <- 800
dataset_lot_sales_cv_standardized$number_of_bathrooms <- 1
dataset_lot_sales_cv_standardized$house_type <- factor("Detached", levels = levels(as.factor(dataset$house_type)))
dataset_lot_sales_cv_standardized$roof_type <- factor("Fiber cement", levels = levels(as.factor(dataset$roof_type)))
dataset_lot_sales_cv_standardized$wall_type <- factor("Brick", levels = levels(as.factor(dataset$wall_type)))
dataset_lot_sales_cv_standardized$heating_type <- factor("District heating", levels = levels(as.factor(dataset$heating_type)))

for (model_name in names(models)) {

  dataset_lot_sales_cv[[paste0("sales_price_standardized_", model_name)]] <- as.vector(predict.gam(models[[model_name]], dataset_lot_sales_cv_standardized, type = "response"))

}


models <- models_lot_sales

get_residuals <- function(model_set, dataset) {

  residual_list <- future_lapply(names(model_set), function(fold_model_name) {

    model <- model_set[[fold_model_name]]
    fold <- as.integer(str_extract(fold_model_name, "\\d+"))

    predicted_sales_price <- as.vector(
      predict.gam(
        model,
        newdata = dataset,
        type = "response"
      )
    )

    sales_price <- dataset$sales_price

    tibble(
      sales_price = sales_price,
      sales_date_numeric = dataset$sales_date_numeric,
      predicted_sales_price = predicted_sales_price,
      residual_sales_price = predicted_sales_price - sales_price,
      residual_sales_price_percent = (predicted_sales_price - sales_price) / sales_price * 100,
      residual_sales_price_log = log(predicted_sales_price) - log(sales_price),
      ratio = predicted_sales_price / sales_price,
      validation = dataset$validation_set == fold,
      id = dataset$id,
      municipality_type = dataset$municipality_type,
      fold_model = fold_model_name
    )
  })

  bind_rows(residual_list)
}

summarise_performance <- function(model_set, dataset, label) {

  residual_df <- get_residuals(model_set, dataset)

  residual_df %>%
    mutate(
      dataset_type = if_else(validation, "Validation", "Training"),
      Model = label
    ) %>%
    group_by(Model, dataset_type) %>%
    summarise(
      municipality_type = "Countrywide",
      MALE = mean(abs(residual_sales_price_log), na.rm = TRUE),
      MAPE = mean(abs(residual_sales_price_percent), na.rm = TRUE),
      MAE = mean(abs(residual_sales_price), na.rm = TRUE),
      PM20 = mean(abs(residual_sales_price_percent) <= 20, na.rm = TRUE) * 100,
      COD = mean(abs(ratio - median(ratio, na.rm = TRUE)), na.rm = TRUE) /
        median(ratio, na.rm = TRUE) * 100,
      PRD = mean(ratio, na.rm = TRUE) /
        (sum(predicted_sales_price, na.rm = TRUE) / sum(sales_price, na.rm = TRUE)),
      .groups = "drop"
    )
}


performance_countrywide <- bind_rows(
  lapply(names(models), function(label) {
    summarise_performance(
      model_set = models[[label]],
      dataset = dataset_lot_sales_cv,
      label = label
    )
  })
)

saveRDS(performance_countrywide, "~/vurdst-avm-extension/saved_files_lot_sales/performance_countrywide_gamle_grundsalg.rds")
