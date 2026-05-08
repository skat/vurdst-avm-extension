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


dataset <- readRDS("~/vurdst-avm-extension/dataset.rds")
dataset_lot_sales <- readRDS("/data/modelkoersler/stable_vuraar2022/kalibreringer/grundsalg.rds")
dataset_lot_sales <- dataset_lot_sales$GVpcl
models <- readRDS("~/vurdst-avm-extension/saved_files_lot_sales/models.rds")
models_lot_sales <- readRDS("~/vurdst-avm-extension/saved_files_lot_sales/models_lot_sales.rds")


model_variable <- c(id = "vurinfo.vurderingsejendom_id",
                    sales_date = "salg.koebsdato",
                    municipality = "kommune_navn",
                    province = "landsdel_navn",
                    region = "region_navn",
                    municipality_type = "kommunegruppe",
                    zone = "delgrund.zone",
                    sales_price = "salg.ialtkoebesum",
                    lot_size = "effektivt_vurinfo.registreretareal_fratrukket_vejareal",
                    coordinate_east = "vurinfo.adresse.koordinatoest",
                    coordinate_north = "vurinfo.adresse.koordinatnord",
                    coast_distance = "vurinfo.afstand_kyst",
                    lake_distance = "vurinfo.afstand_stor_soe",
                    stream_distance = "vurinfo.afstand_stort_vandloeb",
                    road_distance = "vurinfo.afstand_naermeste_trafikvejgennemfart_trafikfordeling",
                    highway_distance = "vurinfo.afstand_motorvej_motortrafikvej",
                    train_station_distance = "vurinfo.afstand_station_any",
                    railway_distance = "vurinfo.afstand_jernbane_any",
                    windturbine_distance = "vurinfo.afstand_vindmoelle_any",
                    ocean_view = "vurinfo.udsigtslaengde_hav",
                    lake_view = "vurinfo.udsigtslaengde_soe",
                    forest_size = "vurinfo.areal_samlet_skov")

#vaelg og omdoeb modelvariable
dataset_lot_sales <- dataset_lot_sales %>%
                     rename(!!!setNames(model_variable, names(model_variable))) %>%
                     select(names(model_variable))


#omdoeb municipality_type
dataset_lot_sales$municipality_type <- as.character(dataset_lot_sales$municipality_type)
dataset_lot_sales$municipality_type[dataset_lot_sales$municipality_type == "Hovedstadskommune"] = "Capital"
dataset_lot_sales$municipality_type[dataset_lot_sales$municipality_type == "Landkommune"] = "Rural"
dataset_lot_sales$municipality_type[dataset_lot_sales$municipality_type == "Oplandskommune"] = "Commuter"
dataset_lot_sales$municipality_type[dataset_lot_sales$municipality_type == "Provinsbykommune"] = "Provincial"
dataset_lot_sales$municipality_type[dataset_lot_sales$municipality_type == "Storbykommune"] = "Large city"
dataset_lot_sales$municipality_type <- as.factor(dataset_lot_sales$municipality_type)

#conversions
dataset_lot_sales <- dataset_lot_sales %>% mutate(sales_date = as.Date(sales_date),
                                                  sales_date_numeric = decimal_date(sales_date))




#standardization
dataset_lot_sales_standardized <- dataset_lot_sales
dataset_lot_sales_standardized$year_built <- 1970
dataset_lot_sales_standardized$year_rebuilt <- 1970
dataset_lot_sales_standardized$living_space <- 140
dataset_lot_sales_standardized$lot_size <- 800
dataset_lot_sales_standardized$number_of_bathrooms <- 1
dataset_lot_sales_standardized$house_type <- factor("Detached", levels = levels(as.factor(dataset$house_type)))
dataset_lot_sales_standardized$roof_type <- factor("Fiber cement", levels = levels(as.factor(dataset$roof_type)))
dataset_lot_sales_standardized$wall_type <- factor("Brick", levels = levels(as.factor(dataset$wall_type)))
dataset_lot_sales_standardized$heating_type <- factor("District heating", levels = levels(as.factor(dataset$heating_type)))

for (model_name in names(models)) {

  dataset_lot_sales[[paste0("sales_price_standardized_", model_name)]] <- as.vector(predict.gam(models[[model_name]], dataset_lot_sales_standardized, type = "response"))

}


dataset_lot_sales <- dataset_lot_sales %>% mutate(maximum_coverage_percentage = 30, #har ikke noget bedre bud
                                                  gross_floor_area = lot_size * maximum_coverage_percentage/100,
                                                  remaining_floor_area = pmax(0, lot_size - gross_floor_area),
                                                  number_of_divisions = 1)




models <- models_lot_sales

get_residuals <- function(model_set, dataset) {

  residual_list <- future_lapply(names(model_set), function(fold_model_name) {

    model <- model_set[[fold_model_name]]

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
      id = dataset$id,
      municipality_type = dataset$municipality_type)
  })

  bind_rows(residual_list)
}

summarise_performance <- function(model_set, dataset, label) {

  residual_df <- get_residuals(model_set, dataset)

  residual_df %>% mutate(Model = label) %>%
                  group_by(Model) %>%
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
                    .groups = "drop")
}


performance_countrywide <- bind_rows(
  lapply(names(models), function(label) {
    summarise_performance(
      model_set = models[[label]],
      dataset = dataset_lot_sales,
      label = label
    )
  })
)


saveRDS(performance_countrywide, "saved_files_lot_sales/performance_countrywide_gamle_grundsalg.rds")
