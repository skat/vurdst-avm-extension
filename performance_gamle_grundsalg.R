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

model_gl <- readRDS("/data/modelkoersler/stable_vuraar2022/kalibreringer/grundmodeller.rds")
model_gl <- model_gl$GVpcl
resids_rel <- (model_gl$fitted.values - model_gl$y)/model_gl$y
length(which(abs(resids_rel) < 0.2))/length(resids_rel)
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
dataset_lot_sales_standardized$maximum_coverage_percentage <- 30
dataset_lot_sales_standardized$unused_floor_area <- 0
dataset_lot_sales_standardized$number_of_divisions <- 1
dataset_lot_sales_standardized$number_of_bathrooms <- 1
dataset_lot_sales_standardized$house_type <- factor("Detached", levels = levels(as.factor(dataset$house_type)))
dataset_lot_sales_standardized$roof_type <- factor("Fiber cement", levels = levels(as.factor(dataset$roof_type)))
dataset_lot_sales_standardized$wall_type <- factor("Brick", levels = levels(as.factor(dataset$wall_type)))
dataset_lot_sales_standardized$heating_type <- factor("District heating", levels = levels(as.factor(dataset$heating_type)))

for (model_name in names(models)) {

  dataset_lot_sales[[paste0("standard_property_value_", model_name)]] <- as.vector(predict.gam(models[[model_name]], dataset_lot_sales_standardized, type = "response"))

}


dataset_lot_sales <- dataset_lot_sales %>% mutate(maximum_coverage_percentage = 30, #har ikke noget bedre bud
                                                  gross_floor_area = lot_size * maximum_coverage_percentage/100,
                                                  remaining_floor_area = pmax(0, lot_size - gross_floor_area),
                                                  number_of_divisions = 1)




models <- models_lot_sales

# -------------------------
# 1. Helpers
# -------------------------

extract_property_model <- function(model_name) {
  out <- str_extract(model_name, "standard_property_value_Model[1-4]") %>%
    str_extract("Model[1-4]")

  ifelse(is.na(out), "Direct", out)
}

extract_model_specification <- function(model_name) {
  model_name %>%
    str_remove("^model_[0-9]+_") %>%
    str_remove("_?standard_property_value_Model[1-4]?") %>%
    str_replace_all("__+", "_") %>%
    str_remove("_$")
}

# -------------------------
# 2. Residuals
# -------------------------

get_residuals_one_model_set <- function(model_set, dataset, model_label) {

  property_model <- extract_property_model(model_label)
  model_specification <- extract_model_specification(model_label)

  future_lapply(names(model_set), function(fold_model_name) {

    model <- model_set[[fold_model_name]]
    fold <- as.integer(str_extract(fold_model_name, "\\d+"))

    pred <- as.vector(predict.gam(model, newdata = dataset, type = "response"))

    obs <- dataset$sales_price

    tibble(Model = model_label,
           model_specification = model_specification,
           property_model = property_model,
           fold_model = fold_model_name,
           fold = fold,
           municipality_type = dataset$municipality_type,
           sales_price = obs,
           predicted_sales_price = pred,
           residual_sales_price = pred - obs,
           residual_sales_price_percent = (pred - obs) / obs * 100,
           residual_sales_price_log = log(pred) - log(obs),
           ratio = pred / obs)}) %>%
    bind_rows()
}

residual_df <- bind_rows(
  lapply(names(models), function(model_label) {
    message("Predicting residuals for ", model_label, " ...")

    get_residuals_one_model_set(
      model_set = models[[model_label]],
      dataset = dataset_lot_sales,
      model_label = model_label
    )
  })
)

saveRDS(residual_df, "saved_files_lot_sales/residual_df_lot_sales_gamle_grundsalg.rds")

# -------------------------
# 3. Metric function
# -------------------------

summarise_performance_from_residuals <- function(residual_df, group_vars) {

  residual_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(municipality_type = "Countrywide",
              MALE = mean(abs(residual_sales_price_log), na.rm = TRUE),
              MAPE = mean(abs(residual_sales_price_percent), na.rm = TRUE),
              MAE = mean(abs(residual_sales_price), na.rm = TRUE),
              PM20 = mean(abs(residual_sales_price_percent) <= 20, na.rm = TRUE) * 100,
              COD = mean(abs(ratio - median(ratio, na.rm = TRUE)), na.rm = TRUE) /
                median(ratio, na.rm = TRUE) * 100,
              PRD = mean(ratio, na.rm = TRUE) / (sum(predicted_sales_price, na.rm = TRUE) / sum(sales_price, na.rm = TRUE)),
              .groups = "drop")
}

# -------------------------
# 4. Exact model performance
# -------------------------

performance_exact <- summarise_performance_from_residuals(residual_df, group_vars = c("Model", "model_specification", "property_model"))


# -------------------------
# 5. Average metrics over property models
# -------------------------

performance_by_specification <- performance_exact %>%
                                group_by(model_specification) %>%
                                summarise(across(c(MALE, MAPE, MAE, PM20, COD, PRD), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
