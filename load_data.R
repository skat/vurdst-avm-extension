library(dplyr)
library(stringr)
library(future)
library(purrr)
library(furrr)
if (!requireNamespace("konstant", quietly = TRUE)) {devtools::install(konstant_path, upgrade = "never")}
if (!requireNamespace("core", quietly = TRUE)) {devtools::install(core_path, upgrade = "never")}
library(konstant)
library(core)


process_vurderingsejendomme_chunk <- function(file) {


  data <- indlaes_vurderingsejendomme(file,
                                      trim = TRUE,
                                      salgsdata = FALSE)

  enheder <- udtraek_nestede_elementer(data = data,
                                       list_col = "vurderingsenheder",
                                       subelementer = "enhed")

  delgrunde <- udtraek_nestede_elementer(data = data,
                                         list_col = "vurderingsgrunde",
                                         subelementer = "delgrund")

  vurinfo <- udtraek_element(data = data, col = "vurinfo")

  vurderingsejendomme <- full_join(enheder, delgrunde, by = c("vurderingsejendom_id_ice",
                                                              "delgrund.delgrund_ids",
                                                              "df_virkningstid"))

  vurderingsejendomme <- vurderingsejendomme %>%
                         mutate(delgrund.alle_jordstykke_id_ices = coalesce(delgrund.alle_jordstykke_id_ices.x, delgrund.alle_jordstykke_id_ices.y),
                                delgrund.arealberegnet = coalesce(delgrund.arealberegnet.x, delgrund.arealberegnet.y),
                                delgrund.arealberegnetudenvej = coalesce(delgrund.arealberegnetudenvej.x, delgrund.arealberegnetudenvej.y),
                                delgrund.zone = coalesce(delgrund.zone.x, delgrund.zone.y)) %>%
                         select(-c(delgrund.alle_jordstykke_id_ices.x,
                                   delgrund.alle_jordstykke_id_ices.y,
                                   delgrund.arealberegnet.x,
                                   delgrund.arealberegnet.y,
                                   delgrund.arealberegnetudenvej.x,
                                   delgrund.arealberegnetudenvej.y,
                                   delgrund.zone.x,
                                   delgrund.zone.y,
                                   subelement.x,
                                   subelement.y))

  vurderingsejendomme <- left_join(x = vurderingsejendomme, y = vurinfo, by = c("vurderingsejendom_id_ice", "df_virkningstid"))

  rm(dat, enheder, delgrunde, vurinfo)
  gc()

  vurderingsejendomme
}

process_vurderingsejendomme <- function(path) {

  chunks <- list.files(path = path,
                       pattern = "vurderingsejendomme.ndjson.xz",
                       recursive = TRUE,
                       full.names = TRUE)

  future_map_dfr(chunks, process_vurderingsejendomme_chunk)

}


# -------------------------
# 1. Hent ejendomssalg og grundsalg
# -------------------------

#2020
chunks <- list.files(path = "/data/data/premodeldataflow/20250613_vuraar2020//",
                     pattern = "salg.ndjson.xz",
                     recursive = TRUE,
                     full.names = TRUE)


markedsdata <- furrr::future_map_dfr(chunks,
                                     function(x) x %>%
                                     indlaes_vurderingsejendomme(trim = TRUE, salgsdata = TRUE))

enheder <- udtraek_nestede_elementer(data = markedsdata,
                                     list_col = "vurderingsenheder",
                                     subelementer = "enhed",
                                     keep_cols = "salg_id_ice")

bygninger <- udtraek_nestede_elementer(data = markedsdata,
                                       list_col = "vurderingsenheder",
                                       subelementer = "bygning",
                                       keep_cols = "salg_id_ice")

tekniske_anlaeg <- udtraek_nestede_elementer(data = markedsdata,
                                             list_col = "vurderingsenheder",
                                             subelementer = "tekniskanlaeg",
                                             keep_cols = "salg_id_ice")

delgrunde <- udtraek_nestede_elementer(data = markedsdata,
                                       list_col = "vurderingsgrunde",
                                       subelementer = "delgrund",
                                       keep_cols ="salg_id_ice")

vurinfo <- udtraek_element(data = markedsdata, col = "vurinfo")

#udtraek salgsflag
vurinfo[["salg_flag"]] <- map_chr(.x = vurinfo[["salg.salg_flag"]], .f = function(x){paste(x[["flag_rule_id"]], collapse = ",")})

#saml salg
salg_2020 <- full_join(enheder, delgrunde, by = c("salg_id_ice",
                                                  "vurderingsejendom_id_ice",
                                                  "delgrund.delgrund_ids",
                                                  "df_virkningstid"))

salg_2020 <- salg_2020 %>%
             mutate(delgrund.alle_jordstykke_id_ices = coalesce(delgrund.alle_jordstykke_id_ices.x, delgrund.alle_jordstykke_id_ices.y),
                    delgrund.arealberegnet = coalesce(delgrund.arealberegnet.x, delgrund.arealberegnet.y),
                    delgrund.arealberegnetudenvej = coalesce(delgrund.arealberegnetudenvej.x, delgrund.arealberegnetudenvej.y),
                    delgrund.zone = coalesce(delgrund.zone.x, delgrund.zone.y)) %>%
             select(-c(delgrund.alle_jordstykke_id_ices.x,
                       delgrund.alle_jordstykke_id_ices.y,
                       delgrund.arealberegnet.x,
                       delgrund.arealberegnet.y,
                       delgrund.arealberegnetudenvej.x,
                       delgrund.arealberegnetudenvej.y,
                       delgrund.zone.x,
                       delgrund.zone.y,
                       subelement.x,
                       subelement.y))

#berig salg med vurinfo
salg_2020 <- left_join(x = salg_2020,
                       y = vurinfo,
                       by = c("salg_id_ice",
                              "vurderingsejendom_id_ice",
                              "df_virkningstid"))

#hent id'er paa alle typer bygninger
bygningsider <- c(enheder[["vurderingsejendom_id_ice"]],
                  bygninger[["vurderingsejendom_id_ice"]],
                  tekniske_anlaeg[["vurderingsejendom_id_ice"]])

#dan ejendomssalg
ejendomssalg_2020 <- salg_2020 %>% filter(vurderingsejendom_id_ice %in% bygningsider)


#dan grundsalg
grundsalg_2020 <- salg_2020 %>% filter(!(vurderingsejendom_id_ice %in% bygningsider))


#2024
chunks <- list.files(path = "/data/data/premodeldataflow/20250601_vuraar2024//",
                     pattern = "salg.ndjson.xz",
                     recursive = TRUE,
                     full.names = TRUE)


markedsdata <- furrr::future_map_dfr(chunks,
                                     function(x) x %>%
                                     indlaes_vurderingsejendomme(trim = TRUE, salgsdata = TRUE))


enheder <- udtraek_nestede_elementer(data = markedsdata,
                                     list_col = "vurderingsenheder",
                                     subelementer = "enhed",
                                     keep_cols = "salg_id_ice")


bygninger <- udtraek_nestede_elementer(data = markedsdata,
                                       list_col = "vurderingsenheder",
                                       subelementer = "bygning",
                                       keep_cols = "salg_id_ice")


tekniske_anlaeg <- udtraek_nestede_elementer(data = markedsdata,
                                             list_col = "vurderingsenheder",
                                             subelementer = "tekniskanlaeg",
                                             keep_cols = "salg_id_ice")

delgrunde <- udtraek_nestede_elementer(data = markedsdata,
                                       list_col = "vurderingsgrunde",
                                       subelementer = "delgrund",
                                       keep_cols ="salg_id_ice")

vurinfo <- udtraek_element(data = markedsdata, col = "vurinfo")

#udtraek salgsflag
vurinfo[["salg_flag"]] <- map_chr(.x = vurinfo[["salg.salg_flag"]], .f = function(x){paste(x[["flag_rule_id"]], collapse = ",")})


#saml salg
salg_2024 <- full_join(enheder, delgrunde, by = c("salg_id_ice",
                                                  "vurderingsejendom_id_ice",
                                                  "delgrund.delgrund_ids",
                                                  "df_virkningstid"))

salg_2024 <- salg_2024 %>%
             mutate(delgrund.alle_jordstykke_id_ices = coalesce(delgrund.alle_jordstykke_id_ices.x, delgrund.alle_jordstykke_id_ices.y),
                    delgrund.arealberegnet = coalesce(delgrund.arealberegnet.x, delgrund.arealberegnet.y),
                    delgrund.arealberegnetudenvej = coalesce(delgrund.arealberegnetudenvej.x, delgrund.arealberegnetudenvej.y),
                    delgrund.zone = coalesce(delgrund.zone.x, delgrund.zone.y)) %>%
             select(-c(delgrund.alle_jordstykke_id_ices.x,
                       delgrund.alle_jordstykke_id_ices.y,
                       delgrund.arealberegnet.x,
                       delgrund.arealberegnet.y,
                       delgrund.arealberegnetudenvej.x,
                       delgrund.arealberegnetudenvej.y,
                       delgrund.zone.x,
                       delgrund.zone.y,
                       subelement.x,
                       subelement.y))

#berig salg med vurinfo
salg_2024 <- left_join(x = salg_2024,
                       y = vurinfo,
                       by = c("salg_id_ice",
                              "vurderingsejendom_id_ice",
                              "df_virkningstid"))

#hent id'er paa alle typer bygninger
bygningsider <- c(enheder[["vurderingsejendom_id_ice"]],
                  bygninger[["vurderingsejendom_id_ice"]],
                  tekniske_anlaeg[["vurderingsejendom_id_ice"]])

#dan ejendomssalg
ejendomssalg_2024 <- salg_2024 %>% filter(vurderingsejendom_id_ice %in% bygningsider)


#dan grundsalg
grundsalg_2024 <- salg_2024 %>% filter(!(vurderingsejendom_id_ice %in% bygningsider))


#saml ejendomssalg og select relevante felter
ejendomssalg <- bind_rows(ejendomssalg_2020, ejendomssalg_2024) %>%
                distinct(vurinfo.vurderingsejendom_id, enhed.enhed_id_ice, salg.koebsdato, .keep_all = TRUE)

#saml grundsalg og select relevante felter
grundsalg <- bind_rows(grundsalg_2020, grundsalg_2024) %>%
             distinct(vurinfo.vurderingsejendom_id, delgrund.delgrund_ids, salg.koebsdato, .keep_all = TRUE)



# -------------------------
# 2. Hent vurderingsejendomme
# -------------------------

vurderingsejendomme <- process_vurderingsejendomme(path = "/data/data/premodeldataflow/20250601_vuraar2024/")

# -------------------------
# 3. Antag historik
# -------------------------

antag_historik <- TRUE
antag_historik_felter <- c("vurinfo.udsigtslaengde_hav", "vurinfo.udsigtslaengde_soe")

if (antag_historik){

  ejendomssalg <- ejendomssalg %>%
                  left_join(vurderingsejendomme %>%
                  select(vurderingsejendom_id_ice, enhed.enhed_id_ice, all_of(antag_historik_felter)) %>%
                  rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)), by = c("vurderingsejendom_id_ice", "enhed.enhed_id_ice")) %>%
                  mutate(vurinfo.udsigtslaengde_hav = coalesce(vurinfo.udsigtslaengde_hav_ny, vurinfo.udsigtslaengde_hav),
                         vurinfo.udsigtslaengde_soe = coalesce(vurinfo.udsigtslaengde_soe_ny, vurinfo.udsigtslaengde_soe)) %>%
                  select(-ends_with("_ny"))

  grundsalg <- grundsalg %>%
               left_join(vurderingsejendomme %>%
               select(vurderingsejendom_id_ice, all_of(antag_historik_felter)) %>%
               slice(1) %>%
               rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)), by = c("vurderingsejendom_id_ice")) %>%
               mutate(vurinfo.udsigtslaengde_hav = coalesce(vurinfo.udsigtslaengde_hav_ny, vurinfo.udsigtslaengde_hav),
                      vurinfo.udsigtslaengde_soe = coalesce(vurinfo.udsigtslaengde_soe_ny, vurinfo.udsigtslaengde_soe)) %>%
               select(-ends_with("_ny"))

}


# -------------------------
# 4. Tilfoej geovariable
# -------------------------

geo <- konstant::geo
kommunegrupper <- konstant::kommunegrupper

ejendomssalg <- left_join(ejendomssalg, geo, by = "vurinfo.kommunenummer")
ejendomssalg <- left_join(ejendomssalg, kommunegrupper, by = "kommune_navn")

grundsalg <- left_join(grundsalg, geo, by = "vurinfo.kommunenummer")
grundsalg <- left_join(grundsalg, kommunegrupper, by = "kommune_navn")

vurderingsejendomme <- left_join(vurderingsejendomme, geo, by = "vurinfo.kommunenummer")
vurderingsejendomme <- left_join(vurderingsejendomme, kommunegrupper, by = "kommune_navn")

# -------------------------
# 4. Gem datasaet
# -------------------------

saveRDS(ejendomssalg, "ejendomssalg.rds")
saveRDS(grundsalg, "grundsalg.rds")
saveRDS(vurderingsejendomme, "vurderingsejendomme.rds")

