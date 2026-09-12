# =============================================================================
# Initial data extraction
# =============================================================================
#
# This script performs the initial data extraction for the analysis.
#
# It loads the data deliveries received by Vurderingsstyrelsen. Each delivery
# contains properties at a specific valuation date together with sales observed
# during the preceding six years. The current analysis uses deliveries for
# 1 January 2020 and 1 January 2024, so the combined sales population covers
# the period from 1 January 2014 to 1 January 2024.
#
# The script extracts:
#
#   1. Danish properties as of 1 January 2024
#   2. Danish property sales from 1 January 2014 to 1 January 2024
#   3. Danish lot sales from 1 January 2014 to 1 January 2024
#
# The resulting data sets are restricted
# to the variables needed for the subsequent analysis. Duplicate sales arising
# from overlapping data deliveries are removed.
#
# For sales observations the data is enriched with a list of sales flag, which indicate the type of sale and the view over ocean and lake variables are furthermore updated using the corresponding
# variables from the 2024 property data, which contains more up-to-date
# information. Lastly, all three data sets are enriched with selected
# geographic variables.
#
# The script saves three data sets:
#
#   - vurderingsejendomme.rds: Danish properties as of 1 January 2024.
#
#   - ejendomssalg.rds: Danish property sales from 1 January 2014 to 1 January 2024.
#
#   - grundsalg.rds: Danish lot sales from 1 January 2014 to 1 January 2024.
#
# These files have been preloaded and are included in the GitHub repository and are used as inputs for
# the subsequent analysis.
# =============================================================================

library(dplyr)
library(stringr)
library(jsonlite)
library(future)
library(purrr)
library(furrr)
library(tidyr)


variables <- c(

  # Base / sale variables
  "salg_id_ice",
  "vurderingsejendom_id_ice",
  "vurderingsejendomid",
  "delgrund_info.delgrund_ids",
  "kommunenummer",
  "ejendomsnummer",
  "df_virkningstid",
  "salg.koebsdato",
  "salg.ialtkoebesum",
  "salg.ejendomkategori",
  "salg.anmeldelseidentifikator",
  "salg.flereejendommeindikator",
  "salg.ubebyggetgrund",
  "salg.bygningsforholdkode",
  "salg.flag",
  "vurbenyttelseskode",
  "adresse.koordinatnord",
  "adresse.koordinatoest",

  # Enhed
  "enhed.enhed_id_ice",
  "enhed.enhed_id",
  "enhed.arealtilbeboelse",
  "enhed.arealtilerhverv",
  "enhed.andetareal",
  "enhed.boligtype",
  "enhed.enhedensanvendelse",
  "enhed.antalbadevaerelser",
  "enhed.antalvaerelser",
  "enhed.antalvaerelsertilerhverv",
  "enhed.antalvandskylledetoiletter",
  "enhed.badeforhold",
  "enhed.koekkenforhold",
  "enhed.opvarmningsmiddel",
  "enhed.varmeinstallation",
  "enhed.toiletforhold",
  "enhed.supplerendevarme",

  # Bygning
  "bygning.bygning_id_ice",
  "bygning.opfoerelsesaar",
  "bygning.omtilbygningsaar",
  "bygning.tagdaekningsmateriale",
  "bygning.supltagdaekningsmateriale",
  "bygning.ydervaeggensmateriale",
  "bygning.suplydervaeggensmateriale",
  "bygning.varmeinstallation",
  "bygning.opvarmningsmiddel",
  "bygning.supplerendevarme",
  "bygning.bygningensanvendelse",
  "bygning.samletbygningsareal",
  "bygning.bygningenssamledeerhvervsareal",
  "bygning.andetareal",
  "bygning.antaletager",
  "bygning.bebyggetareal",
  "bygning.ejendommensejerforholdskode",

  # Deljordstykke
  "deljordstykke",

  # Ice info - area / lot
  "ice_info.registreret_areal",
  "ice_info.registreret_areal_uden_vej",
  "ice_info.arealberegnet",
  "ice_info.beskyttelseslinier_andel",
  "ice_info.alle_bygninger_paa_delgrunden",
  "ice_info.alle_deljordstykke_id_ices",
  "ice_info.landzoneareal_opsummeret_delj",
  "ice_info.byzoneareal_opsummeret_delj",
  "ice_info.sommerhuszoneareal_opsummeret_delj",
  "ice_info.udenzoneareal_opsummeret_delj",
  "ice_info.js_per_delgrund",
  "ice_info.zone",
  "ice_info.unikke_anvendelseskategorier",
  "ice_info.antal_delgrund",
  "ice_info.antal_sfe",
  "ice_info.antal_bfe",

  # Ice info - location
  "ice_info.adresse.afstand_kyst",
  "ice_info.adresse.afstand_naermeste_trafikvejgennemfart_trafikvejfordeling",
  "ice_info.adresse.afstand_motorvej_motortrafikvej",
  "ice_info.adresse.afstand_stor_soe",
  "ice_info.adresse.areal_samlet_skov",
  "ice_info.adresse.afstand_jernbane_any",
  "ice_info.adresse.afstand_stort_vandloeb",
  "ice_info.adresse.afstand_vindmoelle_any",
  "ice_info.adresse.afstand_station_any",
  "ice_info.adresse.udsigtslaengde_hav",
  "ice_info.adresse.udsigtslaengde_soe"
)


unpack_element <- function(data, col, keep_empty = FALSE, ...) {
  data[[col]] <- lapply(
    data[[col]],
    \(x) if (is.list(x)) as.data.frame(x) else x
  )

  data %>% unnest(cols = all_of(col), keep_empty = keep_empty, ...)
}



# -------------------------
# 1. Load property and lot sales
# -------------------------

#2020
chunks <- list.files(path = "/data/data/cyan_data/20250613_vuraar2020/",
                     pattern = "salg.*\\.ndjson\\.xz$",
                     recursive = TRUE,
                     full.names = TRUE)

markedsdata <- future_map_dfr(chunks, function(x) {stream_in(xzfile(x, open = "rb"), verbose = FALSE)})

markedsdata[["salg_id_ice"]] <- map_dbl(.x = markedsdata[["vurinfo"]], .f = function(x){x[["salg"]][["salg_id_ice"]]})

enheder <- markedsdata %>%
           transmute(salg_id_ice, vurderingsejendom_id_ice, enhed = map(vurderingsenheder, "enhed")) %>%
           unpack_element("enhed")

bygninger <- markedsdata %>%
             transmute(salg_id_ice, vurderingsejendom_id_ice, bygning = map(vurderingsenheder, "bygning")) %>%
             unpack_element("bygning")

tekniske_anlaeg <- markedsdata %>%
                   transmute(salg_id_ice, vurderingsejendom_id_ice, tekniskanlaeg = map(vurderingsenheder, "tekniskanlaeg")) %>%
                   unpack_element("tekniskanlaeg")

delgrunde <- markedsdata %>%
             transmute(salg_id_ice, vurderingsejendom_id_ice, delgrund = map(vurderingsgrunde, "delgrund")) %>%
             unpack_element("delgrund")

vurinfo <- markedsdata %>%
           select(salg_id_ice, vurinfo) %>%
           unpack_element("vurinfo")

vurinfo[["salg.koebsdato"]] <- vurinfo$salg$koebsdato
vurinfo[["salg.ialtkoebesum"]] <- vurinfo$salg$ialtkoebesum
vurinfo[["salg.ejendomkategori"]] <- vurinfo$salg$ejendomkategori
vurinfo[["salg.anmeldelseidentifikator"]] <- vurinfo$salg$anmeldelseidentifikator
vurinfo[["salg.flereejendommeindikator"]] <- vurinfo$salg$flereejendommeindikator
vurinfo[["salg.ubebyggetgrund"]] <- vurinfo$salg$ubebyggetgrund
vurinfo[["salg.bygningsforholdkode"]] <- vurinfo$salg$bygningsforholdkode
vurinfo[["salg.flag"]] <- map_chr(vurinfo$salg$salg_flag, function(x) {paste(x[["flag_rule_id"]], collapse = ",")})
vurinfo[["adresse.koordinatnord"]] <- vurinfo$adresse$etrs89koordinatnord
vurinfo[["adresse.koordinatoest"]] <- vurinfo$adresse$etrs89koordinatoest

#saml salg
salg_2020 <- full_join(enheder,
                       delgrunde,
                       by = c("salg_id_ice", "vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"),
                       relationship = "many-to-one")


#berig salg med vurinfo
salg_2020 <- left_join(x = salg_2020,
                       y = vurinfo,
                       by = c("salg_id_ice", "vurderingsejendom_id_ice"))


# For overlapping fields, prioritize information from enhed/delgrund (.x).
# Only use the corresponding vurinfo value (.y) when the enhed/delgrund value is NA.

pairs <- sub("\\.x$", "", grep("\\.x$", names(salg_2020), value = TRUE))
pairs <- pairs[paste0(pairs, ".y") %in% names(salg_2020)]

for (x in pairs) {salg_2020[[x]] <- coalesce(salg_2020[[paste0(x, ".x")]],  salg_2020[[paste0(x, ".y")]])}

salg_2020 <- salg_2020 %>% select(-all_of(c(paste0(pairs, ".x"), paste0(pairs, ".y"))))

# Identify sales containing at least one unit, building or technical installation
bygningssalg <- bind_rows(enheder %>% select(salg_id_ice, vurderingsejendom_id_ice),
                          bygninger %>% select(salg_id_ice, vurderingsejendom_id_ice),
                          tekniske_anlaeg %>% select(salg_id_ice, vurderingsejendom_id_ice)) %>%
                 distinct()

# Property sales: sale/property contains a unit, building or technical installation
ejendomssalg_2020 <- salg_2020 %>% semi_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))

# Lot sales: sale/property contains none of the above
grundsalg_2020 <- salg_2020 %>% anti_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))


#2024
chunks <- list.files(path = "/data/data/cyan_data/20250601_vuraar2024/",
                     pattern = "salg.*\\.ndjson\\.xz$",
                     recursive = TRUE,
                     full.names = TRUE)

markedsdata <- future_map_dfr(chunks, function(x) {stream_in(xzfile(x, open = "rb"), verbose = FALSE)})

markedsdata[["salg_id_ice"]] <- map_dbl(.x = markedsdata[["vurinfo"]], .f = function(x){x[["salg"]][["salg_id_ice"]]})

enheder <- markedsdata %>%
           transmute(salg_id_ice, vurderingsejendom_id_ice, enhed = map(vurderingsenheder, "enhed")) %>%
           unpack_element("enhed")

bygninger <- markedsdata %>%
             transmute(salg_id_ice, vurderingsejendom_id_ice, bygning = map(vurderingsenheder, "bygning")) %>%
             unpack_element("bygning")

tekniske_anlaeg <- markedsdata %>%
                   transmute(salg_id_ice, vurderingsejendom_id_ice, tekniskanlaeg = map(vurderingsenheder, "tekniskanlaeg")) %>%
                   unpack_element("tekniskanlaeg")

delgrunde <- markedsdata %>%
             transmute(salg_id_ice, vurderingsejendom_id_ice, delgrund = map(vurderingsgrunde, "delgrund")) %>%
             unpack_element("delgrund")

vurinfo <- markedsdata %>%
           select(salg_id_ice, vurinfo) %>%
           unpack_element("vurinfo")

vurinfo[["salg.koebsdato"]] <- vurinfo$salg$koebsdato
vurinfo[["salg.ialtkoebesum"]] <- vurinfo$salg$ialtkoebesum
vurinfo[["salg.ejendomkategori"]] <- vurinfo$salg$ejendomkategori
vurinfo[["salg.anmeldelseidentifikator"]] <- vurinfo$salg$anmeldelseidentifikator
vurinfo[["salg.flereejendommeindikator"]] <- vurinfo$salg$flereejendommeindikator
vurinfo[["salg.ubebyggetgrund"]] <- vurinfo$salg$ubebyggetgrund
vurinfo[["salg.bygningsforholdkode"]] <- vurinfo$salg$bygningsforholdkode
vurinfo[["salg.flag"]] <- map_chr(vurinfo$salg$salg_flag, function(x) {paste(x[["flag_rule_id"]], collapse = ",")})
vurinfo[["adresse.koordinatnord"]] <- vurinfo$adresse$etrs89koordinatnord
vurinfo[["adresse.koordinatoest"]] <- vurinfo$adresse$etrs89koordinatoest


#saml salg
salg_2024 <- full_join(enheder,
                       delgrunde,
                       by = c("salg_id_ice", "vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"),
                       relationship = "many-to-one")


#berig salg med vurinfo
salg_2024 <- left_join(x = salg_2024,
                       y = vurinfo,
                       by = c("salg_id_ice", "vurderingsejendom_id_ice"))


# For overlapping fields, prioritize information from enhed/delgrund (.x).
# Only use the corresponding vurinfo value (.y) when the enhed/delgrund value is NA.

pairs <- sub("\\.x$", "", grep("\\.x$", names(salg_2024), value = TRUE))
pairs <- pairs[paste0(pairs, ".y") %in% names(salg_2024)]

for (x in pairs) {salg_2024[[x]] <- coalesce(salg_2024[[paste0(x, ".x")]],  salg_2024[[paste0(x, ".y")]])}

salg_2024 <- salg_2024 %>% select(-all_of(c(paste0(pairs, ".x"), paste0(pairs, ".y"))))


# Identify sales containing at least one unit, building or technical installation
bygningssalg <- bind_rows(enheder %>% select(salg_id_ice, vurderingsejendom_id_ice),
                          bygninger %>% select(salg_id_ice, vurderingsejendom_id_ice),
                          tekniske_anlaeg %>% select(salg_id_ice, vurderingsejendom_id_ice)) %>%
                distinct()

# Property sales: sale/property contains a unit, building or technical installation
ejendomssalg_2024 <- salg_2024 %>% semi_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))

# Lot sales: sale/property contains none of the above
grundsalg_2024 <- salg_2024 %>% anti_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))

#saml ejendomssalg og select relevante felter
ejendomssalg <- bind_rows(ejendomssalg_2020, ejendomssalg_2024) %>%
                distinct(vurderingsejendomid, enhed.enhed_id_ice, delgrund_info.delgrund_ids, salg.koebsdato, .keep_all = TRUE) %>%
                select(any_of(variables))

#saml grundsalg og select relevante felter
grundsalg <- bind_rows(grundsalg_2020, grundsalg_2024) %>%
             distinct(vurderingsejendomid, delgrund_info.delgrund_ids, salg.koebsdato, .keep_all = TRUE) %>%
             select(any_of(variables))

# -------------------------
# 2. Load properties
# -------------------------

chunks <- list.files(path = "/data/data/cyan_data/20250601_vuraar2024/",
                     pattern = "^[0-9]+-[0-9]+\\.ndjson\\.xz$",
                     recursive = TRUE,
                     full.names = TRUE)

vurderingsdata <- future_map_dfr(chunks, \(x) stream_in(xzfile(x, open = "rb"), verbose = FALSE))


enheder <- vurderingsdata %>%
           transmute(vurderingsejendom_id_ice, enhed = map(vurderingsenheder, "enhed")) %>%
           unpack_element("enhed")

delgrunde <- vurderingsdata %>%
             transmute(vurderingsejendom_id_ice, delgrund = map(vurderingsgrunde, "delgrund")) %>%
             unpack_element("delgrund")

vurinfo <- vurderingsdata %>%
           select(vurinfo) %>%
           unpack_element("vurinfo")

vurderingsejendomme <- full_join(enheder,
                                 delgrunde,
                                 by = c("vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"),
                                 relationship = "many-to-one")

vurderingsejendomme <- left_join(vurderingsejendomme,
                                 vurinfo,
                                 by = c("vurderingsejendom_id_ice"))


# For overlapping fields, prioritize information from enhed/delgrund (.x).
# Only use the corresponding vurinfo value (.y) when the enhed/delgrund value is NA.

pairs <- sub("\\.x$", "", grep("\\.x$", names(vurderingsejendomme), value = TRUE))
pairs <- pairs[paste0(pairs, ".y") %in% names(vurderingsejendomme)]

for (x in pairs) {vurderingsejendomme[[x]] <- coalesce(vurderingsejendomme[[paste0(x, ".x")]],  vurderingsejendomme[[paste0(x, ".y")]])}

vurderingsejendomme <- vurderingsejendomme %>%
                       select(-all_of(c(paste0(pairs, ".x"), paste0(pairs, ".y")))) %>%
                       select(any_of(variables))

# -------------------------
# 3. Enrich view variables of sales with information from properties
# -------------------------

antag_historik <- TRUE
antag_historik_felter <- c("ice_info.adresse.udsigtslaengde_hav", "ice_info.adresse.udsigtslaengde_soe")

if (antag_historik){

  ejendomssalg <- ejendomssalg %>%
                  left_join(vurderingsejendomme %>%
                  select(vurderingsejendom_id_ice, enhed.enhed_id_ice, delgrund_info.delgrund_ids, all_of(antag_historik_felter)) %>%
                  rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)), by = c("vurderingsejendom_id_ice", "enhed.enhed_id_ice", "delgrund_info.delgrund_ids")) %>%
                  mutate(ice_info.adresse.udsigtslaengde_hav = coalesce(ice_info.adresse.udsigtslaengde_hav_ny, ice_info.adresse.udsigtslaengde_hav),
                         ice_info.adresse.udsigtslaengde_soe = coalesce(ice_info.adresse.udsigtslaengde_soe_ny, ice_info.adresse.udsigtslaengde_soe)) %>%
                  select(-ends_with("_ny"))

  grundsalg <- grundsalg %>%
               left_join(vurderingsejendomme %>%
               select(vurderingsejendom_id_ice, delgrund_info.delgrund_ids, all_of(antag_historik_felter)) %>%
               slice(1) %>%
               rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)), by = c("vurderingsejendom_id_ice", "delgrund_info.delgrund_ids")) %>%
               mutate(ice_info.adresse.udsigtslaengde_hav = coalesce(ice_info.adresse.udsigtslaengde_hav_ny, ice_info.adresse.udsigtslaengde_hav),
                      ice_info.adresse.udsigtslaengde_soe = coalesce(ice_info.adresse.udsigtslaengde_soe_ny, ice_info.adresse.udsigtslaengde_soe)) %>%
               select(-ends_with("_ny"))

}


# -------------------------
# 4. Add some geographic variables
# -------------------------

geographical_subdivisions <- read.csv2("geographical_subdivisions.csv")

ejendomssalg <- left_join(ejendomssalg, geographical_subdivisions, by = "kommunenummer")
grundsalg <- left_join(grundsalg, geographical_subdivisions, by = "kommunenummer")
vurderingsejendomme <- left_join(vurderingsejendomme, geographical_subdivisions, by = "kommunenummer")

# -------------------------
# 5. Save data
# -------------------------

saveRDS(ejendomssalg, "ejendomssalg.rds")
saveRDS(grundsalg, "grundsalg.rds")
saveRDS(vurderingsejendomme, "vurderingsejendomme.rds")

