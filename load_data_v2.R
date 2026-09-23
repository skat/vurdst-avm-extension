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
# The resulting datasets are restricted to the variables needed for the
# subsequent analysis. Duplicate sales arising from overlapping data
# deliveries are removed.
#
# Sales observations are enriched with sales flags indicating the type of
# transaction. Ocean- and lake-view variables are subsequently updated using
# the corresponding variables from the 2024 property data, which contain more
# up-to-date information. Finally, all three datasets are enriched with
# selected geographic variables.
#
# The script saves three datasets:
#
#   - vurderingsejendomme_ny.rds:
#       Danish properties as of 1 January 2024.
#
#   - ejendomssalg_ny.rds:
#       Danish property sales from 1 January 2014 to 1 January 2024.
#
#   - grundsalg_ny.rds:
#       Danish lot sales from 1 January 2014 to 1 January 2024.
#
# The resulting files are used as inputs for the subsequent construction of
# the modelling dataset.
# =============================================================================


# =============================================================================
# 0. Setup
# =============================================================================

library(dplyr)
library(stringr)
library(jsonlite)
library(future)
library(purrr)
library(furrr)
library(tidyr)


# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------

path_2020 <- "/data/data/cyan_data/20250613_vuraar2020/"
path_2024 <- "/data/data/cyan_data/20250601_vuraar2024/"

property_checkpoint_dir <- "checkpoints/vurderingsejendomme"

dir.create(property_checkpoint_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Variables retained in the extracted datasets
# -----------------------------------------------------------------------------

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


# =============================================================================
# 1. Helper functions
# =============================================================================


# -----------------------------------------------------------------------------
# Unpack nested element
# -----------------------------------------------------------------------------

unpack_element <- function(data, col, keep_empty = FALSE, ...) {

  data[[col]] <- lapply(data[[col]], \(x) if (is.list(x)) as.data.frame(x) else x)

  data %>% unnest(cols = all_of(col), keep_empty = keep_empty, ...)

}


# -----------------------------------------------------------------------------
# Resolve duplicate columns created by joins
# -----------------------------------------------------------------------------
#
# For variables occurring in both joined datasets, information from the
# left-hand dataset (.x) is prioritized. The value from the right-hand
# dataset (.y) is used only when the former is missing.
# -----------------------------------------------------------------------------

coalesce_joined_columns <- function(data) {

  pairs <- sub("\\.x$", "", grep("\\.x$", names(data), value = TRUE))

  pairs <- pairs[paste0(pairs, ".y") %in% names(data)]

  for (x in pairs) {

    data[[x]] <- coalesce(data[[paste0(x, ".x")]], data[[paste0(x, ".y")]])

  }

  data %>% select(-all_of(c(paste0(pairs, ".x"), paste0(pairs, ".y"))))

}


# -----------------------------------------------------------------------------
# Extract property and lot sales from one valuation-year delivery
# -----------------------------------------------------------------------------

extract_sales <- function(path) {

  message("Extracting sales from: ", path)

  chunks <- list.files(path = path,
                       pattern = "salg.*\\.ndjson\\.xz$",
                       recursive = TRUE,
                       full.names = TRUE)

  if (length(chunks) == 0L) {stop("No sales files found in: ", path)}


  # ---------------------------------------------------------------------------
  # Load sales data
  # ---------------------------------------------------------------------------

  markedsdata <- future_map_dfr(chunks, \(x) stream_in(xzfile(x, open = "rb"), verbose = FALSE))

  # ---------------------------------------------------------------------------
  # Extract sale identifier
  # ---------------------------------------------------------------------------

  markedsdata[["salg_id_ice"]] <- map_dbl(markedsdata[["vurinfo"]], \(x) x[["salg"]][["salg_id_ice"]])

  # ---------------------------------------------------------------------------
  # Extract units
  # ---------------------------------------------------------------------------

  enheder <- markedsdata %>%
             transmute(salg_id_ice, vurderingsejendom_id_ice, enhed = map(vurderingsenheder, "enhed")) %>%
             unpack_element("enhed")


  # ---------------------------------------------------------------------------
  # Extract buildings
  # ---------------------------------------------------------------------------

  bygninger <- markedsdata %>%
               transmute(salg_id_ice, vurderingsejendom_id_ice, bygning = map(vurderingsenheder, "bygning")) %>%
               unpack_element("bygning")

  # ---------------------------------------------------------------------------
  # Extract technical installations
  # ---------------------------------------------------------------------------

  tekniske_anlaeg <- markedsdata %>%
                     transmute(salg_id_ice, vurderingsejendom_id_ice, tekniskanlaeg = map(vurderingsenheder, "tekniskanlaeg")) %>%
                     unpack_element("tekniskanlaeg")


  # ---------------------------------------------------------------------------
  # Extract land parcels
  # ---------------------------------------------------------------------------

  delgrunde <- markedsdata %>%
               transmute(salg_id_ice, vurderingsejendom_id_ice, delgrund = map(vurderingsgrunde, "delgrund")) %>%
               unpack_element("delgrund")


  # ---------------------------------------------------------------------------
  # Extract property-level information
  # ---------------------------------------------------------------------------

  vurinfo <- markedsdata %>%
             select(salg_id_ice, vurinfo) %>%
             unpack_element("vurinfo")


  # Sale information
  vurinfo[["salg.koebsdato"]] <- vurinfo$salg$koebsdato
  vurinfo[["salg.ialtkoebesum"]] <- vurinfo$salg$ialtkoebesum
  vurinfo[["salg.ejendomkategori"]] <- vurinfo$salg$ejendomkategori
  vurinfo[["salg.anmeldelseidentifikator"]] <- vurinfo$salg$anmeldelseidentifikator
  vurinfo[["salg.flereejendommeindikator"]] <- vurinfo$salg$flereejendommeindikator
  vurinfo[["salg.ubebyggetgrund"]] <- vurinfo$salg$ubebyggetgrund
  vurinfo[["salg.bygningsforholdkode"]] <- vurinfo$salg$bygningsforholdkode
  vurinfo[["salg.flag"]] <- map_chr(vurinfo$salg$salg_flag, \(x) paste(x[["flag_rule_id"]], collapse = ","))

  # Coordinates
  vurinfo[["adresse.koordinatnord"]] <- vurinfo$adresse$etrs89koordinatnord
  vurinfo[["adresse.koordinatoest"]] <- vurinfo$adresse$etrs89koordinatoest


  # Raw nested data are no longer required

  rm(markedsdata)
  gc()


  # ---------------------------------------------------------------------------
  # Combine units and land parcels
  # ---------------------------------------------------------------------------

  salg <- full_join(enheder, delgrunde, by = c("salg_id_ice", "vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"), relationship = "many-to-one")


  # ---------------------------------------------------------------------------
  # Add property-level information
  # ---------------------------------------------------------------------------

  salg <- left_join(salg, vurinfo, by = c("salg_id_ice", "vurderingsejendom_id_ice"))

  salg <- coalesce_joined_columns(salg)

  rm(vurinfo, delgrunde)
  gc()


  # ---------------------------------------------------------------------------
  # Identify developed and vacant properties
  # ---------------------------------------------------------------------------
  #
  # A sale/property combination is classified as developed if it contains at
  # least one unit, building, or technical installation.
  # ---------------------------------------------------------------------------

  bygningssalg <- bind_rows(enheder %>% select(salg_id_ice, vurderingsejendom_id_ice),
                            bygninger %>% select(salg_id_ice, vurderingsejendom_id_ice),
                            tekniske_anlaeg %>% select(salg_id_ice, vurderingsejendom_id_ice)) %>%
                  distinct()


  # Property sales
  ejendomssalg <- salg %>% semi_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))


  # Lot sales
  grundsalg <- salg %>% anti_join(bygningssalg, by = c("salg_id_ice", "vurderingsejendom_id_ice"))


  # Retain only variables required in subsequent analyses
  ejendomssalg <- ejendomssalg %>% select(any_of(variables))
  grundsalg <- grundsalg %>% select(any_of(variables))


  # ---------------------------------------------------------------------------
  # Clean up
  # ---------------------------------------------------------------------------

  rm(salg, enheder, bygninger, tekniske_anlaeg, bygningssalg)

  gc()


  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------

  list(ejendomssalg = ejendomssalg, grundsalg = grundsalg)
}


# -----------------------------------------------------------------------------
# Process one property-data chunk
# -----------------------------------------------------------------------------

process_vurderingsejendomme_chunk <- function(file) {

  # ---------------------------------------------------------------------------
  # Load one raw chunk
  # ---------------------------------------------------------------------------

  vurderingsdata <- stream_in(xzfile(file, open = "rb"), verbose = FALSE)

  # ---------------------------------------------------------------------------
  # Extract units
  # ---------------------------------------------------------------------------

  enheder <- vurderingsdata %>%
             transmute(vurderingsejendom_id_ice, enhed = map(vurderingsenheder, "enhed")) %>%
             unpack_element("enhed")


  # ---------------------------------------------------------------------------
  # Extract land parcels
  # ---------------------------------------------------------------------------

  delgrunde <- vurderingsdata %>%
               transmute(vurderingsejendom_id_ice, delgrund = map(vurderingsgrunde, "delgrund")) %>%
               unpack_element("delgrund")


  # ---------------------------------------------------------------------------
  # Extract property-level information
  # ---------------------------------------------------------------------------

  vurinfo <- vurderingsdata %>%
             select(vurinfo) %>%
             unpack_element("vurinfo")

  # Coordinates
  vurinfo[["adresse.koordinatnord"]] <- vurinfo$adresse$etrs89koordinatnord
  vurinfo[["adresse.koordinatoest"]] <- vurinfo$adresse$etrs89koordinatoest


  # Raw nested data are no longer required

  rm(vurderingsdata)
  gc()


  # ---------------------------------------------------------------------------
  # Combine units and land parcels
  # ---------------------------------------------------------------------------

  vurderingsejendomme_chunk <- full_join(enheder, delgrunde, by = c("vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"), relationship = "many-to-one")
  rm(enheder, delgrunde)
  gc()


  # ---------------------------------------------------------------------------
  # Add property-level information
  # ---------------------------------------------------------------------------

  vurderingsejendomme_chunk <- left_join(vurderingsejendomme_chunk, vurinfo, by = "vurderingsejendom_id_ice")
  rm(vurinfo)
  gc()


  # ---------------------------------------------------------------------------
  # Resolve overlapping fields
  # ---------------------------------------------------------------------------

  vurderingsejendomme_chunk <- coalesce_joined_columns(vurderingsejendomme_chunk)

  # ---------------------------------------------------------------------------
  # Retain required variables only
  # ---------------------------------------------------------------------------

  vurderingsejendomme_chunk <- vurderingsejendomme_chunk %>% select(any_of(variables))

  gc()

  vurderingsejendomme_chunk
}


# =============================================================================
# 2. Load property and lot sales
# =============================================================================


# -----------------------------------------------------------------------------
# 2020 delivery
# -----------------------------------------------------------------------------

sales_2020 <- extract_sales(path_2020)


# -----------------------------------------------------------------------------
# 2024 delivery
# -----------------------------------------------------------------------------

sales_2024 <- extract_sales(path_2024)


# -----------------------------------------------------------------------------
# Combine deliveries and remove duplicate sales
# -----------------------------------------------------------------------------
#
# The 2020 and 2024 deliveries overlap in time. Sales appearing in both
# deliveries are therefore retained only once.
# -----------------------------------------------------------------------------

ejendomssalg <- bind_rows(sales_2020$ejendomssalg, sales_2024$ejendomssalg) %>%
                distinct(vurderingsejendomid, enhed.enhed_id_ice, delgrund_info.delgrund_ids, salg.koebsdato, .keep_all = TRUE) %>%
                select(any_of(variables))


grundsalg <- bind_rows(sales_2020$grundsalg, sales_2024$grundsalg) %>%
             distinct(vurderingsejendomid, delgrund_info.delgrund_ids, salg.koebsdato, .keep_all = TRUE) %>%
             select(any_of(variables))

rm(sales_2020, sales_2024)
gc()


# =============================================================================
# 3. Load properties
# =============================================================================


# -----------------------------------------------------------------------------
# Locate property chunks
# -----------------------------------------------------------------------------

chunks <- list.files(path = path_2024, pattern = "^[0-9]+-[0-9]+\\.ndjson\\.xz$", recursive = TRUE, full.names = TRUE)

if (length(chunks) == 0L) {stop("No property files found in: ", path_2024)}


# -----------------------------------------------------------------------------
# Process property chunks sequentially
# -----------------------------------------------------------------------------
#
# The raw property files are very large. They are therefore processed one at
# a time. Each processed chunk is reduced to the required variables and saved
# as an intermediate RDS file before the next raw chunk is loaded.
#
# Existing checkpoint files are reused. This makes it possible to resume the
# extraction after an interrupted run without reprocessing completed chunks.
# -----------------------------------------------------------------------------

for (i in seq_along(chunks)) {

  checkpoint_file <- file.path(property_checkpoint_dir, paste0("vurderingsejendomme_", sprintf("%03d", i), ".rds"))

  if (!file.exists(checkpoint_file)) {

    message("Processing property chunk ", i, " of ", length(chunks), ": ", basename(chunks[i]))

    chunk <- process_vurderingsejendomme_chunk(chunks[i])

    saveRDS(chunk, checkpoint_file)

    rm(chunk)
    gc()

  } else {

    message("Property chunk ", i, " of ", length(chunks), " already processed.")
  }
}


# -----------------------------------------------------------------------------
# Combine processed property chunks
# -----------------------------------------------------------------------------

processed_chunks <- file.path(property_checkpoint_dir, paste0("vurderingsejendomme_", sprintf("%03d", seq_along(chunks)), ".rds"))

# Check that all expected chunks exist before combining them

stopifnot(all(file.exists(processed_chunks)))

vurderingsejendomme <- map_dfr(processed_chunks, readRDS)

gc()

# =============================================================================
# 4. Enrich view variables of sales with 2024 property information
# =============================================================================

antag_historik <- TRUE
antag_historik_felter <- c("ice_info.adresse.udsigtslaengde_hav", "ice_info.adresse.udsigtslaengde_soe")


if (antag_historik) {


  # ---------------------------------------------------------------------------
  # Property-sale lookup
  # ---------------------------------------------------------------------------

  view_lookup_property <- vurderingsejendomme %>%
                          select(vurderingsejendom_id_ice, enhed.enhed_id_ice, delgrund_info.delgrund_ids, all_of(antag_historik_felter)) %>%
                          distinct()


  # Check that the lookup key is unique.
  #
  # If this fails, the property data contain multiple different observations
  # for the same property/unit/parcel combination and should be investigated
  # before the join is performed.

  stopifnot(!anyDuplicated(view_lookup_property %>% select(vurderingsejendom_id_ice, enhed.enhed_id_ice, delgrund_info.delgrund_ids)))


  # ---------------------------------------------------------------------------
  # Update property sales
  # ---------------------------------------------------------------------------

  ejendomssalg <- ejendomssalg %>%
                  left_join(view_lookup_property %>% rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)),
                            by = c("vurderingsejendom_id_ice", "enhed.enhed_id_ice", "delgrund_info.delgrund_ids"),
                            relationship = "many-to-one") %>%
                  mutate(ice_info.adresse.udsigtslaengde_hav = coalesce(ice_info.adresse.udsigtslaengde_hav_ny, ice_info.adresse.udsigtslaengde_hav),
                         ice_info.adresse.udsigtslaengde_soe = coalesce(ice_info.adresse.udsigtslaengde_soe_ny, ice_info.adresse.udsigtslaengde_soe)) %>%
                  select(-ends_with("_ny"))


  rm(view_lookup_property)
  gc()


  # ---------------------------------------------------------------------------
  # Lot-sale lookup
  # ---------------------------------------------------------------------------
  #
  # A property can occur more than once in vurderingsejendomme because of
  # units. For the lot-sale lookup we need only one observation for each
  # property/parcel/view combination.
  # ---------------------------------------------------------------------------

  view_lookup_lot <- vurderingsejendomme %>%
                     select(vurderingsejendom_id_ice, delgrund_info.delgrund_ids, all_of(antag_historik_felter)) %>%
                     distinct()


  # Check whether a property/parcel has conflicting view information.
  #
  # If this check fails, simply taking the first observation would be
  # arbitrary and the conflicting records should be investigated.

  conflicting_lot_views <- view_lookup_lot %>%
                           count(vurderingsejendom_id_ice, delgrund_info.delgrund_ids, name = "n") %>%
                           filter(n > 1)


  if (nrow(conflicting_lot_views) > 0L) {

    stop("Multiple sets of view variables found for ", nrow(conflicting_lot_views), " property/parcel combinations.")

  }


  # ---------------------------------------------------------------------------
  # Update lot sales
  # ---------------------------------------------------------------------------

  grundsalg <- grundsalg %>%
               left_join(view_lookup_lot %>% rename_with(~ paste0(.x, "_ny"), all_of(antag_historik_felter)),
                         by = c("vurderingsejendom_id_ice", "delgrund_info.delgrund_ids"),
                         relationship = "many-to-one") %>%
               mutate(ice_info.adresse.udsigtslaengde_hav = coalesce(ice_info.adresse.udsigtslaengde_hav_ny, ice_info.adresse.udsigtslaengde_hav),
                      ice_info.adresse.udsigtslaengde_soe = coalesce(ice_info.adresse.udsigtslaengde_soe_ny, ice_info.adresse.udsigtslaengde_soe)) %>%
               select(-ends_with("_ny"))


  rm(view_lookup_lot, conflicting_lot_views)
  gc()
}


# =============================================================================
# 5. Add geographic variables
# =============================================================================

geographical_subdivisions <- read.csv2("geographical_subdivisions.csv")


# Check that municipality number uniquely identifies a row in the lookup table

stopifnot(!anyDuplicated(geographical_subdivisions$kommunenummer))

ejendomssalg <- left_join(ejendomssalg, geographical_subdivisions, by = "kommunenummer", relationship = "many-to-one")
grundsalg <- left_join(grundsalg, geographical_subdivisions, by = "kommunenummer", relationship = "many-to-one")
vurderingsejendomme <- left_join(vurderingsejendomme, geographical_subdivisions, by = "kommunenummer", relationship = "many-to-one")


# =============================================================================
# 6. Final checks
# =============================================================================

message("Property sales: ", format(nrow(ejendomssalg), big.mark = ","))
message("Lot sales: ", format(nrow(grundsalg), big.mark = ","))
message("Properties: ", format(nrow(vurderingsejendomme), big.mark = ","))


# Check that all required output objects contain rows

stopifnot(nrow(ejendomssalg) > 0L, nrow(grundsalg) > 0L, nrow(vurderingsejendomme) > 0L)


# =============================================================================
# 7. Save data
# =============================================================================

saveRDS(ejendomssalg, "ejendomssalg_ny.rds")
saveRDS(grundsalg, "grundsalg_ny.rds")
saveRDS(vurderingsejendomme, "vurderingsejendomme_ny.rds")
