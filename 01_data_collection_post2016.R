# GOAL: sales dataset with all post 2016 sales and properties' structural characteristics 
# clean variables required for joining, other variables cleaned in data cleaning

# steps

setwd("D:/R_data")

{
  # install.packages("sf", type = "binary", repos = "https://cloud.r-project.org")
# if (!requireNamespace("digest", quietly=TRUE))     install.packages("digest")
}

# --- 0) Packages --------------------------------------------------------------
library(sf)  
library(lwgeom)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(digest) 


{
# extracting zip files
# unzip("D:/R_data/01_SHP/DOR_Parcel.zip", exdir = "D:/R_data/01_SHP/DOR_Parcel", unzip = "internal")
# unzip("D:/R_data/01_SHP/transfers2016_17.zip", 

# check
# shp_files <- list.files("D:/R_data/01_SHP/DOR_Parcel", pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
# shp_files   # should see at least one .shp path here
}

# --- 1) Data Import -----------------------------------------------------------

# ==============================================================================
# PARCELS 
# ==============================================================================
# https://opendataphilly.org/datasets/department-of-records-property-parcels/ 
#  Import parcels.shp
shp_files <- list.files("D:/R_data/01_SHP/DOR_Parcel", 
                        pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_files) == 1)  # fail fast if ambiguous
parcels <- st_read(shp_files[1], quiet = FALSE)

{
# Quick checks
# st_geometry_type(parcels)[1]   # should be POLYGON or MULTIPOLYGON
# st_crs(parcels)                # CRS info
# names(parcels)[1:10]           # confirm you see pin, basereg/mapreg, parcel, etc.
}
# ==============================================================================
# TRANSFERS
# ==============================================================================
# https://opendataphilly.org/datasets/real-estate-transfers/ 
# Import transfers.shp
shp_files1 <- list.files("D:/R_data/01_SHP/transfers2016_17", 
                         pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_files) == 1)  # fail fast if ambiguous
t1617 <- st_read(shp_files1[1], quiet = FALSE)

shp_files2 <- list.files("D:/R_data/01_SHP/transfers2018_19", 
                         pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_files) == 1)  # fail fast if ambiguous
t1819 <- st_read(shp_files2[1], quiet = FALSE)

shp_files3 <- list.files("D:/R_data/01_SHP/transfers2020", 
                        pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
stopifnot(length(shp_files) == 1)  # fail fast if ambiguous
t20 <- st_read(shp_files3[1], quiet = FALSE)

#import transfers.csv
{
#set column variables types to character to avoid parsing issues
# t20_ <- read_csv("transfers_2020-.csv",
#                            col_types = cols(.default = col_character()),
#                            na = c("", "NA", "N/A"),
#                            show_col_types = FALSE)
# t18_19 <- read_csv("transfers_2018-2019.csv",
#                    col_types = cols(.default = col_character()),
#                    na = c("", "NA", "N/A"),
#                    show_col_types = FALSE)
# t16_17 <- read_csv("transfers_2016-2017.csv",
#                              col_types = cols(.default = col_character()),
#                              na = c("", "NA", "N/A"),
#                              show_col_types = FALSE)
}

# ==============================================================================
# properties 
# ==============================================================================
# https://opendataphilly.org/datasets/philadelphia-properties-and-assessment-history/ 
# Import properties.csv
properties <- read_csv("opa_properties_public.csv")

{
# sanity check: same pin yields same address
# subset(parcels, pin == 1001570166, select = c(pin, addr_std))
# subset(properties, pin == 1001570166, select = c(pin, location))
}



# # --- 2) Data Wrangling ------------------------------------------------------

# ==============================================================================
# PARCELS 
# ==============================================================================
parcels <- parcels %>%
  st_make_valid()

# 1) select needed variables
parcels <- parcels %>%
  mutate(
    pin       = as.character(pin),
    mapreg    = as.character(mapreg),
    parcel    = as.character(parcel),
    condoflag = as.integer(condoflag)
  ) %>%
  select(pin, mapreg, parcel, condoflag)
gc()

  
# 2) drop nas and duplicates
# inspect nas
{
# # inspect of nas 
# sum(is.na(parcels$parcel)) # 379 duplicated - um(table(parcels$parcel)>1) - 1536
# sum(is.na(parcels$pin)) # 27840
# sum(is.na(parcels$condoflag)) #0
# sum(is.na(parcels$geometry) | st_is_empty(parcels)) #37
}
# drop nas
parcels <- parcels %>% 
  drop_na(pin, condoflag) %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))


# inspect duplicate pins
{
# parcels_pin_repeats <- parcels %>%
#   filter(!is.na(pin)) %>% 
#   group_by(pin) %>%
#   filter(n() > 1)
# nrow(parcels_pin_repeats) # 552
}
# drop duplicate pins
parcels <- parcels %>%
  group_by(pin) %>%
  filter(n() == 1) %>%
  ungroup()

# inspect duplicate geometries
st_crs(parcels)  # verify EPSG and units - ftUS
snap <- 1  # tolerance in CRS units
{
  # parcels_dups <- parcels %>%
  #   st_snap_to_grid(snap) %>% 
  #   st_buffer(0) %>% 
  #   # returns same nrow as  mutate(key = st_as_text(geometry)) but faster
  #   mutate(key = vapply(st_as_binary(geometry), digest, character(1))) %>% 
  #   group_by(key) %>% 
  #   filter(n() > 1) %>% 
  #   ungroup() %>% 
  #   select(-key)
  
# st_write(parcels_dups, "00_data_clean/parcels_dups.gpkg")
# 
}
# drop duplicate geometries
parcels <- parcels %>%
  st_snap_to_grid(snap) %>%
  st_buffer(0) %>%
  mutate(key = vapply(st_as_binary(geometry), digest, character(1))) %>%
  add_count(key) %>%
  filter(n == 1) %>%
  select(-key, -n)



# ==============================================================================
# TRANSFERS
# ==============================================================================

# 2016 - 2017
# drop nas and property count > 1
t1617 <- t1617 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry))
  # consider distinct instead
  # filter(!document_i %in% document_i[duplicated(document_i)])  # removes doc ids with duplicates

t1617 <- t1617 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons),total_cons != 0
  ) %>% 
  select(
    date                  = display_da,  # canonical date
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id (primary key) for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc,
    property_count        = property_c,
    grantors, 
    grantees,
    local_tax_amount      = local_tax_, # arms lengh transcations helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  ) 
gc()

# 2018 - 2019
# drop nas and property count > 1
t1819 <- t1819 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry)) 
  # filter(!document_i %in% document_i[duplicated(document_i)])  # removes doc ids with duplicates

t1819 <- t1819 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons),total_cons != 0
  ) %>% 
  select(
    date                  = display_da,
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id (primary key) for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc, 
    property_count        = property_c,
    grantors, 
    grantees, 
    local_tax_amount      = local_tax_, # arms lengh transcations helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  ) 
gc()

# 2020+
t20 <- t20 %>%
  filter(property_c == 1) %>% 
  st_make_valid() %>% 
  filter(!is.na(geometry) & !st_is_empty(geometry)) 
  # filter(!document_i %in% document_i[duplicated(document_i)])  # removes doc ids with duplicates

t20 <- t20 %>%
  mutate(
    recording_ = as.Date(recording_),
    total_cons = as.numeric(total_cons),  # readr::parse_number(as.character(total_cons))
    discrepanc = as.logical(discrepanc)
  ) %>%
  filter(
    !is.na(total_cons),total_cons != 0, 
    recording_ <= as.Date("2024-04-01")   # phase 4 begins on 4th Aug 2024
    )%>% 
  select(
    date                  = display_da,
    recording_date        = recording_,
    total_consideration   = total_cons,
    cash_consideration    = cash_consi, 
    other_consideration   = other_cons,
    assessed_value        = assessed_v, 
    street_address        = street_add, 
    common_level_ratio    = common_lev, 
    fair_market_value     = fair_marke, 
    zip_code              = zip_code,
    unit_number           = unit_num,
    condo_name            = condo_name,
    document_type         = document_t,
    document_id           = document_i, # id (primary key) for transfers
    matched_regmap        = matched_re,
    reg_map_id            = reg_map_id, 
    discrepancy           = discrepanc,
    property_count        = property_c,
    grantors, 
    grantees, 
    local_tax_amount      = local_tax_, # arms lengh transcations helpers
    local_tax_percent     = local_ta_1,
    state_tax_amount      = state_tax_, 
    state_tax_percent     = state_ta_1, 
    opa_account_number    = opa_accoun
  ) 
gc()


# ==============================================================================
# properties 
# ==============================================================================
# inspect nas and duplicates key variables for joining
# nas
# inspect
sum(is.na(properties$pin))
sum(is.na(properties$parcel_number))

# drop
properties <- properties %>% drop_na(pin, parcel_number)

# duplicates
{
sum(duplicated(properties$pin))
sum(duplicated(properties$parcel_number))
# 0
}


# to rename: new name = old name
properties <- properties %>%
  mutate(
    pin = as.character(pin),
    basements = as.factor(basements)
  ) %>%
  select(
    basements,
    building_code,
    building_code_description, # could be used for vacant land
    building_code_description_new,
    building_code_new,
    category_code,
    category_code_description,
    census_tract,
    central_air,
    condo_unit = unit,
    exterior_condition,
    fireplaces,
    fuel,
    garage_spaces,
    interior_condition,
    location,
    lot_depth = depth,
    lot_width = frontage,
    mailing_address = mailing_address_1,
    number_of_bathrooms,
    number_of_bedrooms,
    number_of_rooms,
    number_stories,
    parcel_number, # id (primary key)
    pin,
    quality_grade,
    sale_date,
    sale_price,
    separate_utilities,
    site_type,
    topography,
    total_area,
    total_livable_area,
    type_heater,
    unit, # condo
    view = view_type,
    year_built,
    year_built_estimate,
    zip_code
  ) %>%
  distinct(pin, .keep_all = TRUE)
gc()



# # --- 3) Spatial Join --------------------------------------------------------

# 1) transfers -> parcels (point in polygon join)
# Same CRS for spatial joins
t1617 <- st_transform(t1617, st_crs(parcels))
t1819 <- st_transform(t1819, st_crs(parcels))
t20 <- st_transform(t20,   st_crs(parcels))

# join
t1617_parc <- st_join(t1617, parcels, join = st_within)
t1819_parc <- st_join(t1819, parcels, join = st_within)
t20_parc  <- st_join(t20,   parcels, join = st_within)

# remove rows where a point matches more than one parcel polygon (e.g. because of overlap)
# as well as nas (where a point did not fall within any polygon in parcels)
t1617_parc <- t1617_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t1617_parc)

t1819_parc <- t1819_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t1819_parc)

t20_parc <- t20_parc %>%
  filter(!is.na(pin)) %>% 
  group_by(document_id) %>%   
  mutate(n_parcels = n_distinct(pin)) %>%
  ungroup() %>%
  filter(n_parcels == 1) %>%  
  select(-n_parcels)
nrow(t20_parc)

# % of rows dropped at each join
{
print((nrow(t1617) - nrow(t1617_parc))/nrow(t1617))
print((nrow(t1819) - nrow(t1819_parc))/nrow(t1819))
print((nrow(t20) - nrow(t20_parc))/nrow(t20))
}


# 2) transfers_parcels -> properties 
t1617_ready <- left_join(t1617_parc, properties, by = "pin")
t1819_ready <- left_join(t1819_parc, properties, by = "pin")
t20_ready   <- left_join(t20_parc,   properties, by = "pin")

# check if rows matched one to one
{
sum(nrow(t1617_ready) + nrow(t1819_ready) + nrow(t20_ready)) ==
  sum(nrow(t1617_parc) + nrow(t1819_parc) + nrow(t20_parc))
}

# --- 4) Finalizing --------------------------------------------------------------
{
# tidying up
# t1819_ready <- t1819_ready %>%
#   mutate(
#     date  = recording_date,
#     price = total_consideration,
#     source_range = "2018-2019"
#   )
# 
# t20_ready <- t20_ready %>%
#   mutate(
#     date  = recording_date,
#     price = total_consideration,
#     source_range = "2020"
#   )
}
# combine
sales_master <- bind_rows(t1617_ready, t1819_ready, t20_ready)

# order column names in logical order
order_cols <- c(
  # 1) Keys & main date
  "parcel_number", "pin",
  "date", "opa_account_number",
  
  # 2) Price-related (most important first)
  "total_consideration", "sale_price", "assessed_value", "fair_market_value",
  "common_level_ratio", "cash_consideration", "other_consideration",
  "property_count", "local_tax_amount", "local_tax_percent",
  "state_tax_amount", "state_tax_percent",
  
  # 3) Structure (most important to the left)
  "number_of_rooms", "total_livable_area", "total_area",
  "number_of_bedrooms", "number_of_bathrooms",
  "lot_width", "lot_depth", "number_stories",
  "year_built", "year_built_estimate",
  "garage_spaces", "basements", "fireplaces",
  "central_air", "separate_utilities", "type_heater", "fuel",
  "quality_grade", "exterior_condition", "interior_condition",
  "site_type", "topography", "view",
  "building_code", "building_code_description",
  "building_code_new", "building_code_description_new",
  "category_code", "category_code_description",
  
  # 4) Location & condo
  "street_address", "mailing_address", "unit_number",
  "condo_name", "condo_unit", "condoflag",
  "location", "census_tract", "zip_code.x", "zip_code.y",
  
  
  # 5) Other IDs & mapping
  "parcel", "mapreg", "reg_map_id", "matched_regmap",
  
  # 6) Docs, parties, other dates
  "document_type", "document_id", "grantors", "grantees",
  "recording_date", "sale_date",
  
  # 7) Misc
  "discrepancy",
  
  # 8) Geometry last
  "geom"
)

sales_master <- sales_master %>%
  select(any_of(order_cols), everything())



# --- 5) Export --------------------------------------------------------------

#export (gpkg)
# GeoPackage 
st_write(sales_master, "D:/R_data/00_Data_Clean/sales_master_1625.gpkg",
         layer = "sales_master", append = FALSE)


# # Shapefle (shp)
# st_write(sales_master, "D:/R_data/00_Data_Clean/sales_master.shp",
#          delete_layer = TRUE)
# 
# # csv
# sm_no_geom <- st_drop_geometry(sales_master)
# write.csv(sm_no_geom, "D:/R_data/00_Data_Clean/sales_master.csv", row.names = FALSE)






