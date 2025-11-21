# GOAL: clean data for auto regressive component of the price index (eq.1) 





# --- 0) Packages --------------------------------------------------------------
# install.packages("sf", type = "binary", repos = "https://cloud.r-project.org")
# if (!requireNamespace("janitor", quietly=TRUE))     install.packages("janitor")

library(sf)
library(dplyr)
library(lubridate)
library(stringr)
library(quantmod)

# --- 1) Data Import -----------------------------------------------------------
# transfers (geopackage - to preserve data formatting)
s <- st_read("D:/R_data/00_Data_Clean/sales_pre16.gpkg", layer = "sales_pre16") 
# s <- read_csv("D:/R_data/00_Data_Clean/sales_pre16.csv")

# --- 2) Basic screens ---------------------------------------------------------
# keep rows with sale info and a property id; restrict to pre-2016
s <- s %>%
  filter(!is.na(date)) %>%
  filter(date <= as.Date("2015-12-31")) %>%
  filter(!is.na(total_consideration)) %>%
  # filter(total_consideration == cash_consideration + other_consideration) %>%  # price consistency
  mutate(.price_diff = abs(total_consideration - (cash_consideration + other_consideration))) %>%  # (tolerant price consistency)
  filter(.price_diff <= 1) %>%   # allow small rounding error; tweak to <= 5 if your data are in cents
  dplyr::select(-.price_diff) %>%  
  filter(discrepancy == FALSE)

# ZIP (first 5 chars, keep as character)
s <- s %>%
  mutate(zip = ifelse(is.na(zip_code), NA_character_, substr(as.character(zip_code), 1, 5)))

# remove institutional-to-institution transfers (both sides entities)
s <- s %>%
  filter(!(grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantors,  ignore.case = TRUE) &
             grepl("LLC|INC|CORP|LP|LLP|Corporation|Trust", grantees, ignore.case = TRUE)))

# --- 3) De-duplication (exact & near) ----------------------------------------
# (i) exact duplicates by document × OPA
any(duplicated(s[c("document_id", "opa_account_number")]))

# (ii) "near duplicates": same date × OPA × rounded price => keep first
s <- s %>%
  mutate(price_r = round(total_consideration, -2)) %>%
  { 
    message("Any near duplicates? ", any(duplicated(.[c("date", "opa_account_number", "price_r")])))
    distinct(., date, opa_account_number, price_r, .keep_all = TRUE)
  } %>%
  dplyr::select(-price_r)

# --- 4) Normalize prices to $2019M1 ------------------------------------------
#  put prices in real $2019M1.
s <- s %>%
  mutate(total_consideration_nominal = total_consideration)  # keep nominal for FMV/tax checks

suppressMessages(getSymbols("CPIAUCSL", src = "FRED", auto.assign = TRUE))
cpi_tbl <- data.frame(
  month = floor_date(as.Date(index(CPIAUCSL)), "month"),
  cpi   = as.numeric(CPIAUCSL$CPIAUCSL)
) %>% distinct(month, .keep_all = TRUE)
base_cpi <- cpi_tbl$cpi[cpi_tbl$month == as.Date("2019-01-01")]

s <- s %>%
  mutate(month = floor_date(date, "month")) %>%
  left_join(cpi_tbl, by = "month") %>%
  mutate(total_consideration = total_consideration * (base_cpi / cpi)) %>%  # real $2019M1
  dplyr::select(-cpi, -month)

# $10k real floor (as in paper)
s <- s %>% filter(total_consideration > 10000)  # threshold in real $2019M1

# Within-quarter duplicates: keep **highest** price per OPA × quarter
s <- s %>%
  mutate(sale_qtr = paste0(year(date), "Q", quarter(date))) %>%
  group_by(opa_account_number, sale_qtr) %>%
  slice_max(order_by = total_consideration, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  dplyr::select(-sale_qtr)

# --- 5) Arms-length & plausibility screens (relevant subset) -----------------
# Document types: keep deed-type transactions
s <- s %>% 
  filter(document_type %in% c(
    "DEED",
    "DEED MISCELLANEOUS TAXABLE",
    "MISCELLANEOUS DEED TAXABLE",
    "MISCELLANEOUS DEED",
    "DEED MISCELLANEOUS",
    "DEED RTT - OTHER"
  ))

# FMV/price plausibility band (use *nominal* denominator saved earlier)
s <- s %>%
  mutate(ratio = fair_market_value / total_consideration_nominal) %>%  
  filter(ratio >= 0.1, ratio <= 3) %>%
  dplyr::select(-ratio)

# Tax-rate plausibility band (2%–6%)
s <- s %>%
  mutate(actual_tax_percent = 
           (coalesce(state_tax_amount, 0) + coalesce(local_tax_amount, 0)) / 
           total_consideration_nominal) %>%
  filter(actual_tax_percent >= 0.02, actual_tax_percent <= 0.06) %>%
  dplyr::select(-actual_tax_percent)


# --- 6) Anomalies (timing & extreme jumps) -----------------------------------
MIN_GAP_DAYS <- 365
MAX_ABS_DLN  <- 1  # |Δln(price)| > 1 ⇒ drop later sale

s <- s %>%
  arrange(opa_account_number, recording_date) %>%
  group_by(opa_account_number) %>%
  mutate(
    gap_days  = as.numeric(recording_date - lag(recording_date)),
    dln_price = log(total_consideration) - log(lag(total_consideration))
  ) %>%
  filter(is.na(gap_days) | gap_days >= MIN_GAP_DAYS) %>%
  filter(is.na(dln_price) | abs(dln_price) <= MAX_ABS_DLN) %>%
  ungroup() %>%
  dplyr::select(-gap_days, -dln_price)


# --- 7) Finalize (minimal fields for linking & lag price) --------------------
s <- s %>%
  mutate(
    ln_price = log(total_consideration), # real $2019M1
    opa_account_number = str_pad(opa_account_number, 9, pad = "0")  # moved into mutate
  ) %>%
  dplyr::select(
         opa_account_number, date, recording_date,
         price = total_consideration, ln_price,
         document_id, document_type, street_address, zip,
         matched_regmap, reg_map_id)

# note: price reported in 2019 Q1 prices

# --- 8) Export ----------------------------------------------------------------
# GeoPackage (to preserve formatting)
st_write(s, "D:/R_data/00_Data_Clean/sales_pre16_clean.gpkg",
         layer = "sales_pre16", append = FALSE)

# # Export
# write.csv(s, "D:/R_data/00_Data_Clean/sales_pre2016_clean.csv", row.names = FALSE)
