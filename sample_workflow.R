# README -----------------------------------------------------------------------

# A simple of workflow for D. Uhey demonstrating standardizing observations to
# trap count (by average), and changing data format to wide.


# libraries --------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)


# download data ----------------------------------------------------------------

url  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-cap/643/3/b1f57b53f91f113712aad26ffd33de60"
download <- tempfile()

download.file(
  url = url,
  destfile = download,
  method = "curl"
)


# format data ------------------------------------------------------------------

observations <- read_csv(download) %>%
  # combine all size classes
  rowwise() %>%
  mutate(allsizes = sum(lt2mm, `_2_5mm`, `_5_10mm`, gt10mm, unsized, na.rm = T)) %>%
  ungroup() %>%
  # sum observations by collection * taxon
  group_by(site_code, sample_date, trap_count, display_name) %>%
  summarise(total = sum(allsizes)) %>%
  ungroup() %>%
  # standardize observations to trap count
  mutate(average = total / trap_count) %>%
  select(
    -trap_count,
    -total
    ) %>%
  # pivot to wide-data format
  pivot_wider(
    names_from = "display_name",
    values_from = "average"
  )
