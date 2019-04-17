
library(tidyverse)

alta_lake_drying <- readxl::read_excel("data-raw/alta_lake_pb210.xlsx", sheet = "drying")
alta_lake_210Pb_raw <- readxl::read_excel("data-raw/alta_lake_pb210.xlsx", sheet = "pb210")

alta_lake_210Pb <- alta_lake_210Pb_raw %>%
  mutate(
    sample_id = paste0("AL-GC2-", section_top_cm),
    depth = (section_top_cm + section_bottom_cm) / 2
  ) %>%
  unite("date_counted", starts_with("date_counted"), sep = "-") %>%
  unite("date_extracted", starts_with("date_extracted"), sep = "-") %>%
  mutate_at(vars(starts_with("date_")), as.Date) %>%
  select(sample_id, depth, everything())

alta_lake_pb210 <- alta_lake_drying %>%
  left_join(
    alta_lake_210Pb %>%
      transmute(
        sample_id,
        total_pb210_Bq_kg = activity_210Pb_Bq_g * 1000,
        total_pb210_sd = total_pb210_Bq_kg * activity_210Pb_sd_percent / 100,
        published_age_yr = max(crs_age_section_top_ad, na.rm = TRUE) - crs_age_section_top_ad,
        published_age_sd = crs_age_sd_yr
      ),
    by = "sample_id"
  ) %>%
  select(sample_id, depth_cm, starts_with("total_pb210"), everything())

usethis::use_data(alta_lake_pb210, overwrite = TRUE)
