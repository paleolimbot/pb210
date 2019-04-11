
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
  select(sample_id, depth, everything()) %>%
  print()


usethis::use_data(alta_lake_drying, overwrite = TRUE)
usethis::use_data(alta_lake_210Pb, overwrite = TRUE)
