library(readr)
library(dplyr)

read_site_activation_file <- function() {
  active <- read_csv(file.path("data-raw", "ASCOTADAPT_SiteActivationDates_20220620.csv"),
    col_types = cols(
      `Site activated on protocol version 3.0` = col_date(format = "%d-%b-%y"),
      `Site activated on protocol version 5.0 (Therapeutic AC)` = col_date(format = "%d-%b-%y")
    )
  ) %>%
    rename(
      state = STATE,
      site = `Site Code`,
      health_service = `Health service`,
      hospname_hospaddress = `Hospital name/address`,
      active_domains = `Domains Active`,
      active_v3 = `Site activated on protocol version 3.0`,
      active_v5 = `Site activated on protocol version 5.0 (Therapeutic AC)`
    ) %>%
    mutate(
      country = case_when(
        state == "India" ~ "IN",
        state == "Nepal" ~ "NP",
        state == "NZ" ~ "NZ",
        TRUE ~ "AU"
      )
    )
  return(active)
}

site_activation <- read_site_activation_file()
usethis::use_data(site_activation, overwrite = TRUE, compress = 'xz')
