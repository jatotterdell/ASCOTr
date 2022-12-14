# Filter analysis sets ----


#' @title FAS-ITT set
#' @description
#' Filter the full dataset down to the FAS-ITT,
#' that is, those who were enrolled and did not
#' withdraw from follow-up.
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_fas_itt <- function(dat) {
  filter(
    dat,
    ENR_rec == 1,
    WTH_FU == 0
  )
}


#' @title ACS-ITT set
#' @description
#' Filter the full dataset down to the ACS-ITT,
#' that is, those who were enrolled and did not
#' withdraw from follow-up and were randomised to
#' intervention in domain C.
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_acs_itt <- function(dat) {
  filter(
    dat,
    ENR_rec == 1,
    ACS_ITT == 1,
    WTH_FU == 0
  )
}


#' @title AVS-ITT set
#' @description
#' Filter the full dataset down to the AVS-ITT,
#' that is, those who were enrolled and did not
#' withdraw from follow-up and were randomised to
#' intervention in domain A.
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_avs_itt <- function(dat) {
  filter(
    dat,
    ENR_rec == 1,
    AVS_ITT == 1,
    WTH_FU == 0
  )
}


# Filter concurrent controls ----


#' @title filter_concurrent_intermediate
#' @description
#' Filter data to those in the intermediate-dose concurrent dataset
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_concurrent_intermediate <- function(dat) {
  dat %>%
    # Restrict to participants randomised to C1 or C2
    filter(CAssignment %in% c("C1", "C2")) %>%
    mutate(
      CAssignment = droplevels(CAssignment),
      randC = droplevels(randC)
    )
}


#' @title filter_concurrent_std_aspirin
#' @description
#' Filter data to those in the low-dose with aspirin concurrent dataset
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_concurrent_std_aspirin <- function(dat) {
  dat %>%
    # Patients randomised before closure of C3
    filter(RandDate < get_intervention_dates()$endate[3]) %>%
    # Patients ineligible for aspirin arm
    filter(inelgc3 == 0) %>%
    mutate(
      CAssignment = droplevels(CAssignment),
      randC = droplevels(randC),
      ctry = droplevels(ctry)
    )
}


#' @title filter_concurrent_therapeutic
#' @description
#' Filter data to those in the therapeutic-dose concurrent dataset
#' @param dat Dataset
#' @return Filtered dataset
#' @export
filter_concurrent_therapeutic <- function(dat) {
  dat %>%
    # Patients randomised after opening of C4
    # Use the provided activation dates for V5
    filter(EL_ProtocolVersion == "5.0") %>%
    mutate(
      CAssignment = droplevels(CAssignment),
      randC = droplevels(randC),
      ctry = relevel(ctry, ref = "NP")
    )
}


# Transformations ----

#' @title Add epoch term to dataset
#' @description
#' Adds an epoch term to the dataset where each epoch
#' is a 4 week window from the most recent randomisation.
#' If any epoch as less than 5 participants, then
#' it is combined with the most recent adjacent epoch.
#' @param dat A dataset with `RandDate`
#' @return A dataset with epoch variables
#' @export
add_epoch_term <- function(dat) {
  dat |>
    mutate(
      relRandDate = as.numeric(max(RandDate) - RandDate),
      epoch_raw = floor(relRandDate / 28),
    ) |>
    group_by(epoch_raw) |>
    mutate(epoch_raw_lab = paste(
      format(min(RandDate), "%d%b%y"),
      format(max(RandDate), "%d%b%y"),
      sep = "-"
    )) |>
    ungroup()
}


#' @title Add region and site groupings to dataset
#' @description
#' Adds region groups and site nested within regions to dataset.
#' Sites with less than 5 participants are combined into
#' an "other" sites group within region.
#' @param dat A dataset with `Country` and `Location` variables
#' @return A dataset with ctry and site variables.
#' @export
add_region_site_groups <- function(dat) {
  site_counts <- dat |>
    count(
      Region = fct_collapse(
        factor(Country, levels = c("IN", "AU", "NP", "NZ")),
        "AU/NZ" = c("AU", "NZ")
      ),
      Location
    )
  merge_ausnz <- site_counts |>
    filter(Region == "AU/NZ", n < 5) |>
    pull(Location)
  dat <- dat |>
    mutate(
      ctry = fct_collapse(
        factor(Country, levels = c("IN", "AU", "NP", "NZ")),
        "AU/NZ" = c("AU", "NZ")
      ),
      # group sites with < 5 counts
      site = fct_collapse(
        factor(Location, levels = site_counts$Location),
        `AU/NZ other` = merge_ausnz
      )
    )
  region_site <- dat %>%
    count(ctry, site) %>%
    mutate(
      ctry_num = as.numeric(ctry),
      site_num = as.numeric(fct_inorder(site))
    )
  dat |>
    left_join(
      region_site |>
        select(-n),
      by = c("ctry", "site")
    )
}


#' @title Add derived covariates
#' @description
#' Adds a collection of manually coded covariates (e.g. converted "Yes"/"No" data to 1/0 data etc)
#' @param dat A dataset
#' @returns A dataset with added covariates
#' @export
add_derived_covariates <- function(dat) {
  dat |>
    mutate(
      facA = factor(AAssignment, levels = c("A1", "A0", "A2")),
      facC = factor(CAssignment, levels = c("C1", "C0", "C2", "C3", "C4")),
      randA = factor(AAssignment, levels = c("A1", "A2")),
      randC = factor(CAssignment, levels = c("C1", "C2", "C3", "C4")),
      aspirin = if_else(BAS_PatientTakingAspirin == "Yes", 1, 0),
      ddimer_oor = factor(case_when(
        is.na(BAS_DDimerOutOfRange) ~ 2,
        BAS_DDimerOutOfRange == "No" ~ 0,
        BAS_DDimerOutOfRange == "Yes" ~ 1
      ), levels = c(0, 1, 2), labels = c("No", "Yes", "Unknown")),
      weight = BAS_Weight,
      weightgt120 = as.numeric(weight > 120),
      oxygen_sat = if_else(BAS_PeripheralOxygen < 10, NA_real_, BAS_PeripheralOxygen),
      airoxy = case_when(
        is.na(BAS_OnRoomAir24hrs) ~ NA_real_,
        BAS_OnRoomAir24hrs == "No" | (BAS_OnRoomAir24hrs == "Yes" & oxygen_sat < 94) ~ 1,
        BAS_OnRoomAir24hrs == "Yes" ~ 0,
        TRUE ~ NA_real_
      ),
      airoxy2 = factor(if_else(is.na(airoxy), 2, airoxy), levels = 0:2, labels = c("No", "Yes", "Unknown")),
      dsfs = as.numeric(RandDate - EL_FirstSymptoms),
      dsfsgt7 = as.numeric(dsfs > 7),
      vax = case_when(
        is.na(BAS_PatientVaccinated) ~ 2,
        BAS_PatientVaccinated == "Yes" ~ 1,
        BAS_PatientVaccinated == "No" ~ 0,
        TRUE ~ NA_real_
      ),
      vaxfac = factor(vax, levels = 0:2, labels = c("No", "Yes", "Unknown")),
      rec_steroids = if_else(DIS_ImmunoCorticosteroids == "Yes", 1, 0),
      rec_remdesivir = if_else(DIS_RemdesivirReceived == "Yes", 1, 0),
      rec_antiviral = case_when(
        DIS_CamostatReceived == "Yes" ~ 1,
        DIS_FavipiravirReceived == "Yes" ~ 1,
        DIS_DoxycyclineReceived == "Yes" ~ 1,
        DIS_IvermectinReceived == "Yes" ~ 1,
        DIS_ReceivedOther == "Yes" ~ 1,
        TRUE ~ 0
      ),
      age_c = AgeAtEntry - mean(AgeAtEntry),
      agegte60,
      agegte60_c = agegte60 - mean(agegte60),
      inelgc3 = if_else(EL_inelg_c3 == 0 | is.na(EL_inelg_c3), 0, 1)
    )
}


# Analysis Sets ----


#' @title Create the FAS-ITT set with relevant variables
#' @description
#' Creates the FAS-ITT analysis set for the primary outcome.
#' May need some tweaking for secondary outcomes.
#' @param dat A dataset
#' @returns The FAS-ITT set
#' @export
make_fas_itt_set <- function(dat) {
  dat |>
    filter_fas_itt() |>
    add_derived_covariates() |>
    add_region_site_groups() |>
    add_epoch_term() |>
    # Manually correct epoch data
    mutate(
      epoch = case_when(
        epoch_raw == 19 ~ 18,
        epoch_raw %in% 0:6 ~ 6,
        TRUE ~ epoch_raw
      ) - 5
    ) |>
    group_by(epoch) |>
    mutate(epoch_lab = paste(
      format(min(RandDate), "%d%b%y"),
      format(max(RandDate), "%d%b%y"),
      sep = "-"
    )) |>
    ungroup()
}


#' @title Create the ACS-ITT set with relevant variables
#' @description
#' Creates the ACS-ITT analysis set for the primary outcome.
#' May need some tweaking for secondary outcomes.
#' @param dat A dataset
#' @returns The ACS-ITT set
#' @export
make_acs_itt_set <- function(dat) {
  dat |>
    filter_acs_itt() |>
    add_derived_covariates() |>
    add_region_site_groups() |>
    add_epoch_term() |>
    # Manually correct epoch data
    mutate(
      epoch = case_when(
        epoch_raw == 14 ~ 13,
        epoch_raw %in% 0:2 ~ 2,
        TRUE ~ epoch_raw
      ) - 1
    ) |>
    group_by(epoch) |>
    mutate(epoch_lab = paste(
      format(min(RandDate), "%d%b%y"),
      format(max(RandDate), "%d%b%y"),
      sep = "-"
    )) |>
    ungroup()
}


#' @title Create the AVS-ITT set with relevant variables
#' @description
#' Creates the AVS-ITT analysis set for the primary outcome.
#' May need some tweaking for secondary outcomes.
#' @param dat A dataset
#' @returns The AVS-ITT set
#' @export
make_avs_itt_set <- function(dat) {
  dat |>
    filter_avs_itt() |>
    add_derived_covariates() |>
    add_region_site_groups() |>
    add_epoch_term() |>
    # Manually correct epoch data
    mutate(
      epoch = case_when(
        epoch_raw %in% 14:15 ~ 13,
        epoch_raw %in% 0:3 ~ 6,
        epoch_raw %in% 4:6 ~ 7,
        epoch_raw %in% 7:8 ~ 8,
        TRUE ~ epoch_raw
      ) - 5,
      crp_tertile = quantile_cuts(BAS_CRPResult_fixed, seq(0, 1, 1/3))
    ) |>
    group_by(epoch) |>
    mutate(epoch_lab = paste(
      format(min(RandDate), "%d%b%y"),
      format(max(RandDate), "%d%b%y"),
      sep = "-"
    )) |>
    ungroup()
}
