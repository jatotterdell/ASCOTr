# Filter analysis sets ----

#' @title filter_fas_itt
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

#' @title filter_acs_itt
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

#' @title filter_avs_itt
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
      randC  = droplevels(randC)
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
    mutate(CAssignment = droplevels(CAssignment),
           randC  = droplevels(randC),
           ctry = droplevels(ctry))
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
    mutate(CAssignment = droplevels(CAssignment),
           randC  = droplevels(randC),
           ctry = relevel(ctry, ref = "NP"))
}

# Transformations ----

#' @title transmute_model_cols_grp_aus_nz
#' @description
#' Same as transmute_model_cols, but join Australia and new zealand
#' together as one "region" with nested sites.
#' @param dat Dataset
#' @return Transformed dataset with limited variables
#' @export
transmute_model_cols_grp_aus_nz <- function(dat) {
  site_counts <- dat %>%
    dplyr::count(
      Region = fct_collapse(
        factor(Country, levels = c("IN", "AU", "NP", "NZ")),
        "AU/NZ" = c("AU", "NZ")),
      Location)
  merge_ausnz <- site_counts %>%
    filter(Region == "AU/NZ", n < 5) %>%
    pull(Location)
  dat <- dat %>%
    mutate(
      ctry = fct_collapse(
        factor(Country, levels = c("IN", "AU", "NP", "NZ")),
        "AU/NZ" = c("AU", "NZ")),
      # group sites with < 5 counts
      site = fct_collapse(
        factor(Location, levels = site_counts$Location),
        `AU/NZ other` = merge_ausnz
      )
    )
  region_site <- dat %>%
    dplyr::count(ctry, site) %>%
    mutate(
      ctry_num = as.numeric(ctry),
      site_num = as.numeric(fct_inorder(site))
    )
  dat <- dat %>%
    left_join(region_site %>% select(-n), by = c("ctry", "site")) %>%
    transmute(
      StudyPatientID,
      EL_ProtocolVersion,
      Sex,
      AAssignment= droplevels(factor(
        AAssignment, levels = c("A0", "A1", "A2"))),
      CAssignment = droplevels(factor(
        CAssignment, levels = c("C1", "C0", "C2", "C3", "C4"))),
      RandDate,
      randA = factor(AAssignment, levels = c("A1", "A2")),
      randC = factor(CAssignment, levels = c("C1", "C2", "C3", "C4")),
      PO,
      out_rec,
      out_ttr,
      D28_who,
      D28_who2,
      D28_death,
      D28_OutcomeTotalDaysHospitalised,
      DIS_day,
      out_dafh,
      D28_OutcomeDaysFreeOfVentilation,
      out_dafv,
      out_sob,
      out_mmrc_scale,
      AgeAtEntry,
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
      country = factor(Country, levels = c("IN", "AU", "NP", "NZ")),
      inelgc3 = if_else(EL_inelg_c3 == 0 | is.na(EL_inelg_c3), 0, 1),
      ctry = fct_collapse(
        factor(Country, levels = c("IN", "AU", "NP", "NZ")),
        "AU/NZ" = c("AU", "NZ")),
      ctry_num,
      site_raw = factor(Location, levels = site_counts$Location),
      # group sites with < 5 counts
      site = fct_collapse(site_raw, `AU/NZ other` = merge_ausnz),
      site_num,
      relRandDate = as.numeric(max(RandDate) - RandDate),
      epoch_raw = floor(relRandDate / 28),
      # Manual merge after check of count(epoch)
      epoch = case_when(
        epoch_raw %in% 0:1 ~ 2,
        epoch_raw == 14 ~ 13,
        TRUE ~ epoch_raw
      ) - 1,
    ) %>%
    group_by(epoch) %>%
    mutate(epoch_lab = paste(
      format(min(RandDate), "%d%b%y"),
      format(max(RandDate), "%d%b%y"),
      sep = "-")
    ) %>%
    ungroup()
  return(dat)
}
