# Derivations and corrections ----

#' @title add_database_corrections
#' @description
#' Some corrections to the data base are required, but cannot be made
#' to the database directly. These corrections are listed in
#' "ASCOT_ADAPT_DatabaseCorrectionsErrors.xlsx" and are here applied
#' manually to the data extracts.
#' @param dat Dataset containing relevant variables
#' @return `dat` but with fields corrected
#' @export
add_database_corrections <- function(dat) {
  platID <- c(
    paste0("LUD000", formatC(1:22, width = 2, flag = "0")),
    paste0("PUN000", formatC(1:51, width = 2, flag = "0"))
  )
  creaID <- c(
    paste0("PUN000", formatC(1:32, width = 2, flag = "0")),
    paste0("PUN000", c(34, 55, 60, 66))
  )
  dat %>%
    mutate(

      # MMC00081
      # did not consent to participate in the Antiviral Domain but has antiviral
      # domain-specific data stored in their record.
      # These antiviral domain responses need to be removed for this participant as we cannot use their data:
      # Baseline respiratory tract PCR result
      #   BAS_RespiratoryTestPerformed
      # Nafamostat
      #   DD_NafamostatAdministered
      # Laboratory test results
      #   DD_Potassium
      #   DD_Sodium
      #   DD_ALTLabs
      #   DD_ASTLabs
      #   PCR results
      #
      #   DD_RespiratorySampleType
      #   DD_RespiratorySampleResult
      #   DD_CycleThreshold
      #   DD_PCRMethod
      # Major bleeding per ISTH definition
      #   DD_AdverseEventsToday
      # Antiviral-domain specific data at discharge
      #   DIS_ThrombophlebitisIVLine
      #   DIS_NonMajorBleeding
      AAssignment = case_when(
        StudyPatientID == "MMC00081" ~ "A0",
        TRUE ~ AAssignment
      ),
      EL_Con_DomainA = case_when(
        StudyPatientID == "MMC00081" ~ "No",
        TRUE ~ EL_Con_DomainA
      ),
      CON_DomainA = case_when(
        StudyPatientID == "MMC00081" ~ "No",
        TRUE ~ CON_DomainA
      ),
      BAS_RespiratoryTestPerformed = case_when(
        StudyPatientID == "MMC00081" ~ NA_character_,
        TRUE ~ BAS_RespiratoryTestPerformed
      ),
      DIS_ThrombophlebitisIVLine = if_else(StudyPatientID == "MMC00081", NA_character_, DIS_ThrombophlebitisIVLine),
      DIS_NonMajorBleeding = if_else(StudyPatientID == "MMC00081", NA_character_, DIS_NonMajorBleeding),


      # StudyID SAM00117 was not previously screened
      EL_PrevScreened = case_when(
        StudyPatientID == "SAM00117" ~ "No",
        TRUE ~ EL_PrevScreened
      ),

      # Wrong potassium value entered 4.4 instead of 4.0
      EL_SerumPotassium = case_when(
        StudyPatientID == "WES00023" ~ 4.0,
        TRUE ~ EL_SerumPotassium
      ),

      # Date of symptom onset and first positive test incorrect
      EL_FirstSymptoms = case_when(
        StudyPatientID == "MID00014" ~ as.Date("2021-10-07"),
        TRUE ~ EL_FirstSymptoms
      ),
      EL_FirstPositiveTest = case_when(
        StudyPatientID == "MID00014" ~ as.Date("2021-10-08"),
        TRUE ~ EL_FirstPositiveTest
      ),

      # Date of positive test incorrect
      EL_FirstPositiveTest = case_when(
        StudyPatientID == "SYM00002" ~ as.Date("2021-06-05"),
        StudyPatientID == "SYM00003" ~ as.Date("2021-06-14"),
        TRUE ~ EL_FirstPositiveTest
      ),

      # Creatinine units wrongly recorded
      EL_SerumCreatinineUnits = if_else(StudyPatientID == "SYM00001", "mg/dL", EL_SerumCreatinineUnits),
      EL_SerumCreatinine_umolL = if_else(StudyPatientID == "SYM00001", EL_SerumCreatinineBlood * 88.42, EL_SerumCreatinine_umolL),
      EL_SerumCreatinineUnits = if_else(StudyPatientID == "JIV00001", "mg/dL", EL_SerumCreatinineUnits),
      EL_SerumCreatinine_umolL = if_else(StudyPatientID == "JIV00001", EL_SerumCreatinineBlood * 88.42, EL_SerumCreatinine_umolL),

      # Days recorded as 27 but should be 28
      D28_OutcomeDaysFreeOfVentilation =
        if_else(StudyPatientID == "MID00006", 28, D28_OutcomeDaysFreeOfVentilation),

      # Fix platelet units
      EL_BloodPlateletTestValueUnits =
        if_else(StudyPatientID %in% platID, "x 10<sup>9</sup>/L", EL_BloodPlateletTestValueUnits),
      EL_BloodPlateletTestAs_x10_9_L =
        if_else(StudyPatientID %in% platID, EL_BloodPlateletTestValue, EL_BloodPlateletTestAs_x10_9_L),

      # Fix wrong units for Creatinine units, note 1 mg/dL -> 88.42 umol/L
      EL_SerumCreatinineUnits =
        if_else(StudyPatientID %in% creaID, "mg/dL", EL_SerumCreatinineUnits),
      EL_SerumCreatinine_umolL =
        if_else(StudyPatientID %in% creaID, EL_SerumCreatinineBlood * 88.42, EL_SerumCreatinine_umolL),

      # Wrong values entered for screened but not-randomised ID UHPZWK
      EL_BloodPlateletTestAs_x10_9_L =
        case_when(
          EligibilityCode == "UHPZWK" ~ 228,
          TRUE ~ EL_BloodPlateletTestAs_x10_9_L
        ),
      EL_BloodPlateletTestValue =
        case_when(
          EligibilityCode == "UHPZWK" ~ 228,
          TRUE ~ EL_BloodPlateletTestValue
        ),
      EL_SerumCreatinineBlood =
        case_when(
          EligibilityCode == "UHPZWK" ~ 1.1,
          TRUE ~ EL_SerumCreatinineBlood
        ),
      EL_Con_ConsentDate =
        case_when(
          EligibilityCode == "UHPZWK" ~ EL_Con_ConsentDate + 5400,
          TRUE ~ EL_Con_ConsentDate
        ),

      # Wrong platelet value entered for PUN00004
      EL_BloodPlateletTestAs_x10_9_L =
        case_when(
          StudyPatientID == "PUN00004" ~ 276,
          TRUE ~ EL_BloodPlateletTestAs_x10_9_L
        ),
      EL_BloodPlateletTestValue =
        case_when(
          StudyPatientID == "PUN00004" ~ 276,
          TRUE ~ EL_BloodPlateletTestValue
        ),
      EL_eGFR =
        case_when(
          StudyPatientID == "PRC00007" ~ 73,
          TRUE ~ EL_eGFR
        ),
    )
}


#' @title add_outcome_corrections
#' @description
#' Additional follow-up with sites was undertaken for participants
#' randomsied to the antiviral domain who had missing primary outcome data
#' due to unknown day 28 status.
#'
#' The following corrections are made as per email correspondence with study
#' coordinators who contacted sites for further information.
#' @param dat Dataset containing relevant variables
#' @return `dat` but with relevant outcome fields corrected
#' @export
add_outcome_corrections <- function(dat) {
  dat %>%
    mutate(
      # WES00039
      D28_PatientStatusDay28 = if_else(StudyPatientID == "WES00039", "Alive", D28_PatientStatusDay28),
      D28_OutcomeVasopressors = if_else(StudyPatientID == "WES00039", "No", D28_OutcomeVasopressors),
      D28_Status = if_else(StudyPatientID == "WES00039", "1. Not hospitalised, no limitations on activities", D28_Status),
      D28_OutcomeDaysFreeOfVentilation = if_else(StudyPatientID == "WES00039", 28, D28_OutcomeDaysFreeOfVentilation ),

      # WES00048
      D28_PatientStatusDay28 = if_else(StudyPatientID == "WES00048", "Alive", D28_PatientStatusDay28),
      D28_OutcomeVasopressors = if_else(StudyPatientID == "WES00048", "No", D28_OutcomeVasopressors),
      D28_Status = if_else(StudyPatientID == "WES00048", "1. Not hospitalised, no limitations on activities", D28_Status),

      # BXH00086
      D28_PatientStatusDay28 = if_else(StudyPatientID == "BXH00086", "Alive", D28_PatientStatusDay28),
      D28_OutcomeVasopressors = if_else(StudyPatientID == "BXH00086", "No", D28_OutcomeVasopressors),
      D28_Status = if_else(StudyPatientID == "BXH00086", "2. Not hospitalised, limitations on activities", D28_Status),
      D28_OutcomeDaysFreeOfVentilation = if_else(StudyPatientID == "BXH00086", 28, D28_OutcomeDaysFreeOfVentilation),

      # WEL00001
      D28_OutcomeVasopressors = if_else(StudyPatientID == "WEL00001", "No", D28_OutcomeVasopressors),
      D28_OutcomeDaysFreeOfVentilation = if_else(StudyPatientID == "WEL00001", 28, D28_OutcomeDaysFreeOfVentilation)
    )
}


#' @title add_v5_activation_date
#' @description
#' Add site activation data
#'
#' @param dat A dataset
#' @return dat with site data added
#' @export
add_v5_activation_date <- function(dat) {
  act <- site_activation %>%
    select(country, site, active_domains, active_v3, active_v5)
  dat %>%
    left_join(act, by = c("Country" = "country", "Location" = "site"))
}

#' @title add_d28_corrections
#' @description
#' Make corrections to the day 28 dataset
#'
#' @param dat Day 28 dataset
#' @return dat with corrections
#' @export
add_d28_corrections <- function(dat) {
  dat %>%
    mutate(
      D28_OutcomeTotalDaysHospitalised = case_when(
        # <= 19 days, but actual amount unknown
        StudyPatientID == "JIV00223" ~ NA_real_,
        # Reported as 3, but actually hospitalised for 28 days
        StudyPatientID == "SAM00060" ~ 28,
        TRUE ~ D28_OutcomeTotalDaysHospitalised
      ),
      D28_OutcomeDaysFreeOfVentilation = case_when(
        # Followed up on day 27 with 27 days free of vent,
        # really this is interval censored as in {27, 28}, convert to 28,
        # assuming no ventilation on day 28 (given not hospitalised on day 27)
        StudyPatientID == "PUN00027" ~ 28,
        TRUE ~ D28_OutcomeDaysFreeOfVentilation
      )
    )
}


#' @title add_withdrawn_followup
#' @description
#' Add withdrawal flag to participant records.
#'
#' @param dat Dataset containing relevant withdrawal variables
#' @return Returns dat with a new indicator variable for whether
#' participant withdrew from follow-up.
add_withdrawn_followup <- function(dat) {
  dat %>%
    mutate(WTH_FU = if_else(
      WTH_rec == 0, 0L, as.integer(
        CON_WithdrawnContact28 == "Yes" |
          CON_WithdrawnDailyCollection == "Withdrawn" |
          # Covers withdrawal due to ineligibility
          is.na(CON_WithdrawnContact28)
      )
    ))
}


#' @title add_oxygen
#' @description
#' Add requirement of oxygen at baseline
#' @param dat Dataset containing relevant variables
#' @return Returns dat with new variables.
add_oxygen <- function(dat) {
  dat %>%
    mutate(
      # Note one subject is outlier with Oxygen saturation of 10%
      BAS_PeripheralOxygen = if_else(
        BAS_PeripheralOxygen < 10 | is.na(BAS_PeripheralOxygen),
        NA_real_,
        BAS_PeripheralOxygen
      ),
      supp_oxy = case_when(
        BAS_OnRoomAir24hrs == "Yes" ~ 1,
        BAS_PeripheralOxygen < 94 ~ 1,
        BAS_OnRoomAir24hrsUnknown == "Yes" ~ NA_real_,
        is.na(BAS_PeripheralOxygen) ~ NA_real_,
        TRUE ~ 0
      )
    )
}


#' @title add_supplemental_oxygen
#' @description
#' Add requirement of supplemental oxygen at baseline
#' Per email from Steve:
#' For the baseline status, we have that initial question regarding ‘Was the patient on room air for any of the preceding 24 hours?’
#' If no – they needed supplemental oxygen.
#' If yes – the next Q can be used – if O2 saturation <94%, then we code as needing supplemental O2. If ≥94%, then we code as NOT needing supplemental oxygen.
#' @param dat Dataset containing relevant variables
#' @return Returns dat with new variables.
add_supplemental_oxygen <- function(dat) {
  dat %>%
    mutate(
      # Note one subject is outlier with Oxygen saturation of 10%
      BAS_PeripheralOxygen = if_else(
        BAS_PeripheralOxygen < 10 | is.na(BAS_PeripheralOxygen),
        NA_real_,
        BAS_PeripheralOxygen
      ),
      supp_oxy = case_when(
        BAS_OnRoomAir24hrs == "No" ~ 1,
        BAS_OnRoomAir24hrs == "Yes" & BAS_PeripheralOxygen < 94 ~ 1,
        BAS_OnRoomAir24hrs == "Yes" & BAS_PeripheralOxygen >= 94 ~ 0,
        BAS_OnRoomAir24hrsUnknown == "Yes" ~ NA_real_,
        is.na(BAS_PeripheralOxygen) ~ NA_real_,
        TRUE ~ 0
      ),
      supp_oxy2 = if_else(is.na(supp_oxy), 0, supp_oxy)
    )
}


#' @title add_primary_outcome
#' @description Add the derived primary outcome for each participant.
#' @param dat Dataset with one record per participant with their outcomes
#' @return Returns `dat` but with primary outcome fields appended.
add_primary_outcome_components <- function(dat) {
  outcome_components_d28 <- dat %>%
    mutate(
      # DAMA and likely to die
      DIS_DAMAlikelytodie = case_when(
        DIS_DAMA == 1 & DIS_LikelyToDie28 == "Yes" ~ 1L,
        DIS_DAMA == 1 & DIS_LikelyToDie28 == "No" ~ 0L,
        DIS_DAMA == 0 ~ 0L,
        is.na(DIS_DAMA) | DIS_DAMA == 1 & (is.na(DIS_LikelyToDie28) | DIS_LikelyToDie28 == "Unknown") ~ NA_integer_
      ),
      # Study day of discharge
      DIS_day = as.numeric(DIS_DateOfDischarge - RandDate + 1),
      # Study day of death (discharge)
      DIS_dday = as.numeric(DIS_DateOfDeath - RandDate + 1),
      # Discharge death lt 28 days
      DIS_deathlt28 = as.integer(DIS_death == 1 & DIS_day <= 28),
      # Any death by day 28 (discharge/d28)
      D28_death = case_when(
        D28_PatientStatusDay28 == "Alive" ~ 0L,
        D28_PatientStatusDay28 == "Dead" ~ 1L,
        D28_PatientStatusDay28 == "Unknown" ~ NA_integer_,
        DIS_deathlt28 == 1 ~ 1L,
        TRUE ~ NA_integer_
      ),
      # WHO status at day 28 fill in deaths
      D28_who = case_when(
        D28_death == 1L ~ 8L,
        !is.na(D28_who) ~ D28_who,
        TRUE ~ NA_integer_
      ),
      # Any vasopressor use between discharge and day 28
      D28_vasop = case_when(
        D28_OutcomeVasopressors == "Yes" ~ 1,
        # If death or no discharge then no "Vasopressor use between discharge and day 28"
        DIS_death == 1 | DIS_day > 28 ~ 0,
        is.na(D28_OutcomeVasopressors) | D28_OutcomeVasopressors == "Unknown" ~ NA_real_,
        TRUE ~ 0
      ),
      # On ventilation at day 28,
      # note no information on ventilation between discharge and day 28
      D28_vent = case_when(
        D28_StatusVentilation %in% c(
          "2. Non-invasive ventilation with no previous history of home BiPAP/CPAP",
          "4. Non-invasive ventilation with previous history of home BiPAP/CPAP that HAS graduated from CPAP only"
        ) ~ 1,
        DIS_death == 1 ~ 0,
        is.na(D28_Status) ~ NA_real_,
        TRUE ~ 0
      ),
      # Note two participants have D28_vent as missing, but were known to be out of hospital
      # Fix here
      D28_vent = case_when(
        StudyPatientID %in% c("ALF00006", "ALF00012") ~ 0,
        TRUE ~ D28_vent
      ),
      D28_vent2 = case_when(
        is.na(D28_OutcomeDaysFreeOfVentilation) ~ NA_real_,
        D28_OutcomeDaysFreeOfVentilation >= 28 ~ 0,
        D28_OutcomeDaysFreeOfVentilation < 28 ~ 1,
        TRUE ~ NA_real_
      ),
      PO_dama = case_when(
        DIS_DAMAlikelytodie == 1 & (is.na(D28_PatientStatusDay28) | D28_PatientStatusDay28 == "Unknown") ~ 1,
        TRUE ~ 0
      ),
      DAILY_missing = if_else(is.na(DD_total_records) | DD_total_records < pmin(28, DIS_day), 1, 0),
      ANY_DD_vasop = case_when(
        DD_vasop == 1 ~ 1,
        DAILY_missing == 1 | is.na(DAILY_missing) ~ NA_real_,
        TRUE ~ 0
      ),
      # Any vasopressor (pre or post discharge)
      ANY_vasop = case_when(
        ANY_DD_vasop == 1 | D28_vasop == 1 ~ 1,
        is.na(D28_vasop) | DAILY_missing == 1 | is.na(DAILY_missing) ~ NA_real_,
        TRUE ~ 0
      ),
      # Any daily ventilation, accounting for missing daily data records
      ANY_DD_vent = case_when(
        DD_vent == 1 ~ 1,
        DAILY_missing == 1 | is.na(DAILY_missing) ~ NA_real_,
        TRUE ~ 0
      ),
      # Any ventilation, daily or day 28
      ANY_vent = case_when(
        D28_vent == 1 | D28_vent2 == 1 | ANY_DD_vent == 1 ~ 1,
        is.na(ANY_DD_vent) | is.na(D28_vent) ~ NA_real_,
        TRUE ~ 0
      ),
      ANY_vent2 = case_when(
        D28_vent == 1 | ANY_DD_vent == 1 ~ 1,
        is.na(ANY_DD_vent) | is.na(D28_vent) ~ NA_real_,
        TRUE ~ 0
      ),
      # Primary outcome known, or missing
      PO = case_when(
        # If died, required vany vaso, required any new vent, or met DAMA criteria then satisfy primary outcome.
        D28_death == 1 | ANY_vasop == 1 | PO_dama == 1 | ANY_vent == 1 ~ 1,
        # If day 28 status unknown, or missing any daily records, or missing vasop/vent
        is.na(D28_death) | DAILY_missing == 1 | is.na(DAILY_missing) | is.na(D28_vasop) | is.na(ANY_vent) ~ NA_real_,
        TRUE ~ 0
      ),
      # Note - two participants, ALF00006 and ALF00012 had censored D28_Status:
      # - it's known that D28_Status in [1, 2] but not which value, so this is NA in the dataset,
      #   but we know that they did not meet the primary outcome
      #   a correction is made here
      PO = case_when(
        StudyPatientID %in% c("ALF00006", "ALF00012") ~ 0,
        TRUE ~ PO
      )
    )
  return(outcome_components_d28)
}


#' @title add_days_alive_and_free
#' @description
#' Add days alive and free of hospital
#' A death is counted as 0 days
#' @param dat A dataset with relevant variables
#' @returns `dat` with `out_dafh` for days alive and free of hospital
add_days_alive_and_free <- function(dat) {
  out <- dat %>%
    mutate(
      out_dafh = case_when(
        is.na(D28_death) ~ NA_real_,
        is.na(D28_OutcomeTotalDaysHospitalised) & D28_death == 0 ~ NA_real_,
        D28_death == 1 ~ 0,
        TRUE ~ 28 - pmin(D28_OutcomeTotalDaysHospitalised, 28)
      )
    )
  return(out)
}


#' @title add_days_alive_and_free_ventilation
#' @description
#' Add days alive and free of ventilation
#' A death is counted as 0 days
#' @param dat A dataset with relevant variables
#' @returns `dat` with `out_dafv` for days alive and free of ventilation
add_days_alive_and_free_ventilation <- function(dat) {
  out <- dat %>%
    mutate(
      out_dafv = case_when(
        is.na(D28_death) ~ NA_real_,
        is.na(D28_OutcomeDaysFreeOfVentilation) & D28_death == 0 ~ NA_real_,
        D28_death == 1 ~ 0,
        TRUE ~ pmin(D28_OutcomeDaysFreeOfVentilation, 28)
      )
    )
  return(out)
}


#' @title add_shortness_of_breath
#' @description
#' Add a shortness of breath variable
#' @param dat A dataset with relevant variables
#' @returns `dat` with `out_sob` added
add_shortness_of_breath <- function(dat) {
  out <- dat %>%
    mutate(
      out_sob = case_when(
        is.na(D28_BreathSinceGettingCovid) ~ NA_real_,
        D28_BreathSinceGettingCovid == "Unknown" ~ NA_real_,
        D28_BreathSinceGettingCovid == "Yes" ~ 1,
        D28_BreathSinceGettingCovid == "No" ~ 0
      )
    )
  return(out)
}


#' @title add_breathlessness_scale
#' @description
#' Add breathlessness scale variable
#' @param dat A dataset with relevant variables
#' @returns `dat` with `out_mmrc_scale` added
#' @export
add_breathlessness_scale <- function(dat) {
  out <- dat %>%
    mutate(
      out_mmrc_scale = case_when(
        is.na(D28_BreathSinceGettingCovid) ~ NA_real_,
        D28_BreathSinceGettingCovid == "Unknown" ~ NA_real_,
        D28_BreathSinceGettingCovid == "No" ~ -1,
        TRUE ~ D28_BreathScale
      ) + 1
    )
  return(out)
}


#' @title add_time_to_recovery
#' @description
#' Add time to recovery variable
#' @param dat A dataset with relevant variables
#' @returns `dat` with `out_rec`, and `out_ttr` added
#' @export
add_time_to_recovery <- function(dat) {
  out <- dat %>%
    mutate(
      out_rec = case_when(
        # If died and did not recover, then 2
        DIS_deathlt28 == 1 & DD_recover_who == 0 ~ 2,
        # If died, but earlier recovered, then 1
        DIS_deathlt28 == 1 & DD_recover_who == 1 & DD_ttr_who < DIS_day ~ 1,
        # If died and recovered on same day, then 2
        DIS_deathlt28 == 1 & DD_recover_who == 1 & DD_ttr_who >= DIS_day ~ 2,
        # Count as "recovered" if recover as per WHO
        DD_recover_who == 1 ~ 1,
        # Or id discharged alive within 28 days
        DIS_day < 28 & DIS_deathlt28 == 0 ~ 1,
        # If did not meet WHO criteria or discharge then no recovery
        # >= because "... assumed that the participant is not hospitalised on the first day **following** discharge"
        DD_recover_who == 0 & DIS_day >= 28 ~ 0,
        TRUE ~ NA_real_
      ),
      out_ttr = case_when(
        is.na(out_rec) ~ NA_real_,
        # Censoring
        out_rec == 0 ~ 28,
        # Deaths
        out_rec == 2 ~ DIS_day,
        # Satisfy WHO criteria
        out_rec == 1 & is.na(DD_ttr_who) ~ DIS_day + 1,
        out_rec == 1 & !is.na(DD_ttr_who) ~ DD_ttr_who,
        TRUE ~ NA_real_
      )
    )
  return(out)
}


fix_crp_units <- function(dat) {
  dat %>%
    mutate(
      # Use units as provided
      BAS_CRPUnits_fixed = case_when(
        Location == "ALF" ~ "mg/L|ug/mL",
        Location == "AMH" ~ "mg/dL",
        Location == "APM" ~ "mg/dL",
        Location == "BCM" ~ "mg/dL",
        Location == "BHN" ~ "mg/dL",
        Location == "BLK" ~ "mg/L|ug/mL",
        Location == "BXH" ~ "mg/L|ug/mL",
        Location == "CAM" ~ "mg/L|ug/mL",
        Location == "CHR" ~ "mg/L|ug/mL",
        Location == "COR" ~ "mg/dL",
        Location == "DUN" ~ "mg/L|ug/mL",
        Location == "JHH" ~ "mg/L|ug/mL",
        Location == "JIV" ~ "mg/dL",
        Location == "LIV" ~ "mg/L|ug/mL",
        Location == "LUD" ~ "mg/dL",
        Location == "MAS" ~ "mg/dL",
        Location == "MID" ~ "mg/L|ug/mL",
        Location == "MMC" ~ "mg/L|ug/mL",
        Location == "NSH" ~ "mg/L|ug/mL",
        Location == "PRC" ~ "mg/L|ug/mL",
        Location == "PUN" ~ "mg/dL",
        Location == "RMH" ~ "mg/L|ug/mL",
        Location == "RNS" ~ "mg/L|ug/mL",
        Location == "RPH" ~ "mg/L|ug/mL",
        Location == "SAM" ~ "mg/dL",
        Location == "STG" ~ "mg/L|ug/mL",
        Location == "SVH" ~ "mg/L|ug/mL",
        Location == "SYM" ~ "mg/dL",
        Location == "TUT" ~ "mg/dL",
        Location == "VEL" ~ "mg/dL",
        Location == "WEL" ~ "mg/L|ug/mL",
        Location == "WES" ~ "mg/L|ug/mL",
        Location == "WOL" ~ "mg/L|ug/mL",
        Location == "WSH" ~ "mg/L|ug/mL",
        Location == "WWB" ~ "mg/L|ug/mL"
      ),
      # Reapply transformation
      BAS_CRPResult_fixed = case_when(
        BAS_CRPUnits_fixed == "mg/dL" ~ 10 * BAS_CRPEntered,
        BAS_CRPUnits_fixed == "mg/L|ug/mL" ~ BAS_CRPEntered,
        TRUE ~ NA_real_
      )
    )
}


create_per_protocol_flags <- function() {

}

# Formatting ----

#' @title format_eligibility_data
#' @description
#' Format eligibility data
#' @param el Eligibility data
#' @returns Formatted data
#' @importFrom labelled labelled
#' @export
format_eligibility_data <- function(el) {
  el %>%
    select(-EligibilityID) %>%
    mutate(

      # Ineligible for Nafamostat
      EL_inelg_a2 = labelled(as.integer(
        EL_TherapeuticAnticoagBleeding == "Yes" |
          EL_HyperNafamostat == "Yes" |
          EL_ReceivedNafamostat == "Yes" |
          EL_HeartRenalDialysis == "Yes" |
          as.numeric(EL_SerumPotassium) > 5.5 |
          as.numeric(EL_SerumSodium) < 120
      ), label = "Ineligible for Nafamostat (A2)"),

      # Given only two interventions in domain, if
      # ineligible for A2 then ineligible for the domain
      EL_inelg_a = labelled(EL_inelg_a2, label = "Ineligible for domain A"),

      # Ineligible for the anticoagulation domain
      EL_inelg_c = labelled(as.integer(
        EL_TherapeuticAnticoag == "Yes" |
          EL_DualAntiplateletTherapy == "Yes" |
          EL_ContraHeparinReact == "Yes" |
          EL_IntracranialHaemorrhage == "Yes" |
          EL_BleedingConditionThrombo == "Yes" |
          (
            # Note that this criteria changed between version 3.0 and 5.0 of protocol
            (EL_ProtocolVersion == "3.0" & as.numeric(EL_BloodPlateletTestAs_x10_9_L) < 30) |
              (EL_ProtocolVersion == "5.0" & as.numeric(EL_BloodPlateletTestAs_x10_9_L) < 50)
          ) |
          as.numeric(EL_eGFR < 15)
      ), label = "Ineligible for domain C"),

      # Ineligible for the intervention C3
      # Note, once C3 had been removed, ceased to be assessed,
      # protocol v5 participants eligibility is unknown...
      EL_inelg_c3 = labelled(as.integer(
        EL_ReceivingAntiplatelet == "Yes" |
          EL_HyperAspirin == "Yes"
      ), label = "Ineligible for LWMH + Aspirin (C3)")
    )
}


#' @title format_consent_data
#' @param con Consent data
#' @returns Formatted data
#' @export
format_consent_data <- function(con) {
  con
}


#' @title format_enrolled_data
#' @param enr Enrolled data
#' @returns Formatted data
#' @export
format_enrolled_data <- function(enr) {
  enr %>%
    select(-PT_YOB, -PT_DOD, -EID, -ECode) %>%
    mutate(
      ENR_regimen = paste0(AAssignment, BAssignment, CAssignment),
      FAS_ITT = 1L,
      ACS_ITT = if_else(CAssignment != "C0", 1L, 0L),
      AVS_ITT = if_else(AAssignment != "A0", 1L, 0L),
      agegte60 = labelled(as.integer(AgeAtEntry >= 60), label = "Age >= 60"),
      sexF = as.integer(Sex == "Female"),
    )
}


#' @title format_baseline_data
#' @param bas Baseline data
#' @returns Formatted data
#' @export
format_baseline_data <- function(bas) {
  bas %>%
    select(-SDV, -FormLock, -BAS_PatientInitials) %>%
    # Summarise ethnicity data
    relocate(BAS_EthnicityUnknown, .before = "BAS_EthnicityAboriginal") %>%
    rowwise() %>%
    mutate(
      BAS_eth_all_na = all(is.na(c_across(BAS_EthnicityUnknown:BAS_EthnicityOther))),
      BAS_eth_all_but_unknown_na = all(is.na(c_across(BAS_EthnicityAboriginal:BAS_EthnicityOther))),
      BAS_eth_count = sum(c_across(BAS_EthnicityAboriginal:BAS_EthnicityOther) == "Yes", na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      # Replace NA with "No"
      across(BAS_EthnicityUnknown:BAS_EthnicityOther, ~ if_else(is.na(.x), "No", .x))
    ) %>%
    # Summarise co-morbidities data
    relocate(BAS_Comorbidities_None, .before = "BAS_ChonicCardiacDisease") %>%
    rowwise() %>%
    mutate(
      BAS_com_all_na = all(is.na(c_across(BAS_Comorbidities_None:BAS_IatrogenicImmuno))),
      BAS_com_all_but_none_na = all(is.na(c_across(BAS_ChonicCardiacDisease:BAS_IatrogenicImmuno))),
      BAS_com_count = sum(c_across(BAS_ChonicCardiacDisease:BAS_IatrogenicImmuno) == "Yes", na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(
      across(BAS_Comorbidities_None:BAS_IatrogenicImmuno, ~ if_else(is.na(.x), "No", .x))
    ) %>%
    mutate(
      # 11 patients had peripheral oxygen reported but said "No" to no room air
      BAS_PeripheralOxygen = case_when(
        BAS_OnRoomAir24hrs == "No" ~ NA_real_,
        TRUE ~ BAS_PeripheralOxygen
      ),
      # If "Unknown" then set to missing
      BAS_OnRoomAir24hrs = case_when(
        BAS_OnRoomAir24hrsUnknown == "Yes" ~ NA_character_,
        TRUE ~ BAS_OnRoomAir24hrs
      )
    ) |>
    add_supplemental_oxygen()
}


#' @title format_withdrawal_data
#' @param wth Withdrawal data
#' @returns Formatted data
#' @export
format_withdrawal_data <- function(wth) {
  wth %>%
    add_withdrawn_followup()
}


#' @title format_discharge_data
#' @param dis Discharge data
#' @returns Formatted data
#' @export
format_discharge_data <- function(dis) {
  dis %>%
    mutate(
      DIS_Outcome = factor(DIS_Outcome, levels = c(
        "Death",
        "Discharged alive (including hospital in the home)",
        "Discharged alive but against medical advice",
        "Unknown"
      )),
      DIS_death = case_when(
        DIS_Outcome == "Death" ~ 1L,
        is.na(DIS_Outcome) | DIS_Outcome == "Unknown" ~ NA_integer_,
        TRUE ~ 0L
      ),
      DIS_DAMA = case_when(
        DIS_Outcome == "Discharged alive but against medical advice" ~ 1L,
        is.na(DIS_Outcome) | DIS_Outcome == "Unknown" ~ NA_integer_,
        TRUE ~ 0L
      )
    ) %>%
    # Check antiviral and immouno use
    relocate(DIS_ReceivedNone, .before = "DIS_CamostatReceived") %>%
    relocate(Dis_ImmunoNone, .before = "DIS_ImmunoAnakinra") %>%
    rowwise() %>%
    mutate(
      DIS_av_all_na = all(is.na(c_across(DIS_ReceivedNone:DIS_ReceivedOther))),
      DIS_av_count = sum(c_across(DIS_CamostatReceived:DIS_ReceivedOther) == 1, na.rm = TRUE),
      DIS_im_all_na = all(is.na(c_across(Dis_ImmunoNone:DIS_IummunoOther))),
      DIS_im_count = sum(c_across(DIS_ImmunoAnakinra:DIS_IummunoOther) == 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      # Replace NA with "No"
      across(DIS_ReceivedNone:DIS_ReceivedOther, ~ if_else(is.na(.x), "No", .x)),
      across(Dis_ImmunoNone:DIS_IummunoOther, ~ if_else(is.na(.x), "No", .x))
    )
}


#' @title format_d28_data
#' @param dat Day 28 data
#' @returns Formatted data
#' @export
format_d28_data <- function(dat) {
  dat %>%
    add_d28_corrections() %>%
    mutate(
      D28_who = as.integer(substr(D28_Status, 1, 1)),
      D28_who2 = as.integer(substr(D28_StatusVentilation, 1, 1)),
      # Required respiratory support AT D28
      D28_resp = case_when(
        D28_who < 6 ~ 0L,
        D28_who == 7 | (D28_who == 6 & D28_who2 %in% c(2, 4)) ~ 1L,
        is.na(D28_who) ~ NA_integer_,
        TRUE ~ NA_integer_
      ),
      # Days free of ventilation up to day 28
      D28_dfv = pmin(28, D28_OutcomeDaysFreeOfVentilation),
      # Days free of hospital up to day 28
      D28_dfh = pmax(0, 28 - D28_OutcomeTotalDaysHospitalised)
    )
}


#' @title format_d90_data
#' @param dat Day 90 data
#' @returns Formatted data
#' @export
format_d90_data <- function(dat) {
  dat
}


#' @title format_daily_data
#' @description
#' Apply formatting to the raw daily data extract.
#' @param dd Raw daily data extract
#' @returns Formatted daily data
#' @export
format_daily_data <- function(dd) {
  dd %>%
    mutate(
      DD_who = as.integer(substr(DD_ParticipantDailyStatus, 1, 1)),
      DD_who2 = as.integer(substr(DD_O2, 1, 1)),
      DD_DoseLMWH = as.numeric(DD_DoseLMWH)
    ) %>%
    arrange(StudyPatientID, DD_StudyDay) |>
    # Add dose for ALF00015 on day 9
    mutate(
      DD_NafamostatAdministered = if_else(StudyPatientID == "ALF00015" & DD_StudyDay == 9, "Yes", DD_NafamostatAdministered),
      DD_NafamostatDuration = if_else(StudyPatientID == "ALF00015" & DD_StudyDay == 9, 3, DD_NafamostatDuration)
    )
}


#' @title summarise_daily_data
#' @description
#' Summarise the daily data.
#' Requires fields from other baseline extracts, so
#' assumes all necessary variables are included in `dd`.
#' @param dd Daily data extract with additional variables as required.
#' @return Dataset with one row per patient summarising their daily data.
#' @export
summarise_daily_data <- function(dd) {
  dd %>%
    filter(DD_StudyDay <= 28 | is.na(DD_StudyDay)) %>%
    group_by(StudyPatientID) %>%
    summarise(
      DD_total_records = n(),
      DD_total_days = max(DD_StudyDay),
      DD_who_missing = sum(is.na(DD_who)),
      DD_who_worst = max(DD_who, na.rm = TRUE),
      DD_who_best = min(DD_who, na.rm = TRUE),
      DD_who_gteq3 = sum(DD_who >= 3, na.rm = TRUE),
      # Pre-derived variable for meeting primary outcome
      DD_po = as.integer(any(DD_PrimaryEndpointReachedToday == "Yes", na.rm = TRUE)),
      # Daily ventilation status (new ventilation meeting po outcome)
      DD_vent = as.integer(any(DD_who2 %in% c(2, 4) | DD_who == 7)),
      # Daily ventilation status (ECMO/mechanical)
      DD_mechecmo = as.integer(any(DD_who == 7, na.rm = T)),
      DD_vasop = as.integer(any(DD_O2VasopressorsInotropes == "Yes")),
      DD_death = as.integer(any(DD_who == 8)),
      DD_recover_who = as.integer(any(DD_who <= 3)),
      DD_ttr_who = as.numeric(findfirst(DD_who <= 3)),
      # Adherence
      DD_any_aspirin = any(DD_AspirinAdministered == "Yes", na.rm = TRUE),
      DD_any_days_no_aspirin = any(DD_AspirinAdministered == "No" | is.na(DD_AspirinAdministered)),
      DD_any_days_no_aspirin_excld1 = any(
        (DD_AspirinAdministered == "No" | is.na(DD_AspirinAdministered)) & (DD_StudyDay > 1 & DD_StudyDay < max(DD_StudyDay)),
        na.rm = TRUE
      ) | all(is.na(DD_AspirinAdministered)) | all(DD_AspirinAdministered == "No"),
      DD_n_aspirin = sum(DD_AspirinAdministered == "Yes", na.rm = TRUE),
      DD_n_aspirin_2toDis =
        sum((DD_AspirinAdministered == "Yes") & (DD_StudyDay > 1),
          na.rm = TRUE
        ),
      DD_n_aspirin_2toDisExcl =
        sum((DD_AspirinAdministered == "Yes") & (DD_StudyDay > 1 & DD_StudyDay < pmin(max(DD_StudyDay), 28)),
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    mutate(
      DD_missing = pmin(28, DD_total_days) - DD_total_records,
      DD_any_missing = if_else(is.na(DD_total_days) | DD_missing > 0, 1, 0),
    )
}

# Full datasets ----

#' @title create_fulldata_no_daily()
#' @description
#' Basically joins all the datasets together, except for the daily records.
#' Perform additional formatting here, in sub-steps for each raw data table.
#' @returns Full dataset without daily records
#' @export
create_fulldata_no_daily <- function() {
  eligibility %>%
    left_join(enrolled, by = "StudyPatientID") %>%
    left_join(consent, by = "StudyPatientID") %>%
    left_join(baseline, by = "StudyPatientID") %>%
    left_join(withdrawal, by = "StudyPatientID") %>%
    left_join(discharge, by = "StudyPatientID") %>%
    left_join(d28, by = "StudyPatientID") %>%
    left_join(d90, by = "StudyPatientID") %>%
    left_join(
      summarise_daily_data(format_daily_data(daily)),
      by = "StudyPatientID"
    ) %>%
    # Need database corrections for some formatting, so apply first
    add_database_corrections() %>%
    # add_outcome_corrections() %>%
    format_eligibility_data() %>%
    format_enrolled_data() %>%
    format_consent_data() %>%
    format_baseline_data() %>%
    format_withdrawal_data() %>%
    format_discharge_data() %>%
    format_d28_data() %>%
    format_d90_data() %>%
    # Additional derivations requiring full data
    mutate(
      ENR_rec = if_else(is.na(ENR_rec), 0, ENR_rec),
      CON_rec = if_else(is.na(CON_rec), 0, CON_rec),
      BAS_rec = if_else(is.na(BAS_rec), 0, BAS_rec),
      WTH_rec = if_else(is.na(WTH_rec), 0, WTH_rec),
      DIS_rec = if_else(is.na(DIS_rec), 0, DIS_rec),
      D28_rec = if_else(is.na(D28_rec), 0, D28_rec),
      D90_rec = if_else(is.na(D90_rec), 0, D90_rec),
      DD_total_records = if_else(is.na(DD_total_records), 0L, DD_total_records),
      WTH_FU = if_else(is.na(WTH_FU), 0L, WTH_FU),
      WTH_day = as.integer(CON_WithdrawnDate - RandDate + 1)
    ) %>%
    fix_crp_units() %>%
    add_v5_activation_date() %>%
    add_primary_outcome_components() %>%
    add_days_alive_and_free() %>%
    add_days_alive_and_free_ventilation() %>%
    add_shortness_of_breath() %>%
    add_breathlessness_scale() %>%
    add_time_to_recovery()
}


#' @title create_fulldata_add_daily
#' @description
#' Adds daily data to the dataset
#' @param dat Full dataset from `create_fulldata_no_daily`
#' @export
create_fulldata_add_daily <- function(dat) {
  dat %>%
    full_join(
      daily %>% format_daily_data(),
      by = "StudyPatientID"
    ) %>%
    mutate(
      ENR_rec = if_else(is.na(ENR_rec), 0, ENR_rec),
      CON_rec = if_else(is.na(CON_rec), 0, CON_rec),
      BAS_rec = if_else(is.na(BAS_rec), 0, BAS_rec),
      WTH_rec = if_else(is.na(WTH_rec), 0, WTH_rec),
      DIS_rec = if_else(is.na(DIS_rec), 0, DIS_rec),
      D28_rec = if_else(is.na(D28_rec), 0, D28_rec),
      D90_rec = if_else(is.na(D90_rec), 0, D90_rec),
      DD_rec = if_else(is.na(DD_rec), 0, DD_rec)
    )
}


#' @title create_and_save_derived_data
#' @param dir1 Directory for raw data
#' @param dir2 Directory for derived data
#' @export
create_and_save_derived_data <- function(dir1, dir2) {
  read_all_raw_extracts(dir1)
  all_dat <- create_fulldata_no_daily()
  all_dat_daily <- create_fulldata_add_daily(all_dat)
  save_derived_dataset(dir2, all_dat, "all_data.rds")
  save_derived_dataset(dir2, all_dat_daily, "all_daily_data.rds")
}
