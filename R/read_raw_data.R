#' read_eligibility_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
#' @import readr
#' @import dplyr
read_eligibility_file <- function(fn) {
  eligibility <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      EL_DOB = col_date("%d-%b-%Y"),
      EL_YOB = col_integer(),
      EL_AgeAtEntry = col_integer(),
      EL_AdmittedToHospital = col_date("%d-%b-%Y"),
      EL_Referral = col_date("%d-%b-%Y"),
      EL_Screened = col_date("%d-%b-%Y"),
      EL_BloodPlateletTestValue = col_double(),
      EL_BloodPlateletTestAs_x10_9_L = col_double(),
      EL_SerumCreatinineBlood = col_double(),
      EL_SerumCreatinine_umolL = col_double(),
      EL_SerumPotassium = col_double(),
      EL_SerumSodium = col_double(),
      EL_eGFR = col_double(),
      EL_FirstPositiveTest = col_date("%d-%b-%Y"),
      EL_FirstEnteredLocalTime = col_datetime("%d/%m/%Y %H:%M:%S"),
      EL_FirstEnteredUTC = col_datetime("%d/%m/%Y %H:%M:%S"),
      EL_EligibilityLastUpdated = col_datetime("%d/%m/%Y %H:%M:%S"),
      EL_EligibilityLastUpdatedUTC = col_datetime("%d/%m/%Y %H:%M:%S"),
      EL_Con_ConsentDate = col_datetime("%d/%m/%Y %H:%M:%S"),
      EL_PregnancyDueDate = col_date("%d-%b-%Y"),
      FirstSymptoms = col_date("%d-%b-%Y")
    )
  ) %>%
    rename(EL_FirstSymptoms = FirstSymptoms) %>%
    filter(EL_ProtocolVersion != "1.0") %>%
    mutate(EL_rec = 1)

  # Note that there appears to be a duplicated column: "EL_OralTherapeuticAnticoagAgents"
  # The two columns appear to be exact duplicates, so one is removed here
  eligibility <- eligibility %>%
    rename(EL_OralTherapeuticAnticoagAgents = EL_OralTherapeuticAnticoagAgents...38) %>%
    mutate(
      EL_OralTherapeuticAnticoagAgents = if_else(
        is.na(EL_OralTherapeuticAnticoagAgents) & !is.na(EL_OralTherapeuticAnticoagAgents...50),
        EL_OralTherapeuticAnticoagAgents...50,
        EL_OralTherapeuticAnticoagAgents
      )
    ) %>%
    select(-EL_OralTherapeuticAnticoagAgents...50) %>%
    # As requested, exclude ineligible participant
    filter(StudyPatientID != "BLK00001" | is.na(StudyPatientID))
  return(eligibility)
}


#' read_enrolled_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
#' @importFrom stringr str_replace_all
read_enrolled_file <- function(fn) {
  enrolled <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      AgeAtEntry = col_double(),
      PT_DOD = col_date("%d-%b-%Y"),
      PT_YOB = col_double()
    )
  ) %>%
    mutate(
      RandomisedLocal = readr::parse_datetime(str_replace_all(
        RandomisedLocal, c("a.m." = "AM", "p.m." = "PM")
      ), "%d/%m/%Y %H:%M:%S %p"),
      DOB = readr::parse_date(stringr::str_replace_all(DOB, c(" 12:00:00 a.m." = "", " 12:00:00 p.m." = "")), "%d/%m/%Y"),
      RandDate = as.Date(RandomisedLocal)
    ) %>%
    rename(StudyPatientID = StudyID) %>%
    mutate(ENR_rec = 1)
  return(enrolled)
}


#' read_consent_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_consent_file <- function(fn) {
  consent <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      CON_ConsentDate = col_datetime("%d/%m/%Y %H:%M:%S")
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion) %>%
    mutate(CON_rec = 1)
}


#' read_withdrawal_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_withdrawal_file <- function(fn) {
  withdrawal <- read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      Con_WithdrawnDate = col_date("%d-%b-%Y"),
      CON_DatePatientIneligible = col_date("%d-%b-%Y")
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    # Fix some inconsistent naming capitalisation
    rename(
      CON_WithdrawnBy = Con_WithdrawnBy,
      CON_WithdrawnDate = Con_WithdrawnDate,
      CON_WithdrawalReason = Con_WithdrawalReason,
      CON_WithdrawalReason_Other = Con_WithdrawalReason_Other,
      CON_WithdrawalClincianReason = Con_WithdrawalClincianReason,
      CON_WithdrawnDomainA = Con_WithdrawnDomainA
    ) %>%
    select(-PT_ProtocolVersion) %>%
    mutate(WTH_rec = 1)
}


#' read_baseline_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_baseline_file <- function(fn) {
  baseline <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      BAS_Weight = col_double(),
      BAS_VaccineDosesReceived = col_double(),
      BAS_DateLastVaccineDose = col_datetime("%d/%m/%Y %H:%M:%S"),
      BAS_PeripheralOxygen = col_double(),
      BAS_RespRateHighest = col_double(),
      BAS_UreaEntered = col_double(),
      BAS_UreaResult = col_double(),
      BAS_CRPEntered = col_double(),
      BAS_CRPResult = col_double(),
      BAS_DateRespiratoryTest = col_date("%d-%b-%Y"),
      BAS_CycleThresholdValue = col_double(),
      BAS_DDimerEntered = col_double(),
      BAS_DDimerResult = col_double(),
      BAS_APTT = col_double(),
      BAS_INR = col_double(),
      BAS_FibrinogenTest = col_double(),
      BAS_FibrinogenResult = col_double(),
      BAS_ProthrombinTime = col_double()
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion) %>%
    mutate(BAS_rec = 1)
  return(baseline)
}


#' read_discharge_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_discharge_file <- function(fn) {
  discharge <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      DIS_DateOfDischarge = col_date("%d-%b-%Y"),
      DIS_DateOfDeath = col_date("%d-%b-%Y")
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion) %>%
    mutate(DIS_rec = 1)
  return(discharge)
}


#' read_daily_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_daily_file <- function(fn) {
  daily <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      DD_Date = col_date(format = "%d-%b-%Y"),
      DD_StudyDay = col_double(),
      DD_NafamostatDailyDose = col_double(),
      DD_NafamostatDuration = col_double(),
      DD_Potassium = col_double(),
      DD_Sodium = col_double(),
      DD_ALTLabs = col_double(),
      DD_ASTLabs = col_double(),
      DD_RespiratorySampleDate = col_date(format = "%d-%b-%Y")
    )
  ) %>%
    group_by(StudyPatientID) %>%
    mutate(DD_n = max(DD_StudyDay)) %>%
    ungroup() %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion) %>%
    mutate(DD_rec = 1)
}


#' read_d28_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_d28_file <- function(fn) {
  d28 <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      .default = col_character(),
      D28_DateOfDeath = col_date("%d-%b-%Y"),
      D28_DateOfFollowUp = col_date("%d-%b-%Y"),
      D28_LastKnownDateAlive = col_date("%d-%b-%Y"),
      D28_OutcomeTotalDaysHospitalised = col_double(),
      D28_OutcomeDaysFreeOfVentilation = col_double(),
      D28_BreathScale = col_double(),
      D28_EQMobility = col_double(),
      D28_EQPersonalCare = col_double(),
      D28_EQUsualActivities = col_double(),
      D28_EQPainDiscomfort = col_double(),
      D28_EQAnxietyDepression = col_double(),
      D28_EQOverallHealthScore = col_double()
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion) %>%
    mutate(D28_rec = 1)
  return(d28)
}


#' read_d90_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_d90_file <- function(fn) {
  d90 <- read_csv(
    fn,
    na = c("", "NA", "n/a"),
    col_types = cols(
      D90_DateOfFollowUp = col_date(format = "%d-%b-%Y"),
      D90_LastKnownDateAlive = col_date(format = "%d-%b-%Y"),
      PT_ProtocolVersion = col_character()
    )
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion, -FormLock) %>%
    mutate(D90_rec = 1)
  return(d90)
}


#' read_deviation_file
#'
#' @param fn Filename
#' @return tibble of data
#' @export
read_deviation_file <- function(fn) {
  deviation <- readr::read_csv(
    fn,
    na = c("", "NA", "n/a"),
  ) %>%
    filter(PT_ProtocolVersion != "1.0") %>%
    select(-PT_ProtocolVersion)
  return(deviation)
}


#' @title read_raw_extracts
#' @description
#' Read the raw data for extract `fn` from `dir`.
#'
#' @param dir Data directory
#' @param fn The name of the extract as a string, or a vector of extract names
#' @return Returns nothing, but adds the extracted data to the global environment.
#' Always reads the most recent data (based on ordering of file name).
#' @export
#' @importFrom utils tail
read_raw_extracts <- function(dir, fn) {
  dirs <- file.path(dir, fn)
  versions <- sapply(dirs, function(x) tail(list.files(x), 1))
  paths <- normalizePath(file.path(dirs, versions))
  for (i in 1:length(fn)) {
    assign(fn[i], get(paste0("read_", fn[i], "_file"))(paths[i]), envir = .GlobalEnv)
  }
}


#' @title read_all_raw_extracts
#' @description
#' Read all the raw data extracts in `dir`.
#'
#' @param dir Data directory
#' @return Returns nothing, but adds all data extracts to global environment.
#' @export
read_all_raw_extracts <- function(dir) {
  fns <- c("eligibility", "consent", "enrolled", "baseline", "withdrawal", "discharge", "daily", "d28", "d90")
  read_raw_extracts(dir, fns)
}
