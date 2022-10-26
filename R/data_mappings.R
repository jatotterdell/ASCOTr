#' @title intervention_labels
#' @description
#' Get intervention labels for all domains
#' @returns A list of intervention labels with explicit breaks
#' @export
intervention_labels <- function() {
  list(
    AAssignment = c(
      "A0" = "Not randomised to antiviral",
      "A1" = "No specific antiviral",
      "A2" = "Nafamostat"
    ),
    CAssignment = c(
      "C0" = "Not randomised to anticoagulation",
      "C1" = "Low<br>dose",
      "C2" = "Intermediate<br>dose",
      "C3" = "Low dose<br>with aspirin",
      "C4" = "Therapeutic<br>dose"
    )
  )
}


#' @title intervention_labels2
#' @description
#' Get intervention labels for all domains
#' @returns A list of intervention labels without breaks
#' @export
intervention_labels2 <- function() {
  list(
    AAssignment = c(
      "A0" = "Not randomised to antiviral",
      "A1" = "No specific antiviral",
      "A2" = "Nafamostat"
    ),
    CAssignment = c(
      "C0" = "Not randomised to anticoagulation",
      "C1" = "Low-dose",
      "C2" = "Intermediate-dose",
      "C3" = "Low-dose with aspirin",
      "C4" = "Therapeutic-dose"
    )
  )
}


#' @title intervention_labels_short
#' @description
#' Get intervention labels for all domains
#' @returns A list of short intervention labels
#' @export
intervention_labels_short <- function() {
  list(
    AAssignment = c(
      "A0" = "Not randomised A",
      "A1" = "None",
      "A2" = "Nafamostat"
    ),
    CAssignment = c(
      "C0" = "Not randomised C",
      "C1" = "Low",
      "C2" = "Intermediate",
      "C3" = "Low with aspirin",
      "C4" = "Therapeutic"
    )
  )
}


#' @title intervention_labels_short_break
#' @description
#' Get intervention labels for all domains
#' @returns A list of short intervention labels with breaks
#' @export
intervention_labels_short_break <- function() {
  list(
    AAssignment = c(
      "A0" = "Not\nrandomised A",
      "A1" = "None",
      "A2" = "Nafamostat"
    ),
    CAssignment = c(
      "C0" = "Not\nrandomised C",
      "C1" = "Low",
      "C2" = "Intermediate",
      "C3" = "Low with\naspirin",
      "C4" = "Therapeutic"
    )
  )
}


#' @title get_intervention_dates
#' @export
get_intervention_dates <- function() {
  tribble(
    ~ Domain, ~ Intervention, ~ stdate, ~ endate,
    "Anticoagulation", "C1", as.Date("2021-02-18"), as.Date("2022-04-08"),
    "Anticoagulation", "C2", as.Date("2021-02-18"), as.Date("2022-04-08"),
    "Anticoagulation", "C3", as.Date("2021-02-18"), as.Date("2021-09-10"),
    "Anticoagulation", "C4", as.Date("2021-10-14"), as.Date("2022-04-08"),
    "Antiviral", "A1", as.Date("2021-06-10"), as.Date("2022-04-09"),
    "Antiviral", "A2", as.Date("2021-06-10"), as.Date("2022-04-09")
  ) %>%
    mutate(Intervention = labelled(
      Intervention,
      labels = c(
        C1 = "Standard dose", C2 = "Intermediate dose", C3 = "Standard dose + aspirin", C4 = "Therapeutic dose",
        A1 = "No specific antiviral", A2 = "Nafamostat"
      ),
      label = "Domain intervention"))
}


#' @title get_interim_dates
#' @export
get_interim_dates <- function() {
  tribble(
    ~ meet_num, ~ meet_date,
    1, as.Date("2021-07-21"),
    2, as.Date("2021-09-15"),
    3, as.Date("2021-12-01"),
    4, as.Date("2022-02-22")
  )
}


#' @title intervention_strata
#' @export
intervention_strata <- function() {
  tribble(
    ~ stdate, ~ endate, ~ strata_int, ~ strata_lab,
    as.Date("2021-02-18"), as.Date("2021-06-09"), 1, "Add A",
    as.Date("2021-06-10"), as.Date("2021-09-10"), 2, "Drop C3",
    as.Date("2021-09-11"), as.Date("2021-10-13"), 3, "Add C4",
    as.Date("2021-10-14"), as.Date("2022-04-08"), 4, "Drop C",
  )
}


#' @title pp_lwmh_dose
#' @description
#' Note for therapeutic dose
#' Enoxaparin: PP_Dose is 1 x BAS_Weight twice per day, or 1.5 x BAS_Weight once
#' per day. PP_Dose is in milligrams (mg) at the frequency in the 'times' column.
#' Tinzaparin: PP_Dose is IU/kg - Note only 1 patient received Tinzaparin and
#' none received Dalteparin.
#' Common to protocol 3 (incl C3 arm) and protocol 5.
#' Matches original pp_lmwh_dose table that did not account
#' for creatinine levels at baseline and assumes creatinine clearance >30mL/min.
#' @export
pp_lwmh_dose <- function() {
  tribble(
    ~DD_TypeLMWH, ~times, ~weight_low, ~weight_high, ~weight_group, ~CAssignment, ~PP_Dose,
    "Enoxaparin", "Once", 0, 50, "< 50", "C1", 20,
    "Enoxaparin", "Once", 0, 50, "< 50", "C2", 40,
    "Enoxaparin", "Once", 0, 50, "< 50", "C3", 20,
    "Enoxaparin", "Twice", 0, 50, "< 50", "C4", 1,
    "Enoxaparin", "Once", 0, 50, "< 50", "C4", 1.5,
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C1", 40,
    "Enoxaparin", "Twice", 50, 120, "50 - 120", "C2", 40,
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C2", 80,
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C3", 40,
    "Enoxaparin", "Twice", 50, 120, "50 - 120", "C4", 1,
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C4", 1.5,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C1", 60,
    "Enoxaparin", "Twice", 120, Inf, "> 120", "C2", 60,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C2", 120,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C3", 60,
    "Enoxaparin", "Twice", 120, Inf, "> 120", "C4", 1,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C4", 1.5,
    "Tinzaparin", "Once", NA, NA, NA, "C1", 75,
    "Tinzaparin", "Once", NA, NA, NA, "C2", 125,
    "Tinzaparin", "Once", NA, NA, NA, "C3", 75,
    "Tinzaparin", "Once", NA, NA, NA, "C4", 175
  )
}


#' @title pp_lwmh_dose_creat_under
#' @description
#' Protocol 5 Dosing table is not applicable to C3 (standard dose + aspirin)
#' Different means differs from creatinine over >30 dosing
#' @export
pp_lwmh_dose_creat_under <- function() {
  tribble(
    ~DD_TypeLMWH, ~times, ~weight_low, ~weight_high, ~weight_group, ~CAssignment, ~PP_Dose,
    "Enoxaparin", "Once", 0, 50, "< 50", "C1", 20,
    "Enoxaparin", "Once", 0, 50, "< 50", "C2", 0.5, # different
    "Enoxaparin", "Once", 0, 50, "< 50", "C4", 1, # different
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C1", 20,
    "Enoxaparin", "Once", 0, 50, "50 - 120", "C2", 0.5, # different
    "Enoxaparin", "Once", 50, 120, "50 - 120", "C4", 1,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C1", 40,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C2", 0.5,
    "Enoxaparin", "Once", 120, Inf, "> 120", "C4", 1,
    "Tinzaparin", "Once", NA, NA, NA, "C1", 75,
    "Tinzaparin", "Once", NA, NA, NA, "C2", 125,
    "Tinzaparin", "Once", NA, NA, NA, "C4", 175
  )
}
