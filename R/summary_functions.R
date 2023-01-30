# Baseline summary - demographics  ----


#' @title generate_baseline_demographics_by
#' @description
#' Generate baseline demographics summary by a grouping variable
#'
#' @param data The dataset. assumed to have baseline data included
#' @param grpvar The grouping variable
generate_baseline_demographics_by <- function(dat, grpvar = NULL) {
  grpvar <- enquo(grpvar)
  no_baseline <- sum(dat$BAS_rec == 0)
  tab <- dat %>%
    group_by(!!grpvar) %>%
    summarise(
      `Age (years), Median (IQR)` = sprintf("%.0f (%.0f, %.0f)", median(AgeAtEntry), quantile(AgeAtEntry, 0.25), quantile(AgeAtEntry, 0.75)),
      `Country_India, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "IN", na.rm = TRUE), 100 * sum(Country == "IN", na.rm = TRUE) / n()),
      `Country_Australia, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "AU", na.rm = TRUE), 100 * sum(Country == "AU", na.rm = TRUE) / n()),
      `Country_Nepal, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "NP", na.rm = TRUE), 100 * sum(Country == "NP", na.rm = TRUE) / n()),
      `Country_New Zealand, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "NZ", na.rm = TRUE), 100 * sum(Country == "NZ", na.rm = TRUE) / n()),
      `Sex_Male, n (\\%)` = sprintf("%i (%.0f)", sum(Sex == "Male", na.rm = TRUE), 100 * sum(Sex == "Male", na.rm = TRUE) / n()),
      `Sex_Female, n (\\%)` = sprintf("%i (%.0f)", sum(Sex == "Female", na.rm = TRUE), 100 * sum(Sex == "Female", na.rm = TRUE) / n()),
      `Weight_Median, (IQR)` = sprintf("%.0f (%.0f, %.0f)", median(BAS_Weight, na.rm = T), quantile(BAS_Weight, 0.25, na.rm = T), quantile(BAS_Weight, 0.75, na.rm = T)),
      `Weight_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_Weight)), 100 * sum(is.na(BAS_Weight)) / n()),
      `Vaccinated_Yes, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE), 100 * sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE) / n()),
      `Vaccinated_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_PatientVaccinated)), 100 * sum(is.na(BAS_PatientVaccinated)) / n()),
      `Ethnicity_Indian, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_European, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Asian, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Pacific Islander, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityPacificIslander == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityPacificIslander == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Middle Eastern, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Maori, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityMaori == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityMaori == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_African, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Aboriginal, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Latin American, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Other, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityOther == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityOther == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Unknown, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityUnknown == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityUnknown == "Yes", na.rm = TRUE) / n()),
      `Smoking_Current, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Current", na.rm = TRUE), 100 * sum(BAS_Smoking == "Current", na.rm = TRUE) / n()),
      `Smoking_Former, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Former", na.rm = TRUE), 100 * sum(BAS_Smoking == "Former", na.rm = TRUE) / n()),
      `Smoking_Never, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Never", na.rm = TRUE), 100 * sum(BAS_Smoking == "Never", na.rm = TRUE) / n()),
      `Smoking_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_Smoking)), 100 * sum(is.na(BAS_Smoking)) / n())
    ) %>%
    gather(Variable, value, -!!grpvar, factor_key = TRUE) %>%
    spread(!!grpvar, value)
  colnames(tab)[-1] <- dat %>%
    dplyr::count(!!grpvar) %>%
    mutate(lab = paste0(!!grpvar, "<br><br>(n = ", n, ")")) %>%
    pull(lab)
  return(tab)
}


#' @title generate_baseline_demographics
#' @description
#' Generate baseline demographics summary overall
#'
#' @param data The dataset. assumed to have baseline data included
generate_baseline_demographics <- function(dat) {
  no_baseline <- sum(dat$BAS_rec == 0)
  overall <- dat %>%
    summarise(
      `Age (years), Median (IQR)` = sprintf("%.0f (%.0f, %.0f)", median(AgeAtEntry), quantile(AgeAtEntry, 0.25), quantile(AgeAtEntry, 0.75)),
      `Country_India, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "IN", na.rm = TRUE), 100 * sum(Country == "IN", na.rm = TRUE) / n()),
      `Country_Australia, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "AU", na.rm = TRUE), 100 * sum(Country == "AU", na.rm = TRUE) / n()),
      `Country_Nepal, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "NP", na.rm = TRUE), 100 * sum(Country == "NP", na.rm = TRUE) / n()),
      `Country_New Zealand, n (\\%)` = sprintf("%i (%.0f)", sum(Country == "NZ", na.rm = TRUE), 100 * sum(Country == "NZ", na.rm = TRUE) / n()),
      `Sex_Male, n (\\%)` = sprintf("%i (%.0f)", sum(Sex == "Male", na.rm = TRUE), 100 * sum(Sex == "Male", na.rm = TRUE) / n()),
      `Sex_Female, n (\\%)` = sprintf("%i (%.0f)", sum(Sex == "Female", na.rm = TRUE), 100 * sum(Sex == "Female", na.rm = TRUE) / n()),
      `Weight_Median, (IQR)` = sprintf("%.0f (%.0f, %.0f)", median(BAS_Weight, na.rm = T), quantile(BAS_Weight, 0.25, na.rm = T), quantile(BAS_Weight, 0.75, na.rm = T)),
      `Weight_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_Weight)), 100 * sum(is.na(BAS_Weight)) / n()),
      `Vaccinated_Yes, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE), 100 * sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE) / n()),
      `Vaccinated_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_PatientVaccinated)), 100 * sum(is.na(BAS_PatientVaccinated)) / n()),
      `Ethnicity_Indian, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_European, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Asian, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Pacific Islander, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityPacificIslander == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityPacificIslander == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Middle Eastern, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Maori, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityMaori == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityMaori == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_African, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Aboriginal, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Latin American, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Other, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityOther == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityOther == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Unknown, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_EthnicityUnknown == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityUnknown == "Yes", na.rm = TRUE) / n()),
      `Smoking_Current, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Current", na.rm = TRUE), 100 * sum(BAS_Smoking == "Current", na.rm = TRUE) / n()),
      `Smoking_Former, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Former", na.rm = TRUE), 100 * sum(BAS_Smoking == "Former", na.rm = TRUE) / n()),
      `Smoking_Never, n (\\%)` = sprintf("%i (%.0f)", sum(BAS_Smoking == "Never", na.rm = TRUE), 100 * sum(BAS_Smoking == "Never", na.rm = TRUE) / n()),
      `Smoking_Missing, n (\\%)` = sprintf("%i (%.0f)", sum(is.na(BAS_Smoking)), 100 * sum(is.na(BAS_Smoking)) / n())
    ) %>%
    gather(Variable, Overall, factor_key = TRUE)
  colnames(overall)[2] <- paste0("Overall<br><br>(n = ", nrow(dat), ")")
  return(overall)
}


#' @title generate_baseline_demographics_table
#' @description
#' Generate baseline demographics tables
#'
#' @param data The dataset. assumed to have baseline data included
#' If FALSE produce one table per domain.
#' @return Either a single table or a list of tables, one for each domain.
#' @export
generate_baseline_demographics_table <- function(dat, format = "html") {
  byAgrp <- generate_baseline_demographics_by(dat %>% filter(AAssignment != "A0"), AAssignment)
  byCgrp <- generate_baseline_demographics_by(dat %>% filter(CAssignment != "C0"), CAssignment)
  ovrA <- generate_baseline_demographics(dat %>% filter(AAssignment != "A0"))
  ovrC <- generate_baseline_demographics(dat %>% filter(CAssignment != "C0"))
  tabA <- left_join(byAgrp, ovrA, by = "Variable") %>%
    mutate(Variable = str_replace(Variable, "[A-z]*_", ""))
  tabC <- left_join(byCgrp, ovrC, by = "Variable") %>%
    mutate(Variable = str_replace(Variable, "[A-z]*_", ""))
  fsize <- 12
  if (format == "latex") {
    fsize <- 9
    colnames(tabA) <- linebreak(colnames(tabA), linebreaker = "<br>", align = "c")
    colnames(tabC) <- linebreak(colnames(tabC), linebreaker = "<br>", align = "c")
  }
  outA <- kable(
    tabA,
    format = format,
    booktabs = T,
    caption = "Baseline demographics for participants randomised into domain A.",
    escape = F,
    align = "lrrrrr"
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      latex_options = "HOLD_position",
      font_size = fsize
    ) %>%
    pack_rows("Country", 2, 5) %>%
    pack_rows("Sex", 6, 7) %>%
    pack_rows("Weight (kg)", 8, 9) %>%
    pack_rows("Ethnicity", 12, 22) %>%
    pack_rows("Smoking", 23, 26) %>%
    add_header_above(c(" " = 1, "Antiviral" = ncol(byAgrp) - 1, " " = 1)) %>%
    row_spec(0, align = "c") %>%
    footnote(
      number = "Site LUD did not have ethics approval for collection of vaccination status.",
      fixed_small_size = TRUE
    )
  outC <- kable(
    tabC,
    format = format,
    booktabs = T,
    caption = "Baseline demographics for participants randomised into domain C.",
    escape = F,
    align = "lrrrrr"
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      latex_options = "HOLD_position",
      font_size = fsize
    ) %>%
    pack_rows("Country", 2, 5) %>%
    pack_rows("Sex", 6, 7) %>%
    pack_rows("Weight (kg)", 8, 9) %>%
    pack_rows("Ethnicity", 12, 22) %>%
    pack_rows("Smoking", 23, 26) %>%
    add_header_above(c(" " = 1, "Anticoagulation" = ncol(byCgrp) - 1, " " = 1)) %>%
    row_spec(0, align = "c") %>%
    footnote(
      number = "Site LUD did not have ethics approval for collection of vaccination status.",
      fixed_small_size = TRUE
    )
  if (format == "latex") {
    outA <- outA %>% pack_rows(., "Vaccinated\\\\textsuperscript{1}", 10, 11, escape = FALSE)
    outC <- outC %>% pack_rows(., "Vaccinated\\\\textsuperscript{1}", 10, 11, escape = FALSE)
  } else {
    outA <- outA %>% pack_rows("Vaccinated<sup>1</sup>", 10, 11, escape = FALSE)
    outC <- outC %>% pack_rows("Vaccinated<sup>1</sup>", 10, 11, escape = FALSE)
  }
  return(list(A = outA, C = outC))
}


# Baseline summary - co-morbidities  ----


#' @title generate_baseline_comorbidities
#' @description
#' Generates baseline co-morbidity summary across all participants
#' @param dat Dataset with baseline variables
#' @return A tibble giving the summary
generate_baseline_comorbidities <- function(dat) {
  basdat <- dat %>%
    select(
      BAS_rec,
      BAS_Comorbidities_None,
      BAS_ChonicCardiacDisease,
      BAS_Hypertension,
      BAS_Obesity,
      BAS_ChronicLungDisease,
      BAS_ObsSleepApnoea,
      BAS_Asthma,
      BAS_Diabetes,
      BAS_ChronicKidneyDisease,
      BAS_Dialysis,
      BAS_ModSevLiverDisease,
      BAS_Dementia,
      BAS_MalignantNeoplasm,
      BAS_HIVInfection,
      BAS_IatrogenicImmuno
    )
  ovr_missing <- basdat %>%
    summarise(name = "Missing, n (\\%)", value = sprintf("%i (%3.0f)", sum(BAS_rec == 0), 100 * sum(BAS_rec == 0) / n()))
  overall <- basdat %>%
    select(-BAS_rec) %>%
    summarise_all(., list(`.n` = ~ sum(.x == "Yes", na.rm = T), `.p` = ~ sum(.x == "Yes", na.rm = T) / n())) %>%
    pivot_longer(everything(), names_to = c("name", "measure"), names_sep = "_\\.") %>%
    pivot_wider(id_cols = name, names_from = measure, values_from = value) %>%
    arrange(-n) %>%
    mutate(value = sprintf("%i (%3.0f)", n, 100 * p)) %>%
    select(-n, -p) %>%
    mutate(
      name = str_replace(name, "BAS_", ""),
      name = str_replace(name, "_Variable", ""),
      name = str_replace(name, "_", ""),
      name = str_replace(name, "ObsS", "ObstructiveS"),
      name = str_replace(name, "Immuno", "Immunosuppression"),
      name = str_replace(name, "ModSev", "ModerateOrSevere"),
      name = str_replace(name, "Comorbidities", ""),
      name = fct_inorder(gsub("([[:upper:]]*)([[:upper:]][[:lower:]]+)", "\\1 \\2", name)),
      name = trimws(stringr::str_to_sentence(name)),
      name = str_replace(name, "Hiv", "HIV"),
      name = str_replace(name, "Chonic", "Chronic"),
      name = paste0(name, ", n (\\%)")
    ) %>%
    add_row(ovr_missing) %>%
    mutate(name = fct_inorder(name))
  colnames(overall)[1] <- "Comorbidity"
  colnames(overall) <- c("Comorbidity", paste0("Overall<br>(n = ", nrow(basdat), ")"))
  return(overall)
}


#' @title generate_baseline_comorbidities_by
#' @description
#' Generates baseline co-morbidity summary across grouping variable
#' @param dat The dataset
#' @param grpvar The grouping variable
#' @return A tibble giving the summary
generate_baseline_comorbidities_by <- function(dat, grpvar = NULL) {
  grpvar <- enquo(grpvar)
  basdat <- dat %>%
    group_by(!!grpvar) %>%
    select(
      BAS_rec,
      !!grpvar,
      BAS_Comorbidities_None,
      BAS_ChonicCardiacDisease,
      BAS_Hypertension,
      BAS_Obesity,
      BAS_ChronicLungDisease,
      BAS_ObsSleepApnoea,
      BAS_Asthma,
      BAS_Diabetes,
      BAS_ChronicKidneyDisease,
      BAS_Dialysis,
      BAS_ModSevLiverDisease,
      BAS_Dementia,
      BAS_MalignantNeoplasm,
      BAS_HIVInfection,
      BAS_IatrogenicImmuno
    )
  missing <- basdat %>%
    group_by(!!grpvar) %>%
    summarise(name = "Missing, n (\\%)", value = sprintf("%i (%3.0f)", sum(BAS_rec == 0), 100 * sum(BAS_rec == 0) / n())) %>%
    spread(!!grpvar, value)
  tab <- basdat %>%
    select(-BAS_rec) %>%
    summarise_all(., list(
      `.n` = ~ sum(.x == "Yes", na.rm = T),
      `.p` = ~ sum(.x == "Yes", na.rm = T) / n()
    )) %>%
    pivot_longer(-!!grpvar, names_to = c("name", "measure"), names_sep = "_\\.") %>%
    pivot_wider(id_cols = !!grpvar:name, names_from = measure, values_from = value) %>%
    mutate(value = sprintf("%i (%3.0f)", n, 100 * p)) %>%
    select(-n, -p) %>%
    spread(!!grpvar, value) %>%
    mutate(
      name = str_replace(name, "BAS_", ""),
      name = str_replace(name, "_Variable", ""),
      name = str_replace(name, "_", ""),
      name = str_replace(name, "ObsS", "ObstructiveS"),
      name = str_replace(name, "Immuno", "Immunosuppression"),
      name = str_replace(name, "ModSev", "ModerateOrSevere"),
      name = str_replace(name, "Comorbidities", ""),
      name = fct_inorder(gsub("([[:upper:]]*)([[:upper:]][[:lower:]]+)", "\\1 \\2", name)),
      name = trimws(stringr::str_to_sentence(name)),
      name = str_replace(name, "Hiv", "HIV"),
      name = str_replace(name, "Chonic", "Chronic"),
      name = paste0(name, ", n (\\%)")
    ) %>%
    add_row(missing)
  colnames(tab)[1] <- "Comorbidity"
  colnames(tab) <- c(
    "Comorbidity",
    basdat %>%
      count(!!grpvar) %>%
      mutate(lab = paste0(!!grpvar, "<br>(n = ", n, ")")) %>%
      pull(lab)
  )
  return(tab)
}


#' @title generate_baseline_comorbidities_table
#' @description
#' Generates baseline co-morbidity summary tables for each domain
#' @param dat The dataset
#' @param closed If TRUE, one table per domain, otherwise just aggregated table
#' @return A list of tibbles giving the summary tables
#' @export
generate_baseline_comorbidities_table <- function(dat, format = "html") {
  ovr <- generate_baseline_comorbidities(dat)
  ovrA <- generate_baseline_comorbidities(dat %>% filter(AAssignment != "A0"))
  ovrC <- generate_baseline_comorbidities(dat %>% filter(CAssignment != "C0"))
  byAgrp <- generate_baseline_comorbidities_by(dat %>% filter(AAssignment != "A0"), AAssignment)
  byCgrp <- generate_baseline_comorbidities_by(dat %>% filter(CAssignment != "C0"), CAssignment)
  tabA <- left_join(ovrA, byAgrp, by = "Comorbidity")[, c(1, (2 + 1:(ncol(byAgrp) - 1)), 2)]
  tabC <- left_join(ovrC, byCgrp, by = "Comorbidity")[, c(1, (2 + 1:(ncol(byCgrp) - 1)), 2)]
  fsize <- 12
  if (format == "latex") {
    fsize <- 9
    colnames(tabA) <- linebreak(colnames(tabA), linebreaker = "<br>", align = "c")
    colnames(tabC) <- linebreak(colnames(tabC), linebreaker = "<br>", align = "c")
  }
  outA <- kable(
    tabA,
    format = format,
    booktabs = T,
    linesep = "",
    caption = "Baseline co-morbidities for participants randomised into the antiviral domain.",
    align = "lrrrrrrrr",
    escape = F
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    add_header_above(c(" " = 1, "Antiviral" = ncol(byAgrp) - 1, " " = 1)) %>%
    row_spec(0, align = "c")
  outC <- kable(
    tabC,
    format = format,
    booktabs = T,
    linesep = "",
    caption = "Baseline co-morbidities for participants randomised into the anticoagulation domain.",
    align = "lrrrrrrrr",
    escape = F
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    add_header_above(c(" " = 1, "Anticoagulation" = ncol(byCgrp) - 1, " " = 1)) %>%
    row_spec(0, align = "c")
  return(list(A = outA, C = outC))
}


# Baseline summary - prognostics  ----


#' @title Baseline prognostics
#' @description
#' Generates baseline prognostics summary across grouping variable
#' @param dat The dataset
#' @param grpvar The grouping variable
#' @return A tibble giving the summary
generate_baseline_prognostics_by <- function(dat, grpvar = NULL) {
  grpvar <- enquo(grpvar)
  tab <- dat %>%
    group_by(!!grpvar) %>%
    summarise(
      "On Room Air_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE),
        100 * sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE) / n()
      ),
      "On Room Air_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_OnRoomAir24hrs)),
        100 * sum(is.na(BAS_OnRoomAir24hrs)) / n()
      ),
      "GCS < 15_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_PatientGCS == "Yes", na.rm = TRUE),
        100 * sum(BAS_PatientGCS == "Yes", na.rm = TRUE) / n()
      ),
      "GCS < 15_Missing, n (\\%)" = sprintf("%i (%.0f)", sum(is.na(BAS_PatientGCS)), 100 * sum(is.na(BAS_PatientGCS)) / n()),
      "Peripheral oxygen saturation (SpO2)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_PeripheralOxygen, na.rm = TRUE),
        quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.25),
        quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.75)
      ),
      "Peripheral oxygen saturation (SpO2)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_PeripheralOxygen)),
        100 * sum(is.na(BAS_PeripheralOxygen) / n())
      ),
      "Highest respiratory rate (breaths/minute)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_RespRateHighest, na.rm = TRUE),
        quantile(BAS_RespRateHighest, prob = 0.25, na.rm = TRUE),
        quantile(BAS_RespRateHighest, prob = 0.75, na.rm = TRUE)
      ),
      "Highest respiratory rate_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_RespRateHighest)),
        100 * sum(is.na(BAS_RespRateHighest) / n())
      ),
      "Highest recorded Urea (mmol/L)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_UreaResult, na.rm = TRUE),
        quantile(BAS_UreaResult, prob = 0.25, na.rm = TRUE),
        quantile(BAS_UreaResult, prob = 0.75, na.rm = TRUE)
      ),
      "Highest recorded Urea (mmol/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_UreaResult)),
        100 * sum(is.na(BAS_UreaResult) / n())
      ),
      "Highest recorded CRP (mg/L)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_CRPResult, na.rm = TRUE),
        quantile(BAS_CRPResult, prob = 0.25, na.rm = TRUE),
        quantile(BAS_CRPResult, prob = 0.75, na.rm = TRUE)
      ),
      "Highest recorded CRP (mg/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_CRPResult)),
        100 * sum(is.na(BAS_CRPResult) / n())
      ),
      "APTT_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)", median(BAS_APTT, na.rm = TRUE),
        quantile(BAS_APTT, prob = 0.25, na.rm = TRUE),
        quantile(BAS_APTT, prob = 0.75, na.rm = TRUE)
      ),
      "APTT_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_APTT)),
        100 * sum(is.na(BAS_APTT) / n())
      ),
      "INR_Mean (SD)" = sprintf("%.2f (%.2f)", mean(BAS_INR, na.rm = TRUE), sd(BAS_INR, na.rm = TRUE)),
      "INR_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_INR)),
        100 * sum(is.na(BAS_INR) / n())
      ),
      "Fibrinogen (g/L)_Mean (SD)" = sprintf("%.2f (%.2f)", mean(BAS_FibrinogenResult, na.rm = TRUE), sd(BAS_FibrinogenResult, na.rm = TRUE)),
      "Fibrinogen (g/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_FibrinogenResult)),
        100 * sum(is.na(BAS_FibrinogenResult) / n())
      ),
      "Prothrombin time (sec)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)", median(BAS_ProthrombinTime, na.rm = TRUE),
        quantile(BAS_ProthrombinTime, prob = 0.25, na.rm = TRUE),
        quantile(BAS_ProthrombinTime, prob = 0.75, na.rm = TRUE)
      ),
      "Prothrombin time (sec)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_ProthrombinTime)),
        100 * sum(is.na(BAS_ProthrombinTime) / n())
      ),
      "Taking aspirin_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_PatientTakingAspirin == "Yes", na.rm = TRUE),
        100 * sum(BAS_PatientTakingAspirin == "Yes", na.rm = TRUE) / n()
      ),
      "Taking aspirin_Missing, n (\\%)" = sprintf("%i (%.0f)", sum(is.na(BAS_PatientTakingAspirin)), 100 * sum(is.na(BAS_PatientTakingAspirin)) / n()),
      "Time from onset of symptoms to hospitalisation_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms)),
        quantile(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms), 0.25),
        quantile(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms), 0.75)
      ),
      "Time from hospitalisation to randomisation_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(as.numeric(RandDate - EL_AdmittedToHospital)),
        quantile(as.numeric(RandDate - EL_AdmittedToHospital), 0.25),
        quantile(as.numeric(RandDate - EL_AdmittedToHospital), 0.75)
      ),
      "D-dimer_Test performed, n(\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_DDimerTestPerformed == "Yes", na.rm = TRUE),
        100 * mean(BAS_DDimerTestPerformed == "Yes")
      ),
      "D-dimer_Out of range, n(\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE),
        100 * mean(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE)
      )
    ) %>%
    gather(Variable, value, -!!grpvar, factor_key = TRUE) %>%
    spread(!!grpvar, value)
  colnames(tab)[-1] <- dat %>%
    count(!!grpvar) %>%
    mutate(lab = paste0(!!grpvar, "<br>(n = ", n, ")")) %>%
    pull(lab)
  return(tab)
}


#' @title Baseline prognostics overall
#' @description
#' Generates baseline prognostics summary overall
#' @param dat The dataset
#' @return A tibble giving the summary
generate_baseline_prognostics <- function(dat) {
  tab <- dat %>%
    summarise(
      "On Room Air_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE),
        100 * sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE) / n()
      ),
      "On Room Air_Missing, n (\\%)" = sprintf("%i (%.0f)", sum(is.na(BAS_OnRoomAir24hrs)), 100 * sum(is.na(BAS_OnRoomAir24hrs)) / n()),
      "GCS < 15_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_PatientGCS == "Yes", na.rm = TRUE),
        100 * sum(BAS_PatientGCS == "Yes", na.rm = TRUE) / n()
      ),
      "GCS < 15_Missing, n (\\%)" = sprintf("%i (%.0f)", sum(is.na(BAS_PatientGCS)), 100 * sum(is.na(BAS_PatientGCS)) / n()),
      "Peripheral oxygen saturation (SpO2)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_PeripheralOxygen, na.rm = TRUE),
        quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.25),
        quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.75)
      ),
      "Peripheral oxygen saturation (SpO2)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_PeripheralOxygen)),
        100 * sum(is.na(BAS_PeripheralOxygen) / n())
      ),
      "Highest respiratory rate (breaths/minute)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_RespRateHighest, na.rm = TRUE),
        quantile(BAS_RespRateHighest, prob = 0.25, na.rm = TRUE),
        quantile(BAS_RespRateHighest, prob = 0.75, na.rm = TRUE)
      ),
      "Highest respiratory rate_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_RespRateHighest)),
        100 * sum(is.na(BAS_RespRateHighest) / n())
      ),
      "Highest recorded Urea (mmol/L)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_UreaResult, na.rm = TRUE),
        quantile(BAS_UreaResult, prob = 0.25, na.rm = TRUE),
        quantile(BAS_UreaResult, prob = 0.75, na.rm = TRUE)
      ),
      "Highest recorded Urea (mmol/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_UreaResult)),
        100 * sum(is.na(BAS_UreaResult) / n())
      ),
      "Highest recorded CRP (mg/L)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(BAS_CRPResult, na.rm = TRUE),
        quantile(BAS_CRPResult, prob = 0.25, na.rm = TRUE),
        quantile(BAS_CRPResult, prob = 0.75, na.rm = TRUE)
      ),
      "Highest recorded CRP (mg/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_CRPResult)),
        100 * sum(is.na(BAS_CRPResult) / n())
      ),
      "APTT_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)", median(BAS_APTT, na.rm = TRUE),
        quantile(BAS_APTT, prob = 0.25, na.rm = TRUE),
        quantile(BAS_APTT, prob = 0.75, na.rm = TRUE)
      ),
      "APTT_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_APTT)),
        100 * sum(is.na(BAS_APTT) / n())
      ),
      "INR_Mean (SD)" = sprintf("%.2f (%.2f)", mean(BAS_INR, na.rm = TRUE), sd(BAS_INR, na.rm = TRUE)),
      "INR_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_INR)),
        100 * sum(is.na(BAS_INR) / n())
      ),
      "Fibrinogen (g/L)_Mean (SD)" = sprintf("%.2f (%.2f)", mean(BAS_FibrinogenResult, na.rm = TRUE), sd(BAS_FibrinogenResult, na.rm = TRUE)),
      "Fibrinogen (g/L)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_FibrinogenResult)),
        100 * sum(is.na(BAS_FibrinogenResult) / n())
      ),
      "Prothrombin time (sec)_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)", median(BAS_ProthrombinTime, na.rm = TRUE),
        quantile(BAS_ProthrombinTime, prob = 0.25, na.rm = TRUE),
        quantile(BAS_ProthrombinTime, prob = 0.75, na.rm = TRUE)
      ),
      "Prothrombin time (sec)_Missing, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(is.na(BAS_ProthrombinTime)),
        100 * sum(is.na(BAS_ProthrombinTime) / n())
      ),
      "Taking aspirin_Yes, n (\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_PatientTakingAspirin == "Yes", na.rm = TRUE),
        100 * sum(BAS_PatientTakingAspirin == "Yes", na.rm = TRUE) / n()
      ),
      "Taking aspirin_Missing, n (\\%)" = sprintf("%i (%.0f)", sum(is.na(BAS_PatientTakingAspirin)), 100 * sum(is.na(BAS_PatientTakingAspirin)) / n()),
      "Time from onset of symptoms to hospitalisation_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms)),
        quantile(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms), 0.25),
        quantile(as.numeric(EL_AdmittedToHospital - EL_FirstSymptoms), 0.75)
      ),
      "Time from hospitalisation to randomisation_Median (IQR)" = sprintf(
        "%.0f (%.0f, %.0f)",
        median(as.numeric(RandDate - EL_AdmittedToHospital)),
        quantile(as.numeric(RandDate - EL_AdmittedToHospital), 0.25),
        quantile(as.numeric(RandDate - EL_AdmittedToHospital), 0.75)
      ),
      "D-dimer_Test performed, n(\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_DDimerTestPerformed == "Yes", na.rm = TRUE),
        100 * mean(BAS_DDimerTestPerformed == "Yes")
      ),
      "D-dimer_Out of range, n(\\%)" = sprintf(
        "%i (%.0f)",
        sum(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE),
        100 * mean(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE)
      )
    ) %>%
    gather(Variable, value, factor_key = TRUE)
  colnames(tab)[2] <- paste0("Overall<br>(n = ", nrow(dat), ")")
  return(tab)
}


#' @title Baseline prognostics table
#' @description
#' Generates baseline prognostics summary tables
#' @param dat The dataset
#' @param closed If TRUE, generates summary only overall, if FALSE generates by domain
#' @return A tibble giving the summary
#' @export
generate_baseline_prognostics_table <- function(dat, format = "html") {
  ovrA <- generate_baseline_prognostics(dat %>% filter(AAssignment != "A0"))
  ovrC <- generate_baseline_prognostics(dat %>% filter(CAssignment != "C0"))
  byAgrp <- generate_baseline_prognostics_by(dat %>% filter(AAssignment != "A0"), AAssignment)
  byCgrp <- generate_baseline_prognostics_by(dat %>% filter(CAssignment != "C0"), CAssignment)
  tabA <- left_join(ovrA, byAgrp, by = "Variable")[, c(1, (2 + 1:(ncol(byAgrp) - 1)), 2)]
  tabC <- left_join(ovrC, byCgrp, by = "Variable")[, c(1, (2 + 1:(ncol(byCgrp) - 1)), 2)]
  fsize <- 12
  if (format == "latex") {
    fsize <- 8
    colnames(tabA) <- linebreak(colnames(tabA), linebreaker = "<br>", align = "c")
    colnames(tabC) <- linebreak(colnames(tabC), linebreaker = "<br>", align = "c")
  }
  make_packed_rows <- function(tab) {
    tab %>%
      pack_rows("Was the patient on room air for any of the preceding 24 hours?", 1, 2) %>%
      pack_rows("Was the patient's GCS < 15?", 3, 4) %>%
      pack_rows("Peripheral oxygen saturation (SpO2) on room air (Lowest)", 5, 6) %>%
      pack_rows("Highest respiratory rate (breaths/minute)", 7, 8) %>%
      pack_rows("Highest recorded Urea in the last 24 hours (mmol/L)", 9, 10) %>%
      pack_rows("Highest recorded CRP in the last 24 hours (mg/L)", 11, 12) %>%
      pack_rows("APTT\\\\textsuperscript{1}", 13, 14, escape = F) %>%
      pack_rows("INR\\\\textsuperscript{1}", 15, 16, escape = F) %>%
      pack_rows("Fibrinogen\\\\textsuperscript{1} (g/L)", 17, 18, escape = F) %>%
      pack_rows("Prothrombin time\\\\textsuperscript{1} (sec)", 19, 20, escape = F) %>%
      pack_rows("Taking aspirin", 21, 22) %>%
      pack_rows("Time from onset of symptoms to hospitalisation", 23, 23) %>%
      pack_rows("Time from hospitalisation to randomisation", 24, 24) %>%
      pack_rows("D-dimer", 25, 26) %>%
      row_spec(0, align = "c") %>%
      footnote(number = "For APTT, INR, Fibrinogen, and Prothrombin only at least one required.")
  }
  outA <- tabA %>%
    mutate(Variable = str_replace_all(Variable, ".*_", "")) %>%
    kable(
      format = format,
      booktabs = TRUE,
      longtable = F,
      escape = F,
      linesep = "",
      caption = "Baseline prognostic variables for participants randomised into domain C.",
      align = "lrrrrr"
    ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    make_packed_rows() %>%
    add_header_above(c(" " = 1, "Antiviral" = ncol(byAgrp) - 1, " " = 1))
  outC <- tabC %>%
    mutate(Variable = str_replace_all(Variable, ".*_", "")) %>%
    kable(
      format = format,
      booktabs = TRUE,
      longtable = F,
      escape = F,
      linesep = "",
      caption = "Baseline prognostic variables for participants randomised into domain C.",
      align = "lrrrrr"
    ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    make_packed_rows() %>%
    add_header_above(c(" " = 1, "Anticoagulation" = ncol(byCgrp) - 1, " " = 1))
  return(list(A = outA, C = outC))
}


# Discharge summary - other drugs ----


#' @title Drugs used during hospital stay overall
#' @description
#' Generates overall summary of drugs used during hospital stay,
#' as recorded on discharge
#' @param dat The dataset
#' @return A tibble giving the summary
generate_discharge_drugs <- function(dat) {
  tab <- dat %>%
    filter(DIS_rec == 1) %>%
    select(
      DIS_ReceivedAntibacterialDrugs,
      DIS_NoAntiviral = DIS_ReceivedNone,
      DIS_CamostatReceived,
      DIS_FavipiravirReceived,
      # DIS_DoxycyclineReceived,
      DIS_IvermectinReceived,
      DIS_RemdesivirReceived,
      DIS_OtherAntiviral = DIS_ReceivedOther,
      DIS_NoImmunomodulatory = Dis_ImmunoNone,
      DIS_ImmunoAnakinra,
      DIS_ImmunoCorticosteroids,
      DIS_ImmunoSarilumab,
      DIS_ImmunoAzithromycin,
      DIS_ImmunoTocilizumab,
      DIS_ImmunoBaricitinib,
      DIS_ImmunoRuxolitinib,
      DIS_ImmunoTofacitinib,
      DIS_ImmunoZinc,
      DIS_OtherImmunomodulatory = DIS_IummunoOther
    ) %>%
    summarise_all(., list(Variable = ~ sprintf("%i (%.0f)", sum(.x == "Yes", na.rm = TRUE), 100 * sum(.x == "Yes", na.rm = TRUE) / n()))) %>%
    pivot_longer(everything()) %>%
    mutate(
      name = str_replace(name, "DIS_Received", ""),
      name = str_replace(name, "DIS_Immuno", ""),
      name = str_replace(name, "DIS_Iummuno", ""),
      name = str_replace(name, "Dis_Immuno", ""),
      name = str_replace(name, "DIS_", ""),
      name = str_replace(name, "Received_Variable", ""),
      name = str_replace(name, "_Variable", ""),
      name = fct_inorder(gsub("([[:upper:]]*)([[:upper:]][[:lower:]]+)", "\\1 \\2", name)),
      name = str_to_sentence(trimws(name)),
      name = paste0(name, ", n (\\%)")
    )
  colnames(tab) <- c("Drug received", paste0("Overall<br>(n = ", nrow(dat %>% filter(DIS_rec == 1)), ")"))
  return(tab)
}


#' @title Drugs used during hospital stay by group
#' @description
#' Generates summary of drugs used during hospital stay,
#' as recorded on discharge, by grouping variable
#' @param dat The dataset
#' @param grpvar The grouping variable
#' @return A tibble giving the summary
generate_discharge_drugs_by <- function(dat, grpvar = NULL) {
  grpvar <- enquo(grpvar)
  tab <- dat %>%
    filter(DIS_rec == 1) %>%
    group_by(!!grpvar) %>%
    select(
      !!grpvar,
      DIS_ReceivedAntibacterialDrugs,
      DIS_NoAntiviral = DIS_ReceivedNone,
      DIS_CamostatReceived,
      DIS_FavipiravirReceived,
      # DIS_DoxycyclineReceived,
      DIS_IvermectinReceived,
      DIS_RemdesivirReceived,
      DIS_OtherAntiviral = DIS_ReceivedOther,
      DIS_NoImmunomodulatory = Dis_ImmunoNone,
      DIS_ImmunoAnakinra,
      DIS_ImmunoCorticosteroids,
      DIS_ImmunoSarilumab,
      DIS_ImmunoAzithromycin,
      DIS_ImmunoTocilizumab,
      DIS_ImmunoBaricitinib,
      DIS_ImmunoRuxolitinib,
      DIS_ImmunoTofacitinib,
      DIS_ImmunoZinc,
      DIS_OtherImmunomodulatory = DIS_IummunoOther
    ) %>%
    summarise_all(., list(Variable = ~ sprintf("%i (%.0f)", sum(.x == "Yes", na.rm = TRUE), 100 * sum(.x == "Yes", na.rm = TRUE) / n()))) %>%
    gather(name, value, -!!grpvar, factor_key = TRUE) %>%
    spread(!!grpvar, value) %>%
    mutate(
      name = str_replace(name, "DIS_Received", ""),
      name = str_replace(name, "DIS_Immuno", ""),
      name = str_replace(name, "DIS_Iummuno", ""),
      name = str_replace(name, "Dis_Immuno", ""),
      name = str_replace(name, "DIS_", ""),
      name = str_replace(name, "Received_Variable", ""),
      name = str_replace(name, "_Variable", ""),
      name = fct_inorder(gsub("([[:upper:]]*)([[:upper:]][[:lower:]]+)", "\\1 \\2", name)),
      name = str_to_sentence(trimws(name)),
      name = paste0(name, ", n (\\%)")
    )
  colnames(tab) <- c("Drug received", dat %>% filter(DIS_rec == 1) %>% count(!!grpvar) %>% mutate(lab = paste0(!!grpvar, "<br>(n = ", n, ")")) %>% pull(lab))
  return(tab)
}


#' @title Drugs used during hospital stay table
#' @description
#' Generates summary table of drugs used during hospital stay,
#' as recorded on discharge, by domain or overall.
#' @param dat The dataset
#' @param closed If TRUE, generate overall table only, if FALSE one table per domain
#' @return A tibble giving the summary
#' @export
generate_discharge_drugs_table <- function(dat, format = "html") {
  ovrA <- generate_discharge_drugs(dat %>% filter(AAssignment != "A0"))
  ovrC <- generate_discharge_drugs(dat %>% filter(CAssignment != "C0"))
  bygrpA <- generate_discharge_drugs_by(dat %>% filter(AAssignment != "A0"), AAssignment)
  bygrpC <- generate_discharge_drugs_by(dat %>% filter(CAssignment != "C0"), CAssignment)
  tabA <- left_join(ovrA, bygrpA, by = "Drug received")[, c(1, (2 + 1:(ncol(bygrpA) - 1)), 2)]
  tabC <- left_join(ovrC, bygrpC, by = "Drug received")[, c(1, (2 + 1:(ncol(bygrpC) - 1)), 2)]
  fsize <- 12
  if (format == "latex") {
    fsize <- 9
    colnames(tabA) <- linebreak(colnames(tabA), linebreaker = "<br>", align = "c")
    colnames(tabC) <- linebreak(colnames(tabC), linebreaker = "<br>", align = "c")
  }
  outA <- kable(
    tabA,
    format = format,
    booktabs = T,
    caption = "Drugs received during hospital stay, antiviral domain.",
    align = "lrrrrrrrr",
    escape = F
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    pack_rows("Antivirals", 2, 7) %>%
    pack_rows("Immunomodulatory", 8, 18) %>%
    row_spec(0, align = "c") %>%
    add_header_above(c(" " = 1, "Antiviral" = ncol(bygrpA) - 1, " " = 1))
  outC <- kable(
    tabC,
    format = format,
    booktabs = T,
    caption = "Drugs received during hospital stay, anticoagulation domain.",
    align = "lrrrrrrrr",
    escape = F
  ) %>%
    kable_styling(
      bootstrap_options = "striped",
      font_size = fsize,
      latex_options = "HOLD_position"
    ) %>%
    pack_rows("Antivirals", 2, 7) %>%
    pack_rows("Immunomodulatory", 8, 18) %>%
    row_spec(0, align = "c") %>%
    add_header_above(c(" " = 1, "Anticoagulation" = ncol(bygrpC) - 1, " " = 1))
  return(list(A = outA, C = outC))
}


#' @title Interevention assignment table
#' @param dat Dataset with `PO`, `AAssignment`, and `CAssignment`.
#' @return Tibble of intervention assignment summary
#' @export
make_intervention_table <- function(dat) {
  sdat <- dat |>
    transmute(
      PO,
      WTH_FU,
      Antiviral = factor(AAssignment, levels = c("A0", "A1", "A2"), labels = intervention_labels_short()$AAssignment),
      Anticoagulation = factor(CAssignment, levels = c("C0", "C1", "C2", "C3", "C4"), labels = intervention_labels_short()$CAssignment)
    )
  Adat <- sdat |>
    group_by(Antiviral, Anticoagulation = "Total") |>
    summarise(n = sprintf("%i (%i)", n(), sum(is.na(PO) | WTH_FU == 1)), .groups = "drop") |>
    bind_rows(
      sdat |>
        summarise(Antiviral = "Total", Anticoagulation = "Total", n = sprintf("%i (%i)", n(), sum(is.na(PO) | WTH_FU == 1)), .groups = "drop")
    ) |>
    spread(Antiviral, n)
  Cdat <- sdat |>
    group_by(Anticoagulation) |>
    summarise(Total = sprintf("%i (%i)", n(), sum(is.na(PO) | WTH_FU == 1)), .groups = "drop")
  ACdat <- sdat |>
    group_by(Antiviral, Anticoagulation) |>
    summarise(
      n = sprintf("%i (%i)", n(), sum(is.na(PO) | WTH_FU == 1)), .groups = "drop"
    ) |>
    spread(Antiviral, n, fill = "0 (0)")
  ACdat <- ACdat |>
    left_join(Cdat, by = "Anticoagulation") |>
    bind_rows(Adat)
  return(ACdat)
}

# Table 1 ----

#' @title generate_table_1_data
#' @param dat A dataset
generate_table_1_data <- function(dat, grp = NULL) {
  f1 <- function(.data, grp) {
    if (is.null(grp)) {
      .data
    } else {
      group_by(.data, AAssignment = factor(AAssignment, levels = c("A0", "A1", "A2"), labels = intervention_labels2()$AAssignment))
    }
  }
  f2 <- function(data, grp) {
    if (is.null(grp)) {
      val <-
      gather(data, Variable, "All participants") |>
        rename_with( ~ paste0(.x, "\n(n = ", nrow(dat), ")"), `All participants`)
    } else {
      ns <- dat |> count(AAssignment) |> pull(n)
      gather(data, Variable, value, -AAssignment, factor_key = TRUE) |>
        spread(AAssignment, value) |>
        rename_with( ~ paste0(.x, "\n(n = ", ns, ")"), 2:3)
    }
  }
  dat |>
    f1(grp) |>
    summarise(
      Assigned = n(),
      `Age -- yr` = sprintf("%.0f (%.0f, %.0f)", median(AgeAtEntry), quantile(AgeAtEntry, 0.25), quantile(AgeAtEntry, 0.75)),
      `Male sex` = sprintf("%i (%.0f)", sum(Sex == "Male", na.rm = TRUE), 100 * sum(Sex == "Male", na.rm = TRUE) / n()),
      `Country_Australia` = sprintf("%i (%.0f)", sum(Country == "AU", na.rm = TRUE), 100 * sum(Country == "AU", na.rm = TRUE) / n()),
      `Country_New Zealand` = sprintf("%i (%.0f)", sum(Country == "NZ", na.rm = TRUE), 100 * sum(Country == "NZ", na.rm = TRUE) / n()),
      `Country_Nepal` = sprintf("%i (%.0f)", sum(Country == "NP", na.rm = TRUE), 100 * sum(Country == "NP", na.rm = TRUE) / n()),
      `Ethnicity_European` = sprintf("%i (%.0f)", sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityEuropean == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Pacific peoples or Maori` = sprintf("%i (%.0f)", sum(BAS_EthnicityPacificIslander == "Yes" | BAS_EthnicityMaori == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityPacificIslander == "Yes" | BAS_EthnicityMaori == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Middle Eastern` = sprintf("%i (%.0f)", sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityMiddleEastern == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Asian` = sprintf("%i (%.0f)", sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAsian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Aboriginal` = sprintf("%i (%.0f)", sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAboriginal == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Indian` = sprintf("%i (%.0f)", sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityIndian == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_African` = sprintf("%i (%.0f)", sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityAfrican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Latin American` = sprintf("%i (%.0f)", sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE), 100 * sum(BAS_EthnicityLatinAmerican == "Yes", na.rm = TRUE) / n()),
      `Ethnicity_Other/unknown` = sprintf("%i (%.0f)", sum(BAS_EthnicityOther == "Yes" | BAS_EthnicityUnknown == "Yes"), 100 * sum(BAS_EthnicityOther == "Yes" | BAS_EthnicityUnknown == "Yes") / n()),
      `Weight -- kg` = sprintf("%.0f (%.0f, %.0f)", median(BAS_Weight), quantile(BAS_Weight, 0.25), quantile(BAS_Weight, 0.75)),
      `Vaccinated -- no./total no.` = sprintf("%i/%i (%.0f)", sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE), sum(!is.na(BAS_PatientVaccinated)), 100 * sum(BAS_PatientVaccinated == "Yes", na.rm = TRUE) / sum(!is.na(BAS_PatientVaccinated))),
      `Comorbidities_Obesity` = sprintf("%i (%.0f)", sum(BAS_Obesity == "Yes"), 100 * sum(BAS_Obesity == "Yes") / n()),
      `Comorbidities_Hypertension` = sprintf("%i (%.0f)", sum(BAS_Hypertension == "Yes"), 100 * sum(BAS_Hypertension == "Yes") / n()),
      `Comorbidities_Diabetes` = sprintf("%i (%.0f)", sum(BAS_Diabetes == "Yes"), 100 * sum(BAS_Diabetes == "Yes") / n()),
      `Comorbidities_Asthma` = sprintf("%i (%.0f)", sum(BAS_Asthma == "Yes"), 100 * sum(BAS_Asthma == "Yes") / n()),
      `Comorbidities_Chronic lung disease` = sprintf("%i (%.0f)", sum(BAS_ChronicLungDisease == "Yes"), 100 * sum(BAS_ChronicLungDisease == "Yes") / n()),
      `Comorbidities_Chronic cardiac disease` = sprintf("%i (%.0f)", sum(BAS_ChonicCardiacDisease == "Yes"), 100 * sum(BAS_ChonicCardiacDisease == "Yes") / n()),
      `Onset of symptoms to hospitalisation` = sprintf("%.0f (%.0f, %.0f)", median(dsfs), quantile(dsfs, 0.25), quantile(dsfs, 0.75)),
      `Hospitalisation to randomisation` = sprintf("%.0f (%.0f, %.0f)", median(dhtr), quantile(dhtr, 0.25), quantile(dhtr, 0.75)),
      `Any time breathing ambient air in past 24 hours` = sprintf("%i (%.0f)", sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE), 100 * sum(BAS_OnRoomAir24hrs == "Yes", na.rm = TRUE) / n()),
      `Lowest SpO2 while breathing ambient air` = sprintf("%.0f (%.0f, %.0f)", median(BAS_PeripheralOxygen, na.rm = TRUE), quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.25), quantile(BAS_PeripheralOxygen, na.rm = TRUE, prob = 0.75)),
      `Highest respiratory rate in past 24 hours` = sprintf("%.0f (%.0f, %.0f)", median(BAS_RespRateHighest, na.rm = TRUE), quantile(BAS_RespRateHighest, prob = 0.25, na.rm = TRUE), quantile(BAS_RespRateHighest, prob = 0.75, na.rm = TRUE)),
      `Glasgow coma score < 15` = sprintf("%i (%.0f)", sum(BAS_PatientGCS == "Yes", na.rm = TRUE), 100 * sum(BAS_PatientGCS == "Yes", na.rm = TRUE) / n()),
      `Lab_CRP -- mg/L` = str_median_iqr(BAS_CRPResult_fixed, na.rm = TRUE),
      `Lab_CRP Patients evaluated` = sprintf("%i (%.0f)", sum(!is.na(BAS_CRPResult_fixed)), 100 * mean(!is.na(BAS_CRPResult_fixed))),
      `Lab_D-dimer > upper limit normal` = sprintf("%i (%.0f)", sum(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE), 100 * mean(BAS_DDimerOutOfRange == "Yes", na.rm = TRUE)),
      `Lab_D-dimer Patients evaluated` = sprintf("%i (%.0f)", sum(!is.na(BAS_DDimerOutOfRange)), 100 * mean(!is.na(BAS_DDimerOutOfRange))),
      `Lab_APTT -- s` = str_median_iqr(BAS_APTT, na.rm = TRUE),
      `Lab_APTT Patients evaluated` = sprintf("%i (%.0f)", sum(!is.na(BAS_APTT)), 100 * mean(!is.na(BAS_APTT))),
      `Lab_Internation normlised ratio (SD)` = str_mean_sd(BAS_INR, na.rm = TRUE),
      `Lab_INR Patients evaluated` = sprintf("%i (%.0f)", sum(!is.na(BAS_INR)), 100 * mean(!is.na(BAS_INR))),
    ) |>
    f2(grp)
}


#' @title generate_table_1
#' @param dat A dataset
#' @export
generate_table_1 <- function(dat, format = "html") {
  tab <- left_join(generate_table_1_data(dat, sym("AAssignment")), generate_table_1_data(dat)) |>
    mutate(Variable = str_replace(Variable, "[A-z]*_", ""))
  fsize <- ifelse(format == "html", 12, 7)
  if(format == "latex") {
    colnames(tab) <- linebreak(colnames(tab), align = "c")
  }
  out <- tab[-1, ] |>
    kable(
      format = format,
      align = "lrrr",
      escape = F,
      booktabs = TRUE
    ) |>
    kable_styling(latex_options = "HOLD_position", font_size = fsize) |>
    pack_rows("Country", 3, 5) |>
    pack_rows("Ethnicity", 6, 14) |>
    pack_rows("Co-morbidities", 17, 22) |>
    pack_rows("No. of days", 23, 24) |>
    pack_rows("Lab values", 29, 36)
  return(out)
}
