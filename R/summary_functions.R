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
