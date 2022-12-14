# Misc ----

#' @title quantile_cuts
#' @description
#' Convert a numeric vector into a categorical according to quantiles.
#' @param x Numeric vector
#' @param ss Increasing sequence of probability cut-points
#' @return A factor giving the quantiles according to `ss`
#' @export
quantile_cuts <- function(x, ss) {
  fct_explicit_na(cut(
    x,
    breaks = c(quantile(x, probs = ss, na.rm = TRUE)),
    include.lowest = TRUE
  ))
}

#' @title logit
#' @description
#' logit transform
#' @param x Real input
#' @return logit(x)
#' @export
logit <- function(x) {
  log(x) - log(1 - x)
}


#' @title expit
#' @description
#' expit transform
#' @param x Real input
#' @return expit(x)
#' @export
expit <- function(x) {
  1 / (1 + exp(-x))
}


#' @title ordered_logit
#' @description
#' ordered logit transform
#' @param x Real vector input
#' @return ordered_logit(x)
#' @export
ordered_logit <- function(x) {
  c(
    1 - expit(-x[1]),
    expit(-x[1:(length(x) - 1)]) - expit(-x[2:(length(x))]),
    expit(-tail(x, 1))
  )
}


#' @title findfirst
#'
#' @param x Logical vector
#' @param v Default if no TRUE elements
#' @return First TRUE element of x
findfirst <- function(x, v = NA) {
  j <- which(x)
  if (length(j)) min(j) else v
}


#' @title creatinine_clearance
#' @description
#' Serum creatinine clearance by Cockcroft-Gault formula
#' https://www.eviq.org.au/clinical-resources/eviq-calculators/3200-creatinine-clearance-calculator
#' @param sex Either "Male" or "Female"
#' @param age Age in years
#' @param weight Weight in kg
#' @param creatinine Serum creatinine in umol/L
#' @return Creatinine clearance rate in mL/min.
creatinine_clearance <- function(sex, age, weight, creatinine) {
  out <- (140 - age) * weight / (0.814 * 72 * creatinine)
  out[sex == "Female"] <- 0.85 * out[sex == "Female"]
  return(out)
}



# Save outputs ----


#' @title save_tex_table
#' @description
#' Save a latex table
#' @param tab The latex table
#' @param fn The file name (without .tex extension)
#' @return Nothing, but writes the table to outputs/tables/<fn>.tex
#' @export
save_tex_table <- function(tab, fn) {
  writeLines(tab, con = file.path("outputs", "tables", paste0(fn, ".tex")))
}


#' @title save_cmdstanr_model
#' @description
#' Save cmdstanr model
#' @param mod cmdstanr model object
#' @param fn File name
#' @return Nothing, but saves the model object to outputs/models/<fn>
#' @export
save_cmdstanr_model <- function(mod, fn) {
  sf <- paste0(fn, ".rds")
  mod$save_object(file.path("outputs", "models", sf))
}


#' @title save_derived_dataset
#' @description
#' Save a derived dataset to the appropriate directory (ANTICOAG_DATA)
#'
#' @param dir The derived data directory
#' @param dat The dataset
#' @param fn The file name which will be appended to `dir`.
#' @return Nothing
#' @export
save_derived_dataset <- function(dir, dat, fn) {
  path <- file.path(dir, fn)
  saveRDS(dat, path)
}
