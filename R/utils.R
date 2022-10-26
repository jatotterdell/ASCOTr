# Misc ----

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
    expit(-x[1:(length(x)-1)]) - expit(-x[2:(length(x))]),
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
