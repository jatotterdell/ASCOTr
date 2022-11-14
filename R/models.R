#' @title compile_cmdstanr_mod
#' Compile provided cumulative logistic Stan model for cmdstanr.
#' @param mod_name The model name
#' @return A `CmdStanModel` object
#' @importFrom cmdstanr cmdstan_model
compile_cmdstanr_mod <- function(mod_name, ...) {
  cmdstanr::cmdstan_model(stan_file = system.file("stan", paste0(mod_name, ".stan"), package = "ASCOTr"), ... = ...)
}



#' @title make_domA_design
#' @description
#' Create design matrix where domain has an intercept which
#' indicates whether a participant was randomised to it or not
#' and (by default) uses orthonormal contrasts for available
#' interventions.
#'
#' This keeps the "not randomised" group out of the overall
#' mean around which interventions are centred.
#'
#' @param dat A dataset with factor variable "randA" for which NA means not randomised.
#' @param ctr Contrast, by defualt orthonormal
#' @return Design matrix
#' @export
make_domA_design <- function(dat, ctr = contr.equalprior) {
  XA <- model.matrix(
    ~ randA,
    model.frame(~ randA, dat, na.action = na.pass),
    contrasts = list(randA = ctr)
  )
  # If not randomised to A
  XA[is.na(XA[, 2]), ] <- 0
  colnames(XA)[1] <- "randA"
  return(XA)
}


#' @title make_domC_design
#' @description
#' Create design matrix where domain has an intercept which
#' indicates whether a participant was randomised to it or not
#' and (by default) uses orthonormal contrasts for available
#' interventions.
#'
#' This keeps the "not randomised" group out of the overall
#' mean around which interventions are centred.
#'
#' @param dat A dataset with factor variable "randC" for which NA means not randomised.
#' @param ctr Contrast, by defualt orthonormal
#' @return Design matrix
#' @export
make_domC_design <- function(dat, ctr = contr.equalprior) {
  XC <- model.matrix(
    ~ randC,
    model.frame(~ randC, dat, na.action = na.pass),
    contrasts = list(randC = ctr)
  )
  # If not randomised to A
  XC[is.na(XC[, 2]), ] <- 0
  colnames(XC)[1] <- "randC"
  return(XC)
}
