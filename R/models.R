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
#' @importFrom bayestestR contr.equalprior
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


#' @title Make treatment design matrix
#' @description
#' Setup the design matrix for interventions using contrast `ctr`.
#' @param dat A dataset with factor variables `randA` and `randC`
#' @param ctr Contrast, by default equal marginals
#' @return Design matrix
#' @export
make_X_trt_design <- function(dat, ctr = contr.equalprior) {
  XA <- make_domA_design(dat, ctr)
  XC <- make_domC_design(dat, ctr)
  X <- cbind(XA, XC)
  attributes(X)$contrasts <- list(
    "randA" = attr(XA, "contrasts")$randA,
    "randC" = attr(XC, "contrasts")$randC
  )
  return(X)
}


#' @title Make design matrix
#' @description
#' Setup the design matrix for treatments and other covariates as required
#' @param dat A dataset
#' @param vars String vector of other columns to include
#' @param includeA Include domain A design
#' @param includeC Include domain C design
#' @param intercept Include an intercept term
#' @return A design matrix
#' @export
make_X_design <- function(
    dat,
    vars = NULL,
    ctr = contr.equalprior,
    includeA = TRUE,
    includeC = TRUE,
    intercept = TRUE) {

  if (is.null(vars) & !(includeA | includeC | intercept)) error("No variables included.")
  if(intercept) {
    X <- cbind(intercept = rep(1, nrow(dat)))
  } else {
    X <- NULL
  }
  if(includeA) {
    XA <- make_domA_design(dat, ctr)
  } else {
    XA <- NULL
  }
  if(includeC) {
    XC <- make_domC_design(dat, ctr)
  } else {
    XC <- NULL
  }
  if (!is.null(vars)) {
    Xother <- model.matrix(
      as.formula(paste(" ~ ", paste(vars, collapse = " + "))),
      data = dat
    ) [, -1, drop = FALSE]
  } else {
    Xother <- NULL
  }
  X <- cbind(X, XA, XC, Xother)
  attributes(X)$contrasts <- list(
    "randA" = attr(XA, "contrasts")$randA,
    "randC" = attr(XC, "contrasts")$randC
  )
  return(X)
}


#' @title Make primary outcome stan data
#' @description
#' Create a dataset for use with Stan
#' @param dat Dataset
#' @param vars Variables to include in model
#' @param beta_sd_int Prior SD on intercept
#' @param beta_sd_var Prior SD on vars
#' @param beta_sd_trt Prior SD on treatment terms
#' @param ctr Contrast for treatments
#' @param .. Other args
#' @return A list of data for use with Stan model
#' @export
make_primary_model_data <- function(
    dat,
    vars = c("inelgc3", "agegte60", "ctry"),
    beta_sd_int = 2.5,
    beta_sd_var = c(10, 2.5, 1, 1),
    beta_sd_trt = 1,
    ctr = contr.equalprior,
    ...) {

  X <- make_X_design(dat, vars = vars, ctr = ctr, ...)
  nXtrt <- sum(grepl("rand", colnames(X))) - any(grepl("intercept", colnames(X)))
  epoch  <- dat[["epoch"]]
  M_epoch  <- max(epoch)
  region <- dat[["ctry_num"]]
  M_region <- max(region)
  site <- dat[["site_num"]]
  M_site <- max(site)
  region_by_site <- region_by_site <- dat |>
    dplyr::count(ctry_num, site_num) |>
    pull(ctry_num)
  y <- dat[["PO"]]
  N <- dim(X)[1]
  K <- dim(X)[2]
  beta_sd <- c(beta_sd_int, rep(beta_sd_trt, nXtrt), beta_sd_var)
  out <- list(
    N = N, K = K, X = X, y = y,
    M_region = M_region, region = region,
    M_site = M_site, site = site,
    M_epoch = M_epoch, epoch = epoch,
    region_by_site = region_by_site,
    beta_sd = beta_sd,
    ctrA = attr(X, "contrasts")[["randA"]],
    ctrC = attr(X, "contrasts")[["randC"]]
  )
  return(out)
}
