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
    ~randA,
    model.frame(~randA, dat, na.action = na.pass),
    contrasts = list(randA = ctr)
  )
  # If not randomised to A
  XA[is.na(XA[, 2]), ] <- 0
  colnames(XA)[1] <- "randA"
  if (all(XA[, "randA"] == 1)) {
    cX <- attr(XA, "contrasts")$randA
    XA <- XA[, -1, drop = FALSE]
    attributes(XA)$contrasts <- list("randA" = cX)
  }
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
    ~randC,
    model.frame(~randC, dat, na.action = na.pass),
    contrasts = list(randC = ctr)
  )
  # If not randomised to C
  XC[, 1] <- 0
  XC[is.na(XC[, 2]), ] <- 1
  colnames(XC)[1] <- "randC"
  if (all(XC[, "randC"] == 0)) {
    cX <- attr(XC, "contrasts")$randC
    XC <- XC[, -1, drop = FALSE]
    attributes(XC)$contrasts <- list("randC" = cX)
  }
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
make_X_design <- function(dat,
                          vars = NULL,
                          ctr = contr.equalprior,
                          includeA = TRUE,
                          includeC = TRUE,
                          intercept = TRUE) {
  if (is.null(vars) & !(includeA | includeC | intercept)) error("No variables included.")
  if (intercept) {
    X <- cbind(intercept = rep(1, nrow(dat)))
  } else {
    X <- NULL
  }
  if (includeA) {
    XA <- make_domA_design(dat, ctr)
  } else {
    XA <- NULL
  }
  if (includeC) {
    XC <- make_domC_design(dat, ctr)
  } else {
    XC <- NULL
  }
  if (!is.null(vars)) {
    Xother <- model.matrix(
      as.formula(paste(" ~ ", paste(vars, collapse = " + "))),
      data = dat
    )[, -1, drop = FALSE]
  } else {
    Xother <- NULL
  }
  X <- cbind(X, XA, XC, Xother)
  if (includeA) {
    attributes(X)$contrasts <- list("randA" = attr(XA, "contrasts")$randA)
  }
  if (includeC) {
    attributes(X)$contrasts <- c(attributes(X)$contrasts, list("randC" = attr(XC, "contrasts")$randC))
  }
  # attributes(X)$contrasts <- list(
  #   "randA" = attr(XA, "contrasts")$randA,
  #   "randC" = attr(XC, "contrasts")$randC
  # )
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
make_primary_model_data <- function(dat,
                                    vars = c("inelgc3", "agegte60", "ctry"),
                                    beta_sd_int = 2.5,
                                    beta_sd_var = c(10, 2.5, 1, 1),
                                    beta_sd_trt = 1,
                                    ctr = contr.equalprior,
                                    ...) {
  X <- make_X_design(dat, vars = vars, ctr = ctr, ...)
  nXtrt <- sum(grepl("rand", colnames(X)))
  epoch <- dat[["epoch"]]
  M_epoch <- max(epoch)
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


#' @title Fit primary model
#' @description
#' Creates Stan dataset, samples from the model, and generates summaries
#' @param dat Dataset (e.g. output from `make_fas_itt_set`)
#' @param model Stan model
#' @param vars Covariates to include
#' @param beta_sd_int Prior SD for intercept
#' @param beta_sd_var Prior SD for vars
#' @param beta_sd_trt Prior SD for treatments
#' @param ctr Contrast for treatment design
#' @param seed Seed
#' @param ... Other arguments
#' @export
fit_primary_model <- function(dat = NULL,
                              model = NULL,
                              vars = c("inelgc3", "agegte60", "ctry"),
                              beta_sd_int = 2.5,
                              beta_sd_var = c(10, 2.5, 1, 1),
                              beta_sd_trt = 1,
                              ctr = contr.equalprior,
                              seed = 32915,
                              ...) {
  mdat <- make_primary_model_data(
    dat,
    vars = vars,
    beta_sd_int = beta_sd_int,
    beta_sd_var = beta_sd_var,
    beta_sd_trt = beta_sd_trt,
    ctr = ctr,
    ...
  )
  snk <- capture.output(
    mfit <- model[["sample"]](
      data = mdat,
      seed = seed,
      adapt_delta = 0.95,
      refresh = 0,
      iter_warmup = 1000,
      iter_sampling = 2500,
      chains = 8,
      parallel_chains = min(8, parallel::detectCores())
    )
  )
  mpars <- mfit$metadata()$model_params
  keep <- mpars[!grepl("(_raw|epsilon_)", mpars)]
  mdrws <- as_draws_rvars(mfit$draws(keep))
  names(mdrws$beta) <- colnames(mdat$X)
  if (any(grepl("site", keep))) {
    site_map <- dat %>% dplyr::count(site_num, site)
    names(mdrws$gamma_site) <- site_map$site
  }
  if (any(grepl("epoch", keep))) {
    epoch_map <- dat %>% dplyr::count(epoch, epoch_lab)
    names(mdrws$gamma_epoch) <- epoch_map$epoch_lab
  }
  # Transformed samples
  Ca <- mdat$ctrA
  Cc <- mdat$ctrC
  # Get constrained parameters from contrast transformation
  mdrws$Acon <- as.vector(Ca %**% mdrws$beta[grepl("randA[0-9]", names(mdrws$beta))])
  mdrws$Ccon <- as.vector(Cc %**% mdrws$beta[grepl("randC[0-9]", names(mdrws$beta))])
  names(mdrws$Acon) <- rownames(Ca)
  names(mdrws$Ccon) <- rownames(Cc)
  mdrws$Atrt <- mdrws$Acon[-1] - mdrws$Acon[1]
  mdrws$Ctrt <- mdrws$Ccon[-1] - mdrws$Ccon[1]
  mdrws$AOR <- exp(mdrws$Atrt)
  mdrws$COR <- exp(mdrws$Ctrt)
  mdrws$OR <- exp(mdrws$beta[!grepl("(Intercept|rand)", names(mdrws$beta))])
  return(list(dat = mdat, fit = mfit, drws = mdrws))
}


#' @title Plot epoch term posterior summaries
#' @param  rvs_epoch Epoch RVs
#' @return A ggplot
#' @export
plot_epoch_terms <- function(rvs_epoch) {
  orsdat <- tibble(
    Group = "Epoch",
    Parameter = fct_inorder(names(rvs_epoch)),
    posterior = rvs_epoch
  )
  p_epoch <- ggplot(orsdat, aes(xdist = posterior, y = Parameter)) +
    stat_pointinterval(.width = c(0.75, 0.95), fatten_point = 1.5) +
    geom_vline(xintercept = 1, linetype = 2) +
    scale_x_log10("Odds ratio (log scale)") +
    labs(y = "Epoch")
  return(p_epoch)
}


#' @title Plot site term posterior summaries
#' @param rvs_site Site RVs
#' @param region Site region groupings
#' @return A ggplot
#' @export
plot_site_terms <- function(rvs_site, region) {
  orsdat <- tibble(
    Group = "Site",
    Country = region,
    Parameter = fct_inorder(names(rvs_site)),
    posterior = rvs_site
  )
  p_site <- ggplot(orsdat, aes(xdist = posterior, y = Parameter)) +
    facet_grid(Country ~ ., scales = "free_y", space = "free_y") +
    stat_pointinterval(.width = c(0.75, 0.95), fatten_point = 1.5) +
    geom_vline(xintercept = 1, linetype = 2) +
    scale_x_log10("Odds ratio (log scale)") +
    labs(y = "Site") +
    theme(panel.border = element_rect(fill = NA))
  return(p_site)
}


#' @title Plot site and epoch terms posterior summaries in one figure
#' @param rvs_epoch Epoch RVs
#' @param rvs_site Site RVs
#' @param region Site region groupings
#' @return A ggplot
#' @export
plot_epoch_site_terms <- function(rvs_epoch, rvs_site, region) {
  p_epoch <- plot_epoch_terms(rvs_epoch)
  p_site <- plot_site_terms(rvs_site, region)
  p <- p_epoch | p_site
  p
}
