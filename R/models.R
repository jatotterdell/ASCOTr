#' @title compile_cmdstanr_mod
#' Compile provided cumulative logistic Stan model for cmdstanr.
#' @param mod_name The model name
#' @return A `CmdStanModel` object
#' @export
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
  flg <- is.na(XC[, 2])
  XC[flg, -1] <- 0
  XC[flg, 1] <- 1

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
                              adapt_delta = 0.99,
                              iter_sampling = 2500,
                              chains = 8,
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
      adapt_delta = adapt_delta,
      refresh = 0,
      iter_warmup = 1000,
      iter_sampling = iter_sampling,
      chains = chains,
      parallel_chains = min(chains, parallel::detectCores())
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
plot_epoch_terms <- function(rvs_epoch, xlab = "Change in log-odds") {
  orsdat <- tibble(
    Group = "Epoch",
    Parameter = fct_inorder(names(rvs_epoch)),
    posterior = rvs_epoch
  )
  p_epoch <- ggplot(orsdat, aes(xdist = posterior, y = Parameter)) +
    stat_pointinterval(.width = c(0.75, 0.95), fatten_point = 1.5) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(y = "Epoch", x = xlab)
  return(p_epoch)
}


#' @title Plot site term posterior summaries
#' @param rvs_site Site RVs
#' @param region Site region groupings
#' @return A ggplot
#' @export
plot_site_terms <- function(rvs_site, region, xlab = "Change in log-odds") {
  orsdat <- tibble(
    Group = "Site",
    Country = region,
    Parameter = fct_inorder(names(rvs_site)),
    posterior = rvs_site
  )
  p_site <- ggplot(orsdat, aes(xdist = posterior, y = Parameter)) +
    facet_grid(Country ~ ., scales = "free_y", space = "free_y") +
    stat_pointinterval(.width = c(0.75, 0.95), fatten_point = 1.5) +
    geom_vline(xintercept = 0, linetype = 2) +
    labs(y = "Site", x = xlab) +
    theme(panel.border = element_rect(fill = NA))
  return(p_site)
}


#' @title Plot site and epoch terms posterior summaries in one figure
#' @param rvs_epoch Epoch RVs
#' @param rvs_site Site RVs
#' @param region Site region groupings
#' @return A ggplot
#' @export
plot_epoch_site_terms <- function(rvs_epoch, rvs_site, region, xlab = "Change in log-odds") {
  p_epoch <- plot_epoch_terms(rvs_epoch, xlab)
  p_site <- plot_site_terms(rvs_site, region, xlab)
  p <- p_epoch | p_site
  p
}


#' @title Plot OR densities
#' @param rvs RV of ORs
#' @return A ggplot2
#' @export
plot_or_densities <- function(rvs) {
  tibble(Contrast = fct_inorder(names(rvs)), RV = rvs) %>%
    ggplot(., aes(y = Contrast, xdist = RV)) +
    stat_halfeye(
      aes(fill =
            after_stat(cut_cdf_qi(
              cdf,
              .width = c(.5, .8, .95, 0.99),
              labels = scales::percent_format()))),
      adjust = 1, n = 1001, .width = c(0.5, 0.8, 0.95)
    ) +
    scale_fill_brewer(
      palette = "Reds",
      direction = -1,
      na.translate = FALSE) +
    labs(
      x = "Odds ratio contrast",
      fill = "Interval"
    ) +
    scale_x_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 4, 10)) +
    geom_vline(xintercept = 1)
}


#' @title Create odds ratio summary table
#' @param OR Odds ratio RV
#' @param format Table format
#' @param fn Filename
#' @return Table if fn is NULL, otherwise save the table
#' @export
odds_ratio_summary_table <- function(OR, format = "html", fn = NULL) {
  out <- tibble(
    Parameter = names(OR),
    Median = median(OR),
    `95% CrI` = apply(
      quantile(OR, c(0.025, 0.975)), 2,
      function(x) sprintf("(%.2f, %.2f)", x[1], x[2])),
    `Mean (SD)` = sprintf("%.2f (%.2f)", E(OR), sd(OR)),
    `Pr(OR < 1)` = Pr(OR < 1),
  ) %>%
    kable(
      format = format,
      digits = 2,
      align = "lrrrr",
      linesep = "",
      booktabs = TRUE) %>%
    kable_styling(
      font_size = 9,
      bootstrap_options = "striped",
      latex_options = "HOLD_position")
  if (!is.null(fn) & format == "latex") {
    save_tex_table(out, fn)
  } else {
    return(out)
  }
}


#' @title Summarise a posterior
#' @param dat Dataset with column `Posterior` of type `rvar`.
#' @param futval Futility reference value
#' @return A tibble of posterior summaries
#' @export
summarise_posterior <- function(dat, futval = 1.1) {
  dat %>%
    mutate(
      Median =
        sprintf("%.2f", median(Posterior)),
      `95\\% CrI` =
        sprintf("(%.2f, %.2f)", quantile(Posterior, 0.025), quantile(Posterior, 0.975)),
      `Mean (SD)` =
        sprintf("%.2f (%.2f)", mean(Posterior), sd(Posterior)),
      `Pr(OR < 1)` =
        sprintf("%.2f", Pr(Posterior < 1)),
      `Pr(OR > 1/1.1)` =
        sprintf("%.2f", Pr(Posterior > 1/futval))
    )
}


#' @title Summarise decision quantities
#' @param OR RVs
#' @param format Table format
#' @param fut_val Futility reference value
#' @return A tibble
#' @export
decision_quantity_summary_table <- function(OR, format = "html", fut_val = 1.1) {
  tdat <- tibble(
    Intervention = names(OR),
    posterior = OR,
    Posterior = sprintf("%.2f (%.2f, %.2f)",
                        median(posterior),
                        quantile(posterior, 0.025),
                        quantile(posterior, 0.975)),
    `Superior\nPr(OR = min(OR))` = sprintf("%.2f", E(posterior == rvar_min(posterior))),
    `Effective\nPr(OR < 1)` = sprintf("%.2f", Pr(posterior < 1)),
    `Futile\nPr(OR > 1/1.1)` = sprintf("%.2f", Pr(posterior > 1/fut_val)),
    `Equivalent\nPr(1/1.1 < OR < 1.1)` = sprintf("%.2f", Pr(posterior < fut_val & posterior > 1/fut_val))
  ) %>%
    select(-posterior)
  tdat[1, 2] <- "1.00"
  tdat[1, -(1:3)] <- "-"
  if(format == "latex") {
    colnames(tdat) <- linebreak(colnames(tdat), align = "c", linebreaker = "\n")
  }
  return(tdat)
}
