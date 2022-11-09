#' @title compile_cmdstanr_mod
#' Compile provided cumulative logistic Stan model for cmdstanr.
#' @param mod_name The model name
#' @return A `CmdStanModel` object
#' @importFrom cmdstanr cmdstan_model
compile_cmdstanr_mod <- function(mod_name) {
  cmdstanr::cmdstan_model(stan_file = system.file("stan", paste0(mod_name, ".stan"), package = "ASCOTr"))
}
