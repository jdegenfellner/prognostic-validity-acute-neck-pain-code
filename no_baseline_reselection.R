# no_baseline_reselection.R
# ------------------------------------------------------------------------------
# Helpers for the "without baseline" prediction models in
# Petra_longneck_update_25.R.
#
# Co-author's point ("Karten neu mischen"): for the models WITHOUT the baseline
# predictor we must NOT simply drop the baseline term from the with-baseline BIC
# model and keep the rest. Removing the dominant baseline predictor can change
# which of the remaining predictors are BIC-optimal, so we re-run the full
# best-subset search over ALL original candidate predictors *minus* the baseline.
#
# Sourced from the main script after the packages (esp. leaps) are loaded.
# ------------------------------------------------------------------------------

# Map regsubsets coefficient names (incl. factor-dummy levels such as
# "occupationretired" / "sexmale") back to the ORIGINAL predictor variables, so
# that reformulate()/lm() expand the factors themselves. Avoids the
# "object 'occupation...' not found" error when a factor level gets selected.
coef_to_vars <- function(coef_names, candidate_vars) {
  hits <- vapply(candidate_vars, function(v)
    any(coef_names == v | startsWith(coef_names, v)), logical(1))
  candidate_vars[hits]
}

# Re-run best-subset (BIC) selection for `outcome` over `candidate_preds` MINUS
# `baseline_var`.
#   sel_data : data used for the regsubsets search (lin-dep-filtered, e.g.
#              dat_red_w_lin_dep) so the occupation dummies are not collinear.
#   fit_data : data used for the final lm fit (full data, e.g. df_imp), to mirror
#              how the with-baseline BIC models are fitted.
# Returns a list(formula, model, selected).
reselect_no_baseline <- function(outcome, candidate_preds, baseline_var,
                                  sel_data, fit_data) {
  fit_sym  <- substitute(fit_data)            # e.g. the symbol `df_imp`
  preds_nb <- setdiff(candidate_preds, baseline_var)
  d_nb     <- droplevels(sel_data[, c(outcome, preds_nb)])

  reg_form <- reformulate(preds_nb, response = outcome)
  regfit   <- leaps::regsubsets(reg_form, data = d_nb,
                                nbest = 3, nvmax = length(preds_nb))
  reg_sum  <- summary(regfit)
  coefs    <- coef(regfit, id = which.min(reg_sum$bic))
  vars     <- coef_to_vars(names(coefs)[-1], preds_nb)
  if (length(vars) == 0) vars <- "1"          # guard: BIC picks intercept-only
  form     <- reformulate(vars, response = outcome)

  # Fit so the stored call inlines the formula and references the data by its
  # original symbol (e.g. df_imp). Downstream helpers (check_model(), qqPlot(),
  # update(), insight::get_data()) re-evaluate that symbol, so it must resolve
  # to the actual data frame.
  #
  # The previous version evaluated in parent.frame(); that only resolves the
  # symbol when the function is called from the frame that holds `df_imp`
  # (i.e. top level). Called from anywhere else -- inside another function, a
  # sourced-as-local block, knitr/Rmarkdown -- parent.frame() is NOT the global
  # env and the fit fails with "object 'df_imp' not found". That is the likely
  # reason it ran for one of us but not the other. We resolve the data symbol
  # against an environment whose parent is the global env, so the fit (and every
  # downstream re-evaluation) finds the global `df_imp` regardless of where this
  # function is called from.
  fit_env <- new.env(parent = globalenv())
  model   <- eval(bquote(lm(.(form), data = .(fit_sym))), envir = fit_env)

  list(formula = form, model = model, selected = vars)
}

# One-block-per-outcome comparison of the with-baseline BIC model vs the
# re-selected without-baseline model. Returns a tidy data.frame (2 rows/outcome).
compare_with_without_baseline <- function(label, model_with, model_without) {
  fmt <- function(m) paste(deparse(formula(m), width.cutoff = 500), collapse = "")
  data.frame(
    outcome      = label,
    model        = c("with baseline", "without baseline (re-selected)"),
    formula      = c(fmt(model_with),                 fmt(model_without)),
    n_predictors = c(length(coef(model_with)) - 1L,   length(coef(model_without)) - 1L),
    r2_adj       = c(summary(model_with)$adj.r.squared, summary(model_without)$adj.r.squared),
    r2           = c(summary(model_with)$r.squared,     summary(model_without)$r.squared),
    AIC          = c(AIC(model_with),                  AIC(model_without)),
    BIC          = c(BIC(model_with),                  BIC(model_without)),
    stringsAsFactors = FALSE
  )
}
