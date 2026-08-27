# generate_manuscript_tables.R
# Juergen Degenfellner, 2026-08-27
#
# ONE clean run of the canonical pipeline (Petra_longneck_update_260612.R,
# filter <= 30% -> n = 152, kNN imputation, WUR as sum of sides), then all
# manuscript regression tables (3-6) are generated from THAT run via the same
# best-subset BIC selection the Methods describe (select_bic(), identical
# candidate pools as the internal validation). Output: publication-ready
# .docx/.html tables in ../results_20260827/.
#
# Note: Table 6 / strongest pain WITHOUT baseline: the BIC selection keeps
# only recurrent pain here — the published 3-predictor version stemmed from an
# older data state and is NOT reproduced by this pipeline.

# --- 1. Full canonical run (also regenerates validation_results.html) --------
canonical <- "Petra_longneck_update_260612.R"   # run with cwd = code/
stopifnot(file.exists(canonical))
pdf(NULL)
invisible(capture.output(suppressWarnings(
  eval(parse(text = paste(readLines(canonical, warn = FALSE), collapse = "\n")),
       envir = globalenv())
)))
invisible(dev.off())
stopifnot(nrow(df_imp) == 152, exists("select_bic"), exists("cand1"))

out_dir <- "./results"
dir.create(out_dir, showWarnings = FALSE)

# --- 2. The six models, selected exactly as in the pipeline ------------------
models <- list(
  Table3_disability_with_baseline =
    select_bic(df_imp, "disability_t1",  cand1),
  Table4_disability_no_baseline =
    select_bic(df_imp, "disability_t1",  setdiff(cand1, "disability_t0")),
  Table5_average_pain_with_baseline =
    select_bic(df_imp, "paindetect3_t1", cand3),
  Table5_strongest_pain_with_baseline =
    select_bic(df_imp, "paindetect2_t1", cand2),
  Table6_average_pain_no_baseline =
    select_bic(df_imp, "paindetect3_t1", setdiff(cand3, "paindetect3_t0")),
  Table6_strongest_pain_no_baseline =
    select_bic(df_imp, "paindetect2_t1", setdiff(cand2, "paindetect2_t0"))
)

titles <- c(
  Table3_disability_with_baseline     = "Table 3 - Disability (%) at T1, best subset (BIC), with baseline",
  Table4_disability_no_baseline       = "Table 4 - Disability (%) at T1, best subset (BIC), WITHOUT baseline disability",
  Table5_average_pain_with_baseline   = "Table 5a - Average pain (0-10) at T1, best subset (BIC), with baseline",
  Table5_strongest_pain_with_baseline = "Table 5b - Strongest pain (0-10) at T1, best subset (BIC), with baseline",
  Table6_average_pain_no_baseline     = "Table 6a - Average pain (0-10) at T1, best subset (BIC), WITHOUT baseline pain",
  Table6_strongest_pain_no_baseline   = "Table 6b - Strongest pain (0-10) at T1, best subset (BIC), WITHOUT baseline pain"
)

make_table <- function(fit, title) {
  s  <- summary(fit)
  fs <- s$fstatistic
  note <- sprintf(
    "N = %d (kNN-imputed). R² = %.3f, adjusted R² = %.3f, F(%d, %d) = %.2f, p = %s. WUR variables are sums of both sides.",
    nobs(fit), s$r.squared, s$adj.r.squared, fs[2], fs[3], fs[1],
    format.pval(pf(fs[1], fs[2], fs[3], lower.tail = FALSE), digits = 3, eps = 1e-16))
  gtsummary::tbl_regression(fit, intercept = TRUE,
                            estimate_fun = function(x) gtsummary::style_number(x, digits = 2)) |>
    gtsummary::modify_column_unhide(dplyr::any_of("std.error")) |>
    gtsummary::as_gt() |>
    gt::tab_header(title = title) |>
    gt::tab_source_note(note)
}

console_block <- function(name, fit) {
  cat("\n=====", name, "=====\n")
  print(summary(fit)); print(confint(fit))
}

for (nm in names(models)) {
  tab <- make_table(models[[nm]], titles[[nm]])
  ok <- tryCatch({ gt::gtsave(tab, file.path(out_dir, paste0(nm, ".docx"))); TRUE },
                 error = function(e) { message(nm, ": docx export failed - ", conditionMessage(e)); FALSE })
  gt::gtsave(tab, file.path(out_dir, paste0(nm, ".html")))
  console_block(nm, models[[nm]])
}

# --- 3. Validation table (already computed in the canonical run) -------------
try(gt::gtsave(gt_validation, file.path(out_dir, "Validation_bootstrap.docx")), silent = TRUE)
gt::gtsave(gt_validation, file.path(out_dir, "Validation_bootstrap.html"))
file.copy("validation_results.html", file.path(out_dir, "validation_results.html"),
          overwrite = TRUE)

# --- 4. Session info ----------------------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
cat("\nAll tables written to", normalizePath(out_dir), "\n")
