# Packages and shared settings ---------------------------------------------------
pkgs <- c("dplyr", "tidyr", "readr", "ggplot2", "mice", "survival", "lme4")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) stop("Install these packages first: ", paste(missing_pkgs, collapse = ", "))
suppressPackageStartupMessages(invisible(lapply(pkgs, library, character.only = TRUE)))

dir.create("figures", showWarnings = FALSE)
dir.create("results/demo", recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12))
SEED <- 2018
