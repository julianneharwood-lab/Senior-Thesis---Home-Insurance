# ==============================================================================
# 00_config.R
# Shared paths, constants, and plotting defaults used across the analysis
# scripts. Source this at the top of every other script; don't run it alone.
#
# Run all scripts with the repository root as the working directory, e.g.
#   Rscript scripts/01_build_panel.R
# from the top level of the repo, or open an RStudio project at the repo root.
# ==============================================================================

library(data.table)
library(fixest)
library(ggplot2)

# ---- Paths -------------------------------------------------------------------
RAW_DIR       <- "data/raw"         # licensed/raw inputs -- not included, see README
PROCESSED_DIR <- "data/processed"   # analysis-ready panels built by 01_build_panel.R
OUTPUT_DIR    <- "output"           # figures and LaTeX tables

dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUT_DIR,    showWarnings = FALSE, recursive = TRUE)

# ---- Design constants ----------------------------------------------------------
EVENT_REF_YEAR <- 2018   # omitted/reference year in event-study specifications
POST_YEAR      <- 2021   # first year the FL-GA reinsurance-driven premium gap opens up

# mba_delinquency_status codes (CoreLogic LLMA) grouped into the outcomes used
# throughout the analysis
STATUS_30PLUS      <- c("3", "6", "9", "F", "R")
STATUS_90PLUS      <- c("9", "F", "R")
STATUS_FORECLOSURE <- c("F", "R")
STATUS_REO         <- "R"

# ---- Plot styling --------------------------------------------------------------
STATE_COLORS <- c(FL = "#D85A30", GA = "#378ADD")
theme_set(theme_minimal())
