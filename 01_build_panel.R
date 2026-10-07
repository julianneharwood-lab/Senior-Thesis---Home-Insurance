# ==============================================================================
# 01_build_panel.R
# Build the loan-year analysis panels from raw CoreLogic LLMA extracts, merged
# with ZIP-level home-price data (Zillow ZHVI) used both to build the escrow
# panel and to construct a mark-to-market home-equity control.
#
# DATA ACCESS -- READ BEFORE RUNNING
# CoreLogic LLMA loan-level data is licensed to Yale under a Data Use
# Agreement and is NOT included in this repository (see README). This script
# documents the exact construction of the analysis panel and will run once a
# properly licensed copy of the raw extracts is placed in data/raw/.
#
# Expected raw inputs (data/raw/):
#   orig_border.csv            loan-level origination attributes   (CoreLogic LLMA)
#   perf_extracts/*.csv        one file per year of loan-level
#                               monthly performance                (CoreLogic LLMA)
#   border_zip_zhvi_panel.csv  ZIP x year home-price index          (Zillow ZHVI, public)
# ==============================================================================

source("scripts/00_config.R")

# ------------------------------------------------------------------------------
# Loan origination attributes
# ------------------------------------------------------------------------------
orig_border <- fread(file.path(RAW_DIR, "orig_border.csv"))
orig_border[, loan_id := as.character(loan_id)]

ob <- orig_border[, .(
  loan_id = as.character(loan_id),
  state, county_fips,
  fico_score_at_origination,   # kept for planned FICO-tier heterogeneity work; unused below
  origination_date,
  zip5, sale_price, appraised_value
)]

ob[, zip5           := sprintf("%05d", as.integer(zip5))]
ob[, orig_year      := as.integer(origination_date) %/% 100]     # YYYYMM -> YYYY
ob[, orig_home_value := pmin(sale_price, appraised_value, na.rm = TRUE)]
ob[orig_home_value <= 0, orig_home_value := NA]                  # guard against $0 sale/appraisal values

# ------------------------------------------------------------------------------
# ZIP-level home-price index at origination (for mark-to-market equity, below)
# ------------------------------------------------------------------------------
zhvi <- fread(file.path(RAW_DIR, "border_zip_zhvi_panel.csv"))
zhvi[, zip := sprintf("%05d", as.integer(zip))]

ob <- merge(ob, zhvi[, .(zip5 = zip, orig_year = year, idx_orig = idx)],
            by = c("zip5", "orig_year"), all.x = TRUE)

# sanity check: confirm a high match rate on the price-index merge
ob[, .(share_matched = mean(!is.na(idx_orig))), by = orig_year][order(orig_year)]

# ------------------------------------------------------------------------------
# Loan-month performance -> loan-year distress panel
# ------------------------------------------------------------------------------
perf_border <- rbindlist(
  lapply(list.files(file.path(RAW_DIR, "perf_extracts"), full.names = TRUE), fread),
  fill = TRUE
)
perf_border[, loan_id := as.character(loan_id)]

loan_year <- perf_border[, .(
  ever_30plus      = any(mba_delinquency_status %in% STATUS_30PLUS),
  ever_90plus      = any(mba_delinquency_status %in% STATUS_90PLUS),
  ever_foreclosure = any(mba_delinquency_status %in% STATUS_FORECLOSURE),
  ever_reo         = any(mba_delinquency_status == STATUS_REO),
  n_months         = .N
), by = .(loan_id, report_year)]

loan_year <- merge(loan_year, ob, by = "loan_id", all.x = TRUE)
loan_year[, treat := as.integer(state == "FL")]
loan_year[, post  := as.integer(report_year >= POST_YEAR)]

# merge check: both states populated, ~no unmatched loans
loan_year[, .N, by = state]
stopifnot(sum(is.na(loan_year$state)) == 0)

# ------------------------------------------------------------------------------
# Escrow burden (taxes + insurance): current loans only, plausible band
# (Keys & Mulder's escrow-inference method: total payment due - scheduled P&I)
# ------------------------------------------------------------------------------
perf_border[, tpd   := as.numeric(total_payment_due)]
perf_border[, pi    := as.numeric(scheduled_monthly_pi)]
perf_border[, esc_m := tpd - pi]

esc <- perf_border[
  mba_delinquency_status == "C" & tpd > 0 & pi > 0 & esc_m > 10 & esc_m < 2000,
  .(escrow_ann = 12 * median(esc_m)), by = .(loan_id, report_year)
]
esc[, loan_id := as.character(loan_id)]
esc <- merge(esc, ob, by = "loan_id", all.x = TRUE)
esc[, treat := as.integer(state == "FL")]
esc[, post  := as.integer(report_year >= POST_YEAR)]

# outlier sanity check: mean close to median, max in a plausible range
esc[, .(mean = mean(escrow_ann), median = median(escrow_ann), max = max(escrow_ann)), by = state]

# ------------------------------------------------------------------------------
# Mark-to-market current LTV (home-equity control; see 03_robustness.R)
# current value = origination value x (price index now / price index at origination)
# ------------------------------------------------------------------------------
perf_border[, current_balance := as.numeric(current_balance)]

bal <- perf_border[!is.na(current_balance) & current_balance > 0,
                   .(current_balance = median(current_balance, na.rm = TRUE)),
                   by = .(loan_id, report_year)]
bal[, loan_id := as.character(loan_id)]

equity <- merge(bal, ob[, .(loan_id, zip5, orig_year, orig_home_value, idx_orig)],
                by = "loan_id", all.x = TRUE)
equity <- merge(equity, zhvi[, .(zip5 = zip, report_year = year, idx_now = idx)],
                by = c("zip5", "report_year"), all.x = TRUE)

equity[, mtm_home_value := orig_home_value * (idx_now / idx_orig)]
equity[, current_ltv    := current_balance / mtm_home_value]
equity[current_ltv < 0 | current_ltv > 2, current_ltv := NA]   # drop implausible LTVs

loan_year <- merge(loan_year, equity[, .(loan_id, report_year, current_ltv)],
                   by = c("loan_id", "report_year"), all.x = TRUE)

# ------------------------------------------------------------------------------
# Save analysis-ready panels for the downstream scripts
# ------------------------------------------------------------------------------
saveRDS(loan_year, file.path(PROCESSED_DIR, "loan_year.rds"))
saveRDS(esc,       file.path(PROCESSED_DIR, "esc.rds"))
saveRDS(ob,        file.path(PROCESSED_DIR, "ob.rds"))
