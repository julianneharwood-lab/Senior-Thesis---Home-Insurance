# ==============================================================================
# 03_robustness.R
# Two checks aimed at the main identification threat: Florida border ZIPs saw
# a larger home-price run-up than Georgia's over this period (a differential
# equity/in-migration boom), which could offset or mask real mortgage distress
# on the treated side. See README for the full argument.
#
#   (1) Pre-2020-origination restriction -- drops the loan vintage most
#       exposed to the COVID-era in-migration wave and re-estimates the
#       headline DiDs on the rest.
#   (2) Mark-to-market LTV control -- attempts to control for home equity
#       directly, then checks whether that control is doing real work or
#       merely reflecting which loans happen to have usable data.
#
# Run 01_build_panel.R and 02_main_results.R first.
# ==============================================================================

source("scripts/00_config.R")

loan_year   <- readRDS(file.path(PROCESSED_DIR, "loan_year.rds"))
esc         <- readRDS(file.path(PROCESSED_DIR, "esc.rds"))
ob          <- readRDS(file.path(PROCESSED_DIR, "ob.rds"))
main_models <- readRDS(file.path(PROCESSED_DIR, "main_models.rds"))
list2env(main_models, envir = environment())   # loads m_esc, m_fc, m_90, m_30

# ------------------------------------------------------------------------------
# (1) Pre-2020-origination restriction
# ------------------------------------------------------------------------------
pre2020_loans <- ob[orig_year < 2020, loan_id]

loan_year_pre <- loan_year[loan_id %in% pre2020_loans]
esc_pre       <- esc[loan_id %in% pre2020_loans]

# how much of each state's loans are dropped by the cutoff
ob[, .N, by = .(state, pre2020 = orig_year < 2020)]

m_esc_pre <- feols(escrow_ann       ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = esc_pre)
m_fc_pre  <- feols(ever_foreclosure ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year_pre)
m_90_pre  <- feols(ever_90plus      ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year_pre)
m_30_pre  <- feols(ever_30plus      ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year_pre)

etable(m_esc, m_esc_pre, m_fc, m_fc_pre, m_90, m_90_pre, m_30, m_30_pre,
       tex = TRUE, file = file.path(OUTPUT_DIR, "pre2020_robustness.tex"), replace = TRUE,
       headers = c("Escrow", "Escrow (pre-2020)", "Foreclosure", "Foreclosure (pre-2020)",
                   "90+ delinq.", "90+ delinq. (pre-2020)", "30+ delinq.", "30+ delinq. (pre-2020)"),
       dict = c("post::1:treat" = "Florida x Post-2021"),
       digits = 3, fitstat = ~ n + r2,
       title = "Escrow and mortgage distress: full sample vs.\\ pre-2020 originations",
       label = "tab:pre2020")

# ------------------------------------------------------------------------------
# (2) Mark-to-market LTV: does the control do real work, or is it selection?
# ------------------------------------------------------------------------------
m_90_eq <- feols(ever_90plus      ~ i(post, treat, ref = 0) + current_ltv | loan_id + report_year,
                 cluster = ~county_fips, data = loan_year)
m_fc_eq <- feols(ever_foreclosure ~ i(post, treat, ref = 0) + current_ltv | loan_id + report_year,
                 cluster = ~county_fips, data = loan_year)

# Same spec, same (LTV-available) subsample, WITHOUT the control. This isolates
# whether the change in the coefficient between m_90/m_fc and m_90_eq/m_fc_eq
# comes from the control itself, or just from moving to a smaller sample.
m_90_samp <- feols(ever_90plus      ~ i(post, treat, ref = 0) | loan_id + report_year,
                   cluster = ~county_fips, data = loan_year[!is.na(current_ltv)])
m_fc_samp <- feols(ever_foreclosure ~ i(post, treat, ref = 0) | loan_id + report_year,
                   cluster = ~county_fips, data = loan_year[!is.na(current_ltv)])

etable(m_90, m_90_samp, m_90_eq, m_fc, m_fc_samp, m_fc_eq,
       tex = TRUE, file = file.path(OUTPUT_DIR, "ltv_decomposition.tex"), replace = TRUE,
       headers = c("Full sample", "LTV sample, no control", "LTV sample, +LTV",
                   "Full sample", "LTV sample, no control", "LTV sample, +LTV"),
       dict = c("post::1:treat" = "Florida x Post-2021", current_ltv = "Current LTV"),
       digits = 3, fitstat = ~ n + r2)

# Diagnostic: is the LTV-available subsample balanced across treatment status?
# If match rates differ by state, the subsample above is not a clean
# apples-to-apples comparison, regardless of what its coefficients show --
# see README for how this check is actually used in interpreting the result.
loan_year[, .(share_matched = mean(!is.na(current_ltv))), by = state]
