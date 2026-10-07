# ==============================================================================
# 02_main_results.R
# Headline difference-in-differences results: escrow burden and mortgage
# distress, Florida (treated) vs. Georgia (control) border counties.
#
# Run 01_build_panel.R first.
# ==============================================================================

source("scripts/00_config.R")

loan_year <- readRDS(file.path(PROCESSED_DIR, "loan_year.rds"))
esc       <- readRDS(file.path(PROCESSED_DIR, "esc.rds"))

# ------------------------------------------------------------------------------
# Descriptives: distress rates by state and year
# ------------------------------------------------------------------------------
fc_avg <- loan_year[, .(fc_rate = mean(ever_foreclosure)), by = .(state, report_year)]

ggplot(fc_avg, aes(report_year, fc_rate, color = state)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  scale_color_manual(values = STATE_COLORS) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = EVENT_REF_YEAR, linetype = 3) +
  labs(title = "Mortgage foreclosure: FL (treated) vs. GA (control) border counties",
       y = "Share of loans", x = NULL, color = NULL)
ggsave(file.path(OUTPUT_DIR, "foreclosure_flga.png"), width = 10, height = 4.5, dpi = 140)

dq_avg  <- loan_year[, .(dq90 = mean(ever_90plus), dq30 = mean(ever_30plus)),
                     by = .(state, report_year)]
dq_long <- melt(dq_avg, id.vars = c("state", "report_year"),
                measure.vars = c("dq30", "dq90"),
                variable.name = "outcome", value.name = "rate")
dq_long[, outcome := factor(outcome, levels = c("dq30", "dq90"),
                            labels = c("Ever 30+ days delinquent", "Ever 90+ days delinquent"))]

ggplot(dq_long, aes(report_year, rate, color = state)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_color_manual(values = STATE_COLORS) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = EVENT_REF_YEAR, linetype = 3) +
  labs(title = "Mortgage delinquency: FL (treated) vs. GA (control) border counties",
       y = "Share of loans", x = NULL, color = NULL)
ggsave(file.path(OUTPUT_DIR, "delinquency_flga.png"), width = 10, height = 4.5, dpi = 140)

# ------------------------------------------------------------------------------
# Event studies (loan + year fixed effects, clustered by county)
# ------------------------------------------------------------------------------
es_esc  <- feols(escrow_ann  ~ i(report_year, treat, ref = EVENT_REF_YEAR) | loan_id + report_year,
                 cluster = ~county_fips, data = esc)
es_dq90 <- feols(ever_90plus ~ i(report_year, treat, ref = EVENT_REF_YEAR) | loan_id + report_year,
                 cluster = ~county_fips, data = loan_year)
es_dq30 <- feols(ever_30plus ~ i(report_year, treat, ref = EVENT_REF_YEAR) | loan_id + report_year,
                 cluster = ~county_fips, data = loan_year)

png(file.path(OUTPUT_DIR, "event_study_escrow.png"), width = 900, height = 500, res = 120)
iplot(es_esc, main = "Escrow burden: FL vs. GA", xlab = "Year")
abline(v = EVENT_REF_YEAR, lty = 3)
dev.off()

png(file.path(OUTPUT_DIR, "event_study_delinquency.png"), width = 900, height = 500, res = 120)
iplot(es_dq90, main = "Ever 90+ day delinquency: FL vs. GA", xlab = "Year")
abline(v = EVENT_REF_YEAR, lty = 3)
dev.off()

etable(es_esc, tex = TRUE, file = file.path(OUTPUT_DIR, "escrow_event_study.tex"), replace = TRUE,
       title = "Reinsurance shock and escrow burden (taxes + insurance): FL vs.\\ GA border counties",
       label = "tab:escrow_es",
       dict  = c(treat = "Florida", report_year = "Year", escrow_ann = "Annual escrow (\\$)"),
       digits = 2, fitstat = ~ n + r2,
       notes = sprintf("Event study relative to %d. Loan and year fixed effects; SEs clustered by county.",
                       EVENT_REF_YEAR))

# ------------------------------------------------------------------------------
# Headline result: pooled DiD, preferred specification (loan + year FE)
# ------------------------------------------------------------------------------
m_esc <- feols(escrow_ann       ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = esc)
m_fc  <- feols(ever_foreclosure ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year)
m_90  <- feols(ever_90plus      ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year)
m_30  <- feols(ever_30plus      ~ i(post, treat, ref = 0) | loan_id + report_year, cluster = ~county_fips, data = loan_year)

etable(m_esc, m_fc, m_90, m_30, tex = TRUE,
       file = file.path(OUTPUT_DIR, "main_did.tex"), replace = TRUE,
       headers = c("Escrow (\\$)", "Foreclosure", "90+ delinq.", "30+ delinq."),
       dict = c("post::1:treat" = "Florida $\\times$ Post-2021"),
       digits = 3, fitstat = ~ n + r2,
       title = "The insurance burden landed, but mortgage distress did not",
       label = "tab:main")

saveRDS(list(m_esc = m_esc, m_fc = m_fc, m_90 = m_90, m_30 = m_30),
        file.path(PROCESSED_DIR, "main_models.rds"))
