# The analysis workflow of the trial, run on the simulated data ---------------------
#
# 1. Who dropped out?             completers vs non-completers (relative risks)
# 2. Missing data                 MICE with predictive mean matching, m = 35, 15 iterations
# 3. Pooling                      Rubin's rules for parametric models;
#                                 median p-value across imputations for rank tests
# 4. Effect sizes                 relative risk; Hodges–Lehmann median difference
# 5. Repeated measures            linear mixed model, time-by-arm interaction
# 6. Time to event                Kaplan–Meier, log-rank, Cox regression
# 7. Per protocol vs ITT          the same model on completers and on all randomised

trial <- read_csv("results/demo/simulated_trial.csv", show_col_types = FALSE) |>
  mutate(arm = factor(arm, c("LRD", "AID")))

rr_ci <- function(a, n1, b, n2) {               # risk ratio with a log-normal 95% CI
  rr <- (a / n1) / (b / n2); se <- sqrt(1 / a - 1 / n1 + 1 / b - 1 / n2)
  c(rr = rr, lower = rr * exp(-1.96 * se), upper = rr * exp(1.96 * se))
}

# ---- 1. Attrition -------------------------------------------------------------
attr_tbl <- bind_rows(lapply(c("renal", "advanced_stage"), function(v) {
  x <- trial[[v]]; d <- !trial$completed
  r <- rr_ci(sum(x[d]), sum(d), sum(x[!d]), sum(!d))
  tibble(characteristic = v, pct_dropout = 100 * mean(x[d]), pct_completers = 100 * mean(x[!d]),
         rr = r[["rr"]], lower = r[["lower"]], upper = r[["upper"]],
         p = fisher.test(table(x, d))$p.value)
}))

# ---- 2. Multiple imputation ----------------------------------------------------
imp_vars <- c("arm", "age", "advanced_stage", "renal", paste0("strength_v", 1:5))
imp_data <- trial[imp_vars]
meth <- make.method(imp_data); meth[] <- ifelse(colSums(is.na(imp_data)) > 0, "pmm", "")
imp <- mice(imp_data, m = 35, maxit = 15, method = meth, donors = 10, printFlag = FALSE, seed = SEED)

# ---- 3. Pooling ----------------------------------------------------------------
# Parametric: ANCOVA of strength at V5 on arm, adjusted for baseline -> Rubin's rules
fit <- with(imp, lm(strength_v5 ~ arm + strength_v1))
pooled <- summary(pool(fit), conf.int = TRUE) |> as_tibble() |> filter(term == "armAID")
# Rank test: no Rubin's rules, so run it in every completed dataset and report the median p
p_each <- vapply(1:imp$m, function(i) {
  d <- complete(imp, i); wilcox.test(strength_v5 ~ arm, data = d, exact = FALSE)$p.value
}, numeric(1))

# ---- 4. Effect sizes -------------------------------------------------------------
cr <- table(trial$arm, trial$complete_response)
rr_cr <- rr_ci(cr["AID", "1"], sum(cr["AID", ]), cr["LRD", "1"], sum(cr["LRD", ]))
d1 <- complete(imp, 1)                       # Hodges–Lehmann shift, AID minus LRD
hl <- wilcox.test(d1$strength_v5[d1$arm == "AID"], d1$strength_v5[d1$arm == "LRD"],
                  conf.int = TRUE, exact = FALSE)

# ---- 5. Mixed model for the five visits -----------------------------------------
long <- trial |>
  select(id, arm, starts_with("strength_v")) |>
  pivot_longer(starts_with("strength_v"), names_to = "visit", values_to = "strength",
               names_prefix = "strength_v") |>
  mutate(visit = factor(visit))
m_full <- lmer(strength ~ visit * arm + (1 | id), data = long, REML = FALSE)
m_main <- lmer(strength ~ visit + arm + (1 | id), data = long, REML = FALSE)
interaction_test <- anova(m_main, m_full)

# ---- 6. Survival -----------------------------------------------------------------
lr  <- survdiff(Surv(os_months, death) ~ arm, data = trial)
cox <- coxph(Surv(os_months, death) ~ arm + complete_response, data = trial)

# ---- 7. Per protocol vs intention to treat ---------------------------------------
pp  <- glm(under_v5 ~ arm + renal, family = binomial, data = filter(trial, completed))
itt_data <- trial |> select(arm, age, advanced_stage, renal, starts_with("under_v")) |>
  mutate(across(starts_with("under_v"), factor))
meth2 <- make.method(itt_data); meth2[] <- ifelse(colSums(is.na(itt_data)) > 0, "logreg", "")
itt_imp <- mice(itt_data, m = 35, maxit = 15, method = meth2, printFlag = FALSE, seed = SEED)
itt <- summary(pool(with(itt_imp, glm(under_v5 ~ arm + renal, family = binomial))), conf.int = TRUE) |>
  as_tibble() |> filter(term == "armAID")

summary_tbl <- tibble(
  analysis = c("Attrition: renal deterioration (RR dropout vs completers)",
               "ANCOVA strength V5, pooled with Rubin's rules (AID − LRD, kg)",
               "Mann–Whitney strength V5, median p over 35 imputations",
               "Complete response (RR, AID vs LRD)",
               "Hodges–Lehmann median difference, strength V5 (kg)",
               "Mixed model: time-by-arm interaction (LRT p)",
               "Overall survival: log-rank p",
               "Overall survival: Cox HR for AID",
               "Undernutrition V5, per protocol (OR for AID)",
               "Undernutrition V5, ITT with imputation (OR for AID)"),
  estimate = c(attr_tbl$rr[1], pooled$estimate, median(p_each), rr_cr[["rr"]], unname(hl$estimate),
               interaction_test$`Pr(>Chisq)`[2], 1 - pchisq(lr$chisq, 1),
               exp(coef(cox))[["armAID"]], exp(coef(pp))[["armAID"]], exp(itt$estimate)),
  lower = c(attr_tbl$lower[1], pooled$`2.5 %`, NA, rr_cr[["lower"]], hl$conf.int[1], NA, NA,
            exp(confint(cox))["armAID", 1], exp(confint.default(pp))["armAID", 1], exp(itt$`2.5 %`)),
  upper = c(attr_tbl$upper[1], pooled$`97.5 %`, NA, rr_cr[["upper"]], hl$conf.int[2], NA, NA,
            exp(confint(cox))["armAID", 2], exp(confint.default(pp))["armAID", 2], exp(itt$`97.5 %`))
)
write_csv(attr_tbl, "results/demo/attrition.csv")
write_csv(summary_tbl, "results/demo/analysis_summary.csv")
print(summary_tbl, n = Inf)

# Figure: Kaplan–Meier on the simulated data, to show the output of the workflow
km <- survfit(Surv(os_months, death) ~ arm, data = trial)
km_df <- tibble(time = km$time, surv = km$surv,
                arm = rep(sub("arm=", "", names(km$strata)), km$strata))
p_km <- ggplot(km_df, aes(time, surv, colour = arm)) + geom_step(linewidth = 0.9) +
  scale_colour_manual(values = c(AID = "#d93025", LRD = "#188038")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Months", y = "Survival probability", colour = NULL,
       title = "Kaplan–Meier output of the workflow (simulated data)") +
  theme(legend.position = "top")
ggsave("figures/demo_km_simulated.png", p_km, width = 7, height = 4, dpi = 150)
