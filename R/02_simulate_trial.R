# Simulated trial with the structure of the published study -----------------------
#
# The patient data are not public. This script builds a synthetic two-arm trial
# with the same design features, so the analysis code can be run end to end:
#   * 136 women randomised 1:1 in permuted blocks within three strata
#     (no comorbidity 40%, comorbidity 30%, renal deterioration 30%);
#   * five visits (V1 to V5) with a continuous outcome (handgrip strength, kg)
#     and a binary one (undernutrition);
#   * monotone dropout that depends on baseline risk factors (stage, renal
#     function), so completers differ from the randomised population;
#   * tumour response and overall survival with censoring.
# The numbers it produces are NOT the study's results.

set.seed(SEED)
n <- 136

stratum <- sample(c("none", "comorbidity", "renal"), n, replace = TRUE, prob = c(0.4, 0.3, 0.3))
# Permuted blocks of 4 within each stratum
arm <- character(n)
for (s in unique(stratum)) {
  ix <- which(stratum == s)
  blocks <- unlist(replicate(ceiling(length(ix) / 4), sample(rep(c("AID", "LRD"), 2)), simplify = FALSE))
  arm[ix] <- blocks[seq_along(ix)]
}

trial <- tibble(
  id = sprintf("P%03d", 1:n), arm = factor(arm, c("LRD", "AID")), stratum,
  age = round(pmin(pmax(rnorm(n, 47, 11), 22), 80)),
  advanced_stage = rbinom(n, 1, 0.6),
  renal = as.integer(stratum == "renal")
)

# Handgrip strength: dips during treatment (V3–V4) and recovers by V5
visit_effect <- c(0, -0.5, -2.0, -2.5, -1.0)
subject_re <- rnorm(n, 0, 4.5)
strength <- sapply(1:5, function(v)
  22 + subject_re + visit_effect[v] + 0.6 * (trial$arm == "AID") * (v >= 4) -
    0.05 * (trial$age - 47) + rnorm(n, 0, 1.6))
colnames(strength) <- paste0("strength_v", 1:5)

# Undernutrition: peaks at V3
p_under <- sapply(1:5, function(v) plogis(-1.6 + c(0, 0.3, 1.9, 1.2, -0.6)[v] +
                                            0.6 * trial$renal - 0.4 * (trial$arm == "AID") * (v == 5)))
under <- matrix(rbinom(n * 5, 1, p_under), n, 5, dimnames = list(NULL, paste0("under_v", 1:5)))

trial <- bind_cols(trial, as_tibble(strength), as_tibble(under))

# Monotone dropout after V2, more likely with advanced stage or renal deterioration
p_drop <- plogis(-1.4 + 0.9 * trial$advanced_stage + 1.1 * trial$renal)
drop_at <- ifelse(rbinom(n, 1, p_drop) == 1, sample(3:5, n, replace = TRUE), 6)
for (v in 3:5) {
  gone <- drop_at <= v
  trial[[paste0("strength_v", v)]][gone] <- NA
  trial[[paste0("under_v", v)]][gone] <- NA
}
trial$completed <- drop_at == 6

# Tumour response and survival (months), administrative censoring at 72 months
trial$complete_response <- rbinom(n, 1, plogis(0.2 + 0.4 * (trial$arm == "AID") - 0.5 * trial$advanced_stage))
rate <- 0.006 * exp(1.2 * (1 - trial$complete_response) + 0.3 * (trial$arm == "LRD"))
event_time <- rexp(n, rate)
censor_time <- runif(n, 24, 72)
trial$os_months <- pmin(event_time, censor_time)
trial$death <- as.integer(event_time <= censor_time)

write_csv(trial, "results/demo/simulated_trial.csv")
cat(sprintf("Simulated trial: %d randomised, %d completed all visits\n", n, sum(trial$completed)))
