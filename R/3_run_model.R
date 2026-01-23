# ---
# _ RUN MODEL
# ---


# Packages
library(tidyverse)
library(cmdstanr)
library(here)


# Create dir to store model results
if (!dir.exists(here("results"))) dir.create(here("results"))


# Load data
load(here("data", "input_dataset.Rda"))


# Create input list
list_data <- list(
    J = denmark_panel$ministry_id %>% unique() %>% length(),
    K = 4,
    P = denmark_panel$party_id %>% unique() %>% length(),
    N = nrow(denmark_panel),
    T = denmark_panel$year_id %>% unique() %>% length(),
    C = denmark_panel$y %>% unique() %>% length(),
    jj = denmark_panel$ministry_id,
    kk = denmark_panel$indicator_id,
    pp = denmark_panel$party_id,
    tt = denmark_panel$year_id,
    y = denmark_panel$y,
    occ = denmark_panel$occupied
)

# Compile the model
options(mc.cores = 4)
model <- cmdstan_model(here("stan", "party_effects_occupation_model.stan"))

# Run the model (~4h, depending on resources)
inicio <- Sys.time()
fit <- model$sample(
  data = list_data,
  seed = 44,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_warmup = 2000,
  iter_sampling = 2000
)
fim <- Sys.time()
fim - inicio

fit$save_object(file = here("models", "model_full.Rds"))
#fit <- readRDS(here("models", "model_full.Rds"))


# Extract and save portfolio-party draws
set.seed(44)
est_param <- fit %>%
  as_draws_array() %>%
  subset_draws("est") %>%
  as_draws_df() %>%
  na.omit() %>%
  sample_n(1000)

saveRDS(est_param, file = here("models", "est_param_fullmodel.Rds"))
rm(est_param)
gc()

# Save estimates
estimates <- fit$summary(variables = c("alpha", "beta", "theta", "lambda", "gamma", "est"), 
            "mean", ~ quantile2(., probs = c(0.025, 0.1, 0.9, 0.975)))  %>%
  as_tibble() %>%
  set_names("parameter", "estimate", "lo95", "up80", "lo80", "up95")

saveRDS(estimates, file = here("models", "model_summary_fullmodel.Rds"))


# Save model predictions
y_pred <- fit %>%
  spread_draws(y_pred[n], ndraws = 1000, seed = 44)

saveRDS(y_pred, file = here("models", "model_predictions_fullmodel.Rds"))


# Check model with shinystan
fit$diagnostic_summary()

draws <- fit$draws()
mcmc_trace(draws)


shinystan::launch_shinystan(shinystan::as.shinystan(fit))