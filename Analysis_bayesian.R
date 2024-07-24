
### Bayesian analysis of beach cohort outcome data

pacman::p_load(
  Matrix,
  tidyverse,  
  rstatix,
  janitor,
  brms,
  tidybayes, 
  bayesplot,
  marginaleffects,
  cmdstanr
)

# Add remaining datasets together

data <- data |> mutate(
  across(contains("consent"), as.character),
  water_time = as.character(water_time),
  others = as.character(others),
  rec_act1 = as.character(rec_act1),
  symp_date = as.character(symp_date),
  misswork_days = as.character(misswork_days),
  others_follow = as.character(others_follow),
  month = as.factor(month),
  dow = as.factor(dow),
  date = as.Date(date, "%Y-%m-%d"))

data <- full_join(data, data_TO)

data_follow <- data |> filter(follow == "Yes") 

# Start with AGI outcome, first build model with no confounders then add confounders
# Varying-effects for date, household, and beach

get_prior(agi3 ~ water_contact2 + (1 | house_id/date) + (1 | beach), 
          family = bernoulli, data = data_follow)

priors <- set_prior("normal(0, 1)", class = "b")

m1 <- brm(agi3 ~ water_contact2 + (1 | house_id/date) + (1 | beach),
          family = bernoulli, data = data_follow, prior = priors, control = list(adapt_delta = 0.9),
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m1)
get_variables(m1)
plot(m1)
pp_check(m1, ndraws=100)
pp_check(m1, type = "stat", stat = "mean")
mcmc_acf(m1, pars = vars(contains("b_")), lags = 10)
mcmc_pairs(m1, pars = vars(contains("b_")), diag_fun = "den", off_diag_fun = "hex")

# Check if treating exposure as monotonic variable fits better than as indicator variable

data_follow <- data_follow |> 
  mutate(water_contact3 = factor(water_contact2, ordered = T, 
                          levels = c("No contact", "Minimal contact", "Body immersion", "Swallowed water")))

get_prior(agi3 ~ mo(water_contact3) + (1 | house_id/date) + (1 | beach), 
          family = bernoulli, data = data_follow)

# Dirichlet prior for the monotonic with expectation of stronger association from body immersion to minimal contact

priors2 <- c(set_prior("normal(0,1)",class= "b"),
            set_prior("dirichlet(c(1, 2, 1))", class = "simo", coef = "mowater_contact31"))

m1.2 <- brm(agi3 ~ mo(water_contact3) + (1 | house_id/date) + (1 | beach),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.9),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m1.2)
get_variables(m1.2)
plot(m1.2)
pp_check(m1.2, ndraws=100)
pp_check(m1.2, type = "stat", stat = "mean")
mcmc_acf(m1, pars = vars(contains("b_")), lags = 10)
mcmc_pairs(m1.2, pars = vars(contains("b_")), diag_fun = "den", off_diag_fun = "hex")
conditional_effects(m1.2)

loo(m1, m1.2)

# Model with monotonic predictor fits better
# Add minimal adjustment set of confounders

m2 <- brm(agi3 ~ mo(water_contact3) + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + e_coli_s + (1 | house_id/date) + (1 | beach),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.9),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2)
get_variables(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")

conditional_effects(m2)

# Add interaction with E. coli levels

m3 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + (1 | house_id/date) + (1 | beach),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3)
get_variables(m3)
plot(m3)
pp_check(m3, ndraws=100)
pp_check(m3, type = "stat", stat = "mean")

conditional_effects(m3)

plot_predictions(m3, re_formula=NA, condition = c("e_coli_s", "water_contact3"))

loo(m2, m3)

# Compare to model with human MST marker instead of E. coli as predictor

m3.2 <- brm(agi3 ~ mo(water_contact3)*mst_human_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + (1 | house_id/date) + (1 | beach),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")),
          control = list(adapt_delta = 0.95))

summary(m3.2)
get_variables(m3.2)
plot(m3.2)
pp_check(m3.2, ndraws=100)
pp_check(m3.2, type = "stat", stat = "mean")

conditional_effects(m3.2)

plot_predictions(m3.2, re_formula=NA, condition = c("mst_human_s", "water_contact3"))

loo(m2, m3, m3.2)



### Examine marginal effects

avg_comparisons(m3, variables = "water_contact3", comparison = "lnoravg", transform = "exp")

predictions(m3, re_formula = NA, type = "response", 
            newdata = datagrid(model = m3, water_contact3 = unique))

pred <- predictions(m3, re_formula = NA, type = "response", 
                    newdata = datagrid(model = m3, water_contact3 = unique)) |>  posteriordraws()

ggplot(pred, aes(x = draw, fill = water_contact3)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 0.1)

comparisons(m3, re_formula = NA, variables = "water_contact3",
            newdata = datagrid(model = m3)) 

mfx <- comparisons(m3, re_formula = NA, variables = "water_contact3", 
                   newdata = datagrid(model = m3)) |> posteriordraws()

ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact Level on AGI Incidence", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  

mfx <- comparisons(m2, re_formula = NA, type = "link", variables = "water_contact3", 
                   newdata = datagrid(model = m2)) |> posteriordraws()

ggplot(mfx, aes(x = exp(draw), fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Odds Ratios of Water Contact Level vs. No Contact", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  


# Marginal effects - E. coli

quantile(data_follow$e_coli, na.rm = TRUE)
quantile(data_follow$e_coli_s, na.rm = TRUE)

pred <- predictions(m3, re_formula = NA, type = "response",
                    newdata = datagrid(model = m3, water_contact3 = unique,
                                       e_coli_s = seq(-0.6, 3, by = 0.3))) |> 
  posteriordraws()

pred <- pred |> mutate(e_coli = round(e_coli_s*sd(data_follow$e_coli) + mean(data_follow$e_coli)))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(100, 200, 300, 500, 1000))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(100, 200, 300, 500, 1000)) +
  facet_wrap(~ water_contact3) 

mfx <- slopes(m3, re_formula = NULL, type = "response", variable = "e_coli_s",
              newdata = datagrid(water_contact3 = unique,
                                 e_coli_s = c(0.04, 1.7))) |> 
  posteriordraws()

mfx <- mfx |> mutate(e_coli = round(e_coli_s*sd(data_follow$e_coli) + mean(data_follow$e_coli)))

ggplot(mfx, aes(x = draw, fill = factor(e_coli))) +
  stat_halfeye() +
  labs(x = "Marginal Effect of E. coli Geometric Mean Values",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_wrap(~ contact) 

slopes(m3, re_formula = NULL, type = "response", variable = "e_coli_s",
       newdata = datagrid(water_contact3 = unique,
                          e_coli_s = c(0.06, 1.59)))





