
### Bayesian analysis of beach cohort survey data - AGI outcome

pacman::p_load(
  Matrix,
  tidyverse,  
  rstatix,
  janitor,
  brms,
  tidybayes, 
  bayesplot,
  marginaleffects,
  cmdstanr,
  modelr
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
  date = as.Date(date, "%Y-%m-%d"),
  prev_act1 = as.character(prev_act1),
  water_contact = as.character(water_contact),
  sand1 = as.character(sand1),
  sand_mouth1 = as.character(sand_mouth1), 
  misswork = as.character(misswork),
  blood_stool1 = as.character(blood_stool1),
  stool_test1 = as.character(stool_test1),
  healthcare1 = as.character(healthcare1),
  emergency = as.character(emergency),
  hospital = as.character(hospital))

data_TO <- import(here("Datasets", "Toronto", "data_TO.csv"))

data_TO <- data_TO |> mutate(
  date = as.Date(date, "%Y-%m-%d"))

data_TO <- data_TO |> mutate(
  misswork_days = as.character(misswork_days))

data <- full_join(data, data_TO)

data_follow <- data |> filter(follow == "Yes") 

# Start with AGI outcome, first build model with no confounders then add confounders
# Varying-effects for date, household, beach (hierarchical) - ignore site as 4th level for now
# Use weakly informative priors for water contact; standard 0,1 Normal prior for other covariates

get_prior(agi3 ~ water_contact2 + (1 | beach/recruit_date/house_id), 
          family = bernoulli, data = data_follow)

priors <- c(set_prior("normal(0.3, 0.6)", class = "b", coef = "water_contact2Minimalcontact"),
            set_prior("normal(0.5, 0.5)", class = "b", coef = "water_contact2Bodyimmersion"),
            set_prior("normal(0.7, 0.4)", class = "b", coef = "water_contact2Swallowedwater"))

m1 <- brm(agi3 ~ water_contact2 + (1 | beach/recruit_date/house_id),
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

get_prior(agi3 ~ mo(water_contact3) + (1 | beach/recruit_date/house_id), 
          family = bernoulli, data = data_follow)

# Dirichlet prior for the monotonic effect
# Prior expectation of dose-response relationship - first examination probability distribution of prior

dirichlet <- brms::rdirichlet(n = 1000, alpha = c(1, 2, 3)) |> 
  data.frame() |> 
  mutate(draw = 1:n()) 

dirichlet |> 
  pivot_longer(-draw, names_to = "level", values_to = "proportion") |> 
  group_by(level) |> 
  summarize(avg_prop = mean(proportion))

# Set priors and run model

priors2 <- c(set_prior("normal(0,1)",class= "b"),
             set_prior("dirichlet(c(1, 2, 3))", class = "simo", coef = "mowater_contact31"))

m1.2 <- brm(agi3 ~ mo(water_contact3) + (1 | beach/recruit_date/house_id),
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

# Monotonic version fits better

# Add minimal adjustment set of confounders

m2 <- brm(agi3 ~ mo(water_contact3) + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + e_coli_s + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.9),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2)
get_variables(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")

conditional_effects(m2, effects = "e_coli_s")
conditional_effects(m2, effects = "water_contact3")

# Add interaction with E. coli levels

m3 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3)
get_variables(m3)
plot(m3)
pp_check(m3, ndraws=100)
pp_check(m3, type = "stat", stat = "mean")

conditional_effects(m2, effects = "e_coli_s:water_contact3")

plot_predictions(m3, re_formula=NA, condition = c("e_coli_s", "water_contact3"))

loo(m2, m3)

# Compare to model with highest single sample E. coli result instead of geometric mean

m3.2 <- brm(agi3 ~ mo(water_contact3)*e_coli_max_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3.2)
get_variables(m3.2)
plot(m3.2)
pp_check(m3.2, ndraws=100)
pp_check(m3.2, type = "stat", stat = "mean")

conditional_effects(m3.2, effects = "e_coli_max_s:water_contact3")


loo(m2, m3, m3.2)

# Compare to model with swallowed water only as the predictor to simplify

get_prior(agi3 ~ water_exp_mouth*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
              prev_act1 + beach_exp_food + sand_contact + (1 | beach/recruit_date/house_id),
            family = bernoulli, data = data_follow)

priors3 <- c(set_prior("normal(0.7, 0.4)", class = "b", coef = "water_exp_mouthYes"),
             set_prior("normal(0, 1)",class= "b"))

m3.3 <- brm(agi3 ~ water_exp_mouth*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
              prev_act1 + beach_exp_food + sand_contact + (1 | beach/recruit_date/house_id),
            family = bernoulli, data = data_follow, prior = priors3,
            iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
            backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3.3)
get_variables(m3.3)
plot(m3.3)
pp_check(m3.3, ndraws=100)
pp_check(m3.3, type = "stat", stat = "mean")

conditional_effects(m3.3, effects = "e_coli_s:water_exp_mouth")

loo(m2, m3, m3.3)

### Compare to model with varying slopes for E. coli

get_prior(agi3 ~ mo(water_contact3)*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
             prev_act1 + beach_exp_food + sand_contact + 
             (1 + e_coli_s | beach/recruit_date/house_id),
           family = bernoulli, data = data_follow)

priors4 <- c(set_prior("normal(0,1)",class = "b"),
           set_prior("dirichlet(c(1, 2, 3))", class = "simo", coef = "mowater_contact31"),
           set_prior("lkj(2)", class = "cor"))

m4 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + 
            (1 + e_coli_s | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors4,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m4)
get_variables(m4)
plot(m4)
pp_check(m4, ndraws=100)
pp_check(m4, type = "stat", stat = "mean")

conditional_effects(m2, effects = "e_coli_s:water_contact3")

plot_predictions(m3, re_formula=NA, condition = c("e_coli_s", "water_contact3"))

loo(m2, m3, m4)


### Conditional and marginal effects with 'marginaleffects' R package
### Examine posterior predictions of water contact exposure (predicted probabilities)
# Predictions ignore cluster-level variables (re_formula = NA)

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = mean(e_coli_s, na.rm=TRUE), age1 = "15-19", gender = "woman/girl", 
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", prev_act1 = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No") 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "No contact", "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

predictions(m3, re_formula = NA, type = "response", newdata = nd)

pred <- predictions(m3, re_formula = NA, type = "response", newdata = nd) |> posterior_draws()

pred <- pred |> mutate(draw = draw*1000)

ggplot(pred, aes(x = draw, y = water_contact3, fill = water_contact3)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence per 1000 Beachgoers", y = "Level of Water Contact",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 10)

# Examine marginal effects/contrast of water contact exposure effect - probability scale

avg_comparisons(m3, re_formula = NA, variables = "water_contact3", newdata = nd)

mfx <- comparisons(m3, re_formula = NA, variables = "water_contact3", 
                   newdata = nd) |> posteriordraws()

mfx <- mfx |> mutate(draw = draw*1000)

ggplot(mfx, aes(x = draw, y = contrast, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact on AGI Incidence per 1000 Beachgoers", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(-0.5, 1) 

# Odds ratio scale

avg_comparisons(m3, re_formula = NA, variables = "water_contact3", newdata = nd,
                comparison = "lnoravg", transform = "exp")

mfx <- comparisons(m3, re_formula = NA, type = "link", variables = "water_contact3", 
                   newdata = nd) |> posteriordraws()

ggplot(mfx, aes(x = exp(draw), y = contrast, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Odds Ratios of Water Contact Level vs. No Contact", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-1, 20)

### Predicted probabilities of E. coli, conditional on water contact level

quantile(data_follow$e_coli, na.rm = TRUE)
quantile(data_follow$e_coli_s, na.rm = TRUE)

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = seq(-0.6, 3.9, by = 0.3), age1 = "15-19", gender = "woman/girl", 
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", prev_act1 = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No") 

pred <- predictions(m3, re_formula = NA, type = "response", newdata = nd) |> 
  posteriordraws()

pred <- pred |> mutate(e_coli = round(e_coli_s*sd(data_follow$e_coli, na.rm=TRUE) + mean(data_follow$e_coli, na.rm=TRUE)))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(100, 500, 1000, 1500))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(100, 500, 1000, 1500)) +
  facet_wrap(~ water_contact3) 

### Marginal effects of E. coli, conditional on water contact, at specific cut-points

quantile(data_follow$e_coli, probs = c(0.5, 0.6, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)
quantile(data_follow$e_coli_s, probs = c(0.5, 0.6, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)

# Cut-points of 75th & 95th percentile

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = c(0.02, 3.2), age1 = "15-19", gender = "woman/girl", 
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", prev_act1 = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No") 

mfx <- slopes(m3, re_formula = NA, type = "response", variable = "e_coli_s",
              newdata = nd) |> 
  posteriordraws()

mfx <- mfx |> mutate(e_coli = round(e_coli_s*sd(data_follow$e_coli, na.rm=TRUE) + mean(data_follow$e_coli, na.rm=TRUE)))

ggplot(mfx, aes(x = draw, y = water_contact3, fill = factor(e_coli))) +
  stat_halfeye() +
  labs(x = "Marginal Effect of E. coli Geometric Mean Values",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  xlim(0, 5)

### Beach-specific posterior probabilities 
# Note that predictions may take some time to compute with random-effects accounted for

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = mean(e_coli_s, na.rm=TRUE), age1 = "15-19", gender = "woman/girl", 
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", prev_act1 = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No", 
            beach = c("English Bay Beach", "Kitsilano Beach", "Grand Beach West","Grand Beach East")) 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "No contact", "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

pred <- predictions(m3, re_formula = ~ (1 | beach), type = "response", newdata = nd) |> posterior_draws()

pred <- pred |> mutate(draw = draw*1000)

ggplot(pred, aes(x = draw, y = beach, fill = water_contact3)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence per 1000 Beachgoers", y = "Level of Water Contact",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  xlim(0, 10)

ggplot(pred, aes(x = draw, y = water_contact3, fill = water_contact3)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence per 1000 Beachgoers", y = "Level of Water Contact",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 10) +
  facet_wrap(~ beach)







