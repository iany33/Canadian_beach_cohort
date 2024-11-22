
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

# Start with AGI outcome, first build model with no confounders then add confounders
# Varying-effects for date, household, beach (hierarchical) - ignore site as 4th level for now
# Use weakly informative priors for water contact; standard 0,1 Normal prior for other covariates

get_prior(agi3 ~ water_contact2 + (1 | beach/recruit_date/house_id), 
          family = bernoulli, data = data_follow)

priors <- c(set_prior("normal(0.3, 0.6)", class = "b", coef = "water_contact2Minimalcontact"),
            set_prior("normal(0.5, 0.5)", class = "b", coef = "water_contact2Bodyimmersion"),
            set_prior("normal(0.7, 0.4)", class = "b", coef = "water_contact2Swallowedwater"),
            set_prior("exponential(1)", class = "sd"))

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
             set_prior("exponential(1)", class = "sd"),
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

### Add E. coli variable with interaction

m2 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2)
get_variables(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")

conditional_effects(m2, effects = "e_coli_s:water_contact3")
conditional_effects(m2, effects = "water_contact3")

loo(m2)

# Compare to model with no interaction

m2.1 <- brm(agi3 ~ mo(water_contact3) + e_coli_s + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2.1)
get_variables(m2.1)
plot(m2.1)
pp_check(m2.1, ndraws=100)
pp_check(m2.1, type = "stat", stat = "mean")

conditional_effects(m2.1, effects = "e_coli_s:water_contact3")
conditional_effects(m2.1, effects = "water_contact3")

loo(m2, m2.1)

# Interaction model has better fit, also fits with DAG and is part of research question/hypothesis

### Add minimal adjustment set of confounders

data_follow$ethnicity <- C(data_follow$ethnicity, contr.treatment, base=9)

m3 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + age4 + gender + education2 + ethnicity + cond_GI + 
            other_rec_act + beach_exp_food + sand_contact + (1 | beach/recruit_date/house_id),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.99),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3)
get_variables(m3)
plot(m3)
pp_check(m3, ndraws=100)
pp_check(m3, type = "stat", stat = "mean")

conditional_effects(m3, effects = "e_coli_s:water_contact3")
conditional_effects(m3, effects = "water_contact3") -> fit
fit$water_contact3

loo(m3)

# Given very low posterior predictions and high Pareto K values due to ~50% of households having n=1
# Re-run model without the household cluster and instead include indicator for household size >1

m4 <- brm(agi3 ~ mo(water_contact3)*e_coli_s + age4 + gender + education2 + ethnicity + cond_GI + 
            other_rec_act + beach_exp_food + sand_contact + household_group +
            (1 | beach/recruit_date),
          family = bernoulli, data = data_follow, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m4, robust = TRUE)
get_variables(m4)
plot(m4)
pp_check(m4, ndraws=100)
pp_check(m4, type = "stat", stat = "mean")

conditional_effects(m4, effects = "e_coli_s:water_contact3")
conditional_effects(m4, effects = "water_contact3") -> fit
fit$water_contact3

loo(m3, m4)

# Compare again to using simpler model with no monotonic effects for water contact

priors3 <- c(set_prior("normal(0.3, 0.6)", class = "b", coef = "water_contact2Minimalcontact"),
             set_prior("normal(0.5, 0.5)", class = "b", coef = "water_contact2Bodyimmersion"),
             set_prior("normal(0.7, 0.4)", class = "b", coef = "water_contact2Swallowedwater"),
             set_prior("normal(0, 1)", class = "b"),
             set_prior("exponential(1)", class = "sd"))

m4.1 <- brm(agi3 ~ water_contact2*e_coli_s + age4 + gender + education2 + ethnicity + cond_GI + 
              other_rec_act + beach_exp_food + sand_contact + household_group +
              (1 | beach/recruit_date),
            family = bernoulli, data = data_follow, prior = priors3,
            iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.9),
            backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

conditional_effects(m4.1, effects = "e_coli_s:water_contact2")
conditional_effects(m4.1, effects = "water_contact2")

loo(m4, m4.1)

# Monotonic predictor still fits better

# Compare to # Compare to model with highest single sample E. coli 

m4.2 <- brm(agi3 ~ mo(water_contact3)*e_coli_max_s + age4 + gender + education2 + ethnicity + cond_GI + 
              other_rec_act + beach_exp_food + sand_contact + household_group + 
              (1 | beach/recruit_date),
            family = bernoulli, data = data_follow, prior = priors2,
            iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
            backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m4.2)
get_variables(m4.2)
plot(m4.2)
pp_check(m4.2, ndraws=100)
pp_check(m4.2, type = "stat", stat = "mean")

conditional_effects(m4.2, effects = "e_coli_max_s:water_contact3")
conditional_effects(m4.2, effects = "water_contact3")

loo(m4, m4.2)

# Compare to model with turbidity instead of E. coli

m4.3 <- brm(agi3 ~ mo(water_contact3)*turbidity_s + age4 + gender + education2 + ethnicity + cond_GI + 
              other_rec_act + beach_exp_food + sand_contact + household_group +
              (1 | beach/recruit_date),
            family = bernoulli, data = data_follow, prior = priors2,
            iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, control = list(adapt_delta = 0.95),
            backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m4.3)
get_variables(m4.3)
plot(m4.3)
pp_check(m4.3, ndraws=100)
pp_check(m4.3, type = "stat", stat = "mean")

conditional_effects(m4.3, effects = "turbidity_s:water_contact3")

loo(m4, m4.2, m4.3)



### Conditional and marginal effects with 'marginaleffects' R package
### Examine posterior predictions of water contact exposure (predicted probabilities)
# Predictions ignore cluster-level variables (re_formula = NA) to get overall averages

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = mean(e_coli_s, na.rm=TRUE), 
            age4 = c("0-4", "5-9", "10-14", "15-19", "20+"),
            gender = c("woman/girl", "man/boy", "fluid/trans"),
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", other_rec_act = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No", household_group = 0) 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "No contact", "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

predictions(m4, re_formula = NA, by = "water_contact3", type = "response", newdata = nd)

pred <- predictions(m4, re_formula = NA, by = "water_contact3", type = "response", newdata = nd) |> posterior_draws()

pred <- pred |> mutate(draw = draw*1000)

ggplot(pred, aes(x = draw, y = water_contact3, fill = water_contact3)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incident Risk per 1000 Beachgoers", y = "Level of Water Contact",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 200)

# Examine marginal effects/contrast of water contact exposure effect - probability scale

avg_comparisons(m4, re_formula = NA, variables = "water_contact3", newdata = nd)

mfx <- comparisons(m4, re_formula = NA, variables = "water_contact3", by = "water_contact3", 
                   newdata = nd) |> posteriordraws()

mfx <- mfx |> mutate(draw = draw*1000)

mfx <- mfx |> 
  mutate(contrast = recode(contrast, "mean(Body immersion) - mean(No contact)" = "Body immersion",
                           "mean(Swallowed water) - mean(No contact)" = "Swallowed water",
                           "mean(Minimal contact) - mean(No contact)" = "Minimal contact")) |> 
  mutate(contrast = fct_relevel(contrast, "Body immersion", after = 1)) 

ggplot(mfx, aes(x = draw, y = contrast, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact on AGI Incident Risk per 1000 Beachgoers", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-0.5, 100) 

# Odds ratio scale

avg_comparisons(m4, re_formula = NA, variables = "water_contact3", newdata = nd,
                comparison = "lnoravg", transform = "exp")

mfx <- comparisons(m4, re_formula = NA, type = "link", variables = "water_contact3",   
                   by = "water_contact3", newdata = nd) |> posteriordraws()

mfx <- mfx |> 
  mutate(contrast = recode(contrast, "mean(Body immersion) - mean(No contact)" = "Body immersion",
                           "mean(Swallowed water) - mean(No contact)" = "Swallowed water",
                           "mean(Minimal contact) - mean(No contact)" = "Minimal contact")) |> 
  mutate(contrast = fct_relevel(contrast, "Body immersion", after = 1)) 

ggplot(mfx, aes(x = exp(draw), y = contrast, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(x = "Odds Ratios of Water Contact Level vs. No Contact", y="") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(0, 10)

### Gender specific estimates 

avg_comparisons(m4, re_formula = NA, variables = "water_contact3", newdata = nd, by = "gender")

mfx <- comparisons(m4, re_formula = NA, variables = "water_contact3", by = "gender",
                   newdata = nd) |> posteriordraws()

mfx <- mfx |> mutate(draw = draw*1000)

mfx <- mfx |> 
  mutate(contrast = recode(contrast, "mean(Body immersion) - mean(No contact)" = "Body immersion",
                           "mean(Swallowed water) - mean(No contact)" = "Swallowed water",
                           "mean(Minimal contact) - mean(No contact)" = "Minimal contact")) |> 
  mutate(contrast = fct_relevel(contrast, "Body immersion", after = 1)) |> 
  mutate(gender = recode(gender, "man/boy" = "Man/boy", "woman/girl" = "Woman/girl",
                         "fluid/trans" = "Fluid/trans"))

ggplot(mfx, aes(x = draw, y = gender, fill = gender)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact on AGI Incident Risk per 1000 Beachgoers", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-0.5, 100) +
  facet_wrap(~ contrast)

### Age specific estimates 

avg_comparisons(m4, re_formula = NA, variables = "water_contact3", newdata = nd, by = "age4")

mfx <- comparisons(m4, re_formula = NA, variables = "water_contact3", by = "age4",
                   newdata = nd) |> posteriordraws()

mfx <- mfx |> mutate(draw = draw*1000)

mfx <- mfx |> 
  mutate(contrast = recode(contrast, "mean(Body immersion) - mean(No contact)" = "Body immersion",
                           "mean(Swallowed water) - mean(No contact)" = "Swallowed water",
                           "mean(Minimal contact) - mean(No contact)" = "Minimal contact")) |> 
  mutate(contrast = fct_relevel(contrast, "Body immersion", after = 1)) |> 
  mutate(age4 = fct_relevel(age4, "0-4", "5-9", "10-14", "15-19", "20+"))

ggplot(mfx, aes(x = draw, y = age4, fill = age4)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact on AGI Incident Risk per 1000 Beachgoers", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-0.5, 50) +
  facet_wrap(~ contrast)

### Predicted probabilities of E. coli, conditional on water contact level
# Sequence E. coli by range of standardized and centered variable then back-transform

quantile(data_follow$e_coli, na.rm = TRUE)
quantile(data_follow$e_coli_s, na.rm = TRUE)

nd <- data_follow |> 
  data_grid(water_contact3 = c("Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = seq(-0.56, 4.40, by = 0.3), 
            age4 = c("0-4", "5-9", "10-14", "15-19", "20+"),
            gender = c("woman/girl", "man/boy", "fluid/trans"),
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", other_rec_act = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No", household_group = 0) 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "No contact", "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

pred <- predictions(m4, re_formula = NA, type = "response", newdata = nd) |> 
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


avg_slopes(m4, re_formula = NA, variables = "e_coli_s", newdata = nd)

avg_slopes(m4, re_formula = NA, variables = "e_coli_s", newdata = nd, by = "water_contact3")


### Marginal effects of E. coli, conditional on water contact, at specific cut-points

quantile(data_follow$e_coli, probs = c(0.5, 0.6, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)
quantile(data_follow$e_coli_s, probs = c(0.5, 0.6, 0.75, 0.9, 0.95, 0.99), na.rm = TRUE)

# Cut-points of 75th & 99th percentile

nd <- data_follow |> 
  data_grid(water_contact3 = c("Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = c(-0.08571, 3.213), 
            age4 = c("0-4", "5-9", "10-14", "15-19", "20+"),
            gender = c("woman/girl", "man/boy", "fluid/trans"),
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", other_rec_act = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No", household_group = 0) 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

mfx <- slopes(m4, re_formula = NA, type = "response", variable = "e_coli_s",
              newdata = nd) |> 
  posteriordraws()

mfx <- mfx |> mutate(e_coli = round(e_coli_s*sd(data_follow$e_coli, na.rm=TRUE) + mean(data_follow$e_coli, na.rm=TRUE)))

ggplot(mfx, aes(x = draw, y = water_contact3, fill = factor(e_coli))) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Marginal Effect of E. coli Geometric Mean Values",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "none") +
  xlim(-0.025, 0.05) +
  facet_wrap(~ factor(e_coli)) 


### Beach-specific posterior probabilities and contrasts

nd <- data_follow |> 
  data_grid(water_contact3 = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
            e_coli_s = mean(e_coli_s, na.rm=TRUE), 
            age4 = c("0-4", "5-9", "10-14", "15-19", "20+"),
            gender = c("woman/girl", "man/boy", "fluid/trans"),
            ethnicity = "White", education2 = "bachelors", cond_GI = "No", other_rec_act = "Yes", 
            beach_exp_food = "Yes", sand_contact = "No", household_group = 0,
            beach = c("English Bay Beach", "Kitsilano Beach", "Grand Beach West", "Grand Beach East",
                      "Sunnyside", "Marie Curtis")) 

nd <- nd |> mutate(water_contact3 = fct_relevel(water_contact3, "No contact", "Minimal contact", 
                                                "Body immersion", "Swallowed water")) 

pred <- predictions(m4, re_formula = ~ (1 | beach), type = "response", newdata = nd) |> posterior_draws()

pred <- pred |> mutate(draw = draw*1000)

ggplot(pred, aes(x = draw, y = beach, fill = beach)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incident Risk per 1000 Beachgoers", y = "Beach",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ water_contact3) +
  xlim(0, 300) 

avg_comparisons(m4, re_formula = ~ (1 | beach),
                variables = "water_contact3", newdata = nd, by = "beach")

mfx <- comparisons(m4, re_formula = ~ (1 | beach), variables = "water_contact3", by = "beach",
                   newdata = nd) |> posteriordraws()

mfx <- mfx |> mutate(draw = draw*1000)

mfx <- mfx |> 
  mutate(contrast = recode(contrast, "mean(Body immersion) - mean(No contact)" = "Body immersion",
                           "mean(Swallowed water) - mean(No contact)" = "Swallowed water",
                           "mean(Minimal contact) - mean(No contact)" = "Minimal contact")) |> 
  mutate(contrast = fct_relevel(contrast, "Body immersion", after = 1))

ggplot(mfx, aes(x = draw, y = beach, fill = beach)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact on AGI Incident Risk per 1000 Beachgoers", y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-0.5, 50) +
  facet_wrap(~ contrast)



