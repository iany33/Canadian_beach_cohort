
### Bayesian analysis of beach cohort outcome data

pacman::p_load(
  Matrix,
  tidyverse,  
  rstatix,
  janitor,
  brms,
  tidybayes, 
  bayesplot,
  marginaleffects
)

# Start with AGI outcome, first build model with no confounders then add confounders
# Clustering by date and by household

get_prior(agi3 ~ water_contact2 + (1 | house_id/date), 
          family = bernoulli, data = data_follow)

priors <- set_prior("normal(0, 1)", class = "b")

m1 <- brm(agi3 ~ water_contact2 + (1 | house_id/date),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m1)
get_variables(m1)
plot(m1)
pp_check(m1, ndraws=100)
pp_check(m1, type = "stat", stat = "mean")
pp_check(m1, type = "loo_pit")
mcmc_acf(m1, pars = vars(contains("b_")), lags = 10)
mcmc_pairs(m1, pars = vars(contains("b_")), diag_fun = "den", off_diag_fun = "hex")

# Add minimal adjustment set of confounders

m2 <- brm(agi3 ~ water_contact2 + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + beach + e_coli_s + (1 | house_id/date),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2)
get_variables(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")
pp_check(m2, type = "loo_pit")

conditional_effects(m2)

# Add interaction with E. coli levels

m3 <- brm(agi3 ~ water_contact2*e_coli_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + beach + (1 | house_id/date),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m3)
get_variables(m3)
plot(m3)
pp_check(m3, ndraws=100)
pp_check(m3, type = "stat", stat = "mean")
pp_check(m3, type = "loo_pit")

conditional_effects(m3)

plot_cap(m3, re_formula=NA, condition = c("e_coli_s", "water_contact2"))

loo(m2, m3)

# Compare to model with human MST marker instead of E. coli as predictor

m3.2 <- brm(agi3 ~ water_contact2*mst_human_s + age1 + gender + education2 + ethnicity + cond_GI + 
            prev_act1 + beach_exp_food + sand_contact + beach + (1 | house_id/date),
          family = bernoulli, data = data_follow, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")),
          control = list(adapt_delta = 0.95))

summary(m3.2)
get_variables(m3.2)
plot(m3.2)
pp_check(m3.2, ndraws=100)
pp_check(m3.2, type = "stat", stat = "mean")
pp_check(m3.2, type = "loo_pit")

conditional_effects(m3.2)

plot_cap(m3.2, re_formula=NA, condition = c("mst_human_s", "water_contact2"))

loo(m2, m3, m3.2)



### Examine marginal effects

avg_comparisons(m2, variables = "water_contact2", transform_pre = "lnoravg", transform_post = "exp")

predictions(m2, re_formula = NA, type = "response", 
            newdata = datagrid(model = m2, water_contact2 = unique))

pred <- predictions(m2, re_formula = NA, type = "response", 
                    newdata = datagrid(model = m2, water_contact2 = unique)) |>  posteriordraws()

ggplot(pred, aes(x = draw, fill = water_contact2)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "bottom")  

comparisons(m2, variables = "water_contact2", newdata = datagrid(model = m2)) 

mfx <- comparisons(m2, type = "response", variables = "water_contact2", 
                   newdata = datagrid(model = m2)) |> posteriordraws()

ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact Level on AGI Incidence", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  

mfx <- comparisons(m2, type = "link", variables = "water_contact2", 
                   newdata = datagrid(model = m2)) |> posteriordraws()

ggplot(mfx, aes(x = exp(draw), fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Odds Ratios of Water Contact Level vs. No Contact", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  


# Marginal effects - E. coli

quantile(data_follow$e_coli, na.rm = TRUE)
quantile(data_follow$e_coli_s, na.rm = TRUE)

pred <- predictions(m2, re_formula = NA, type = "response",
                    newdata = datagrid(model = m2, water_contact2 = unique,
                                       e_coli_s = seq(-0.6, 3.9, by = 0.3))) |> 
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
  facet_wrap(~ water_contact2) 

mfx <- slopes(m2, re_formula = NULL, type = "response", variable = "e_coli_s",
              newdata = datagrid(water_contact2 = unique,
                                 e_coli_s = c(0.04, 1.7))) |> 
  posteriordraws()

mfx <- mfx |> mutate(e_coli = round(e_coli_s*sd(sim$e_coli) + mean(sim$e_coli)))

ggplot(mfx, aes(x = draw, fill = factor(e_coli))) +
  stat_halfeye() +
  labs(x = "Marginal Effect of E. coli Geometric Mean Values",
       y = "Posterior Density",
       fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_wrap(~ contact) 

slopes(m2, re_formula = NULL, type = "response", variable = "e_coli_s",
       newdata = datagrid(water_contact2 = unique,
                          e_coli_s = c(0.06, 1.59)))





