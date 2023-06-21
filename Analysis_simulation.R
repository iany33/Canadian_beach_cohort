
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

# Simulate dataset for beach cohort study

beaches <- 10
n <- 500*beaches
n1 <- 0.40*n
n2 <- 0.15*n
n3 <- 0.35*n
n4 <- 0.10*n

days <- 35

set.seed(10)
sim <-
  tibble(id = 1:n,
         beach = rep(1:beaches, times = 1000), 
         contact = rep(c(1:4), c(n1, n2, n3, n4)) |> as.factor() )

sim <- sim |> arrange(beach) |> mutate(day = sample.int(35, n(), replace = TRUE))

sim2 <- sim |> group_by(day) |> summarize(n = n()) |> mutate(e_coli = rnorm(days, 100, 75)) |>  
  mutate(e_coli = pmax(e_coli, 10)) |> ungroup() |> select(day, e_coli)

sim <- left_join(sim, sim2, by = "day")
remove(sim2)

quantile(sim$e_coli, probs = c(0, .25, .50, .75, .95), na.rm = TRUE)
skimr::skim(sim)

sim <- sim |> 
  mutate(agi = case_when(
    contact == 1 ~ rbinom(n = n, size = 1, p = 0.03),
    contact == 2 ~ rbinom(n = n, size = 1, p = 0.04),
    (contact == 3 & e_coli <100) ~ rbinom(n = n, size = 1, p = 0.05),
    (contact == 3 & e_coli >=100) ~ rbinom(n = n, size = 1, p = 0.08),
    (contact == 4 & e_coli <100) ~ rbinom(n = n, size = 1, p = 0.06),
    (contact == 4 & e_coli >=100) ~ rbinom(n = n, size = 1, p = 0.1)))

sim |> group_by(contact) |> summarize(proportion = mean(agi), e_coli = mean(e_coli))

sim |> ggplot(aes(x = contact, y = e_coli, fill = contact)) +
  geom_violin()

sim |> ggplot(aes(x = e_coli)) + geom_histogram()
     
sim <- sim |> mutate(contact = factor(contact,
                                      levels = c(1:4),
                                      labels = c("No contact", "Minimal contact", "Body immersion", "Swallowed water"),
                                      ordered = TRUE))

sim <- sim |> mutate(e_coli_s = (e_coli - mean(e_coli)) / sd(e_coli))


# Run Bayesian analysis on mock dataset

get_prior(agi ~ contact*e_coli_s + (1 | day/beach), 
          family = bernoulli, data = sim)

priors <- c(set_prior("normal(0.3, 0.6)", class = "b", coef = "contact.C"),
            set_prior("normal(0.5, 0.5)", class = "b", coef = "contact.L"),
            set_prior("normal(0.7, 0.4)", class = "b", coef = "contact.Q"),
            set_prior("normal(0, 1)", class = "b"))
  
m1 <- brm(agi ~ contact*e_coli_s + (1 | day/beach),
           family = bernoulli, data = sim, prior = priors,
           iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 5, 
           backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m1)
get_variables(m1)
plot(m1)
pp_check(m1, ndraws=100)
pp_check(m1, type = "stat", stat = "mean")
pp_check(m1, type = "loo_pit")
mcmc_acf(m1, pars = vars(contains("b_")), lags = 10)
mcmc_pairs(m1, pars = vars(contains("b_")), diag_fun = "den", off_diag_fun = "hex")

conditional_effects(m1)

plot_cap(m1, re_formula=NULL, condition = c("e_coli_s", "contact"))

# Run alternative model that models water contact as monotonic ordinal variable

get_prior(agi ~ mo(contact)*e_coli_s + (1 | day/beach), 
          family = bernoulli, data = sim)

x <- rdirichlet(100, c(2, 2, 2))
x |> as_tibble() |> pivot_longer(everything(), names_to = "level", values_to = "value") |> 
  ggplot(aes(y = value, x = as.factor(level), fill = as.factor(level))) + 
  scale_colour_viridis_b() +
  geom_violin() + geom_jitter() + 
  theme(legend.position = "none") 

priors2 <- c(set_prior("dirichlet(c(2, 2, 2))", class = "simo", coef = "mocontact1"),
            set_prior("normal(0, 1)", class = "b"))

m2 <- brm(agi ~ mo(contact)*e_coli_s + (1 | day/beach),
          family = bernoulli, data = sim, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 5, 
          backend = "cmdstanr", stan_model_args = list(stanc_options = list("O1")))

summary(m2)
get_variables(m2)
plot(m2)
plot(m2, variable = "simo", regex = TRUE)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")
pp_check(m2, type = "loo_pit")
mcmc_acf(m2, pars = vars(contains("b_")), lags = 10)
mcmc_pairs(m2, pars = vars(contains("b_")), diag_fun = "den", off_diag_fun = "hex")

conditional_effects(m2)

plot_cap(m2, re_formula=NULL, condition = c("e_coli_s", "contact"))

loo(m1, m2)


# Marginal effects - water contact

avg_comparisons(m2, variables = "contact", transform_pre = "lnoravg", transform_post = "exp")

predictions(m2, re_formula = NULL, type = "response", 
            newdata = datagrid(model = m2, contact = unique))

pred <- predictions(m2, re_formula = NULL, type = "response", 
                    newdata = datagrid(model = m2, contact = unique)) |>  posteriordraws()

ggplot(pred, aes(x = draw, fill = contact)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted AGI Incidence", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Water contact") +
  theme_minimal() +
  theme(legend.position = "bottom")  

comparisons(m2, variables = "contact", newdata = datagrid(model = m2)) 

mfx <- comparisons(m2, type = "response", variables = "contact", 
            newdata = datagrid(model = m2)) |> posteriordraws()

ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Effect of Water Contact Level on AGI Incidence", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  

mfx <- comparisons(m2, type = "link", variables = "contact", 
                   newdata = datagrid(model = m2)) |> posteriordraws()

ggplot(mfx, aes(x = exp(draw), fill = contrast)) +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Odds Ratios of Water Contact Level vs. No Contact", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")  


# Marginal effects - E. coli

quantile(sim$e_coli, na.rm = TRUE)
quantile(sim$e_coli_s, na.rm = TRUE)

pred <- predictions(m2, re_formula = NULL, type = "response",
                    newdata = datagrid(model = m2, contact = unique,
                                       e_coli_s = seq(-1.5, 2.5, by = 0.2))) |> 
  posteriordraws()

pred <- pred |> mutate(e_coli = round(e_coli_s*sd(sim$e_coli) + mean(sim$e_coli)))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250, 300))

ggplot(pred, aes(x = e_coli, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "E. coli Geometric Mean",
       y = "Predicted Probability of AGI",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(50, 100, 150, 200, 250, 300)) +
  facet_wrap(~ contact) 

mfx <- slopes(m2, re_formula = NULL, type = "response", variable = "e_coli_s",
                    newdata = datagrid(contact = unique,
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
       newdata = datagrid(contact = unique,
                          e_coli_s = c(0.06, 1.59)))







