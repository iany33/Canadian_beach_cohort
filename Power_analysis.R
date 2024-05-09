
pacman::p_load(brms,
               Matrix,
               tidyverse
)

# Simulate priors for water contact level

set.seed(123)
x <- tibble(minimal = rnorm(n = 1000, mean = 0.3, sd = 0.6),
            immersion = rnorm(n = 1000, mean = 0.4, sd = 0.5),
            swallow = rnorm(n = 1000, mean = 0.6, sd = 0.4))

x |> ggplot(aes(x = minimal)) + 
  geom_density(colour = "steelblue") +
  geom_density(aes(x = immersion)) + 
  geom_density(aes(x = swallow), colour = "maroon") +
  theme_minimal()
               
# Bayes power analysis

n <- 5000
n1 <- 0.40*n # No water contact
n2 <- 0.15*n # Minimal contact 
n3 <- 0.35*n # Body immersion without swallowing water
n4 <- 0.10*n # Swallowing water

p1 <- 0.03
p2 <- 0.04
p3 <- 0.05
p4 <- 0.06

d <-
  tibble(contact = rep(c(0,1,2,3), c(n1, n2, n3, n4))) |> 
  mutate(agi = case_when(
    contact == 0 ~ rbinom(n = n, size = 1, p = p1),
    contact == 1 ~ rbinom(n = n, size = 1, p = p2),
    contact == 2 ~ rbinom(n = n, size = 1, p = p3),
    contact == 3 ~ rbinom(n = n, size = 1, p = p4)))  |> 
  mutate(contact = as.factor(contact))

d |>
  ggplot(aes(x = agi, fill = contact)) +
  geom_bar()

get_prior(agi ~ 0 + Intercept + contact, family = bernoulli, data = d)

fit1 <-
  brm(data = d,
      family = bernoulli(),
      agi ~ 0 + Intercept + contact,
      prior = c(prior(normal(0, 1), class = "b", coef = "Intercept"), 
                prior(normal(0.3, 0.6), class = "b", coef = "contact1"),
                prior(normal(0.4, 0.5), class = "b", coef = "contact2"),
                prior(normal(0.6, 0.4), class = "b", coef = "contact3")),
      iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123,
      backend = "cmdstanr", 
      stan_model_args = list(stanc_options = list("O1")))

plot(fit1)

fit_stats <- fit1 |> fixef() |> 
  data.frame() |> 
  rownames_to_column("parameter")

hyp1 <- hypothesis(fit1, "contact1 > 0", alpha = 0.025)
hyp2 <- hypothesis(fit1, "contact2 > 0", alpha = 0.025)
hyp3 <- hypothesis(fit1, "contact3 > 0", alpha = 0.025)
hyp <- tibble(PPS = c(NA, hyp1$hypothesis$Post.Prob, hyp2$hypothesis$Post.Prob, 
                hyp3$hypothesis$Post.Prob))

fit_stats <- cbind(fit_stats, hyp)
  

# Simulation function

sim_data_fit <- function(seed, n) {
  
  # define probabilities in the function
  p1 <- 0.03
  p2 <- 0.04
  p3 <- 0.05
  p4 <- 0.06
  
  n1 <- 0.40*n
  n2 <- 0.15*n
  n3 <- 0.35*n
  n4 <- 0.10*n
  
  # make results reproducible
  set.seed(seed)
  
  # simulate the data
  d <-
    tibble(contact = rep(c(0,1,2,3), c(n1, n2, n3, n4))) |> 
    mutate(agi = case_when(
      contact == 0 ~ rbinom(n = n, size = 1, p = p1),
      contact == 1 ~ rbinom(n = n, size = 1, p = p2),
      contact == 2 ~ rbinom(n = n, size = 1, p = p3),
      contact == 3 ~ rbinom(n = n, size = 1, p = p4)))  |> 
    mutate(contact = as.factor(contact))  
  
  # fit and summarize
  fit <-
    update(fit1, 
           newdata = d,
           seed = seed)
  
  fit_stats <- fit |> fixef() |> 
    data.frame() |> 
    rownames_to_column("parameter")
  
  hyp1 <- hypothesis(fit, "contact1 > 0", alpha = 0.025)
  hyp2 <- hypothesis(fit, "contact2 > 0", alpha = 0.025)
  hyp3 <- hypothesis(fit, "contact3 > 0", alpha = 0.025)
  hyp <- tibble(PPS = c(NA, hyp1$hypothesis$Post.Prob, hyp2$hypothesis$Post.Prob, 
                        hyp3$hypothesis$Post.Prob))
  
  cbind(fit_stats, hyp)
}

# Check simulations with projected sample size of 5000, assume 20% dropout

n_sim <- 500

sim <-
  tibble(seed = 1:n_sim) |> 
  mutate(b1 = map(seed, sim_data_fit, n = 4000)) |> 
  unnest(b1)

sim <- sim |> mutate(parameter = case_when(
  parameter == "Intercept" ~ "Intercept",
  parameter == "contact1" ~ "Minimal contact",
  parameter == "contact2"  ~ "Body immersion",
  parameter == "contact3"  ~ "Swallowed water")) |> 
  mutate(parameter = as.factor(parameter)) |> 
  mutate(parameter = fct_relevel(parameter, "Body immersion", after = 2)) 

sim |> 
  filter(parameter != "Intercept")  |> 
  mutate(width = Q97.5 - Q2.5) |> 
  mutate(width_check = ifelse(width <1, 1, 0)) |> 
  mutate(PPS_check = ifelse(PPS >0.95, 1, 0)) |> 
  group_by(parameter) |> 
  summarize(avg_CI_width = mean(width),
            width_below_1 = mean(width_check),
            avg_PPS = mean(PPS),
            PPS_above_95 = mean(PPS_check))

sim |> 
  filter(parameter != "Intercept")  |> 
  ggplot(aes(x = seed, y = Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(fatten = 1/2) +
  labs(x = "Simulation number",
       y = "Beta value (log odds)") +
  facet_wrap(~parameter, ncol = 1)

sim |> 
  filter(parameter != "Intercept")  |> 
  mutate(width = Q97.5 - Q2.5) |> 
  ggplot(aes(x = width)) +
  geom_histogram() +
  labs(x = "95% credible interval width",
       y = "Number of simulations") +
  facet_wrap(~parameter, ncol = 1, scale = "free")

sim |> 
  filter(parameter != "Intercept")  |> 
  ggplot(aes(x = PPS)) +
  geom_histogram() +
  labs(x = "Posterior probability of success",
       y = "Number of simulations") +
  facet_wrap(~parameter, ncol = 1, scale = "free")







