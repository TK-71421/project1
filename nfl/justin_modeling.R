library(brms)
library(tidyverse)

options(brms.backend = "cmdstanr") 

data_all_past$Tm = factor(data_all_past$Tm) 
data_all_past$W = as.integer(data_all_past$W) 
data_all_past$L = as.integer(data_all_past$L) 
data_all_past$G = data_all_past$W + data_all_past$L 

brm_fit = brm(
  bf(W | trials(G) ~ past_1 + (1 | Tm)), 
  family = beta_binomial(link = "logit", link_phi = "log"), 
  data = data_all_past, 
  prior = prior(exponential(1), class = phi),
  chains = 2, 
  cores = 2, 
  warmup = 1000, 
  iter = 2000
) 

summary(brm_fit) # model summary 
ranef(brm_fit) # random effect component 

# posterior predictive check
# seems good enough 
pp_check(brm_fit, ndraws=200) 

# add posterior probs 
posterior = posterior_linpred(brm_fit, transform = TRUE)
data_all_past$posterior_p = colMeans(posterior)
hist(data_all_past$posterior_p) 
hist(data_all_past$win_pct)
