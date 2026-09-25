##--------------------------------------------------------------------------------------------------------
## SCRIPT : Graded Response Model
##
## Authors : Matthieu Authier, Sean Heighton
## Last update : 2025-05-26
##
## R version 4.2.2 (2022-10-31 ucrt) -- "Innocent and Trusting"
## Copyright (C) 2020 The R Foundation for Statistical Computing
## Platform: x86_64-w64-mingw32/x64 (64-bit)
##--------------------------------------------------------------------------------------------------------

what_u_need <- c("lubridate", "tidyverse", "rstan", "sn")
cran_packages <- what_u_need[!(what_u_need %in% installed.packages())]
if(length(cran_packages) != 0) {
  lapply(cran_packages, install.packages, dependencies = TRUE)
}
lapply(what_u_need, library, character.only = TRUE)

# rstan version 2.32.7 (Stan version 2.32.2)

# For execution on a local, multicore CPU with excess RAM we recommend calling
options(mc.cores = parallel::detectCores())
# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)


prior_scale_t <- function(nu = 4, sigma = 1) { sqrt((nu - 2)/nu) * sigma }
prior_scale_t()

### check that the std. dev is equal to 1
# rst(n = 1e5, xi = 0, omega = prior_scale_t(), nu = 4) %>%
#   sd()

### graded response model
## GRM 1 dimension
grm_nosimplex = "
data {
	int<lower = 1> n; // number of studies
	int<lower = 1> N; // number of target
	int<lower = 1> P; // number of pressure
	int<lower = 1> K; // number of categories
	int<lower = 1, upper = K> Y[n]; // score data
	int<lower = 1, upper = N> TARGET[n]; // target ID
	int<lower = 1, upper = P> PRESSURE[n]; // pressure ID
	real<lower = 0.0> prior_scale_kappa;
	real<lower = 0.0> prior_scale_theta;
}

parameters {
	ordered[K - 1] unscaled_kappa[P]; // thresholds for each pressure
	vector[N] unscaled_theta; // factor scores for each target
	real unscaled_mu; // mean of the prior distribution of category difficulty
	real<lower=0> sigma_kappa; // sd of the prior distribution of category difficulty
	real<lower=0> sigma_theta;
}

transformed parameters {
  real mu = unscaled_mu * log(5) / 2;
  vector[N] theta = unscaled_theta * sigma_theta; 
  vector[K - 1] kappa[P];
  for(p in 1:P){
		kappa[p] = mu + sigma_kappa * unscaled_kappa[p];
	}
}
model {
  // likelihood 
  for(i in 1:n) {
    Y[i] ~ ordered_logistic(theta[TARGET[i]], kappa[PRESSURE[i]]);
  }
  // priors
	for(p in 1:P){
		unscaled_kappa[p] ~ std_normal();
	}
	unscaled_mu ~ std_normal();
	unscaled_theta ~ std_normal();
	sigma_kappa ~ student_t(4, 0, prior_scale_kappa);
	sigma_theta ~ student_t(4, 0, prior_scale_theta);
}

generated quantities {
	vector[n] log_lik; //log likelihood
	int y_rep[n]; // posterior predictive checks
	for(i in 1:n){
		log_lik[i] = ordered_logistic_lpmf(Y[i] | theta[TARGET[i]], kappa[PRESSURE[i]]);
		y_rep[i] = ordered_logistic_rng(theta[TARGET[i]], kappa[PRESSURE[i]]);
	}
}
"

grm_null = "
data {
	int<lower = 1> n; // number of studies
	int<lower = 1> N; // number of target
	int<lower = 1> P; // number of pressure
	int<lower = 1> K; // number of categories
	int<lower = 1, upper = K> Y[n]; // score data
	int<lower = 1, upper = N> TARGET[n]; // target ID
	int<lower = 1, upper = P> PRESSURE[n]; // pressure ID
	real<lower = 0.0> prior_scale;
	//int<lower = 1> W[n]; // weights
}

parameters {
	ordered[K - 1] unscaled_kappa[P]; // thresholds for each pressure
	vector[N] unscaled_theta; // factor scores for each target
	real unscaled_mu; // mean of the prior distribution of category difficulty
	simplex[2] prop;
	real<lower = 0.0> psi;
	real<lower = 0.0> tau;
}

transformed parameters {
  real mu = unscaled_mu * log(5) / 2;
  real sigma = prior_scale * sqrt(psi / tau);
  real sigma_kappa = sqrt(prop[1]) * sigma;
  real sigma_theta = sqrt(prop[2]) * sigma;
  vector[N] theta = unscaled_theta * sigma_theta; 
  vector[K - 1] kappa[P];
  for(p in 1:P){
		kappa[p] = mu + sigma_kappa * unscaled_kappa[p];
	}
}
model {
  // likelihood 
  for(i in 1:n) {
    //target +=- W[i] * ordered_logistic_lpmf(Y[i] | theta[TARGET[i]], kappa[PRESSURE[i]]);
    Y[i] ~ ordered_logistic(theta[TARGET[i]], kappa[PRESSURE[i]]);
  }
  // priors
	for(p in 1:P){
		unscaled_kappa[p] ~ std_normal();
	}
	unscaled_mu ~ std_normal();
	unscaled_theta ~ std_normal();
	psi ~ gamma(1.0, 1.0);
	tau ~ gamma(1.0, 1.0);
}

generated quantities {
	vector[n] log_lik; //log likelihood
	int y_rep[n]; // posterior predictive checks
	for(i in 1:n){
		log_lik[i] = ordered_logistic_lpmf(Y[i] | theta[TARGET[i]], kappa[PRESSURE[i]]);
		y_rep[i] = ordered_logistic_rng(theta[TARGET[i]], kappa[PRESSURE[i]]);
	}
}
"

grm_cov = "
data {
	int<lower = 1> n; // number of studies
	int<lower = 1> n_cov; // number of cov
	int<lower = 1> N; // number of target
	int<lower = 1> P; // number of pressure
	int<lower = 1> K; // number of categories
	int<lower = 1, upper = K> Y[n]; // score data
	int<lower = 1, upper = N> TARGET[n]; // target ID
	int<lower = 1, upper = P> PRESSURE[n]; // pressure ID
	real<lower = 0.0> prior_scale;
	matrix[n, n_cov] X; // covariates
	real<lower = 1> nu_global; // for horseshoe prior
  real<lower = 1> nu_local; // for horseshoe prior
}

parameters {
	ordered[K - 1] unscaled_kappa[P]; // thresholds for each pressure
	vector[N] unscaled_theta; // factor scores for each target
	real unscaled_mu; // mean of the prior distribution of category difficulty
	simplex[2] prop;
	real<lower = 0.0> psi;
	real<lower = 0.0> tau;
	vector[n_cov] unscaled_beta;
  vector<lower = 0.0>[n_cov] local_sq;
  vector<lower = 0.0>[n_cov] aux_local;
  real<lower = 0.0> global_sq;
  real<lower = 0.0> aux_global;
}

transformed parameters {
  real mu = unscaled_mu * log(5) / 2;
  real sigma = prior_scale * sqrt(psi / tau);
  real sigma_kappa = sqrt(prop[1]) * sigma;
  real sigma_theta = sqrt(prop[2]) * sigma;
  vector[N] theta = unscaled_theta * sigma_theta;
  real global = sigma * aux_global * sqrt(global_sq); 
  vector[n_cov] local = aux_local .* sqrt(local_sq); 
  vector[n_cov] beta = global * unscaled_beta .* local;
  // linear predictor
  vector[n] linpred = X * beta;
  vector[K - 1] kappa[P];
  for(p in 1:P){
		kappa[p] = mu + sigma_kappa * unscaled_kappa[p];
	}
}
model {
  // likelihood 
  for(i in 1:n) {
    //target +=- W[i] * ordered_logistic_lpmf(Y[i] | theta[TARGET[i]] + linpred[i], kappa[PRESSURE[i]]);
    Y[i] ~ ordered_logistic(theta[TARGET[i]] + linpred[i], kappa[PRESSURE[i]]);
  }
  // priors
	for(p in 1:P){
		unscaled_kappa[p] ~ std_normal();
	}
	unscaled_mu ~ std_normal();
	unscaled_theta ~ std_normal();
	psi ~ gamma(1.0, 1.0);
	tau ~ gamma(1.0, 1.0);
	// horseshoe prior: half-t distributions as scale mixture of normal distributions
	unscaled_beta ~ std_normal();
  aux_local ~ normal(0.0, 1.0);
  local_sq ~ inv_gamma(0.5 * nu_local, 0.5 * nu_local);
  aux_global ~ std_normal();
  global_sq ~ inv_gamma(0.5 * nu_global, 0.5 * nu_global);
}

generated quantities {
	vector[n] log_lik; //log likelihood
	int y_rep[n]; // posterior predictive checks
	for(i in 1:n){
		log_lik[i] = ordered_logistic_lpmf(Y[i] | theta[TARGET[i]] + linpred[i], kappa[PRESSURE[i]]);
		y_rep[i] = ordered_logistic_rng(theta[TARGET[i]] + linpred[i], kappa[PRESSURE[i]]);
	}
}
"

grm_cov2 = "
data {
	int<lower = 1> n; // number of studies
	int<lower = 1> n_cov; // number of cov
	int<lower = 1> N; // number of target
	int<lower = 1> P; // number of pressure
	int<lower = 1> K; // number of categories
	int<lower = 1, upper = K> Y[n]; // score data
	int<lower = 1, upper = N> TARGET[n]; // target ID
	int<lower = 1, upper = P> PRESSURE[n]; // pressure ID
	real<lower = 0.0> prior_scale;
	matrix[n, n_cov] X; // covariates
	real<lower = 1> nu_global; // for horseshoe prior
  real<lower = 1> nu_local; // for horseshoe prior
}

parameters {
	ordered[K - 1] unscaled_kappa[P]; // thresholds for each pressure
	vector[P] unscaled_alpha; // slopes for each pressure
	vector[N] theta; // factor scores for each target
	real unscaled_mu; // mean of the prior distribution of category difficulty
	simplex[2] prop;
	real<lower = 0.0> psi;
	real<lower = 0.0> tau;
	vector[n_cov] unscaled_beta;
  vector<lower = 0.0>[n_cov] local_sq;
  vector<lower = 0.0>[n_cov] aux_local;
  real<lower = 0.0> global_sq;
  real<lower = 0.0> aux_global;
}

transformed parameters {
  real mu = unscaled_mu * log(5) / 2;
  real sigma = prior_scale * sqrt(psi / tau);
  real sigma_kappa = sqrt(prop[1]) * sigma;
  real sigma_alpha = sqrt(prop[2]) * sigma;
  vector[P] alpha = exp(unscaled_alpha - 0.5) * sigma_alpha;
  real global = aux_global * sqrt(global_sq); 
  vector[n_cov] local = aux_local .* sqrt(local_sq); 
  vector[n_cov] beta = global * unscaled_beta .* local;
  // linear predictor
  vector[n] linpred = X * beta;
  vector[K - 1] kappa[P];
  vector[K - 1] thresholds[P];
  for(p in 1:P){
		kappa[p] = mu + sigma_kappa * unscaled_kappa[p];
		for(k in 1:(K-1)){
			thresholds[p, k] = kappa[p, k] / alpha[p];
		}
	}
}
model {
  // likelihood 
  for(i in 1:n) {
    //target +=- W[i] * ordered_logistic_lpmf(Y[i] | alpha[PRESSURE[i]] * (theta[TARGET[i]] + linpred[i]), kappa[PRESSURE[i]]);
    Y[i] ~ ordered_logistic(alpha[PRESSURE[i]] * (theta[TARGET[i]] + linpred[i]), kappa[PRESSURE[i]]);
  }
  // priors
	for(p in 1:P){
		unscaled_kappa[p] ~ std_normal();
	}
	unscaled_mu ~ std_normal();
	unscaled_alpha ~ std_normal();
	theta ~ std_normal();
	psi ~ gamma(1.0, 1.0);
	tau ~ gamma(1.0, 1.0);
	// horseshoe prior: half-t distributions as scale mixture of normal distributions
	unscaled_beta ~ std_normal();
  aux_local ~ normal(0.0, 1.0);
  local_sq ~ inv_gamma(0.5 * nu_local, 0.5 * nu_local);
  aux_global ~ std_normal();
  global_sq ~ inv_gamma(0.5 * nu_global, 0.5 * nu_global);
}

generated quantities {
	vector[n] log_lik; //log likelihood
	int y_rep[n]; // posterior predictive checks
	for(i in 1:n){
		log_lik[i] = ordered_logistic_lpmf(Y[i] | alpha[PRESSURE[i]] * (theta[TARGET[i]] + linpred[i]), kappa[PRESSURE[i]]);
		y_rep[i] = ordered_logistic_rng(alpha[PRESSURE[i]] * (theta[TARGET[i]] + linpred[i]), kappa[PRESSURE[i]]);
	}
}
"

allparam <- c("theta", "kappa", "sigma_theta", "sigma_kappa", "mu", "sigma", "prop", "y_rep", "log_lik")
param <- c("theta", "kappa", "sigma_theta", "sigma_kappa", "mu", "sigma", "prop")
grm_stan <- stan_model(model_code = grm_null, model_name = "Graded Response Model")

allparamcov <- c("theta", "alpha", "kappa", "sigma_alpha", "sigma_kappa", "mu", "sigma", "prop",
                 "thresholds", "beta", "local", "global", "y_rep", "log_lik"
                 )
paramcov <- c("theta", "alpha", "kappa", "sigma_alpha", "sigma_kappa", "mu", "sigma", "prop")
grmcov_stan <- stan_model(model_code = grm_cov, model_name = "Graded Response Model with covariates (horseshoe prior)")

prepare_datastan <- function(df) {
  # recode target
  target_name <- df %>%
    group_by(target) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(target_code = factor(target, levels = target),
           target_code = as.numeric(target_code)
           ) %>%
    select(-n)
  # recode pressure
  pressure_name <- df %>%
    group_by(pressure) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(pressure_code = factor(pressure, levels = pressure),
           pressure_code = as.numeric(pressure_code)
           ) %>%
    select(-n)
  # recode
  df <- df %>%
    left_join(.,
              target_name
              ) %>%
    left_join(.,
              pressure_name
              )
  # if(!any(names(df) == "w")) {
  #   df <- df %>%
  #     mutate(w = 1)
  # }
  out <- list(standata = list(n = nrow(df),
                              N = nrow(target_name),
                              P = nrow(pressure_name),
                              K = df %>% pull(score) %>% max(),
                              Y = df %>% pull(score),
                              TARGET = df %>% pull(target_code),
                              PRESSURE = df %>% pull(pressure_code),
                              prior_scale = log(2) / 2
                              # W = df %>% pull(w)
                              # prior_scale_kappa = prior_scale_t(sigma = log(2) / 2),
                              # prior_scale_theta = prior_scale_t(sigma = log(2) / 2)
                              ),
              target = target_name,
              pressure = pressure_name
              )
  return(out)

}

prepare_covariate <- function(df) {
  df <- df %>%
    select(owf_fixed,
           owf_float,
           owf_component,
           owf_op_insitu,
           # owf_dc_insitu, no variation on that one: always no
           owf_pd_insitu,
           owf_not_insitu,
           owf_blade,
           owf_nacelle,
           owf_tower_above,
           owf_tower_below,
           owf_foundation,
           owf_fixation,
           owf_substation,
           owf_cable,
           owf_park,
           owf_vessel,
           study_type,
           starts_with("study_emp"),
           study_mod,
           study_participatory,
           study_other
           )
  out <- apply(df, 2, function(x) { ifelse(x == "No", 0, 1) }) %>%
    as.matrix()
  return(out)
}

staninit <- function(id, standata) {
  prob <- runif(1, 0, 1)
  out <- list(psi = rgamma(1, 2, 2),
              tau = rgamma(1, 2, 2),
              theta = rnorm(standata$N),
              unscaled_mu = rnorm(1),
              unscaled_alpha = rnorm(standata$P),
              kappa = replicate(standata$P, rnorm(standata$K - 1)) %>% t(),
              prop = c(prob, 1 - prob)
              )
  if(!is.null(standata$n_cov)){
    out$unscaled_beta <- rnorm(standata$n_cov)
    out$global_sq <- 1/rgamma(1, shape = 3/2, rate = 3/2)
    out$aux_global <- abs(rnorm(1))
    out$local_sq <- 1/rgamma(standata$n_cov, shape = 3/2, rate = 3/2)
    out$aux_local <- abs(rnorm(standata$n_cov))
  }
  return(out)
}

spearman <- function(modelfit, y_obs) {
  y_rep <- rstan::extract(modelfit, pars = "y_rep")$y_rep
  out <- map(.x = 1:nrow(y_rep),
             .f = function(i) { cor(y_obs, as.numeric(y_rep[i, ]), method = "spearman") }
             ) %>%
    unlist()
  return(out)
}

format_output <- function(modelfit, modeldata, ebv) {
  target <- modeldata$target %>%
    mutate(estimate = apply(rstan::extract(modelfit, pars = "theta")$theta, 2, mean),
           se = apply(rstan::extract(modelfit, pars = "theta")$theta, 2, sd),
           lower = apply(rstan::extract(modelfit, pars = "theta")$theta, 2, quantile, probs = 0.1),
           upper = apply(rstan::extract(modelfit, pars = "theta")$theta, 2, quantile, probs = 0.9),
           param = "theta",
           ebv = ebv
           )
  if(any(modelfit@model_pars == "thresholds")) {
    pressure <- modeldata$pressure %>%
      mutate(estimate = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,1], 2, mean),
             se = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,1], 2, sd),
             lower = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,1], 2, quantile, probs = 0.1),
             upper = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,1], 2, quantile, probs = 0.9),
             param = "kappa_1",
             ebv = ebv
             ) %>%
      rbind(.,
            modeldata$pressure %>%
              mutate(estimate = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,2], 2, mean),
                     se = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,2], 2, sd),
                     lower = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,2], 2, quantile, probs = 0.1),
                     upper = apply(rstan::extract(modelfit, pars = "thresholds")$thresholds[,,2], 2, quantile, probs = 0.9),
                     param = "kappa_2",
                     ebv = ebv
                     )
            )
  } else {
    pressure <- modeldata$pressure %>%
      mutate(estimate = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,1], 2, mean),
             se = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,1], 2, sd),
             lower = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,1], 2, quantile, probs = 0.1),
             upper = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,1], 2, quantile, probs = 0.9),
             param = "kappa_1",
             ebv = ebv
             ) %>%
      rbind(.,
            modeldata$pressure %>%
              mutate(estimate = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,2], 2, mean),
                     se = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,2], 2, sd),
                     lower = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,2], 2, quantile, probs = 0.1),
                     upper = apply(rstan::extract(modelfit, pars = "kappa")$kappa[,,2], 2, quantile, probs = 0.9),
                     param = "kappa_2",
                     ebv = ebv
                     )
            )
  }
  
  out <- list(target = target, pressure = pressure)
  return(out)
}

fit_grm <- function(grm_data, ebv) {
  ebv_data <- grm_data %>%
    filter(ebv_name == ebv) %>%
    select(target, pressure, score) %>%
    prepare_datastan()
  
  grm_fit <- sampling(grm_stan,
                      data = ebv_data$standata,
                      pars = allparam,
                      chains = 4,
                      iter = 2000,
                      warmup = 1000,
                      thin = 1
                      )
  ### wrap-up
  out <- list(modelfit = grm_fit,
              gof_spearman = spearman(modelfit = grm_fit,
                                      y_obs = ebv_data$standata$Y
                                      ),
              posterior = format_output(modelfit = grm_fit, 
                                        modeldata = ebv_data, 
                                        ebv = ebv
                                        )
              )
  return(out)
}


pressure_map <- function(pressure_name) {
  pr <- post_master$pressure %>%
    filter(pressure == pressure_name)
  
  mylim <- range(post_master$pressure$lower, post_master$pressure$upper,
                 post_master$target$lower, post_master$target$upper
                 )
  
  df <- post_master$target %>%
    arrange(desc(estimate)) %>%
    mutate(target_code = 1:n()) %>%
    left_join(.,
              model_based %>%
                filter(pressure == pressure_name)
              )
  boxdf <- data.frame(target_code = rep(0.5, 5),
                      estimate = rep(100, 5),
                      score = factor(c("1", "1-2", "2", "2-3", "3"),
                                     levels = c("1", "1-2", "2", "2-3", "3")
                                     )
                      )
  # boxdf <- data.frame(x1 = rep(0, 5),
  #                     x2 = rep(nrow(df) + 1, 5),
  #                     y1 = c(pr %>% filter(param == "kappa_1") %>% pull(lower),
  #                            pr %>% filter(param == "kappa_1") %>% pull(lower),
  #                            pr %>% filter(param == "kappa_1") %>% pull(upper),
  #                            pr %>% filter(param == "kappa_2") %>% pull(lower),
  #                            pr %>% filter(param == "kappa_2") %>% pull(upper)
  #                            ),
  #                     y2 = c(-Inf,
  #                            pr %>% filter(param == "kappa_1") %>% pull(upper),
  #                            pr %>% filter(param == "kappa_2") %>% pull(lower),
  #                            pr %>% filter(param == "kappa_2") %>% pull(upper),
  #                            +Inf
  #                            ),
  #                     score = factor(c("1", "1-2", "2", "2-3", "3"), 
  #                                    levels = c("1", "1-2", "2", "2-3", "3")
  #                                    )
  #                     )
  
  ### deal with extrapolation
  out <- df %>%
    ggplot(aes(x = target_code, y = estimate)) +
    geom_point(data = boxdf,
               aes(x = target_code, y = estimate, color = score)
               ) +
    geom_rect(aes(ymin = -Inf, ymax = pr %>% filter(param == "kappa_1") %>% pull(lower),
                  xmin = 0, xmax = nrow(df) + 1
                  ),
              fill = "#253494", alpha = 0.1, color = NA
              ) +
    geom_rect(aes(ymin = pr %>% filter(param == "kappa_1") %>% pull(lower),
                  ymax = pr %>% filter(param == "kappa_1") %>% pull(upper),
                  xmin = 0, xmax = nrow(df) + 1
                  ),
              fill = "#2c7fb8", alpha = 0.1, color = NA
              ) +
    geom_rect(aes(ymin = pr %>% filter(param == "kappa_1") %>% pull(upper),
                  ymax = pr %>% filter(param == "kappa_2") %>% pull(lower),
                  xmin = 0, xmax = nrow(df) + 1
                  ),
              fill = "#41b6c4", alpha = 0.1, color = NA
              ) +
    geom_rect(aes(ymin = pr %>% filter(param == "kappa_2") %>% pull(lower),
                  ymax = pr %>% filter(param == "kappa_2") %>% pull(upper),
                  xmin = 0, xmax = nrow(df) + 1
                  ),
              fill = "#a1dab4", alpha = 0.1, color = NA
              ) +
    geom_rect(aes(ymin = pr %>% filter(param == "kappa_2") %>% pull(upper), ymax = +Inf,
                  xmin = 0, xmax = nrow(df) + 1
                  ),
              fill = "#ffffcc", alpha = 0.1, color = NA
              ) +
    geom_linerange(aes(ymin = lower, ymax = upper), color = "black", alpha = 0.6) +
    geom_point(aes(alpha = interpolation), size = 2, color = "black") +
    scale_x_continuous(name = "Target",
                       breaks = df$target_code,
                       labels = df$target
                       ) +
    scale_y_continuous(name = "", 
                       breaks = NULL
                       # breaks = 0.5 * c(mylim[1] + pr %>% filter(param == "kappa_1") %>% pull(estimate) / 2,
                       #                  pr %>% pull(estimate) %>% sum(),
                       #                  mylim[2] - pr %>% filter(param == "kappa_2") %>% pull(estimate) / 2
                       #                  ),
                       # label = c("No\neffect", "Mixed\neffect", "Effect\n(+ or -)")
                       ) +
    scale_color_manual(name = "Score", values = c("#253494", "#2c7fb8", "#41b6c4", "#a1dab4", "#ffffcc")) +
    coord_flip(ylim = mylim,
               xlim = c(0, nrow(df) + 0.5) 
               ) +
    theme_bw() +
    labs(title = pressure_name,
         subtitle = "Model-based integration",
         caption = "Source: ESCO"
         ) +
    guides(alpha = "none",
           color = guide_legend(override.aes = list(size = 3)) 
           )
  return(out)
}
