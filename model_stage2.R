
model_stage2 <- function() {
  
  # for that specific study: No.5
  for (i in (n_pat-n_pat_mis+1):n_pat) {
    logit_br[i] = m_coef[1] + m_coef[2]*sex[i] + m_coef[3]*age[i] + m_coef[4]*bmi[i] + 
      m_coef[5]*duration[i] + m_coef[6]*dmard[i] + m_coef[7]*biologic[i] + m_coef[8]*tjc[i] +
      m_coef[9]*sjc[i] + m_coef[10]*patg[i] + m_coef[11]*phyg[i] + m_coef[12]*pain[i] +
      m_coef[13]*crp[i] + m_coef[14]*esr[i] + m_coef[15]*rf[i] + m_coef[16]*haq[i] + m_coef[17]*fatig[i]
    # the distribution of the missing variables follow the distribution
    # based on the published aggregate level summaries
    sex[i] ~ dbern(p_sex) # binary: p=prob of y=1, 1-p --> y=0
    bmi[i] ~ dnorm(m_bmi, 1/pow(sd_bmi, 2))
    duration[i] ~ dnorm(m_duration, 1/pow(sd_duration, 2))
    dmard[i] ~ dbern(p_dmard)
    biologic[i] ~ dbern(p_biologic)
    
  }
  
  ## likelihood
  for (i in 1:n_pat) {
    outcome[i] ~ dbern(p[i])
    logit(p[i]) <- a[studyid[i]] + d[studyid[i], treat[i]] + 
      g0[studyid[i]]*(logit_br[i]-logit_mean) + 
      g[studyid[i], treat[i]]*(logit_br[i]-logit_mean)
  }
  
  # average treatment effect d: random-effects model
  for (j in 1:n_study) {
    d[j,1] <- 0
    d[j,2] ~ dnorm(delta, prec_d)
  }
  delta ~ dnorm(0, 0.0001)
  # prior for tau: only take posive value
  # vague prior: half-normal distribution
  tau_d ~ dnorm(0, 1) %_% T(0,)
  tau_d.sq <- pow(tau_d, 2)
  prec_d <- 1/pow(tau_d, 2)
  
  OR <- exp(delta)
  
  # coef of risk score g0: random-effects model
  for (j in 1:n_study) {
    g0[j] ~ dnorm(gamma0, prec_g0)
  }
  gamma0 ~ dnorm(0, 0.0001)
  # prior for tau: only take posive value
  # vague prior: half-normal distribution
  tau_g0 ~ dnorm(0, 1) %_% T(0,)
  tau_g0.sq <- pow(tau_g0, 2)
  prec_g0 <- 1/pow(tau_g0, 2)
  
  # coef of interaction g: random-effects model
  for (j in 1:n_study) {
    g[j,1] <- 0
    g[j,2] ~ dnorm(gamma, prec_g)
  }
  gamma ~ dnorm(0, 0.0001)
  # prior for tau: only take posive value
  # vague prior: half-normal distribution
  tau_g ~ dnorm(0, 1) %_% T(0,)
  tau_g.sq <- pow(tau_g, 2)
  prec_g <- 1/pow(tau_g, 2)
  
  # prior for a: independent across trials
  for (j in 1:n_study) {
    a[j] ~ dnorm(0, 0.001)
  }
  
  # in order to prefict for a new patient not from any trials
  # we need to estimate the intercept and the mean logit
  # the intercept a_new is estimated from all the control arms: 
  # log-odds for placebo who has average baseline risk score
  for (i in 1:n_placebo) {
    outcome_placebo[i] ~ dbern(p_placebo[i])
    logit(p_placebo[i]) <- logit_placebo
  }
  logit_placebo ~ dnorm(0, 0.001)
  
  # calculate the predicted risk for new patients
  for (i in 1:n_new) {
    logit_new_placebo[i] <- logit_placebo + gamma0*(logit_new[i] - logit_new_mean)
    logit_new_ctz[i] <- logit_placebo + delta + gamma0*(logit_new[i] - logit_new_mean) + 
      gamma*(logit_new[i] - logit_new_mean)
  }
  
  for (i in 1:n_new) {
    p_new_placebo[i] <- exp(logit_new_placebo[i])/(1+exp(logit_new_placebo[i]))
    p_new_ctz[i] <- exp(logit_new_ctz[i])/(1+exp(logit_new_ctz[i]))
    rd[i] <- p_new_placebo[i] - p_new_ctz[i]
  }
  
}

