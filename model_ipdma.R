
model_ipdma <- function() {
  ## likelihood
  for (i in 1:n_pat) {
    outcome[i] ~ dbern(p[i])
    logit(p[i]) <- a[studyid[i]] + d[studyid[i], treat[i]]
  }
  
  # treatment effect d: random-effects model
  for (j in 1:n_study) {
    d[j,1] <- 0
    d[j,2] ~ dnorm(delta, prec)
  }
  delta ~ dnorm(0, 0.0001)
  # prior for tau: only take posive value
  # vague prior: half-normal distribution
  tau ~ dnorm(0, 1) %_% T(0,)
  tau.sq <- pow(tau, 2)
  prec <- 1/pow(tau, 2)
  
  OR <- exp(delta)
  
  # prior for a: independent across trials
  for (j in 1:n_study) {
    a[j] ~ dnorm(0, 0.001)
  }
}
