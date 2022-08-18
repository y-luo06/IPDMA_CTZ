
####################################################
# IPD-MA

## df: including missing values and all the variables

library(R2jags)

# data for IPD-MA: LDA
jagsDataMA.LDA <- list(
  n_study = length(unique(df$STUDY)),
  n_pat = length(unique(df$PATID[!is.na(df$LDA)])), 
  
  studyid = as.numeric(df$STUDY[!is.na(df$LDA)]),
  treat = df$TREAT[!is.na(df$LDA)] + 1,
  outcome = df$LDA[!is.na(df$LDA)]
)

# data for IPD-MA: ACR50
jagsDataMA.ACR <- list(
  n_study = length(unique(df$STUDY)),
  n_pat = length(unique(df$PATID[!is.na(df$ACR50)])), # 3733
  
  studyid = as.numeric(df$STUDY[!is.na(df$ACR50)]),
  treat = df$TREAT[!is.na(df$ACR50)] + 1,
  outcome = df$ACR50[!is.na(df$ACR50)]
)

# data for IPD-MA: SAE
jagsDataMA.SAE <- list(
  n_study = length(unique(df$STUDY)),
  n_pat = length(unique(df$PATID[!is.na(df$SAE)])), # 3790
  
  studyid = as.numeric(df$STUDY),
  treat = df$TREAT + 1,
  outcome = df$SAE
)

# data for IPD-MA: infection-AE
jagsDataMA.INFECT <- list(
  n_study = length(unique(df$STUDY)),
  n_pat = length(unique(df$PATID[!is.na(df$INFECTION)])), # 3790
  
  studyid = as.numeric(df$STUDY),
  treat = df$TREAT + 1,
  outcome = df$INFECTION
)


# meta-analysis
source("model_ipdma.R")

## LDA
ipdMA.LDA <- jags.parallel(data = jagsDataMA.LDA, inits = NULL, 
                           parameters.to.save = c("a", "delta", "tau", "tau.sq", "prec", "OR"),
                           model.file = model_ipdma, n.chains = 2, n.iter = 100000, n.burnin = 10000,
                           DIC = F, n.thin = 100)
print(ipdMA.LDA)
# traceplots
par(mfrow=c(3,4), mar=c(3.2,3.2,1,1), mgp=c(2,0.6,0))
traceplot(ipdMA.LDA$BUGSoutput, mfrow=c(3,4), ask=FALSE) #12*9
par(mfrow=c(1,1))
plot(ipdMA.LDA$BUGSoutput)


## ACR50
ptm<-proc.time()
ipdMA.ACR <- jags.parallel(data = jagsDataMA.ACR, inits = NULL, 
                           parameters.to.save = c("a", "delta", "tau", "tau.sq", "prec", "OR"),
                           model.file = model_ipdma, n.chains = 2, n.iter = 100000, n.burnin = 10000,
                           DIC = F, n.thin = 100)
proc.time()-ptm
print(ipdMA.ACR)
# traceplots
par(mfrow=c(3,4), mar=c(3.2,3.2,1,1), mgp=c(2,0.6,0))
traceplot(ipdMA.ACR$BUGSoutput, mfrow=c(3,4), ask=FALSE) #12*9
par(mfrow=c(1,1))
plot(ipdMA.ACR$BUGSoutput)


## SAE
ipdMA.SAE <- jags.parallel(data = jagsDataMA.SAE, inits = NULL, 
                           parameters.to.save = c("a", "delta", "tau", "tau.sq", "prec", "OR"),
                           model.file = model_ipdma, n.chains = 2, n.iter = 100000, n.burnin = 10000,
                           DIC = F, n.thin = 100)
print(ipdMA.SAE)
# traceplots
par(mfrow=c(3,4), mar=c(3.2,3.2,1,1), mgp=c(2,0.6,0))
traceplot(ipdMA.SAE$BUGSoutput, mfrow=c(3,4), ask=FALSE)
par(mfrow=c(1,1))
plot(ipdMA.SAE$BUGSoutput)


## INFECTION
ipdMA.INFECT <- jags.parallel(data = jagsDataMA.INFECT, inits = NULL, 
                              parameters.to.save = c("a", "delta", "tau", "tau.sq", "prec", "OR"),
                              model.file = model_ipdma, n.chains = 2, n.iter = 100000, n.burnin = 10000,
                              DIC = F, n.thin = 100)
print(ipdMA.INFECT)
# traceplots
par(mfrow=c(3,4), mar=c(3.2,3.2,1,1), mgp=c(2,0.6,0))
traceplot(ipdMA.INFECT$BUGSoutput, mfrow=c(3,4), ask=FALSE)
par(mfrow=c(1,1))
plot(ipdMA.INFECT$BUGSoutput)

