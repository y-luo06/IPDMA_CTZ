
#########################################################################################

# stage 2: Bayesian IPD meta-regression model 

library(tidyverse)
library(R2jags)

## df: including missing values and all the variables
## df's derivative datasets:
## lda.16: outcome = LDA, predictors = 16
## complete case only dataset: cca_lda16

# prepare variables that are necessary for stage 2 model
lda16 <- read.csv("cca_lda16.csv") # only 4 studies

study5 <- subset(lda.16, STUDY == "C87027") # study 5 has missing variables

# incorporate study5 to lda16
misvar <- c("SEX", "BMI", "DURATION.T", "DMARD_PRE", "BIOLOGIC_PRE")
study5 <- study5 %>% filter_at(vars(-misvar), all_vars(!is.na(.))) # excluding patients who have NA in other variables
lda16 <- bind_rows(lda16, study5) # now lda16 has 5 studies
lda16$STUDY <- factor(lda16$STUDY, levels = c("C87014","C87050","C87094","RA0055","C87027"))
lda16$STUDY <- as.numeric(lda16$STUDY)


# new data for prediction
RISK <- list(seq(0.01, 0.99, 0.01))
LOGIT <- lapply(RISK, function(x) log(x/(1-x)))
risk_new <- data.frame(RISK=unlist(RISK), LOGIT=unlist(LOGIT))


# data for prediction model
jagsDataLR16.LDA <- list(
  n_study = length(unique(lda16$STUDY)), # 5
  n_pat = nrow(lda16), 
  n_pat_mis = nrow(lda16[lda16$STUDY == 5,]), ## study 5 has missing variables
  n_placebo = nrow(lda16[lda16$TREAT == 0,]),
  n_new = 99,
  
  studyid = lda16$STUDY,
  treat = lda16$TREAT + 1,
  outcome = lda16$LDA,
  outcome_placebo = lda16$LDA[lda16$TREAT == 0],
  
  ## mean logit for  all 5 studies 
  logit_mean = (mean(lda16$LOGIT.LR16, na.rm = T)*(3405-966) 
                + sum(var.average.5$varbycoef)*966)/3405, #-1.013653
  
  ## logitP of the new dataset  
  logit_new = risk_new$LOGIT,
  ## logit_new_mean is in context of what population we will predict
  ## here we estimate it as the mean logit for all the participants in 5 trials (same as logit_mean)
  logit_new_mean = (mean(lda16$LOGIT.LR16, na.rm = T)*(3405-966) 
                    + sum(var.average.5$varbycoef)*966)/3405, #-1.013653
  
  ## for 1 study that has missing data, the following variables will be used (not missing in study5)
  age = lda16$AGE.T, tjc = lda16$TEND28_BAS.T, sjc = lda16$SWOL28_BAS, patg = lda16$PATG_BAS,
  phyg = lda16$PHYG_BAS, pain = lda16$PAIN_BAS, crp = lda16$CRP_BAS.T, esr = lda16$ESR_BAS,
  rf = lda16$RF_BAS.T, haq = lda16$HAQ_BAS, fatig = lda16$FATIG_BAS,
  ## coef
  m_coef = lr16.coef$estimate.average,
  
  ## borrow data from aggregate level summaries for study 5
  p_sex = 165/982,
  p_dmard = (982-318)/982,
  p_biologic = 34/982,
  m_bmi = 27.4,
  sd_bmi = 5.83,
  m_duration = log(6.14), # because duration was transformed
  sd_duration = log(4.3)
)


# prediction model
source("model_stage2.R")

ipdPRED.LDA16 <- jags.parallel(data = jagsDataLR16.LDA, inits = NULL,
                               parameters.to.save = c("a", "delta", "gamma0", "gamma", "tau_d", "tau_d.sq", "prec_d", "OR",
                                                      "tau_g0", "tau_g0.sq", "prec_g0", "tau_g", "tau_g.sq", "prec_g", "logit_placebo",
                                                      "p_new_placebo", "p_new_ctz", "rd"),
                               model.file = model_stage2, n.chains = 2, n.iter = 100000, n.burnin = 10000,
                               DIC = F, n.thin = 100)

df_ipdPRED.LDA16 <- as.data.frame(ipdPRED.LDA16$BUGSoutput$summary[c(1:10,308:310,509:514),])
df_pPRED.LDA16 <- as.data.frame(ipdPRED.LDA16$BUGSoutput$summary[c(11:307,311:508),])

par(mfrow=c(4,4), mar=c(3.2,3.2,1,1), mgp=c(2,0.6,0))
traceplot(ipdPRED.LDA16$BUGSoutput, 
          varname=c("OR", "a", "delta", "gamma", "tau", "logit_placebo"),
          mfrow=c(4,4), ask=FALSE) #12*12

pred_new <- df_predpLDA16[,c(1,2,4,8)]
colnames(pred_new) <- c("var", "estimate", "lower", "upper")
pred_new$risk.bas <- rep(seq(0.01, 0.99, 0.01), 5)
pred_new$var <- c(rep("p_new_ctz", 99), rep("p_new_placebo", 99), rep("rd", 99))
pred_new$var <- as.factor(pred_new$var)

pred_p_plot <- subset(pred_new, var == "p_new_ctz" | var == "p_new_placebo" )
rd_plot <- subset(pred_new, var == "rd")
rd_sig <- subset(rd_plot, risk.bas>=0.19 & risk.bas<=0.65)

RISKmin <- min(lda16$RISK.LR16, na.rm = T) # 0.0288238
RISKmax <- max(lda16$RISK.LR16, na.rm = T) # 0.8680566

df_breaks <- data.frame(start=c(-Inf, RISKmin, RISKmax),
                        end=c(RISKmin, RISKmax, Inf),
                        fill=c("out", "in", "out"))

library(ggnewscale)

g1 <- ggplot(data = pred_p_plot, aes(x=risk.bas, y=estimate)) +
  ggtitle("(A) Predicted probability for each treatment given the baseline expected probability") +
  xlab("Baseline expected probability of low disease activity or remission regardless of treatment at stage one") +
  ylab("Predicted probability of low disease activity or remission after treatment at 3-month") +
  scale_y_continuous(limits = c(-0.03, 1), breaks = c(0,0.2,0.4,0.6,0.8,1.0)) +
  geom_point(aes(group=var, col=var), size=1, alpha=0.7) +
  scale_color_manual(name="Treatment", 
                     values=c("deepskyblue1", "goldenrod1"),
                     labels=c("CTZ+csDMARDs", "Placebo+csDMARDs")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group=var, fill=var), alpha=0.1) +
  scale_fill_manual(values=c("deepskyblue1", "goldenrod1"), guide="none") +
  geom_vline(xintercept=c(RISKmin, RISKmax), col="darkgray", lty=2) +
  new_scale_fill() +
  geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
            inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white", "gray"), 0.2), guide="none") +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                     axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                     axis.title.x = element_text(margin = margin(t = 10))) +
  coord_cartesian(clip = "off")

g2 <- ggplot(data = rd_plot, aes(x=risk.bas, y=estimate)) +
  ggtitle("(B) Predicted risk difference between two treatments given the baseline expected probability") +
  xlab("Baseline expected probability of low disease activity or remission regardless of treatment at stage one") +
  ylab("Predicted risk difference after treatment at 3-month") +
  scale_y_continuous(limits = c(-0.03, 1), breaks = c(0,0.1,0.2,0.4,0.6,0.8,1.0)) +
  geom_point(aes(group=var, col=var), size=2, alpha=0.7) +
  scale_color_manual(name="Effect measure", 
                     values=c("red2"),
                     labels=c("Risk difference")) +
  geom_ribbon(aes(ymin=lower, ymax=upper, group=var, fill=var), alpha=0.1) +
  scale_fill_manual(values=c("red2"), guide="none") +
  geom_vline(xintercept=c(RISKmin, RISKmax), col="darkgray", lty=2) +
  geom_hline(yintercept=0.1, col="red", lty=3) +
  geom_ribbon(data=rd_sig, aes(ymin=lower, ymax=upper), fill="red3", alpha=0.2) +
  new_scale_fill() +
  geom_rect(data = df_breaks, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf, fill=fill), 
            inherit.aes = FALSE) + scale_fill_manual(values = alpha(c("white", "gray"), 0.2), guide="none") +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.2),
                     axis.title = element_text(size = 13), legend.text = element_text(size = 12),
                     axis.title.x = element_text(margin = margin(t = 10)),
                     axis.text.y = element_text(colour = c('black','red','black','black','black','black','black')),
                     axis.ticks.y = element_line(colour = c('black','red','black','black','black','black','black'))) + 
  coord_cartesian(clip = "off") 
g2

library(egg)
ggarrange(g1, g2, ncol=1) # export 11*14

  


