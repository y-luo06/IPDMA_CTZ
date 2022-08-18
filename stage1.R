
####################################################
# imputation
library(mice)

## df: including missing values and all the variables
## df's derivative datasets:
## lda.22: outcome = LDA, predictors = 22
## lda.16: outcome = LDA, predictors = 16
## complete case only dataset: cca_lda22 & cca_lda16

# basic function
imp_lda.22 <- mice(lda.22, maxit = 0, seed = 1)
imp_lda.16 <- mice(lda.16, maxit = 0, seed = 1)

# check methods
imp_lda.22$method
imp_lda.16$method

# check predictor matrix
imp_lda.22$predictorMatrix
imp_lda.16$predictorMatrix

# imputation
imp_lda.22 <- mice(lda.22, maxit = 20, m = 10, seed = 1)
imp_lda.16 <- mice(lda.22, maxit = 20, m = 10, seed = 1)

# post-processing
## okay for all sets

# check the imputation
plot(imp_lda.22, DURATION.T + TEND28_BAS.T + SWOL28_BAS + MSTIF_BAS +
       PATG_BAS + PHYG_BAS + PAIN_BAS + CRP_BAS.T + ESR_BAS + RF_BAS.T + HAQ_BAS +
       FATIG_BAS ~ .it|.ms, layout = c(6,4)) #12*8
plot(imp_lda.16, DURATION.T + TEND28_BAS.T + SWOL28_BAS + PATG_BAS + 
       PHYG_BAS + PAIN_BAS + CRP_BAS.T + ESR_BAS + RF_BAS.T + HAQ_BAS +
       FATIG_BAS ~ .it|.ms, layout = c(6,4)) 

# density plot
densityplot(imp_lda.22) #12*9
densityplot(imp_lda.16) 

write.csv(complete(imp_lda.22, "long"), file = "mi_lda.22.csv", row.names = F)
write.csv(complete(imp_lda.16, "long"), file = "mi_lda.16.csv", row.names = F)

############# sample size calculation #############
library(pmsampsize)

# event rate for both LDA and ACR50: ~30%
# rsquared reported 0.2~0.4, use 0.1 to be more conservative
# full model of 22 variables: number of variable = 23
pmsampsize(type = "b", rsquared = 0.1, parameters = 23, prevalence = 0.3) # at least 1868

#########################################################################################
############# stage 1: risk model training and internal validation ######################

library(mice)
library(glmnet)
library(rms)
library(pROC)
library(dplyr)
library(caret)

mi_lda22 <- read.csv("mi_lda.22.csv")
mi_lda16 <- read.csv("mi_lda.16.csv")

#####################

# risk model training
# (1) 22-variable logistic regression + penalization
# (2) 22-variable logistic regression + LASSO (lambda.min & lambda.1se)
# (3) 16-variable logistic regression + penalization


# (1) 22-variable logistic regression + penalization: lr22

m = 10

lr22.coef <- c()

for (i in 1:m) {
  df_sub <- subset(mi_lda22, .imp == i)
  df_sub <- df_sub[, -c(1:2)] # exclude imputation index
  lr22 <- lrm(LDA ~ ., data = df_sub, x = T, y = T) # logistic regression
  penalized <- pentrace(lr22, seq(0,200,0.1))
  penalty <- penalized$penalty
  lr22.pen <- update(lr22, penalty = penalty)
  lr22.coef[[i]] <- data.frame(rownames(lr22.pen$var),
                               matrix(coef(lr22.pen)))
  colnames(lr22.coef[[i]]) <- c("coef.name", paste0("estimate.", i))
}

lr22.coef <- lr22.coef %>% 
  Reduce(function(d1, d2) left_join(d1, d2, by = c("coef.name")), .)
lr22.coef <- lr22.coef %>% rowwise() %>%
  mutate(estimate.average = mean(estimate.1:estimate.10))
## lr22.coef is a df that contains the coef for each predictor in 10 MI datasets and their average

# (2) 22-variable logistic regression + LASSO (lambda.min & lambda.1se): lassomin22,lasso1se22

m = 10

la.coef.min <- c()
la.coef.1se <- c()

for (i in 1:m) {
  df_sub <- subset(mi_lda22, .imp == i)
  df_sub <- df_sub[, -c(1:2)]
  lr22 <- lrm(LDA ~ ., data = df_sub, x = T, y = T)
  set.seed(1)
  cv.lasso <- cv.glmnet(x = lr22$x, y = lr22$y, alpha = 1,
                        family = c("binomial"), type.measure = "auc")
  lasso.min <- glmnet(x = lr22$x, y = lr22$y, alpha = 1,
                      lambda = cv.lasso$lambda.min, family = c("binomial"))
  lasso.1se <- glmnet(x = lr22$x, y = lr22$y, alpha = 1,
                      lambda = cv.lasso$lambda.1se, family = c("binomial"))
  la.coef.min[[i]] <- data.frame(rownames(coef(lasso.min)),
                                 matrix(coef(lasso.min)))
  la.coef.1se[[i]] <- data.frame(rownames(coef(lasso.1se)),
                                 matrix(coef(lasso.1se)))
  colnames(la.coef.min[[i]]) <- c("coef.name", paste0("estimate.", i))
  colnames(la.coef.1se[[i]]) <- c("coef.name", paste0("estimate.", i))
}

la.coef.min <- la.coef.min %>% 
  Reduce(function(d1, d2) left_join(d1, d2, by = c("coef.name")), .)
la.coef.min <- la.coef.min %>% rowwise() %>%
  mutate(estimate.average = mean(estimate.1:estimate.10))

la.coef.1se <- la.coef.1se %>% 
  Reduce(function(d1, d2) left_join(d1, d2, by = c("coef.name")), .)
la.coef.1se <- la.coef.1se %>% rowwise() %>%
  mutate(estimate.average = mean(estimate.1:estimate.10))
## la.coef is a df that contains the coef for each predictor in 10 MI datasets and their average


# (3) 16-variable logistic regression + penalization: lr16

m = 10

lr16.coef <- c()

for (i in 1:m) {
  df_sub <- subset(mi_lda16, .imp == i)
  df_sub <- df_sub[, -c(1:2)] # exclude imputation index
  lr16 <- lrm(LDA ~ ., data = df_sub, x = T, y = T) # logistic regression
  penalized <- pentrace(lr16, seq(0,200,0.1))
  penalty <- penalized$penalty
  lr16.pen <- update(lr16, penalty = penalty)
  lr16.coef[[i]] <- data.frame(rownames(lr16.pen$var),
                               matrix(coef(lr16.pen)))
  colnames(lr16.coef[[i]]) <- c("coef.name", paste0("estimate.", i))
}

lr16.coef <- lr16.coef %>% 
  Reduce(function(d1, d2) left_join(d1, d2, by = c("coef.name")), .)
lr16.coef <- lr16.coef %>% rowwise() %>%
  mutate(estimate.average = mean(estimate.1:estimate.10))
## lr16.coef is a df that contains the coef for each predictor in 10 MI datasets and their average


##############################

# model performance

## complete case only dataset: cca_lda22 & cca_lda16

# make a df to show various performances
performance <- data.frame(matrix(ncol = 12, nrow = 3))
colnames(performance) <- c("AUC.LR22", "AUC.LR16", "AUC.LASSOmin22", "AUC.LASSO1se22",
                           "CALint.LR22", "CALint.LR16", "CALint.LASSOmin22", "CALint.LASSO1se22",
                           "CALslp.LR22", "CALslp.LR16", "CALslp.LASSOmin22", "CALslp.LASSO1se22")
rownames(performance) <- c("apparent", "boot_optimism", "boot_corrected")


# predict risks for each participant
# (1) lr22
cca_lda22$LOGIT.LR22 <- apply(cca_lda22, 1, function(x) sum(x[-24]*lr22.coef$estimate.average))
cca_lda22$RISK.LR22 <- exp(cca_lda22$LOGIT.LR22)/(1+exp(cca_lda22$LOGIT.LR22))
hist(cca_lda22$RISK.LR22)

# (2) lassomin22 & lasso1se22
cca_lda22$LOGIT.LASSOMIN22 <- apply(cca_lda22, 1, function(x) sum(x[-c(24:26)]*lassomin22.coef$estimate.average))
cca_lda22$RISK.LASSOMIN22 <- exp(cca_lda22$LOGIT.LASSOMIN22)/(1+exp(cca_lda22$LOGIT.LASSOMIN22))
hist(cca_lda22$RISK.LASSOMIN22)

cca_lda22$LOGIT.LASSO1SE22 <- apply(cca_lda22, 1, function(x) sum(x[-c(24:28)]*lasso1se22.coef$estimate.average))
cca_lda22$RISK.LASSO1SE22 <- exp(cca_lda22$LOGIT.LASSO1SE22)/(1+exp(cca_lda22$LOGIT.LASSO1SE22))
hist(cca_lda22$RISK.LASSO1SE22)

# (3) lr16
cca_lda16$LOGIT.LR16 <- apply(cca_lda16, 1, function(x) sum(x[-18]*lr16.coef$estimate.average))
cca_lda16$RISK.LR16 <- exp(cca_lda16$LOGIT.LR16)/(1+exp(cca_lda16$LOGIT.LR16))
hist(cca_lda16$RISK.LR16)


# apparent performances
performance$AUC.LR22[1] <- roc(cca_lda22$LDA ~ cca_lda22$RISK.LR22)$auc
performance$AUC.LR16[1] <- roc(cca_lda16$LDA ~ cca_lda16$RISK.LR16)$auc
performance$AUC.LASSOmin22[1] <- roc(cca_lda22$LDA ~ cca_lda22$RISK.LASSOMIN22)$auc
performance$AUC.LASSO1se22[1] <- roc(cca_lda22$LDA ~ cca_lda22$RISK.LASSO1SE22)$auc

performance$CALint.LR22[1] <- coef(glm(cca_lda22$LDA ~ offset(cca_lda22$LOGIT.LR22), family = "binomial"))[[1]]
performance$CALslp.LR22[1] <- coef(glm(cca_lda22$LDA ~ cca_lda22$LOGIT.LR22, family = "binomial"))[[2]]
performance$CALint.LR16[1] <- coef(glm(cca_lda16$LDA ~ offset(cca_lda16$LOGIT.LR16), family = "binomial"))[[1]]
performance$CALslp.LR16[1] <- coef(glm(cca_lda16$LDA ~ cca_lda16$LOGIT.LR16, family = "binomial"))[[2]]
performance$CALint.LASSOmin22[1] <- coef(glm(cca_lda22$LDA ~ offset(cca_lda22$LOGIT.LASSOMIN22), family = "binomial"))[[1]]
performance$CALslp.LASSOmin22[1] <- coef(glm(cca_lda22$LDA ~ cca_lda22$LOGIT.LASSOMIN22, family = "binomial"))[[2]]
performance$CALint.LASSO1se22[1] <- coef(glm(cca_lda22$LDA ~ offset(cca_lda22$LOGIT.LASSO1SE22), family = "binomial"))[[1]]
performance$CALslp.LASSO1se22[1] <- coef(glm(cca_lda22$LDA ~ cca_lda22$LOGIT.LASSO1SE22, family = "binomial"))[[2]]

# plots of apparent performance: the best model LR16
# ROC
roc(cca_lda16$LDA ~ cca_lda16$RISK.LR16, 
    plot = T, print.auc = T, legacy.axes = T, 
    main = "ROC")
# calibration plot
val.prob(cca_lda16$RISK.LR16, cca_lda16$LDA, m = 50, cex = 1)
# dicrimination plot: 6*5
ggplot() +
  xlab("Baseline expected probability of achieving low disease activity or remission regardless of treatment") +
  geom_density(data = cca_lda16, 
               aes(x=RISK.LR16, group=as.factor(LDA), 
                   col=as.factor(LDA), fill=as.factor(LDA)), size=1) +
  scale_color_manual(name="Actually low disease activity at 3 months", 
                     values=c("goldenrod1", "deepskyblue3"),
                     labels=c("No", "Yes")) +
  scale_fill_manual(name="Actually low disease activity at 3 months", 
                    values=alpha(c("goldenrod1", "deepskyblue2"), 0.5),
                    labels=c("No", "Yes")) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.line.x = element_line(), plot.title = element_text(hjust = 0.5),
        legend.justification = c("right", "top"), legend.position = c(0.95,0.98))



## bootstrap optimism-corrected performance

# lr22
m = 10
B = 200

auc.op.boot <-c()
calint.op.boot <-c()
calslp.op.boot <-c()
auc.op <-c()
calint.op <-c()
calslp.op <-c()

for (i in 1:m) {
  df_sub <- subset(mi_lda22, .imp == i)
  df_sub <- df_sub[, -c(1:2)]
  
  set.seed(1)
  # bootstrap
  for (j in 1:B) {
    boots <- sample(nrow(df_sub), replace = TRUE)
    boot_sub <- df_sub[boots, ]
    
    # model on boot samples
    lr.boot <- lrm(LDA ~ ., data = boot_sub, x = T, y = T)
    penalized <- pentrace(lr.boot, seq(0,200,0.1))
    lr.pen.boot <- update(lr.boot, penalty = penalized$penalty)
    
    # test performance of the boot model (df_sub, the MI samples)
    logit.test <- predict(lr.pen.boot, df_sub)
    risk.test <- predict(lr.pen.boot, df_sub, type = "fitted")
    auc.test <- auc(roc(df_sub$LDA ~ c(risk.test))) 
    # calibration
    calint.test <- coef(glm(df_sub$LDA ~ offset(logit.test), family = "binomial"))[[1]]
    calslp.test <- coef(glm(df_sub$LDA ~ logit.test, family = "binomial"))[[2]]
    
    # apparent performance of the boot model (boot_sub, bootstrap samples)
    logit.apparent <- predict(lr.pen.boot, boot_sub)
    risk.apparent <- predict(lr.pen.boot, boot_sub, type = "fitted")
    auc.apparent <- auc(roc(boot_sub$LDA ~ c(risk.apparent))) 
    calint.apparent <- coef(glm(boot_sub$LDA ~ offset(logit.apparent), family = "binomial"))[[1]]
    calslp.apparent <- coef(glm(boot_sub$LDA ~ logit.apparent, family = "binomial"))[[2]]
    
    # optimism
    auc.op.boot[j] <- auc.apparent - auc.test
    calint.op.boot[j] <- calint.apparent - calint.test 
    calslp.op.boot[j] <- calslp.apparent - calslp.test
    
    print(paste0(i, "_", j))
  }
  auc.op[[i]] <- mean(auc.op.boot)
  calint.op[[i]] <- mean(calint.op.boot)
  calslp.op[[i]] <- mean(calslp.op.boot)
}

performance$AUC.LR22[2] <- mean(unlist(auc.op)) 
performance$CALint.LR22[2] <- mean(unlist(calint.op))
performance$CALslp.LR22[2] <- mean(unlist(calslp.op))


# lr16
m = 10
B = 200

auc.op.boot <-c()
calint.op.boot <-c()
calslp.op.boot <-c()
auc.op <-c()
calint.op <-c()
calslp.op <-c()

for (i in 1:m) {
  df_sub <- subset(mi_lda16, .imp == i)
  df_sub <- df_sub[, -c(1:2)]
  
  set.seed(1)
  # bootstrap
  for (j in 1:B) {
    boots <- sample(nrow(df_sub), replace = TRUE)
    boot_sub <- df_sub[boots, ]
    
    # model on boot samples
    lr.boot <- lrm(LDA ~ ., data = boot_sub, x = T, y = T)
    penalized <- pentrace(lr.boot, seq(0,200,0.1))
    lr.pen.boot <- update(lr.boot, penalty = penalized$penalty)
    
    # test performance of the boot model (df_sub, the MI samples)
    logit.test <- predict(lr.pen.boot, df_sub)
    risk.test <- predict(lr.pen.boot, df_sub, type = "fitted")
    auc.test <- auc(roc(df_sub$LDA ~ c(risk.test))) 
    calint.test <- coef(glm(df_sub$LDA ~ offset(logit.test), family = "binomial"))[[1]]
    calslp.test <- coef(glm(df_sub$LDA ~ logit.test, family = "binomial"))[[2]]
    
    # apparent performance of the boot model (boot_sub, bootstrap samples)
    logit.apparent <- predict(lr.pen.boot, boot_sub)
    risk.apparent <- predict(lr.pen.boot, boot_sub, type = "fitted")
    auc.apparent <- auc(roc(boot_sub$LDA ~ c(risk.apparent))) 
    calint.apparent <- coef(glm(boot_sub$LDA ~ offset(logit.apparent), family = "binomial"))[[1]]
    calslp.apparent <- coef(glm(boot_sub$LDA ~ logit.apparent, family = "binomial"))[[2]]
    
    # optimism
    auc.op.boot[j] <- auc.apparent - auc.test ## for AUC, test is supposed to be smaller (worse)
    calint.op.boot[j] <- calint.apparent - calint.test 
    calslp.op.boot[j] <- calslp.apparent - calslp.test
    
    print(paste0(i, "_", j))
  }
  auc.op[[i]] <- mean(auc.op.boot)
  calint.op[[i]] <- mean(calint.op.boot)
  calslp.op[[i]] <- mean(calslp.op.boot)
}

performance$AUC.LR16[2] <- mean(unlist(auc.op)) 
performance$CALint.LR16[2] <- mean(unlist(calint.op))
performance$CALslp.LR16[2] <- mean(unlist(calslp.op))


# LASSO

auc.min.op.boot <-c()
auc.1se.op.boot <-c()
calint.min.op.boot <-c()
calint.1se.op.boot <-c()
calslp.min.op.boot <-c()
calslp.1se.op.boot <-c()

auc.min.op <-c()
auc.1se.op <-c()
calint.min.op <-c()
calint.1se.op <-c()
calslp.min.op <-c()
calslp.1se.op <-c()

## different calibration method
for (i in 1:m) {
  df_sub <- subset(mi_lda22, .imp == i)
  df_sub <- df_sub[, -c(1:2)]
  lr.sub <- lrm(LDA ~ ., data = df_sub, x = T, y = T)
  
  set.seed(1)
  # bootstrap
  for (j in 1:B) {
    boots <- sample(nrow(df_sub), replace = TRUE)
    boot_sub <- df_sub[boots, ]
    
    lr.boot <- lrm(LDA ~ ., data = boot_sub, x = T, y = T)
    cv.lasso.boot <- cv.glmnet(x = lr.boot$x, y = lr.boot$y, alpha = 1, 
                               family = c("binomial"), type.measure = "auc")
    lasso.min.boot <- glmnet(x = lr.boot$x, y = lr.boot$y, alpha = 1,
                             lambda = cv.lasso.boot$lambda.min, family = c("binomial"))
    lasso.1se.boot <- glmnet(x = lr.boot$x, y = lr.boot$y, alpha = 1,
                             lambda = cv.lasso.boot$lambda.1se, family = c("binomial"))
    
    # test performance of the boot model (df_sub, the MI samples)
    logit.min.test <- predict(lasso.min.boot, newx = lr.sub$x)
    logit.1se.test <- predict(lasso.1se.boot, newx = lr.sub$x)
    risk.min.test <- predict(lasso.min.boot, newx = lr.sub$x, type = "response")
    risk.1se.test <- predict(lasso.1se.boot, newx = lr.sub$x, type = "response")
    auc.min.test <- auc(roc(df_sub$LDA ~ c(risk.min.test)))
    auc.1se.test <- auc(roc(df_sub$LDA ~ c(risk.1se.test)))
    # calibration 
    calint.min.test <- coef(glm(df_sub$LDA ~ offset(logit.min.test), family = "binomial"))[[1]]
    calint.1se.test <- coef(glm(df_sub$LDA ~ offset(logit.1se.test), family = "binomial"))[[1]]
    calslp.min.test <- coef(glm(df_sub$LDA ~ logit.min.test, family = "binomial"))[[2]]
    calslp.1se.test <- coef(glm(df_sub$LDA ~ logit.1se.test, family = "binomial"))[[2]]
    
    # apparent performance (boot_sub, bootstrap samples)
    logit.min.apparent <- predict(lasso.min.boot, newx = lr.boot$x)
    logit.1se.apparent <- predict(lasso.1se.boot, newx = lr.boot$x)
    risk.min.apparent <- predict(lasso.min.boot, newx = lr.boot$x, type = "response")
    risk.1se.apparent <- predict(lasso.1se.boot, newx = lr.boot$x, type = "response")
    auc.min.apparent <- auc(roc(boot_sub$LDA ~ c(risk.min.apparent)))
    auc.1se.apparent <- auc(roc(boot_sub$LDA ~ c(risk.1se.apparent)))
    # calibration
    calint.min.apparent <- coef(glm(boot_sub$LDA ~ offset(logit.min.apparent), family = "binomial"))[[1]]
    calint.1se.apparent <- coef(glm(boot_sub$LDA ~ offset(logit.1se.apparent), family = "binomial"))[[1]]
    calslp.min.apparent <- coef(glm(boot_sub$LDA ~ logit.min.apparent, family = "binomial"))[[2]]
    calslp.1se.apparent <- coef(glm(boot_sub$LDA ~ logit.1se.apparent, family = "binomial"))[[2]]
    
    # optimism
    auc.min.op.boot[j] <- auc.min.apparent - auc.min.test
    auc.1se.op.boot[j] <- auc.1se.apparent - auc.1se.test
    calint.min.op.boot[j] <- calint.min.apparent - calint.min.test
    calint.1se.op.boot[j] <- calint.1se.apparent - calint.1se.test
    calslp.min.op.boot[j] <- calslp.min.apparent - calslp.min.test
    calslp.1se.op.boot[j] <- calslp.1se.apparent - calslp.1se.test 
  
    print(paste0(i, "_", j))
  }
  auc.min.op[[i]] <- mean(auc.min.op.boot)
  auc.1se.op[[i]] <- mean(auc.1se.op.boot)
  calint.min.op[[i]] <- mean(calint.min.op.boot)
  calint.1se.op[[i]] <- mean(calint.1se.op.boot)
  calslp.min.op[[i]] <- mean(calslp.min.op.boot)
  calslp.1se.op[[i]] <- mean(calslp.1se.op.boot)
}

performance$AUC.LASSOmin22[2] <- mean(unlist(auc.min.op))
performance$AUC.LASSO1se22[2] <- mean(unlist(auc.1se.op))
performance$CALint.LASSOmin22[2] <- mean(unlist(calint.min.op))
performance$CALint.LASSO1se22[2] <- mean(unlist(calint.1se.op))
performance$CALslp.LASSOmin22[2] <- mean(unlist(calslp.min.op))
performance$CALslp.LASSO1se22[2] <- mean(unlist(calslp.1se.op))


performance.l <- as.data.frame(t(performance))
performance.l$boot_corrected <- performance.l$apparent - performance.l$boot_optimism


