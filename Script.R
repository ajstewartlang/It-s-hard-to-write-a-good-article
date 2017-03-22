#install the lme4, lsmeans and lmertest packages first
install.packages ("lme4")
install.packages ("lmerTest") 
install.packages ("lsmeans")
library (lme4)
library (lmerTest)
library(lsmeans) 

#C1 = Neutral condition
#C2 = Negative condition
#C3 = Positive condition

# The critical region is region 3 in the data files
# The post-critical reigon is region 4 in the data files

#note zeros in the reading time measures reflects skipped regions or tracker loss.  They are excluded from the reading time analyses as they are meaningless.  They are meaningful in the regression out analyses so are kept in for that.

#Read in First Pass Data
fpfp0 <- read.csv("~/fpfp0.ixs")

fpfp0$cond <- as.factor (fpfp0$cond)

#Critical Region
index <- fpfp0$DV > 0 & fpfp0$reg == "R3"                                             # this removes invalid trials and only select region 3 data.
fpfp0$cond <- relevel (fpfp0$cond, ref = 2)                                          # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 + cond|subj) + (1 + cond|item), data = fpfp0[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 + cond|subj) + (1 + cond|item), data = fpfp0[index,], REML=TRUE)
anova(model.null, model.full)
summary (model.full)

#Post-Critical Region
index <- fpfp0$DV > 0 & fpfp0$reg == "R4"                                             # this removes invalid trials and only select region 4 data.
fpfp0$cond <- relevel (fpfp0$cond, ref = 2)                                          # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 +cond |subj) + (1 +cond|item), data = fpfp0[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 |subj) + (1 +cond|item), data = fpfp0[index,], REML=TRUE)
summary (model.full)

#Read in Regression Path Data
GPgp1 <- read.csv("~/GPgp1.ixs")

GPgp1$cond <- as.factor (GPgp1$cond)

#Critical Region
index <- GPgp1$DV > 0 & GPgp1$reg == "R3"                                             # this removes invalid trials and only select region 3 data.
GPgp1$cond <- relevel (GPgp1$cond, ref = 2)                                          # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 + cond|subj) + (1 + cond|item), data = GPgp1[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 + cond|subj) + (1 + cond|item), data = GPgp1[index,], REML=TRUE)
anova(model.null, model.full)
summary (model.full)

#Post Critical Region
index <- GPgp1$DV > 0 & GPgp1$reg == "R4"                                             # this removes invalid trials and only select region 4 data.
GPgp1$cond <- relevel (GPgp1$cond, ref = 2)                                          # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 + cond|subj) + (1 + cond|item), data = GPgp1[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 + cond|subj) + (1 + cond|item), data = GPgp1[index,], REML=TRUE)
anova(model.null, model.full)
summary (model.full)

#Read in Total Time Data
TTtt0 <- read.csv("~/TTtt0.ixs")

TTtt0$cond <- as.factor (TTtt0$cond)

#Critical Region
index <- TTtt0$DV > 0 & TTtt0$reg == "R3"                                             # this removes invalid trials and only select region 3 data.
TTtt0$cond <- relevel (TTtt0$cond, ref = 2)                                          # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 + cond|subj) + (1 + cond|item), data = TTtt0[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 + cond|subj) + (1 + cond|item), data = TTtt0[index,], REML=TRUE)
anova(model.null, model.full)
summary (model.full)

#Post-Critical Region
index <- TTtt0$DV > 0 & TTtt0$reg == "R4"                                            # this removes invalid trials and only select region 4 data.
TTtt0$cond <- relevel (TTtt0$cond, ref = 2)                                         # relevel so that the Negative condition is the reference level in the model
model.null <- lmer(DV ~ (1 + cond|subj) + (1 + cond|item), data = TTtt0[index,], REML=TRUE)
model.full <- lmer(DV ~ cond + (1 + cond|subj) + (1 + cond|item), data = TTtt0[index,], REML=TRUE)
anova(model.null, model.full)
summary (model.full)

#Read in FPRO Data
FPRO <- read.delim("~/FPRO_correct.ixs")

FPRO$cond <- as.factor (FPRO$cond)

#F1 model names are by-participants, F2 by-items

#Critical Region
index <-  FPRO$reg == "R3"                                            # this selects region 3 data.
FPRO$cond <- relevel (FPRO$cond, ref = 2)                            # relevel so that the Negative condition is the reference level in the model
modelF1 <- glmer(DV ~ cond + (1 + cond|subj) , data = FPRO [index,], family=binomial) 
summary (modelF1)

index <- FPRO$reg == "R3"                                            # this selects region 3 data.
FPRO$cond <- relevel (FPRO$cond, ref = 2)                            # relevel so that the Negative condition is the reference level in the model
modelF2 <- glmer(DV ~ cond + (1 |item), data = FPRO[index,], family=binomial) 
summary (modelF2)


#Post Critical Region
index <- FPRO$reg == "R4"                                            # this selects region 4 data.
FPRO$cond <- relevel (FPRO$cond, ref = 2)                          # relevel so that the Negative condition is the reference level in the model
modelF1 <- glmer(DV ~ cond + (1 + cond|subj) , data = FPRO[index,], family=binomial) 
summary (modelF1)

index <- FPRO$reg == "R4"                                          # this selects region 4 data.
FPRO$cond <- relevel (FPRO$cond, ref = 2)                         # relevel so that the Negative condition is the reference level in the model
modelF2 <- glmer(DV ~ cond + (1 |item), data = FPRO[index,], family=binomial) 
summary (modelF2)