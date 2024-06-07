library(rstan)
library(bayesplot)
library(ggplot2)
library(loo)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load(file = "DataTridac2024_1.Rdata")
load(file = "HdVAll_Pedigree2.Rdata")
load(file = "HdVAllBinarized_Pedigree2.Rdata")
load(file = "Data_LRS_2PC.RDATA")
load(file = "DATA_LRS_TAL1_FN.RDATA")

Coolors<- c("#8ecae6", "#219EBC", "#023047", "#FFB703", "#FB8500")

df<- Data1_2

HdVTAL1


first <- apply(HdVTAL1, 1, function(x) min(which(x != 0 )))

last<- apply(HdVTAL1, 1, function(x) max(which(x != 0)))

lo <- last - first

head(lo)

head(df)
last




DATA<- as.data.frame(D_LRS_TAL1)

head(DATA)
nrow(DATA)

plot(DATA)


class(DATA$LRS)

DATA$MotherAge<- as.integer(DATA$MotherAge)
DATA$FatherAge<- as.integer(DATA$FatherAge)
DATA$LRS<- as.integer(DATA$LRS)
attach(DATA)

DATA2<- DATA
DATA2[DATA2$FatherAge >= 15, 1] <- 15 #subset(DATA, DATA$MotherAge<=20)
nrow(DATA2)



colnames(DATA2)[9]<- 'SexChick'

DATA2$SexChick

DATA3<- subset(DATA2, DATA2$SexChick != '9')

nrow(DATA3)

DATA3$SexChick

SexB<- DATA3$SexChick
SexB[SexB == 'M']<- 0
SexB[SexB == 'F']<- 1


SexB<- as.numeric(SexB)
SexB

sum(SexB)
length(SexB) - sum (SexB)

# SexC<- DATA3$SexChick
# SexC[SexC == 'M']<- 1
# SexC[SexC == 'F']<- 0
# 
# 
# SexC<- as.numeric(SexC)
# SexC
# 


DATA4<- DATA3
DATA4[DATA4$FatherAge >= 15, 3 ] <- 15 #subset(DATA, DATA$MotherAge<=20)
nrow(DATA4)

plot(DATA4$LRS~SexB)



hist(DATA4$LRS, main = "LRS distribution", xlab = "LRS", breaks = 25)

#cs effect----
# 
# data = list(N = nrow(DATA4),x = SexB, y = DATA4$LRS)
# 
# fit_ZIPCS0 = stan(file = 'reglin.stan',
#                   data = data,
#                   chains = 3,             
#                   warmup = 10000,
#                   iter = 20000,)
# 
# 
# summary(fit_ZIPCS0, pars = c("alpha","beta"))
# rhat(fit_ZIPCS0, pars = c("alpha","beta", "theta"))
# params <- extract(fit_ZIPCS0)
# alpha<- mean(params$alpha)
# beta<- mean(params$beta)
# 
# 
# (ggplot(data = DATA4, mapping = aes( x  = DATA4$LRS, group = SexB, fill = SexB))
#   + geom_bar(position = "dodge")
#   # + geom_point( y = exp( alpha + beta*DATA2$FatherAge) , color = "red")
# )





#########################models----


data = list(N = nrow(DATA4), x = DATA4$FatherAge, y = DATA4$LRS, kp = 12, Sex = SexB)#, betakp_lower = 1, betakp_upper = 20)

fit_BS12CS = stan(file = 'BS_MN2_Sex.stan',
               data = data,
               chains = 3,             
               warmup = 10000,
               iter = 20000,)



data = list(N = nrow(DATA4), x = DATA4$FatherAge, y = DATA4$LRS)

fit_ZIP = stan(file = 'ZIP_MN.stan',
               data = data,
               chains = 3,             
               warmup = 10000,
               iter = 20000,)





data = list(N = nrow(DATA4), x = DATA4$FatherAge, y = DATA4$LRS, Sex = SexB)#, betakp_lower = 1, betakp_upper = 20)

fit_ZIPCS = stan(file = 'ZIP_MN_CS.stan',
               data = data,
               chains = 3,             
               warmup = 10000,
               iter = 20000,)



data = list(N = nrow(DATA4), y = DATA4$LRS, Sex = SexB)

fit_0 = stan(file = 'ZIP_0.stan',
             data = data,
             chains = 3,             
             warmup = 10000,
             iter = 20000,)



##################averaging----


model_list<- list(fit_BS12CS, fit_ZIP, fit_ZIPCS, fit_0)
log_lik_list <- lapply(model_list, extract_log_lik)


r_eff_list <- lapply(model_list, function(x) {
  ll_array <- extract_log_lik(x, merge_chains = FALSE)
  relative_eff(exp(ll_array))
})




wts1 <- loo_model_weights(
  log_lik_list,
  method = "stacking",
  r_eff_list = r_eff_list,
  optim_control = list(reltol=1e-10)
)
print(wts1)

#########################################
detach(package:tidyr)
M1<- wts1[1]
M2<- wts1[2]
M3<- wts1[3]
M4<- wts1[4]


P1<- extract(fit_BS12CS)
P1_alphaM<- mean(P1$alpha_1)
P1_alphaF<- mean(P1$alpha_2) + mean(P1$alpha_1)
P1_betaM<- mean(P1$beta_1)
P1_betaF<- mean(P1$beta_2) + mean(P1$beta_1)

P2<- extract(fit_ZIP)
P2_alphaM<- mean(P2$alpha)
P2_alphaF<- mean(P2$alpha)
P2_betaM<- mean(P2$beta)
P2_betaF<- mean(P2$beta)

P3<- extract(fit_ZIPCS)
P3_alphaM<- mean(P3$alpha)
P3_alphaF<- mean(P3$alpha_2)+mean(P3$alpha)
P3_betaM<- mean(P3$beta)
P3_betaF<- mean(P3$beta_2)+mean(P3$beta)


P4<- extract(fit_0)
P4_alphaM<- mean(P4$alpha)
P4_alphaF<- mean(P4$beta)+mean(P4$alpha)

alphaF<- P1_alphaF*M1 + P2_alphaF*M2 + P3_alphaF * M3+ P4_alphaF *M4 
alphaM<- P1_alphaM*M1 + P2_alphaM*M2 + P3_alphaM*M3+P4_alphaM*M4

betaF<- P1_betaF*M1 + P2_betaF*M2 + P3_betaF*M3
betaM<- P1_betaM*M1 + P2_betaM*M2 + P3_betaM*M3

x = seq(2.5, 22 , 0.5)
y = seq(0,3.9, 0.1)

tv1<- as.data.frame(matrix(data = c(x, y,(alphaF+  betaF*x) , (alphaM+  betaM*x)), ncol = 4))

(ggplot(data = tv1, mapping = aes(x = V1, y = V2 ))
  + geom_line(y = tv1$V3, color = 'red')
  + geom_line(y =  tv1$V4, color = 'blue')
  
)

plot(x = tv1$V1, y = tv1$V2, col = "white",
     main = 'LRS function of  the Fater Age',
     xlab = 'Father Age', ylab = 'LRS',
     abline(a = alphaF, b = betaF, col = Coolors[1], lwd = 3),
     
     )
abline(a = alphaM, b = betaM, col = Coolors[5], lwd = 3)


save(alphaF, alphaM, betaF, betaM,P1, P2, P3, wts1, file = "muLRSFN.RDATA")
load("muLRSFN.RDATA")
