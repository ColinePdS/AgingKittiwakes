library(rstan)
library(bayesplot)
library(ggplot2)
library(loo)
library(data.table)
library(tidyverse)
library(cmdstanr)

# cmdstanr::install_cmdstan()
# cmdstanr::check_cmdstan_toolchain(fix = TRUE)
# detach('package:rstan')
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

load(file = "DataTridac2024_1.Rdata")
load(file = "DATA_LRS_TAL1_MN.RDATA")

Data1_2
HdVTAL1
D_LRS_TAL1

usefullF<-D_LRS_TAL1[,c(1,3,6,7, 8,9)] 

dfgA<- cbind(HdVTAL1, usefullF)

vector<- dfgA[1,]

getAttempt<- function(vector){
  
  nb<- vector[1]
  hdv<- vector[2:46]
  ID<- vector[47:52]
  
  first <-  min(which(hdv != 0 ))+1

  last<- max(which(hdv != 0))

  alive<- hdv[first:last]
  
  lo<- length(alive)
  
  tv<- matrix(data = 0, nrow = lo, ncol =12)
  tv<- as.data.frame(tv)
  
  tv[,1]<- nb[1]
  
  tv[,2]<- ID[3]
  
  tv[,3] <- as.numeric(names(hdv[first:last]))
  
  tv[,4]<- as.matrix(hdv)[first:last]
  
  tv[,7]<- as.matrix(hdv)[first:last]
  tv[,5]<- as.matrix(hdv)[first:last]
  tv[,6]<- as.matrix(hdv)[first:last]
  tv[,8]<- as.matrix(hdv)[first:last]
 
  Cage<- c(1:lo)
  
  tv[,9]<- Cage
  
  
  tv[,10]<- ID[1]
  tv[,11]<- ID[2]
  tv[,12]<- ID[6]
  
  # tv<- as.data.frame(tv)
  
  colnames(tv)<- c("nb", "ID", "Y","code", "A", "S", "N","code2", "C_age", "M_Age", "F_Age","CS")
  
  
  return (tv)
  
}



dfgA2<-  apply(dfgA, 1, getAttempt)
dfgA2$'7'


dfgA3<- as.data.frame(rbindlist(dfgA2[1:566]))

class(dfgA3)

dfgA4<- subset(dfgA3, dfgA3$A != 0)

vector<- dfgA4[5,]

getA<- function(vector){
  A<- vector[5]
  
  if (str_detect(A, "BA")&&!str_detect(A, "NBA")){
    vector[5]<- 1
  }else{
    vector[5]<- 0
  }
  
  
  if (vector[5]==1&&!str_detect(A, "0")){
    vector[6]<- 1
  }else{
    vector[6]<- 0
  } 
  
 
  if (vector[6]==1 && as.numeric(str_sub(vector[7], 3, 4)>=2)){
    vector[7]<- 1
  }else{
    vector[7]<- 0
  } 
  

  if (vector[7]==1){
    vector[8]<- 4
  }
  
  
  if (vector[7]==0){
    vector[8]<- 3
  }
  
  
  if (vector[6]==0){
    vector[8]<- 2
  }
  
  
  if (vector[5]==0){
    vector[8]<- 1
  }
  
  return(vector)
}


dfA <- as.data.frame(matrix(apply(dfgA4, 1, getA), nrow = nrow(dfgA4), ncol = 12, byrow = T))


colnames(dfA)<- c("nb", "ID", "Y", "code", "A", "S", "N","code2", "C_age", "M_Age", "F_Age", "CS")


head(dfA)

dfA2<- subset(dfA, dfA$code!="NBS")


dfA2


length(unique(dfA2$ID))

K <- 4
N<- nrow(dfA2)
D<- 2
y<- as.numeric(dfA2$code2)

x<- (cbind(as.numeric(dfA2$C_age),as.numeric( dfA2$M_Age)))



data = list(K = K, N = N, D = D,  x = x, y = y)


fit_PLR1 = stan(file = 'PLR.stan',
                  data = data,
                  chains = 2,
                  warmup = 10000,
                  iter = 20000,)


detach("package:tidyverse")
detach(package:tidyr)
library(rstan)

summary(fit_PLR1)$summary[,1]

rhat(fit_PLR1)

params <- extract(fit_PLR1)

beta<- mean(params$beta)

plot(fit_PLR1)
traceplot(fit_PLR1)

beta<- matrix(data =summary(fit_PLR1)$summary[(1:8),1], ncol = 4, nrow = 2, byrow = T )


beta

rownames(beta)<- c("C_age", "F_age")
colnames(beta)<- c(1:4)
beta


save(fit_PLR1, file = "fit_PLR1.RDATA")
load( file = "fit_PLR1.RDATA")

#########################brms----

# install.packages("brms")


library(brms)

# fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
#             data = epilepsy, family = poisson())


dfA2

# dfA3<- dfA2
# 
# dfA3$C_age[dfA3$C_age<=5] <- 0
# dfA3$C_age[dfA3$C_age>=6] <- 1
# 
# dfA3$F_Age[dfA3$F_Age<=5] <- 0
# dfA3$F_Age[dfA3$F_Age>=6] <- 1
# 


# head(dfA3)

class(dfA2$C_age)

dfA2$C_age<- as.numeric(dfA2$C_age) 

dfA2$M_Age<- as.numeric(dfA2$M_Age)

fitA<- brm(formula = A~C_age+ M_Age,
           data = dfA2,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


plot(fit1)

print(fit1)

plot(fitA)

print(fitA)



dfA2

dfS <- subset(dfA2, dfA2$A==1)
dfN <-subset(dfA2, dfA2$S==1)

dfS





fitS<- brm(formula = S~C_age+ M_Age,
           data = dfS,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


plot(fitS)
print(fitS)





fitN<- brm(formula = N~C_age+ M_Age,
           data = dfN,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


plot(fitN)
print(fitN)




save(fitA, fitS, fitN, file = "fitreproMN")



load('fitreproMN')


estA<- fixef(fitA)[,1]

estS<- fixef(fitS)[,1]
estN<- fixef(fitN)[,1]



library(lattice)
Coolors<- c("#023047", "#219EBC","#8ecae6" , "#FFB703", "#FB8500")
CoolP100<- grDevices::colorRampPalette(Coolors)(100)



x <- seq(3, 12, length=30)
# x = age M
y <- seq(3, 12, length=30)
# y = age F
g <- expand.grid(x = x, y = y)


g$z <- (estA[1]+ estA[2] *g$x+ estA[3] * g$y)
g$z<- (1/(1+exp(- (g$z))))
wireframe(z ~ x * y, g, drape = TRUE,xlab = "Chick age", ylab = "MotherAge", zlab = "Attempt",
          colorkey = TRUE,col.regions = CoolP100)





x <- seq(3, 25, length=30)
# x = age M
y <- seq(3, 25, length=30)
# y = age F
h <- expand.grid(x = x, y = y)


h$z <- (estS[1]+ estS[2] *h$x+ estS[3] * h$y)
h$z <- (1/(1+exp(-h$z)))
wireframe(z ~ x * y, h, drape = TRUE,xlab = "Chick age", ylab = "MotherAge", zlab = "Succes",
          colorkey = TRUE,col.regions = CoolP100)





x <- seq(3, 25, length=30)
# x = age M
y <- seq(3, 25, length=30)
# y = age F
k <- expand.grid(x = x, y = y)


k$z <- (estN[1]+ estN[2] *k$x+ estN[3] * k$y)
k$z<- 1/(1+exp(- k$z))
wireframe(z ~ x * y, k, drape = TRUE,xlab = "Chick age", ylab = "MotherAge", zlab = "<1 chick",
          colorkey = TRUE,col.regions = CoolP100)






load('fitreproFN')


estA<- fixef(fitA)[,1]

estS<- fixef(fitS)[,1]
estN<- fixef(fitN)[,1]



library(lattice)




x <- seq(3, 12, length=30)
# x = age M
y <- seq(3, 12, length=30)
# y = age F
g <- expand.grid(x = x, y = y)


g$z <- (estA[1]+ estA[2] *g$x+ estA[3] * g$y)
g$z<- (1/(1+exp(- (g$z))))
wireframe(z ~ x * y, g, drape = TRUE,xlab = "Chick age", ylab = "FatherAge", zlab = "Attempt",
          colorkey = TRUE,)





x <- seq(3, 25, length=30)
# x = age M
y <- seq(3, 25, length=30)
# y = age F
h <- expand.grid(x = x, y = y)


h$z <- (estS[1]+ estS[2] *h$x+ estS[3] * h$y)
h$z <- (1/(1+exp(-h$z)))
wireframe(z ~ x * y, h, drape = TRUE,xlab = "Chick age", ylab = "FatherAge", zlab = "Succes",
          colorkey = TRUE,)





x <- seq(3, 25, length=30)
# x = age M
y <- seq(3, 25, length=30)
# y = age F
k <- expand.grid(x = x, y = y)


k$z <- (estN[1]+ estN[2] *k$x+ estN[3] * k$y)
k$z<- 1/(1+exp(- k$z))
wireframe(z ~ x * y, k, drape = TRUE,xlab = "Chick age", ylab = "FatherAge", zlab = "<1 chick",
          colorkey = TRUE,)



############################## CS----

dfA2$CS<- as.factor(dfA2$CS)

class(dfA3$C_age)

dfA3<- dfA2[dfA2$CS!= 9,]


dfA3$M_Age<- as.numeric(dfA3$M_Age)
dfA3$C_age<- as.numeric(dfA3$C_age)

nrow(dfA3)
class(dfA3$CS)

length(unique(dfA3$ID))#419


fitPA_A<- brm(formula = A ~ M_Age,
              data = dfA3,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)

WAIC_PA_M<- waic(fitPA_A)


fitCS_A<- brm(formula = A ~ M_Age*CS,
              data = dfA3,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


plot(fitCS_A)

print(fitCS_A)

WAIC_CS_A<- waic(fitCS_A)





fitA<- brm(formula = A~C_age+ M_Age,
           data = dfA3,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


print(fitA)

WAIC_CA_A<- waic(fitA)


fitTotA<- brm(formula = A ~ C_age+ M_Age*CS,
              data = dfA3,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


WAIC_TOT_A<- waic(fitTotA)


fitH0<- brm(formula = A~C_age,
            data = dfA3,
            family = bernoulli(link = "logit"),
            iter=20000, warmup = 10000)


print(fitH0)

WAIC_H0<- waic(fitH0)

fitTotA

EstA<- fixef(fitTotA)[,1]



library(lattice)


x <- seq(3, 12, length=30)
# x = age M
y <- seq(3, 12, length=30)
# y = age F
g <- expand.grid(x = x, y = y)


g$z <- (EstA[1]+ EstA[2] *g$x+ (EstA[3]+EstA[5]) * g$y)
g$z<- (1/(1+exp(- (g$z))))
wireframe(z ~ x * y, g, drape = TRUE,xlab = "Chick age", ylab = "FatherAge", zlab = "Attempt",
          colorkey = TRUE,col.regions = CoolP100)





g <- expand.grid(x = x, y = y)


g$z <- (EstA[1]+ EstA[2] *g$x+ (EstA[3]) * g$y)
g$z<- (1/(1+exp(- (g$z))))
wireframe(z ~ x * y, g, drape = TRUE,xlab = "Chick age", ylab = "FatherAge", zlab = "Attempt",
          colorkey = TRUE,col.regions = CoolP100)





save(fitCS_A, fitTotA,fitH0,fitA, WAIC_CS_A, WAIC_CA_A, WAIC_TOT_A,WAIC_H0 , file = "fitCS_1_MN.RDATA")




df3S <- subset(dfA3, dfA3$A==1)
df3N <-subset(dfA3, dfA3$S==1)




fitCS_S<- brm(formula = S ~ M_Age*CS,
              data = df3S,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


plot(fitCS_S)

print(fitCS_S)

WAIC_CS_S<- waic(fitCS_S)





fitS<- brm(formula = S~C_age+ M_Age,
           data = df3S,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


print(fitS)

WAIC_CA_S<- waic(fitS)


fitTotS<- brm(formula = S ~ C_age+ M_Age*CS,
              data = df3S,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


WAIC_TOT_S<- waic(fitTotS)


fitH0S<- brm(formula = S~C_age,
             data = df3S,
             family = bernoulli(link = "logit"),
             iter=20000, warmup = 10000)


print(fitH0S)

WAIC_H0_S<- waic(fitH0S)

fitTotA

EstA<- fixef(fitTotA)[,1]


fitPA_N<- brm(formula = N ~ M_Age,
              data = df3N,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)

WAIC_PA_M<- waic(fitPA_N)



fitCS_N<- brm(formula = N ~ M_Age*CS,
              data = df3N,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


plot(fitCS_S)

print(fitCS_S)




WAIC_CS_N<- waic(fitCS_N)





fitN<- brm(formula = N~C_age+ M_Age,
           data = df3N,
           family = bernoulli(link = "logit"),
           iter=20000, warmup = 10000)


print(fitN)

WAIC_CA_N<- waic(fitN)


fitTotN<- brm(formula = N ~ C_age+ M_Age*CS,
              data = df3N,
              family = bernoulli(link = "logit"),
              iter=20000, warmup = 10000)


WAIC_TOT_N<- waic(fitTotN)


fitH0N<- brm(formula = N~C_age,
             data = df3N,
             family = bernoulli(link = "logit"),
             iter=20000, warmup = 10000)


print(fitH0S)

WAIC_H0_N<- waic(fitH0N)

save(fitCS_S,fitS, fitTotS,fitH0S, WAIC_CS_S, WAIC_CA_S, WAIC_TOT_S,WAIC_H0_S , file = "fitCS_S_MN.RDATA")

save(fitCS_N,fitN, fitTotN,fitH0N, WAIC_CS_N, WAIC_CA_N, WAIC_TOT_N,WAIC_H0_N , file = "fitCS_N_MN.RDATA")








