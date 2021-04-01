library(readxl)
library(lubridate)
library(survival)
library(Matching)
library(knitr)
library(kableExtra)
library(dplyr)
library(pander)
library(xtable)
library(tab)
library(memisc)
library(stargazer)
library(jtools)
library(cobalt)
library(Rglpk)
library(slam)
library(MASS)
library(lattice)
library(ebal)
library(plm)
library(pglm)
library(splines)
library(tidyverse)
library(mgcv)
library("writexl")
library(broom)
library(progress)


load("C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/data1.Rda")


# creation d'un variable niveau pour critère éligibilité obtenu par les données des concentration d'Airparif

for (i in (1:nrow(data))){
  data$Niveau_elig[i]<- if (data$elig_Inf[i]==1 & data$elig_Al[i]==1) "Alerte" else 
    if (data$elig_Inf[i]==1 & data$elig_Al[i]==0) "Information" else NA
}

for (i in (1:nrow(data))){
  data$Niveau_picPM10[i]<- if (data$Dates[i]<="2007-12-31") data$Niveau_elig[i] else data$`Niveau PM10`[i]
}


# Creation d'une variable indicatrice de période: 

data$annee<-as.numeric(as.character(data$annee))
data$period<-NA
for (i in 1:nrow(data)) {
  data$period[i]<-if (data$Dates[i]<"2008-01-01") "1" 
  else if (data$Dates[i]>="2008-01-01" & data$Dates[i]<"2011-11-30") "2" 
  else if (data$Dates[i]>="2011-11-30") "3"
}

for (i in 1:nrow(data)) {
  data$Period[i]<-if (data$Dates[i]<"2008-01-01") "1" 
  else "2"
}


## Creation variable mois
data$mois<-month(data$Dates)

data$annee<-as.factor(as.character(data$annee))
data$mois<-as.factor(as.character(data$mois))
data$period<-as.factor(as.character(data$period))


# creation dataset jusqu'à année 2015
`%notin%` <- Negate(`%in%`)
data<-data[which(data$annee %notin% c("2016","2017")),]


# transformation variable elipic en variable numerique
data$elipic<-as.numeric(as.character(data$elipic))

# Creation variable pop
pop<-data.frame("annee"=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015), 
                "pop"=c(2125851,2142424,2150382,2158340,2142800,2154000,2181374,2193031,2211297,2234105,2243833,2249975,2240621,2229621,2220445,2206488))
                
data<-merge(data,pop,by="annee",all.y=TRUE)


### **BOOTSTRAP METHOD**

nreps=500 #number of bootstrap reps

# Define model outputs
### Linear Model
### Results for overall population
L_coef_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_cardio_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_respi_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_coef_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_cardio_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_respi_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

### Results for elderly people
L_coef75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_cardio75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_respi75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_coef75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_coef75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_cardio75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_cardio75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

L_respi75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
L_respi75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))


### Poisson Model
### Results for overall population
coef_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

cardio_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

respi_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

coef_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

cardio_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

respi_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

### Results for elderly people
coef75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

cardio75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

respi75_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi75_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi75_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

coef75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
coef75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

cardio75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
cardio75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

respi75_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi75_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
respi75_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))


### Negative Bin
### Results for overall population
NB_coef_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_coef_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_coef_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

NB_cardio_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_cardio_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_cardio_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

NB_respi_period_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_respi_elipic_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_respi_did_P1P2<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

NB_coef_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_coef_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_coef_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

NB_cardio_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_cardio_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_cardio_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))

NB_respi_period_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_respi_elipic_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))
NB_respi_did_P2P3<-data.frame("est"=rep(NA,nreps),"2.5int"=rep(NA,nreps),"97.5int"=rep(NA,nreps))


AF<-c()
MDD<-c()
PD<-c()
AF_cardio<-c()
MDD_cardio<-c()
PD_cardio<-c()
AF_respi<-c()
MDD_respi<-c()
PD_respi<-c()

AF_75<-c()
MDD_75<-c()
PD_75<-c()
AF_cardio75<-c()
MDD_cardio75<-c()
PD_cardio75<-c()
AF_respi75<-c()
MDD_respi75<-c()
PD_respi75<-c()

pb <- progress_bar$new(
  format = "  downloading [:bar] :percent eta: :eta",
  total = 500, clear = FALSE, width= 60)

for (rep in 495:nreps)
  
{
  dat_resample <- sample(1:nrow(data),nrow(data),replace=T)
  dat_boot <- data[dat_resample,]
  
  dat_boot<-dat_boot[order(dat_boot[,"Dates"]),]
  
  dat_boot[,1:5]<-data[,1:5]
  dat_boot$time<-1:nrow(dat_boot)
  
  ### creation lag O3 jours précédents 
  dat_boot$lag_O3<-NA
  dat_boot$lag2_O3<-NA
  dat_boot$lag3_O3<-NA
  
  for (i in 2:(nrow(dat_boot))) {
    dat_boot$lag_O3[i]<-dat_boot$o3[i-1]
  }
  for (i in 3:(nrow(dat_boot))) {
    dat_boot$lag2_O3[i]<-dat_boot$o3[i-2]
  }
  for (i in 4:(nrow(dat_boot))) {
    dat_boot$lag3_O3[i]<-dat_boot$o3[i-3]
  }
  
  
  ### creation lag PM10 jours précédents 
  dat_boot$lag_PM10<-NA
  dat_boot$lag2_PM10<-NA
  dat_boot$lag3_PM10<-NA
  
  
  for (i in 2:(nrow(dat_boot))) {
    dat_boot$lag_PM10[i]<-dat_boot$moyPM10[i-1]
  }
  for (i in 3:(nrow(dat_boot))) {
    dat_boot$lag2_PM10[i]<-dat_boot$moyPM10[i-2]
  }
  for (i in 4:(nrow(dat_boot))) {
    dat_boot$lag3_PM10[i]<-dat_boot$moyPM10[i-3]
  }
  
  
  ### creation lag tempmax jours précédents 
  dat_boot$lag_tempmax<-NA
  dat_boot$lag2_tempmax<-NA
  dat_boot$lag3_tempmax<-NA
  
  
  for (i in 2:(nrow(dat_boot))) {
    dat_boot$lag_tempmax[i]<-dat_boot$tempmax[i-1]
  }
  for (i in 3:(nrow(dat_boot))) {
    dat_boot$lag2_tempmax[i]<-dat_boot$tempmax[i-2]
  }
  for (i in 4:(nrow(dat_boot))) {
    dat_boot$lag3_tempmax[i]<-dat_boot$tempmax[i-3]
  }
  
  
  ### creation lag tempmoy jours précédents 
  dat_boot$lag_tempmoy<-NA
  dat_boot$lag2_tempmoy<-NA
  dat_boot$lag3_tempmoy<-NA
  
  
  for (i in 2:(nrow(dat_boot))) {
    dat_boot$lag_tempmoy[i]<-dat_boot$tempmoy[i-1]
  }
  for (i in 3:(nrow(dat_boot))) {
    dat_boot$lag2_tempmoy[i]<-dat_boot$tempmoy[i-2]
  }
  for (i in 4:(nrow(dat_boot))) {
    dat_boot$lag3_tempmoy[i]<-dat_boot$tempmoy[i-3]
  }
  
  
  ### Propensity score computation
  
  mod<-gam(elipic ~ s(tempmax,k=7,bs="tp") + s(tempmoy,k=7, bs="tp")+s(lag_tempmax,k=7,bs="tp")+ s(lag_tempmoy,k=7, bs="tp")+ s(lag2_tempmax,k=7,bs="tp") + s(lag2_tempmoy,k=7, bs="tp") + s(lag3_tempmax,k=7,bs="tp") + s(lag3_tempmoy,k=7, bs="tp") + Jours + mois  + jours_fériés + Vacances + annee + s(lag_O3,k=6) + s(lag2_O3,k=6) + s(lag3_O3,k=6) + s(lag_PM10,k=6) + s(lag2_PM10,k=6) + s(lag3_PM10,k=6), family="binomial", data=dat_boot)
  
  dat_boot$SP<-NA
  ll_fit<-length(mod$fitted)
  delta_ll<-nrow(dat_boot)-ll_fit+1
  dat_boot$SP[delta_ll:nrow(dat_boot)]<-mod$fitted
  dat_boot<-dat_boot[-c(1:3),]
  dat_boot$nrow<-c(1:nrow(dat_boot))
  
  ### Propensity score Matching
  
  var.match<-cbind(dat_boot$SP,dat_boot$period)
  App<-Match(Tr = dat_boot$elipic,X=var.match, M=3, ties=FALSE)
  
  control=data.frame(App$index.control)
  treated=data.frame(App$index.treated)
  
  colnames(control) <- c("nrow")
  colnames(treated) <- c("nrow")
  
  cas<-unique(treated)
  
  cas$PAIR<-c(1:nrow(cas))
  
  treated<-merge(treated,cas,by="nrow")
  
  control$PAIR <- treated$PAIR
  
  casm<-merge(cas,dat_boot,by="nrow")
  controlm<- merge(control,dat_boot, by ="nrow")
  
  datam<-rbind(casm,controlm)
  
  datam<-datam[order(datam[,"PAIR"]),]
  
  # Convert variable PAIR into factor
  datam$PAIR<-as.factor(as.character(datam$PAIR))
  
  ### Create rates
  datam$nocc_rate=datam$nocc_tot/datam$pop*1000
  datam$cv_rate=datam$cv_tot/datam$pop*1000
  datam$respi_rate=datam$respi_tot/datam$pop*1000
  
  datam$nocc_rate75=datam$nocc_75/datam$pop*1000
  datam$cv_rate75=datam$cv_75/datam$pop*1000
  datam$respi_rate75=datam$respi_75/datam$pop*1000
  
  # Create panel data dataframe 
  datam<-pdata.frame(datam,index="PAIR")
  
  ### Differences in Differences
  
  # Creation dataframe par période et par type appariement 
  datam_p1<-datam[which(datam$period!="3"),]
  datam_p2<-datam[which(datam$period!="1"),]
  
  ### Drop unused levels
  datam_p1$period<-as.factor(as.character(datam_p1$period))
  datam_p2$period<-as.factor(as.character(datam_p2$period))
  

  ### Overall population
  
  ## Non accidental causes 
  # Gaussian Version
  nocc_P1<-plm(nocc_rate ~ period + elipic + period*elipic, data=datam_p1, model="within")
  
  nocc_P2<-plm(nocc_rate ~ period + elipic + period*elipic, data=datam_p2, model="within")
  
  # Poisson Version
  noccP_P1<-pglm(nocc_tot ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  
  noccP_P2<-pglm(nocc_tot ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  
  # nEGATIVE BIN Version
  negbin <- function(link = c('log'), vlink = c('nb1', 'nb2')){
    link <- match.arg(link)
    vlink <- match.arg(vlink)
    list(family = 'negbin', link = link, vlink = vlink)
  }
  
  noccNB_P1<-pglm(nocc_tot ~ period + elipic + period*elipic, data=datam_p1, family=negbin(link = "log"), model="within")
  
  noccNB_P2<-pglm(nocc_tot ~ period + elipic + period*elipic, data=datam_p2, family=negbin, model="within")
  
  ## Cardiovascular causes 
  # Gaussian Version
  cv_P1<-plm(cv_rate ~ period + elipic + period*elipic, data=datam_p1, model="within")
  
  cv_P2<-plm(cv_rate ~ period + elipic + period*elipic, data=datam_p2, model="within")
  
  # Poisson Version
  cvP_P1<-pglm(cv_tot ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  
  cvP_P2<-pglm(cv_tot ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  
  # Negative Binomial Version
  cvNB_P1<-pglm(cv_tot ~ period + elipic + period*elipic, data=datam_p1, family=negbin, model="within")
  
  cvNB_P2<-pglm(cv_tot ~ period + elipic + period*elipic, data=datam_p2, family=negbin, model="within")
  
  
  
  ## Respiratory causes 
  # Gaussian Version
  respi_P1<-plm(respi_rate ~ period + elipic + period*elipic, data=datam_p1, model="within")
  
  respi_P2<-plm(respi_rate ~ period + elipic + period*elipic, data=datam_p2, model="within")
  
  # Poisson Version
  respiP_P1<-pglm(respi_tot ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  
  respiP_P2<-pglm(respi_tot ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  
  # NegativeBin Version
  respiNB_P1<-pglm(respi_tot ~ period + elipic + period*elipic, data=datam_p1, family=negbin, model="within")
  
  respiNB_P2<-pglm(respi_tot ~ period + elipic + period*elipic, data=datam_p2, family=negbin, model="within")
  
  ## Results Poisson
  # P2 vs P1
  # Nonaccidental mortality
  est<-exp(coefficients(noccP_P1))
  CI<-exp(confint(noccP_P1))
  res_P1<-cbind(est,CI)
  coef_period_P1P2[rep,]<-res_P1[1,]
  coef_elipic_P1P2[rep,]<-res_P1[2,]
  coef_did_P1P2[rep,]<-res_P1[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvP_P1))
  CI<-exp(confint(cvP_P1))
  rescardio_P1<-cbind(est,CI)
  cardio_period_P1P2[rep,]<-rescardio_P1[1,]
  cardio_elipic_P1P2[rep,]<-rescardio_P1[2,]
  cardio_did_P1P2[rep,]<-rescardio_P1[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiP_P1))
  CI<-exp(confint(respiP_P1))
  resrespi_P1<-cbind(est,CI)
  respi_period_P1P2[rep,]<-resrespi_P1[1,]
  respi_elipic_P1P2[rep,]<-resrespi_P1[2,]
  respi_did_P1P2[rep,]<-resrespi_P1[3,]
  
  
  ## Results Neative Binomial
  # P2 vs P1
  # Nonaccidental mortality
  est<-exp(coefficients(noccNB_P1))
  CI<-exp(confint(noccNB_P1))
  res_P1<-cbind(est,CI)
  NB_coef_period_P1P2[rep,]<-res_P1[1,]
  NB_coef_elipic_P1P2[rep,]<-res_P1[2,]
  NB_coef_did_P1P2[rep,]<-res_P1[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvNB_P1))
  CI<-exp(confint(cvNB_P1))
  rescardio_P1<-cbind(est,CI)
  NB_cardio_period_P1P2[rep,]<-rescardio_P1[1,]
  NB_cardio_elipic_P1P2[rep,]<-rescardio_P1[2,]
  NB_cardio_did_P1P2[rep,]<-rescardio_P1[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiNB_P1))
  CI<-exp(confint(respiNB_P1))
  resrespi_P1<-cbind(est,CI)
  NB_respi_period_P1P2[rep,]<-resrespi_P1[1,]
  NB_respi_elipic_P1P2[rep,]<-resrespi_P1[2,]
  NB_respi_did_P1P2[rep,]<-resrespi_P1[3,]
  
  ## Results Linear Model 
  ## Non accidental mortality
  est<-coefficients(nocc_P1)
  CI<-confint(nocc_P1)
  res_P1<-cbind(est,CI)
  L_coef_period_P1P2[rep,]<-res_P1[1,]
  L_coef_elipic_P1P2[rep,]<-res_P1[2,]
  L_coef_did_P1P2[rep,]<-res_P1[3,]
  # Cardiovascular Mortality
  est<-coefficients(cv_P1)
  CI<-confint(cv_P1)
  rescardio_P1<-cbind(est,CI)
  L_cardio_period_P1P2[rep,]<-rescardio_P1[1,]
  L_cardio_elipic_P1P2[rep,]<-rescardio_P1[2,]
  L_cardio_did_P1P2[rep,]<-rescardio_P1[3,]
  # Respiratory Mortality
  est<-coefficients(respi_P1)
  CI<-confint(respi_P1)
  resrespi_P1<-cbind(est,CI)
  L_respi_period_P1P2[rep,]<-resrespi_P1[1,]
  L_respi_elipic_P1P2[rep,]<-resrespi_P1[2,]
  L_respi_did_P1P2[rep,]<-resrespi_P1[3,]
  
  
  
  
  # P3 vs P2
  # Poisson
  # Nonaccidental mortality
  est<-exp(coefficients(noccP_P2))
  CI<-exp(confint(noccP_P2))
  res_P2<-cbind(est,CI)
  coef_period_P2P3[rep,]<-res_P2[1,]
  coef_elipic_P2P3[rep,]<-res_P2[2,]
  coef_did_P2P3[rep,]<-res_P2[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvP_P2))
  CI<-exp(confint(cvP_P2))
  rescv_P2<-cbind(est,CI)
  cardio_period_P2P3[rep,]<-rescv_P2[1,]
  cardio_elipic_P2P3[rep,]<-rescv_P2[2,]
  cardio_did_P2P3[rep,]<-rescv_P2[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiP_P2))
  CI<-exp(confint(respiP_P2))
  resrespi_P2<-cbind(est,CI)
  respi_period_P2P3[rep,]<-resrespi_P2[1,]
  respi_elipic_P2P3[rep,]<-resrespi_P2[2,]
  respi_did_P2P3[rep,]<-resrespi_P2[3,]
  
  
  ## Negative Binomial
  # P3 vs P2
  
  # Nonaccidental mortality
  est<-exp(coefficients(noccNB_P2))
  CI<-exp(confint(noccNB_P2))
  res_P2<-cbind(est,CI)
  NB_coef_period_P2P3[rep,]<-res_P2[1,]
  NB_coef_elipic_P2P3[rep,]<-res_P2[2,]
  NB_coef_did_P2P3[rep,]<-res_P2[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvNB_P2))
  CI<-exp(confint(cvNB_P2))
  rescv_P2<-cbind(est,CI)
  NB_cardio_period_P2P3[rep,]<-rescv_P2[1,]
  NB_cardio_elipic_P2P3[rep,]<-rescv_P2[2,]
  NB_cardio_did_P2P3[rep,]<-rescv_P2[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiNB_P2))
  CI<-exp(confint(respiNB_P2))
  resrespi_P2<-cbind(est,CI)
  NB_respi_period_P2P3[rep,]<-resrespi_P2[1,]
  NB_respi_elipic_P2P3[rep,]<-resrespi_P2[2,]
  NB_respi_did_P2P3[rep,]<-resrespi_P2[3,]
  
  ### Linear Model 
  # Nonaccidental mortality
  est<-coefficients(nocc_P2)
  CI<-confint(nocc_P2)
  res_P2<-cbind(est,CI)
  L_coef_period_P2P3[rep,]<-res_P2[1,]
  L_coef_elipic_P2P3[rep,]<-res_P2[2,]
  L_coef_did_P2P3[rep,]<-res_P2[3,]
  # Cardiovascular Mortality
  est<-coefficients(cv_P2)
  CI<-confint(cv_P2)
  rescv_P2<-cbind(est,CI)
  L_cardio_period_P2P3[rep,]<-rescv_P2[1,]
  L_cardio_elipic_P2P3[rep,]<-rescv_P2[2,]
  L_cardio_did_P2P3[rep,]<-rescv_P2[3,]
  # Respiratory Mortality
  est<-coefficients(respi_P2)
  CI<-confint(respi_P2)
  resrespi_P2<-cbind(est,CI)
  L_respi_period_P2P3[rep,]<-resrespi_P2[1,]
  L_respi_elipic_P2P3[rep,]<-resrespi_P2[2,]
  L_respi_did_P2P3[rep,]<-resrespi_P2[3,]
  
  
  ### People older than 75 years old
  ## Linear Models
  ## Non accidental causes 
  nocc_P1<-plm(nocc_rate75 ~ period + elipic + period*elipic, data=datam_p1, model="within")
  nocc_P2<-plm(nocc_rate75 ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  ## Cardiovascular causes 
  cv_P1<-plm(cv_rate75 ~ period + elipic + period*elipic, data=datam_p1, model="within")
  cv_P2<-plm(cv_rate75 ~ period + elipic + period*elipic, data=datam_p2, model="within")
  ## Respiratory causes 
  respi_P1<-plm(respi_rate75 ~ period + elipic + period*elipic, data=datam_p1, model="within")
  respi_P2<-plm(respi_rate75 ~ period + elipic + period*elipic, data=datam_p2, model="within")
  
  ### Poisson Model
  ## Non accidental causes 
  noccP_P1<-pglm(nocc_75 ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  noccP_P2<-pglm(nocc_75 ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  ## Cardiovascular causes 
  cvP_P1<-pglm(cv_75 ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  cvP_P2<-pglm(cv_75 ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  ## Respiratory causes 
  respiP_P1<-pglm(respi_75 ~ period + elipic + period*elipic, data=datam_p1, family=poisson(link = "log"), model="within")
  respiP_P2<-pglm(respi_75 ~ period + elipic + period*elipic, data=datam_p2, family=poisson(link = "log"), model="within")
  
  ## Results Poisson Model
  # P2 vs P1
  # Nonaccidental mortality
  est<-exp(coefficients(noccP_P1))
  CI<-exp(confint(noccP_P1))
  res_P1<-cbind(est,CI)
  coef75_period_P1P2[rep,]<-res_P1[1,]
  coef75_elipic_P1P2[rep,]<-res_P1[2,]
  coef75_did_P1P2[rep,]<-res_P1[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvP_P1))
  CI<-exp(confint(cvP_P1))
  rescardio_P1<-cbind(est,CI)
  cardio75_period_P1P2[rep,]<-rescardio_P1[1,]
  cardio75_elipic_P1P2[rep,]<-rescardio_P1[2,]
  cardio75_did_P1P2[rep,]<-rescardio_P1[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiP_P1))
  CI<-exp(confint(respiP_P1))
  resrespi_P1<-cbind(est,CI)
  respi75_period_P1P2[rep,]<-resrespi_P1[1,]
  respi75_elipic_P1P2[rep,]<-resrespi_P1[2,]
  respi75_did_P1P2[rep,]<-resrespi_P1[3,]
  
  ## Results Linear Model 
  ## Non accidental mortality
  est<-coefficients(nocc_P1)
  CI<-confint(nocc_P1)
  res_P1<-cbind(est,CI)
  L_coef75_period_P1P2[rep,]<-res_P1[1,]
  L_coef75_elipic_P1P2[rep,]<-res_P1[2,]
  L_coef75_did_P1P2[rep,]<-res_P1[3,]
  # Cardiovascular Mortality
  est<-coefficients(cv_P1)
  CI<-confint(cv_P1)
  rescardio_P1<-cbind(est,CI)
  L_cardio75_period_P1P2[rep,]<-rescardio_P1[1,]
  L_cardio75_elipic_P1P2[rep,]<-rescardio_P1[2,]
  L_cardio75_did_P1P2[rep,]<-rescardio_P1[3,]
  # Respiratory Mortality
  est<-coefficients(respi_P1)
  CI<-confint(respi_P1)
  resrespi_P1<-cbind(est,CI)
  L_respi75_period_P1P2[rep,]<-resrespi_P1[1,]
  L_respi75_elipic_P1P2[rep,]<-resrespi_P1[2,]
  L_respi75_did_P1P2[rep,]<-resrespi_P1[3,]
  
  
  # P3 vs P2
  
  # Nonaccidental mortality
  est<-exp(coefficients(noccP_P2))
  CI<-exp(confint(noccP_P2))
  res_P2<-cbind(est,CI)
  coef75_period_P2P3[rep,]<-res_P2[1,]
  coef75_elipic_P2P3[rep,]<-res_P2[2,]
  coef75_did_P2P3[rep,]<-res_P2[3,]
  # Cardiovascular Mortality
  est<-exp(coefficients(cvP_P2))
  CI<-exp(confint(cvP_P2))
  rescv_P2<-cbind(est,CI)
  cardio75_period_P2P3[rep,]<-rescv_P2[1,]
  cardio75_elipic_P2P3[rep,]<-rescv_P2[2,]
  cardio75_did_P2P3[rep,]<-rescv_P2[3,]
  # Respiratory Mortality
  est<-exp(coefficients(respiP_P2))
  CI<-exp(confint(respiP_P2))
  resrespi_P2<-cbind(est,CI)
  respi75_period_P2P3[rep,]<-resrespi_P2[1,]
  respi75_elipic_P2P3[rep,]<-resrespi_P2[2,]
  respi75_did_P2P3[rep,]<-resrespi_P2[3,]
  
  ### Linear Model 
  # Nonaccidental mortality
  est<-coefficients(nocc_P2)
  CI<-confint(nocc_P2)
  res_P2<-cbind(est,CI)
  L_coef75_period_P2P3[rep,]<-res_P2[1,]
  L_coef75_elipic_P2P3[rep,]<-res_P2[2,]
  L_coef75_did_P2P3[rep,]<-res_P2[3,]
  # Cardiovascular Mortality
  est<-coefficients(cv_P2)
  CI<-confint(cv_P2)
  rescv_P2<-cbind(est,CI)
  L_cardio75_period_P2P3[rep,]<-rescv_P2[1,]
  L_cardio75_elipic_P2P3[rep,]<-rescv_P2[2,]
  L_cardio75_did_P2P3[rep,]<-rescv_P2[3,]
  # Respiratory Mortality
  est<-coefficients(respi_P2)
  CI<-confint(respi_P2)
  resrespi_P2<-cbind(est,CI)
  L_respi75_period_P2P3[rep,]<-resrespi_P2[1,]
  L_respi75_elipic_P2P3[rep,]<-resrespi_P2[2,]
  L_respi75_did_P2P3[rep,]<-resrespi_P2[3,]

  ## Number of prevented deaths for overall population
  RR<-coef_did_P2P3$est[rep]
  AF[rep]<-(1/RR-1)/(1/RR)
  MDD[rep]<-mean(dat_boot$nocc_tot[which(dat_boot$period =="3")])
  PD[rep]<-AF[rep]*MDD[rep]*99
  
  RR<-cardio_did_P2P3$est[rep]
  AF_cardio[rep]<-(1/RR-1)/(1/RR)
  MDD_cardio[rep]<-mean(dat_boot$cv_tot[which(dat_boot$period =="3")])
  PD_cardio[rep]<-AF_cardio[rep]*MDD_cardio[rep]*99
  
  RR<-respi_did_P2P3$est[rep]
  AF_respi[rep]<-(1/RR-1)/(1/RR)
  MDD_respi[rep]<-mean(dat_boot$respi_tot[which(dat_boot$period =="3")])
  PD_respi[rep]<-AF_respi[rep]*MDD_respi[rep]*99
  
  ## Number of prevented deaths for elderly people
  RR<-coef75_did_P2P3$est[rep]
  AF_75[rep]<-(1/RR-1)/(1/RR)
  MDD_75[rep]<-mean(dat_boot$nocc_75[which(dat_boot$period =="3")])
  PD_75[rep]<-AF_75[rep]*MDD_75[rep]*99
  
  RR<-cardio75_did_P2P3$est[rep]
  AF_cardio75[rep]<-(1/RR-1)/(1/RR)
  MDD_cardio75[rep]<-mean(dat_boot$cv_75[which(dat_boot$period =="3")])
  PD_cardio75[rep]<-AF_cardio75[rep]*MDD_cardio75[rep]*99
  
  RR<-respi75_did_P2P3$est[rep]
  AF_respi75[rep]<-(1/RR-1)/(1/RR)
  MDD_respi75[rep]<-mean(dat_boot$respi_75[which(dat_boot$period =="3")])
  PD_respi75[rep]<-AF_respi75[rep]*MDD_respi75[rep]*99
  
  pb$tick()
  Sys.sleep(1 / 100)
}


list_results<- list(coef_period_P1P2, coef_elipic_P1P2, coef_did_P1P2, cardio_period_P1P2, cardio_elipic_P1P2, cardio_did_P1P2, respi_period_P1P2, respi_elipic_P1P2, respi_did_P1P2, coef_period_P2P3, coef_elipic_P2P3, coef_did_P2P3, cardio_period_P2P3, cardio_elipic_P2P3, cardio_did_P2P3, respi_period_P2P3, respi_elipic_P2P3, respi_did_P2P3)
list_resultsL<- list(L_coef_period_P1P2, L_coef_elipic_P1P2, L_coef_did_P1P2, L_cardio_period_P1P2, L_cardio_elipic_P1P2, L_cardio_did_P1P2, L_respi_period_P1P2, L_respi_elipic_P1P2, L_respi_did_P1P2, L_coef_period_P2P3, L_coef_elipic_P2P3, L_coef_did_P2P3, L_cardio_period_P2P3, L_cardio_elipic_P2P3, L_cardio_did_P2P3, L_respi_period_P2P3, L_respi_elipic_P2P3, L_respi_did_P2P3)
list_resultsNB<- list(NB_coef_period_P1P2, NB_coef_elipic_P1P2, NB_coef_did_P1P2, NB_cardio_period_P1P2, NB_cardio_elipic_P1P2, NB_cardio_did_P1P2, NB_respi_period_P1P2, NB_respi_elipic_P1P2, NB_respi_did_P1P2, NB_coef_period_P2P3, NB_coef_elipic_P2P3, NB_coef_did_P2P3, NB_cardio_period_P2P3, NB_cardio_elipic_P2P3, NB_cardio_did_P2P3, NB_respi_period_P2P3, NB_respi_elipic_P2P3, NB_respi_did_P2P3)

list_results75<-list(coef75_period_P1P2, coef75_elipic_P1P2, coef75_did_P1P2, cardio75_period_P1P2, cardio75_elipic_P1P2, cardio75_did_P1P2, respi75_period_P1P2, respi75_elipic_P1P2, respi75_did_P1P2, coef75_period_P2P3, coef75_elipic_P2P3, coef75_did_P2P3, cardio75_period_P2P3, cardio75_elipic_P2P3, cardio75_did_P2P3, respi75_period_P2P3, respi75_elipic_P2P3, respi75_did_P2P3)
list_results75L<-list(L_coef75_period_P1P2, L_coef75_elipic_P1P2, L_coef75_did_P1P2, L_cardio75_period_P1P2, L_cardio75_elipic_P1P2, L_cardio75_did_P1P2, L_respi75_period_P1P2, L_respi75_elipic_P1P2, L_respi75_did_P1P2, L_coef75_period_P2P3, L_coef75_elipic_P2P3, L_coef75_did_P2P3, L_cardio75_period_P2P3, L_cardio75_elipic_P2P3, L_cardio75_did_P2P3, L_respi75_period_P2P3, L_respi75_elipic_P2P3, L_respi75_did_P2P3)


results<-matrix(NA,nrow = 18,ncol=4)
colnames(results)<-c("estimate","value","CI_2.5","CI_97.5")

resultsL<-matrix(NA,nrow = 18,ncol=4)
colnames(resultsL)<-c("estimate","value","CI_2.5","CI_97.5")

resultsNB<-matrix(NA,nrow = 18,ncol=4)
colnames(resultsNB)<-c("estimate","value","CI_2.5","CI_97.5")

results_75<-matrix(NA,nrow = 18,ncol=4)
colnames(results_75)<-c("estimate","value","CI_2.5","CI_97.5")

resultsL_75<-matrix(NA,nrow = 18,ncol=4)
colnames(resultsL_75)<-c("estimate","value","CI_2.5","CI_97.5")

results[,1]<-c("nocc_period_P1P2","nocc_elipic_P1P2","nocc_did_P1P2","cardio_period_P1P2","cardio_elipic_P1P2","cardio_did_P1P2","respi_period_P1P2","respi_elipic_P1P2","respi_did_P1P2","nocc_period_P2P3","nocc_elipic_P2P3","nocc_did_P2P3","cardio_period_P2P3","cardio_elipic_P2P3","cardio_did_P2P3","respi_period_P2P3","respi_elipic_P2P3","respi_did_P2P3")
resultsL[,1]<-c("nocc_period_P1P2","nocc_elipic_P1P2","nocc_did_P1P2","cardio_period_P1P2","cardio_elipic_P1P2","cardio_did_P1P2","respi_period_P1P2","respi_elipic_P1P2","respi_did_P1P2","nocc_period_P2P3","nocc_elipic_P2P3","nocc_did_P2P3","cardio_period_P2P3","cardio_elipic_P2P3","cardio_did_P2P3","respi_period_P2P3","respi_elipic_P2P3","respi_did_P2P3")
resultsNB[,1]<-c("nocc_period_P1P2","nocc_elipic_P1P2","nocc_did_P1P2","cardio_period_P1P2","cardio_elipic_P1P2","cardio_did_P1P2","respi_period_P1P2","respi_elipic_P1P2","respi_did_P1P2","nocc_period_P2P3","nocc_elipic_P2P3","nocc_did_P2P3","cardio_period_P2P3","cardio_elipic_P2P3","cardio_did_P2P3","respi_period_P2P3","respi_elipic_P2P3","respi_did_P2P3")

results_75[,1]<-c("nocc_period_P1P2","nocc_elipic_P1P2","nocc_did_P1P2","cardio_period_P1P2","cardio_elipic_P1P2","cardio_did_P1P2","respi_period_P1P2","respi_elipic_P1P2","respi_did_P1P2","nocc_period_P2P3","nocc_elipic_P2P3","nocc_did_P2P3","cardio_period_P2P3","cardio_elipic_P2P3","cardio_did_P2P3","respi_period_P2P3","respi_elipic_P2P3","respi_did_P2P3")
resultsL_75[,1]<-c("nocc_period_P1P2","nocc_elipic_P1P2","nocc_did_P1P2","cardio_period_P1P2","cardio_elipic_P1P2","cardio_did_P1P2","respi_period_P1P2","respi_elipic_P1P2","respi_did_P1P2","nocc_period_P2P3","nocc_elipic_P2P3","nocc_did_P2P3","cardio_period_P2P3","cardio_elipic_P2P3","cardio_did_P2P3","respi_period_P2P3","respi_elipic_P2P3","respi_did_P2P3")


for (i in 1:18) {  
  for (ii in 1:3) {
    results[i,ii+1]<-mean(list_results[[i]][,ii][which(list_results[[i]][,ii]<100)], na.rm = TRUE)
  }
}  

for (i in 1:18) {  
  for (ii in 1:3) {
    resultsL[i,ii+1]<-mean(list_resultsL[[i]][,ii][which(list_resultsL[[i]][,ii]<100)], na.rm = TRUE)
  }
}  

for (i in 1:18) {  
  for (ii in 1:3) {
    resultsNB[i,ii+1]<-mean(list_resultsNB[[i]][,ii][which(list_resultsNB[[i]][,ii]<100)], na.rm = TRUE)
  }
}  

for (i in 1:18) {  
  for (ii in 1:3) {
    results_75[i,ii+1]<-mean(list_results75[[i]][,ii][which(list_results[[i]][,ii]<100)], na.rm = TRUE)
  }
}  
for (i in 1:18) {  
  for (ii in 1:3) {
    resultsL_75[i,ii+1]<-mean(list_results75L[[i]][,ii][which(list_resultsL[[i]][,ii]<100)], na.rm = TRUE)
  }
}  


results<-data.frame(results)
resultsL<-data.frame(resultsL)
resultsNB<-data.frame(resultsNB)
results_75<-data.frame(results_75)
resultsL_75<-data.frame(resultsL_75)

### Save bootstrap results for model estimates
write_xlsx(results,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/bootstrap_Poisson.xlsx")
write_xlsx(resultsL,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/bootstrap_Linear.xlsx")
write_xlsx(resultsNB,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/bootstrap_NegBin.xlsx")
write_xlsx(results_75,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/bootstrap_75.xlsx")
write_xlsx(resultsL_75,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/bootstrap_75L.xlsx")




res_PD<-data.frame("type"= c("nocc","cardio","respi","nocc_75","cardio_75","respi_75"),"value" = rep(NA,6),"CI_2.5" = rep(NA,6), "CI_97.5" = rep(NA,6))

res_PD$value[1]<-mean(PD[which(PD>0)])
res_PD$value[2]<-mean(PD_cardio[which(PD_cardio>0)])
res_PD$value[3]<-mean(PD_respi[which(PD_respi>0)])
res_PD$value[4]<-mean(PD_75[which(PD_75>0)])
res_PD$value[5]<-mean(PD_cardio75[which(PD_cardio75>0)])
res_PD$value[6]<-mean(PD_respi75[which(PD_respi75>0)])
                 
res_PD$CI_2.5[1]<-quantile(PD[which(PD>0)],0.025)
res_PD$CI_2.5[2]<-quantile(PD_cardio[which(PD_cardio>0)],0.025)
res_PD$CI_2.5[3]<-quantile(PD_respi[which(PD_respi>0)],0.025)
res_PD$CI_2.5[4]<-quantile(PD_75[which(PD_75>0)],0.025)
res_PD$CI_2.5[5]<-quantile(PD_cardio75[which(PD_cardio75>0)],0.025)
res_PD$CI_2.5[6]<-quantile(PD_respi75[which(PD_respi75>0)],0.025)

res_PD$CI_97.5[1]<-quantile(PD[which(PD>0)],0.975)
res_PD$CI_97.5[2]<-quantile(PD_cardio[which(PD_cardio>0)],0.975)
res_PD$CI_97.5[3]<-quantile(PD_respi[which(PD_respi>0)],0.975)
res_PD$CI_97.5[4]<-quantile(PD_75[which(PD_75>0)],0.975)
res_PD$CI_97.5[5]<-quantile(PD_cardio75[which(PD_cardio75>0)],0.975)
res_PD$CI_97.5[6]<-quantile(PD_respi75[which(PD_respi75>0)],0.975)


write_xlsx(res_PD,"C:/Users/Anna/Dropbox/CEPEM/Axe 2/Tache 1/results/Pr_deaths_bootstrap.xlsx")

