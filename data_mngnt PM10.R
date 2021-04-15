### Data Management PM10 data
library(xtable)
library(xlsx)
library(readxl)

PM10_00_20 <- read_excel("C:/Users/Anna/Dropbox/CEPEM/Axe 2/PM10_00_20.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

str(PM10_00_20)

pm10<-PM10_00_20[which(PM10_00_20$Dates<"2017-12-31"),]


head(which(pm10$Dates == "2011-12-01 UTC"))

pm10<-data.frame(pm10)

## the following steps have been implemented in order to create a variable "eligibility", which indicates, for each day, if the day is eligibile to Air Quality informational Alerts (AQAs) ou Air Quality Warning Alert according to the daily level of PM10 concentrations recorded by background and traffic monitoring stations 

###################################  Informational Alert ##########################################

### Background Stations ###

# Information Alert threshold - Period 1 
for (i in 1:4354) {
pm10$count[i]<-if (any(pm10[i,2:22] > 80, na.rm = TRUE)) 1 else 0}
# Information Alert threshold - Period 2
for (i in 4355:nrow(pm10)) {
  pm10$count[i]<-if (any(pm10[i,2:22] > 50, na.rm = TRUE)) 1 else 0}

sum(pm10$count)

### Traffic Stations ###

# Information Alert threshold - Period 1 
for (i in 1:4354) {
  pm10$count2[i]<-if (any(pm10[i,23:33] > 80, na.rm = TRUE)) 1 else 0}
# Information Alert threshold - Period 2
for (i in 4355:nrow(pm10)) {
  pm10$count2[i]<-if (any(pm10[i,23:33] > 50, na.rm = TRUE)) 1 else 0}

sum(pm10$count2)

# Total number of inf. alerts between background stations and traffic stations

for (i in 1:nrow(pm10)) {
  pm10$tot[i]<- pm10$count[i] + pm10$count2[i]
}

table(pm10$tot)

## 166 days having value 2, which means that there is one day in which the inf. alert was detected in the background stations but not in the traffic stations

which(pm10$count==1 & pm10$count2==0)

## is the observation at row 27

View(pm10[27,])

# This is only due to the fact that we there are not measures for the trafic stations until semptember 2001

# For the other inf.alert days, we observe that every time a infor. alert is detected by a background station,
# trafic stations also detect the alert, which means that the day is considered as a pollution peak


###################################  Warning Alert ##########################################
### Background Stations ###

# Warning Alert threshold - Period 1 
for (i in 1:4354) {
  pm10$count_Al[i]<-if (any(pm10[i,2:22] > 125 , na.rm = TRUE)) 1 else 0}
# Warning Alert threshold - Period 2
for (i in 4355:nrow(pm10)) {
  pm10$count_Al[i]<-if (any(pm10[i,2:22] > 80, na.rm = TRUE)) 1 else 0}

sum(pm10$count_Al)

### Traffic Stations ###

# Warning Alert threshold - Period 1
for (i in 1:4354) {
  pm10$count2_Al[i]<-if (any(pm10[i,23:33] > 125, na.rm = TRUE)) 1 else 0}
# Warning Alert threshold - Period 2
for (i in 4355:nrow(pm10)) {
  pm10$count2_Al[i]<-if (any(pm10[i,23:33] > 80, na.rm = TRUE)) 1 else 0}


sum(pm10$count2_Al)

# Total number of inf. alerts between background stations and traffic stations

for (i in 1:nrow(pm10)) {
  pm10$tot_Al[i]<- pm10$count_Al[i] + pm10$count2_Al[i]
}

table(pm10$tot_Al)

## in this case also, all warning alerts detected by background stations correspond to a warning alert detected by trafic stations



############ Creation of eligibility variable for informational alerts and warning alerts

for (i in 1:nrow(pm10)){
 pm10$elig_Inf[i]<-if (pm10$tot[i]==2) 1 else 0
}


for (i in 1:nrow(pm10)){
  pm10$elig_Al[i]<-if (pm10$tot_Al[i]==2) 1 else 0
}

table(pm10$elig_Inf)
table(pm10$elig_Al)

### correction for row 27, for which we don't have traffic-stations measures 

pm10$elig_Inf[27]<-1

table(pm10$elig_Inf)

## looks fine

pm10$pm10moy<-rowMeans(pm10[,2:32],na.rm=TRUE)

summary(pm10$pm10moy)


### Comparing days that we considered as "eligible" after 2007 (when AQA policy was implemented) and the effective list of days that were declared AQ (Informational or Warning) alerts (https://www.airparif.asso.fr/historique-des-episodes-de-pollution)

load("C:/Users/Anna/Dropbox/CEPEM/Axe 2/data1.Rda")


data<-cbind(data,pm10)


View(data[which(data$`Pic PM10`==1),c(1,40,84,85)])



data$`Pic PM10`[is.na(data$`Pic PM10`)]<-0
for (i in (1:(nrow(data)))){
  data$check[i]<-as.numeric(data[i,40])- as.numeric(data[i,84])
}

table(data$check)

# Over the whole study period, 36 days are considered as eligible according to monitoring stations measures but they were not declared as AQA days by the Paris prefecture and Airparif

# We also observe 10 official AQA days that were not detected by our monitoring stations measures

View(data[which(data$check==-1),c(1,40,84,85)])

table(data$check[which(data$check==-1 & data$Dates>"2007-12-31")])
table(data$check[which(data$check==-1 & data$Dates<"2007-12-31")])

View(data[which(data$check==-1 & data$Dates>"2007-12-31") ,c(1,19,47:67,40,84,85)])
View(data[which(data$check==-1 & data$Dates>"2007-12-31" & data$Dates<"2011-12-31") ,c(1,19,47,48,49,50,51,40,84,85)])

## Among the 36 eligible days that were not officialy declared as AQA days, 15 are in the period before the end of 2007 (so it's normal that there were not official AQA days) 
# However, 19 eligible days that were not officialy declared as AQA days are in the period after the end of 2007 

# This may be due to a discrepancy between air pollution predictions (that are used to declare AQA days) and PM10 concentrations effectively observed during the day (==> FALSE NEGATIVE?) 


View(data[which(data$check==1),c(1,19,47:67,40,84,85)])

# About days that were officially declared as AQA days but were not dected as eligible with our data:
# These are days in the first AQA period (before change in reglementation in 2011) and it seems like they were considered as AQA as according to the new regulatory threshold values

# Therefore, days exceeding threshold values BEFORE AQA policy implementation (end of 2007) are defined as "eligible" (to AQA measures)
# Days exceeding threshold values AFTER AQA policy implementation (end of 2007) are defined as "AQA days"

table(data$`Pic PM10`[which(data$Dates>"2008-12-31")])
table(data$elig_Inf[which(data$Dates>"2008-12-31")])
table(data$elig_Inf)

View(data[which(data$Dates>"2009-01-01"),])


View(data[which(data$elig_Inf==1),c(1,40,84,85)])

#data<-data[,-(46:86)]
#save(data,file="C:/Users/Anna/Dropbox/CEPEM/Axe 2/data1.Rda")


# For the period before AQA reglementation (before the end of 2007), we use the list of eligible days detected with our data from monitoring stations and according to the first threshold value criterias 
# For the period after AQA reglementation, we use the official list of AQA days provided in the AirParif website because it rappresent a rialble information to know when AQA measures were applyed

### Construction of new variable elipic, which indicates eligible days before 2008 and official AQA days after the end of 2007 

for (i in 1:nrow(data)) {
 data$elipic[i]<-if (data$Dates[i]<"2008-01-01") data$elig_Inf[i] else data$`Pic PM10`[i] 
}

table(data$elipic)
table(data$`Pic PM10`)

# There are 143 official AQA days according to Airparif, if we add the 15 eligible days of the period before 2007, we obtain a total of 158 eligible/AQA days

#Change name of pm10 variable
colnames(data)
str(data)

data$moyPM10<-data[,86]

data<-data[,-(86)]

View(data)

## Save our final list of eligible/AQA days

#save(data,file="C:/Users/Anna/Dropbox/CEPEM/Axe 2/data1.Rda")
