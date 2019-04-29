########TElECOM CHURN CASE STUDY#######

#Setting Working directory
setwd("E:\\Jigsaw\\Capstone")


#*****DATA EXPLORATION AND PREPARATION******************#
#Reading the file 
dat=read.csv("sampletelecomfinal.csv")
View(dat)
str(dat)
summary(dat)
dim(dat)

#Generating the data quality report
#install.packages("dataQualityR")
library(dataQualityR)
checkDataQuality(data=dat,out.file.num="Churnnumeric.csv",
                 out.file.cat="Churncharacter.csv")

#checking churn % before removing cols:
nrow(dat[dat$churn==1,])/nrow(dat)

#Dropping variables having high % of missing values:-->got from quality report
#REading numeric Quality report file
NumericQR<-read.csv("Churnnumeric.csv")
library(dplyr)
#checking for all cols having >15% missing values
NumericGt_15=filter(NumericQR,missing.percent>15)
NumericGt_15
NumericGt_15$X


#Removing numbcars
#Imputing retdays

#REading Character Quality Report file
CharacterQR<-read.csv("Churncharacter.csv")


#checking for all variables having >15% missing values
CharacterGt_15=filter(CharacterQR,n.miss.percent>15)
CharacterGt_15
CharacterGt_15$X

#Removing categorical variables:dwlltype,dwllsize,mailordr,occu1,wrkwoman,solflag,proptype,mailresp
#..cartype,children,div_type
#Removing continuous variable:numbcars
dat1=select(dat,-c(dwlltype,dwllsize,mailordr,occu1,wrkwoman,solflag,
                   proptype,mailresp,cartype,children,div_type,numbcars))

#For retdays according to data dictionary missing values can be said to be considered as 0
#Hence if no calls made we impute 0 and if any calls have been made we change the value to 1
summary(dat1$retdays) #We have 12825 missing values
dat1$retdays<-ifelse(is.na(dat1$retdays),0,1)
View(dat1$retdays)
summary(dat1$retdays) #now no NA's
dim(dat1)
dim(dat1)
colnames(dat1)
summary(dat1)

#checking churn % after removing cols:
nrow(dat1[dat1$churn==1,])/nrow(dat1)

#Checking correlations b/w range and mean of vars before imputation:
#cor(dat1$callwait_Mean,dat1$callwait_Range)#High correlated
#cor(dat1$da_Mean,dat1$da_Range)#Na
#cor(dat1$datovr_Mean,dat1$datovr_Range)#na
#cor(dat1$comp_vce_Mean,dat1$drop_vce_Range)#LESS correlated 
#cor(dat1$rev_Mean,dat1$rev_Range)#NA



#Considering the continuous variables for decile binning:
#Variable1:mou_Mean
colnames(dat1)
summary(dat1$mou_Mean) #26NA's
str(dat1$mou_Mean)
dat1%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mou_Mean1
mou_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                        count(dec)%>%unname())[[2]]
mou_Mean1$churn_perc<-mou_Mean1$n/mou_Mean1$N
mou_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                                  group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
mou_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(mou_Mean,n=10))%>%
                               group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
mou_Mean1$varname<-rep("mou_Mean",nrow(mou_Mean1))
mou_Mean1
#Imputing missing values with mean

dat1$mou_Mean[is.na(dat1$mou_Mean)]<-529.3
summary(dat1$mou_Mean) #All the NA's removed

#Variable2:totmrc_Mean
summary(dat1$totmrc_Mean) #26 NA's
dat1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->totmrc_Mean1
totmrc_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                           count(dec)%>%unname())[[2]]
totmrc_Mean1$churn_perc<-totmrc_Mean1$n/totmrc_Mean1$N
totmrc_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                                     group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]

totmrc_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%
                                  group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
totmrc_Mean1$varname<-rep("totmrc_Mean",nrow(totmrc_Mean1))

totmrc_Mean1
#Imputing missing value with mean
dat1$totmrc_Mean[is.na(dat1$totmrc_Mean)]<-46.960
summary(dat1$totmrc_Mean) #All the NA's removed


#VAriable 3:rev_Range
summary(dat1$rev_Range) #26 NA's
dat1%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->rev_Range1
rev_Range1$N<-unclass(dat1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                         count(dec)%>%unname())[[2]]
rev_Range1$churn_perc<-rev_Range1$n/rev_Range1$N
rev_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                                   group_by(dec)%>%summarise(min(rev_Range)))[[2]]
rev_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(rev_Range,n=10))%>%
                                group_by(dec)%>%summarise(max(rev_Range)))[[2]]
rev_Range1$varname<-rep("rev_Range",nrow(rev_Range1))
rev_Range1

#Imputing missing values with mean
dat1$rev_Range[is.na(dat1$rev_Range)]<-44.70
summary(dat1$rev_Range) #All the NA's removed

#Variable 4:mou_Range
summary(dat1$mou_Range) # 26 NA's
dat1%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mou_Range1

mou_Range1$N<-unclass(dat1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                         count(dec)%>%unname())[[2]]
mou_Range1$churn_perc<-mou_Range1$n/mou_Range1$N

mou_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                                   group_by(dec)%>%summarise(min(mou_Range)))[[2]]

mou_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(mou_Range,n=10))%>%
                                group_by(dec)%>%summarise(max(mou_Range)))[[2]]
mou_Range1$varname<-rep("mou_Range",nrow(mou_Range1))
mou_Range1

#Imputing missing value withmean
summary(dat1$mou_Range)
dat1$mou_Range[is.na(dat1$mou_Range)]<-382.4
summary(dat1$mou_Range) #All the NA's removed
dat1$mou_Range


#VAriable 5:change_mou
summary(dat1$change_mou) # 71 NA's
dat1%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->change_mou1
change_mou1$N<-unclass(dat1%>%mutate(dec=ntile(change_mou,n=10))%>%
                          count(dec)%>%unname())[[2]]
change_mou1$churn_perc<-change_mou1$n/change_mou1$N

change_mou1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(change_mou,n=10))%>%
                                    group_by(dec)%>%summarise(min(change_mou)))[[2]]

change_mou1$LessThan<-unclass(dat1%>%mutate(dec=ntile(change_mou,n=10))%>%
                                 group_by(dec)%>%summarise(max(change_mou)))[[2]]
change_mou1$varname<-rep("change_mou",nrow(change_mou1))
change_mou1

#Imputing
summary(dat1$change_mou)
dat1$change_mou[is.na(dat1$change_mou)]<--7.546
summary(dat1$change_mou) #All the NA's removed

#Variable6:drop_blk_Mean#

summary(dat1$drop_blk_Mean) 
dat1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->drop_blk_Mean1
drop_blk_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%
                         count(dec)%>%unname())[[2]]
drop_blk_Mean1$churn_perc<-drop_blk_Mean1$n/drop_blk_Mean1$N
drop_blk_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%
                                   group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]

drop_blk_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%
                                group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
drop_blk_Mean1$varname<-rep("drop_blk_Mean",nrow(drop_blk_Mean1))
drop_blk_Mean1

#No missing values

#Variable 7:drop_vce_Range#
summary(dat1$drop_vce_Range)
dat1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->drop_vce_Range1
drop_vce_Range1$N<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%
                             count(dec)%>%unname())[[2]]

drop_vce_Range1$churn_perc<-drop_vce_Range1$n/drop_vce_Range1$N

drop_vce_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%
                                       group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
drop_vce_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%
                                    group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
drop_vce_Range1$varname<-rep("drop_vce_Range",nrow(drop_vce_Range1)) 
drop_vce_Range1

#Nomissing values

#VAriable 8:owylis_vce_Range#
summary(dat1$owylis_vce_Range)

dat1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->owylis_vce_Range1

owylis_vce_Range1$N<-unclass(dat1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%
                  count(dec)%>%unname())[[2]]

owylis_vce_Range1$churn_perc<-owylis_vce_Range1$n/owylis_vce_Range1$N

owylis_vce_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%
                            group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]

owylis_vce_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%
                         group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
owylis_vce_Range1$varname<-rep("owylis_vce_Range",nrow(owylis_vce_Range1))
owylis_vce_Range1

#Variable 9 :mou_opkv_Range#
summary(dat1$mou_opkv_Range)

dat1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->mou_opkv_Range1

mou_opkv_Range1$N<-unclass(dat1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%
                  count(dec)%>%unname())[[2]]

mou_opkv_Range1$churn_perc<-mou_opkv_Range1$n/mou_opkv_Range1$N


mou_opkv_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%
                            group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]

mou_opkv_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%
                         group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]

mou_opkv_Range1$varname<-rep("mou_opkv_Range",nrow(mou_opkv_Range1))
mou_opkv_Range1

#variable10:months#
summary(dat1$months)

dat1%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->months1

months1$N<-unclass(dat1%>%mutate(dec=ntile(months,n=10))%>%
                   count(dec)%>%unname())[[2]]

months1$churn_perc<-months1$n/months1$N

months1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(months,n=10))%>%
                             group_by(dec)%>%summarise(min(months)))[[2]]

months1$LessThan<-unclass(dat1%>%mutate(dec=ntile(months,n=10))%>%
                            group_by(dec)%>%summarise(max(months)))[[2]]
months1$varname<-rep("months",nrow(months1))
months1

#VAriable11:totcalls#
summary(dat1$totcalls)

dat1%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->totcalls1


totcalls1$N<-unclass(dat1%>%mutate(dec=ntile(totcalls,n=10))%>%
                       count(dec)%>%unname())[[2]]

totcalls1$churn_perc<-totcalls1$n/totcalls1$N

totcalls1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(totcalls,n=10))%>%
                             group_by(dec)%>%summarise(min(totcalls)))[[2]]
totcalls1$LessThan<-unclass(dat1%>%mutate(dec=ntile(totcalls,n=10))%>%
                          group_by(dec)%>%summarise(max(totcalls)))[[2]]
totcalls1$varname<-rep("totcalls",nrow(totcalls1))
totcalls1


#Variable12:income
summary(dat1$income) # 3254 NA's
dat1%>%mutate(dec=ntile(income,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->income1
income1
income1$N<-unclass(dat1%>%mutate(dec=ntile(income,n=4))%>%
                      count(dec)%>%unname())[[2]]
income1$churn_perc<-income1$n/income1$N

income1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(income,n=4))%>%
                                group_by(dec)%>%summarise(min(income)))[[2]]

income1$LessThan<-unclass(dat1%>%mutate(dec=ntile(income,n=4))%>%
                             group_by(dec)%>%summarise(max(income)))[[2]]
income1$varname<-rep("income",nrow(income1))
income1 # 

#NA missing is 25% so we dont impute

#Variable13:eqpdays#
summary(dat1$eqpdays)

dat1%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)->eqpdays1

eqpdays1$N<-unclass(dat1%>%mutate(dec=ntile(eqpdays,n=10))%>%
                   count(dec)%>%unname())[[2]]

eqpdays1$churn_perc<-eqpdays1$n/eqpdays1$N

eqpdays1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%
                             summarise(min(eqpdays)))[[2]]

eqpdays1$LessThan<-unclass(dat1%>%mutate(dec=ntile(eqpdays,n=10))%>%
                          group_by(dec)%>%summarise(max(eqpdays)))[[2]]

eqpdays1$varname<-rep("eqpdays",nrow(eqpdays1))
eqpdays1

#Variable14:custcare_Mean...#OMIT because it gives less number of deciles
summary(dat1$custcare_Mean)
dat1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->custcare_Mean1
custcare_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%
                      count(dec)%>%unname())[[2]]
custcare_Mean1$churn_perc<-custcare_Mean1$n/custcare_Mean1$N

custcare_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%
                             group_by(dec)%>%summarise(max(custcare_Mean)))[[2]]

custcare_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%
                                summarise(min(custcare_Mean)))[[2]]

custcare_Mean1$varname<-rep("custcare_Mean",nrow(custcare_Mean1))

custcare_Mean1

#Variable15:callwait_Mean
summary(dat1$callwait_Mean)

dat1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->callwait_Mean1

callwait_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%
                           count(dec)%>%unname())[[2]]

callwait_Mean1$churn_perc<-callwait_Mean1$n/callwait_Mean1$N

callwait_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%
                             group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]

callwait_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(callwait_Mean,n=4))%>%
                                   group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
callwait_Mean1$varname<-rep("callwait_Mean",nrow(callwait_Mean1))
callwait_Mean1

#Variable16:iwylis_vce_Mean
summary(dat1$iwylis_vce_Mean)
dat1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%
  filter(churn==1)->iwylis_vce_Mean1
iwylis_vce_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%
                              count(dec)%>%unname())[[2]]
iwylis_vce_Mean1$churn_perc<-iwylis_vce_Mean1$n/iwylis_vce_Mean1$N
iwylis_vce_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%
                                        group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
iwylis_vce_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%
                                     group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
iwylis_vce_Mean1$varname<-rep("iwylis_vce_Mean",nrow(iwylis_vce_Mean1))
iwylis_vce_Mean1

#Variable 17:callwait_Range----->OMIT less that 4 deciles
summary(dat1$callwait_Range)
dat1%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->callwait_Range1
callwait_Range1$N<-unclass(dat1%>%mutate(dec=ntile(callwait_Range,n=4))%>%
                              count(dec)%>%unname())[[2]]
callwait_Range1$churn_perc<-callwait_Range1$n/callwait_Range1$N
callwait_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(callwait_Range,n=4))%>%
                                        group_by(dec)%>%summarise(min(callwait_Range)))[[2]]
callwait_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(callwait_Range,n=4))%>%
                                     group_by(dec)%>%summarise(max(callwait_Range)))[[2]]
callwait_Range1$varname<-rep("callwait_Range",nrow(callwait_Range1))
callwait_Range1

#VAriable18:ccrndmou_Range------>OMIT less than 4 deciles
summary(dat1$ccrndmou_Range)
dat1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->ccrndmou_Range1
ccrndmou_Range1$N<-unclass(dat1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%
                             count(dec)%>%unname())[[2]]
ccrndmou_Range1$churn_perc<-ccrndmou_Range1$n/ccrndmou_Range1$N
ccrndmou_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%
                                       group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]]
ccrndmou_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%
                                    group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]]
ccrndmou_Range1$varname<-rep("ccrndmou_Range",nrow(ccrndmou_Range1))
ccrndmou_Range1

#Variable 19:adjqty
summary(dat1$adjqty)
dat1%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->adjqty1
adjqty1$N<-unclass(dat1%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
adjqty1$churn_perc<-adjqty1$n/adjqty1$N
adjqty1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(adjqty,n=10))%>%
                               group_by(dec)%>%summarise(min(adjqty)))[[2]]
adjqty1$LessThan<-unclass(dat1%>%mutate(dec=ntile(adjqty,n=10))%>%
                            group_by(dec)%>%summarise(max(adjqty)))[[2]]
adjqty1$varname<-rep("adjqty",nrow(adjqty1))
adjqty1


#VAriable20:ovrrev_Mean
summary(dat1$ovrrev_Mean)#26 NA's
dat1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)->ovrrev_Mean1
ovrrev_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%
                          count(dec)%>%unname())[[2]]
ovrrev_Mean1$churn_perc<-ovrrev_Mean1$n/ovrrev_Mean1$N
ovrrev_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%
                                    group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
ovrrev_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%
                                 group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
ovrrev_Mean1$varname<-rep("ovrrev_Mean",nrow(ovrrev_Mean1))
ovrrev_Mean1


#Missing % is 0.2 % so impute
summary(dat1$ovrrev_Mean)
dat1$ovrrev_Mean[is.na(dat1$ovrrev_Mean)]<-12.9321
summary(dat1$ovrrev_Mean) #All the NA's removed


#Variable 21:rev_Mean
summary(dat1$rev_Mean)#26 NA's
dat1%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->rev_Mean1
rev_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                      count(dec)%>%unname())[[2]]
rev_Mean1$churn_perc<-rev_Mean1$n/rev_Mean1$N
rev_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                                 group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
rev_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(rev_Mean,n=10))%>%
                              group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
rev_Mean1$varname<-rep("rev_Mean",nrow(rev_Mean1))
rev_Mean1

#IMputing missing values with mean
dat1$rev_Mean[is.na(dat1$rev_Mean)]<-58.963
summary(dat1$rev_Mean) # no NA's


#Variable22:ovrmou_Mean
summary(dat1$ovrmou_Mean)#26 NA's
dat1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->ovrmou_Mean1
ovrmou_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%
                          count(dec)%>%unname())[[2]]
ovrmou_Mean1$churn_perc<-ovrmou_Mean1$n/ovrmou_Mean1$N
ovrmou_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%
                                    group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
ovrmou_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%
                                 group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
ovrmou_Mean1$varname<-rep("ovrmou_Mean",nrow(ovrmou_Mean1))
ovrmou_Mean1

#IMputing missing values with mean
dat1$ovrmou_Mean[is.na(dat1$ovrmou_Mean)]<-39.45
summary(dat1$ovrmou_Mean) # no NA's

#Variable 23:comp_vce_Mean
summary(dat1$comp_vce_Mean)
dat1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->comp_vce_Mean1
comp_vce_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%
                            count(dec)%>%unname())[[2]]
comp_vce_Mean1$churn_perc<-comp_vce_Mean1$n/comp_vce_Mean1$N
comp_vce_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%
                                      group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
comp_vce_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%
                                   group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
comp_vce_Mean1$varname<-rep("comp_vce_Mean",nrow(comp_vce_Mean1))
comp_vce_Mean1


#Variable 24:plcd_vce_Mean
summary(dat1$plcd_vce_Mean)
dat1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->plcd_vce_Mean1
plcd_vce_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%
                            count(dec)%>%unname())[[2]]
plcd_vce_Mean1$churn_perc<-plcd_vce_Mean1$n/plcd_vce_Mean1$N
plcd_vce_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%
                                      group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
plcd_vce_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%
                                   group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
plcd_vce_Mean1$varname<-rep("plcd_vce_Mean",nrow(plcd_vce_Mean1))
plcd_vce_Mean1

#Variable 25:avg3mou
summary(dat1$avg3mou)
dat1%>%mutate(dec=ntile(avg3mou,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->avg3mou1
avg3mou1$N<-unclass(dat1%>%mutate(dec=ntile(avg3mou,n=10))%>%
                      count(dec)%>%unname())[[2]]
avg3mou1$churn_perc<-avg3mou1$n/avg3mou1$N
avg3mou1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avg3mou,n=10))%>%
                                group_by(dec)%>%summarise(min(avg3mou)))[[2]]
avg3mou1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avg3mou,n=10))%>%
                             group_by(dec)%>%summarise(max(avg3mou)))[[2]]
avg3mou1$varname<-rep("avg3mou",nrow(avg3mou1))
avg3mou1

#VAriable 26:avgmou
summary(dat1$avgmou)
dat1%>%mutate(dec=ntile(avgmou,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->avgmou1
avgmou1$N<-unclass(dat1%>%mutate(dec=ntile(avgmou,n=10))%>%
                     count(dec)%>%unname())[[2]]
avgmou1$churn_perc<-avgmou1$n/avgmou1$N
avgmou1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avgmou,n=10))%>%
                               group_by(dec)%>%summarise(min(avgmou)))[[2]]
avgmou1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avgmou,n=10))%>%
                            group_by(dec)%>%summarise(max(avgmou)))[[2]]
avgmou1$varname<-rep("avgmou",nrow(avgmou1))
avgmou1

#Variable27:avg3qty
summary(dat1$avg3qty)
dat1%>%mutate(dec=ntile(avg3qty,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->avg3qty1
avg3qty1$N<-unclass(dat1%>%mutate(dec=ntile(avg3qty,n=10))%>%
                      count(dec)%>%unname())[[2]]
avg3qty1$churn_perc<-avg3qty1$n/avg3qty1$N
avg3qty1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avg3qty,n=10))%>%
                                group_by(dec)%>%summarise(min(avg3qty)))[[2]]
avg3qty1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avg3qty,n=10))%>%
                             group_by(dec)%>%summarise(max(avg3qty)))[[2]]
avg3qty1$varname<-rep("avg3qty",nrow(avg3qty1))
avg3qty1

#Variable28:avgqty
summary(dat1$avgqty)
dat1%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avgqty1
avgqty1$N<-unclass(dat1%>%mutate(dec=ntile(avgqty,n=10))%>%
                     count(dec)%>%unname())[[2]]
avgqty1$churn_perc<-avgqty1$n/avgqty1$N
avgqty1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avgqty,n=10))%>%
                               group_by(dec)%>%summarise(min(avgqty)))[[2]]
avgqty1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avgqty,n=10))%>%
                            group_by(dec)%>%summarise(max(avgqty)))[[2]]
avgqty1$varname<-rep("avgqty",nrow(avgqty1))
avgqty1

#Variable 29:avg6mou
summary(dat1$avg6mou) #402 NA's
dat1%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avg6mou1
avg6mou1$N<-unclass(dat1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                      count(dec)%>%unname())[[2]]
avg6mou1$churn_perc<-avg6mou1$n/avg6mou1$N
avg6mou1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                                group_by(dec)%>%summarise(min(avg6mou)))[[2]]
avg6mou1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avg6mou,n=10))%>%
                             group_by(dec)%>%summarise(max(avg6mou)))[[2]]
avg6mou1$varname<-rep("avg6mou",nrow(avg6mou1))
avg6mou1

#Imputing missing values with mean
dat1$avg6mou[is.na(dat1$avg6mou)]<-521.6
summary(dat1$avg6mou)

#Variable 30:avg6qty
summary(dat1$avg6qty)
dat1%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avg6qty1
avg6qty1$N<-unclass(dat1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                      count(dec)%>%unname())[[2]]
avg6qty1$churn_perc<-avg6qty1$n/avg6qty1$N
avg6qty1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                                group_by(dec)%>%summarise(min(avg6qty)))[[2]]
avg6qty1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avg6qty,n=10))%>%
                             group_by(dec)%>%summarise(max(avg6qty)))[[2]]
avg6qty1$varname<-rep("avg6qty",nrow(avg6qty1))
avg6qty1

#imputing with mean
dat1$avg6qty[is.na(dat1$avg6qty)]<-182.5
summary(dat1$avg6qty) 

#Variable 31:age1....Used as factor variable
summary(dat1$age1)#214 NA's
dat1%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%
  filter(churn==1)->age1n
age1n$N<-unclass(dat1%>%mutate(dec=ntile(age1,n=6))%>%
                   count(dec)%>%unname())[[2]]
age1n$churn_perc<-age1n$n/age1n$N
age1n$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(age1,n=6))%>%
                             group_by(dec)%>%summarise(min(age1)))[[2]]
age1n$LessThan<-unclass(dat1%>%mutate(dec=ntile(age1,n=6))%>%
                          group_by(dec)%>%summarise(max(age1)))[[2]]
age1n$varname<-rep("age1",nrow(age1n))
age1n

#Imputing missing values with mean
dat1$age1[is.na(dat1$age1)]<-31.32
summary(dat1$age1)

#Variable 32:age2..F...less than 4 deciles...Used as factor variable
summary(dat1$age2)#214 NA's
dat1%>%mutate(dec=ntile(age2,n=6))%>%count(churn,dec)%>%
  filter(churn==1)->age2n
age2n$N<-unclass(dat1%>%mutate(dec=ntile(age2,n=6))%>%
                   count(dec)%>%unname())[[2]]

age2n$churn_perc<-age2n$n/age2n$N
age2n$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(age2,n=6))%>%
                             group_by(dec)%>%summarise(min(age2)))[[2]]
age2n$LessThan<-unclass(dat1%>%mutate(dec=ntile(age2,n=6))%>%
                          group_by(dec)%>%summarise(max(age2)))[[2]]
age2n$varname<-rep("age2",nrow(age2n))
age2n


#Imputing missing values with mean
dat1$age2[is.na(dat1$age2)]<-21.09
summary(dat1$age2)

#Variable33:models..F....less than 4 deciles
summary(dat1$models)
dat1%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->models1
models1$N<-unclass(dat1%>%mutate(dec=ntile(models,n=4))%>%
                   count(dec)%>%unname())[[2]]

models1$churn_perc<-models1$n/models1$N
models1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(models,n=4))%>%
                             group_by(dec)%>%summarise(min(models)))[[2]]
models1$LessThan<-unclass(dat1%>%mutate(dec=ntile(models,n=4))%>%
                          group_by(dec)%>%summarise(max(models)))[[2]]
models1$varname<-rep("models",nrow(models1))
models1


#Variable34:hnd_price....
summary(dat1$hnd_price)#130 NA's
dat1%>%mutate(dec=ntile(hnd_price,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->hnd_price1
hnd_price1$N<-unclass(dat1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                        count(dec)%>%unname())[[2]]
hnd_price1$churn_perc<-hnd_price1$n/hnd_price1$N
hnd_price1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                                  group_by(dec)%>%summarise(min(hnd_price)))[[2]]
hnd_price1$LessThan<-unclass(dat1%>%mutate(dec=ntile(hnd_price,n=10))%>%
                               group_by(dec)%>%summarise(max(hnd_price)))[[2]]
hnd_price1$varname<-rep("hnd_price",nrow(hnd_price1))
hnd_price1

#Imputing NA with mean
dat1$hnd_price[is.na(dat1$hnd_price)]<-105.12
summary(dat1$hnd_price)

#Variable 35:actvsubs...Used as factor variable
summary(dat1$actvsubs)
dat1%>%mutate(dec=ntile(actvsubs,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->actvsubs1
actvsubs1$N<-unclass(dat1%>%mutate(dec=ntile(actvsubs,n=4))%>%
                        count(dec)%>%unname())[[2]]
actvsubs1$churn_perc<-actvsubs1$n/actvsubs1$N
actvsubs1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(actvsubs,n=4))%>%
                                  group_by(dec)%>%summarise(min(actvsubs)))[[2]]
actvsubs1$LessThan<-unclass(dat1%>%mutate(dec=ntile(actvsubs,n=4))%>%
                               group_by(dec)%>%summarise(max(actvsubs)))[[2]]
actvsubs1$varname<-rep("actvsubs1",nrow(actvsubs1))
actvsubs1

#Variable36:uniqsubs..Used as factor variable
summary(dat1$uniqsubs)
dat1%>%mutate(dec=ntile(uniqsubs,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->uniqsubs1
uniqsubs1$N<-unclass(dat1%>%mutate(dec=ntile(uniqsubs,n=4))%>%
                       count(dec)%>%unname())[[2]]
uniqsubs1$churn_perc<-uniqsubs1$n/uniqsubs1$N
uniqsubs1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(uniqsubs,n=4))%>%
                                 group_by(dec)%>%summarise(min(uniqsubs)))[[2]]
uniqsubs1$LessThan<-unclass(dat1%>%mutate(dec=ntile(uniqsubs,n=4))%>%
                              group_by(dec)%>%summarise(max(uniqsubs)))[[2]]
uniqsubs1$varname<-rep("uniqsubs",nrow(uniqsubs1))
uniqsubs1

#Variable 37:forgntvl....Used as factor variable..dont impute na as categorical...less than 4 deciles
summary(dat1$forgntvl)
dat1%>%mutate(dec=ntile(forgntvl,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->forgntvl1
forgntvl1$N<-unclass(dat1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                       count(dec)%>%unname())[[2]]
forgntvl1$churn_perc<-forgntvl1$n/forgntvl1$N
forgntvl1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                                 group_by(dec)%>%summarise(min(forgntvl)))[[2]]
forgntvl1$LessThan<-unclass(dat1%>%mutate(dec=ntile(forgntvl,n=4))%>%
                              group_by(dec)%>%summarise(max(forgntvl)))[[2]]
forgntvl1$varname<-rep("forgntvl",nrow(forgntvl1))
forgntvl1

#Variable38:opk_dat_Mean...OMIT...Lessthan 4 deciles
summary(dat1$opk_dat_Mean)
dat1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->opk_dat_Mean1
opk_dat_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%
                       count(dec)%>%unname())[[2]]
opk_dat_Mean1$churn_perc<-opk_dat_Mean1$n/opk_dat_Mean1$N
opk_dat_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%
                                 group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]]
opk_dat_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%
                              group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]]
opk_dat_Mean1$varname<-rep("opk_dat_Mean",nrow(opk_dat_Mean1))
opk_dat_Mean1

#Variable 39:mtrcycle...Dont impute...categorical variable..Used as factor variable..less than 4 deciles
summary(dat1$mtrcycle)
dat1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->mtrcycle1
mtrcycle1$N<-unclass(dat1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                           count(dec)%>%unname())[[2]]
mtrcycle1$churn_perc<-mtrcycle1$n/mtrcycle1$N
mtrcycle1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                                     group_by(dec)%>%summarise(min(mtrcycle)))[[2]]
mtrcycle1$LessThan<-unclass(dat1%>%mutate(dec=ntile(mtrcycle,n=4))%>%
                                  group_by(dec)%>%summarise(max(mtrcycle)))[[2]]
mtrcycle1$varname<-rep("mtrcycle",nrow(mtrcycle1))
mtrcycle1


#Variable 40:truck..Dont impute..categorical...Used as factor variable
summary(dat1$truck)
dat1%>%mutate(dec=ntile(truck,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->truck1
truck1$N<-unclass(dat1%>%mutate(dec=ntile(truck,n=4))%>%
                       count(dec)%>%unname())[[2]]
truck1$churn_perc<-truck1$n/truck1$N
truck1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(truck,n=4))%>%
                                 group_by(dec)%>%summarise(min(truck)))[[2]]
truck1$LessThan<-unclass(dat1%>%mutate(dec=ntile(truck,n=4))%>%
                              group_by(dec)%>%summarise(max(truck)))[[2]]
truck1$varname<-rep("truck",nrow(truck1))
truck1

#Variable 41:roam_Mean...Less than 4 deciles so OMIT
summary(dat1$roam_Mean)
dat1%>%mutate(dec=ntile(roam_Mean,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->roam_Mean1
roam_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(roam_Mean,n=4))%>%
                    count(dec)%>%unname())[[2]]
roam_Mean1$churn_perc<-roam_Mean1$n/roam_Mean1$N
roam_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(roam_Mean,n=4))%>%
                              group_by(dec)%>%summarise(min(roam_Mean)))[[2]]
roam_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(roam_Mean,n=4))%>%
                           group_by(dec)%>%summarise(max(roam_Mean)))[[2]]
roam_Mean1$varname<-rep("roam_Mean",nrow(roam_Mean1))
roam_Mean1
dat1$roam_Mean[is.na(dat1$roam_Mean)]<-1.441 # imputing with mean
summary(dat1$roam_Mean) 


#Variable 42:recv_sms_Mean..OMIT..less than 4 deciles
summary(dat1$recv_sms_Mean)
dat1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->recv_sms_Mean1
recv_sms_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%
                        count(dec)%>%unname())[[2]]
recv_sms_Mean1$churn_perc<-recv_sms_Mean1$n/recv_sms_Mean1$N
recv_sms_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%
                                  group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]]
recv_sms_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%
                               group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]]
recv_sms_Mean1$varname<-rep("recv_sms_Mean",nrow(recv_sms_Mean1))
recv_sms_Mean1

#Variable 43:blck_dat_Mean...#OMIT..less than 4 deciles
summary(dat1$blck_dat_Mean)
dat1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%
  count(churn,dec)%>%filter(churn==1)->blck_dat_Mean1
blck_dat_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%
                            count(dec)%>%unname())[[2]]
blck_dat_Mean1$churn_perc<-blck_dat_Mean1$n/blck_dat_Mean1$N
blck_dat_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%
                                      group_by(dec)%>%summarise(min(blck_dat_Mean)))[[2]]
blck_dat_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(blck_dat_Mean,n=10))%>%
                                   group_by(dec)%>%summarise(max(blck_dat_Mean)))[[2]]
blck_dat_Mean1$varname<-rep("blck_dat_Mean",nrow(blck_dat_Mean1))
blck_dat_Mean1

#Variable44:mou_pead_Mean..OMIT
summary(dat1$mou_pead_Mean)
dat1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%
  count(churn,dec)%>%filter(churn==1)->mou_pead_Mean1
mou_pead_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%
                            count(dec)%>%unname())[[2]]
mou_pead_Mean1$churn_perc<-mou_pead_Mean1$n/mou_pead_Mean1$N
mou_pead_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%
                                      group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]]
mou_pead_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%
                                   group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]]
mou_pead_Mean1$varname<-rep("mou_pead_Mean",nrow(mou_pead_Mean1))
mou_pead_Mean1

#Variable45:churn

#Variable 46:da_Mean
summary(dat1$da_Mean)#26 NA's
dat1%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->da_Mean1
da_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(da_Mean,n=4))%>%
                      count(dec)%>%unname())[[2]]
da_Mean1$churn_perc<-da_Mean1$n/da_Mean1$N
da_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(da_Mean,n=4))%>%
                                group_by(dec)%>%summarise(min(da_Mean)))[[2]]
da_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(da_Mean,n=4))%>%
                             group_by(dec)%>%summarise(max(da_Mean)))[[2]]
da_Mean1$varname<-rep("da_Mean",nrow(da_Mean1))
da_Mean1

#Imputing
dat1$da_Mean[is.na(dat1$da_Mean)]<-0.9404 # imputing with mean
summary(dat1$da_Mean)

#Variable47:da_Range
summary(dat1$da_Range) #26 NA's
dat1%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->da_Range1
da_Range1$N<-unclass(dat1%>%mutate(dec=ntile(da_Range,n=4))%>%
                       count(dec)%>%unname())[[2]]
da_Range1$churn_perc<-da_Range1$n/da_Range1$N
da_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(da_Range,n=4))%>%
                                 group_by(dec)%>%summarise(min(da_Range)))[[2]]
da_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(da_Range,n=4))%>%
                              group_by(dec)%>%summarise(max(da_Range)))[[2]]
da_Range1$varname<-rep("da_Range",nrow(da_Range1))
da_Range1

#Imputing
dat1$da_Range[is.na(dat1$da_Range)]<-1.695 # imputing with mean
summary(dat1$da_Range)


#Variable 48:datovr_Mean..Omit...less than 4 deciles
summary(dat1$datovr_Mean) #26 NA's
dat1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->datovr_Mean1
datovr_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%
                       count(dec)%>%unname())[[2]]
datovr_Mean1$churn_perc<-datovr_Mean1$n/datovr_Mean1$N
datovr_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%
                                 group_by(dec)%>%summarise(min(datovr_Mean)))[[2]]
datovr_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(datovr_Mean,n=4))%>%
                              group_by(dec)%>%summarise(max(datovr_Mean)))[[2]]
datovr_Mean1$varname<-rep("datovr_Mean",nrow(datovr_Mean1))
datovr_Mean1

#Imputing
dat1$datovr_Mean[is.na(dat1$datovr_Mean)]<-0.2392 # imputing with mean
summary(dat1$datovr_Mean)

#Variable 49:datovr_Range...OMIT
summary(dat1$datovr_Range) #26 NA's
dat1%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->datovr_Range1
datovr_Range1$N<-unclass(dat1%>%mutate(dec=ntile(datovr_Range,n=4))%>%
                          count(dec)%>%unname())[[2]]
datovr_Range1$churn_perc<-datovr_Range1$n/datovr_Range1$N
datovr_Range1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(datovr_Range,n=4))%>%
                                    group_by(dec)%>%summarise(min(datovr_Range)))[[2]]
datovr_Range1$LessThan<-unclass(dat1%>%mutate(dec=ntile(datovr_Range,n=4))%>%
                                 group_by(dec)%>%summarise(max(datovr_Range)))[[2]]
datovr_Range1$varname<-rep("datovr_Range",nrow(datovr_Range1))
datovr_Range1

#Imputing
dat1$datovr_Range[is.na(dat1$datovr_Range)]<-0.6963 # imputing with mean
summary(dat1$datovr_Range)

#Variable 50:drop_dat_Mean...OMIT
summary(dat1$drop_dat_Mean) #26 NA's
dat1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%
  filter(churn==1)->drop_dat_Mean1
drop_dat_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%
                           count(dec)%>%unname())[[2]]
drop_dat_Mean1$churn_perc<-drop_dat_Mean1$n/drop_dat_Mean1$N
drop_dat_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%
                                     group_by(dec)%>%summarise(min(drop_dat_Mean)))[[2]]
drop_dat_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%
                                  group_by(dec)%>%summarise(max(drop_dat_Mean)))[[2]]
drop_dat_Mean1$varname<-rep("drop_dat_Mean",nrow(drop_dat_Mean1))
drop_dat_Mean1

#Imputing
dat1$drop_dat_Mean[is.na(dat1$drop_dat_Mean)]<-0.05229 # imputing with mean
summary(dat1$drop_dat_Mean)

#Variable 52:drop_vce_Mean
summary(dat1$drop_vce_Mean)
dat1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->drop_vce_Mean1
drop_vce_Mean1$N<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%
                            count(dec)%>%unname())[[2]]
drop_vce_Mean1$churn_perc<-drop_vce_Mean1$n/drop_vce_Mean1$N
drop_vce_Mean1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%
                                      group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
drop_vce_Mean1$LessThan<-unclass(dat1%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%
                                   group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
drop_vce_Mean1$varname<-rep("drop_vce_Mean",nrow(drop_vce_Mean1))
drop_vce_Mean1

#VAriable 53:adjmou
summary(dat1$adjmou)
dat1%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->adjmou1
adjmou1$N<-unclass(dat1%>%mutate(dec=ntile(adjmou,n=10))%>%
                     count(dec)%>%unname())[[2]]
adjmou1$churn_perc<-adjmou1$n/adjmou1$N
adjmou1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(adjmou,n=10))%>%
                               group_by(dec)%>%summarise(min(adjmou)))[[2]]
adjmou1$LessThan<-unclass(dat1%>%mutate(dec=ntile(adjmou,n=10))%>%
                            group_by(dec)%>%summarise(max(adjmou)))[[2]]
adjmou1$varname<-rep("adjmou",nrow(adjmou1))
adjmou1

#Variable 54:totrev
summary(dat1$totrev)
dat1%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->totrev1
totrev1$N<-unclass(dat1%>%mutate(dec=ntile(totrev,n=10))%>%
                     count(dec)%>%unname())[[2]]
totrev1$churn_perc<-totrev1$n/totrev1$N
totrev1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(totrev,n=10))%>%
                               group_by(dec)%>%summarise(min(totrev)))[[2]]
totrev1$LessThan<-unclass(dat1%>%mutate(dec=ntile(totrev,n=10))%>%
                            group_by(dec)%>%summarise(max(totrev)))[[2]]
totrev1$varname<-rep("totrev",nrow(totrev1))
totrev1


#Variable55:adjrev
summary(dat1$adjrev)
dat1%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->adjrev1
adjrev1$N<-unclass(dat1%>%mutate(dec=ntile(adjrev,n=10))%>%
                     count(dec)%>%unname())[[2]]
adjrev1$churn_perc<-adjrev1$n/adjrev1$N
adjrev1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(adjrev,n=10))%>%
                               group_by(dec)%>%summarise(min(adjrev)))[[2]]
adjrev1$LessThan<-unclass(dat1%>%mutate(dec=ntile(adjrev,n=10))%>%
                            group_by(dec)%>%summarise(max(adjrev)))[[2]]
adjrev1$varname<-rep("adjrev",nrow(adjrev1))
adjrev1

#Variable 56:avgrev
summary(dat1$avgrev)
dat1%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%
  filter(churn==1)->avgrev1
avgrev1$N<-unclass(dat1%>%mutate(dec=ntile(avgrev,n=10))%>%
                     count(dec)%>%unname())[[2]]
avgrev1$churn_perc<-avgrev1$n/avgrev1$N
avgrev1$GreaterThan<-unclass(dat1%>%mutate(dec=ntile(avgrev,n=10))%>%
                               group_by(dec)%>%summarise(min(avgrev)))[[2]]
avgrev1$LessThan<-unclass(dat1%>%mutate(dec=ntile(avgrev,n=10))%>%
                            group_by(dec)%>%summarise(max(avgrev)))[[2]]
avgrev1$varname<-rep("avgrev",nrow(avgrev1))
avgrev1

#Putting together all the required variables to get final object
?rbind


#combining all deciled variables
datcontf<-rbind(mou_Mean1,totmrc_Mean1,rev_Range1,mou_Range1,change_mou1,
             drop_blk_Mean1,drop_vce_Range1,owylis_vce_Range1,
             mou_opkv_Range1,months1,totcalls1,income1,eqpdays1,
             callwait_Mean1,iwylis_vce_Mean1,adjqty1,ovrrev_Mean1,
             rev_Mean1,ovrmou_Mean1,comp_vce_Mean1,plcd_vce_Mean1,
             avg3mou1,avgmou1,avg3qty1,avgqty1,avg6mou1,avg6qty1,
             hnd_price1,da_Mean1,da_Range1,drop_vce_Mean1,
             adjmou1,totrev1,adjrev1,avgrev1,drop_dat_Mean1,datovr_Range1,
             datovr_Mean1,mou_pead_Mean1,blck_dat_Mean1,recv_sms_Mean1,
             roam_Mean1,opk_dat_Mean1,ccrndmou_Range1,
             callwait_Range1,custcare_Mean1)

#Writing all deciled variables to a file
write.csv(datcontf,"DeciledContinuousVariables.csv",row.names = F)


#Decile binning of continuous variables

#Variable1:asl_flag
summary(dat1$asl_flag)
dat1%>%count(churn,levels=asl_flag)%>%filter(churn==1)->asl_flag1
asl_flag1$N<-unclass(dat1%>%filter(asl_flag%in%asl_flag1$levels)%>%count(asl_flag))[[2]]
asl_flag1$ChurnPerc<-asl_flag1$n/asl_flag1$N
asl_flag1$Var.Name<-rep("asl_flag",nrow(asl_flag1))
asl_flag1


#Variable2:prizm_social_one
summary(dat1$prizm_social_one)
dat1%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)->prizm_social_one1
prizm_social_one1$N<-unclass(dat1%>%filter(prizm_social_one%in%prizm_social_one1$levels)%>%count(prizm_social_one))[[2]]
prizm_social_one1$ChurnPerc<-prizm_social_one1$n/prizm_social_one1$N
prizm_social_one1$Var.Name<-rep("prizm_social_one",nrow(prizm_social_one1))
prizm_social_one1

dat1$prizm_social_one[is.na(dat1$prizm_social_one)]<-"T"
summary(dat1$prizm_social_one)

#Variable 3:refurb_new
summary(dat1$refurb_new)
dat1%>%count(churn,levels=refurb_new)%>%filter(churn==1)->refurb_new1
refurb_new1$N<-unclass(dat1%>%filter(refurb_new%in%refurb_new1$levels)%>%count(refurb_new))[[2]]
refurb_new1$ChurnPerc<-refurb_new1$n/refurb_new1$N
refurb_new1$Var.Name<-rep("refurb_new",nrow(refurb_new1))
refurb_new1

#Variable4:hnd_webcap
summary(dat1$hnd_webcap)
dat1%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)->hnd_webcap1
hnd_webcap1$N<-unclass(dat1%>%filter(hnd_webcap%in%hnd_webcap1$levels)%>%count(hnd_webcap))[[2]]
hnd_webcap1$ChurnPerc<-hnd_webcap1$n/hnd_webcap1$N
hnd_webcap1$Var.Name<-rep("hnd_webcap",nrow(hnd_webcap1))
hnd_webcap1

dat1$hnd_webcap[is.na(dat1$hnd_webcap)]<-"WC"
summary(dat1$hnd_webcap)

#Variable5:car_buy
summary(dat1$car_buy)
dat1%>%count(churn,levels=car_buy)%>%filter(churn==1)->car_buy1
car_buy1$N<-unclass(dat1%>%filter(car_buy%in%car_buy1$levels)%>%count(car_buy))[[2]]
car_buy1$ChurnPerc<-car_buy1$n/car_buy1$N
car_buy1$Var.Name<-rep("car_buy",nrow(car_buy1))
car_buy1

dat1$car_buy[is.na(dat1$car_buy)]<-"New"
summary(dat1$car_buy)

#Variable6:csa...Removing because of too many categories
summary(dat1$csa)
dat1%>%count(churn,levels=csa)%>%filter(churn==1)->csa1
csa1$N<-unclass(dat1%>%filter(csa%in%csa1$levels)%>%count(csa))[[2]]
csa1$ChurnPerc<-csa1$n/csa1$N
csa1$Var.Name<-rep("csa",nrow(csa1))
csa1
View(csa1)

#variable 7:Marital
summary(dat1$marital)
dat1%>%count(churn,levels=marital)%>%filter(churn==1)->marital1
marital1$N<-unclass(dat1%>%filter(marital%in%marital1$levels)%>%count(marital))[[2]]
marital1$ChurnPerc<-marital1$n/marital1$N
marital1$Var.Name<-rep("marital",nrow(marital1))
marital1

dat1$marital[is.na(dat1$marital)] <- "S"
summary(dat1$marital)

dat1$marital<-gsub("B|S","Single",dat1$marital)
dat1$marital<-gsub("U","Unknown",dat1$marital)
dat1$marital
dat1$marital<-gsub("M|A","Married",dat1$marital)
summary(dat1$marital)
str(dat1$marital)
dat1$marital<-as.factor(dat1$marital)

#Variable7:ethnic...
summary(dat1$ethnic)
dat1%>%count(churn,levels=ethnic)%>%filter(churn==1)->ethnic1
ethnic1$N<-unclass(dat1%>%filter(ethnic%in%ethnic1$levels)%>%count(ethnic))[[2]]
ethnic1$ChurnPerc<-ethnic1$n/ethnic1$N
ethnic1$Var.Name<-rep("ethnic",nrow(ethnic1))
ethnic1

dat1$ethnic[is.na(dat1$ethnic)]<-"N"
summary(dat1$ethnic)

#Variable 8:area...
summary(dat1$area)
dat1%>%count(churn,levels=area)%>%filter(churn==1)->area1
area1$N<-unclass(dat1%>%filter(area%in%area1$levels)%>%count(area))[[2]]
area1$ChurnPerc<-area1$n/area1$N
area1$Var.Name<-rep("area",nrow(area1))
area1

#Variable:crclscod....Removing because of too many levels
summary(dat1$crclscod)
dat1%>%count(churn,levels=crclscod)%>%filter(churn==1)->crclscod1
crclscod1$N<-unclass(dat1%>%filter(crclscod%in%crclscod1$levels)%>%count(crclscod))[[2]]
crclscod1$ChurnPerc<-crclscod1$n/crclscod1$N
crclscod1$Var.Name<-rep("crclscod",nrow(crclscod1))
crclscod1

#Converting continuous variables to factor variables:
#Variable 1:age1
summary(dat1$age1)
#dat1$newage1<-ifelse(dat1$age1<=25,"Young",ifelse((dat1$age1>25 & dat1$age1<=55),"Middle","Old"))
#dat1$newage1

dat1$newage1<-ifelse(dat1$age1==0,"Default",ifelse(dat1$age1<=30,"Young",
                                                     ifelse(dat1$age1>30 & dat1$age1<=55,"Middle","Old")))

summary(dat1$newage1)
dat1%>%count(churn,levels=newage1)%>%filter(churn==1)->newage1n
newage1n$N<-unclass(dat1%>%filter(newage1%in%newage1n$levels)%>%count(newage1))[[2]]
newage1n$ChurnPerc<-newage1n$n/newage1n$N
newage1n$Var.Name<-rep("newage1",nrow(newage1n))
newage1n

#DRop dat1$age1

#Variable2:age2
summary(dat1$age2)
#dat1$newage2<-ifelse(dat1$age2<=25,"Young",ifelse((dat1$age2>25 & dat1$age2<=55),"Middle","Old"))
#dat1$newage2

dat1$newage2<-ifelse(dat1$age2==0,"Default",ifelse(dat1$age2<=30,"Young",
                                                    ifelse(dat1$age2>30 & dat1$age2<=55,"Middle","Old")))
summary(dat1$newage2)
dat1%>%count(churn,levels=newage2)%>%filter(churn==1)->newage2n
newage2n$N<-unclass(dat1%>%filter(newage2%in%newage2n$levels)%>%count(newage2))[[2]]
newage2n$ChurnPerc<-newage2n$n/newage2n$N
newage2n$Var.Name<-rep("newage2",nrow(newage2n))
newage2n

#Drop dat1$age2

#Variable3:models:
summary(dat1$models)
unique(dat1$models)
dat1$models<-as.factor(dat1$models)

summary(dat1$models)
dat1%>%count(churn,levels=models)%>%filter(churn==1)->models2
models2$N<-unclass(dat1%>%filter(models%in%models2$levels)%>%count(models))[[2]]
models2$ChurnPerc<-models2$n/models2$N
models2$Var.Name<-rep("newage2",nrow(models2))
models2
str(dat1$models)


#Variable 4:actvsubs
unique(dat1$actvsubs)
dat1$actvsubs<-as.factor(dat1$actvsubs)

summary(dat1$actvsubs)
dat1%>%count(churn,levels=actvsubs)%>%filter(churn==1)->actvsubs1
actvsubs1$N<-unclass(dat1%>%filter(actvsubs%in%actvsubs1$levels)%>%count(actvsubs))[[2]]
actvsubs1$ChurnPerc<-actvsubs1$n/actvsubs1$N
actvsubs1$Var.Name<-rep("actvsubs",nrow(actvsubs1))
actvsubs1
str(dat1$actvsubs)


#Variable 5:uniqsubs
unique(dat1$uniqsubs)
dat1$uniqsubs<-as.factor(dat1$uniqsubs)

summary(dat1$uniqsubs)
dat1%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->uniqsubs1
uniqsubs1$N<-unclass(dat1%>%filter(uniqsubs%in%uniqsubs1$levels)%>%count(uniqsubs))[[2]]
uniqsubs1$ChurnPerc<-uniqsubs1$n/uniqsubs1$N
uniqsubs1$Var.Name<-rep("uniqsubs",nrow(uniqsubs1))
uniqsubs1
str(dat1$uniqsubs)

#Variable 6:forgntvl
unique(dat1$forgntvl)
summary(dat1$forgntvl)

summary(dat1$forgntvl)
dat1%>%count(churn,levels=forgntvl)%>%filter(churn==1)->forgntvl1
forgntvl1$N<-unclass(dat1%>%filter(forgntvl%in%forgntvl1$levels)%>%count(forgntvl))[[2]]
forgntvl1$ChurnPerc<-forgntvl1$n/forgntvl1$N
forgntvl1$Var.Name<-rep("forgntvl",nrow(forgntvl1))
forgntvl1

dat1$forgntvl<-as.factor(dat1$forgntvl)
dat1$forgntvl

#Variable 7:mtrcycle
summary(dat1$mtrcycle)
dat1%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->mtrcycle1
mtrcycle1$N<-unclass(dat1%>%filter(mtrcycle%in%mtrcycle1$levels)%>%count(mtrcycle))[[2]]
mtrcycle1$ChurnPerc<-mtrcycle1$n/mtrcycle1$N
mtrcycle1$Var.Name<-rep("mtrcycle",nrow(mtrcycle1))
mtrcycle1

dat1$mtrcycle<-as.factor(dat1$mtrcycle)
dat1$mtrcycle

#Variable8:truck
summary(dat1$truck)
dat1%>%count(churn,levels=truck)%>%filter(churn==1)->truck1
truck1$N<-unclass(dat1%>%filter(truck%in%truck1$levels)%>%count(truck))[[2]]
truck1$ChurnPerc<-truck1$n/truck1$N
truck1$Var.Name<-rep("truck",nrow(truck1))
truck1

dat1$truck<-as.factor(dat1$truck)
dat1$truck

#Variable churn:
summary(dat1$churn)

datcatf<-rbind(asl_flag1,prizm_social_one1,refurb_new1,hnd_webcap1,car_buy1,csa1,marital1,ethnic1,
               area1,crclscod1,newage1n,newage2n,models2,forgntvl1,mtrcycle1,truck1,actvsubs1,
               uniqsubs1)

#Writing all deciled variables to a file
write.csv(datcatf,"DeciledCategoricalVariables.csv",row.names = F)

#Dropping all the variables we do not require from the data set


#Variable14:custcare_Mean...#OMIT because it gives less number of deciles
#Variable 17:callwait_Range----->OMIT less that 4 deciles
#VAriable18:ccrndmou_Range------>OMIT less than 4 deciles
#Variable38:opk_dat_Mean...OMIT...Lessthan 4 deciles
#Variable 42:recv_sms_Mean..OMIT..less than 4 deciles
#Variable 43:blck_dat_Mean...#OMIT..less than 4 deciles
#Variable44:mou_pead_Mean..OMIT
#Variable 48:datovr_Mean..Omit...less than 4 deciles
#Variable 49:datovr_Range...OMIT
#Variable 50:drop_dat_Mean...OMIT
#drop csa and crclscod
#DRop dat1$age1
#Drop dat1$age2
#Drop customer_ID

datfinal=select(dat1,-c(custcare_Mean,callwait_Range,ccrndmou_Range,opk_dat_Mean,
                        recv_sms_Mean,blck_dat_Mean,mou_pead_Mean,datovr_Mean,
                        datovr_Range,drop_dat_Mean,age1,age2,csa,crclscod))
datfinal$retdays<-as.factor(dat1$retdays)

#****************Building the Model**************#
#Logistic Regression Model

set.seed(200)
index<-sample(nrow(datfinal),0.70*nrow(datfinal),replace=F)
train<-datfinal[index,]
test<-datfinal[-index,]
dim(test)
dim(train)
names(train)

#Checking Churn Rate 
table(train$churn)/nrow(train)
table(test$churn)/nrow(test)

#Model building

reg<-glm(churn~.,data=train[,-53],family="binomial")
summary(reg)

#Building dummy variables for the significant variables:

summary(datfinal$asl_flag)
train$asl_flag_Y<-ifelse(train$asl_flag == "Y", 1, 0)
test$asl_flag_Y<-ifelse(test$asl_flag == "Y", 1, 0)

summary(train$area)
train$area_ohio<-ifelse(train$area == "OHIO AREA", 1, 0)
test$area_ohio<-ifelse(test$area == "OHIO AREA", 1, 0)

summary(train$marital)
train$maritalSingle<-ifelse(train$marital == "Single", 1, 0)
test$maritalSingle<-ifelse(test$marital == "Single", 1, 0)

summary(train$ethnic)
train$ethnicO<-ifelse(train$ethnic == "O", 1, 0)
test$ethnicO<-ifelse(test$ethnic == "O", 1, 0)

summary(train$models)
train$models3<-ifelse(train$models == 3, 1, 0)
test$models3<-ifelse(test$models == 3, 1, 0)

summary(train$models)
train$models4<-ifelse(train$models == 4, 1, 0)
test$models4<-ifelse(test$models == 4, 1, 0)

summary(train$uniqsubs)
train$uniqsubs2<-ifelse(train$uniqsubs == 2, 1, 0)
test$uniqsubs2<-ifelse(test$uniqsubs == 2, 1, 0)

summary(train$uniqsubs)
train$uniqsubs4<-ifelse(train$uniqsubs == 4, 1, 0)
test$uniqsubs4<-ifelse(test$uniqsubs == 4, 1, 0)

summary(train$retdays)
train$retdays1<-ifelse(train$retdays==1,1,0)
test$retdays1<-ifelse(test$retdays==1,1,0)

#Iteration1:
reg1<-glm(churn~totmrc_Mean+mou_Range+drop_vce_Range+months+eqpdays+callwait_Mean+avgmou+totrev+
            adjrev+asl_flag_Y+area_ohio+maritalSingle+ethnicO+models3+models4+uniqsubs2+
            uniqsubs4+retdays1,data=train,family="binomial")
summary(reg1)

#Iteration2:
reg2<-glm(churn~totmrc_Mean+mou_Range+months+eqpdays+callwait_Mean+totrev+adjrev+asl_flag_Y+
            maritalSingle+ethnicO+models4+uniqsubs2+retdays1,data=train,family="binomial")
summary(reg2)

#Iteration3:
reg3<-glm(churn~totmrc_Mean+mou_Range+months+eqpdays+totrev+adjrev+asl_flag_Y+maritalSingle+
            ethnicO+uniqsubs2+retdays1,data=train,family="binomial")
summary(reg3)

#Iteration4:
reg4<-glm(churn~totmrc_Mean+mou_Range+months+eqpdays+totrev+adjrev+asl_flag_Y+
            ethnicO+uniqsubs2+retdays1,data=train,family="binomial")
summary(reg4)

#iteration 5:
reg5<-glm(churn~totmrc_Mean+mou_Range+months+eqpdays+totrev+asl_flag_Y+
            ethnicO+uniqsubs2+retdays1,data=train,family="binomial")
summary(reg5)

#Checking for multicollinearity
library(car)
vif(reg5)
#No multicollinearity

#******************Validating the Model******************#
#Checking data on train data set
train$predicted <- reg5$fitted.values
train$predicted

head(train$churn)
head(train$predicted)

#ROCR curve
library(ROCR)
pred<-prediction(train$predicted,train$churn)
class(pred)
perf <- performance(pred,"acc")
class(perf)
class(perf@x.values)
cutoffprob <- as.numeric(unlist(perf@x.values))
cutoffprob

class(perf@y.values)
accuracies <- as.numeric(unlist(perf@y.values))

cutoffs <- data.frame(cutoffprob, accuracies)

cutoffs <- cutoffs[order(cutoffs$accuracies, decreasing=TRUE),]
View(cutoffs)
train$predclass <- ifelse(train$predicted>0.5180983,1,0)#Prob corresponding to highestaccuracy

#KAPPA
library(caret)
library(irr)

kappa2(data.frame(train$churn,train$predclass))

#Confusion Matrix
confusionMatrix(as.factor(train$churn),as.factor(train$predclass), positive = "1")
?confusionMatrix
#Accuracy of 76 % obtained by model

#ROC curve
perf<-performance(pred,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(perf,col="red")
abline(0,1, lty = 8, col = "blue")
auc<-performance(pred,"auc")
auc

#AUC of 0.60 obtained by model

#Gains Chart
library(gains)
gains(as.numeric(train$churn),train$predicted, groups =10)
quantile(train$predicted, seq(0,1,0.1))


#Checking on test data
test$pred <- predict(reg5, type = "response",newdata = test)

#ROCR curve:
library(ROCR)
pred1<-prediction(test$pred,test$churn)
class(pred1)
pref <- performance(pred1,"acc")
class(pref)
pref

class(pref@x.values)
cutoffprob1 <- as.numeric(unlist(pref@x.values))
cutoffprob1

class(pref@y.values)
accuracies1 <- as.numeric(unlist(pref@y.values))

cutoffs1 <- data.frame(cutoffprob1, accuracies1 )
cutoffs1 <- cutoffs1[order(cutoffs1$accuracies1, decreasing=TRUE),]

View(cutoffs1)

test$predclass2<- ifelse(test$pred>0.4983576,1,0)#Prob corresponding to highestaccuracy

#KAPPA
kappa2(data.frame(test$churn,test$predclass2))

#Confusion MAtrix
confusionMatrix(as.factor(test$churn),as.factor(test$predclass2), positive = "1")
#Accuracy of 76 % obtained

pref<-performance(pred1,"tpr","fpr") #tpr=TP/P fpr=FP/N
plot(pref,col="red")

abline(0,1, lty = 8, col = "blue")
auc1<-performance(pred1,"auc")
auc1
#AUC of 0.60 obtained

gains(as.numeric(test$churn),test$pred, groups =10)
quantile(test$pred, seq(0,1,0.1))

#Targeting top 10 customers
targeted <- which(test$pred >= 0.31377201)
targeted



summary(reg1)

#*************Questions***********#
#1.Question 1:
head(sort(abs(reg5$coefficients),decreasing = T),10)
summary(reg5)

#Factors        Beta coeffs
#retdays1	      1.006806112
#ethnicO	      0.415694888
#asl_flag_Y	   -0.322195722
#uniqsubs2	    0.23742313
#months	       -0.026512076

#With a unit increase in :

#retdays1 there is an increase of 1.00680611 units in churn 
#ethnicO there is an increase of 0.41569489 units in churn
#uniqsubs2 there is an increase of 0.23742313 units in churn
#asl_flag_Y there is a decrease of 0.32219572 units in churn
#months there is a decrease of 0.02651208 units in churn


#2a:Question 2a:
#Cost and Billing Factors:
#totmrc_Mean -0.00546467  0.00135743  
#mou_Range    0.00022185  0.00006337   3.501             
#totrev       0.00019465

#These factors have no much impact on Churn so Cost and Billing not very important for churn

#Network and Service Quality
#retdays1 there is an increase of 1.00680611 units in churn 
#retdays1 has a huge impact on churn 
#Therefore Network and Service Quality is an   important factor in influencing Churn


#2b:Question 2b:
#Data connectivity issues :
quantile(dat1$opk_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dat1$blck_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dat1$datovr_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dat1$datovr_Range,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
quantile(dat1$drop_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))

#Firstly none of the above specified factors are significant factors
#Secondly it is noticed that only 10-20 % of total customers make use of the Internet Services offered by Mobicom
#Hence Data Connectivity Issues is not an important factor in influencing churn

#3.Question 3:
#Factors affecting overage:
#ovrrev_Mean: Mean overage revenue
#ovrmou_Mean: Mean overage minutes of use
#datovr_Mean: Mean revenue of data overage
#datovr_Range: Range of revenue of data overage

#None of the above listed factors are found to be significant factors in determining churn
#Therefore a customer using an Optimal Rate Plan or Non Optimal Rate Plan has no impact on churn
#Hence Rate Plan Migration would not be suggested as a Proactive Retention Strategy



#4.Question 4:
#from gains chart
gains(as.numeric(test$churn),test$pred, groups =10)
quantile(test$pred, seq(0,1,0.1))

test$prob<-predict(reg5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))

pred4<-predict(reg5, type="response", newdata=test)
pred4<-ifelse(pred4>=0.28408809 , 1, 0)
table(pred4,test$churn)
View(test)

#The predicted probabilities of the top 20 % of customers who are likely to churn is in the range of 0.2840881 and 0.6932365

Targeted<-test[test$prob>0.28408809 & test$prob<=0.69323655 & test$churn=="1","Customer_ID"]
Targeted<-as.data.frame(Targeted)
View(Targeted)
nrow(Targeted)

#These 261 customers should be actively targeted.
#Writing out the customer IDs of the customers to be targetted

write.csv(Targeted,"TargetProb1_Customers.csv",row.names = F)

#5:Question 5:
#Dividing customer based on probability of churn Into Low,High and Medium Score 
pred5<-predict(reg5, type="response", newdata=test)
test$prob<-predict(reg5,type="response",newdata=test)
quantile(test$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
pred6<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))

##Dividing customer based on Revenue Into Low,High and Medium Revenue

table(pred6,test$churn)
str(test$totrev)
quantile(test$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
Revenue_Levels<-ifelse(test$totrev<670.076,"Low_Revenue",ifelse(test$totrev>=670.076 & 
                                                                  test$totrev<922.456,"Medium_Revenue","High_Revenue"))
table(Revenue_Levels,test$churn)


table(pred6,Revenue_Levels)


test$prob_levels<-ifelse(pred5<0.20,"Low_Score",ifelse(pred5>=0.20 & pred5<0.30,"Medium_Score","High_Score"))
test$Revenue_Levels<-ifelse(test$totrev<670.076,"Low_Revenue",ifelse(test$totrev>=670.076 & 
                                                                       test$totrev<922.456,"Medium_Revenue","High_Revenue"))
#Considering customers giving high revenue and hig probability of churning
Targeted1<-test[test$prob_levels=="High_Score" & test$Revenue_Levels=="High_Revenue","Customer_ID"]
Targeted1<-as.data.frame(Targeted1)
nrow(Targeted1)

#Writing out the Customer IDs of the customers to be targetted:
write.csv(Targeted1,"High_Revenue_Target_Customers.csv",row.names = F)

