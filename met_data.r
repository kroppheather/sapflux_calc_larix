setwd("c:\\Users\\hkropp\\Google Drive\\sap_flux\\R\\fixed_met")

datmet<-read.csv("met_w_gap.csv")

names(datmet)

#set up day and doy indexes
doy<-c(rep(194, 15),rep(seq(195,365),each=48),rep(seq(1,365),each=48),rep(seq(1,248),each=48),
rep(249,39))

dayall<-c(rep(1,15),rep(seq(2,785),each=48),rep(786,39))

#fit for Bulldozer Airtemp
fitT.dozer<-lm(datmet$Temp.HOBO~datmet$Temp.DOZER)
summary(fitT.dozer)

#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       -0.809121   0.042875  -18.87   <2e-16 ***
#datmet$Temp.DOZER  0.968822   0.004354  222.49   <2e-16 ***

#Residual standard error: 1.516 on 2097 degrees of freedom
 # (35588 observations deleted due to missingness)
#Multiple R-squared:  0.9594,    Adjusted R-squared:  0.9593 
#F-statistic: 4.95e+04 on 1 and 2097 DF,  p-value: < 2.2e-16

#fit for Bulldozer RH
fitRH.dozer<-lm(datmet$RH.HOBO~datmet$RH.DOZER)
summary(fitRH.dozer)

#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     14.38800    0.91772   15.68   <2e-16 ***
#datmet$RH.DOZER  0.73037    0.01267   57.66   <2e-16 ***

#Residual standard error: 12.03 on 2068 degrees of freedom
 # (35617 observations deleted due to missingness)
#Multiple R-squared:  0.6165,    Adjusted R-squared:  0.6163 
#F-statistic:  3324 on 1 and 2068 DF,  p-value: < 2.2e-16

#fit for Bulldozer PAR
fitP.dozer1<-lm(datmet$PAR.HOBO[datmet$PAR.DOZER<100]~datmet$PAR.DOZER[datmet$PAR.DOZER<100])
summary(fitP.dozer1)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)
#(Intercept)                              107.42734    9.22044   11.65   <2e-16
#datmet$PAR.DOZER[datmet$PAR.DOZER > 100]   0.82555    0.01051   78.56   <2e-16
#                                            
#datmet$PAR.DOZER[datmet$PAR.DOZER > 100] ***

#Residual standard error: 238.3 on 2392 degrees of freedom
 # (33821 observations deleted due to missingness)
#Multiple R-squared:  0.7207,    Adjusted R-squared:  0.7206 
#F-statistic:  6173 on 1 and 2392 DF,  p-value: < 2.2e-16

fitP.dozer<-lm(datmet$PAR.HOBO[datmet$PAR.DOZER>100]~datmet$PAR.DOZER[datmet$PAR.DOZER>100])
summary(fitP.dozer)

#Coefficients:
#                                          Estimate Std. Error t value Pr(>|t|)
#(Intercept)                              107.42734    9.22044   11.65   <2e-16
#datmet$PAR.DOZER[datmet$PAR.DOZER > 100]   0.82555    0.01051   78.56   <2e-16
#                                            
#(Intercept)                              ***
#datmet$PAR.DOZER[datmet$PAR.DOZER > 100] ***

#Residual standard error: 238.3 on 2392 degrees of freedom
 # (33821 observations deleted due to missingness)
#Multiple R-squared:  0.7207,    Adjusted R-squared:  0.7206 
#F-statistic:  6173 on 1 and 2392 DF,  p-value: < 2.2e-16


#fit for air temperature for LDF

fitT.LDF2<-lm(datmet$Temp.HOBO~datmet$Temp.LDF2)
summary(fitT.LDF2)
#Coefficients:
 #                  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)      -0.0795465  0.0130325   -6.104 1.06e-09 ***
#datmet$Temp.LDF2  1.0068623  0.0006517 1544.930  < 2e-16 ***

#Residual standard error: 1.518 on 17048 degrees of freedom
#  (20683 observations deleted due to missingness)
#Multiple R-squared:  0.9929,    Adjusted R-squared:  0.9929 
#F-statistic: 2.387e+06 on 1 and 17048 DF,  p-value: < 2.2e-16

#Note if RH from LDF is to be used need to convert to % from proportion
RHp.LDF<-100*datmet$RH.LDF2

fitRH.LDF2<-lm(datmet$RH.HOBO~RHp.LDF)
summary(fitRH.LDF2)

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 13.667800   0.207836   65.76   <2e-16 ***
#RHp.LDF      0.825624   0.002611  316.20   <2e-16 ***

#Residual standard error: 6.279 on 19691 degrees of freedom
#  (17993 observations deleted due to missingness)
#Multiple R-squared:  0.8355,    Adjusted R-squared:  0.8355 
#F-statistic: 9.998e+04 on 1 and 19691 DF,  p-value: < 2.2e-16


fitP.LBR<-lm(datmet$PAR.HOBO~datmet$PAR.LBR)
summary(fitP.LBR)



plot(seq(1,length(datmet$PAR.HOBO[datmet$Year==2015])),datmet$PAR.HOBO[datmet$Year==2015], type="l")

#check daily maximum and average of PAR to see if bird damage affected PAR

Pardaym<-aggregate(datmet$PAR.HOBO[datmet$Year==2015],by=list(doy[datmet$Year==2015]),FUN="max")
Pardaya<-aggregate(datmet$PAR.HOBO[datmet$Year==2015],by=list(doy[datmet$Year==2015]),FUN="mean")
LBRParm<-aggregate(datmet$PAR.LBR[datmet$Year==2015],by=list(doy[datmet$Year==2015]),FUN="max")
LBRPara<-aggregate(datmet$PAR.LBR[datmet$Year==2015],by=list(doy[datmet$Year==2015]),FUN="mean")
par(mfrow=c(2,1))
plot(Pardaym$Group.1,Pardaym$x,type="l",lwd=2, xlab="doy", ylab="PAR Max")
points(LBRParm$Group.1,LBRParm$x,type="l",lwd=2,col="red")
plot(Pardaya$Group.1,Pardaya$x,type="l",lwd=2, xlab="doy", ylab="PAR Ave")
points(LBRPara$Group.1,LBRPara$x,type="l",lwd=2,col="red")
legend(20,1500,c("HOBO Y4","LBR"),pch=19,col=c("black","red"),bty="n")
#plot days 125-175
plot(seq(1,length(datmet$PAR.HOBO[datmet$Year==2015&doy>=125&doy<=200])),
datmet$PAR.HOBO[datmet$Year==2015&doy>=125&doy<=200],type="l",lwd=2,ylab="PAR",xaxt="n",
xlab="DOY",ylim=c(0,1500))
axis(1, seq(1,76*48,by=48),seq(125,200))
points(seq(1,length(datmet$PAR.LBR[datmet$Year==2015&doy>=125&doy<=200])),
datmet$PAR.LBR[datmet$Year==2015&doy>=125&doy<=200],type="l",lwd=1,col="red",lty=2)

legend(20,1500,c("HOBO Y4","LBR"),lty=c(1,2),col=c("black","red"),bty="n")

#from half hourly data days 149-176 in 2015 have oddly shaped peaks, may be a result of a bird?
#gap fill days from LBR for these days

#Items to gapfill RH, Temp, and PAR in 2014, and Temp in 2014
#data for gaps from bulldozer site
cord.RH<-fitRH.dozer$coefficients[1]+(fitRH.dozer$coefficients[2]*datmet$RH.DOZER)
cord.T<-fitT.dozer$coefficients[1]+(fitT.dozer$coefficients[2]*datmet$Temp.DOZER)


cord.PAR<-ifelse(datmet$PAR.DOZER<3,0,
			ifelse(datmet$PAR.DOZER<100&datmet$PAR.DOZER>=3,fitP.dozer1$coefficients[1]+(fitP.dozer1$coefficients[2]*datmet$PAR.DOZER),
			fitP.dozer$coefficients[1]+(fitP.dozer$coefficients[2]*datmet$PAR.DOZER)))
#data for gaps from LDF2 site
cort.T<-fitT.LDF2$coefficients[1]+(fitT.LDF2$coefficients[2]*datmet$Temp.LDF2)
#data from LBR for PAR
corb.P<-fitP.LBR$coefficients[1]+(fitP.LBR$coefficients[2]*datmet$PAR.LBR)



gap.Temp<-ifelse(is.na(datmet$Temp.HOBO)&datmet$Year==2014,cord.T,
			ifelse(is.na(datmet$Temp.HOBO)&datmet$Year==2015,cort.T,datmet$Temp.HOBO))
			
gap.RHtemp<-ifelse(is.na(datmet$RH.HOBO)&datmet$Year==2014,cord.RH,datmet$RH.HOBO)	
#fill in gaps

#use zoo package for interpolation for missing data with no Bulldozer or LDF or LBR
#install.packages(c("zoo"))
library(zoo)
gap.RH<-na.spline(gap.RHtemp)

#fill in PAR gaps 

gap.PARtemp<-ifelse(is.na(datmet$PAR.HOBO)&datmet$Year==2014,cord.PAR,
			ifelse(is.na(datmet$PAR.HOBO)&datmet$Year==2015,corb.P,datmet$PAR.HOBO))
#fill in gaps in 2014 where bulldozer data is not availabe but LBR data is
gap.PAR<-ifelse(is.na(gap.PARtemp),corb.P,gap.PARtemp)

#set up dataframe for gapfilled data output

met.complete<-data.frame(Year=datmet$Year,DOY=doy,DayID=dayall,Time=datmet$Hour,
Temp=gap.Temp,PAR=gap.PAR,RH=gap.RH)

write.table(met.complete,"Met gap filled.csv",sep=",",row.names=FALSE)

