
###############Notes########################

#2013 goes from 7/16-8/5 in half hour inc from 16:30-12:00
#total should be 19 full days (912 obs) plus on 16th (15obs) and 5th (25obs)
#2013 obs=952

#2014 goes from 7/3 in half hour from 11:30	
#ends on 8/14 at 12:00
#deleted one lone 8/15 measurement with all NAN	
#total should be 41 (1968obs) full days plus 3 (25obs) 15th (25obs)
#2014 obs=2018

#2015 goes from 6/25 at 18:30 
#ends at 9/6 at 18:00 (two periods of all NAN excluded) 
#total should be 72 full days (3456 obs) 6th 37obs and 25th (11obs)
#2015 total = 3504

#total =6474

###set wd
setwd("c:\\Users\\hkropp\\Google Drive\\sap_flux\\R")

##########################################
###########Set up sensor info#############
##########################################

#set up vector of sensor numbers
sens<-seq(1,17)

#indicates sensor size in cm
size<-c(rep(1,6),rep(3,11))

#species s=salix, l=larix
sp<-c("s","s","l","l","l","s",rep("l",11))

#plant names
name<-c("shrub plant1","shrub plant2","Larix plant3 N","Larix plant4 N",
"Larix plant5 N","shrub plant 6 N", "Larix plant 7 S","Larix plant 7 N", "Larix plant 9 N",
 "Larix plant 10 S", "Larix plant 11 N", "Larix plant 12 N", "Larix plant 13 N",
 "Larix plant 14 S", "Larix plant 14 N", "Larix plant 10 N", "Larix plant 17 N")

#plant number
plant<-c(1,2,3,4,5,6,7,7,9,10,11,12,13,14,14,10,17)
#direction 7S,8N, 10 S, 16N
#all other sensors are on the N side
aspect<-c(rep("N",6),"S",rep("N",2),"S",rep("N",3),"S",rep("N",3))
#DBH or BD (for s) in cm

diam<-c(3.1,4.7,6.1,7.2,4.9,4.5,25.9,
		25.9,28.4,15.3,13.3,12.5,18.1,22.6,22.6,15.3,9)
		
#sapwood area (cm2)
sap<-c(1.27,1.62,6.79,9.12,3.69,1.58,27.2,27.2,28.5,19.78,
		17.79,16.92,22.14,25.29,25.29,19.78,12.28)
##define lengths
Ndays<-138
Nsensor<-17
Nobs<-6474

#probes switched Augst 25th 2015 voltage turned down from 3 to 1.8V 
#Trees 8,9,10,11,12,13,14,16 switched from 3cm to 1cm
#doy 237
#changed at obs 5902
###################################################
#########read in sensor info#######################
###################################################

#dT measurement in col 5-21
datS<-read.csv("sap_Y4_forR.csv")
names(datS)
#check observation number in each day
days<-aggregate(datS$Time, by=list(datS$DOY,datS$Year),FUN="length")
days$ID<-seq(1,138)
#set up a matrix of just dT
dTs<-datS[,5:21]

#create unique day label
dayid<-rep(0,length(datS$Time))
for(i in 1:length(datS$Time)){
	for(m in 1:138){
		if(datS$DOY[i]==days$Group.1[m]&datS$Year[i]==days$Group.2[m]){
		dayid[i]<-days$ID[m]}
	}
}


##################################################
####set up sapflow function on 12:00 days#########
####no correction for sensor length###############
##################################################

#calculate daily maximum DT
#omit NAS to not loose data on days with only an NA
#Note warnings will appear, but that is just putting in -INF for days with no data
maxDT<-matrix(rep(0,Ndays*Nsensor),ncol=Nsensor)

for(j in 1:Nsensor){
	for(i in 1:Ndays){
		maxDT[i,j]<-max(na.omit(dTs[dayid==i,j]))
	}
}
#turn days with no data (will be -INF) into NA
maxDT.t<-ifelse(maxDT==-Inf,NA,maxDT)

#add maxDT for each daily observation for ease of calculation

reps<-rep(days$x,times=Nsensor)
allmax<-as.vector(maxDT.t)
tempmax<-rep(allmax,times=reps)
DTmax<-matrix(tempmax,ncol=Nsensor,byrow=FALSE)	


#calculate K from observed dT and Max dT
Dcalc<-(DTmax-dTs)/dTs

#calculate velocity in cm/s
#note this comes from Clearwater et al 1999 paper and has been adjusted from units of mm -s to cm -s

V<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
	V[,j]<-ifelse(Dcalc[,j]>=0,0.0119*(Dcalc[,j]^1.231),NA)
}
#calculate F in g/hr
F<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
F[,j]<-V[,j]*sap[j]*3600
}


#########################################################################
#########################################################################
###Plots of all years without data flag
#doy labels
time.pl<-c(seq(16,928,by=48),seq(978,2946,by=48),seq(3030,6438,by=48))
time.lab<-c(seq(198,217),seq(185,226), seq(178,249))

#set up plotting labels
{
#doy labels
time.pl<-c(seq(16,928,by=48),seq(978,2946,by=48),seq(3030,6438,by=48))
time.lab<-c(seq(198,217),seq(185,226), seq(178,249))

#plot max dT
wi<-windows(15)
for(i in 1:17){
wi[i]<-windows(15)
plot(seq(1,Nobs),DTmax[,i],type="l",lwd=2,xaxt="n",ylab="dT max",ylim=c(0,20),
xlab=paste(i))
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953)
abline(v=2971)
}

#plot dT calc
wi<-windows(15)
for(i in 1:17){
wi[i]<-windows(15)
plot(seq(1,Nobs),Dcalc[,i],type="l",lwd=2,xaxt="n",ylab="dT calc",ylim=c(0,1.5),
xlab=paste(i))
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953)
abline(v=2971)
}
#plot V
wi<-windows(15)
for(i in 1:17){
wi[i]<-windows(15)
plot(seq(1,Nobs),dTs[,i],type="l",lwd=2,xaxt="n",ylab="dT uncorrected",ylim=c(0,10),
xlab=paste(i))
points(seq(1,Nobs),DTmax[,i],type="l",lwd=2,lty=2,col="grey50")
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953)
abline(v=2971)
}

#plot V
wi<-windows(15)
for(i in 1:17){
wi[i]<-windows(15)
plot(seq(1,Nobs),V[,i],type="l",lwd=2,xaxt="n",ylab="dT uncorrected",ylim=c(0,.1),
xlab=paste(i))
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953)
abline(v=2971)
}

#plot each 3cm sensor to check for any issues
wi<-windows(15)
for(i in 1:17){
wi[i]<-windows(15)
plot(seq(1,Nobs),F[,i],type="l",lwd=2,xaxt="n",ylab="Sapflow (g/hr)",ylim=c(0,450),
xlab=paste(i))
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953)
abline(v=2971)
}
}

######################################################
#######################################################
#for correction of length of sensors (for 3cm) not in sapwood
#need to establish relationship between
#sapwood thickness and DBH
#from other data taken at the site 

datSWT<-read.csv("SWT_forR.csv")
#check linear fit
par(mai=c(1,1,1,1))
plot(datSWT$DBH,datSWT$SWT,ylim=c(0,3),xlim=c(0,35), pch=19, xlab="Diameter Breast Height (DBH, cm)",
		ylab="Sapwood thickness (cm)", yaxt="n", cex.lab=1.5, box="n", xaxs="i", yaxs="i", axes=FALSE)
axis(1, seq(0,35, by=5), cex.axis=1.25)
axis(2, seq(0,3, by=.5), cex.axis=1.25, las=2)
fitS<-lm(datSWT$SWT~datSWT$DBH)
summary(fitS)
abline(fitS)
#check normality
qqnorm(fitS$residuals)

#results

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  1.594027   0.128662  12.389 1.17e-11 ***
#datSWT$DBH  -0.031002   0.007733  -4.009  0.00055 ***

#Residual standard error: 0.3144 on 23 degrees of freedom
#Multiple R-squared:  0.4114,    Adjusted R-squared:  0.3858 
#F-statistic: 16.07 on 1 and 23 DF,  p-value: 0.00055


#note this will not be accurate for shrubs!!!
depth.es<-(diam*-.031)+1.59

#set shrub sensor depths to zero for now since this sapwood depth correction
#does  not apply to them

#Lu et al state that no correction is needed if sapwood exceeds sensor
totdepth<-c(0,0,1,1,1,0,rep(3,11))


#according to Clearwater et al to account for portion of probe not in sapwood
#DT.SW=(DT-b*DTmax)/a where b =1-a and a is the proportion of probe
#with sapwood 

#calculate the porportion of the sensor not in sapwood
b<-ifelse(totdepth-depth.es>0,(totdepth-depth.es)/totdepth,0)
a<-1-b


#calculate daily maximum DT
#omit NAS to not loose data on days with only an NA
#Note warnings will appear, but that is just putting in -INF for days with no data
maxDT<-matrix(rep(0,Ndays*Nsensor),ncol=Nsensor)

for(j in 1:Nsensor){
	for(i in 1:Ndays){
		maxDT[i,j]<-max(na.omit(dTs[dayid==i,j]))
	}
}
#turn days with no data (will be -INF) into NA
maxDT.t<-ifelse(maxDT==-Inf,NA,maxDT)

#add maxDT for each daily observation for ease of calculation

reps<-rep(days$x,times=Nsensor)
allmax<-as.vector(maxDT.t)
tempmax<-rep(allmax,times=reps)
DTmax<-matrix(tempmax,ncol=Nsensor,byrow=FALSE)	

#calculate K from observed dT and Max dT
DTcalc.l<-matrix(rep(0,Nsensor*Nobs),ncol=Nsensor)
for(j in 1:Nsensor){
DTcalc.l[,j]<-(dTs[,j]-(b[j]*DTmax[,j]))/a[j]
}

DTall1<-cbind(dTs[1:5902,1:6],DTcalc.l[1:5902,7:17])
#no correction after 5902 for 8-16
DTall2<-cbind(dTs[5903:6474,1:6],DTcalc.l[5903:6474,7],dTs[5903:6474,8:16],DTcalc.l[5903:6474,17])
colnames(DTall2)<-seq(1,17)
colnames(DTall1)<-seq(1,17)
DTall3<-rbind(DTall1,DTall2)

#T diff calc
K<-(DTmax-DTall3)/DTall3

#calculate velocity
V.l<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
	V.l[,j]<-ifelse(K[,j]>=0,0.0119*(K[,j]^1.231),NA)
}
#calculate F in g/hr
F.l<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
F.l[,j]<-V.l[,j]*sap[j]*3600
}
#################################
###############
#set up data flag for spikes
Fl.cor<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
	Fl.cor[,j]<-ifelse(F.l[,j]>=400,NA,F.l[,j])

}

#######plotting

time.pl<-c(seq(16,928,by=48),seq(978,2946,by=48),seq(3030,6438,by=48))
time.lab<-c(seq(198,217),seq(185,226), seq(178,249))
wi<-windows(15)
for(i in 1:17){
	wi[i]<-windows(15)
	plot(seq(1,Nobs),dTs[,(i)],type="l",lwd=2,xaxt="n",ylab="dT",ylim=c(0,50),
	xlab=paste(i))
	points(seq(1,Nobs),DTall3[,(i)],type="l",lwd=2,col="red",lty=2)
	text(5000,420,paste(name[i]), cex=2)
	axis(1,time.pl,time.lab)
	#add year divisions
	abline(v=953, lty=2, lwd=2, col="grey60")
	abline(v=2971, lty=2, lwd=2, col="grey60")
}

sub.S<-c(7,10,14)
sub.N<-c(8,16,15)
for(i in 1:3){
	wi[i]<-windows(15)
	plot(seq(1,Nobs),Fl.cor[,sub.S[i]],type="l",lwd=2,xaxt="n",ylab="Sapflow (g/hr)",ylim=c(0,500),
	xlab=paste(i),col="red")
	points(seq(1,Nobs),Fl.cor[,sub.N[i]],type="l",lwd=2,col="grey50")
	points(seq(1,Nobs),Fl.cor[,sub.S[i]]-Fl.cor[,sub.N[i]],type="l",lwd=2,col="grey20")
	#text(5000,420,paste(name[i]), cex=2)
	axis(1,time.pl,time.lab)
	#add year divisions
	abline(v=953, lty=2, lwd=2, col="grey60")
	abline(v=2971, lty=2, lwd=2, col="grey60")
}
ave.d<-rep(0,3)
sub.S<-c(7,10,14)
sub.N<-c(8,16,15)
for(i in 1:3){
wi[i]<-windows(15)
plot(seq(1,Nobs),Fl.cor[,sub.S[i]]-Fl.cor[,sub.N[i]],type="l",lwd=2,xaxt="n",ylab="Sapflow (g/hr)",ylim=c(-100,300),
xlab=paste(i),col="grey20")
#text(5000,420,paste(name[i]), cex=2)
axis(1,time.pl,time.lab)
#add year divisions
abline(v=953, lty=2, lwd=2, col="grey60")
abline(v=2971, lty=2, lwd=2, col="grey60")
ave.d[i]<-mean(na.omit(Fl.cor[,sub.S[i]]-Fl.cor[,sub.N[i]]))
}
#check relationship between N and S on all measurements
fit.d7<-lm(Fl.cor[,7]~Fl.cor[,8])
summary(fit.d7)
fit.d10<-lm(Fl.cor[,10]~Fl.cor[,16])
summary(fit.d10)
fit.d14<-lm(Fl.cor[,14]~Fl.cor[,15])
summary(fit.d14)

fit.all<-lm(c(Fl.cor[,7],Fl.cor[,10],Fl.cor[,14])~c(Fl.cor[,8],Fl.cor[,16],Fl.cor[,15]))
summary(fit.all)
#see how daily means compare
flux.df7N<-na.omit(data.frame(F=Fl.cor[,8], DOY=datS$DOY,Year=datS$Year))
flux.df7S<-na.omit(data.frame(F=Fl.cor[,7], DOY=datS$DOY,Year=datS$Year))
flux.df10N<-na.omit(data.frame(F=Fl.cor[,16], DOY=datS$DOY,Year=datS$Year))
flux.df10S<-na.omit(data.frame(F=Fl.cor[,10], DOY=datS$DOY,Year=datS$Year))
flux.df14N<-na.omit(data.frame(F=Fl.cor[,15], DOY=datS$DOY,Year=datS$Year))
flux.df14S<-na.omit(data.frame(F=Fl.cor[,14], DOY=datS$DOY,Year=datS$Year))

flux.ave7N<-aggregate(flux.df7N$F, by=list(flux.df7N$DOY,flux.df7N$Year),FUN="mean")
flux.ave7S<-aggregate(flux.df7S$F, by=list(flux.df7S$DOY,flux.df7S$Year),FUN="mean")
flux.ave10N<-aggregate(flux.df10N$F, by=list(flux.df10N$DOY,flux.df10N$Year),FUN="mean")
flux.ave10S<-aggregate(flux.df10S$F, by=list(flux.df10S$DOY,flux.df10S$Year),FUN="mean")
flux.ave14N<-aggregate(flux.df14N$F, by=list(flux.df14N$DOY,flux.df14N$Year),FUN="mean")
flux.ave14S<-aggregate(flux.df14S$F, by=list(flux.df14S$DOY,flux.df14S$Year),FUN="mean")

plot(seq(1, length(flux.ave7N$x)),flux.ave7N$x,xlim=c(1,138),ylim=c(0,400),type="l",col="grey30",lwd=2,
xlab="dayxyear",ylab="Mean daily Sap flow g hr-1")
points(seq(1, length(flux.ave10N$x)),flux.ave10N$x,type="l",col="grey30",lwd=2)
points(seq(1, length(flux.ave14N$x)),flux.ave14N$x,type="l",col="grey30",lwd=2)
points(seq(1, length(flux.ave14S$x)),flux.ave14S$x,type="l",col="grey70",lwd=2)
points(seq(1, length(flux.ave10S$x)),flux.ave10S$x,type="l",col="grey70",lwd=2)
points(seq(1, length(flux.ave7S$x)),flux.ave7S$x,type="l",col="grey70",lwd=2)
legend(10,400,c("North","South"),col=c("grey30","grey70"),lwd=2,bty="n")

diff.7<-rep(0,138)
diff.10<-rep(0,138)
for(i in 1:138){
	if(flux.ave7N$Group.1[i]==flux.ave7S$Group.1[i]&flux.ave7N$Group.2[i]==flux.ave7S$Group.2[i]){
	diff.7[i]<-flux.ave7S$x[i]-flux.ave7N$x[i]

	}
		if(flux.ave10N$Group.1[i]==flux.ave10S$Group.1[i]&flux.ave10N$Group.2[i]==flux.ave10S$Group.2[i]){
	diff.10[i]<-flux.ave10S$x[i]-flux.ave10N$x[i]
	}

}
cor.7<-cor(flux.ave7S$x,flux.ave7N$x)
cor.10<-cor(flux.ave10S$x,flux.ave10N$x)
ave.diffall<-rep(0,130)
diff.14<-rep(0,130)
sub.14N<-rep(0,130)
for(i in 1:130){
	for(j in 1:137){
		if(flux.ave14N$Group.1[j]==flux.ave14S$Group.1[i]&flux.ave14N$Group.2[j]==flux.ave14S$Group.2[i]){
			diff.14[i]<-flux.ave14S$x[j]-flux.ave14N$x[i]
			ave.diffall[i]<-mean(diff.7[j],diff.10[j],diff.14[i])
			sub.14N[i]<-flux.ave14N$x[i]
			
		}
	}
}
cor14<-cor(sub.14N,flux.ave14S$x)

hist(diff.7)
hist(diff.10)
hist(diff.14)
hist(ave.diffall[1:123],breaks=10)
t.test(ave.diffall[1:123],mu=0)





#estimate leaf area from biomass using an allometric equation from Alexander et al 2012
leaf.bio<-function(DBH,a,b){a*(DBH^b)}

a.bio<-c(22.5,22.5,rep(40.5,3),22.5,rep(40.5,11))
b.bio<-c(1.45,1.45,rep(1.41,3),1.45,rep(1.41,11))
#leaf biomass is in g gry wt tree-1
lf.wgt<-leaf.bio(diam,a.bio,b.bio)

#SLM for Larix from Wang et al 2001 ia 143 cm^2/g
#cm^2 per tree
leaf.area<-lf.wgt*143


##############################################################

# so E.l should be g cm-2 hr
#to convert to g m-2 s 

#removed data flag for now, try adding data flag later
E.l.lcor<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
	E.l.lcor[,j]<-((F.l[,j]/leaf.area[j])/3600)*10000
}
#data without temperature corrections
E.l<-matrix(rep(0,Nobs*Nsensor),ncol=Nsensor)
for(j in 1:Nsensor){
	E.l[,j]<-((F[,j]/leaf.area[j])/3600)*10000
}

######################################################################
#####################################################################
###### more plotting
time.pl<-c(seq(16,928,by=48),seq(978,2946,by=48),seq(3030,6438,by=48))
time.lab<-c(seq(198,217),seq(185,226), seq(178,249))
wi<-windows(15)
for(i in 1:17){
	wi[i]<-windows(15)
	plot(seq(1,Nobs),(E.l[,(i)]/18)*1000,type="l",lwd=2,xaxt="n",ylab="Sapflow (mmol m-2 s-1)",ylim=c(0,2),
	xlab=paste(i))
	text(5000,420,paste(name[i]), cex=2)
	axis(1,time.pl,time.lab)
	points(seq(1,Nobs),(E.l.lcor[,(i)]/18)*1000,type="l",lwd=2,col="grey70")
	#add year divisions
	abline(v=953, lty=2, lwd=2, col="grey60")
	abline(v=2971, lty=2, lwd=2, col="grey60")
}

#write data frame for analysis later

datEt<-data.frame(E.l[,3:5],E.l[,7:17],datS$Year,datS$DOY,datS$Time)
colnames(datEt)<-c(seq(3,5),seq(7,17),"Year","DOY","Time")
#exclude day at the start and end of each year since there appear to be some
#irregularities in the data as it gets set up the first day
datE<-rbind(datEt[16:927,],datEt[978:2945,],datEt[2982:6437,])

datEt.lcor<-data.frame(E.l.lcor[,3:5],E.l.lcor[,7:17],datS$Year,datS$DOY,datS$Time)
colnames(datEt.lcor)<-c(seq(3,5),seq(7,17),"Year","DOY","Time")
#exclude day at the start and end of each year since there appear to be some
#irregularities in the data as it gets set up the first day
datE.lcor<-rbind(datEt.lcor[16:927,],datEt.lcor[978:2945,],datEt.lcor[2982:6437,])
#write.table(E.final,"El_formodel.csv",row.names=FALSE,sep=",")



#read in transpiration data from sapflow
#datE<-read.csv("El_formodel.csv")

#read in met data
datM<-read.csv("Met gap filled.csv")

#calculate D from RH and Temp

#calculate VPD
#saturated vapor pressure kpa
e.sat<-function(Temp){0.611*exp((17.502*Temp)/(Temp+240.97))}
#VPD fucntion from Relative humidity
VPD<-function(esat,RH.d){esat-((RH.d/100)*esat)}

satD<-e.sat(datM$Temp)
D<-VPD(satD,datM$RH)

#read in pressure data
datP<-read.csv("Pressure.csv")
names(datP)
#set up day and year for data
#127-365,1-365, 1-365
YearP<-c(rep(2015,365*24),rep(2014,365*24),rep(2013,239*24))
doyP<-c(rep(seq(365,1),each=24),rep(seq(365,1),each=24),rep(seq(365,127),each=24))
timehr<-rep(seq(24,1),times=(969))
#fill in Na from missing time points first
library(zoo)
Pfill<-na.approx(datP$P.kpa)
Phr<-rep(Pfill,each=3)

doyP.o<-doyP[23256:1]
YearP.o<-YearP[23256:1]
P.o<-Phr[23256:1]
doyP.all<-rep(doyP.o,each=2)
P.all<-rep(P.o,each=2)
YearP.all<-rep(YearP.o,each=2)

Phhr<-data.frame(P=P.all,DOY=doyP.all,Year=YearP.all)

#need to subset met to match up with sapflow observation time frame
#subset each year
#2013 198-216, 2014 185-225, 2015 177-248

#make 3 data frames for each year then bind 

year1<-datM[datM$Year==2013&datM$DOY>=198&datM$DOY<=216,]
year2<-datM[datM$Year==2014&datM$DOY>=185&datM$DOY<=225,]
year3<-datM[datM$Year==2015&datM$DOY>=177&datM$DOY<=248,]
Msub<-rbind(year1,year2,year3)


#make a Pressure dataframe to match
pyear1<-Phhr$P[Phhr$Year==2013&Phhr$DOY>=198&Phhr$DOY<=216]
pyear2<-Phhr$P[Phhr$Year==2014&Phhr$DOY>=185&Phhr$DOY<=225]
pyear3<-Phhr$P[Phhr$Year==2015&Phhr$DOY>=177&Phhr$DOY<=248]
Psub<-round(c(pyear1,pyear2,pyear3),1)

Temp.K<-Msub$Temp+273.15
satD.sub<-e.sat(Msub$Temp)
Dsub<-VPD(satD.sub,Msub$RH)


###calculate gs from transpiration
#Gs=Kg*El/D from Ewers and Oren 2000
#function for conductance coefficient (kPa m3 kg-1)
Kg.coeff<-function(T){115.8+(.423*T)}

#calculate Kg
Kg.d<-Kg.coeff(Msub$Temp)
#convert El to kg m-2 s-1 from g m-2 s-1
El.kg<-datE[,1:14]*(1/1000)

El.kg.lcor<-datE.lcor[,1:14]*(1/1000)

#calculate Gs and convert to cm/s from m/s

Gs.convert1<-function(Kg,El,D){((Kg*El)/D)*100}
Gs<-matrix(rep(0,14*6336),ncol=14)
Gs.lcor<-matrix(rep(0,14*6336),ncol=14)
for(i in 1:14){
	Gs[,i]<-Gs.convert1(Kg.d,El.kg[,i],Dsub)
	Gs.lcor[,i]<-Gs.convert1(Kg.d,El.kg.lcor[,i],Dsub)
}
#convert cm/s to mmol m-2 s using the equation from Pearcy et al
#here the term P/101.3 is consered to be equal to one
unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}

Gs.mmol<-matrix(rep(0,14*6336),ncol=14)
Gs.mmol.lcor<-matrix(rep(0,14*6336),ncol=14)
for(i in 1:14){
	Gs.mmol[,i]<-unit.conv(Gs[,i],Msub$Temp,Psub)*1000
	Gs.mmol.lcor[,i]<-unit.conv(Gs.lcor[,i],Msub$Temp,Psub)*1000
}


#plot results

Nobs2<-6336
plant<-c(3,4,5,7,7,9,10,11,12,13,14,14,10,17)
time.pl<-c(seq(1,866,by=48),seq(913,2833,by=48),seq(2881,6289,by=48))
time.lab<-c(seq(198,216),seq(185,225), seq(177,248))
wi<-windows(15)
for(i in 1:14){
	wi[i]<-windows(15)
	plot(seq(1,Nobs2),Gs.mmol[,(i)],type="l",lwd=2,xaxt="n",ylab="Gs (mmol m-2 s-1)",ylim=c(0,400),
	xlab=paste(plant[i]))
	points(seq(1,Nobs2),Gs.mmol.lcor[,(i)],type="l",lwd=2,col="gray70")
	axis(1,time.pl,time.lab)
	#add year divisions
	abline(v=953, lty=2, lwd=2, col="grey60")
	abline(v=2971, lty=2, lwd=2, col="grey60")
}

Nobs2<-6336
plant<-c(3,4,5,7,7,9,10,11,12,13,14,14,10,17)
time.pl<-c(seq(1,866,by=48),seq(913,2833,by=48),seq(2881,6289,by=48))
time.lab<-c(seq(198,216),seq(185,225), seq(177,248))
wi<-windows(15)
for(i in 1:14){
	wi[i]<-windows(15)
	plot(seq(1,Nobs2),Gs[,(i)],type="l",lwd=2,xaxt="n",ylab="Gs (cm s-1)",ylim=c(0,1),
	xlab=paste(plant[i]))
	points(seq(1,Nobs2),Gs.lcor[,(i)],type="l",lwd=2,col="gray70")
	axis(1,time.pl,time.lab)
	#add year divisions
	abline(v=953, lty=2, lwd=2, col="grey60")
	abline(v=2971, lty=2, lwd=2, col="grey60")
}

################################################################
################################################################
#make plots to just look at some daily timescales

wi<-windows(15)


xlabhr<-seq(200,230)
xlabt<-seq(1,48*31,by=48)
for(i in 1:14){
	wi[i]<-windows(15)
	plot(seq(1,length(Gs.mmol.lcor[datE$Year==2015&datE$DOY>=200&datE$DOY<=230,i])),
	Gs.mmol.lcor[datE$Year==2015&datE$DOY>=200&datE$DOY<=230,i],type="l",ylim=c(0,700),lwd=2,
	xaxt="n",ylab="Gs mmol m-2 s-1", xlab=paste(plant[i]))
	axis(1,xlabt,xlabhr )
}

#day 237 was the sensor change, try plots with 220-250

xlabhr<-seq(220,250)
xlabt<-seq(1,48*31,by=48)
for(i in 1:14){
	wi[i]<-windows(15)
	plot(seq(1,length(Gs.mmol.lcor[datE$Year==2015&datE$DOY>=220&datE$DOY<=250,i])),
	Gs.mmol.lcor[datE$Year==2015&datE$DOY>=220&datE$DOY<=250,i],type="l",ylim=c(0,1000),lwd=2,
	xaxt="n",ylab="Gs mmol m-2 s-1", xlab=paste(plant[i]))
	axis(1,xlabt,xlabhr )
	abline(v=17*48,col="grey50",lwd=2,lty=2)
}

xlabhr<-seq(220,250)
xlabt<-seq(1,48*31,by=48)
for(i in 1:14){
	wi[i]<-windows(15)
	plot(seq(1,Nobs),(E.l[,(i)]/18)*1000,type="l",lwd=2,xaxt="n",ylab="Sapflow (mmol m-2 s-1)",ylim=c(0,2),
	xlab=paste(i))
	plot(seq(1,length(E.l.lcor[datE$Year==2015&datE$DOY>=220&datE$DOY<=250,i])),
	E.l.lcor[datE$Year==2015&datE$DOY>=220&datE$DOY<=250,i],type="l",ylim=c(0,1),lwd=2,
	xaxt="n",ylab="T g m-2 s-1", xlab=paste(plant[i]))
	axis(1,xlabt,xlabhr )
	abline(v=17*48,col="grey50",lwd=2,lty=2)
}


###############################################################
################################################################
################################################################
#write data to file
#use corrected data, write to csv

gs.dat<-data.frame(Gs.mmol.lcor,Year=datE.lcor$Year,DOY=datE.lcor$DOY,Time=datE.lcor$Time )

T.dat<-data.frame((datE.lcor/18)*1000,Year=datE.lcor$Year,DOY=datE.lcor$DOY,Time=datE.lcor$Time)

write.table(gs.dat,"Gs_mmol_out_eqfix.csv",sep=",",row.names=FALSE)
write.table(T.dat,"T_mmol_out_eqfix.csv",sep=",",row.names=FALSE)