#####################################################
# Data Prep by Jose Manuel Aburto, inherited Sep 7, #
#####################################################

# deaths data originally from:
# http://www3.inegi.org.mx/sistemas/microdatos/encuestas.aspx?c=33398&s=est

# let's see if we can make an automatic downloader / processing script.




########## Program to analyze sex mortality differences in Avoidable Mortality
#setwd("//Users//josemanuelaf//Desktop//Aburto 2015 gender disparities//Data")
#setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Aburto 2015 gender disparities\\Data')

#install.packages('reshape')
#install.packages('gdata')
#install.packages('latticeExtra')
#install.packages('ggplot2')
#(install.packages("cause.decomp_1.0.zip", repos = NULL))

library(ggplot2)
library(reshape)
library(gdata)
library(latticeExtra)
library(cause.decomp)

makeTransparent<-function(someColor, alpha=100)
{
	newColor<-col2rgb(someColor)
	apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
						blue=curcoldata[3],alpha=alpha, maxColorValue=255)})
}

#load("90_2013_AMData.RData")
#### 90_2013_AMData.RData file contains Information from official vital registers of 11,244,953 death ocurred from
#### 1990 to 2013 by sex, age, year of ocurrence, state and AM classification according to:
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

#### First, I use 90_2013_AMData.RData file to tabulate and reduce the DB in 5-year age groups.
#unique(AM_Data$AM)
#AM_Data$Age.group<-23
#AM_Data$Age.group[AM_Data$EDAD==0] <- 1 #=0
#AM_Data$Age.group[AM_Data$EDAD>=1 & AM_Data$EDAD<=4] <- 2 #=2-4
#AM_Data$Age.group[AM_Data$EDAD>=5 & AM_Data$EDAD<=9] <- 3 #=5-9 
#AM_Data$Age.group[AM_Data$EDAD>=10 & AM_Data$EDAD<=14] <- 4 #=10-14 
#AM_Data$Age.group[AM_Data$EDAD>=15 & AM_Data$EDAD<=19] <- 5 #=15-19 
#AM_Data$Age.group[AM_Data$EDAD>=20 & AM_Data$EDAD<=24] <- 6 #=20-24
#AM_Data$Age.group[AM_Data$EDAD>=25 & AM_Data$EDAD<=29] <- 7 #=25-29
#AM_Data$Age.group[AM_Data$EDAD>=30 & AM_Data$EDAD<=34] <- 8 #=30-34
#AM_Data$Age.group[AM_Data$EDAD>=35 & AM_Data$EDAD<=39] <- 9 #=35-39  
#AM_Data$Age.group[AM_Data$EDAD>=40 & AM_Data$EDAD<=44] <- 10 #=40-44  
#AM_Data$Age.group[AM_Data$EDAD>=45 & AM_Data$EDAD<=49] <- 11 #=45-49  
#AM_Data$Age.group[AM_Data$EDAD>=50 & AM_Data$EDAD<=54] <- 12 #=50-54
#AM_Data$Age.group[AM_Data$EDAD>=55 & AM_Data$EDAD<=59] <- 13 #=55-59
#AM_Data$Age.group[AM_Data$EDAD>=60 & AM_Data$EDAD<=64] <- 14 #=60-64
#AM_Data$Age.group[AM_Data$EDAD>=65 & AM_Data$EDAD<=69] <- 15 #=65-69  
#AM_Data$Age.group[AM_Data$EDAD>=70 & AM_Data$EDAD<=74] <- 16 #=70-74  
#AM_Data$Age.group[AM_Data$EDAD>=75 & AM_Data$EDAD<=79] <- 17 #=75-79  
#AM_Data$Age.group[AM_Data$EDAD>=80 & AM_Data$EDAD<=84] <- 18 #=80-84
#AM_Data$Age.group[AM_Data$EDAD>=85 & AM_Data$EDAD<=89] <- 19 #=85-89
#AM_Data$Age.group[AM_Data$EDAD>=90 & AM_Data$EDAD<=94] <- 20 #=90-94
#AM_Data$Age.group[AM_Data$EDAD>=95 & AM_Data$EDAD<=99] <- 21 #=95-99
#AM_Data$Age.group[AM_Data$EDAD>=100 & AM_Data$EDAD<998] <- 22 #=100 y más

#
#AM_Data$N<-1
#Deaths<-aggregate.data.frame(AM_Data$N, by=list(AM_Data$ANIO_OCUR,AM_Data$ENT_OCURR,AM_Data$SEXO,AM_Data$AM,
#                                                 AM_Data$Age.group),FUN=sum)
#
#Deaths<-rename.vars(Deaths,c("Group.1","Group.2","Group.3","Group.4","Group.5","x"),
#                 c("Year","State","Sex","AM","Age.Group","Casualties"),info=TRUE)
#head(Deaths)
##Years need to be corrected
#Deaths$Year[Deaths$Year==90] <- 1990
#Deaths$Year[Deaths$Year==91] <- 1991
#Deaths$Year[Deaths$Year==92] <- 1992
#Deaths$Year[Deaths$Year==93] <- 1993
#Deaths$Year[Deaths$Year==94] <- 1994
#Deaths$Year[Deaths$Year==95] <- 1995
#Deaths$Year[Deaths$Year==96] <- 1996
#Deaths$Year[Deaths$Year==97] <- 1997
#summary(Deaths$Year)
#
#Deaths<-Deaths[order(Deaths$AM,Deaths$Year,Deaths$Sex,Deaths$State,Deaths$Age.Group),]
#head(Deaths)
#keep(Deaths,sure=T)
#
#load("State_Mid_Pop.RData")
#Population<-State.Mid.Pop
#keep(Population,Deaths,sure=T)
#
#Population$Age.group<-23
#Population$Age.group[Population$Age==0] <- 1 #=0
#Population$Age.group[Population$Age>=1 & Population$Age<=4] <- 2 #=2-4
#Population$Age.group[Population$Age>=5 & Population$Age<=9] <- 3 #=5-9 
#Population$Age.group[Population$Age>=10 & Population$Age<=14] <- 4 #=10-14 
#Population$Age.group[Population$Age>=15 & Population$Age<=19] <- 5 #=15-19 
#Population$Age.group[Population$Age>=20 & Population$Age<=24] <- 6 #=20-24
#Population$Age.group[Population$Age>=25 & Population$Age<=29] <- 7 #=25-29
#Population$Age.group[Population$Age>=30 & Population$Age<=34] <- 8 #=30-34
#Population$Age.group[Population$Age>=35 & Population$Age<=39] <- 9 #=35-39  
#Population$Age.group[Population$Age>=40 & Population$Age<=44] <- 10 #=40-44  
#Population$Age.group[Population$Age>=45 & Population$Age<=49] <- 11 #=45-49  
#Population$Age.group[Population$Age>=50 & Population$Age<=54] <- 12 #=50-54
#Population$Age.group[Population$Age>=55 & Population$Age<=59] <- 13 #=55-59
#Population$Age.group[Population$Age>=60 & Population$Age<=64] <- 14 #=60-64
#Population$Age.group[Population$Age>=65 & Population$Age<=69] <- 15 #=65-69  
#Population$Age.group[Population$Age>=70 & Population$Age<=74] <- 16 #=70-74  
#Population$Age.group[Population$Age>=75 & Population$Age<=79] <- 17 #=75-79  
#Population$Age.group[Population$Age>=80 & Population$Age<=84] <- 18 #=80-84
#Population$Age.group[Population$Age>=85 & Population$Age<=89] <- 19 #=85-89
#Population$Age.group[Population$Age>=90 & Population$Age<=94] <- 20 #=90-94
#Population$Age.group[Population$Age>=95 & Population$Age<=99] <- 21 #=95-99
#Population$Age.group[Population$Age>=100 & Population$Age<998] <- 22 #=100 y más
#
#Population<-aggregate.data.frame(Population$Mid.Population, by=list(Population$Year,Population$Sex,Population$State,Population$Age.group
#                                                                    ),FUN=sum)
#Population<-rename.vars(Population,c("Group.1","Group.2","Group.3","Group.4","x"),
#                        c("Year","Sex","State","Age.Group","Mid.Pop"),info=TRUE)
#

#
##I just Population data until 2010
#Deaths<-subset(Deaths,Year<=2010)
#Deaths$Age.Group<-as.integer(Deaths$Age.Group)
#
#Deaths.Pop<-NULL
#for (i in 1990:2010){
#  sub1<-as.data.frame(subset(Deaths,Deaths$Year==i))
#  sub2<-as.data.frame(subset(Population,Population$Year==i))
#  sub3 <- merge(sub1,sub2, by=c("Year","State","Sex","Age.Group"))
#  Deaths.Pop <-rbind(Deaths.Pop,sub3)
#  
#}

#save(Deaths.Pop,file="AM_Data&Population1990-2010.RData")

#################Starts new section


load("AM_Data&Population1990-2010.RData")
#keep(Deaths.Pop,sure=T)
load("State_Mid_Pop.RData")

Deaths.Pop$Spcf.DRate<-Deaths.Pop$Casualties/Deaths.Pop$Mid.Pop

Population<-State.Mid.Pop
keep(Deaths.Pop,Population,sure=T)

Population$Age.group<-23
Population$Age.group[Population$Age==0] <- 1 #=0
Population$Age.group[Population$Age>=1 & Population$Age<=4] <- 2 #=2-4
Population$Age.group[Population$Age>=5 & Population$Age<=9] <- 3 #=5-9 
Population$Age.group[Population$Age>=10 & Population$Age<=14] <- 4 #=10-14 
Population$Age.group[Population$Age>=15 & Population$Age<=19] <- 5 #=15-19 
Population$Age.group[Population$Age>=20 & Population$Age<=24] <- 6 #=20-24
Population$Age.group[Population$Age>=25 & Population$Age<=29] <- 7 #=25-29
Population$Age.group[Population$Age>=30 & Population$Age<=34] <- 8 #=30-34
Population$Age.group[Population$Age>=35 & Population$Age<=39] <- 9 #=35-39  
Population$Age.group[Population$Age>=40 & Population$Age<=44] <- 10 #=40-44  
Population$Age.group[Population$Age>=45 & Population$Age<=49] <- 11 #=45-49  
Population$Age.group[Population$Age>=50 & Population$Age<=54] <- 12 #=50-54
Population$Age.group[Population$Age>=55 & Population$Age<=59] <- 13 #=55-59
Population$Age.group[Population$Age>=60 & Population$Age<=64] <- 14 #=60-64
Population$Age.group[Population$Age>=65 & Population$Age<=69] <- 15 #=65-69  
Population$Age.group[Population$Age>=70 & Population$Age<=74] <- 16 #=70-74  
Population$Age.group[Population$Age>=75 & Population$Age<=79] <- 17 #=75-79  
Population$Age.group[Population$Age>=80 & Population$Age<=84] <- 18 #=80-84
Population$Age.group[Population$Age>=85 & Population$Age<=89] <- 19 #=85-89
Population$Age.group[Population$Age>=90 & Population$Age<=94] <- 20 #=90-94
Population$Age.group[Population$Age>=95 & Population$Age<=99] <- 21 #=95-99
Population$Age.group[Population$Age>=100 & Population$Age<998] <- 22 #=100 y más

# I standarize with 2000 total population, it is the midpoint of the period
Pop.Agregate<-aggregate.data.frame(Population$Mid.Population, by=list(Population$Year,Population$Age.group),FUN=sum)
Pop.Agregate<-rename.vars(Pop.Agregate,c("Group.1","Group.2","x"),c("Year","Age.Group","Mid.Pop"),info=TRUE)
Std.Pop<-subset(Std.Pop,Year==2000)
Std.Pop$Struct<-Std.Pop$Mid.Pop/sum(Std.Pop$Mid.Pop)
Std.Pop<-subset(Std.Pop,select=c(Age.Group,Struct))
Deaths.Pop.Rates <- merge(Deaths.Pop,Std.Pop, by=c("Age.Group"))
Deaths.Pop.Rates$Struc.SRate<-Deaths.Pop.Rates$Spcf.DRate*Deaths.Pop.Rates$Struct

# I calculate Cause-Sex-Age-specific death rates
ASDR<-aggregate.data.frame(Deaths.Pop.Rates$Struc.SRate, by=list(Deaths.Pop.Rates$Year,
				Deaths.Pop.Rates$State,
				Deaths.Pop.Rates$Sex,
				Deaths.Pop.Rates$AM),FUN=sum)
ASDR<-rename.vars(ASDR,c("Group.1","Group.2","Group.3","Group.4","x"),
		c("Year","State","Sex","AM","CAS.DR"),info=TRUE)

save(ASDR,file="ASDR_1990-2010_AM.RData")


################## The analysis can start
ASDR <- local(get(load("ASDR_1990-2010_AM.RData")))
keep(ASDR,sure=T)

#ASDR contains the cause specific age standarized death rates by state, year, sex and
#Avoidable Mortality Classification
#Now, I calculate sex ratios
Males<-subset(ASDR,Sex==1)
Females<-subset(ASDR,Sex==2)
Sex.Ratios<-merge(Males,Females, by=c("Year","State","AM"))
Sex.Ratios<-subset(Sex.Ratios,select=c(Year,State,AM,CAS.DR.x,CAS.DR.y))
Sex.Ratios<-rename.vars(Sex.Ratios,c("Year","State","AM","CAS.DR.x","CAS.DR.y"),
		c("Year","State","AM","Male.R","Female.R"),info=TRUE)
Sex.Ratios$SRatio <- Sex.Ratios$Male.R/Sex.Ratios$Female.R

#Sex.Ratios$Sex<-factor(Sex.Ratios$Sex,levels=c(1,2),labels=c("Males", "Females"))
Sex.Ratios$AMClass<-Sex.Ratios$AM
Sex.Ratios$AMClass<-factor(Sex.Ratios$AMClass,levels=c(seq(1,16,1)),labels=c("Infectious & Respiratory", "Cancers",
				"Circulatory","Birth Conditions","Diabetes",
				"Other Medical AM","IHD","HIV","Suicide","Lung Cancer","Cirrhosis","Homicide","Road Traffic Accidents",
				"Other heart diseases","Ill-Defined","Non-AM"))


keep(Sex.Ratios,sure=T)

Sex.Ratios$region<-0
attach(Sex.Ratios)
Sex.Ratios$region[Sex.Ratios$State==1] <- 2
Sex.Ratios$region[Sex.Ratios$State==2] <- 3
Sex.Ratios$region[Sex.Ratios$State==3] <- 3
Sex.Ratios$region[Sex.Ratios$State==4] <- 1
Sex.Ratios$region[Sex.Ratios$State==5] <- 3
Sex.Ratios$region[Sex.Ratios$State==6] <- 2
Sex.Ratios$region[Sex.Ratios$State==7] <- 1
Sex.Ratios$region[Sex.Ratios$State==8] <- 3
Sex.Ratios$region[Sex.Ratios$State==9] <- 2
Sex.Ratios$region[Sex.Ratios$State==10] <- 3
Sex.Ratios$region[Sex.Ratios$State==11] <- 2
Sex.Ratios$region[Sex.Ratios$State==12] <- 1
Sex.Ratios$region[Sex.Ratios$State==13] <- 2
Sex.Ratios$region[Sex.Ratios$State==14] <- 2
Sex.Ratios$region[Sex.Ratios$State==15] <- 2
Sex.Ratios$region[Sex.Ratios$State==16] <- 2
Sex.Ratios$region[Sex.Ratios$State==17] <- 1
Sex.Ratios$region[Sex.Ratios$State==18] <- 2
Sex.Ratios$region[Sex.Ratios$State==19] <- 3
Sex.Ratios$region[Sex.Ratios$State==20] <- 1
Sex.Ratios$region[Sex.Ratios$State==21] <- 1
Sex.Ratios$region[Sex.Ratios$State==22] <- 2
Sex.Ratios$region[Sex.Ratios$State==23] <- 1
Sex.Ratios$region[Sex.Ratios$State==24] <- 3
Sex.Ratios$region[Sex.Ratios$State==25] <- 3
Sex.Ratios$region[Sex.Ratios$State==26] <- 3
Sex.Ratios$region[Sex.Ratios$State==27] <- 1
Sex.Ratios$region[Sex.Ratios$State==28] <- 3
Sex.Ratios$region[Sex.Ratios$State==29] <- 2
Sex.Ratios$region[Sex.Ratios$State==30] <- 1
Sex.Ratios$region[Sex.Ratios$State==31] <- 1
Sex.Ratios$region[Sex.Ratios$State==32] <- 3
detach(Sex.Ratios)
Sex.Ratios$region<-factor(Sex.Ratios$region,levels=c(1,2,3),labels=c("South", "Central", "North"))
save(Sex.Ratios, file="Sex_Ratio_1990-2010.RData")
