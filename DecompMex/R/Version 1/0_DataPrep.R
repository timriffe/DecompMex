setwd('C:\\Users\\aburtoflores\\Desktop\\Working papers\\Aburto & Riffe 2015')

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DecompMex/DecompMex")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}
#install.packages('reshape')
#install.packages('gdata')
#install.packages('latticeExtra')
#install.packages('ggplot2')
#(install.packages("cause.decomp_1.0.zip", repos = NULL))
#install.packages("devtools",dependencies=TRUE)
#install.packages('data.table')
#install_github("LifeTable", subdir = "LifeTable", username = "timriffe")

library(ggplot2)
library(reshape)
library(gdata)
library(latticeExtra)
#library(cause.decomp)
library(data.table)
library(devtools)
library(LifeTable)
# READ IN:

Deaths.Data   <- local(get(load('Data/Deaths_1990-2013.RData')))
Deaths        <- subset(Deaths.Data,(Deaths.Data$Year < 2011 & Deaths.Data$Year > 1989) & Deaths.Data$State < 33)
list.files("Data")
Pop           <- local(get(load('Data/Mex_Exposures_19902010.Rdata')))
keep(Deaths, Pop, sure = TRUE)

#######################################
# IMPORTANT: impute zeros for missing ages, causes, states, years
# otherwise BP is too high...






#######################################
# recoding Pop vars
# data from the crazy code for reading the Excel file
Pop$Sex1                 <- Pop$Sex
Pop$Sex[Pop$Sex=="f"]    <- 2
Pop$Sex[Pop$Sex=="m"]    <- 1
Pop$Sex                  <- as.integer(Pop$Sex)

# start working on deaths...
Deaths$Dx   <- 1

# this line takes forever on Tim's machine (10 min) better replace with data.table statement
Deaths.Data <- aggregate.data.frame(
		                    Deaths$Dx, 
		                    by = list(Deaths$Year, Deaths$Sex, Deaths$State, Deaths$Age), 
							FUN = sum)
Deaths.Data <- rename.vars(Deaths.Data, c("Group.1","Group.2","Group.3","Group.4","x"),
                           c("Year","Sex","State","Age","Dx"), info = TRUE)

##########################################
# merge pop deaths
#Take care of NA.s of Age and Sex (998 & 9) how are we going to treat these fellows?, is it okay jus with
#truncating the age at 75, I prefer this than distributing them uniformly.
Deaths.Data <- subset(Deaths.Data, Deaths.Data$Age <= 75)
Deaths.Data <- subset(Deaths.Data, Deaths.Data$Sex < 9)
Pop.Data    <- subset(Pop, Pop$Age <= 75)
Pop.Deaths  <- merge(Pop.Data, Deaths.Data, by = c("Year","Sex","State","Age"), all.x = TRUE)
Pop.Deaths[is.na(Pop.Deaths)] <- 0

# SORT POP DEATHS
Pop.Deaths    <- as.data.frame(Pop.Deaths)
Pop.Deaths    <- Pop.Deaths[with(Pop.Deaths, order(Year, Sex, State, Age)),]
Pop.Deaths    <- as.data.table(Pop.Deaths)
Pop.Deaths$Mx <- Pop.Deaths$Dx/Pop.Deaths$Exposure

# now recode deaths
##### Construct BP life expectancy taking into account Avoidable Mortality
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

#Groups used in the article:
# 1. Amenable to meical service: 1+2+3+4+6
# 2. Diabetes: 5
# 3. IHD:7
# 4. HIV:8
# 5. Lung cancer: 10
# 6. Cirrhosis: 11
# 7. Homicide: 12
# 8. Road traffic: 13 
# 9. Suicide: 9
# 10. All other causes: 14+15+16
Deaths.AM                              <- Deaths
Deaths.AM$AM.Group                     <- 10
Deaths.AM$AM.Group[Deaths.AM$AM <= 4]  <- 1
Deaths.AM$AM.Group[Deaths.AM$AM == 6]  <- 1
Deaths.AM$AM.Group[Deaths.AM$AM == 5]  <- 2
Deaths.AM$AM.Group[Deaths.AM$AM == 7]  <- 3
Deaths.AM$AM.Group[Deaths.AM$AM == 8]  <- 4
Deaths.AM$AM.Group[Deaths.AM$AM == 10] <- 5
Deaths.AM$AM.Group[Deaths.AM$AM == 11] <- 6
Deaths.AM$AM.Group[Deaths.AM$AM == 12] <- 7
Deaths.AM$AM.Group[Deaths.AM$AM == 13] <- 8
Deaths.AM$AM.Group[Deaths.AM$AM == 9]  <- 9

###########################################
# subset deaths 
Deaths.AM  <- aggregate.data.frame(Deaths.AM$Dx, by=list(Deaths.AM$Year,Deaths.AM$Sex,Deaths.AM$State,
                                                        Deaths.AM$AM.Group,Deaths.AM$Age),FUN=sum)
Deaths.AM  <- rename.vars(Deaths.AM,c("Group.1","Group.2","Group.3","Group.4","Group.5","x"),
                         c("Year","Sex","State","AM.Group","Age","Dx"),info=TRUE)

Deaths.AM  <-  subset(Deaths.AM,Deaths.AM$Age<76)
Deaths.AM  <-  subset(Deaths.AM,Deaths.AM$Sex<9)
# subset pop to like ages
Pop.1      <- subset(Pop,Pop$Age<76)

# now merge, in loop for memory management
Deaths.Pop <- NULL
for (i in 1:32){
  D   <- subset(Deaths.AM,Deaths.AM$State==i)
  P   <- subset(Pop.1, Pop.1$State==i)
  M   <-merge(D,P,by=c("Year","Sex","Age"),all.x=T)
  Deaths.Pop<-rbind(Deaths.Pop,M)
}

# take only needed columns, then rename
Deaths.Pop <- subset(Deaths.Pop,select=c("Year","Sex","Age","State.x","AM.Group","Dx","Exposure","StateNom"))
Deaths.Pop <- rename.vars(Deaths.Pop,c("State.x"),c("State"),info=TRUE)

Mxs   <- Pop.Deaths
Mxsc  <- Deaths.Pop

Mxsc    <- as.data.frame(Mxsc)
Mxsc    <- Mxsc[with(Mxsc,order(Year,Sex,State,Age,AM.Group)),]
Mxsc    <- as.data.table(Mxsc)

Mxs    <- as.data.frame(Mxs)
Mxs    <- Mxs[with(Mxs,order(Year,Sex,State,Age)),]
Mxs    <- as.data.table(Mxs)

save(Mxs, file = "Data/Mxs.Rdata")
save(Mxsc, file = "Data/Mxsc.Rdata")

############################################
# end data prep

