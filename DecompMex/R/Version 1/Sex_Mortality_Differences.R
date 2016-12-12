getwd()

#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories



# Perform Analyses --------------------------------------------------------

# 1) read in data that was pre-processed
Deaths.Pop <- local(get(load("Data/AM_Data&Population1990-2010.RData")))

Deaths.Pop$Interval <- ifelse(Deaths.Pop$Age.Group == 1, 1, ifelse(
				Deaths.Pop$Age.Group == 2,4,5))
Deaths.Pop <- Deaths.Pop[with(Deaths.Pop,order(Year,Sex,Age.Group,AM, State)),]
library(data.table)
Deaths.Pop <- data.table(Deaths.Pop)
BPmxc      <- Deaths.Pop[,min(Spcf.DRate), by = list(Year, Sex, Age.Group, AM)]
BPmx       <- BPmxc[,sum(V1), by = list(Year, Sex, Age.Group)]

BPmx$Interval <- ifelse(BPmx$Age.Group == 1, 1, ifelse(
				                 BPmx$Age.Group == 2,4,5))
#Interval      <- BPmx$Interval[BPmx$Sex == 1 & BPmx$Year == 2000]
#mx     <-  BPmx$V1[BPmx$Sex == 1 & BPmx$Year == 2000]
mxabridged2ex <- function(mx, Interval){
	mx <- unlist(mapply(rep, x = mx, times = Interval ))
	sum(exp(-cumsum(mx)))
}

BPe0 <- BPmx[,mxabridged2ex(V1,Interval), by = list(Year,Sex)]

nrow(BPmx)

BPmxc      <- Deaths.Pop[,min(Spcf.DRate), by = list(Year, Sex, Age.Group, AM)]
BPmx       <- BPmxc[,sum(V1), by = list(Year, Sex, Age.Group)]

BPmx$Interval <- ifelse(BPmx$Age.Group == 1, 1, ifelse(
				BPmx$Age.Group == 2,4,5))

interval2age <- function(Int){
	cumsum(Int) - Int
}
BPmx[,Age := interval2age(Interval), by = list(Year,Sex)]

BPmx$AgeGroup <- ifelse(BPmx$Age < 15, "young", ifelse(BPmx$Age < 55, "middle", "old"))
cbind(BPmx$Age,BPmx$AgeGroup )
BPetemp <- BPmx[,mxabridged2ex(V1,Interval), by = list(Year,Sex, AgeGroup)]

#Interval      <- BPmx$Interval[BPmx$Sex == 1 & BPmx$Year == 2000]
#mx     <-  BPmx$V1[BPmx$Sex == 1 & BPmx$Year == 2000]
mxabridged2ex <- function(mx, Interval){
	mx <- unlist(mapply(rep, x = mx, times = Interval ))
	lx <- c(1, exp(-cumsum(mx)),0)
	Lx <- (lx[-1] + lx[-length(lx)]) / 2
	sum(Lx)
}


library(reshape2)
e0    <- acast(BPe0, Year~Sex, value.var = "V1")
etemp <- acast(BPetemp, Year~Sex+AgeGroup, value.var = "V1")

par(mfrow=c(2,2))
matplot(1990:2010, etemp[,c("1_young","2_young")], type = 'l')
matplot(1990:2010, etemp[,c("1_middle","2_middle")], type = 'l')
matplot(1990:2010, etemp[,c("1_old","2_old")], type = 'l')
matplot(1990:2010, e0, type = 'l')

Sex.Ratios <- local(get(load("Data/Sex_Ratio_1990-2010.RData")))

str(Sex.Ratios)

Deaths.Pop$AMClass <-Deaths.Pop$AM
Deaths.Pop$AMClass <-factor(Deaths.Pop$AMClass,levels=c(seq(1,16,1)),labels=c("Infectious & Respiratory", "Cancers",
                                                                             "Circulatory","Birth Conditions","Diabetes",
                                                                             "Other Medical AM","IHD","HIV","Suicide","Lung Cancer","Cirrhosis","Homicide","Road Traffic Accidents",
                                                                             "Other heart diseases","Ill-Defined","Non-AM"))


my.settings <- list(  
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

F1<-xyplot(log(SRatio) ~ Year|AMClass,data=Sex.Ratios,groups=State,type="l"
                          ,col=makeTransparent("black",alpha=150))
F1



qplot(Casualties, data = Deaths.Pop, geom = "histogram",fill=AMClass)
qplot(SRatio, data = Sex.Ratios, geom = "density",colour=AMClass)

Fig1<-qplot(Year, log(SRatio), data = Sex.Ratios, facets= .~AMClass,alpha = I(1/10))+ geom_smooth()
pdf(file="Figure 1.pdf",width=9,height=9,pointsize=4)
print(Fig1)
dev.off()

Fig2<-qplot(Year, SRatio, data = subset(Sex.Ratios,Sex.Ratios$AM<=7),alpha = I(1/1.5),ylim=c(.5,2.5),colour=AMClass)
Fig2
pdf(file="Figure 2.pdf",width=9,height=9,pointsize=4)
print(Fig2)
dev.off()

Fig3<-qplot(Year, SRatio, data =  subset(Sex.Ratios,Sex.Ratios$AM>7 & Sex.Ratios$AM <14),alpha = I(1/1.5),ylim=c(0,20),colour=AMClass)
Fig3
pdf(file="Figure 3.pdf",width=9,height=9,pointsize=4)
print(Fig3)
dev.off()

p <- ggplot(subset(Sex.Ratios,AM==11), aes(Year, SRatio, group = State)) +
  geom_line()
p

depth_dist <- ggplot(Deaths.Pop, aes(Year)) + xlim(1990, 2010)

depth_dist + geom_histogram(aes(fill = AMClass), binwidth = 0.1,
                            position = "fill")

depth_dist + geom_freqpoly(aes(y = ..density.., colour = cut),
                           binwidth = 0.1)

#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

