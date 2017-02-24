
setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}



# Load data and group causes according to paper ---------------------------

#sex=1 <- males
#### 1. Infectious and respiratory diseases, 2. Cancers, 3. Circulatory, 4. Birth, 5. Diabetes, 6. Other Medical Care AM
#### 7. IHD, 8. HIV, 9. Suicide, 10. Lung Cancer, 11. Cirrhosis, 12. Homicide, 13. Road traffic accidents, 
#### 14. other heart diseases, 15. Ill-defined causes, 16. All other Non-AM
#### Note: these data do not contain Not Specified categories

load('Data/Counts&Rates_1990-2015Mex.RData')



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

#group accordingly
Counts <- Data_Counts[,1:4]
Counts <- cbind(Counts, g1=rowSums(Data_Counts[, c(6,7,8,9,11)]))
Counts <- cbind(Counts, g2=Data_Counts[,10])
Counts <- cbind(Counts, g3=Data_Counts[,12])
Counts <- cbind(Counts, g4=Data_Counts[,13])
Counts <- cbind(Counts, g5=Data_Counts[,15])
Counts <- cbind(Counts, g6=Data_Counts[,16])
Counts <- cbind(Counts, g7=Data_Counts[,17])
Counts <- cbind(Counts, g8=Data_Counts[,18])
Counts <- cbind(Counts, g9=Data_Counts[,14])
Counts <- cbind(Counts, g10=rowSums(Data_Counts[,19:21]))
Counts <- cbind(Counts, g11=rowSums(Counts[,5:14]))
Counts$Pop <- Data_Counts$Pop

Rates <- Data_rates[,c(1:4,21)]
gdata::keep(Counts,Rates, sure = T)
library(data.table)
Counts <- data.table(Counts)
Rates <- data.table(Rates)

library(xtable)
library(reshape2)
library(latticeExtra)



#### Table 1
Dx <- melt(Counts, id.vars = 1:4, variable.name = "cause", value.name = "Dx" )
Dx.75 <- Dx[age<76]

Dxc2 <- tapply(Dx.75$Dx,list(Dx.75$year, Dx.75$cause), sum)[,1:10]

am.names <- c("Causes amenable to medical service","Diabetes","Ischemic heart diseases",
              "HIV/AIDS","Lung cancer","Cirrhosis","Homicide","Road traffic accidents",
              "Suicide","Other causes")

tab1<- round(Dxc2/1000,1)
per <-   round(t(100 * Dxc2/ rowSums(Dxc)),1)
t1  <- paste(per[,1],"%",sep=" ")
t2  <- paste(per[,2],"%",sep=" ")
ta  <- cbind(Category=am.names,Males =t1,MalesPercent=tab1[1,],Females=t2,FemalesPercent=tab1[2,])

print(xtable(prettyNum(ta,big.mark=","),
             caption = "Avoidable Mortality classification, 
             with crude percentages below age 75.",
             label="tab:causes"), include.rownames=F,caption.placement = "top")

######### Sensitivity analysis of classification


c.names <- c(am.names,"Total deaths","Exposures")

Dxc <- tapply(Dx$Dx,list(Dx$year, Dx$cause), sum)

colnames(Dxc) <- c.names
Dxc.df    <- melt(Dxc)

Sens.fig <- xyplot(value~Var1|Var2,group=Var2, data=Dxc.df, type="l",
                   ylab="Death counts", xlab="Year",
                   scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2015,5))),
                               y=list(relation="free")),                                       
                   panel = function(x, y,...){           
                     panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)
                     panel.abline(v=1998,col='red',lty=1)
                     panel.grid(h=-1,v=0,col='dark grey',lty=3)           
                     panel.xyplot(x, y,lty=1,...)  
                   })

pdf(file="Sensitivity_fig.pdf",width=10,height=7,pointsize=12)
print(Sens.fig)
dev.off()



#### Gini coefficient over states, a measure of inequality
############### Gini analysis


giniSimple <- function(x){
  N          <- length(x)
  S          <- sum(x)
  xlorenz    <- cumsum(sort(x)) / S
  xequal     <- cumsum(rep(S / N, N)) / S
  2 * sum(xequal - xlorenz)
}

my.settings <- list(
  strip.background=list(col="grey"),
  strip.border=list(col="black")
)

load("Data/Temp_e0_results.RData")

Greens           <- brewer.pal(5,"Greens")[2:5]

temp.data <- temp.data[state < 33]

tot.tempe0.st <- temp.data[,sum(temp_e0), by = list(year,sex,state)]
tot.gini <- tot.tempe0.st[,giniSimple(V1), by = list(year,sex)]
tot.gini$age.g <- 4
setcolorder(tot.gini,c("year","sex","age.g","V1"))

gini.states <- temp.data[,giniSimple(temp_e0), by = list(year,sex,age.g)]

gini.states <- rbind(gini.states,tot.gini)

gini.states$sex <- factor(gini.states$sex,levels=c(1,2),labels=c("Males", "Females"))

Gini.fig <-xyplot(V1~year|sex,data=gini.states,groups=age.g,type="l",lwd=2,between=list(x=1),
                  xlim=c(1990,2015),ylim=c(0,.6),ylab="Gini coefficient",
                  layout=c(2,1),strip.left=F,strip=T,
                  col=Greens,par.settings=my.settings,xlab="Year", 
                  key=list(x=.2,y=.9,background="transparent",
                           text=list(c('Young (0-14)','Young adults(15-39)','Older adults(40-74)','Total (0-74)'),
                                     col="black"),cex=1,
                           lines=list(lty=c(1,1,1,2),lwd=2,col=Greens)),
                  scales=list(alternating=1,x=list(cex=.75,at=c(seq(1990,2015,5))),
                              y=list(cex=.75,at=seq(0,.5,.1),alternating=1)),                                       
                  panel = function(x, y,...){           
                    panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)
                    panel.abline(h=c(seq(0,.5,.1)),col='dark grey',lty=3)
                    panel.xyplot(x, y,lty=c(1,1,1,2),...)  
                  })
Gini.fig             

pdf(file="Gini_fig.pdf",width=10,height=6,pointsize=12)
print(Gini.fig)
dev.off()


####################3
cv <- function(x){ y <- sd(x)/mean(x)
y}

tot.tempe0.st <- temp.data[,sum(temp_e0), by = list(year,sex,state)]
tot.cv <- tot.tempe0.st[,cv(V1), by = list(year,sex)]
tot.cv$age.g <- 4
setcolorder(tot.cv,c("year","sex","age.g","V1"))



cv.states <- temp.data[,cv(temp_e0), by = list(year,sex,age.g)]

cv.states <- rbind(cv.states,tot.cv)

cv.states$sex <- factor(cv.states$sex,levels=c(1,2),labels=c("Males", "Females"))



cv.fig <-xyplot(V1~year|sex,data=cv.states,groups=age.g,type="l",lwd=2,between=list(x=1),
                  xlim=c(1990,2015),main="Survival inequality",
                #ylim=c(0,.6),
                ylab="Coefficient of variation",
                  layout=c(2,1),strip.left=F,strip=T,
                  col=Greens,par.settings=my.settings,xlab="Year", 
                  key=list(x=.2,y=.9,background="transparent",
                           text=list(c('Young (0-14)','Young adults(15-39)','Older adults(40-74)','Total (0-74)'),
                                     col="black"),cex=1,
                           lines=list(lty=c(1,1,1,2),lwd=2,col=Greens)),
                  scales=list(x=list(cex=.75,at=c(seq(1990,2015,5))),
                              y=list(cex=.75,at=seq(0,.025,.005),alternating=1)),                                       
                  panel = function(x, y,...){           
                    panel.abline(v=c(seq(1990,2015,5)),col='dark grey',lty=3)
                    panel.abline(h=c(seq(0,.025,.005)),col='dark grey',lty=3)
                    panel.xyplot(x, y,lty=c(1,1,1,2),...)  
                  })
cv.fig         

pdf(file="CVfig.pdf",width=11,height=7,pointsize=12)
print(cv.fig)
dev.off()
