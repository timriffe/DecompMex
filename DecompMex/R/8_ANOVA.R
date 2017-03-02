
setwd("C:/Users/jmaburto/Documents/GitHub/DecompMex/DecompMex")

if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
  # if I'm on the laptop
  setwd("/home/tim/git/DecompMex/DecompMex")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DecompMex/DecompMex"))
}

library(latticeExtra)
library(data.table)

load("Data/Temp_e0_results.RData")
source("R/Functions_fig.R")


### standardize temporary life expectancy with the maximum survival
temp.data <- temp.data[state < 33]
temp.data$sd.e <- 0
temp.data[age.g==1]$sd.e <- temp.data[age.g==1]$temp_e0/15
temp.data[age.g==2]$sd.e <- temp.data[age.g==2]$temp_e0/35
temp.data[age.g==3]$sd.e <- temp.data[age.g==3]$temp_e0/35
range(temp.data$sd.e)

### try two way anova
mod0 <- aov(sd.e ~ factor(age.g) + factor(state) + factor(year), data=temp.data)
anova(mod0)

### with interaction (makes sense)

interaction.plot(factor(temp.data$age.g), factor(temp.data$state),temp.data$sd.e)
### lines parallels, so not very important the interaction in this case

options(contrasts = c("contr.sum", "contr.poly"))
mod2.1 <- aov(temp_e0 ~ as.factor(age.g)*as.factor(state) + year, data=temp.data)
anova(mod2.1)
summary(lm(temp_e0 ~ factor(age.g)*factor(state),  data=temp.data))
hist(mod2.1$residuals,main="Histogram of residuals",xlab="Residuals")


# analyz the post hoc test
tk.test <- TukeyHSD(mod2.1)
tk.1 <- tk.test$`as.factor(age.g):as.factor(state)`
tk.1.names <- rownames(tk.1)
tk.1 <- as.data.table(tk.1)
setnames(tk.1, "p adj", "p")
tk.1$combination <- tk.1.names
head(tk.1)
rownames(tk.1)
dim(tk.1[p < .5])

3415/4560

# single out just needed e0 year ranges
dat <- temp.data
dat <- dat[dat$year >= 2010 & dat$state <= 32, ]
# get avg for 6 years starting 2010
dat <- dat[, list(mean_temp_e0 = mean(temp_e0)), by=list(sex,state,age.g)]

# get regions
state.code.recvec <- 
		c("Aguascalientes","Baja California","Baja California Sur","Campeche",
				"Coahuila","Colima","Chiapas","Chihuahua","Mexico City","Durango",
				"Guanajuato","Guerrero","Hidalgo","Jalisco","México State","Michoacán",
				"Morelos","Nayarit","Nuevo León","Oaxaca","Puebla","Querétaro",
				"Quintana Roo","San Luis Potosí","Sinaloa","Sonora","Tabasco","Tamaulipas",
				"Tlaxcala","Veracruz","Yucatán","Zacatecas")
names(state.code.recvec) <- 1:32


# state and region codes
region.recvec            <- c(2,3,3,1,3,2,1,3,2,3,2,1,2,2,2,
		2,1,2,3,1,1,2,1,3,3,3,1,3,2,1,1,3)
names(region.recvec)     <- 1:32

head(dat)
dat$region               <- region.recvec[as.character(dat$state)]

table(dat$region) / 3 / 2

library(reshape2)
blues   <- colorRampPalette(brewer.pal(9,"Blues")[-c(1:3)], space = "Lab")(10)
purples <- colorRampPalette(brewer.pal(9,"Purples")[-c(1:3)], space = "Lab")(11)
greens  <- colorRampPalette(brewer.pal(9,"Greens")[-c(1:3)], space = "Lab")(11)

# fatter lines for light colors (maybe start at 3?
lwdblues   <- seq(from=4,to=1,length=10)
lwdpurples <- seq(from=4,to=1,length=11)
lwdgreens  <- seq(from=4,to=1,length=11)

# want darker lines for higher ranks? palettes go from light to dark, so order
# in ascending
young <- dat[dat$age.g == 1, ]
young <- young[order(young$region, young$sex, young$mean_temp_e0), ]


# separate sexes
males   <- dat[dat$sex == 1, ]
females <- dat[dat$sex == 2, ]
# order based on left column, which will have state labels. This refers
# to young age group, ergo, we need within
youngmales   <- males[males$age.g == 1, ]
youngmales   <- youngmales[order(youngmales$region, youngmales$mean_temp_e0), ]
youngfemales <- females[females$age.g == 1, ]
youngfemales <- youngfemales[order(youngfemales$region, youngfemales$mean_temp_e0), ]

order.vec.females        <- youngfemales$state
order.vec.males        <- youngmales$state
names(order.vec.males)   <- 1:32
names(order.vec.females) <- 1:32

# get into matrices
m.mat   <- acast(males, state ~ age.g, value.var = "mean_temp_e0")
f.mat   <- acast(females, state ~ age.g, value.var = "mean_temp_e0")

m.mat <- m.mat[order.vec.males, ]
f.mat <- f.mat[order.vec.females, ]

col <- c(blues, purples, greens)
lwd <- c(lwdblues, lwdpurples, lwdgreens)

# TODO: get ordering right for col and lwd. These should be ordered as in the original matrix,
# and get reordered inside the function as the matrix itself is reordered. 

rankplot <- function(mat, 
		col,
		lwd, 
		...){
	
	rank <- mat * 0
	for (i in 1:nrow(rank)){
		rank[i, ] <- rank(mat[i, ])
	}
	ys                <- rank - 1
	Nrank             <- ncol(ys)
	# ys               <- abs(Nrank - rankmat)
	laby              <- ys[1,]
	#ys[mat < 1e-5] <- NA
	xs                <- as.integer(rownames(ys))
	causes            <- colnames(ys)
	plot(NULL, 
			type = "n", 
			xlim = range(xs), 
			ylim = c(0, Nrank + 1), 
			axes = FALSE,
			xlab = "",
			ylab = "")
	text(min(xs),laby,causes,pos=2,xpd=TRUE)
	for (i in 1:Nrank){
		lines(xs, ys[,i ], col = col[i], lwd = lwd[i])
	}
	points(rep(xs[1],Nrank), laby, col = col, pch=16,cex=1)
	
	for (i in 1:Nrank){
		points(xs[-1], ys[, i][-1], col = col[i], pch=16,cex=1)
	}
}

library(RcolorBrewer)
display.brewer.all()

rankplot(t(m.mat), col = col, lwd = lwd)



