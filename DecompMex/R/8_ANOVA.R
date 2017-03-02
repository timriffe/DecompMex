
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
ource("R/Functions_fig.R")

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


