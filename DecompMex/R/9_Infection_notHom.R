# test of female homicide spike 2009

head(data)
# g7 is homicide

load('Data/Counts&Rates_1990-2015Mex.RData')

head(Data_rates)
dim(Data_Counts)
dim(data)
data <- data[with(data, order(state,year,sex,age)), ]
Data_Counts <- Data_Counts[with(Data_Counts, order(state,year,sex,age)), ]
Data_rates <- Data_rates[with(Data_rates, order(state,year,sex,age)), ]
Data_rates$Exp <- Data_rates$Pop

Data_rates[,standage := sum(Exp), by=list(year,sex,age)]

strate <- function(rate,standardage,age,from=0,to=14){
  keep <- age >= from & age <= to
  sum(rate[keep] * standardage[keep]) / sum(standardage[keep])
}
setnames(Data_rates,as.character(1:12),letters[1:12])
asdr <- Data_rates[ ,list(asdr = strate(a,standage,age,from=50,to=59)),by=list(state,year,sex)]
mat <- acast(asdr[sex == 1, ], state ~ year, value.var = "asdr")
matplot(1990:2015, t(mat), type = 'l', lty = 1)
abline(v=2009)
