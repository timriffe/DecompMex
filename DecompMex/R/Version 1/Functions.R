
# utility functions for working inside data.table, which is strange:
#myLT <- function(Mx, Sex){
#  LifeTable::LT(Mx = Mx, ages = 0:(length(Mx) - 1),
#     axmethod = "midpoint", sex = ifelse(unique(Sex) == 1, "male","female"), mxsmooth = FALSE,
#     axsmooth = FALSE, radix = 1, verbose = FALSE)$ex[1]
#}
#myLTlx <- function(Mx, Sex){
#  LifeTable::LT(Mx = Mx, ages = 0:(length(Mx) - 1),
#     axmethod = "midpoint", sex = ifelse(unique(Sex) == 1, "male","female"), mxsmooth = FALSE,
#     axsmooth = FALSE, radix = 1, verbose = FALSE)$lx
#}
# myLTLx <- function(Mx, Sex){
#   LifeTable::LT(Mx = Mx, ages = 0:(length(Mx) - 1),
#      axmethod = "midpoint", sex = ifelse(unique(Sex) == 1, "male","female"), mxsmooth = FALSE,
#      axsmooth = FALSE, radix = 1, verbose = FALSE)$Lx
# }
lx2Lx <- function(lx){
  lxt <- c(lx,0)
  c(lxt[-1] + lxt[-length(lxt)])/2
}

# temp life expectancy
getTempe0 <- function(.SD,lowera = 0, uppera = 14){ 
  # stopifnot(all(c(lowera,uppera) %in% LT$Age)) 
  sum(.SD$Lx[.SD$Age >= lowera & .SD$Age <= uppera] ) / .SD$lx[.SD$Age == lowera] 
} 

#mcx2etemp <- function(mxc,Sex,lowera,uppera){
#  dim(mxc) <- c(length(mxc)/10,10)
#  mx       <- rowSums(mxc, na.rm = TRUE)
#  ThisLT <- LifeTable::LT(Mx = mx, ages = 0:(length(mx) - 1),
#                axmethod = "midpoint", sex = ifelse(unique(Sex) == 1, "male","female"), mxsmooth = FALSE,
#                axsmooth = FALSE, radix = 1, verbose = FALSE)
#  lx  <- ThisLT$lx
#  Lx  <- ThisLT$Lx
#  Age <- ThisLT$ages
#  sum(Lx[Age >= lowera & Age <= uppera] ) / lx[Age == lowera] 
#}

myLT <- function(mx,sex){
	sex <- ifelse(all(sex == 1),"m","f")
	LTuniformvecminimal(mx,sex)
}
myLTlx <- function(mx,sex){
	sex <- ifelse(all(sex == 1),"m","f")
	LTuniform(mx,sex)$lx
}
