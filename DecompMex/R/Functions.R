#Decomposition function
mydecomp <- function (func, rates1, rates2, N, ...) {
  y1 <- func(rates1, ...)
  y2 <- func(rates2, ...)
  d <- rates2 - rates1
  n <- length(rates1)
  delta <- d/N
  x <- rates1 + d * matrix(rep(0.5:(N - 0.5)/N, length(rates1)), 
                           byrow = TRUE, ncol = N)
  cc <- matrix(0, nrow = n, ncol = N)
  for (j in 1:N) {
    for (i in 1:n) {
      z <- rep(0, n)
      z[i] <- delta[i]/2
      cc[i, j] <- func((x[, j] + z), ...) - func((x[, j] - 
                                                    z), ...)
    }
  }
  return(rowSums(cc))
}


# smoothing functions
  
sm.mat   <- function(DX, EX){
  ages  		<- 0:109
  years 		<- 1990:2015
  W     		<- EX
  W[W > 0] 	<- 1
  W[DX < 1 & W == 1] 	<- .3
  fit   <- Mort2Dsmooth(
    x = ages, 
    y = years, 
    Z = DX,
    offset = log(EX), 
    W = W,
    control=list(MAX.IT=1000))
  mxs <- exp(fit$logmortality)
  mxs <- melt(mxs, varnames=c("age","year"), value.name = "mxs")
  mxs
}

sm.chunk <- function(.SD){
  DX      <- acast(.SD, age~year, value.var = colnames(Counts)[i], fill = 0)
  EX      <- acast(.SD, age~year, value.var = "Pop", fill = 0)
  
  mxs     <- sm.mat(DX,EX)
  
  .SD$mxs <- mxs$mxs
  
  # still need to normalize exposure?
  .SD
}



#Some lifetable functions from LTuniform
AKm02a0 <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

LTuniform <- function(mx,sex = "f"){
  mx <- as.matrix(mx)
  #install.packages("/home/tim/git/HMDLifeTables/HMDLifeTables/HMDLifeTables",repos=NULL)
  #require(HMDLifeTables)
  i.openage <- nrow(mx)
  ax        <- mx * 0 + .5                                          # ax = .5, pg 38 MPv5
  
  ax[1, ]   <- AKm02a0(m0 = mx[1, ], sex = sex)
  
  #  if (testa0){
  #    ax[1, ]   <- AKm02a0_direct(m0 = mx[1, ], sex = sex)
  #  }
  # multiplying 2 matrices using '*' does the hadamard product in R (elementwise).
  qx        <- mx / (1 + (1 - ax) * mx)                             # Eq 60 MPv5
  # ---------------------------------------------------------------------------------
  # set open age qx to 1
  qx[i.openage, ]       <- ifelse(is.na(qx[i.openage, ]), NA, 1)
  ax[i.openage, ]       <- 1 / mx[i.openage, ]                   
  # ---------------------------------------------------------------------------------
  # define remaining lifetable columns:
  px 				      <- 1 - qx 																				# Eq 64 MPv5
  px[is.nan(px)]  <- 0 # skips BEL NAs, as these are distinct from NaNs
  # lx needs to be done columnwise over px, argument 2 refers to the margin.
  lx 			        <- apply(px, 2, function(px., RADIX, OPENAGE){ 		# Eq 65 MPv5
    if (all(is.na(px.))) {
      px.
    } else {
      c(RADIX, RADIX * cumprod(px.[1:OPENAGE]))
    }
  }, RADIX = 1, OPENAGE = i.openage - 1
  )
  rownames(lx)    <- 0:(i.openage - 1) # these got thrown off because l0 imputed.
  # NA should only be possible if there was a death with no Exp below age 80- impossible, but just to be sure
  # lx[is.na(lx)]   <- 0 # removed for BEL testing        
  dx 				      <- lx * qx 																				# Eq 66 MPv5
  Lx 				      <- lx - (1 - ax) * dx 														# Eq 67 MPv5
  
  Lx[i.openage, ]	<- lx[i.openage, ] * ax[i.openage, ]
  # we need to do operations on Lx, but taking its NAs to mean 0
  # Lx[is.na(Lx)] 	<- 0 # removed for BEL testing
  # Tx needs to be done columnwise over Lx, argument 2 refers to the column margin.
  Tx 				      <- apply(Lx, 2, function(Lx., i.openage, OPENAGE){
    c(rev(cumsum(rev(Lx.[1:OPENAGE]))),0) + Lx.[i.openage]	# Eq 68 MPv5
  }, OPENAGE = i.openage - 1, i.openage = i.openage
  )
  rownames(Tx)    <- rownames(lx)
  ex 				      <- Tx / lx 	                                      # Eq 69 MPv5
  list(e0=ex[1,],ex=ex,lx=lx,mx=mx)
}

LTuniformvecminimal <- compiler::cmpfun(function(mx,sex = "f"){
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]
  if (ax[i.openage]==Inf){ax[i.openage] <- .5}
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  Tx 				    <- c(rev(cumsum(rev(Lx[1:OPENAGE]))),0) + Lx[i.openage]
  ex 				    <- Tx / lx
  ex[1]
})

mcx2etemp <- compiler::cmpfun(function(mxc,sex,lowera,uppera){
  dim(mxc) <- c(length(mxc)/10,10)
  mx       <- rowSums(mxc, na.rm = TRUE)
  i.openage <- length(mx)
  OPENAGE   <- i.openage - 1
  RADIX     <- 1
  ax        <- mx * 0 + .5
  ax[1]     <- AKm02a0(m0 = mx[1], sex = sex)
  qx        <- mx / (1 + (1 - ax) * mx)
  qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
  ax[i.openage]       <- 1 / mx[i.openage]                   
  px 				    <- 1 - qx
  px[is.nan(px)]      <- 0
  lx 			        <- c(RADIX, RADIX * cumprod(px[1:OPENAGE]))
  dx 				    <- lx * qx
  Lx 				    <- lx - (1 - ax) * dx
  Lx[i.openage ]	    <- lx[i.openage ] * ax[i.openage ]
  
  Age <- 1:length(mx) - 1
  sum(Lx[Age >= lowera & Age <= uppera] ) / lx[Age == lowera] 
})

lx2Lx <- function(lx){
  lxt <- c(lx,0)
  c(lxt[-1] + lxt[-length(lxt)])/2
}

# temp life expectancy
getTempe0 <- function(.SD,lowera = 0, uppera = 14){ 
  # stopifnot(all(c(lowera,uppera) %in% LT$Age)) 
  sum(.SD$Lx[.SD$age >= lowera & .SD$age <= uppera] ) / .SD$lx[.SD$age == lowera] 
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
