
LTuniform <- function(mx,sex = "f"){
	#install.packages("/home/tim/git/HMDLifeTables/HMDLifeTables/HMDLifeTables",repos=NULL)
	require(HMDLifeTables)
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
	ax[1]   <- HMDLifeTables:::AKm02a0(m0 = mx[1], sex = sex)
	qx        <- mx / (1 + (1 - ax) * mx)
	qx[i.openage]       <- ifelse(is.na(qx[i.openage]), NA, 1)
	ax[i.openage]       <- 1 / mx[i.openage]                   
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




