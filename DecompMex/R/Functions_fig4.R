# rankplot skeleton function (minimal)
rankplot <- function(mat, 
                     col,
                     lwd, 
                     pch,
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
  labels            <- colnames(ys)
  plot(NULL, 
       type = "n", 
       xlim = range(xs), 
       ylim = c(0, Nrank + 1), 
       axes = FALSE,
       xlab = "",
       ylab = "",
       ...)
  text(min(xs),laby,labels,pos=2,xpd=TRUE)
  for (i in 1:Nrank){
    lines(xs, ys[,i ], col = col[i], lwd = lwd[i])
  }
  
  #points(rep(xs[1],Nrank), laby, col = col, pch=16,cex=1
  for (i in 1:Nrank){
    points(xs, ys[, i], col = col[i], pch=pch[i],cex=1.2)
  }
}
