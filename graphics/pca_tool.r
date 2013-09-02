#
#
plotVar <- function(pca){
  sdev <- pca$sdev
  vars <- sdev^2
  vars_ratio <- vars/sum(vars)
  vars_accum <- vars_ratio
  for(i in 1:length(vars_accum)){
    vars_accum[i] = sum(vars_ratio[1:i])
  }
  plot(vars_accum,type="l",ylim=c(0,1))
  lines(vars_ratio,lty=2,col=2)
}


