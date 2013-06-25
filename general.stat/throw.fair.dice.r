# file: throw.fair.dice.r
# Copyright (c) T.S.Yo 2008
# A R-script to illustration "the more samples, the less skewness" with throwing dice example.
#
# Generate the random 
# Throw a dice 50000 times
u500 <- floor(runif(500,1,7))
# Throw a dice 1000 times
u200 <- floor(runif(200,1,7))
# Throw a dice 100 times
u100 <- floor(runif(100,1,7))
# Throw a dice 10000 times
u50 <- floor(runif(50,1,7))
# Create table
dice.tab <- matrix(ncol=6,nrow=4,
        dimnames=list(c("500","200","100","50"),c("1","2","3","4","5","6")))
dice.p.tab <- dice.tab
dice.tab[1,] <- table(u500)
dice.tab[2,] <- table(u200)
dice.tab[3,] <- table(u100)
dice.tab[4,] <- table(u50)
dice.p.tab[1,] <- table(u500)/500
dice.p.tab[2,] <- table(u200)/200
dice.p.tab[3,] <- table(u100)/100
dice.p.tab[4,] <- table(u50)/50
# Visualization
dice.tab
par(mfrow=c(2, 2))
plot(dice.p.tab[1,],type="n",xlim=c(0.5,6.5),ylim=c(0,0.3),ylab="Probability")
rect(c(0.5,1.5,2.5,3.5,4.5,5.5),0,c(1.4,2.4,3.4,4.4,5.4,6.4),dice.p.tab[1,])
title("Throw a fair dice for 500 times")
plot(dice.p.tab[2,],type="n",xlim=c(0.5,6.5),ylim=c(0,0.3),ylab="Probability")
rect(c(0.5,1.5,2.5,3.5,4.5,5.5),0,c(1.4,2.4,3.4,4.4,5.4,6.4),dice.p.tab[2,])
title("Throw a fair dice for 200 times")
plot(dice.p.tab[3,],type="n",xlim=c(0.5,6.5),ylim=c(0,0.3),ylab="Probability")
rect(c(0.5,1.5,2.5,3.5,4.5,5.5),0,c(1.4,2.4,3.4,4.4,5.4,6.4),dice.p.tab[3,])
title("Throw a fair dice for 100 times")
plot(dice.p.tab[4,],type="n",xlim=c(0.5,6.5),ylim=c(0,0.3),ylab="Probability")
rect(c(0.5,1.5,2.5,3.5,4.5,5.5),0,c(1.4,2.4,3.4,4.4,5.4,6.4),dice.p.tab[4,])
title("Throw a fair dice for 50 times")
