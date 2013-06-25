load(".RData")
#no rd
# start=COVM
print("norm2d.nord.COVM.cv10")
norm2d.covm.cv10 <- cv10nca(norm2d.x,norm2d.y,start="COVM")
dimnames(norm2d.covm.cv10)[[2]][3] <- "NCA-COV"
print("")
# start=PCOVM
print("norm2d.nord.PCOVM.cv10")
norm2d.pcovm.cv10 <- cv10nca(norm2d.x,norm2d.y,start="PCOVM")
dimnames(norm2d.pcovm.cv10)[[2]][3] <- "NCA-PCOV"
print("")
norm2d.out1 <- cbind(norm2d.covm.cv10,norm2d.pcovm.cv10)
rm(norm2d.covm.cv10,norm2d.pcovm.cv10)
#rd=1
# start=COVM
print("norm2d.rd1.COVM.cv10")
norm2d.covm.cv10.rd1 <- cv10nca(norm2d.x,norm2d.y,rd=1,start="COVM")
dimnames(norm2d.covm.cv10.rd1)[[2]][3] <- "NCA-COV-rd1"
print("")
# start=PCOVM
print("norm2d.rd1.PCOVM.cv10")
norm2d.pcovm.cv10.rd1 <- cv10nca(norm2d.x,norm2d.y,rd=1,start="PCOVM")
dimnames(norm2d.pcovm.cv10.rd1)[[2]][3] <- "NCA-PCOV-rd1"
print("")
norm2d.out2 <- cbind(norm2d.covm.cv10.rd1,norm2d.pcovm.cv10.rd1)
rm(norm2d.covm.cv10.rd1,norm2d.pcovm.cv10.rd1)
#
#ring.out <- cbind(norm2d.out1[,-3],norm2d.out2[,c(2,4)])
#rm(norm2d.out1,norm2d.out2)
quit(save="yes")
