load(".RData")
#no rd
# start=COVM
print("ring2.nord.COVM.cv10")
ring2.covm.cv10 <- cv10nca(ring2.x,ring2.y,start="COVM")
dimnames(ring2.covm.cv10)[[2]][3] <- "NCA-COV"
print("")
# start=PCOVM
print("ring2.nord.PCOVM.cv10")
ring2.pcovm.cv10 <- cv10nca(ring2.x,ring2.y,start="PCOVM")
dimnames(ring2.pcovm.cv10)[[2]][3] <- "NCA-PCOV"
print("")
ring2.out1 <- cbind(ring2.covm.cv10,ring2.pcovm.cv10)
rm(ring2.covm.cv10,ring2.pcovm.cv10)
#rd=1
# start=COVM
print("ring2.rd1.COVM.cv10")
ring2.covm.cv10.rd2 <- cv10nca(ring2.x,ring2.y,rd=2,start="COVM")
dimnames(ring2.covm.cv10.rd2)[[2]][3] <- "NCA-COV-rd2"
print("")
# start=PCOVM
print("ring2.rd1.PCOVM.cv10")
ring2.pcovm.cv10.rd2 <- cv10nca(ring2.x,ring2.y,rd=2,start="PCOVM")
dimnames(ring2.pcovm.cv10.rd2)[[2]][3] <- "NCA-PCOV-rd2"
print("")
ring2.out2 <- cbind(ring2.covm.cv10.rd2,ring2.pcovm.cv10.rd2)
rm(ring2.covm.cv10.rd2,ring2.pcovm.cv10.rd2)
#
#ring.out <- cbind(ring2.out1[,-3],ring2.out2[,c(2,4)])
#rm(ring2.out1,ring2.out2)
quit(save="yes")
