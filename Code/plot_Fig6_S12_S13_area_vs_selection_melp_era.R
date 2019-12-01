## Figure 6

table <- read.table("Fig6_melp_era_CLR_th.txt",h=T)

layout(matrix(c(1:3), nrow=1))
par(mai=c(0.5,0.5,0.5,0.5), oma=c(0,0,0,0))

plot(table$E_wntA/max(table$E_wntA),table$M_wntA/max(table$M_wntA), pch=19, ylab="",xlab="", xlim=c(0,1),ylim=c(0,1), cex=2)
max1 <- table$E_wntA/max(table$E_wntA)
max2 <- table$M_wntA/max(table$M_wntA)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))

plot(table$E_cor/max(table$E_cor),table$M_cor/max(table$M_cor), pch=19, ylab="",xlab="", xlim=c(0,1),ylim=c(0,1), cex=2)
max1 <- table$E_cor/max(table$E_cor)
max2 <- table$M_cor/max(table$M_cor)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))

plot(table$E_opt/max(table$E_opt),table$M_opt/max(table$M_opt), pch=19, ylab="",xlab="", xlim=c(0,1),ylim=c(0,1), cex=2)
max1 <- table$E_opt/max(table$E_opt)
max2 <- table$M_opt/max(table$M_opt)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))

# table <- read.table("melp_era_minalpha.txt",h=T)
# 
# plot(1/(table$E_wntA*1000000),1/table$M_wntA, pch=19, ylab="",xlab="", xlim=c(0,0.4),ylim=c(0,0.4), cex=2)
# max1 <- 1/(table$E_wntA*1000000)
# max2 <- 1/(table$M_wntA)
# abline(lm(max2 ~max1))
# summary(lm(max2 ~max1))
# 
# plot(1/(table$E_cor*1000000),1/table$M_cor, pch=19, ylab="",xlab="", xlim=c(0,0.4),ylim=c(0,0.4), cex=2)
# max1 <- 1/(table$E_cor*1000000)
# max2 <- 1/(table$M_cor)
# abline(lm(max2 ~max1))
# summary(lm(max2 ~max1))
# 
# plot(1/(table$E_opt*1000000),1/table$M_opt, pch=19, ylab="",xlab="", xlim=c(0,0.4),ylim=c(0,0.4), cex=2)
# max1 <- 1/(table$E_opt*1000000)
# max2 <- 1/(table$M_opt)
# abline(lm(max2 ~max1))
# summary(lm(max2 ~max1))


table <- read.table("Fig6_melp_era_avgalpha_th.txt",h=T)

plot(1/(table$E_wntA*1000000),1/table$M_wntA, pch=19, ylab="",xlab="", xlim=c(0,0.1),ylim=c(0,0.1), cex=2)
max1 <- 1/(table$E_wntA*1000000)
max2 <- 1/(table$M_wntA)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))

plot(1/(table$E_cor*1000000),1/table$M_cor, pch=19, ylab="",xlab="", xlim=c(0,0.1),ylim=c(0,0.1), cex=2)
max1 <- 1/(table$E_cor*1000000)
max2 <- 1/(table$M_cor)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))

plot(1/(table$E_opt*1000000),1/table$M_opt, pch=19, ylab="",xlab="", xlim=c(0,0.1),ylim=c(0,0.1), cex=2)
max1 <- 1/(table$E_opt*1000000)
max2 <- 1/(table$M_opt)
abline(lm(max2 ~max1))
summary(lm(max2 ~max1))


## Fig S12
area <- read.table('FigS12_melp_era_CLR_th_area.txt', h=T)

layout(matrix(c(1:6), nrow=2, byrow = T))
par(mai=c(0.5,0.5,0.4,0.2), oma=c(1,0.5,1.5,0.5))

plot(area$areaM, area$M_wntA/max(area$M_wntA), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. melpomene - WntA")))
max1 <- area$M_wntA/max(area$M_wntA)
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaM, area$M_cor/max(area$M_cor), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. melpomene - cortex")))
max1 <- area$M_cor/max(area$M_cor)
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaM, area$M_opt/max(area$M_opt), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. melpomene - optix")))
max1 <- area$M_opt/max(area$M_opt)
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))


plot(area$areaE, area$E_wntA/max(area$E_wntA), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. erato - WntA")))
max1 <- area$E_wntA/max(area$E_wntA)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaE, area$E_cor/max(area$E_cor), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. erato - cortex")))
max1 <- area$E_cor/max(area$E_cor)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaE, area$E_opt/max(area$E_opt), pch=19, xlim=c(0,2854088),ylim=c(0,1), cex=2, ylab = "%CLR > th", xlab = "Area (km2)", main = substitute(italic("H. erato - optix")))
max1 <- area$E_opt/max(area$E_opt)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.9, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

mtext("%CLR > threshold", 3, outer = T)
####

## Fig S13
area <- read.table('FigS13_melp_era_minalpha_area.txt', h=T)

layout(matrix(c(1:6), nrow=2, byrow = T))
par(mai=c(0.5,0.5,0.4,0.2), oma=c(1,0.5,1.5,0.5))

plot(area$areaM, 1/area$M_wntA, pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. melpomene - WntA")))
max1 <- 1/area$M_wntA
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaM, 1/area$M_cor, pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. melpomene - cortex")))
max1 <- 1/area$M_cor
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaM, 1/area$M_opt, pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. melpomene - optix")))
max1 <- 1/area$M_opt
abline(lm(max1 ~area$areaM))
test <- summary(lm(max1 ~area$areaM))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))


plot(area$areaE, 1/(area$E_wntA*1000000), pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. erato - WntA")))
max1 <- 1/(area$E_wntA*1000000)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaE, 1/(area$E_cor*1000000), pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. erato - cortex")))
max1 <- 1/(area$E_cor*1000000)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

plot(area$areaE, 1/(area$E_opt*1000000), pch=19, xlim=c(0,2854088),ylim=c(0,0.4), cex=2, ylab = "max(1/alpha)", xlab = "Area (km2)", main = substitute(italic("H. erato - optix")))
max1 <- 1/(area$E_opt*1000000)
abline(lm(max1 ~area$areaE))
test <- summary(lm(max1 ~area$areaE))
text(1700000,0.38, paste("R2 = ", round(test$adj.r.squared,2), "; p = ", round(test$coefficients[8],2), sep=''))

mtext("max(1/alpha)", 3, outer = T)
####














