layout(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2, byrow=T), heights=c(1,1,1))
layout.show(n=8)
par(mai=c(0.5,0.5,0.5,0), oma=c(2,2,2,2))



################
library(readxl)
table <- read.csv("SLiM_Simulations_UPDATES/simulation_summaries_UPDATE.csv", h=T)

head(table)

table$position_bp <- gsub('\\(', '', table$position_bp)
table$position_bp <- gsub(')', '', table$position_bp)

library(tidyr)
table <- separate(table, position_bp, into=c("start","end"), sep=", ")

head(table)

table$start <- as.numeric(table$start)
table$end <- as.numeric(table$end)
table$position <- (table$start + table$end)/2

pi   <- table[ , grepl( "pi" , names( table ) ) ]
tajD <- table[ , grepl( "tajD" , names( table ) ) ]
SF   <- table[ , grepl( "LR" , names( table ) ) ]

piM <- rowMeans(pi, na.rm = T)
TajDM <- rowMeans(tajD, na.rm = T)
SFM <- rowMeans(SF, na.rm = T)

# piM <- pi$pi1
# TajDM <- tajD$tajD1
# SFM <- SF$LR1

tablePi <- cbind(table$position, as.character(table$population), table$Generations_since_sweep_4Ne, piM)
tablePi <- as.data.frame(tablePi)

tableTajD <- cbind(table$position, as.character(table$population), table$Generations_since_sweep_4Ne, TajDM)
tableTajD <- as.data.frame(tableTajD)

tableSF <- cbind(table$position, as.character(table$population), table$Generations_since_sweep_4Ne, SFM)
tableSF <- as.data.frame(tableSF)

colnames(tablePi) <- c('position', 'population', 'generations', 'piM')
colnames(tableTajD) <- c('position', 'population', 'generations', 'TajDM')
colnames(tableSF) <- c('position', 'population', 'generations', 'SFM')

tablePi$position <- as.numeric(as.character(tablePi$position))
tablePi$piM <- as.numeric(as.character(tablePi$piM))

tableTajD$position <- as.numeric(as.character(tableTajD$position))
tableTajD$TajDM <- as.numeric(as.character(tableTajD$TajDM))


tableSF$population <- as.character(tableSF$population)
tableSF$generations <- as.numeric(as.character(tableSF$generations))
tableSF$position <- as.numeric(as.character(tableSF$position))
tableSF$SFM <- as.numeric(as.character(tableSF$SFM))

gens <- c(0.01,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,1.00)
colf <- colorRampPalette(c("red", "yellow"))
cols <- colf(length(gens))

library(viridis)
cols <- viridis(length(gens))

library(wesanderson)
cols <- wes_palette("Zissou1", 10, type = "continuous")

cols <- cols[10:1]
tableSF <- na.omit(tableSF)

plot(0, pch = "", xlim = c(0-374999,750000-374999), ylim = c(0,100), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n",axes = FALSE,
     main = "Classic sweep")
segments(-374999,0,750000-374999,0, "gray")
for(e in length(gens):1){
  tableS <- subset(tableSF, tableSF$population == 'p1' & tableSF$generations == gens[e])
  # tableS <- na.omit(tableS)
  if(nrow(tableS) >0){
  lines(smooth.spline(tableS$position-374999, tableS$SFM, df = 70), lwd =2, col = cols[e], xlim = c(0,750000), ylim = c(0,100), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
  }}
axis(2,col="gray")
mtext('CLR', 2, 3)
axis(1, at=c(-400000,-200000,0,200000,400000), labels = c("-0.4","-0.2","0", "0.2", "0.4"), col = "gray")
mtext('Position',1, 3)

plot(0, pch = "", xlim = c(0-374999,750000-374999), ylim = c(0,100), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n",axes = FALSE,
     main ="Introgressed sweep")
segments(-374999,0,750000-374999,0, "gray")
for(e in length(gens):1){
  tableS <- subset(tableSF, tableSF$population == 'p2' & tableSF$generations == gens[e])
  tableS <- na.omit(tableS)
  if(nrow(tableS) >0){
  lines(smooth.spline(tableS$positio-374999, tableS$SFM, df = 70), lwd=2, col = cols[e], xlim = c(0,750000), ylim = c(0,100), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
  }}
axis(2,col="gray", labels=F)
axis(1, at=c(-400000,-200000,0,200000,400000), labels = c("-0.4","-0.2","0", "0.2", "0.4"), col = "gray")
mtext('Position',1, 3)
# mtext('LR', 2, 2)

plot(0, pch = "", xlim = c(0-374999,750000-374999), ylim = c(-3,3), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE)
segments(-374999,0,750000-374999,0, "gray")
for(e in length(gens):1){
  tableS <- subset(tableTajD, tableTajD$population == 'p1' & tableTajD$generations == gens[e])
  
  lines(smooth.spline(tableS$position-374999, tableS$TajDM, df = 70), lwd = 2, col = cols[e], xlim = c(0,750000), ylim = c(-3,3), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '')
}
segments(374999-374999,-3,374999-374999,-2.5, "black", lty=1, lwd=2)
segments(394998.5-374999,-3,394998.5-374999,-2.5, "black", lty=2, lwd=2)
segments(414998.5-374999,-3,414998.5-374999,-2.5, "black", lty=3, lwd=2)

axis(2,col="gray")
mtext('Tajima\'s D', 2, 3)
axis(1, at=c(-400000,-200000,0,200000,400000), labels = c("-0.4","-0.2","0", "0.2", "0.4"), col = "gray")
mtext('Position',1, 3)

plot(0, pch = "", xlim = c(0-374999,750000-374999), ylim = c(-3,3), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE)
segments(-374999,0,750000-374999,0, "gray")
for(e in length(gens):1){
  tableS <- subset(tableTajD, tableTajD$population == 'p2' & tableTajD$generations == gens[e])
  
  lines(smooth.spline(tableS$position-374999, tableS$TajDM, df = 70), lwd = 2, col = cols[e], xlim = c(0,750000), ylim = c(-3,3), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '')
}
segments(374999-374999,-3,374999-374999,-2.5, "black", lty=1, lwd=2)
segments(394998.5-374999,-3,394998.5-374999,-2.5, "black", lty=2, lwd=2)
segments(414998.5-374999,-3,414998.5-374999,-2.5, "black", lty=3, lwd=2)
axis(2,col="gray",labels = F)
axis(1, at=c(-400000,-200000,0,200000,400000), labels = c("-0.4","-0.2","0", "0.2", "0.4"), col = "gray")
mtext('Position',1, 3)
# mtext('Tajima\'s D', 2, 2)


################





allgens_tajD_p2 <- read.csv("SLiM_Simulations_UPDATES/Classic_sweep_CLR_overtime.csv", header=FALSE)
# attach(allgens_tajD_p2)

n = 20

# means <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),mean)[1:6,2:75];
# sd <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),std)[1:6,2:75];

means <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), mean,  na.rm = TRUE)[,2:75];
sd <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), sd)[,2:75];

generations <- c(-0.2, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1) # -0.2 added as a reference for diversity pre-sweep

means <- cbind(generations, means)
sd <- cbind(generations, sd)


means$cols <- 'black'
for(e in (length(generations)):2){
  if(means$generations[e] == generations[e]){
    means$cols[e] <- cols[e-1]
  }
  if(means$generations[e] == -0.2){
    means$cols[e] <- 'black'
  }
}

# colf <- colorRampPalette(c("red", "blue"))


plot(means$generations, means$V38, type = "n", ylim = c(0, 100), ylab = "", xlab = "", xaxt = "n", main = "", bty="none", xaxt="none", yaxt="none")
axis(2,col="gray")
axis(1, at=c(-0.2, 0.01, 0.2, 0.4, 0.6, 0.8, 1), labels = c("BG", "0.01", "0.2", "0.4", "0.6", "0.8", "1"), col = "gray")
mtext('CLR', 2, 3)
segments(-0.20,0,1,0, "gray")
# polygon(c(means$generations,rev(means$generations)),c(means$V38+sd$V38,rev(means$V38-sd$V38)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V38, lwd=2, col = means$cols, lty=1, type='b')
# points(means$generations, means$V38, pch = 19, col = "black") 
# polygon(c(means$generations,rev(means$generations)),c(means$V40+sd$V40,rev(means$V40-sd$V40)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V40, lwd=2, col = means$cols, lty=2, type='b')
# points(means$generations, means$V40, pch = 19, col = "black") 
# polygon(c(means$generations,rev(means$generations)),c(means$V44+sd$V44,rev(means$V44-sd$V44)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V44, lwd=2, col = means$cols, lty=3, type='b')
# points(means$generations, means$V44, pch = 19, col = "black") 
mtext('Time (4Ne)',1, 3)

n = 29

allgens_tajD_p2 <- read.csv("SLiM_Simulations_UPDATES/Introgressed_sweep_CLR_overtime.csv", header=FALSE)
# attach(allgens_tajD_p2)

# means <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),mean)[2:7,2:75];
# sd <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),std)[2:7,2:75];
means <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), mean,  na.rm = TRUE)[,2:75];
sd <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), sd)[,2:75];

generations <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1, -0.2) # -0.2 added as a reference for diversity pre-sweep
means <- cbind(generations, means)
sd <- cbind(generations, sd)

# colf <- colorRampPalette(c("red", "blue"))
# colL <- colf(6)


means$cols <- 'black'
for(e in (length(generations)-1):1){
  if(means$generations[e] == generations[e]){
    means$cols[e] <- cols[e]
  }
  if(means$generations[e] == -0.2){
    means$cols[e] <- 'black'
  }
}

means <- means[order(generations),] 

plot(means$generations, means$V38, type = "n", ylim = c(0, 100), ylab = "", xlab = "", xaxt = "n", main = "", bty="none", xaxt="none", yaxt="none")
segments(-0.20,0,1,0, "gray")
axis(2,col="gray",labels=F)
axis(1, at=c(-0.2, 0.01, 0.2, 0.4, 0.6, 0.8, 1), labels = c("BG", "0.01", "0.2", "0.4", "0.6", "0.8", "1"), col = "gray")
lines(smooth.spline(means$generations, means$V38, df=6), lwd=2, col = means$cols, lty=1, type='b')
# points(means$generations, means$V38, pch = 19, col = "black") 
lines(smooth.spline(means$generations, means$V40, df=6), lwd=2, col = means$cols, lty=2, type='b')
# points(means$generations, means$V40, pch = 19, col = "black") 
lines(smooth.spline(means$generations, means$V44, df=6), lwd=2, col = means$cols, lty=3, type='b')
# points(means$generations, means$V44, pch = 19, col = "black") 
mtext('Time (4Ne)',1, 3)























allgens_tajD_p2 <- read.csv("SLiM_Simulations_UPDATES/Classic_sweep_TajimaD_overtime.csv", header=FALSE)
# attach(allgens_tajD_p2)

n = 20

# means <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),mean)[1:6,2:75];
# sd <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),std)[1:6,2:75];

means <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), mean,  na.rm = TRUE)[,2:75];
sd <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), sd)[,2:75];

generations <- c(-0.2, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1) # -0.2 added as a reference for diversity pre-sweep
means <- cbind(generations, means)
sd <- cbind(generations, sd)


means$cols <- 'black'
for(e in (length(generations)):2){
  if(means$generations[e] == generations[e]){
    means$cols[e] <- cols[e-1]
  }
  if(means$generations[e] == -0.2){
    means$cols[e] <- 'black'
  }
}

# colf <- colorRampPalette(c("red", "blue"))


plot(means$generations, means$V38, type = "n", ylim = c(-3, 3), ylab = "", xlab = "", xaxt = "n", main = "", bty="none", xaxt="none", yaxt="none")
axis(2,col="gray")
axis(1, at=c(-0.2, 0.01, 0.2, 0.4, 0.6, 0.8, 1), labels = c("BG", "0.01", "0.2", "0.4", "0.6", "0.8", "1"), col = "gray")
mtext('Tajima\'s D', 2, 3)
segments(-0.20,0,1,0, "gray")
# polygon(c(means$generations,rev(means$generations)),c(means$V38+sd$V38,rev(means$V38-sd$V38)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V38, lwd=2, col = means$cols, lty=1, type='b')
# points(means$generations, means$V38, pch = 19, col = "black") 
# polygon(c(means$generations,rev(means$generations)),c(means$V40+sd$V40,rev(means$V40-sd$V40)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V40, lwd=2, col = means$cols, lty=2, type='b')
# points(means$generations, means$V40, pch = 19, col = "black") 
# polygon(c(means$generations,rev(means$generations)),c(means$V44+sd$V44,rev(means$V44-sd$V44)), col=adjustcolor("gray",alpha.f = 0.5), border=NA)
lines(means$generations, means$V44, lwd=2, col = means$cols, lty=3, type='b')
# points(means$generations, means$V44, pch = 19, col = "black") 
mtext('Time (4Ne)',1, 3)

n = 29

allgens_tajD_p2 <- read.csv("SLiM_Simulations_UPDATES/Introgressed_sweep_TajimaD_overtime.csv", header=FALSE)
# attach(allgens_tajD_p2)

# means <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),mean)[2:7,2:75];
# sd <- aggregate(allgens_tajD_p2[,2:75],list(rep(1:(nrow(allgens_tajD_p2)%/%n),each=n,len=nrow(allgens_tajD_p2))),std)[2:7,2:75];
means <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), mean,  na.rm = TRUE)[,2:75];
sd <- aggregate(allgens_tajD_p2[,2:75], by=list(allgens_tajD_p2$V1), sd)[,2:75];

generations <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1, -0.2) # -0.2 added as a reference for diversity pre-sweep
means <- cbind(generations, means)
sd <- cbind(generations, sd)

# colf <- colorRampPalette(c("red", "blue"))
# colL <- colf(6)


means$cols <- 'black'
for(e in (length(generations)-1):1){
  if(means$generations[e] == generations[e]){
    means$cols[e] <- cols[e]
  }
  if(means$generations[e] == -0.2){
    means$cols[e] <- 'black'
  }
}

means <- means[order(generations),] 

plot(means$generations, means$V38, type = "n", ylim = c(-3, 3), ylab = "", xlab = "", xaxt = "n", main = "", bty="none", xaxt="none", yaxt="none")
segments(-0.20,0,1,0, "gray")
axis(2,col="gray",labels=F)
axis(1, at=c(-0.2, 0.01, 0.2, 0.4, 0.6, 0.8, 1), labels = c("BG", "0.01", "0.2", "0.4", "0.6", "0.8", "1"), col = "gray")
lines(smooth.spline(means$generations, means$V38, df=6), lwd=2, col = means$cols, lty=1, type='b')
# points(means$generations, means$V38, pch = 19, col = "black") 
lines(smooth.spline(means$generations, means$V40, df=6), lwd=2, col = means$cols, lty=2, type='b')
# points(means$generations, means$V40, pch = 19, col = "black") 
lines(smooth.spline(means$generations, means$V44, df=6), lwd=2, col = means$cols, lty=3, type='b')
# points(means$generations, means$V44, pch = 19, col = "black") 
mtext('Time (4Ne)',1, 3)



cols <- wes_palette("Zissou1", 255, type = "continuous")[0:255]
cols
cols[246]
