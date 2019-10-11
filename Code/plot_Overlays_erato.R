rm(list=ls())
library("scales")


####select dataset####
setwd("/home/moestmar/Dropbox/CAPTURE_Steven/SF_Markus/NEW_FILES/ERATO")
list.files()

data1 <- ("POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE")
data2 <- ("POLSUBALLPOL_75_OM2_LG50_BGSFS_RECRATE")
data3 <- ("POLSUBALLPOL_75_OM1_LG50_BGSFSwALL_RECRATE")

####choose data####
data <- data1
run <- 'data1' 
####choose method####
meth <- 'CLR' #'scaledCLR'
setwd(paste0(getwd(),"/",data))
files <- list.files()

####extract unique IDs from folder####

uniqIDs <- unique(unlist(lapply(files,function(x) unlist(strsplit(x,split = '.OM'))[1])))
uniqIDs <- unique(unlist(lapply(uniqIDs,function(x) unlist(strsplit(x,split = '_'))[2])))
head(uniqIDs)
IDs <- uniqIDs[!uniqIDs=="chestertonii" & !uniqIDs=="himera"]

###define scaffolds####
scaffs <- data.frame(scaffold=c("Herato1001","Herato1505","Herato1801"), start=c(3500000,1000000,900000), end=c(5500000,3000000,1800000))

#png("Overlays_erato.png", width=2048, height=1024)
svg("Overlays_erato.svg", width=14, height=8, pointsize = 12)
###set layout###
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(2,2,2,2), heights = c(1,1,1,1))
par(mar=c(5, 5, 3, 2) + 0.1)

## show the regions that have been allocated to each plot

for (i in 1:3){
  ####select scaffold####
  scaf <- scaffs[i,]
  ####set ylim #####
  if (meth=='scaledCLR'){ 
    ylim=c(0,1)
    ylab="CLR/CLRmax"
    annoffset=0.05
  }
  if (meth=='CLR'){ 
    ylim=c(0,1500)
    ylab="CLR"
    annoffset=50
  }
  
  cex=1.2
  cex_n=0.8
  ####create an empty plot####
  plot(NULL, type="n", xlab="Scaffold Position [Mb]", ylab=ylab, xlim=c(scaf$start/10^6,scaf$end/10^6), ylim=c(0,(ylim[2]+2*annoffset)), bty="l", xaxt="n", cex.axis=cex, cex.lab=cex, yaxs = "i")

####add annotations####
 if (scaf$scaffold== "Herato1001") {
   ##plot annotation
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1001.gff",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }   
  ##wntA annotation
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1001_wntA.gff",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }   
   
   axis(side=1, at=c(3.50,3.60,3.70,3.80,3.90,4.00,4.10,4.20,4.30,4.30,4.40,4.50,4.60,4.70,4.80,4.90,5.00,5.10,5.20,5.30,5.40,5.00,5.10,5.20,5.30,5.40,5.50), cex.axis=cex)
   rect(4626226/10^6,0,4707387/10^6,ylim[2], col=alpha("yellow",0.1), border=alpha("yellow",0.5)) 
   mtext("1",at = 4626226/10^6, cex = cex_n) #wntA 
   
   rect(4637657/10^6,0,4637727/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("2", line=1, at = 4637657/10^6, cex = cex_n) #Sd1 
   
   rect(4639853/10^6,0,4641535/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("3",line=2, at = 4639853/10^6, cex = cex_n) #Sd2
   
   rect(4657452/10^6,0,4658207/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("4",line=1,at = 4657452/10^6, cex = cex_n) #St  
   
   rect(4666909/10^6,0,4670474/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("5",at = 4666909/10^6, cex = cex_n) #Ly1 
   
   rect(4700932/10^6,0,4708441/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("6",at = 4700932/10^6, cex = cex_n) #Ly2 
   
   mtext("A) WntA",side = 3,line = 1, adj=0, cex = cex_n*1.5)
 }
 if (scaf$scaffold == "Herato1505") {
   ##plot annotation
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1505.gff",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }     
   
   ##cortex annotation
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1505_cortex_fixed.txt",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }     
   
   axis(side=1, at=c(1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80,1.90,2.00,2.10,2.20,2.30,2.40,2.50,2.60,2.70,2.80,2.90,3.00), cex.axis=cex)
     rect(2074108/10^6,0,2238899/10^6,ylim[2], col=alpha("yellow",0.1), border=alpha("yellow",0.5)) 
     mtext("8",line=1, at = 2074108/10^6, cex = cex_n) # cortex
     
     rect(2434679/10^6,0,2436667/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
     mtext("10",at = 2434679/10^6, cex = cex_n) #truncated domeless - actually no published evidence for erato!!! This seems to be the domeless with evidence for melpomene in Nadeau et al
     
     rect(2444519/10^6,0,2476891/10^6,ylim[2], col=alpha("green",0.1), border=alpha("green",0.3)) 
     mtext("11",line=1,at = 2444519/10^6, cex = cex_n) #wash - actually no published evidence for erato!!!
     
     rect(2476323/10^6,0,2480084/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
     mtext("12",line=2,at = 2476323/10^6, cex = cex_n) #domeless - actually no published evidence for erato!!! 
     
     rect(2481027/10^6,0,2501792/10^6,ylim[2], col=alpha("green",0.1), border=alpha("green",0.3))
     mtext("13", at = 2481027/10^6, cex = cex_n) #lethal 2 - evidence in Nadeau!
     
     rect(2053037/10^6,0,2171230/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) #Cr1: 
     mtext("7",at = 2053037/10^6, cex = cex_n)
     
     rect(2211881/10^6,0,2315926/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) #Cr2: 
     mtext("9",at = 2211881/10^6, cex = cex_n)
     
     mtext("B) cortex",side = 3, line = 1, adj = 0, cex = cex_n*1.5)
 }
 if (scaf$scaffold == "Herato1801") {
   ##plot annotation
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1801.gff",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   ##plot optix
   ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Herato1801_optix.gff",sep="\t")
   names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }
   
   for (g in 1:nrow(ANNOT)){
     if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
       rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
     }
   }   
   
   axis(side=1, at=c(0.90,0.95,1.00,1.05,1.10,1.15,1.20,1.25,1.30,1.35,1.40,1.45,1.50,1.55,1.60,1.65,1.70,1.75,1.80), cex.axis=cex)
   rect(1250582/10^6,0,1251211/10^6,ylim[2], col=alpha("yellow",0.1), border=alpha("yellow",0.5)) 
   mtext("14",line=2,at = 1250582/10^6, cex = cex_n) #optix 
   
   rect(1377801/10^6,0,1384841/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("15",at = 1377801/10^6, cex = cex_n) #rays
   
   rect(1403328/10^6,0,1412865/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("16",at = 1403328/10^6, cex = cex_n) #band Y1 
   
   rect(1420912/10^6,0,1422355/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("18", line=2,at = 1420912/10^6, cex = cex_n) #band Y2
   
   rect(1412888/10^6,0,1419375/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
   mtext("17",line=1, at = 1412888/10^6, cex = cex_n) #dennis D1
   
   rect(1422585/10^6,0,1428307/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1))
   mtext("19",at = 1422585/10^6, cex = cex_n) #dennis D2 
   
   mtext("C) optix",side = 3, line = 1, adj=0, cex=cex_n*1.5) 
 }
  
####loop over all pops and plot them on top of each other####
for (popnum in 1:length(IDs)){
    
####select population####
pop <- IDs[popnum]
file <- dir(pattern=paste0(scaf$scaffold,"_",pop))    
print(file)
sweep <- read.table(file, header=T, as.is=T)

maxCLR <- max(sweep$LR)
#scaled CLR
if (meth=='scaledCLR') polygon(x=c(sweep$location/10^6,sweep$location[length(sweep$location)]/10^6,sweep$location[1]/10^6),y=c(sweep$LR/maxCLR,0,0), lty=0, col = alpha("black",0.05), pch=1)
#CLR
if (meth=='CLR'){ 
  sweep$LR <- ifelse(sweep$LR>ylim[2],ylim[2],sweep$LR)
  polygon(x=c(sweep$location/10^6,sweep$location[length(sweep$location)]/10^6,sweep$location[1]/10^6),y=c(sweep$LR,0,0), lty=0, col = alpha("black",0.08), pch=1)
}            
}
}

dev.off()







  
