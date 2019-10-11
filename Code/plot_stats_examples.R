layout(matrix(c(1:12), nrow=4, ncol=3, byrow=F), heights=c(0.3,1,1,0.5), widths = c(0.2881356,0.4745763,0.2372881))
layout.show(n=12)
par(mai=c(0.2,0,0,0), oma=c(4,5,1,1))


start = 1.35
end = 2.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel210004.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "black", border = "black", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "black", border = "black", lwd=0.3)
}
ANNOT <- read.table("gff/Hmel210004_wntA_fixed.txt",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "red", border = "red", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "red", border = "red", lwd=0.3)
}


tableStats <- read.csv('captures_stats/Hmel210004_ple.S.stats', h=T, stringsAsFactors=F)
colnames(tableStats) <- c('scaffold','start','end','mid','sites','pi','pi_OUT','S','Pi_egg','Ke_egg','thetaW_egg','D_egg','Dfl_egg','Hsd_egg','Hns_egg','lseff_egg')
tableStats$Pi_egg <- as.numeric(tableStats$Pi_egg)/tableStats$lseff_egg
tableStats$D_egg <- as.numeric(tableStats$D_egg)
tableStats <- na.omit(tableStats)

tableSF <- read.table('SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/Mple_all_filtered_noINDELS_noAST_SF_bi_mono_OGpolmerged_M9.OM1.polsuballpol_75.Pop1_Hmel210004.SF2.lrg50.BGSFS.recrate.out', h=T, stringsAsFactors=F)


plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,0.05), xlim = c(start,end))
rect((1806000)/1000000, 0, (1833000)/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # split
par(new=T)
plot(tableStats$mid/1000000, tableStats$Pi_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(0,0.05), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$Pi_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(0,0.05), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(2,seq(0,0.05,by=0.01),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
mtext(expression(pi),2, line=3)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-3,3), xlim = c(start,end))
rect((1806000)/1000000, -3, (1833000)/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # split

# abline(h=0, col='gray')
segments(start,0,end,0, "gray")
par(new=T)
plot(tableStats$mid/1000000, tableStats$D_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(-3,3), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$D_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(-3,3), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(2,seq(-3,3,by=1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
mtext("Tajima's D",2, line=3)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,250), xlim = c(start,end))
rect((1806000)/1000000, 0, (1833000)/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # split

# abline(h=0, col='lightgray')

colfuncR <- colorRampPalette(c("black", "red"))
colL <- colfuncR(201)
tableSF$alpha <- sapply(((1/as.numeric(as.character(tableSF$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
tableSF$col <-colfuncR(201)[as.integer((tableSF$alpha)*1000)+1]

tableSF$LR <- lapply(tableSF$LR, function(x) ifelse(x > 1000, 1000, x))

par(new=T)
plot(tableSF$location/1000000, tableSF$LR, type = 'p', pch=19, cex= 0.8, col = tableSF$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
mtext("CLR",2, line=3)

# mtext('SF LR', 2, col = 'black', line=2.5, cex=2)

axis(2,seq(0,1000,by=200),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)

axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
mtext("Position (Mb)", side=1, line = 3)



#############################

start = 0.55
end = 1.95

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel215006.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "black", border = "black", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "black", border = "black", lwd=0.3)
}
ANNOT <- read.table("gff/Hmel215006_cortex.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "red", border = "red", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "red", border = "red", lwd=0.3)
}

tableStats <- read.csv('captures_stats/Hmel215006_weyWey.S.stats', h=T, stringsAsFactors=F)
colnames(tableStats) <- c('scaffold','start','end','mid','sites','pi','pi_OUT','S','Pi_egg','Ke_egg','thetaW_egg','D_egg','Dfl_egg','Hsd_egg','Hns_egg','lseff_egg')
tableStats$Pi_egg <- as.numeric(tableStats$Pi_egg)/tableStats$lseff_egg
tableStats$D_egg <- as.numeric(tableStats$D_egg)
tableStats <- na.omit(tableStats)

tableSF <- read.table('SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/Cweyw_C_filtered_noINDELS_noAST_SF_bi_mono_OGpolmerged_M8.OM1.polsuballpol_75.Pop1_Hmel215006.SF2.lrg50.BGSFS.recrate.out', h=T, stringsAsFactors=F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,0.05), xlim = c(start,end))
rect((1193549-2000)/1000000, 0, (1195549+4000)/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr1
rect((1217149-2000)/1000000, 0, (1219149+4000)/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr2
rect((1323223)/1000000, 0, (1338526)/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # FW

par(new=T)
plot(tableStats$mid/1000000, tableStats$Pi_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(0,0.05), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$Pi_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(0,0.05), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-3,3), xlim = c(start,end))
rect((1193549-2000)/1000000, -3, (1195549+4000)/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr1
rect((1217149-2000)/1000000, -3, (1219149+4000)/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr2
rect((1323223)/1000000, -3, (1338526)/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # FW

segments(start,0,end,0, "gray")
par(new=T)
plot(tableStats$mid/1000000, tableStats$D_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(-3,3), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$D_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(-3,3), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)


plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,250), xlim = c(start,end))
rect((1193549-2000)/1000000, 0, (1195549+4000)/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr1
rect((1217149-2000)/1000000, 0, (1219149+4000)/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # cr2
rect((1323223)/1000000, 0, (1338526)/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # FW

colfuncR <- colorRampPalette(c("black", "red"))
colL <- colfuncR(201)
tableSF$alpha <- sapply(((1/as.numeric(as.character(tableSF$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
tableSF$col <-colfuncR(201)[as.integer((tableSF$alpha)*1000)+1]

tableSF$LR <- lapply(tableSF$LR, function(x) ifelse(x > 1000, 1000, x))

par(new=T)
plot(tableSF$location/1000000, tableSF$LR, type = 'p', pch=19, cex= 0.8, col = tableSF$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)

# mtext('SF LR', 2, col = 'black', line=2.5, cex=2)

# axis(2,seq(0,1000,by=200),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
mtext("Position (Mb)", side=1, line = 3)






######################


start = 0.5
end = 1.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel218003.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "black", border = "black", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "black")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "black", border = "black", lwd=0.3)
}
ANNOT <- read.table("gff/Hmel218003_optix.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT <- subset(ANNOT,ANNOT$con_start/1000000 > start & ANNOT$con_end/1000000 < end)
for (g in 1:nrow(ANNOT)){
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.25, ANNOT$con_end[g]/1000000, 0.25, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "-") 
    rect(ANNOT$con_start[g]/1000000, 0.1, ANNOT$con_end[g]/1000000, 0.4, col = "red", border = "red", lwd=0.3)
  if (ANNOT$type[g] == "gene" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.75, ANNOT$con_end[g]/1000000, 0.75, col = NULL, border = "red")
  if (ANNOT$type[g] == "exon" && ANNOT$str[g] == "+") 
    rect(ANNOT$con_start[g]/1000000, 0.6, ANNOT$con_end[g]/1000000, 0.9, col = "red", border = "red", lwd=0.3)
}

tableStats <- read.csv('captures_stats/Hmel218003_merNEW.S.stats', h=T, stringsAsFactors=F)
# tableStats <- read.csv('captures_stats/Hmel218003_heu.S.stats', h=T, stringsAsFactors=F)
colnames(tableStats) <- c('scaffold','start','end','mid','sites','pi','pi_OUT','S','Pi_egg','Ke_egg','thetaW_egg','D_egg','Dfl_egg','Hsd_egg','Hns_egg','lseff_egg')
tableStats$Pi_egg <- as.numeric(tableStats$Pi_egg)/tableStats$lseff_egg
tableStats$D_egg <- as.numeric(tableStats$D_egg)
tableStats <- na.omit(tableStats)

tableSF <- read.table('SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/Mmer_C_HQ_filtered_noINDELS_noAST_SF_bi_mono_OGpolmerged_M5.OM1.polsuballpol_75.Pop1_Hmel218003.SF2.lrg50.BGSFS.recrate.out', h=T, stringsAsFactors=F)
# tableSF <- read.table('SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/HEU_all_filtered_noINDELS_noAST_SF_bi_mono_OGpolmerged_M18.OM1.polsuballpol_75.Pop1_Hmel218003.SF2.lrg50.BGSFS.recrate.out', h=T, stringsAsFactors=F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,0.05), xlim = c(start,end))
rect(814176/1000000, 0, 820339/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # dennis
rect(773272/1000000, 0, 787408/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # ray
rect(718819/1000000, 0, 730976/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 1
rect(780304/1000000, 0, 796765/1000000, 0.05, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 2
par(new=T)
plot(tableStats$mid/1000000, tableStats$Pi_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(0,0.05), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$Pi_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(0,0.05), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)

plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-3,3), xlim = c(start,end))
rect(814176/1000000, -3, 820339/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # dennis
rect(773272/1000000, -3, 787408/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # ray
rect(718819/1000000, -3, 730976/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 1
rect(780304/1000000, -3, 796765/1000000, 3, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 2
segments(start,0,end,0, "gray")
par(new=T)
plot(tableStats$mid/1000000, tableStats$D_egg, type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(-3,3), xlim = c(start,end),ann=FALSE)
lines(smooth.spline(tableStats$mid/1000000, tableStats$D_egg, df = 100), lwd =2, col = "orange", xlim = c(start,end), ylim = c(-3,3), type ='l', xlab = "", bty = "n", xaxt = "n", ylab = '', pch=19)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, labels = F)


plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,250), xlim = c(start,end))
rect(814176/1000000, 0, 820339/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # dennis
rect(773272/1000000, 0, 787408/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # ray
rect(718819/1000000, 0, 730976/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 1
rect(780304/1000000, 0, 796765/1000000, 250, col = adjustcolor('gray50',alpha=0.2), border = NA) # band 2

colfuncR <- colorRampPalette(c("black", "red"))
colL <- colfuncR(201)
tableSF$alpha <- sapply(((1/as.numeric(as.character(tableSF$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
tableSF$col <-colfuncR(201)[as.integer((tableSF$alpha)*1000)+1]
# tableSF$col <- colfuncR(500)[as.numeric(cut((1/tableSF$alpha)*1000, breaks = 500))]

tableSF$LR <- lapply(tableSF$LR, function(x) ifelse(x > 1000, 1000, x))

par(new=T)
plot(tableSF$location/1000000, tableSF$LR, type = 'p', pch=19, cex= 0.8, col = tableSF$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)

# mtext('SF LR', 2, col = 'black', line=2.5, cex=2)

# axis(2,seq(0,1000,by=200),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
axis(1,seq(start,end,by=0.1),line=0,col="gray",col.ticks="gray",col.axis="black", cex.axis=1)
mtext("Position (Mb)", side=1, line = 3)


