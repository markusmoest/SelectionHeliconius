pops <- c('dem','not','emm','fav','eraFG','hydFG','ama','che','him')
popsSF <- c('demophoon','notabilis','emma','favorinus','eratoFG','hydara','amalfreda','chestertonii','himera')
colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','mediumseagreen','steelblue2')
names <- c('H. e. demophoon','H. e. notabilis','H. e. emma','H. e. favorinus','H. e. erato','H. e. hydara (FG)','H. e. amalfreda','H. e. chestertonii','H. himera') 

pops <- c('dem','not','emm','fav','che','him')
popsSF <- c('demophoon','notabilis','emma','favorinus','chestertonii','himera')
colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','mediumseagreen','steelblue2')
names <- c('H. e. demophoon','H. e. notabilis','H. e. emma','H. e. favorinus','H. e. chestertonii','H. himera')


filesSF_Herato1001 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1001', full.names = T)

filesEHH_Herato1001 <- list.files(path='erato_EHH', pattern='Herato1001.PHASED.ihh12.out.norm', full.names = T)
filesEHH_Herato1505 <- list.files(path='erato_EHH', pattern='Herato1505.PHASED.ihh12.out.norm', full.names = T)
filesEHH_Herato1801 <- list.files(path='erato_EHH', pattern='Herato1801.PHASED.ihh12.out.norm', full.names = T)

filesStats_Herato1001 <- list.files(path='erato_stats/', pattern='Herato1001', full.names = T)
filesStats_Herato1505 <- list.files(path='erato_stats/', pattern='Herato1505', full.names = T)
filesStats_Herato1801 <- list.files(path='erato_stats/', pattern='Herato1801', full.names = T)

filesGenoPS_Herato1001 <- list.files(path='erato_genoPS/', pattern='Herato1001', full.names = T)
filesGenoPS_Herato1505 <- list.files(path='erato_genoPS/', pattern='Herato1505', full.names = T)
filesGenoPS_Herato1801 <- list.files(path='erato_genoPS/', pattern='Herato1801', full.names = T)


for(e in 1:length(pops)){
  
  png(filename=paste("C:/Users/vanbe/Dropbox (Personal)/Manuscript_Selection/Figures/Suppl_stats_erato/Herato1001_",names[e], ".png", sep=""), width = 1000, height = 1000)
  
  print(pops[e])
  
  indxSF <- grep(popsSF[e], filesSF_Herato1001)
  indxEHH <- grep(popsSF[e], filesEHH_Herato1001)
  indxStats <- grep(pops[e], filesStats_Herato1001)
  indxGenoPS <- grep(pops[e], filesGenoPS_Herato1001)
  
  tableSF <- read.table(filesSF_Herato1001[indxSF], h=T, stringsAsFactors=F)
  tableEHH <- read.table(filesEHH_Herato1001[indxEHH], h=T, stringsAsFactors=F)
  tableStats <- read.csv(filesStats_Herato1001[indxStats], h=T, stringsAsFactors=F)
  tableGenoPS <- read.csv(filesGenoPS_Herato1001[indxGenoPS], h=T, stringsAsFactors=F)
  
  colnames(tableGenoPS) <- c('scaffold', 'mid', 'start','end','sites','Pi','genoPS')
  colnames(tableStats) <- c('scaffold','start','end','mid','sites','pi','pi_OUT','Pi_egg','Ke_egg','thetaW_egg','D_egg','Dfl_egg','Hsd_egg','Hns_egg','lseff_egg')
  
  
  ANNOT <- read.table("gff/Herato1001.gff",sep="\t")
  names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  ANNOT2 <- read.table("gff/Herato1001_wntA.gff",sep="\t")
  names(ANNOT2) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  
  
  par(mfrow=c(1,5),mai=c(1,1,0.05,0.05), oma=c(3,3,1,3)+0)
  
  layout(matrix(c(1:5), ncol=1))
  
  start = 3.5
  end = 5.5
  
  
  # genoPS
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,1.2), xlim = c(start,end))
  # rect(4695169/1000000, 0, 4707387/1000000, 26, col = adjustcolor('gray', 0.5), border = NA)
  rect(4695169/1000000, 0, 4707387/1000000, 1.2, col = adjustcolor('gray', 0.5), border = NA)
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/1000000 >= start & ANNOT$con_end[g]/1000000 <= end){
      rect(ANNOT$con_start[g]/1000000, 1.1, ANNOT$con_end[g]/1000000, 1.1, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/1000000 >= start & ANNOT$con_end[g]/1000000 <= end){
      rect(ANNOT$con_start[g]/1000000, 1, ANNOT$con_end[g]/1000000, 1.2, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  for (g in 1:nrow(ANNOT2)){
    if (ANNOT2$type[g] == "gene" & ANNOT2$con_start[g]/1000000 >= start & ANNOT2$con_end[g]/1000000 <= end){
      rect(ANNOT2$con_start[g]/1000000, 1.1, ANNOT2$con_end[g]/1000000, 1.1, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT2)){
    if (ANNOT2$type[g] == "exon" & ANNOT2$con_start[g]/1000000 >= start & ANNOT2$con_end[g]/1000000 <= end){
      rect(ANNOT2$con_start[g]/1000000, 1, ANNOT2$con_end[g]/1000000, 1.2, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  abline(h=0, col='lightgray')
  
  rect(4624122/1000000, 0, 4647331/1000000, 1, density = NULL, angle = 45, col = adjustcolor("blue",alpha=0.3), border = "blue", lty = par("lty"), lwd = par("lwd"))
  rect(4647332/1000000, 0, 4674571/1000000, 1, density = NULL, angle = 45, col = adjustcolor("green",alpha=0.3), border = "green", lty = par("lty"), lwd = par("lwd"))
  rect(4666909/1000000, 0, 4670474/1000000, 1, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  rect(4700932/1000000, 0, 4708441/1000000, 1, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  
  
  par(new=T)
  plot(tableGenoPS$mid/1000000, tableGenoPS$genoPS, type = 'l', col ='NA', axes = FALSE, ylim=c(0,1.2), xlim = c(start,end), ann=FALSE)
  
  y <- rep(tableGenoPS$genoPS, each=2)
  y <- y[-length(y)]
  x <- rep(tableGenoPS$mid/1000000, each=2)[-1]
  x2 <- c(min(x), x, max(x))
  y2 <- c(0, y, 0)
  
  polygon(x2, y2, col = "gray", border = NA)
  
  mtext('Coverage', 2, col = 'black', line=2.5, cex=2)
  axis(2,seq(0,1,by=0.2),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  text(5,0.2, substitute(paste(italic(nn)), list(nn=names[e])), cex=2)
  
  
  
  # Pi 
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,0.06), xlim = c(start,end))
  rect(4695169/1000000, 0, 4707387/1000000, 0.06, col = adjustcolor('gray', 0.5), border = NA)
  abline(h=0, col='lightgray') 
  
  rect(4624122/1000000, 0, 4647331/1000000, 0.06, density = NULL, angle = 45, col = adjustcolor("blue",alpha=0.3), border = "blue", lty = par("lty"), lwd = par("lwd"))
  rect(4647332/1000000, 0, 4674571/1000000, 0.06, density = NULL, angle = 45, col = adjustcolor("green",alpha=0.3), border = "green", lty = par("lty"), lwd = par("lwd"))
  rect(4666909/1000000, 0, 4670474/1000000, 0.06, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  rect(4700932/1000000, 0, 4708441/1000000, 0.06, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  
  
  par(new=T)
  plot(tableStats$mid/1000000, as.numeric(tableStats$Pi_egg)/tableStats$lseff_egg, type = 'p', pch=19, cex= 0.8, col = "black", axes = FALSE, ylim=c(0,0.06), xlim = c(start,end),ann=FALSE)
  
  mtext('Pi', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,0.06,by=0.01),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  # TajD
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-3,3), xlim = c(start,end))
  rect(4695169/1000000, -3, 4707387/1000000, 3, col = adjustcolor('gray', 0.5), border = NA)
  abline(h=0, col='lightgray') 
  
  rect(4624122/1000000, -3, 4647331/1000000, 3, density = NULL, angle = 45, col = adjustcolor("blue",alpha=0.3), border = "blue", lty = par("lty"), lwd = par("lwd"))
  rect(4647332/1000000, -3, 4674571/1000000, 3, density = NULL, angle = 45, col = adjustcolor("green",alpha=0.3), border = "green", lty = par("lty"), lwd = par("lwd"))
  rect(4666909/1000000, -3, 4670474/1000000, 3, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  rect(4700932/1000000, -3, 4708441/1000000, 3, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  
  par(new=T)
  plot(tableStats$mid/1000000, as.numeric(tableStats$D_egg), type = 'p', pch=19, cex= 0.8, col = "black", axes = FALSE, ylim=c(-3,3), xlim = c(start,end),ann=FALSE)
  
  mtext('Taj D', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(-3,3,by=1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  
  
  # ihh12
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-1.5,26), xlim = c(start,end))
  rect(4695169/1000000, 0, 4707387/1000000, 26, col = adjustcolor('gray', 0.5), border = NA)
  abline(h=0, col='lightgray')
  
  rect(4624122/1000000, 0, 4647331/1000000, 26, density = NULL, angle = 45, col = adjustcolor("blue",alpha=0.3), border = "blue", lty = par("lty"), lwd = par("lwd"))
  rect(4647332/1000000, 0, 4674571/1000000, 26, density = NULL, angle = 45, col = adjustcolor("green",alpha=0.3), border = "green", lty = par("lty"), lwd = par("lwd"))
  rect(4666909/1000000, 0, 4670474/1000000, 26, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  rect(4700932/1000000, 0, 4708441/1000000, 26, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  
  par(new=T)
  plot(tableEHH$pos/1000000, tableEHH$normihh12, type = 'p', pch=19, cex= 0.8, col ="black", axes = FALSE,ylim=c(-1.5,26),xlim = c(start,end),ann=FALSE)
  
  mtext('ihh12', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,26,by=4),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)

  # SF

  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,120), xlim = c(start,end))
  rect(4695169/1000000, 0, 4707387/1000000, 120, col = adjustcolor('gray', 0.5), border = NA)

  abline(h=0, col='lightgray')

  rect(4624122/1000000, 0, 4647331/1000000, 120, density = NULL, angle = 45, col = adjustcolor("blue",alpha=0.3), border = "blue", lty = par("lty"), lwd = par("lwd"))
  rect(4647332/1000000, 0, 4674571/1000000, 120, density = NULL, angle = 45, col = adjustcolor("green",alpha=0.3), border = "green", lty = par("lty"), lwd = par("lwd"))
  rect(4666909/1000000, 0, 4670474/1000000, 120, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))
  rect(4700932/1000000, 0, 4708441/1000000, 120, density = NULL, angle = 45, col = adjustcolor("orange",alpha=0.3), border = "orange", lty = par("lty"), lwd = par("lwd"))

  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSF$col <- colfuncR(500)[as.numeric(cut((1/tableSF$alpha)*1000, breaks = 500))]
  
  tableSF$LR <- lapply(tableSF$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSF$location/1000000, tableSF$LR, type = 'p', pch=19, cex= 0.8, col = tableSF$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
  
  mtext('SF LR', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,1000,by=200),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)

  axis(1,seq(0,6,by=0.1),line=1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)

  mtext('Position (Mb)', 1, col = 'black', line=5, cex=2)

  dev.off()
}


