pops <- c('agl','ama','cyt','ecu','malC','malE','melC','melG','melP','mer','nan','ple','ros','vul','xen',
          'chi','cyd','pac','weyGus','weyWey','zel',
          'flo','heu','lin','the','timCon','timTim','timVic','tspn',
          'ele','bes')

popsSF <- c('Magl','Mama','Mcyt','Mecu','MmalC','MmalE','MmelC','MmelG','MmelP','Mmer','Mnan_all_NORTH','Mple','Mros','Mvul','Mxen',
            'Cchi','Ccyd','PAC','Cweyg','Cweyw','Czel',
            'Tflo','HEU','Tlin','Tthe','Ttimc','Ttimt','Tvic','TspnE',
            'ELE','BES')
colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen',
               'steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2',
               'orange','orange')
names <- c('H. melpomene aglaope','H. melpomene amaryllis','H. melpomene cythera','H. melpomene ecuadoriensis','H. melpomene malleti C','H. melpomene malleti E','H. melpomene melpomene C','H. melpomene melpomene G','H. melpomene melpomene E','H. melpomene meriana','H. melpomene nanna','H. melpomene plesseni','H. melpomene rosina','H. melpomene vulcanus','H. melpomene xenoclea',
           'H. cydno chioneus','H. cydno cydnides','H. pachinus ','H. cydno weymeri gustavi','H. cydno weymeri weymeri','H. cydno zelinde',
           'H. timareta florencia','H. heurippa ','H. timareta linaresi','H. timareta thelxinoe','H. timareta contigua','H. timareta timareta','H. timareta XXX','H. timareta sp-nov_ecu',
           'H. elevatus ','H. besckei ') 

mel <- c('agl','ama','cyt','ecu','malC','malE','melC','melG','melP','mer','nan','ple','ros','vul','xen')
cyd <- c('chi','cyd','pac','weyGus','weyWey','zel')
tim <- c('flo','heu','lin','the','timCon','timTim','timVic','tspn')
sil <- c('ele','bes')


# ANNOT <- read.table("gff/Hmel201011.gff",sep="\t")
# names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")

ANNOT <- read.table("gff/Hmel218003.gff",sep="\t")
names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
ANNOT2 <- read.table("gff/Hmel218003_optix.gff",sep="\t")
names(ANNOT2) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
# ANNOT_Hmel215006 <- read.table("gff/Hmel215006.gff",sep="\t")
# names(ANNOT_Hmel215006) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
 
# ANNOT <- read.table("gff/Hmel210004.gff",sep="\t")
# names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")


filesSF_Hmel201011 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel201011', full.names = T)
filesSF_Hmel210004 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel210004', full.names = T)
filesSF_Hmel215006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel215006', full.names = T)
filesSF_Hmel218003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel218003', full.names = T)

filesEHH_Hmel201011 <- list.files(path='EHH', pattern='Hmel201011.bi.g4000.oriented.ihh12.out.norm', full.names = T)
filesEHH_Hmel210004 <- list.files(path='EHH', pattern='Hmel210004.bi.g4000.oriented.ihh12.out.norm', full.names = T)
filesEHH_Hmel215006 <- list.files(path='EHH', pattern='Hmel215006.bi.g4000.oriented.ihh12.out.norm', full.names = T)
filesEHH_Hmel218003 <- list.files(path='EHH', pattern='Hmel218003.bi.g4000.oriented.ihh12.out.norm', full.names = T)

filesStats_Hmel201011 <- list.files(path='captures_stats/', pattern='Hmel201011', full.names = T)
filesStats_Hmel210004 <- list.files(path='captures_stats/', pattern='Hmel210004', full.names = T)
filesStats_Hmel215006 <- list.files(path='captures_stats/', pattern='Hmel215006', full.names = T)
filesStats_Hmel218003 <- list.files(path='captures_stats/', pattern='Hmel218003', full.names = T)

filesGenoPS_Hmel201011 <- list.files(path='genoPS/', pattern='Hmel201011', full.names = T)
filesGenoPS_Hmel210004 <- list.files(path='genoPS/', pattern='Hmel210004', full.names = T)
filesGenoPS_Hmel215006 <- list.files(path='genoPS/', pattern='Hmel215006', full.names = T)
filesGenoPS_Hmel218003 <- list.files(path='genoPS/', pattern='Hmel218003', full.names = T)

# filesDiplo_Hmel218003 <- list.files(path='diploSHIC/', pattern='Hmel218003', full.names = T)

start = 0.5
end = 1.2



pops <- c('agl','ama','malC','malE','ple','xen','merNEW','melC','melG','nanNorth','nanSouth',
          'weyGus','weyWey','cyd',
          'melP','ros','vul','cyt','ecu',
          'chi','zel','pac',
          'the','timCon','timTim','timVic','tspn','heu','flo','lin')

popsEHH <- c('agl','ama','malC','malE','ple','xen','mer','melC','melG','nan','nan',
             'weyGus','weyWey','cyd',
             'melP','ros','vul','cyt','ecu',
             'chi','zel','pac',
             'the','timCon','timTim','timVic','tspn','heu','flo','lin')

popsSF <- c('Magl','Mama','MmalC','MmalE','Mple','Mxen','Mmer','MmelC','MmelG','Mnan_all_NORTH','Mnan_all_SOUTH',
            'Cweyg','Cweyw','Ccyd',
            'MmelP','Mros','Mvul','Mcyt','Mecu',
            'Cchi','Czel','PAC',
            'Tthe','Ttimc','Ttimt','Tvic','TspnE','HEU','Tflo','Tlin')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'mediumseagreen','mediumseagreen','mediumseagreen',
               'hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'mediumseagreen','mediumseagreen','mediumseagreen',
               'steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2')

names <- c('H. melpomene aglaope','H. melpomene amaryllis','H. melpomene malleti C','H. melpomene malleti E','H. melpomene plesseni','H. melpomene xenoclea','H. melpomene meriana','H. melpomene melpomene C','H. melpomene melpomene G','H. melpomene nanna (North)','H. melpomene nanna (South)',
           'H. cydno weymeri gustavi','H. cydno weymeri weymeri','H. cydno cydnides',
           'H. melpomene melpomene P','H. melpomene rosina','H. melpomene vulcanus','H. melpomene cythera','H. melpomene ecuadoriensis',
           'H. cydno chioneus','H. cydno zelinde','H. pachinus ',
           'H. timareta thelxinoe','H. timareta contigua','H. timareta timareta','H. timareta XXX','H. timareta sp-nov_ecu','H. heurippa ','H. timareta florencia','H. timareta linaresi') 


for(e in 1:length(pops)){
  png(filename=paste("C:/Users/vanbe/Dropbox (Personal)/Manuscript_Selection/Figures/Suppl_stats_melp/Hmel218003_",names[e], ".png", sep=""), width = 1000, height = 1000)
  
  print(pops[e])
  
  indxSF <- grep(popsSF[e], filesSF_Hmel218003)
  indxEHH <- grep(popsEHH[e], filesEHH_Hmel218003)
  indxStats <- grep(paste(pops[e],'.S.stats',sep=''), filesStats_Hmel218003)
  indxGenoPS <- grep(popsEHH[e], filesGenoPS_Hmel218003)
  # indxDiplo <- grep(pops[e], filesDiplo_Hmel218003)
  
  tableSF <- read.table(filesSF_Hmel218003[indxSF], h=T, stringsAsFactors=F)
  tableEHH <- read.table(filesEHH_Hmel218003[indxEHH], h=T, stringsAsFactors=F)
  tableStats <- read.csv(filesStats_Hmel218003[indxStats], h=T, stringsAsFactors=F)
  tableGenoPS <- read.csv(filesGenoPS_Hmel218003[indxGenoPS], h=T, stringsAsFactors=F)
  # tableDiplo <- read.table(filesDiplo_Hmel218003[indxDiplo], h=T, stringsAsFactors=F)
  
  colnames(tableGenoPS) <- c('scaffold', 'mid', 'start','end','sites','Pi','genoPS')
  colnames(tableStats) <- c('scaffold','start','end','mid','sites','pi','pi_OUT','S','Pi_egg','Ke_egg','thetaW_egg','D_egg','Dfl_egg','Hsd_egg','Hns_egg','lseff_egg')
  
  
  par(mfrow=c(1,5),mai=c(1,1,0.05,0.05), oma=c(3,3,1,3)+0)
  
  layout(matrix(c(1:5), ncol=1)) 
  
  # GenoPS
  plot(0,type='n',axes=FALSE,ann=FALSE, ylim=c(0,1.2), xlim = c(start,end))
  rect(705604/1000000, 0, 706407/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  # 
  # rect(814176/1000000, 0, 820339/1000000, 1, col = 'black', border = NA) # dennis
  # rect(773272/1000000, 0, 787408/1000000, 1, col = 'red', border = NA) # ray
  # rect(718819/1000000, 0, 730976/1000000, 1, col = 'yellow', border = NA) # band 1
  # rect(780304/1000000, 0, 796765/1000000, 1, col = 'yellow', border = NA) # band 2
  abline(h=0, col='lightgray')
  
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
  
  par(new=T)
  plot(tableGenoPS$mid/1000000, tableGenoPS$genoPS, type = 'l', col ='NA', axes = FALSE, ylim=c(0,1.2), xlim = c(start,end),ann=FALSE)
  
  y <- rep(tableGenoPS$genoPS, each=2)
  y <- y[-length(y)]
  x <- rep(tableGenoPS$mid/1000000, each=2)[-1]
  x2 <- c(min(x), x, max(x))
  y2 <- c(0, y, 0)
  
  polygon(x2, y2, col = "gray", border = NA)
  
  mtext('Coverage', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,1,by=0.2),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  text(0.9,0.2, substitute(paste(italic(nn)), list(nn=names[e])), cex=2)

  
  # Pi 
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,0.06), xlim = c(start,end))
  rect(705604/1000000, 0, 706407/1000000, 0.06, col = adjustcolor('gray', 0.5), border = NA)
  
  # rect(814176/1000000, 0, 820339/1000000, 0.06, col = 'black', border = NA) # dennis
  # rect(773272/1000000, 0, 787408/1000000, 0.06, col = 'red', border = NA) # ray
  # rect(718819/1000000, 0, 730976/1000000, 0.06, col = 'yellow', border = NA) # band 1
  # rect(780304/1000000, 0, 796765/1000000, 0.06, col = 'yellow', border = NA) # band 2
  abline(h=0, col='lightgray') 
  
  par(new=T)
  plot(tableStats$mid/1000000, as.numeric(tableStats$Pi_egg)/tableStats$lseff_egg, type = 'p', pch=19, cex= 0.8, col = "black", axes = FALSE, ylim=c(0,0.06), xlim = c(start,end),ann=FALSE)
  
  mtext('Pi', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,0.06,by=0.01),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  # TajD
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-3,3), xlim = c(start,end))
  rect(705604/1000000, -3, 706407/1000000, 3, col = adjustcolor('gray', 0.5), border = NA)
  
  # rect(814176/1000000, -3, 820339/1000000, 3, col = 'black', border = NA) # dennis
  # rect(773272/1000000, -3, 787408/1000000, 3, col = 'red', border = NA) # ray
  # rect(718819/1000000, -3, 730976/1000000, 3, col = 'yellow', border = NA) # band 1
  # rect(780304/1000000, -3, 796765/1000000, 3, col = 'yellow', border = NA) # band 2
  abline(h=0, col='lightgray') 
  
  par(new=T)
  plot(tableStats$mid/1000000, as.numeric(tableStats$D_egg), type = 'p', pch=19, cex= 0.8, col = 'black', axes = FALSE, ylim=c(-3,3), xlim = c(start,end),ann=FALSE)
  
  mtext('Taj D', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(-3,3,by=1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  # ihh12
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(-1.5,26), xlim = c(start,end))
  rect(705604/1000000, 0, 706407/1000000, 26, col = adjustcolor('gray', 0.5), border = NA)
  
  # rect(814176/1000000, 0, 820339/1000000, 26, col = 'black', border = NA) # dennis
  # rect(773272/1000000, 0, 787408/1000000, 26, col = 'red', border = NA) # ray
  # rect(718819/1000000, 0, 730976/1000000, 26, col = 'yellow', border = NA) # band 1
  # rect(780304/1000000, 0, 796765/1000000, 26, col = 'yellow', border = NA) # band 2
  
  abline(h=0, col='lightgray')
  
  par(new=T)
  plot(tableEHH$pos/1000000, tableEHH$normihh12, type = 'p', pch=19, cex= 0.8, col = "black", axes = FALSE, ylim = c(-1.5,26), xlim = c(start,end),ann=FALSE)
  
  mtext('ihh12', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,26,by=4),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  # SF
  
  plot(0,type='n',axes=FALSE,ann=FALSE,ylim=c(0,180), xlim = c(start,end))
  rect(705604/1000000, 0, 706407/1000000, 180, col = adjustcolor('gray', 0.5), border = NA)
  
  # rect(814176/1000000, 0, 820339/1000000, 180, col = 'black', border = NA) # dennis
  # rect(773272/1000000, 0, 787408/1000000, 180, col = 'red', border = NA) # ray
  # rect(718819/1000000, 0, 730976/1000000, 180, col = 'yellow', border = NA) # band 1
  # rect(780304/1000000, 0, 796765/1000000, 180, col = 'yellow', border = NA) # band 2
  
  abline(h=0, col='lightgray')
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSF$col <- colfuncR(500)[as.numeric(cut((1/tableSF$alpha)*1000, breaks = 500))]
  
  tableSF$LR <- lapply(tableSF$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSF$location/1000000, tableSF$LR, type = 'p', pch=19, cex= 0.8, col = tableSF$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
  
  mtext('SF LR', 2, col = 'black', line=2.5, cex=2)
  
  axis(2,seq(0,1000,by=200),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  
  # diploSHIC
  
  # plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # for(d in 1:nrow(tableDiplo)){
  #   # if(as.character(ple$predClass[e]) == 'neutral'){
  #   #   rect(ple$classifiedWinStart[e],0,ple$classifiedWinEnd[e],1, col = adjustcolor('yellow', alpha.f = ple$prob.neutral.[e]), border = NA)
  #   # }
  #   if(as.character(tableDiplo$predClass[d]) == 'hard'){
  #     rect(tableDiplo$classifiedWinStart[d]/1000000,0.5,tableDiplo$classifiedWinEnd[d]/1000000,1, col = adjustcolor('black', alpha.f = tableDiplo$prob.hard.[d]), border = NA)
  #   }
  #   if(as.character(tableDiplo$predClass[d]) == 'soft'){
  #     rect(tableDiplo$classifiedWinStart[d]/1000000,0.5,tableDiplo$classifiedWinEnd[d]/1000000,1, col = adjustcolor('pink', alpha.f = tableDiplo$prob.soft.[d]), border = NA)
  #   }
  #   if(as.character(tableDiplo$predClass[d]) == 'linkedHard'){
  #     rect(tableDiplo$classifiedWinStart[d]/1000000,0,tableDiplo$classifiedWinEnd[d]/1000000,0.5, col = adjustcolor('black', alpha.f = tableDiplo$prob.linkedHard.[d]), border = NA)
  #   }
  #   if(as.character(tableDiplo$predClass[d]) == 'linkedSoft'){
  #     rect(tableDiplo$classifiedWinStart[d]/1000000,0,tableDiplo$classifiedWinEnd[d]/1000000,0.5, col = adjustcolor('pink', alpha.f = tableDiplo$prob.likedSoft.[d]), border = NA)
  #   }
  # }
  # mtext('diploSHIC',2, cex =2)
  
  axis(1,seq(0,3,by=0.1),line=1,col="gray",col.ticks="gray",col.axis="black", cex.axis=2)
  
  mtext('Position (Mb)', 1, col = 'black', line=5, cex=2)
  
  dev.off()
}
