filesSF_Hmel210004 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel210004', full.names = T)
filesSF_Hmel215006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel215006', full.names = T)
filesSF_Hmel218003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel218003', full.names = T)

filesSF_Hmel204017 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel204017', full.names = T)
filesSF_Hmel206006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel206006', full.names = T)
filesSF_Hmel208051 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel208051', full.names = T)
filesSF_Hmel219003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel219003', full.names = T)

pops <- c('ros','melP','mer','vul','cyt','malE','ecu','ama','ple')

names <- c('H. m. rosina','H. m. melpomene (Panama)','H. m. meriana','H. m. vulcanus','H. m. cythera','H. m. malleti (Ecuador)','H. m. ecuadoriensis','H. m. amaryllis','H. m. plesseni')

popsSF <- c('Mros','MmelP','Mmer','Mvul','Mcyt','MmalE','Mecu','Mama','Mple')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2')

# pops <- c('ple')
# 
# names <- c('H. m. plesseni')
# 
# popsSF <- c('Mple')
# 
# colorList <- c('hotpink2')

#####
# Make list of 99th percentie bg scaffolds
#####

bgList <- c()

for(e in 1:length(popsSF)){
  print(popsSF[e])
  
  indxSFHmel204017 <- grep(popsSF[e], filesSF_Hmel204017)
  indxSFHmel206006 <- grep(popsSF[e], filesSF_Hmel206006)
  indxSFHmel208051 <- grep(popsSF[e], filesSF_Hmel208051)
  indxSFHmel219003 <- grep(popsSF[e], filesSF_Hmel219003)
  
  tableSFHmel204017 <- read.table(filesSF_Hmel204017[indxSFHmel204017], h=T, stringsAsFactors=F)
  tableSFHmel204017 <- as.data.frame(t(sapply(split(tableSFHmel204017, as.integer(gl(nrow(tableSFHmel204017), 10, nrow(tableSFHmel204017)))), colMeans, na.rm = TRUE)))
  tableSFHmel206006 <- read.table(filesSF_Hmel206006[indxSFHmel206006], h=T, stringsAsFactors=F)
  tableSFHmel206006 <- as.data.frame(t(sapply(split(tableSFHmel206006, as.integer(gl(nrow(tableSFHmel206006), 10, nrow(tableSFHmel206006)))), colMeans, na.rm = TRUE)))
  tableSFHmel208051 <- read.table(filesSF_Hmel208051[indxSFHmel208051], h=T, stringsAsFactors=F)
  tableSFHmel208051 <- as.data.frame(t(sapply(split(tableSFHmel208051, as.integer(gl(nrow(tableSFHmel208051), 10, nrow(tableSFHmel208051)))), colMeans, na.rm = TRUE)))
  tableSFHmel219003 <- read.table(filesSF_Hmel219003[indxSFHmel219003], h=T, stringsAsFactors=F)
  tableSFHmel219003 <- as.data.frame(t(sapply(split(tableSFHmel219003, as.integer(gl(nrow(tableSFHmel219003), 10, nrow(tableSFHmel219003)))), colMeans, na.rm = TRUE)))
  
  tableConcat <- rbind(tableSFHmel204017,tableSFHmel206006,tableSFHmel208051,tableSFHmel219003)
  
  bgList <- c(bgList, quantile(tableConcat$LR, c(.999)) )
}

bgList <- as.vector(bgList)

##################################################################################
layout(matrix(c(1:69), nrow=23, byrow=TRUE), widths=c(rep(c(0.2881356,0.4745763,0.2372881),3)), heights=c(1,rep(c(2),9),1,2,1,rep(c(2),9),1))
layout.show(n=69)

# layout(matrix(c(1:21), nrow=7, byrow=TRUE), widths=c(rep(c(0.2881356,0.4745763,0.2372881),3)), heights=c(1,rep(c(2),1),1,2,1,rep(c(2),1),1))
# layout.show(n=21)

(2.2-1.35)/(2.2-1.35+1.95-0.55+1.2-0.5)
(1.95-0.55)/(2.2-1.35+1.95-0.55+1.2-0.5)
(1.2-0.5)/(2.2-1.35+1.95-0.55+1.2-0.5)

par(mar=c(0.1,0,0.1,0), oma=c(2,0,2,2))
##################################################################################

start = 1.35
end = 2.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(3,seq(start,end,by=0.1),line=0,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 0)

start = 0.55
end = 1.95

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(3,seq(start,end,by=0.1),line=0,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 0)

start = 0.5
end = 1.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(3,seq(start,end,by=0.1),line=0,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 0)

for(e in 1:length(popsSF)){
  
  
  print(popsSF[e])
  
  # indxSFHmel201011 <- grep(popsSF[e], filesSF_Hmel201011)
  indxSFHmel210004 <- grep(popsSF[e], filesSF_Hmel210004)
  indxSFHmel215006 <- grep(popsSF[e], filesSF_Hmel215006)
  indxSFHmel218003 <- grep(popsSF[e], filesSF_Hmel218003)
  
  # tableSFHmel201011 <- read.table(filesSF_Hmel201011[indxSFHmel201011], h=T, stringsAsFactors=F)
  # tableSFHmel201011 <- as.data.frame(t(sapply(split(tableSFHmel201011, as.integer(gl(nrow(tableSFHmel201011), 10, nrow(tableSFHmel201011)))), colMeans, na.rm = TRUE)))
  tableSFHmel210004 <- read.table(filesSF_Hmel210004[indxSFHmel210004], h=T, stringsAsFactors=F)
  tableSFHmel210004 <- as.data.frame(t(sapply(split(tableSFHmel210004, as.integer(gl(nrow(tableSFHmel210004), 10, nrow(tableSFHmel210004)))), colMeans, na.rm = TRUE)))
  tableSFHmel215006 <- read.table(filesSF_Hmel215006[indxSFHmel215006], h=T, stringsAsFactors=F)
  tableSFHmel215006 <- as.data.frame(t(sapply(split(tableSFHmel215006, as.integer(gl(nrow(tableSFHmel215006), 10, nrow(tableSFHmel215006)))), colMeans, na.rm = TRUE)))
  tableSFHmel218003 <- read.table(filesSF_Hmel218003[indxSFHmel218003], h=T, stringsAsFactors=F)
  tableSFHmel218003 <- as.data.frame(t(sapply(split(tableSFHmel218003, as.integer(gl(nrow(tableSFHmel218003), 10, nrow(tableSFHmel218003)))), colMeans, na.rm = TRUE)))
  

  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])

  
  start = 1.35
  end = 2.2
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  
  
  # rect(1848666/1000000, 0, 1858224/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  rect((1806000)/1000000, 0, (1833000)/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # split
  
  
  # for(d in 1:nrow(tableSFHmel210004)){
  #   rect(tableSFHmel210004$location[d]/1000000-250/1000000,0,tableSFHmel210004$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel210004$LR[d])), border = NA)
  # }
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel210004$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel210004$alpha)*1000, breaks = 500))]
  tableSFHmel210004$alpha <- sapply(((1/as.numeric(as.character(tableSFHmel210004$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel210004$col <-colfuncR(201)[as.integer((tableSFHmel210004$alpha)*1000)+1]
  
  tableSFHmel210004$LR <- lapply(tableSFHmel210004$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel210004$location/1000000, tableSFHmel210004$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel210004$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel210004, tableSFHmel210004$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  

  text(1.35,800, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  start = 0.55
  end = 1.95
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(1205164/1000000, 0, 1324501/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  rect((1193549-2000)/1000000, 0, (1195549+4000)/1000000, 1, col = 'gray85', border = NA) # cr1
  rect((1217149-2000)/1000000, 0, (1219149+4000)/1000000, 1, col = 'gray85', border = NA) # cr2
  
  rect((1323223)/1000000, 0, (1338526)/1000000, 1, col = 'gray85', border = NA) # FW
  # for(d in 1:nrow(tableSFHmel215006)){
  #   rect(tableSFHmel215006$location[d]/1000000-250/1000000,0,tableSFHmel215006$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel215006$LR[d])), border = NA)
  # }
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel215006$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel215006$alpha)*1000, breaks = 500))]
  tableSFHmel215006$alpha <- sapply(((1/as.numeric(as.character(tableSFHmel215006$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel215006$col <-colfuncR(201)[as.integer((tableSFHmel215006$alpha)*1000)+1]
  
  tableSFHmel215006$LR <- lapply(tableSFHmel215006$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel215006$location/1000000, tableSFHmel215006$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel215006$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel215006, tableSFHmel215006$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 0.5
  end = 1.2
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  
  
  # rect(705604/1000000, 0, 706407/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  rect(814176/1000000, 0, 820339/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # dennis
  rect(773272/1000000, 0, 787408/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # ray
  rect(718819/1000000, 0, 730976/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # band 1
  rect(780304/1000000, 0, 796765/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # band 2
  
  # 
  # for(d in 1:nrow(tableSFHmel218003)){
  #   rect(tableSFHmel218003$location[d]/1000000-250/1000000,0,tableSFHmel218003$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel218003$LR[d])), border = NA)
  # }
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel218003$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel218003$alpha)*1000, breaks = 500))]
  tableSFHmel218003$alpha <- sapply(((1/as.numeric(as.character(tableSFHmel218003$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel218003$col <-colfuncR(201)[as.integer((tableSFHmel218003$alpha)*1000)+1]
  
  tableSFHmel218003$LR <- lapply(tableSFHmel218003$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel218003$location/1000000, tableSFHmel218003$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel218003$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel218003, tableSFHmel218003$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  
}



##################################################################################

# plot annotation melpomene
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

# Plot alignment
alignWntA <- read.table("align_scaffolds/wntA_eratoDem_melp_nucmer.txt", h=F)
colnames(alignWntA) <- c("S1","E1","S2","E2","LEN1","LEN2","IDY","REF","QUE")

alignCortex <- read.table("align_scaffolds/cortex_eratoDem_melp_nucmer.txt", h=F)
colnames(alignCortex) <- c("S1","E1","S2","E2","LEN1","LEN2","IDY","REF","QUE")

alignOptix <- read.table("align_scaffolds/optix_eratoDem_melp_nucmer.txt", h=F)
colnames(alignOptix) <- c("S1","E1","S2","E2","LEN1","LEN2","IDY","REF","QUE")

start = 1.35*1000000
end = 2.2*1000000
start2 = 4.35*1000000 + ((4707387-4.35*1000000 )- (1858244-1.35*1000000))
end2 = 5.2*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))

alignWntAF <- subset(alignWntA, alignWntA$IDY > 80 & alignWntA$LEN1 >200 & alignWntA$S1 > start2 & alignWntA$E1 < end2 & alignWntA$S2 > start & alignWntA$E2 < end)

plot(0, pch = "",xlim = c(0,end-start), ylim = c(0,10), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE)
for(e in 1:nrow(alignWntAF)){
  polygon(c(alignWntAF$S1[e]-start2,alignWntAF$E1[e]-start2,alignWntAF$E2[e]-start,alignWntAF$S2[e]-start), c(0,0,10,10), col = adjustcolor('black', alpha.f = 0.5), border=F)
}

start = 0.55*1000000
end = 1.95*1000000
start2 = 1.4*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
end2 = 2.8*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))

alignCortexF <- subset(alignCortex, alignCortex$IDY > 80 & alignCortex$LEN1 >200 & alignCortex$S1 > start2 & alignCortex$E1 < end2 & alignCortex$S2 > start & alignCortex$E2 < end)

plot(0, pch = "",xlim = c(0,end-start), ylim = c(0,10), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE)
for(e in 1:nrow(alignCortexF)){
  polygon(c(alignCortexF$S1[e]-start2,alignCortexF$E1[e]-start2,alignCortexF$E2[e]-start,alignCortexF$S2[e]-start), c(0,0,10,10), col = adjustcolor('black', alpha.f = 0.5), border=F)
}

start = 0.5*1000000
end = 1.2*1000000
start2 = 1*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
end2 = 1.7*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))

alignOptixF <- subset(alignOptix, alignOptix$IDY > 80 & alignOptix$LEN1 >200 & alignOptix$S1 > start2 & alignOptix$E1 < end2 & alignOptix$S2 > start & alignOptix$E2 < end)

plot(0, pch = "",xlim = c(0,end-start), ylim = c(0,10), ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE)
for(e in 1:nrow(alignOptixF)){
  polygon(c(alignOptixF$S1[e]-start2,alignOptixF$E1[e]-start2,alignOptixF$E2[e]-start,alignOptixF$S2[e]-start), c(0,0,10,10), col = adjustcolor('black', alpha.f = 0.5), border=F)
}

# annotation erato
start = 4.35*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
end = 5.2*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato1001.gff",sep="\t")
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
ANNOT <- read.table("gff/Herato1001_wntA.gff",sep="\t")
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

start = 1.4*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
end = 2.8*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato1505.gff",sep="\t")
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
ANNOT <- read.table("gff/Herato1505_cortex_fixed.txt",sep="\t")
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

start = 1*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
end = 1.7*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato1801.gff",sep="\t")
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
ANNOT <- read.table("gff/Herato1801_optix.gff",sep="\t")
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
##################################################################################
filesSF_Herato1001 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1001', full.names = T)
filesSF_Herato1505 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1505', full.names = T)
filesSF_Herato1801 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1801', full.names = T)

filesSF_Herato0411 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0411', full.names = T)
filesSF_Herato0601 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0601', full.names = T)
filesSF_Herato0821 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0821', full.names = T)
filesSF_Herato1901 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1901', full.names = T)


pops <- c('ros','melP','mer','vul','cyt','malE','ecu','ama','ple')

names <- c('H. e. notabilis','H. e. favorinus','H. e. etylus','H. e. lativitta','H. e. cyrbia','H. e. venus','H. e. amalfreda','H. e. hydara (Panama)','H. e. demophoon')

popsSF <- c('notabilis','favorinus','etylus','lativitta','cyrbia','venus','amalfreda','hydaraP','demophoon')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2')

# pops <- c('ros')
# 
# names <- c('H. e. notabilis')
# 
# popsSF <- c('notabilis')
# 
# colorList <- c('hotpink2')
#####
# Make list of 99th percentie bg scaffolds
#####

bgListErato <- c()

for(e in length(popsSF):1){
  print(popsSF[e])
  
  indxSFHerato0411 <- grep(popsSF[e], filesSF_Herato0411)
  indxSFHerato0601 <- grep(popsSF[e], filesSF_Herato0601)
  indxSFHerato0821 <- grep(popsSF[e], filesSF_Herato0821)
  indxSFHerato1901 <- grep(popsSF[e], filesSF_Herato1901)
  
  tableSFHerato0411 <- read.table(filesSF_Herato0411[indxSFHerato0411], h=T, stringsAsFactors=F)
  tableSFHerato0411 <- as.data.frame(t(sapply(split(tableSFHerato0411, as.integer(gl(nrow(tableSFHerato0411), 10, nrow(tableSFHerato0411)))), colMeans, na.rm = TRUE)))
  tableSFHerato0601 <- read.table(filesSF_Herato0601[indxSFHerato0601], h=T, stringsAsFactors=F)
  tableSFHerato0601 <- as.data.frame(t(sapply(split(tableSFHerato0601, as.integer(gl(nrow(tableSFHerato0601), 10, nrow(tableSFHerato0601)))), colMeans, na.rm = TRUE)))
  tableSFHerato0821 <- read.table(filesSF_Herato0821[indxSFHerato0821], h=T, stringsAsFactors=F)
  tableSFHerato0821 <- as.data.frame(t(sapply(split(tableSFHerato0821, as.integer(gl(nrow(tableSFHerato0821), 10, nrow(tableSFHerato0821)))), colMeans, na.rm = TRUE)))
  tableSFHerato1901 <- read.table(filesSF_Herato1901[indxSFHerato1901], h=T, stringsAsFactors=F)
  tableSFHerato1901 <- as.data.frame(t(sapply(split(tableSFHerato1901, as.integer(gl(nrow(tableSFHerato1901), 10, nrow(tableSFHerato1901)))), colMeans, na.rm = TRUE)))
  
  tableConcat <- rbind(tableSFHerato0411,tableSFHerato0601,tableSFHerato0821,tableSFHerato1901)
  
  bgListErato <- c(bgListErato, quantile(tableConcat$LR, c(.999)) )
}

bgListErato <- as.vector(bgListErato)

for(e in length(popsSF):1){
  
  
  print(popsSF[e])
  
  indxSFHerato1001 <- grep(popsSF[e], filesSF_Herato1001)
  indxSFHerato1505 <- grep(popsSF[e], filesSF_Herato1505)
  indxSFHerato1801 <- grep(popsSF[e], filesSF_Herato1801)
  
  tableSFHerato1001 <- read.table(filesSF_Herato1001[indxSFHerato1001], h=T, stringsAsFactors=F)
  tableSFHerato1001 <- as.data.frame(t(sapply(split(tableSFHerato1001, as.integer(gl(nrow(tableSFHerato1001), 10, nrow(tableSFHerato1001)))), colMeans, na.rm = TRUE)))
  tableSFHerato1505 <- read.table(filesSF_Herato1505[indxSFHerato1505], h=T, stringsAsFactors=F)
  tableSFHerato1505 <- as.data.frame(t(sapply(split(tableSFHerato1505, as.integer(gl(nrow(tableSFHerato1505), 10, nrow(tableSFHerato1505)))), colMeans, na.rm = TRUE)))
  tableSFHerato1801 <- read.table(filesSF_Herato1801[indxSFHerato1801], h=T, stringsAsFactors=F)
  tableSFHerato1801 <- as.data.frame(t(sapply(split(tableSFHerato1801, as.integer(gl(nrow(tableSFHerato1801), 10, nrow(tableSFHerato1801)))), colMeans, na.rm = TRUE)))
  
  
  start = 4.35*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
  end = 5.2*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
  start = start/1000000
  end=end/1000000
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(4.1,0,4.2,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  rect(4624122/1000000, 0, 4647331/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # Sd
  rect(4647332/1000000, 0, 4674571/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # St
  rect(4666909/1000000, 0, 4670474/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) #Ly1
  rect(4700932/1000000, 0, 4708441/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) #Ly2
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  tableSFHerato1001 <- subset(tableSFHerato1001, tableSFHerato1001$location > start*1000000 & tableSFHerato1001$location < end*1000000)
  # tableSFHerato1001$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato1001$alpha)*1000, breaks = 500))]
  tableSFHerato1001$alpha <- sapply(((1/(as.numeric(as.character(tableSFHerato1001$alpha*1000000))))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHerato1001$col <-colfuncR(201)[as.integer((tableSFHerato1001$alpha)*1000)+1]
  
  tableSFHerato1001$LR <- lapply(tableSFHerato1001$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  text(start +0.02,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato1001$location/1000000, tableSFHerato1001$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato1001$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato1001, tableSFHerato1001$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 1.4*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
  end = 2.8*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
  start = start/1000000
  end = end/1000000
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  rect(2053037/1000000, 0, 2171230/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # cr1
  rect(2211881/1000000, 0, 2315926/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # cr2
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  tableSFHerato1505 <- subset(tableSFHerato1505, tableSFHerato1505$location > start*1000000 & tableSFHerato1505$location < end*1000000)
  # tableSFHerato1505$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato1505$alpha)*1000, breaks = 500))]
  tableSFHerato1505$alpha <- sapply(((1/(as.numeric(as.character(tableSFHerato1505$alpha*1000000))))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHerato1505$col <-colfuncR(201)[as.integer((tableSFHerato1505$alpha)*1000)+1]
  
  tableSFHerato1505$LR <- lapply(tableSFHerato1505$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  # text(3.5,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato1505$location/1000000, tableSFHerato1505$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato1505$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato1505, tableSFHerato1505$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 1*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
  end = 1.7*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
  start = start/1000000
  end = end/1000000
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  rect(1377801/1000000, 0, 1384841/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # rays
  rect(1412888/1000000, 0, 1419375/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # dennis1
  rect(1422585/1000000, 0, 1428307/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # dennis2
  rect(1403328/1000000, 0, 1403691/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # band1
  rect(1420912/1000000, 0, 1422355/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA) # band2
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  tableSFHerato1801 <- subset(tableSFHerato1801, tableSFHerato1801$location > start*1000000 & tableSFHerato1801$location < end*1000000)
  # tableSFHerato1801$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato1801$alpha)*1000, breaks = 500))]
  tableSFHerato1801$alpha <- sapply(((1/(as.numeric(as.character(tableSFHerato1801$alpha*1000000))))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHerato1801$col <-colfuncR(201)[as.integer((tableSFHerato1801$alpha)*1000)+1]
  
  tableSFHerato1801$LR <- lapply(tableSFHerato1801$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  # text(3.5,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato1801$location/1000000, tableSFHerato1801$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato1801$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato1801, tableSFHerato1801$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  
 }

start = 4.35*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
end = 5.2*1000000 + ((4707387-4.35*1000000) - (1858244-1.35*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(4.2,5,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

start = 1.4*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
end = 2.8*1000000 + ((2074108-1.4*1000000 )- (1205164-0.55*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(1.45,2.8,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

start = 1*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
end = 1.7*1000000 + ((1250582-1*1000000 )- (705604-0.5*1000000))
start = start/1000000
end = end/1000000

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(1.05,1.75,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)