filesSF_Herato1001 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1001', full.names = T)
filesSF_Herato1505 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1505', full.names = T)
filesSF_Herato1801 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1801', full.names = T)

filesSF_Herato0411 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0411', full.names = T)
filesSF_Herato0601 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0601', full.names = T)
filesSF_Herato0821 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato0821', full.names = T)
filesSF_Herato1901 <- list.files(path='SF_Markus/NEW_FILES/ERATO/POLSUBALLPOL_75_OM1_LG50_BGSFS_RECRATE', pattern='Herato1901', full.names = T)

pops <- c('')

names <- c('H. e. amalfreda', 'H. e. erato', 'H. e. hydara (French Guiana)', 'H. e. hydara (Panama)', 'H. e. demophoon', 'H. e. emma',
           'H. e. etylus', 'H. e. lativitta', 'H. e. notabilis', 'H. e. favorinus', 'H. e. venus', 'H. e. cyrbia', 'H. e. chestertonii', 'H. himera')

popsSF <- c('amalfreda', 'erato.OM', 'hydaraFG', 'hydaraP', 'demophoon', 'emma',
            'etylus', 'lativitta', 'notabilis', 'favorinus', 'venus', 'cyrbia', 'chestertonii', 'himera')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','mediumseagreen','steelblue2')


bgListErato <- c()

for(e in 1:length(popsSF)){
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

pdf('SF_all_erato_neutral.pdf', width=15, height=15)
layout(matrix(c(1:64), nrow=16, byrow=TRUE), widths=c(rep(c(0.2890173,0.2890173,0.2890173,0.132948),16)))
# layout.show(n=64)

1/(3+0.46)
1/(3+0.46)
1/(3+0.46)
0.46/(3+0.46)

par(mar=c(0.1,0,0.1,0), oma=c(0,0,0,2))



start = 4
end = 5

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato0411.gff",sep="\t")
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



start = 0.8
end = 1.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato0601.gff",sep="\t")
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


start = 2
end = 3

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato0821.gff",sep="\t")
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

start = 3
end = 3.46

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Herato1901.gff",sep="\t")
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



for(e in 1:length(popsSF)){
  
  
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
  
  
  start = 4
  end = 5
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  rect(3.8,0,4,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHerato0411 <- subset(tableSFHerato0411, tableSFHerato0411$location > start*1000000 & tableSFHerato0411$location < end*1000000)
  tableSFHerato0411$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato0411$alpha)*1000, breaks = 500))]
  
  tableSFHerato0411$LR <- lapply(tableSFHerato0411$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  text(start +0.02,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato0411$location/1000000, tableSFHerato0411$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato0411$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato0411, tableSFHerato0411$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 0.8
  end = 1.8
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHerato0601 <- subset(tableSFHerato0601, tableSFHerato0601$location > start*1000000 & tableSFHerato0601$location < end*1000000)
  tableSFHerato0601$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato0601$alpha)*1000, breaks = 500))]
  
  tableSFHerato0601$LR <- lapply(tableSFHerato0601$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  # text(3.5,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato0601$location/1000000, tableSFHerato0601$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato0601$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato0601, tableSFHerato0601$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 2
  end = 3
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHerato0821 <- subset(tableSFHerato0821, tableSFHerato0821$location > start*1000000 & tableSFHerato0821$location < end*1000000)
  tableSFHerato0821$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato0821$alpha)*1000, breaks = 500))]
  
  tableSFHerato0821$LR <- lapply(tableSFHerato0821$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  # text(3.5,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato0821$location/1000000, tableSFHerato0821$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato0821$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato0821, tableSFHerato0821$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  
  start = 3
  end = 3.46
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHerato1901 <- subset(tableSFHerato1901, tableSFHerato1901$location > start*1000000 & tableSFHerato1901$location < end*1000000)
  tableSFHerato1901$col <- colfuncR(500)[as.numeric(cut((1/tableSFHerato1901$alpha)*1000, breaks = 500))]
  
  tableSFHerato1901$LR <- lapply(tableSFHerato1901$LR, function(x) ifelse(x > 1000, 1000, x))
  
  
  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }
  
  # text(3.5,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHerato1901$location/1000000, tableSFHerato1901$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHerato1901$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHerato1901, tableSFHerato1901$LR >= bgListErato[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  # start = 1.35
  # end = 2.2
  # 
  # plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # 
  # 
  # # rect(1848666/1000000, 0, 1858224/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  # 
  # 
  # # for(d in 1:nrow(tableSFHmel210004)){
  # #   rect(tableSFHmel210004$location[d]/1000000-250/1000000,0,tableSFHmel210004$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel210004$LR[d])), border = NA)
  # # }
  # 
  # colfuncR <- colorRampPalette(c("black", "red"))
  # colL <- colfuncR(500)
  # tableSFHmel210004$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel210004$alpha)*1000, breaks = 500))]
  # 
  # tableSFHmel210004$LR <- lapply(tableSFHmel210004$LR, function(x) ifelse(x > 1000, 1000, x))
  # 
  # par(new=T)
  # plot(tableSFHmel210004$location/1000000, tableSFHmel210004$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel210004$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
  # 
  # # text(1.35,800, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  # 
  # start = 0.55
  # end = 1.95
  # 
  # plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # # rect(start,0,end,1, col = NA, border = colorList[e])
  # 
  # # rect(1205164/1000000, 0, 1324501/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  # 
  # # for(d in 1:nrow(tableSFHmel215006)){
  # #   rect(tableSFHmel215006$location[d]/1000000-250/1000000,0,tableSFHmel215006$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel215006$LR[d])), border = NA)
  # # }
  # 
  # colfuncR <- colorRampPalette(c("black", "red"))
  # colL <- colfuncR(500)
  # tableSFHmel215006$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel215006$alpha)*1000, breaks = 500))]
  # 
  # tableSFHmel215006$LR <- lapply(tableSFHmel215006$LR, function(x) ifelse(x > 1000, 1000, x))
  # 
  # par(new=T)
  # plot(tableSFHmel215006$location/1000000, tableSFHmel215006$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel215006$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
  # 
  # start = 0.5
  # end = 1.2
  # 
  # plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # # rect(start,0,end,1, col = NA, border = colorList[e])
  # 
  # 
  # 
  # # rect(705604/1000000, 0, 706407/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  # 
  # # rect(814176/1000000, 0, 820339/1000000, 1, col = 'blue', border = NA) # dennis
  # # rect(773272/1000000, 0, 787408/1000000, 1, col = 'green', border = NA) # ray
  # # rect(718819/1000000, 0, 730976/1000000, 1, col = 'yellow', border = NA) # band 1
  # # rect(780304/1000000, 0, 796765/1000000, 1, col = 'yellow', border = NA) # band 2
  # 
  # # 
  # # for(d in 1:nrow(tableSFHmel218003)){
  # #   rect(tableSFHmel218003$location[d]/1000000-250/1000000,0,tableSFHmel218003$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel218003$LR[d])), border = NA)
  # # }
  # 
  # colfuncR <- colorRampPalette(c("black", "red"))
  # colL <- colfuncR(500)
  # tableSFHmel218003$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel218003$alpha)*1000, breaks = 500))]
  # 
  # tableSFHmel218003$LR <- lapply(tableSFHmel218003$LR, function(x) ifelse(x > 1000, 1000, x))
  # 
  # par(new=T)
  # plot(tableSFHmel218003$location/1000000, tableSFHmel218003$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel218003$col, axes = FALSE, ylim = c(0,1000), xlim = c(start,end),ann=FALSE)
  # 
  # axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  # 
}

start = 4
end = 5

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)


start = 0.8
end = 1.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

start = 2
end = 3

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)


start = 3
end = 3.46

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

# start = 0.5
# end = 1.2
# 
# plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
# 
# axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)
# 

dev.off()








