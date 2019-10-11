filesSF_Hmel204017 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel204017', full.names = T)
filesSF_Hmel206006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel206006', full.names = T)
filesSF_Hmel208051 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel208051', full.names = T)
filesSF_Hmel219003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE/', pattern='Hmel219003', full.names = T)



pops <- c('melP','melC','melG','nanN','nanS',
          'mer','vic','malC','malE','ecu','ple','xen','ama',
          'ros','cyt','vul',
          'weyGus','weyWey','cyd','zel','chi','pac',
          'lin','heu','flo','timVic','tspn','timCon','timTim','the',
          'ele','bes')

names <- c('H. m. melpomene P','H. m. melpomene C','H. m. melpomene FG','H. m. nanna N', 'H. m. nanna S',
           'H. m. meriana','H. m. vicina','H. m. malleti C','H. m. malleti E', 'H. m. ecuadoriensis','H. m. plesseni','H. m. xenoclea','H. m. amaryllis',
           'H. m. rosina','H. m. cythera','H. m. vulcanus',
           'H. c. weymeri gustavi','H. c. weymeri weymeri','H. c. cydnides','H. c. zelinde','H. c. chioneus','H. c. pachinus',
           'H. t. linaresi','H. heurippa','H. t. florencia','H. t. sp-nov_col','H. t. sp-nov_ecu','H. t. contigua','H. t. timareta','H. t. thelxinoe',
           'H. elevatus', 'H. besckei')

popsSF <- c('MmelP','MmelC','MmelG','Mnan_all_NORTH','Mnan_all_SOUTH',
            'Mmer','Mvic','MmalC','MmalE','Mecu','Mple','Mxen','Mama',
            'Mros','Mcyt','Mvul',
            'Cweyg','Cweyw','Ccyd','Czel','Cchi','PAC',
            'Tlin','HEU','Tflo','Tvic','TspnE','Ttimc','Ttimt','Tthe',
            'ELE','BES')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'hotpink2','hotpink2','hotpink2',
               'mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen',
               'steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2',
               'orange','orange')


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

pdf('SF_BGSC_260919.pdf', width=18, height=19)

layout(matrix(c(1:140), nrow=35, byrow=TRUE), widths=c(rep(c(0.25,0.25,0.25,0.25),35)))
# layout.show(n=140)

(2.36-1.85)/(2.36-1.85+0.8-0.29+1.14-0.63+5.73-5.22) 
(0.8-0.29)/(2.36-1.85+0.8-0.29+1.14-0.63+5.73-5.22)
(1.14-0.63)/(2.36-1.85+0.8-0.29+1.14-0.63+5.73-5.22)
(5.73-5.22)/(2.36-1.85+0.8-0.29+1.14-0.63+5.73-5.22)

par(mar=c(0.1,0,0.1,0), oma=c(0,0,0,2))




start = 1.85
end = 2.36

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel204017.gff",sep="\t")
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

start = 0.29
end = 0.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel206006.gff",sep="\t")
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

start = 0.63
end = 1.14

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel208051.gff",sep="\t")
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

start = 5.22
end = 5.73

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel219003.gff",sep="\t")
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
  
  start = 1.85
  end = 2.36
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  rect(1.82,0,1.85,1, col = colorList[e], border = colorList[e])
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHmel204017$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel204017$alpha)*1000, breaks = 500))]
  
  tableSFHmel204017$LR <- lapply(tableSFHmel204017$LR, function(x) ifelse(x > 1000, 1000, x))
  
  text(1.85,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  par(new=T)
  plot(tableSFHmel204017$location/1000000, tableSFHmel204017$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel204017$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)

  subT <- subset(tableSFHmel204017, tableSFHmel204017$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 0.29
  end = 0.8
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(0.27,0,0.29,1, col = colorList[e], border = colorList[e])
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHmel206006$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel206006$alpha)*1000, breaks = 500))]
  
  tableSFHmel206006$LR <- lapply(tableSFHmel206006$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel206006$location/1000000, tableSFHmel206006$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel206006$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel206006, tableSFHmel206006$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 0.63
  end = 1.14
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(0.6,0,0.63,1, col = colorList[e], border = colorList[e])
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHmel208051$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel208051$alpha)*1000, breaks = 500))]
  
  tableSFHmel208051$LR <- lapply(tableSFHmel208051$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel208051$location/1000000, tableSFHmel208051$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel208051$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel208051, tableSFHmel208051$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  start = 5.22
  end = 5.73
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(5.19,0,5.22,1, col = colorList[e], border = colorList[e])
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(500)
  tableSFHmel219003$col <- colfuncR(500)[as.numeric(cut((1/tableSFHmel219003$alpha)*1000, breaks = 500))]
  
  tableSFHmel219003$LR <- lapply(tableSFHmel219003$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel219003$location/1000000, tableSFHmel219003$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel219003$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel219003, tableSFHmel219003$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  
}

start = 1.85
end = 2.36

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, pos = 1)


start = 0.29
end = 0.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, pos = 1)

start = 0.63
end = 1.14

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, pos = 1)

start = 5.22
end = 5.73

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="gray",col.ticks="gray",col.axis="black", cex.axis=1, pos = 1)




dev.off()






