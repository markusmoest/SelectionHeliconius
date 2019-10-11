filesSF_Hmel201011 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel201011', full.names = T)
filesSF_Hmel210004 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel210004', full.names = T)
filesSF_Hmel215006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel215006', full.names = T)
filesSF_Hmel218003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel218003', full.names = T)

filesSF_Hmel204017 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel204017', full.names = T)
filesSF_Hmel206006 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel206006', full.names = T)
filesSF_Hmel208051 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel208051', full.names = T)
filesSF_Hmel219003 <- list.files(path='SF_Markus/NEW_FILES/POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE', pattern='Hmel219003', full.names = T)

pops <- c('melP','melC','melG','nanN','nanS',
          'mer','malC','malE','ecu','ple','xen','ama',
          'ros','cyt','vul',
          'weyGus','weyWey','cyd','zel','chi','pac',
          'lin','heu','flo','timVic','tspn','timCon','timTim','the',
          'ele','bes')

names <- c('H. m. melpomene (Panama)','H. m. melpomene (Colombia)','H. m. melpomene (French Guyana)','H. m. nanna (North)', 'H. m. nanna (South)',
           'H. m. meriana','H. m. malleti (Colombia)','H. m. malleti (Ecuador)', 'H. m. ecuadorensis','H. m. plesseni','H. m. xenoclea','H. m. amaryllis',
           'H. m. rosina','H. m. cythera','H. m. vulcanus',
           'H. c. weymeri gustavi','H. c. weymeri weymeri','H. c. cydnides','H. c. zelinde','H. c. chioneus','H. c. pachinus',
           'H. t. linaresi','H. heurippa','H. t. florencia','H. t. ssp. nov. (Colombia)','H. t. ssp. nov. (Ecuador)','H. t. timareta f. contigua','H. t. timareta f. timareta','H. t. thelxinoe',
           'H. elevatus', 'H. besckei')

popsSF <- c('MmelP','MmelC','MmelG','Mnan_all_NORTH','Mnan_all_SOUTH',
            'Mmer','MmalC','MmalE','Mecu','Mple','Mxen','Mama',
            'Mros','Mcyt','Mvul',
            'Cweyg','Cweyw','Ccyd','Czel','Cchi','PAC',
            'Tlin','HEU','Tflo','Tvic','TspnE','Ttimc','Ttimt','Tthe',
            'ELE','BES')

colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
               'hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
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
#####

# pops <- c('agl','ama','bur','ecu','cyt','malC','malE','melC','melG','melP','mer','nan','ros','vic','vul','ple','xen',
#           'chi','cor','cyd','weyGus','weyWey','zel','pac',
#           'flo','heu','lin','the','timCon','timTim','timVic','tspn',
#           'ele','bes','hec','ism')

# colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
#                'mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen',
#                'steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2',
#                'orange','orange','orange','orange')
# 
# names <- c('H. m. aglaope','H. m. amaryllis','H. m. burchelli','H. m. ecuadoriensis','H. m. cythera','H. m. malleti C','H. m. malleti E','H. m. melpomene C','H. m. melpomene G',
#            'H. m. melpomene P','H. m. meriana','H. m. nanna','H. m. rosina','H. m. vicina','H. m. vulcanus','H. m. plesseni','H. m. xenoclea',
#            'H. c. chioneus','H. c. cordula','H. c. cydnides','H. c. weymeri gustavi','H. c. weymeri weymeri','H. c. zelinde','H. pachinus ',
#            'H. t. florencia','H. heurippa','H. t. linaresi','H. t. thelxinoe','H. t. contigua','H. t. timareta','H. t. XXX','H. t. sp-nov_ecu',
#            'H. elevatus ','H. besckei ', 'H. hecale','H. ismenius') 
# 
# pops <- c('agl','ama','bur','ecu','cyt','malC','malE','melC','melG','melP','mer','nan','ros','vic','vul','ple','xen',
#           'chi','cor','cyd','weyGus','weyWey','zel','pac',
#           'flo','heu','lin','the','timCon','timTim','timVic','tspn',
#           'ele','bes','hec','ism')
# 
# colorList <- c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
#                'mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen',
#                'steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2','steelblue2',
#                'orange','orange')
# 
# names <- c('H. m. aglaope','H. m. amaryllis','H. m. ecuadoriensis','H. m. cythera','H. m. malleti C','H. m. malleti E','H. m. melpomene C','H. m. melpomene G','H. m. melpomene P','H. m. meriana','H. m. nanna','H. m. rosina','H. m. vicina','H. m. vulcanus','H. m. plesseni','H. m. xenoclea',
#            'H. c. chioneus','H. c. cordula','H. c. cydnides','H. c. weymeri gustavi','H. c. weymeri weymeri','H. c. zelinde','H. pachinus ',
#            'H. t. florencia','H. heurippa ','H. t. linaresi','H. t. thelxinoe','H. t. contigua','H. t. timareta','H. t. XXX','H. t. sp-nov_ecu',
#            'H. elevatus ','H. besckei ') 
# 
# popsSF <- c('Magl','Mama','Mecu','Mcyt','MmalC','MmalE','MmelC','MmelG','MmelP','Mmer','Mnan','Mros','Mvic','Mvul','Mple','Mxen',
#             'Cchi','Ccor','Ccyd','Cweyg','Cweyw','Czel','PAC',
#             'Tflo','HEU','Tlin','Tthe','Ttimc','Ttimt','Tvic','TspnE',
#             'ELE','BES')



layout(matrix(c(1:136), nrow=34, byrow=TRUE), widths=c(rep(c(0.1060606,0.2575758,0.4242424,0.2121212),35)))
layout.show(n=136)

par(mar=c(0.1,0,0.1,0), oma=c(0,0,0,2))

(2.8-2.45)/(2.8-2.45+2.2-1.35+1.95-0.55+1.2-0.5) 
(2.2-1.35)/(2.8-2.45+2.2-1.35+1.95-0.55+1.2-0.5)
(1.95-0.55)/(2.8-2.45+2.2-1.35+1.95-0.55+1.2-0.5)
(1.2-0.5)/(2.8-2.45+2.2-1.35+1.95-0.55+1.2-0.5)


###
start = 2.45
end = 2.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect((2600000)/1000000, 0, (2630000)/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA)
# rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

start = 1.35
end = 2.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
# rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))
rect((1806000)/1000000, 0, (1833000)/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # split

start = 0.55
end = 1.95

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect((1193549-2000)/1000000, 0, (1195549+4000)/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # cr1
rect((1217149-2000)/1000000, 0, (1219149+4000)/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # cr2
rect((1323223)/1000000, 0, (1338526)/1000000, 1, col = adjustcolor('gray50',alpha=0.2), border = NA) # FW

# rect(1193549-2000, 0, 1195549+4000, 1, col = adjustcolor('pink', alpha.f = 0.5), border = NA) # cr1
# rect(1217149-2000, 0, 1219149+4000, 1, col = adjustcolor('blue', alpha.f = 0.5), border = NA) # cr2
# rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

start = 0.5
end = 1.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(814176/1000000, 0, 820339/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # dennis
rect(773272/1000000, 0, 787408/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # ray
rect(718819/1000000, 0, 730976/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # band 1
rect(780304/1000000, 0, 796765/1000000, 1, col = adjustcolor('gray50',alpha=0.5), border = NA) # band 2
# rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

###


start = 2.45
end = 2.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
rect(start,0,end,1, col = adjustcolor('gray', 0.5), border = adjustcolor('gray', 0.5))

ANNOT <- read.table("gff/Hmel201011.gff",sep="\t")
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
ANNOT <- read.table("gff/Hmel201011_aristaless.gff",sep="\t")
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

# maxalphas <-c()
# for(e in 1:length(popsSF)){
#   
#   
#   print(popsSF[e])
#   
#   indxSFHmel201011 <- grep(popsSF[e], filesSF_Hmel201011)
#   indxSFHmel210004 <- grep(popsSF[e], filesSF_Hmel210004)
#   indxSFHmel215006 <- grep(popsSF[e], filesSF_Hmel215006)
#   indxSFHmel218003 <- grep(popsSF[e], filesSF_Hmel218003)
#   
#   tableSFHmel201011 <- read.table(filesSF_Hmel201011[indxSFHmel201011], h=T, stringsAsFactors=F)
#   tableSFHmel201011 <- as.data.frame(t(sapply(split(tableSFHmel201011, as.integer(gl(nrow(tableSFHmel201011), 10, nrow(tableSFHmel201011)))), colMeans, na.rm = TRUE)))
#   tableSFHmel210004 <- read.table(filesSF_Hmel210004[indxSFHmel210004], h=T, stringsAsFactors=F)
#   tableSFHmel210004 <- as.data.frame(t(sapply(split(tableSFHmel210004, as.integer(gl(nrow(tableSFHmel210004), 10, nrow(tableSFHmel210004)))), colMeans, na.rm = TRUE)))
#   tableSFHmel215006 <- read.table(filesSF_Hmel215006[indxSFHmel215006], h=T, stringsAsFactors=F)
#   tableSFHmel215006 <- as.data.frame(t(sapply(split(tableSFHmel215006, as.integer(gl(nrow(tableSFHmel215006), 10, nrow(tableSFHmel215006)))), colMeans, na.rm = TRUE)))
#   tableSFHmel218003 <- read.table(filesSF_Hmel218003[indxSFHmel218003], h=T, stringsAsFactors=F)
#   tableSFHmel218003 <- as.data.frame(t(sapply(split(tableSFHmel218003, as.integer(gl(nrow(tableSFHmel218003), 10, nrow(tableSFHmel218003)))), colMeans, na.rm = TRUE)))
# 
#   maxalpha <- max(max(1/tableSFHmel201011$alpha),max(1/tableSFHmel210004$alpha),max(1/tableSFHmel215006$alpha),max(1/tableSFHmel218003$alpha))
#   
#   maxalphas <- c(maxalphas,maxalpha)
#   }
# 
# max(maxalphas)

alphaMAX <- 0.404

for(e in 1:length(popsSF)){

  
  print(popsSF[e])
  
  indxSFHmel201011 <- grep(popsSF[e], filesSF_Hmel201011)
  indxSFHmel210004 <- grep(popsSF[e], filesSF_Hmel210004)
  indxSFHmel215006 <- grep(popsSF[e], filesSF_Hmel215006)
  indxSFHmel218003 <- grep(popsSF[e], filesSF_Hmel218003)

  tableSFHmel201011 <- read.table(filesSF_Hmel201011[indxSFHmel201011], h=T, stringsAsFactors=F)
  tableSFHmel201011 <- as.data.frame(t(sapply(split(tableSFHmel201011, as.integer(gl(nrow(tableSFHmel201011), 10, nrow(tableSFHmel201011)))), colMeans, na.rm = TRUE)))
  tableSFHmel210004 <- read.table(filesSF_Hmel210004[indxSFHmel210004], h=T, stringsAsFactors=F)
  tableSFHmel210004 <- as.data.frame(t(sapply(split(tableSFHmel210004, as.integer(gl(nrow(tableSFHmel210004), 10, nrow(tableSFHmel210004)))), colMeans, na.rm = TRUE)))
  tableSFHmel215006 <- read.table(filesSF_Hmel215006[indxSFHmel215006], h=T, stringsAsFactors=F)
  tableSFHmel215006 <- as.data.frame(t(sapply(split(tableSFHmel215006, as.integer(gl(nrow(tableSFHmel215006), 10, nrow(tableSFHmel215006)))), colMeans, na.rm = TRUE)))
  tableSFHmel218003 <- read.table(filesSF_Hmel218003[indxSFHmel218003], h=T, stringsAsFactors=F)
  tableSFHmel218003 <- as.data.frame(t(sapply(split(tableSFHmel218003, as.integer(gl(nrow(tableSFHmel218003), 10, nrow(tableSFHmel218003)))), colMeans, na.rm = TRUE)))

  start = 2.45
  end = 2.8

  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  rect(2.42,0,2.45,1, col = colorList[e], border = colorList[e])
  
  rect((2600000)/1000000, 0, (2630000)/1000000, 1, col = adjustcolor('gray85',alpha=0.5), border = NA)
  # rect(start,0,end,1, col = NA, border = colorList[e])

  # rect(2508177/1000000, 0, 2515380/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel201011$col <- colfuncR(201)[as.numeric(cut((1/tableSFHmel201011$alpha)*1000, breaks = 500))]
  tableSFHmel201011$alpha <- sapply(((1/as.numeric(as.character(tableSFHmel201011$alpha)))), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel201011$col <-colfuncR(201)[as.integer((tableSFHmel201011$alpha)*1000)+1]
  
  tableSFHmel201011$LR <- lapply(tableSFHmel201011$LR, function(x) ifelse(x > 1000, 1000, x))


  # for(d in 1:nrow(tableSFHmel201011)){
  #   rect(tableSFHmel201011$location[d]/1000000-250/1000000,0,tableSFHmel201011$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel201011$LR[d])), border = NA)
  # }

  par(xpd=NA)
  text(2.47,0.8, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)

  par(new=T, xpd=TRUE)
  plot(tableSFHmel201011$location/1000000, tableSFHmel201011$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel201011$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)

  subT <- subset(tableSFHmel201011, tableSFHmel201011$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)

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
  # tableSFHmel210004$col <- colfuncR(201)[as.numeric(cut((1/tableSFHmel210004$alpha)*1000, breaks = 500))]
  tableSFHmel210004$alpha <- sapply((1/as.numeric(tableSFHmel210004$alpha)), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel210004$col <-colfuncR(201)[as.integer((tableSFHmel210004$alpha)*1000)+1]
  
  tableSFHmel210004$LR <- lapply(tableSFHmel210004$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel210004$location/1000000, tableSFHmel210004$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel210004$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel210004, tableSFHmel210004$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  # text(1.35,800, substitute(paste(italic(nn)), list(nn=names[e])), cex=1.5, adj=0)
  
  start = 0.55
  end = 1.95
  
  plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)
  # rect(start,0,end,1, col = NA, border = colorList[e])
  
  # rect(1205164/1000000, 0, 1324501/1000000, 1, col = adjustcolor('gray', 0.5), border = NA)
  rect((1193549-2000)/1000000, 0, (1195549+4000)/1000000, 1000, col = 'gray85', border = NA) # cr1
  rect((1217149-2000)/1000000, 0, (1219149+4000)/1000000, 1000, col = 'gray85', border = NA) # cr2
  rect((1323223)/1000000, 0, (1338526)/1000000, 1000, col = 'gray85', border = NA) # FW
  
  # for(d in 1:nrow(tableSFHmel215006)){
  #   rect(tableSFHmel215006$location[d]/1000000-250/1000000,0,tableSFHmel215006$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel215006$LR[d])), border = NA)
  # }
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel215006$col <- colfuncR(201)[as.numeric(cut((1/tableSFHmel215006$alpha)*1000, breaks = 500))]
  tableSFHmel215006$alpha <- sapply(as.numeric(1/tableSFHmel215006$alpha), function(x) ifelse(x > 0.2, 0.2, x))
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
  
  rect(814176/1000000, 0, 820339/1000000, 1000, col = 'gray85', border = NA) # dennis
  rect(773272/1000000, 0, 787408/1000000, 1000, col = 'gray85', border = NA) # ray
  rect(718819/1000000, 0, 730976/1000000, 1000, col = 'gray85', border = NA) # band 1
  rect(780304/1000000, 0, 796765/1000000, 1000, col = 'gray85', border = NA) # band 2
  
  # 
  # for(d in 1:nrow(tableSFHmel218003)){
  #   rect(tableSFHmel218003$location[d]/1000000-250/1000000,0,tableSFHmel218003$location[d]/1000000+250/1000000,1, col = adjustcolor('black', alpha.f = log(tableSFHmel218003$LR[d])), border = NA)
  # }
  
  colfuncR <- colorRampPalette(c("black", "red"))
  colL <- colfuncR(201)
  # tableSFHmel218003$col <- colfuncR(201)[as.numeric(cut((1/tableSFHmel218003$alpha)*1000, breaks = 500))]
  tableSFHmel218003$alpha <- sapply(as.numeric(1/tableSFHmel218003$alpha), function(x) ifelse(x > 0.2, 0.2, x))
  tableSFHmel218003$col <-colfuncR(201)[as.integer((tableSFHmel218003$alpha)*1000)+1]
  
  tableSFHmel218003$LR <- lapply(tableSFHmel218003$LR, function(x) ifelse(x > 1000, 1000, x))
  
  par(new=T)
  plot(tableSFHmel218003$location/1000000, tableSFHmel218003$LR, type = 'p', pch=19, cex= 0.8, col = tableSFHmel218003$col, axes = FALSE, ylim = c(0,1100), xlim = c(start,end),ann=FALSE)
  
  subT <- subset(tableSFHmel218003, tableSFHmel218003$LR >= bgList[e])
  points(subT$location/1000000, rep(1100, nrow(subT)), pch=15, col="blue", cex=0.5)
  
  axis(4,seq(0,1000,by=250),col="black",col.ticks="black",col.axis="black",labels=F, cex.axis=1)
  
  }

start = 2.45
end = 2.8

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)


start = 1.35
end = 2.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

start = 0.55
end = 1.95

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)

start = 0.5
end = 1.2

plot(NULL, xlim=c(start,end), ylim = c(0,1), axes=FALSE,ann=FALSE)

axis(1,seq(start,end,by=0.1),line=-1,col="black",col.ticks="black",col.axis="black", cex.axis=1, pos = 1)











