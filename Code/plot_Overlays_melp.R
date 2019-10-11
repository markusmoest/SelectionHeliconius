rm(list=ls())
library("scales")


####select dataset####
setwd("/home/moestmar/Dropbox/CAPTURE_Steven/SF_Markus/NEW_FILES")
list.files()
data1 <- ("POLONLY_50_LRG100_BGSFS_RECRATE")                                                
data2 <- ("POLONLY_75_OM1_LRG50_BGSFS_RECRATE")
data3 <- ("POLONLY_75_OM1_NOBGSFS_NORECRATE")
data4 <- ("POLONLY_75_OM3_LRG50_BGSFS_RECRATE")
data5 <- ("POLONLY_75_OM3_SG50_NOBGSFS_NORECRATE")                                 
data6 <- ("POLSUBALLPOL_75_OM1_LRG50_BGSFS_RECRATE")                    
data7 <- ("POLSUBALLPOL_75_OM1_SG50_NOBGSFS_NORECRATE")
data8 <- ("POLSUBALLPOL_75_OM3_LRG50_BGSFS_RECRATE") 
data9 <- ("POLSUBALLPOL_75_OM3_SG50_NOBGSFS_NORECRATE")
data10 <- ("POLSUBALLPOL_75_OM1_LRG50_BGSFSwALL_RECRATE")


####choose data####
data <- data6
run <- 'data6' 
####choose method####
meth <- 'CLR' #'scaledCLR'
setwd(paste0(getwd(),"/",data))
files <- list.files()
files
####extract unique IDs from folder####
uniqIDs <- unique(unlist(lapply(files,function(x) unlist(strsplit(x,split = 'noINDEL'))[1])))
uniqIDs[1:9]


###Select dataset###

mel <- c("Magl_filtered_","Mama_all_filtered_","Mbur_C_filtered_","Mcyt_C_filtered_","Mecu_C_filtered_","MmalC_all_CLUSTER_filtered_","MmalE_all_filtered_", "MmelC_all_filtered_","MmelG_HQ1_filtered_","MmelP_all_filtered_","Mmer_C_HQ_filtered_","Mnan_all_NORTH_filtered_","Mnan_all_SOUTH_filtered_","Mple_all_filtered_","Mros_filtered_","Mvic_C_filtered_","Mvul_filtered_", "Mxen_C_filtered_")

cyd <- c("Cchi_filtered_", "Ccor_filtered_", "Ccyd_C_filtered_", "Cweyg_C_filtered_", "Cweyw_C_filtered_", "Czel_filtered_","PAC_C_HQ_filtered_") 

tim <- c("Tflo_all_filtered_", "Tlin_C_filtered_", "TspnE_C_filtered_","Tthe_all_filtered_", "Ttimc_C_filtered_", "Ttimt_C_filtered_", "Tvic_C_filtered_", "HEU_all_filtered_") 

sil <- c("ELE_C_Ecu_filtered_", "BES_C_filtered_")

allmelclade <- c("Magl_filtered_","Mama_all_filtered_","Mbur_C_filtered_","Mcyt_C_filtered_","Mecu_C_filtered_","MmalC_all_CLUSTER_filtered_","MmalE_all_filtered_", "MmelC_all_filtered_","MmelG_HQ1_filtered_","MmelP_all_filtered_","Mmer_C_HQ_filtered_","Mnan_all_NORTH_filtered_","Mnan_all_SOUTH_filtered_","Mple_all_filtered_","Mros_filtered_","Mvic_C_filtered_","Mvul_filtered_", "Mxen_C_filtered_", "Cchi_filtered_", "Ccor_filtered_", "Ccyd_C_filtered_", "Cweyg_C_filtered_", "Cweyw_C_filtered_", "Czel_filtered_","PAC_C_HQ_filtered_", "Tflo_all_filtered_", "Tlin_C_filtered_", "TspnE_C_filtered_","Tthe_all_filtered_", "Ttimc_C_filtered_", "Ttimt_C_filtered_", "Tvic_C_filtered_", "HEU_all_filtered_", "ELE_C_Ecu_filtered_", "BES_C_filtered_")

allmelcladeHQ <- c("Mama_all_filtered_","Mcyt_C_filtered_","Mecu_C_filtered_","MmalC_all_CLUSTER_filtered_","MmalE_all_filtered_", "MmelC_all_filtered_","MmelG_HQ1_filtered_","MmelP_all_filtered_","Mmer_C_HQ_filtered_","Mnan_all_NORTH_filtered_","Mnan_all_SOUTH_filtered_","Mple_all_filtered_","Mros_filtered_","Mvul_filtered_", "Mxen_C_filtered_", "Cchi_filtered_", "Ccor_filtered_", "Ccyd_C_filtered_", "Cweyg_C_filtered_", "Cweyw_C_filtered_", "Czel_filtered_","PAC_C_HQ_filtered_", "Tflo_all_filtered_", "Tlin_C_filtered_", "TspnE_C_filtered_","Tthe_all_filtered_", "Ttimc_C_filtered_", "Ttimt_C_filtered_", "Tvic_C_filtered_", "HEU_all_filtered_", "ELE_C_Ecu_filtered_", "BES_C_filtered_")

melWEST <- c("Mcyt_C_filtered_","MmelP_all_filtered_","Mros_filtered_","Mvul_filtered_")

melEAST <- c("Mama_all_filtered_","Mecu_C_filtered_","MmalC_all_CLUSTER_filtered_","MmalE_all_filtered_", "MmelC_all_filtered_","MmelG_HQ1_filtered_","Mmer_C_HQ_filtered_","Mnan_all_NORTH_filtered_","Mnan_all_SOUTH_filtered_","Mple_all_filtered_")

IDs <- allmelcladeHQ
#IDs <- melWEST
####define scaffolds####

scaffs <- data.frame(scaffold=c("Hmel201011", "Hmel210004","Hmel215006","Hmel218003"), start=c(2476100, 1378000, 560500, 509700), end=c(2790900, 2198000, 1931800, 1194700)) 
#png("Overlays_mel.png", width=2048, height=1024)
svg("Overlays_mel.svg", width=14, height=8, pointsize = 12)
###set layout###
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), widths=c(2,2,2,2), heights = c(1,1,1,1))
## show the regions that have been allocated to each plot
par(mar=c(5, 5, 3, 2) + 0.1, mex = 0.7)

for (i in 1:4){
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
plot(NULL, type="n", xlab="Scaffold Position [Mb]", ylab=ylab, xlim=c(scaf$start/10^6,scaf$end/10^6), ylim=c(0,(ylim[2]+2*annoffset)), bty="l", xaxt="n", cex.lab=cex, cex.axis=cex, yaxs="i")

####add annotations####
if (scaf$scaffold == "Hmel201011") {
  axis(side=1, at=c(2.50,2.55,2.60,2.65,2.70,2.75,2.80), cex.axis=cex)
  ##plot annotation
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel201011.gff",sep="\t")
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
  
  ##plot aristaless
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel201011_aristaless.gff",sep="\t")
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
  
  rect(2508177/10^6,0,2515380/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("1",at = 2508177/10^6, cex = cex_n)
  #aristaless 2 Scaffold 
  rect(2589268/10^6,0,2594604/10^6,ylim[2], col=alpha("yellow",0.2), border=alpha("yellow",0.5))
  mtext("2",at = 2589268/10^6, cex = cex_n)
  # aristaless 1  
  rect(2610000/10^6,0,2622000/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("3",at = 2610000/10^6, cex = cex_n)
  #aristaless CRE from Erica Westerman
  #rect(2600000/10^6,0,2630000/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  #mtext("3",at = 2610000/10^6, cex = cex_n)
  #aristaless CRE Twisst Steven2600000-2630000
  
  mtext("A) aristaless",side = 3,adj=0, line =1, cex = cex_n*1.5)
}

if (scaf$scaffold == "Hmel210004") {
  axis(side=1, at=c(1.40, 1.50, 1.60, 1.70, 1.80, 1.90, 2.00, 2.10, 2.20),cex.axis=cex)
  ##plot annotation
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel210004.gff",sep="\t")
  names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2]+ annoffset, ANNOT$con_end[g]/10^6, ylim[2] + annoffset, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  ##plot wntA
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel210004_wntA_fixed.txt",sep="\t")
  names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2]+ annoffset, ANNOT$con_end[g]/10^6, ylim[2] + annoffset, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+ 2*annoffset, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  rect(1794660/10^6,0,1858224/10^6,ylim[2], col=alpha("yellow",0.2), border=alpha("yellow",0.5))
  mtext("4",at = 1794660/10^6, cex = cex_n) #wntA, incl new annotated signal peptide
  
  #rect(1794660/10^6,0,1794722/10^6,ylim[2], col=alpha("darkgreen",0.9), border=alpha("darkgreen",0.9)) #wntA, signal peptide...not discussed in the current MS
  rect(1806000/10^6,0,1833000/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1))
  mtext("5",at = 1806000/10^6, cex = cex_n) #Steven's Twisst results
  
  mtext("B) WntA",side = 3, line = 1, adj=0, cex = cex_n*1.5)  
}

if (scaf$scaffold == "Hmel215006") {
  axis(side=1, at=c(0.60,0.70,0.80,0.90,1.00,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80,1.90), cex.axis=cex)
  ##plot annotation
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel215006.gff",sep="\t")
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
  
  ##plot cortex
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel215006_cortex.gff",sep="\t")
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
  
  rect(984339/10^6,0,995970/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("6",at = 984339/10^6, cex = cex_n) # HM00002
  
  rect(1036281/10^6,0,1048598/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("7",at = 1036281/10^6, cex = cex_n) # HM00008
  
  rect(1059385/10^6,0,1081123/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("8",at = 1059385/10^6, cex = cex_n) # CG2519
  
  rect(1122045/10^6,0,1123538/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("9",at = 1122045/10^6, cex = cex_n) # BmSuc2
  
  rect(1205164/10^6,0,1324501/10^6,ylim[2], col=alpha("yellow",0.2), border=alpha("yellow",0.5)) 
  mtext("10",line=2, at = 1205164/10^6, cex = cex_n) # cortex
  
  rect(1193549/10^6,0,1195549/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("11",at = 1193549/10^6, cex = cex_n) # downstream cortex/ dorsal topology
  
  rect(1217149/10^6,0,1219149/10^6,ylim[2], col=alpha("blue",0.2), border=alpha("red",0.2))
  mtext("12", line = 1, at = 1217149/10^6, cex = cex_n) # upstream cortex/ ventral topology
  
  rect(1323223/10^6,0,1338526/10^6,ylim[2], col=alpha("red",0.2), border=alpha("red",0.2))
  mtext("13", line = 1, at = 1323223/10^6, cex = cex_n)# strongest FW association Nadeau et al.
  
  rect(1363618/10^6,0,1387381/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("14",at = 1363618/10^6, cex = cex_n) # parn
  
  rect(1399178/10^6,0,1405923/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("15", at = 1399178/10^6, cex = cex_n) # HM00031
  
  rect(1407551/10^6,0,1418070/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("16", line=2, at = 1407551/10^6, cex = cex_n) # Zinc phosphodiesterase
  
  rect(1418342/10^6,0,1464802/10^6,ylim[2], col=alpha("green",0.1), border=alpha("green",0.3))
  mtext("17", line=1, at = 1418342/10^6, cex = cex_n) # LMTK1
  
  rect(1467797/10^6,0,1475759/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("18",at = 1467797/10^6, cex = cex_n) # WDr13
  
  rect(1476102/10^6,0,1478091/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("19",line=2, at = 1476102/10^6, cex = cex_n) # truncated domeless-hit, that's the one in Nicola's paper
  
  rect(1483975/10^6,0,1509990/10^6,ylim[2], col=alpha("green",0.1), border=alpha("green",0.3)) 
  mtext("20",line=1, at = 1483975/10^6, cex = cex_n) # wash
  
  rect(1507269/10^6,0,1514386/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("21",line=2, at = 1507269/10^6, cex = cex_n) # domeless - could be involved but has no (published) evidence
  
  rect(1515034/10^6,0,1520608/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8)) 
  mtext("22",at = 1515034/10^6, cex = cex_n) # lethal(2) - could be involved but has no (published) evidence 
  
  rect(1596367/10^6,0,1667453/10^6,ylim[2], col=alpha("lightblue",0.4), border=alpha("lightblue",0.8))
  mtext("23",at = 1596367/10^6, cex = cex_n) # HM000052

  ##These associations would also be interesting - coords are BAC-walc coords and would need to be transformed. Steven did that for the most important FW associations and we show it now  
##HW associations: 457083, 439063, 602131, 457056   
##FW associations: 584465, 584418,584633, 603344   
  mtext("C) cortex",side = 3, line = 1,adj = 0, cex = cex_n*1.5)
  }

if (scaf$scaffold == "Hmel218003") {
  axis(side=1, at=c(0.50, 0.60, 0.70, 0.80, 0.90, 1.00, 1.10, 1.20), cex.axis=cex)
  ##plot annotation
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel218003.gff",sep="\t")
  names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+2*annoffset, density = NULL, angle = 45, col = "black", border = "black", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  ##plot annotation
  ANNOT <- read.table("/home/moestmar/Dropbox/CAPTURE_Steven/gff/Hmel218003_optix.gff",sep="\t")
  names(ANNOT) <- c("contig", "HGC", "type", "con_start", "con_end", "dot", "str", "unk", "descr")
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "gene" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2]+annoffset, ANNOT$con_end[g]/10^6, ylim[2]+annoffset, density = NULL, angle = 45, col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  for (g in 1:nrow(ANNOT)){
    if (ANNOT$type[g] == "exon" & ANNOT$con_start[g]/10^6 >= scaf$start/10^6 & ANNOT$con_end[g]/10^6 <= scaf$end/10^6){
      rect(ANNOT$con_start[g]/10^6, ylim[2], ANNOT$con_end[g]/10^6, ylim[2]+2*annoffset, density = NULL, angle = 45, col = "red", border = "red", lty = par("lty"), lwd = par("lwd"))
    }
  }
  
  rect(705977/10^6,0,706769/10^6,ylim[2], col=alpha("yellow",0.2), border=alpha("yellow",0.5)) 
  mtext("23",at = 705977/10^6, cex = cex_n) #optix
  
  rect(813673/10^6,0,820339/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("27",at = 813673/10^6, cex = cex_n) #dennis, get latest coords, check with Steven
  
  rect(773272/10^6,0,787408/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("25",at = 773272/10^6, cex = cex_n) #rays
  
  rect(718819/10^6,0,730976/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("24",line=1, at = 718819/10^6, cex = cex_n) #band1
  
  rect(780304/10^6,0,796765/10^6,ylim[2], col=alpha("red",0.1), border=alpha("red",0.1)) 
  mtext("26",line = 1, at = 780304/10^6, cex = cex_n) #band2
  
  rect(851049/10^6,0,862925/10^6,ylim[2], col=alpha("green",0.1), border=alpha("green",0.3))
  mtext("28",at = 851049/10^6, cex = cex_n)
  #kinesin
  
  
  #rect(890729/10^6,0,892076/10^6,1, col="red") #bves - not interesting
  
  mtext("D) optix",side = 3, line = 1, adj = 0, cex = cex_n*1.5)
  }

####loop over all pops and plot them on top of each other####
for (popnum in 1:length(IDs)){

####select population####
pop <- IDs[popnum]

print(paste0(pop,".*",scaf$scaffold))
file <- dir(pattern=paste0(pop,".*",scaf$scaffold))
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










