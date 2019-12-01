
library(matrixStats)

##
minor.ticks.axis <- function(ax,n,t.ratio=0.5,mn,mx,...){
  
  lims <- par("usr")
  if(ax %in%c(1,3)) lims <- lims[1:2] else lims[3:4]
  
  major.ticks <- pretty(lims,n=5)
  if(missing(mn)) mn <- min(major.ticks)
  if(missing(mx)) mx <- max(major.ticks)
  
  major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
  
  labels <- sapply(major.ticks,function(i)
    as.expression(bquote(10^ .(i)))
  )
  axis(ax,at=major.ticks,labels=labels,...)
  
  n <- n+2
  minors <- log10(pretty(10^major.ticks[1:2],n))-major.ticks[1]
  minors <- minors[-c(1,n)]
  
  minor.ticks = c(outer(minors,major.ticks,`+`))
  minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
  
  
  axis(ax,at=minor.ticks,tcl=par("tcl")*t.ratio,labels=FALSE)
}

####
# per pop

chi=c('chi.CAM25091','chi.CAM25137','chi.CAM580','chi.CAM582','chi.CAM585','chi.CAM586','chi.CJ553','chi.CJ560','chi.CJ564','chi.CJ565')
cor=c('cor.CS3','cor.CS4','cor.STRI7')
zel=c('zel.CS1','zel.CS1028','zel.CS1029','zel.CS1030','zel.CS1033','zel.CS1035','zel.CS2','zel.CS2262','zel.CS273','zel.CS30')
agl=c('agl.JM108','agl.JM112','agl.JM569','agl.JM572')
ama=c('ama.JM160','ama.JM216','ama.JM293','ama.JM48','ama.MJ11-3188','ama.MJ11-3189','ama.MJ11-3202','ama.MJ12-3217','ama.MJ12-3258','ama.MJ12-3301')
mal=c('mal.CS1002','mal.CS1011','mal.CS1815','mal.CS21','mal.CS22','mal.CS24','mal.CS586','mal.CS594','mal.CS604','mal.CS615')
melC=c('melC.CS25','melC.CS26','melC.CS27','melC.CS3','melC.CS6')
melG=c('melG.CAM1349','melG.CAM1422','melG.CAM2035','melG.CAM8171','melG.CAM8216','melG.CAM8218','melG.CJ13435','melG.CJ9315','melG.CJ9316','melG.CJ9317')
melP=c('melP.CJ18038','melP.CJ18097')
nan=c('nan.MK14','nan.MK62','nan.MK63','nan.MK64')
ple=c('malPle.CJ16042','ple.CJ16293','ple.CJ9156')
ros=c('ros.CAM1841','ros.CAM1880','ros.CAM2045','ros.CAM2059','ros.CAM2519','ros.CAM2552','ros.CJ2071','ros.CJ531','ros.CJ533','ros.CJ546')
vul=c('vul.CS10','vul.CS3603','vul.CS3605','vul.CS3606','vul.CS3612','vul.CS3614','vul.CS3615','vul.CS3617','vul.CS3618','vul.CS3621')
flo=c('flo.CS12','flo.CS13','flo.CS14','flo.CS15','flo.CS2337','flo.CS2338','flo.CS2341','flo.CS2350','flo.CS2358','flo.CS2359')
thx=c('thxn.JM313','thxn.JM57','thxn.JM84','thxn.JM86','thxn.MJ12-3221','thxn.MJ12-3233','thxn.MJ12-3308','txn.MJ11-3339','txn.MJ11-3340','txn.MJ11-3460')
heu=c('heu.CS20','heu.CS9','heu.STRI2')

pac=c('pacERR1143614','pacERR1143613')
mer=c('merERR1143604','merERR1143603')
ele=c('eleERR1143575','eleERR1143574','eleERR1143573')
bes=c('besSRR3102131','besSRR3102061','besSRR3102026','besSRR3102011')

pops=list(agl,ama,mal,ple,nan,melC,melG,melP,ros,vul,mer,cor,chi,zel,pac,flo,thx,heu,ele,bes)
popNames=c('Heliconius m. aglaope',
'Heliconius m. amaryllis',
'Heliconius m. malleti',
'Heliconius m. plesseni',
'Heliconius m. nanna',
'Heliconius m. melpomene 
(Colombia)',
'Heliconius m. melpomene 
(French Guiana)',
'Heliconius m. melpomene 
(Panama)',
'Heliconius m. rosina',
'Heliconius m. vulcanus',
'Heliconius m. meriana',
'Heliconius c. cordula',
'Heliconius c. chioneus',
'Heliconius c. zelinde',
'Heliconius pachinus',
'Heliconius t. florencia',
'Heliconius t. thelxinoe',
'Heliconius heurippa',
'Heliconius elevatus', 
'Heliconius besckei')
color = c('hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2',
          'mediumseagreen','mediumseagreen','mediumseagreen','mediumseagreen',
          'steelblue2','steelblue2','steelblue2',
          'orange','orange')
linetype=c(1,1,1,1,1,1,1,2,2,3,4,
           1,2,3,
           1,1,1,2,
           1,2)


par(mfrow=c(4,5),mai=c(0.05,0,0.05,0.05), oma=c(5,5,1,3)+0)

layout(matrix(c(1:20), ncol=4)) 


for(i in 1:length(pops)){
  plot(0, pch = "", ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE,  xlim=c(5000,2000000),ylim=c(5,8))
  
  for(name in pops[[i]]){
    fileN <- read.table(paste(name,'.final.Rin.txt', sep=""),h=T)
    par(new=TRUE)
    plot(fileN$leftX, log10(fileN$popSize), lty=linetype[[i]], type = "S", col=color[[i]],yaxt='n',xaxt='n',ylab="",xlab="",log="x",  xlim=c(5000,2000000),ylim=c(5,8))
    text(100000,7.6,substitute(paste(italic(nn)), list(nn=popNames[i])))
    
    }
  if(i%%5 ==0){
    at.x <- outer(1:9, 10^(0:7))
    lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
    axis(1, at=at.x, labels=lab.x, las=1)
    mtext(side = 1, text = expression(paste("Years (g= 0.25, ?= 2 x 10"^"  -9",")")), cex=0.7, line=3) 
  }
  if(i<=5){
    minor.ticks.axis(2,9,mn=0,mx=8)
    mtext(side = 2, text = expression(paste("Ne")), cex=0.7, line =3) 
  }
}
#axis(1, at=seq(0,1000000,by=100000))

# mtext(side = 1, text = expression(paste("Years (g= 0.25, ?= 2 x 10"^"  -9",")")), cex=1, line=3) 
# mtext(side = 2, text = expression(paste("Effective population size")), cex=1, line =3) 
# 
# at.x <- outer(1:9, 10^(0:7))
# lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
# axis(1, at=at.x, labels=lab.x, las=1)
# 
# axis(2, at=seq(0,20000000,by=1000000))
# 
# 
# legend(450000,8000000, c(expression(italic("H. melpomene")), expression(italic("H. timareta")), expression(italic("H. cydno"))),col=c("red","blue","green"),lty=c(1,1,1),bty = "n")
# 




# per species

cyd=c('chi.CAM25091','chi.CAM25137','chi.CAM580','chi.CAM582','chi.CAM585','chi.CAM586','chi.CJ553','chi.CJ560','chi.CJ564','chi.CJ565','cor.CS3','cor.CS4','cor.STRI7','zel.CS1','zel.CS1028','zel.CS1029','zel.CS1030','zel.CS1033','zel.CS1035','zel.CS2','zel.CS2262','zel.CS273','zel.CS30')
cydE=c('cor.CS3','cor.CS4','cor.STRI7')
cydW=c('zel.CS1','zel.CS1028','zel.CS1029','zel.CS1030','zel.CS1033','zel.CS1035','zel.CS2','zel.CS2262','zel.CS273','zel.CS30')
cydP=c('chi.CAM25091','chi.CAM25137','chi.CAM580','chi.CAM582','chi.CAM585','chi.CAM586','chi.CJ553','chi.CJ560','chi.CJ564','chi.CJ565')
cydWP=c('zel.CS1','zel.CS1028','zel.CS1029','zel.CS1030','zel.CS1033','zel.CS1035','zel.CS2','zel.CS2262','zel.CS273','zel.CS30','chi.CAM25091','chi.CAM25137','chi.CAM580','chi.CAM582','chi.CAM585','chi.CAM586','chi.CJ553','chi.CJ560','chi.CJ564','chi.CJ565')
tim=c('flo.CS12','flo.CS13','flo.CS14','flo.CS15','flo.CS2337','flo.CS2338','flo.CS2341','flo.CS2350','flo.CS2358','flo.CS2359','thxn.JM313','thxn.JM57','thxn.JM84','thxn.JM86','thxn.MJ12-3221','thxn.MJ12-3233','thxn.MJ12-3308','txn.MJ11-3339','txn.MJ11-3340','txn.MJ11-3460')

mel=c('agl.JM108','agl.JM112','agl.JM569','agl.JM572','ama.JM160','ama.JM216','ama.JM293','ama.JM48','ama.MJ11-3188','ama.MJ11-3189','ama.MJ11-3202','ama.MJ12-3217','ama.MJ12-3258','ama.MJ12-3301','mal.CS1002','mal.CS1011','mal.CS1815','mal.CS21','mal.CS22','mal.CS24','mal.CS586','mal.CS594','mal.CS604','mal.CS615','melC.CS25','melC.CS26','melC.CS27','melC.CS3','melC.CS6','melG.CAM1349','melG.CAM1422','melG.CAM2035','melG.CAM8171','melG.CAM8216','melG.CAM8218','melG.CJ13435','melG.CJ9315','melG.CJ9316','melG.CJ9317','melP.CJ18038','melP.CJ18097','nan.MK14','nan.MK62','nan.MK63','nan.MK64','malPle.CJ16042','ple.CJ16293','ple.CJ9156','ros.CAM1841','ros.CAM1880','ros.CAM2045','ros.CAM2059','ros.CAM2519','ros.CAM2552','ros.CJ2071','ros.CJ531','ros.CJ533','ros.CJ546','vul.CS10','vul.CS3603','vul.CS3605','vul.CS3606','vul.CS3612','vul.CS3614','vul.CS3615','vul.CS3617','vul.CS3618','vul.CS3621')
melW=c('vul.CS10','vul.CS3603','vul.CS3605','vul.CS3606','vul.CS3612','vul.CS3614','vul.CS3615','vul.CS3617','vul.CS3618','vul.CS3621')
melP=c('melP.CJ18038','melP.CJ18097','ros.CAM1841','ros.CAM1880','ros.CAM2045','ros.CAM2059','ros.CAM2519','ros.CAM2552','ros.CJ2071','ros.CJ531','ros.CJ533','ros.CJ546')
melWP=c('vul.CS10','vul.CS3603','vul.CS3605','vul.CS3606','vul.CS3612','vul.CS3614','vul.CS3615','vul.CS3617','vul.CS3618','vul.CS3621','melP.CJ18038','melP.CJ18097','ros.CAM1841','ros.CAM1880','ros.CAM2045','ros.CAM2059','ros.CAM2519','ros.CAM2552','ros.CJ2071','ros.CJ531','ros.CJ533','ros.CJ546')
melE=c('agl.JM108','agl.JM112','agl.JM569','agl.JM572','ama.JM160','ama.JM216','ama.JM293','ama.JM48','ama.MJ11-3188','ama.MJ11-3189','ama.MJ11-3202','ama.MJ12-3217','ama.MJ12-3258','ama.MJ12-3301','mal.CS1002','mal.CS1011','mal.CS1815','mal.CS21','mal.CS22','mal.CS24','mal.CS586','mal.CS594','mal.CS604','mal.CS615','malPle.CJ16042','ple.CJ16293','ple.CJ9156')
melFG=c('merERR1143604','merERR1143603','melG.CAM1349','melG.CAM1422','melG.CAM2035','melG.CAM8171','melG.CAM8216','melG.CAM8218','melG.CJ13435','melG.CJ9315','melG.CJ9316','melG.CJ9317')
melC=c('melC.CS25','melC.CS26','melC.CS27','melC.CS3','melC.CS6')
meln=c('nan.MK14','nan.MK62','nan.MK63','nan.MK64')
heu=c('heu.CS20','heu.CS9','heu.STRI2')

pac=c('pacERR1143614','pacERR1143613')
ele=c('eleERR1143575','eleERR1143574','eleERR1143573')
bes=c('besSRR3102131','besSRR3102061','besSRR3102026','besSRR3102011')

races = list(cydE,cydWP,pac,tim,heu,melE,melWP,melFG,melC,meln,ele,bes)
races = list(melE)
races = list(tim)
races = list(pac)
races = list(melFG)
races = list(c(melWP,melC))
raceN = c("cydE",'cydWP','pac',"tim","heu","melE",'melWP','melFG','melC','meln','heu')
color = c('mediumseagreen','mediumseagreen','mediumseagreen','steelblue2','steelblue2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','orange','orange')
linetype = c(1,2,3,1,2,1,2,3,4,5,1,2)

par(mfrow=c(1,1),mai=c(0.5,1.2,0.5,0.8), oma=c(2,0,1,0)+0)
plot(0, pch = "", xlim=c(5000,2000000),ylim=c(5,8),ylab = "", yaxt = "n", lwd = 0.5, xlab = "", xaxt = "n", bty = "n", main = "",axes = FALSE,log="x")

for(i in 1:length(races)){
  Popsize <- c()
  time <- c()
  for(name in races[[i]]){
    fileN <- read.table(paste(name,'.final.Rin.txt', sep=""),h=T)
    time <- cbind(time,fileN$leftX)
    Popsize <- cbind(Popsize, fileN$popSize)
    #par(new=TRUE)
    #plot(fileN$leftX, fileN$popSize, type = "S", col=color[[i]],yaxt='n',xaxt='n',ylab="",xlab="",log="x",lwd=1, xlim=c(5000,2000000),ylim=c(500000,9000000))
    
  }
  x2 <- rowSds(time)
  y2 <- rowSds(Popsize) / sqrt(nrow(Popsize))  
  x <- rowMeans(time)
  y <- rowMeans(Popsize) 
  for(l in 1:length(x2)){
    rect(x[l]+40,log10(y[l+1]-1.95*y2[l+1]),x[l+1]-40,log10(y[l+1]+1.96*y2[l+1]), col =adjustcolor(color[[i]], alpha=0.3), lwd = 0, border = NA)
    
  }
  
  
  
  par(new=TRUE)
  plot(x, log10(y), type = "S", lty=linetype[[i]], col=color[[i]],yaxt='n',xaxt='n',ylab="",xlab="",log="x",lwd=2, xlim=c(5000,2000000),ylim=c(5,8))
  
}

as.data.frame(cbind(x,y))



mtext(side = 1, text = expression(paste("Years (g= 0.25, µ= 2 x 10"^"  -9",")")), cex=1, line=3) 
mtext(side = 2, text = expression(paste("Effective population size")), cex=1, line =3) 

at.x <- outer(1:9, 10^(0:7))
lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
axis(1, at=at.x, labels=lab.x, las=1)

# at.x <- outer(1:9, 10^(0:8))
# lab.x <- ifelse(log10(at.x) %% 1 == 0, at.x, NA)
# axis(2, at=at.x, labels=lab.x, las=2)
# minor.ticks.axis(1,9,mn=4,mx=8)
minor.ticks.axis(2,9,mn=0,mx=8)


legend(150000,8, c(expression(italic("H. cydno (East)")), 
                          expression(italic("H. cydno (Panama/West)")), 
                   expression(italic("H. pachinus")),
                          expression(italic("H. timareta")),
                   expression(italic("H. heurippa")),
                          expression(italic("H. melpomene (East)")),
                          expression(italic("H. melpomene (Panama/West)")),
                   expression(italic("H. melpomene (FG)")),
                   expression(italic("H. melpomene (C)")),
                   expression(italic("H. melpomene nanna")),
                   expression(italic("H. elevatus")),
                   expression(italic("H. besckei"))),
                   
       col=c('mediumseagreen','mediumseagreen','mediumseagreen','steelblue2','steelblue2','hotpink2','hotpink2','hotpink2','hotpink2','hotpink2','orange','orange'),lty=c(1,2,3,1,2,1,2,3,4,5,1,2),bty = "n",lwd=2)


