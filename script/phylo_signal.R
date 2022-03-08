library(phytools)

setwd("C:/Users/divco/Documents/COGEDIV/")

tree=read.tree("data/all.fa.treefile")
lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
fst<-read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx")

fst<-fst %>% right_join(lfh)

load(file="data/dxy_all.Rdata")
dxy=dxy_data
colnames(dxy)[1]="Species_code"
fst_dxy=merge(as.data.frame(fst),dxy,on=c("Species_code"))

outgroup=c("NA","NA","Lepisosteus_oculatus",rep("NA",ncol(fst_dxy)-3))
fst_dxy=rbind(fst_dxy,outgroup)

fst_dxy<-fst_dxy[match(tree$tip.label, fst_dxy$Species_code),]

## FST LI-GA
a<-phylosig(tree, as.numeric(fst_dxy$FST_LI_GA_WEIGHTED), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$FST_LI_GA_WEIGHTED), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## DXY-Li-Ga
a<-phylosig(tree, as.numeric(fst_dxy$dxy_Li_Ga), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$dxy_Li_Ga), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Da-Li-Ga
a<-phylosig(tree, as.numeric(fst_dxy$da_Li_Ga), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$da_Li_Ga), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Body size
a<-phylosig(tree, as.numeric(fst_dxy$Body_Size), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Body_Size), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## TROPHIC LEVEL
a<-phylosig(tree, as.numeric(fst_dxy$Trophic_Level), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Trophic_Level), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## fec
a<-phylosig(tree, as.numeric(fst_dxy$Fec), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Fec), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Propagule size
a<-phylosig(tree, as.numeric(fst_dxy$Propagule_Size), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Propagule_Size), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Age at maturity
a<-phylosig(tree, as.numeric(fst_dxy$Age_Mat), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Age_Mat), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Lifespan
a<-phylosig(tree, as.numeric(fst_dxy$Lifespan), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Lifespan), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## Adult Lifespan
a<-phylosig(tree, as.numeric(fst_dxy$Adult_Lifespan), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$Adult_Lifespan), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)

## PLD
a<-phylosig(tree, as.numeric(fst_dxy$PLD), method="lambda", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
a<-phylosig(tree, as.numeric(fst_dxy$PLD), method="K", test=T, nsim=10000, se=NULL, start=NULL,
            control=list())
print(a)
