### Packages ----
library(RColorBrewer)
library(formattable)
library(ggplot2)
library(ggrepel)
library(strex)
library(reshape2)
library(wesanderson)
library(ggridges)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
library(cowplot)
library(htmltools)
library(plotly)
library(ggiraph)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)
library("rnaturalearth")
library(sf)
library(ggsn)
library(rgeos)
library(plotly)
library(png)
library(stringr)
library(DT)
library("readxl")
library(ggridges)
library(leaflet)
library(R.utils)
library(png)
library(readxl)
color_med_atl=data.frame(Location=c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
                         Col=brewer.pal(n = 4, name = "RdBu"))
setwd("C:/Users/divco/Documents/COGEDIV/")

### Fig 6 ----

lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
lfh$Fec=as.numeric(lfh$Fec)
lfh=as.data.frame(lfh)
lfh$Fec=log(lfh$Fec)
lfh$Propagule_Size=log(lfh$Propagule_Size)
for (i in c(5:11,14)){
  lfh[,i]=as.numeric(as.vector(lfh[,i]))
}

lfh=lfh[,c(3,seq(5,14))]

## Genetic
fst<-as.data.frame(read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx"))
fst<-fst[,c(2,seq(9,14))]
fst<-fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]

for (i in 2:ncol(fst)){
  fst[,i]=as.numeric(fst[,i])
}

load(file="data/dxy_all.Rdata")
dxy=dxy_data
#dxy=dxy[,seq(1,19)]

for (i in 2:ncol(dxy)){
  dxy[,i]=as.numeric(dxy[,i])
}
colnames(dxy)[1]="Species_code"
merge_data=merge(fst,dxy,on=c("Species_code"))

abc<-read.csv("data/data_ABC.csv",header=T,sep=",")
abc=abc[abc$Method=="Random forest" & abc$Migr=="Beta" & abc$Format=="Li_Ga",]
abc=abc[,c(1,3,5)]
#abc$Estimate=as.numeric(abc$Estimate)
#abc$SP=as.character(abc$SP)
#abc$Param=as.character(abc$Param)
abc=as.data.frame(dcast(abc, SP ~ Param))
colnames(abc)[1]="Species_code"
for (i in 2:5){
  for (j in 1:nrow(abc)){
    if (is.na(abc[j,i])==T){
      
      abc[j,i]=0
      
    }
    
  }
  
}
merge_data=merge(merge_data,abc[,c(1,2:10,21)],on=c("Species_code"))
##
#lfh$Species[1]="Atherina boyeri"


for (i in 1:nrow(lfh)){
  if (lfh$Parental_Care[i]=="Yes"){
    lfh$Parental_Care[i]=1
  } else if (lfh$Parental_Care[i]=="No"){
    lfh$Parental_Care[i]=0
  }
  if (lfh$Hermaphrodism[i]=="Yes"){
    lfh$Hermaphrodism[i]=1
  } else if (lfh$Hermaphrodism[i]=="No"){
    lfh$Hermaphrodism[i]=0
  }
}

##

lfh$Parental_Care=as.numeric(lfh$Parental_Care)
lfh$Hermaphrodism=as.numeric(lfh$Hermaphrodism)

introgress_in_med=c(0,0,0,0,1,0,1,1,0,1,0,1,1,0,0,0,0)
introgress_in_atl=c(0,0,0,1,1,0,1,0,0,0,0,0,0,0,0,0,0)

D_in_med=c(NA,NA,NA,1,NA,0,1,NA,NA,1,NA,1,1,1,1,0,0)
D_in_atl=c(NA,NA,NA,1,NA,0,0,NA,NA,0,NA,1,0,1,0,0,1)

proba_ongoing_in=c(1,0,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0)
proba_ongoing_out=c(1,0,1,1,0,1,1,0,1,1,1,1,0,0,1,1,0)


merge_data$introgress_in_med=as.numeric(introgress_in_med)
merge_data$introgress_in_atl=as.numeric(introgress_in_atl)
merge_data$D_in_med=as.numeric(D_in_med)
merge_data$D_in_atl=as.numeric(D_in_atl)
merge_data$proba_ongoing_in=as.numeric(proba_ongoing_in)
merge_data$proba_ongoing_out=as.numeric(proba_ongoing_out)
library(vegan)

#for (i in 2:ncol(lfh)){
#  lfh[,i]=scale(lfh[,i])
#}
#for (i in 2:ncol(merge_data)){
#  merge_data[,i]=scale(merge_data[,i])
#}


rda1 <- rda (merge_data[-c(1,25:34,37:38,39:40)] ~ Body_Size + Trophic_Level + Propagule_Size + Age_Mat + Lifespan + Adult_Lifespan + PLD  + Hermaphrodism , data = lfh[-c(2,7),],scale=T)  # calculate tb-RDA with two explanatory variables

summary(rda1)
anova(rda1, permutations = 100000)
anova(rda1,by="margin", permutations = 100000)
vif.cca(rda1)
mod <- step(rda1, scope = formula(rda1), test = "perm",permutations=10000)

rda1 <- rda (merge_data[-c(1,25:34,37:38,39:40)] ~ PLD + Hermaphrodism + Lifespan, data = lfh[-c(2,7),],scale=T)  # calculate tb-RDA with two explanatory variables
summary(rda1)
anova(rda1, permutations = 100000)
anova(rda1,by="margin", permutations = 100000)
vif.cca(rda1)
mod <- step(rda1, scope = formula(rda1), test = "perm",permutations=10000)

smry <- summary(rda1)
df1  <- data.frame(smry$sites[,1:2])
df1$FST=merge_data$FST_LI_GA_WEIGHTED# PC1 and PC2
df1$Parental_Care=lfh[-c(2,7),]$Parental_Care# PC1 and PC2
df2  <- data.frame(smry$species[,1:2])
df2  <- data.frame(smry$biplot[,1:2])
rownames(df2)<-c("PLD","H","L")
label=italic_species=c("italic('A. boyeri')",
                       #"italic('A. fallax')",
                       "italic('C. galerita')",
                       "italic('C. julis')",
                       "italic('D. labrax')",
                       "italic('D. puntazzo')",
                       #"italic('E. encrasicolus')",
                       #"italic('G. niger')",
                       "italic('H. guttulatus')",
                       "italic('L. budegassa')",
                       "italic('L. mormyrus')",
                       "italic('M. merluccius')",
                       "italic('M. surmuletus')",
                       "italic('P. erythrinus')",
                       "italic('S. cabrilla')",
                       "italic('S. cantharus')",
                       "italic('S. cinereus')",
                       "italic('S. pilchardus')",
                       "italic('S. sarda')",
                       "italic('S. typhle')")
df1$Parental_Care=factor(df1$Parental_Care)
rda.plot <- ggplot(df1, aes(x=RDA1, y=RDA2)) + 
  geom_point(size=2,alpha=0.75,aes(col=FST,shape=Parental_Care)) +
  #geom_text(data=df1,label=merge_data$Species_code,size=4) +
  geom_text_repel(label=italic_species,parse=T,col="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  #coord_fixed() +
  theme_classic() +
  xlab(paste("RDA1 (",52.31,"%)",sep="")) +
  ylab(paste("RDA2 (",26.92,"%)",sep="")) +
  scale_shape_manual(name="Parental Care",labels=c("No","Yes"),values = c(16,15))
#rda.plot

rda.biplot_first <- rda.plot +
  geom_segment(data=df2, aes(x=0, xend=RDA1, y=0, yend=RDA2), 
               alpha=0.75,
               color="black", arrow=arrow(length=unit(0.01,"npc"))) +
  geom_text(data=df2, 
            label=rownames(df2),
            aes(x=RDA1,y=RDA2,
                fontface="bold",
                hjust=0.5*(1-sign(RDA2)),vjust=0.5*(1-sign(RDA2))), 
            color="black", size=3.5,
            alpha=0.75,
            check_overlap = T) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust = 0.5)) +
  labs(title="All species")

#pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig5.pdf",sep=""),width=7.5,height=7.5)
#plot(rda.biplot)
#dev.off()

#jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig5.jpg",sep=""),width=7.5,height=7.5,units="in",res=2000)
#plot(rda.biplot)
#dev.off()


rda1 <- rda (merge_data[-c(1,25:34,37:38,39:40)][lfh[-c(2,7),]$Parental_Care==0,] ~ Body_Size + Trophic_Level + Propagule_Size + Age_Mat + Lifespan + Adult_Lifespan + PLD  + Hermaphrodism , data = lfh[-c(2,7),][lfh[-c(2,7),]$Parental_Care==0,],scale=T)  # calculate tb-RDA with two explanatory variables

summary(rda1)
anova(rda1, permutations = 100000)
anova(rda1,by="margin", permutations = 100000)
vif.cca(rda1)
mod <- step(rda1, scope = formula(rda1), test = "perm",permutations=10000)

rda1 <- rda (merge_data[-c(1,25:34,37:38,39:40)][lfh[-c(2,7),]$Parental_Care==0,] ~ Body_Size + Trophic_Level + Propagule_Size + Age_Mat + Lifespan  + PLD  + Hermaphrodism, data = lfh[-c(2,7),][lfh[-c(2,7),]$Parental_Care==0,],scale=T)  # calculate tb-RDA with two explanatory variables
summary(rda1)
anova(rda1, permutations = 100000)
anova(rda1,by="margin", permutations = 100000)
vif.cca(rda1)
mod <- step(rda1, scope = formula(rda1), test = "perm",permutations=10000)

smry <- summary(rda1)
df1  <- data.frame(smry$sites[,1:2])
df1$FST=merge_data[lfh[-c(2,7),]$Parental_Care==0,]$FST_LI_GA_WEIGHTED# PC1 and PC2
df1$Parental_Care=lfh[-c(2,7),][lfh[-c(2,7),]$Parental_Care==0,]$Parental_Care# PC1 and PC2
df2  <- data.frame(smry$species[,1:2])
df2  <- data.frame(smry$biplot[,1:2])
row.names(df2)<-c("BS","TL","PS","AM","L","PLD","H")
label=italic_species=c(
  "italic('A. boyeri')",
  #"italic('A. fallax')",
  #"italic('C. galerita')",
  "italic('C. julis')",
  "italic('D. labrax')",
  "italic('D. puntazzo')",
  #"italic('E. encrasicolus')",
  #"italic('G. niger')",
  #"italic('H. guttulatus')",
  "italic('L. budegassa')",
  "italic('L. mormyrus')",
  "italic('M. merluccius')",
  "italic('M. surmuletus')",
  "italic('P. erythrinus')",
  "italic('S. cabrilla')",
  #"italic('S. cantharus')",
  #"italic('S. cinereus')",
  "italic('S. pilchardus')",
  "italic('S. sarda')"
  #"italic('S. typhle')"
)
df1$Parental_Care=factor(df1$Parental_Care,levels=c("0","1"),labels=c("0","1"))
rda.plot <- ggplot(df1, aes(x=RDA1, y=RDA2)) + 
  geom_point(size=2,alpha=0.75,aes(col=FST)) +
  #geom_text(data=df1,label=merge_data$Species_code,size=4) +
  geom_text_repel(label=italic_species,parse=T,col="black") +
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  #coord_fixed() +
  theme_classic() +
  xlab(paste("RDA1 (",50.07,"%)",sep="")) +
  ylab(paste("RDA2 (",18.62,"%)",sep="")) #rda.plot

df2$var=rownames(df2)
rda.biplot_second <- rda.plot +
  geom_segment(data=df2, aes(x=0, xend=RDA1, y=0, yend=RDA2), 
               alpha=0.75,
               arrow=arrow(length=unit(0.01,"npc"))) +
  geom_text(data=df2, 
            label=rownames(df2),
            aes(x=RDA1,y=RDA2,
                hjust=0.5*(1-sign(RDA2)),vjust=0.5*(1-sign(RDA2))), 
            color="black", size=3.5, fontface="bold",
            alpha=0.75,
            check_overlap = T) +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust = 0.5),
        legend.position = "None") +
  labs(title="Only no parental care species") 

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig5.pdf",sep=""),width=12.5,height=7.5)
print(ggpubr::ggarrange(rda.biplot_first,rda.biplot_second,
                        legend="bottom",
                        #common.legend = T,
                        labels=c("A","B"),
                        nrow=1,ncol=2))
#nrow=1,ncol=2))
#nrow=1,ncol=2,
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig5.jpg",sep=""),width=12.5,height=7.5,units="in",res=2000)
print(ggpubr::ggarrange(rda.biplot_first,rda.biplot_second,
                        legend="bottom",
                        #common.legend = T,
                        labels=c("A","B"),
                        nrow=1,ncol=2))
#nrow=1,ncol=2))
#nrow=1,ncol=2,
dev.off()
