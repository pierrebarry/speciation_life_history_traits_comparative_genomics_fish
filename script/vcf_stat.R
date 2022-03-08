library(RColorBrewer)
library(formattable)
library(ggplot2)
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

labels_species=c()

for (sp in 1:length(levels(factor(VCF_STATS_INDIV$SPECIES)))){
  
  labels_species=c(labels_species,
                   paste(levels(factor(VCF_STATS_INDIV$SPECIES))[sp]," \n n = ",table(VCF_STATS_INDIV$SPECIES)[sp],sep=" "))
  
}

labels_species=as.vector(labels_species)

n_fun <- function(x){
  return(data.frame(y = 1.05*max(VCF_STATS_INDIV$DEPTH), label = paste0(round(median(x),2),"X",sep="")))
}

p3<-ggplot(VCF_STATS_INDIV, aes(x=SPECIES, y=DEPTH))  +
  geom_violin(fill="chartreuse3",col="black",alpha=0.25) +
  geom_point(aes(col=LOCATION),
             size = 1,
             shape=19,
             position='jitter',
             alpha=0.75) +
  stat_summary(fun=median, geom="point", size=2, color="orange")+
  stat_summary(fun.data = n_fun, geom = "text") +
  scale_x_discrete(name="Species",
                   labels=labels_species)+
  scale_colour_manual(name = "Location",
                      labels = c("Gulf of Lion","Murcia","Faro","Gulf of Gascogne"),
                      values = brewer.pal(n = 4, name = "RdBu"))+
  ylab("Depth")+
  xlab("Species")+
  theme_classic()+
  scale_colour_manual(name = "Location",
                      labels = c("Med-out","Med-in","Atl-in","Atl-out"),
                      values = brewer.pal(n = 4, name = "RdBu"))+
  theme(legend.position="bottom")

n_fun <- function(x){
  return(data.frame(y = 1.05*max(VCF_STATS_INDIV$PERCENTAGE_SNPS_SAME_AS_REF), label = paste0(round(median(x),2),"%",sep="")))
}

p4<-ggplot(VCF_STATS_INDIV, aes(x=SPECIES, y=PERCENTAGE_SNPS_SAME_AS_REF))  +
  geom_violin(fill="chartreuse3",col="black",alpha=0.25) +
  geom_point(aes(col=LOCATION),
             size = 1,
             shape=19,
             position='jitter',
             alpha=0.75) +
  stat_summary(fun=median, geom="point", size=2, color="orange")+
  stat_summary(fun.data = n_fun, geom = "text") +
  scale_x_discrete(name="Species",
                   labels=labels_species)+
  scale_colour_manual(name = "Location",
                      labels = c("Med-out","Med-in","Atl-in","Atl-out"),
                      values = brewer.pal(n = 4, name = "RdBu"))+
  ylab("Percentage same as reference (%)")+
  xlab("Species")+
  theme_classic()+
  scale_colour_manual(name = "Location",
                      labels = c("Med-out","Med-in","Atl-in","Atl-out"),
                      values = brewer.pal(n = 4, name = "RdBu"))+
  theme(legend.position="bottom")

data_snps=VCF_SPECIES[[1]]

data_snps$DEPTH=tapply(VCF_STATS_INDIV$DEPTH,VCF_STATS_INDIV$SPECIES,"median")
data_snps=data_snps[,c(1,2,6)]
data_change=melt(data_snps,id.vars=c("SPECIES"))

p1<-ggplot(data_change[data_change$variable=="DEPTH",],aes(x=SPECIES,y=value)) +
  coord_flip() +
  geom_bar(stat='identity',alpha=0.25,fill="black") +
  geom_bar(data=data_change[data_change$SPECIES==i,],
           stat='identity',alpha=0.6,fill="dodgerblue2") +
  scale_fill_viridis_d() +
  #facet_grid(.~variable, scales="free") +
  theme_classic() +
  ylab("Depth") +
  xlab("") +
  geom_text(
    aes(x = SPECIES, y = value, label = paste(round(value,2),"X",sep="")),
    position = position_dodge(width = 1),
    hjust = -0.25, size = 3,col="grey"
  ) +
  geom_text(data=data_change[data_change$variable=="DEPTH",],
            aes(x = SPECIES, y = value, label = paste(round(value,2),"X",sep="")),
            position = position_dodge(width = 1),
            hjust = -0.25, size = 3,fontface=2
  ) +
  theme(legend.position="none") +
  scale_y_continuous(expand=c(0,0),limits=c(0,47.5)) 

p2<-ggplot(data_change[data_change$variable=="NBR_SNPS",],aes(x=SPECIES,y=value)) +
  coord_flip() +
  geom_bar(stat='identity',alpha=0.25,fill="grey") +
  geom_bar(data=data_change[data_change$SPECIES==i,],
           stat='identity',alpha=0.6,fill="dodgerblue2") +
  scale_fill_viridis_d() +
  #facet_grid(.~variable, scales="free") +
  theme_classic() +
  ylab("Number of SNPS") +
  xlab("") +
  geom_text(
    aes(x = SPECIES, y = value, label = paste(round(value/1e6,2),"M",sep="")),
    position = position_dodge(width = 1),
    hjust = -0.25, size = 3,col="grey"
  ) +
  geom_text(
    data=data_change[data_change$variable=="NBR_SNPS",],
    aes(x = SPECIES, y = value, label = paste(round(value/1e6,2),"M",sep="")),
    position = position_dodge(width = 1),
    hjust = -0.25, size = 3, fontface=2
  ) +
  theme(legend.position="none") +
  scale_y_continuous(expand=c(0,0),limits=c(0,5.25e7)) 

pp<-ggpubr::ggarrange(p3,p4,nrow=2,common.legend = T)
ppp<-ggpubr::ggarrange(p1,p2,nrow=1)
pdf(paste("/Users/divco/Documents/COGEDIV/figures/stats_vcf.pdf",sep=""),width=15,height=15)
plot(ggpubr::ggarrange(pp,ppp,nrow=2))
dev.off()
