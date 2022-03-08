library(ggplot2)
library(dplyr)
library(ggalt)
library(cowplot)
library(tibble)
library(lubridate)
library(ggeasy)
library(RColorBrewer)
library(png)
library(grid)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library(ggsn)

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, size=3, alpha=0.6,color=brewer.pal(6, "Dark2")[2],
             arrow = arrow(length = unit(0.25, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

##
# Draw pca ----
load(file="/Users/divco/Documents/COGEDIV/data/PCA_list.Rdata")


## Cgale ----
pca=PCA_list$Cgale$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_cgale<-p

## Cjuli ----
pca=PCA_list$Cjuli$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_cjuli<-p

## Dlabr ----
pca=PCA_list$Dlabr$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_dlabr<-p

## Hgutt ----
pca=PCA_list$Hgutt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",11),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_hgutt<-p

## Lbude ----
pca=PCA_list$Lbude$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",3)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_lbude<-p

## Msurm ----
pca=PCA_list$Msurm$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_msurm<-p

## Scabr ----
pca=PCA_list$Scabr$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_scabr<-p

## Scant ----
pca=PCA_list$Scant$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_scant<-p
## Scine ----
pca=PCA_list$Scine$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_scine<-p

## Spilc ----
pca=PCA_list$Spilc$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_spilc<-p
## Ssard ----
pca=PCA_list$Ssard$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_ssard<-p
## Styph ----
pca=PCA_list$Styph$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_styph<-p
## Aboye ----
pca=PCA_list$Aboye$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_aboye<-p

## Dpunt ----
pca=PCA_list$Dpunt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",4)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_dpunt<-p

## Lmorm ----
pca=PCA_list$Lmorm$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_lmorm<-p

## Mmerl ----
pca=PCA_list$Mmerl$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_mmerl<-p

## Peryt ----
pca=PCA_list$Peryt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_peryt<-p

## Gnige ----
pca=PCA_list$Gnige$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_gnige<-p

## Afall ----
pca=PCA_list$Afall$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",4),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",0)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5),
        axis.title=element_text(size=7.5),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste(round(pca$varprop[1]*100,2),"%",sep=""))+
  ylab(paste(round(pca$varprop[2]*100,2),"%",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]))+
  theme(legend.position = "none")
#p1<-ggplotly(p,tooltip='label',height = 400,width=1250)
p_afall<-p
## Draw map ----
color_med_atl=data.frame(Location=c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
                         Col=brewer.pal(n = 4, name = "RdBu"))
load(file="/Users/divco/Documents/COGEDIV/data/sampling.Rdata")
sampling$lon=as.numeric(sampling$lon)
sampling$lat=as.numeric(sampling$lat)
for (i in 1:nrow(sampling)){
  if (sampling$LOCATION[i]=="Mar Menor"){
    sampling$LOCATION[i]="Costa Calida"
  }
}
sampling$LOCATION=factor(sampling$LOCATION)
sites <- st_as_sf(data.frame(longitude = sampling[is.na(sampling$lon)==FALSE,]$lon, 
                             latitude = sampling[is.na(sampling$lat)==FALSE,]$lat, 
                             loc=sampling[is.na(sampling$lon)==FALSE,]$LOCATION,
                             detail_loc=sampling[is.na(sampling$lon)==FALSE,]$DETAILED_LOCATION,
                             sp=sampling[is.na(sampling$lon)==FALSE,]$SPECIES_CODE),
                  coords = c("longitude", "latitude"),
                  crs = 4326,
                  agr = "constant"
)

col=c()
for (i in 1:length(sites$loc)){
  col[i]=as.character(color_med_atl$Col[which(sites$loc[i]==color_med_atl$Location)])
}
sites$col=as.character(col)
world <- ne_countries(scale='medium',returnclass = 'sf')
lon <- c (-10, 10)
lat <- c (35, 47.5)

europe <- ggplot(data = world) +
  geom_sf(fill="grey",lwd=0) +
  coord_sf(xlim = c(-100,100), ylim = c(-25,75))+
  geom_rect(xmin = -10, xmax = 10, ymin = 35, ymax = 47.5, 
            fill = NA, colour = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA),
        legend.position = "none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

all<-ggplot(data = world) +
  geom_sf(fill="grey") +
  geom_sf(data = sites, size = 2, shape = 1, col=col,aes(label=detail_loc)) +
  coord_sf(xlim = c(-10, 10), ylim = c(35,47.5))+
  annotate(geom = "text", x = 5.1, y = 38, label = "Mediterranean \n Sea", 
           fontface = "italic", color = "grey22", size = 5.5) +
  annotate(geom = "text", x = -7.5, y = 44.75, label = "Atlantic \n Ocean", 
           fontface = "italic", color = "grey22", size = 5.5) +
  annotate(geom = "text", x = 5, y = 42.35, label = "Gulf of \n Lion", 
           size = 4) +
  annotate(geom = "text", x = 0.75, y = 37.25, label = "Costa \n Calida", 
           size = 4) +
  annotate(geom = "text", x = -8, y = 36.5, label = "Algarve", 
           size = 4) +
  annotate(geom = "text", x = -2.85, y = 44.5, label = "Bay of \n Biscay", 
           size = 4) +
  theme(plot.tag.position = 'topleft')+
  xlab("")+
  ylab("")+
  scalebar(dist = 250, 
           dist_unit = "km", 
           model = 'WGS84',
           st.size = 2.5,
           transform=TRUE,
           x.min=8,
           x.max=9,
           y.min=36,
           y.max=45)+
  north(x.min=-9,x.max=-5,
        y.min=46,y.max=47)+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = NA),
        legend.position = "none")
  #annotation_north_arrow(location = "bl", which_north = "true", 
  #                       pad_x = unit(3.25, "in"), pad_y = unit(2.5, "in"),
  #                       style = north_arrow_fancy_orienteering) 

map<-ggplot() +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  annotation_custom(ggplotGrob(all), xmin = 0, xmax = 100, ymin = 0, 
                    ymax = 100) +
  annotation_custom(ggplotGrob(europe), xmin = 2.5, xmax = 37.5, ymin = 80, 
                    ymax = 100) + 
  theme_void()

## Draw fst ----

library(readxl)
fst=as.data.frame(read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx"))
fst$SPECIES_code=c("Aboye","Afall","Cgale","Cjuli","Dlabr","Dpunt","Eencr","Gnige","Hgutt","Lbude","Lmorm","Mmerl","Msurm","Peryt","Scabr","Scant","Scine","Spilc","Ssard","Styph")

fst<-fst %>%
  filter(is.na(FST_LI_GA_WEIGHTED)==F) %>%
  arrange(FST_LI_GA_WEIGHTED)
#fst<-fst %>%
#  add_column(y=rep(rep(c(0.5,-0.5,2,-2),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/4),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]) %>%
#  add_column(xmin=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$FST_LI_GA_WEIGHTED-0.05) %>%
#  add_column(xmax=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$FST_LI_GA_WEIGHTED+0.05) %>%
#  add_column(ymin=rep(rep(c(0.5,-0.5,2,-2),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/4),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]) %>%
#  add_column(ymax=rep(rep(c(0.5,-0.5,2,-2),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/4),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]+sign(rep(rep(c(0.5,-0.5,2,-2),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/4),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]))
fst<-fst %>%
  add_column(y=rep(rep(c(0.5,-0.5,2,-2,3.5,-3.5),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/6),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]) %>%
  add_column(xmin=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$FST_LI_GA_WEIGHTED-0.05) %>%
  add_column(xmax=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$FST_LI_GA_WEIGHTED+0.05) %>%
  add_column(ymin=rep(rep(c(0.5,-0.5,2,-2,3.5,-3.5),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/6),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]) %>%
  add_column(ymax=rep(rep(c(0.5,-0.5,2,-2,3.5,-3.5),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/6),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]+sign(rep(rep(c(0.5,-0.5,2,-2,3.5,-3.5),nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])/6),2)[1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])]))


## Image
cgale <- readPNG("/Users/divco/Documents/COGEDIV/data/Cgale.png")
cgale <- rasterGrob(cgale, interpolate=TRUE)
cjuli <- readPNG("/Users/divco/Documents/COGEDIV/data/Cjuli.png")
cjuli <- rasterGrob(cjuli, interpolate=TRUE)
dlabr <- readPNG("/Users/divco/Documents/COGEDIV/data/Dlabr.png")
dlabr <- rasterGrob(dlabr, interpolate=TRUE)
hgutt <- readPNG("/Users/divco/Documents/COGEDIV/data/Hgutt.png")
hgutt <- rasterGrob(hgutt, interpolate=TRUE)
lbude <- readPNG("/Users/divco/Documents/COGEDIV/data/Lbude.png")
lbude <- rasterGrob(lbude, interpolate=TRUE)
msurm <- readPNG("/Users/divco/Documents/COGEDIV/data/Msurm.png")
msurm <- rasterGrob(msurm, interpolate=TRUE)
scabr <- readPNG("/Users/divco/Documents/COGEDIV/data/Scabr.png")
scabr <- rasterGrob(scabr, interpolate=TRUE)
scant <- readPNG("/Users/divco/Documents/COGEDIV/data/Scant.png")
scant <- rasterGrob(scant, interpolate=TRUE)
scine <- readPNG("/Users/divco/Documents/COGEDIV/data/Scine.png")
scine <- rasterGrob(scine, interpolate=TRUE)
spilc <- readPNG("/Users/divco/Documents/COGEDIV/data/Spilc.png")
spilc <- rasterGrob(spilc, interpolate=TRUE)
ssard <- readPNG("/Users/divco/Documents/COGEDIV/data/Ssard.png")
ssard <- rasterGrob(ssard, interpolate=TRUE)
styph <- readPNG("/Users/divco/Documents/COGEDIV/data/Styph.png")
styph <- rasterGrob(styph, interpolate=TRUE)
aboye <- readPNG("/Users/divco/Documents/COGEDIV/data/Aboye.png")
aboye <- rasterGrob(aboye, interpolate=TRUE)
dpunt <- readPNG("/Users/divco/Documents/COGEDIV/data/Dpunt.png")
dpunt <- rasterGrob(dpunt, interpolate=TRUE)
lmorm <- readPNG("/Users/divco/Documents/COGEDIV/data/Lmorm.png")
lmorm <- rasterGrob(lmorm, interpolate=TRUE)
mmerl <- readPNG("/Users/divco/Documents/COGEDIV/data/Mmerl.png")
mmerl <- rasterGrob(mmerl, interpolate=TRUE)
peryt <- readPNG("/Users/divco/Documents/COGEDIV/data/Peryt.png")
peryt <- rasterGrob(peryt, interpolate=TRUE)

gnige <- readPNG("/Users/divco/Documents/COGEDIV/data/Gnige.png")
gnige <- rasterGrob(gnige, interpolate=TRUE)
afall <- readPNG("/Users/divco/Documents/COGEDIV/data/Afall.png")
afall <- rasterGrob(afall, interpolate=TRUE)

fst$SPECIES=factor(fst$SPECIES,levels=fst$SPECIES,labels=fst$SPECIES)
p1<-ggplot(fst,aes(x=FST_LI_GA_WEIGHTED,y=y))+
  geom_lollipop(stat='identity',alpha=0.6,aes(col=SPECIES),size=1.5)+
  theme_classic()+
  easy_remove_axes()+
  ylim(c(-4.5,4.5))+
  xlim(c(-0.025,0.8))+
  theme(legend.position = "none")+
  scale_colour_viridis_d()+
  annotation_custom(ggplotGrob(p_cgale),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Cgale")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Cgale")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Cgale")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Cgale")])+
  annotation_custom(ggplotGrob(p_cjuli),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Cjuli")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Cjuli")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Cjuli")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Cjuli")])+
  annotation_custom(ggplotGrob(p_dlabr),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Dlabr")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Dlabr")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Dlabr")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Dlabr")])+
  annotation_custom(ggplotGrob(p_hgutt),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Hgutt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Hgutt")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Hgutt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Hgutt")])+
  annotation_custom(ggplotGrob(p_lbude),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Lbude")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Lbude")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Lbude")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Lbude")])+
  annotation_custom(ggplotGrob(p_msurm),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Msurm")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Msurm")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Msurm")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Msurm")])+
  annotation_custom(ggplotGrob(p_scabr),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scabr")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scabr")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Scabr")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scabr")])+
  annotation_custom(ggplotGrob(p_scant),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scant")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scant")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Scant")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scant")])+
  annotation_custom(ggplotGrob(p_styph),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Styph")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Styph")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Styph")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Styph")])+
  annotation_custom(ggplotGrob(p_spilc),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Spilc")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Spilc")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Spilc")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Spilc")])+
  annotation_custom(ggplotGrob(p_ssard),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Ssard")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Ssard")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Ssard")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Ssard")])+
  annotation_custom(ggplotGrob(p_scine),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scine")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scine")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Scine")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scine")])+
  annotation_custom(ggplotGrob(p_aboye),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Aboye")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Aboye")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Aboye")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Aboye")])+
  annotation_custom(ggplotGrob(p_lmorm),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Lmorm")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Lmorm")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Lmorm")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Lmorm")])+
  annotation_custom(ggplotGrob(p_dpunt),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Dpunt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Dpunt")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Dpunt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Dpunt")])+
  annotation_custom(ggplotGrob(p_mmerl),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Mmerl")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Mmerl")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Mmerl")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Mmerl")])+
  annotation_custom(ggplotGrob(p_peryt),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Peryt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Peryt")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Peryt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Peryt")])+
  annotation_custom(ggplotGrob(p_gnige),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Gnige")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Gnige")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Gnige")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Gnige")])+
  annotation_custom(ggplotGrob(p_afall),
                    xmin = fst$xmin[which(fst$SPECIES_code=="Afall")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Afall")],
                    ymin=fst$ymin[which(fst$SPECIES_code=="Afall")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Afall")])+
  annotate(geom="text", x=0.7, y=0.25, label="bold(italic(F[st]))", parse=T,
           size = 10,
           fontface = "bold")+ 
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Cgale")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Cgale")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Cgale")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Cjuli")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Cjuli")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Cjuli")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Dlabr")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Dlabr")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Dlabr")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Hgutt")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Hgutt")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Hgutt")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Lbude")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Lbude")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Lbude")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Msurm")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Msurm")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Msurm")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scabr")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Scabr")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scabr")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scant")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Scant")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scant")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scine")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Scine")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Scine")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Spilc")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Spilc")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Spilc")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Ssard")],
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Ssard")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Ssard")],3),
           size = 7.5,
           fontface = "bold")+
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Styph")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Styph")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Styph")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Aboye")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Aboye")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Aboye")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Dpunt")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Dpunt")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Dpunt")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Lmorm")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Lmorm")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Lmorm")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Mmerl")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Mmerl")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Mmerl")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Peryt")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Peryt")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Peryt")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Gnige")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Gnige")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Gnige")],3),
           size = 7.5,
           fontface = "bold") +
  annotate(geom="text", 
           x= fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Afall")] ,
           y=-0.075*sign(fst$ymin[which(fst$SPECIES_code=="Afall")]), 
           label= round(fst$FST_LI_GA_WEIGHTED[which(fst$SPECIES_code=="Afall")],3),
           size = 7.5,
           fontface = "bold") +
  annotation_custom(ggplotGrob(map),
                    xmin = 0.675, 
                    xmax=0.825,
                    ymin=2,
                    ymax=4.5)

p1<-shift_axis(p1, 0,0.7)

p1 <- p1 + annotation_custom(cgale,
                             xmin = fst$xmin[which(fst$SPECIES_code=="Cgale")], 
                             xmax=fst$xmax[which(fst$SPECIES_code=="Cgale")],
                             ymin=fst$ymax[which(fst$SPECIES_code=="Cgale")],
                             ymax=fst$ymax[which(fst$SPECIES_code=="Cgale")]+sign(fst$ymax[which(fst$SPECIES_code=="Cgale")])*0.25) +
  
  annotation_custom(cjuli,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Cjuli")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Cjuli")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Cjuli")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Cjuli")]+sign(fst$ymax[which(fst$SPECIES_code=="Cjuli")])*0.25) +
  
  annotation_custom(dlabr,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Dlabr")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Dlabr")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Dlabr")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Dlabr")]+sign(fst$ymax[which(fst$SPECIES_code=="Dlabr")])*0.25) +
  
  annotation_custom(hgutt,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Hgutt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Hgutt")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Hgutt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Hgutt")]+sign(fst$ymax[which(fst$SPECIES_code=="Hgutt")])*0.25) +
  
  annotation_custom(lbude,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Lbude")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Lbude")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Lbude")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Lbude")]+sign(fst$ymax[which(fst$SPECIES_code=="Lbude")])*0.25) +
  
  annotation_custom(msurm,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Msurm")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Msurm")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Msurm")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Msurm")]+sign(fst$ymax[which(fst$SPECIES_code=="Msurm")])*0.25) +
  
  annotation_custom(scabr,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scabr")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scabr")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Scabr")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scabr")]+sign(fst$ymax[which(fst$SPECIES_code=="Scabr")])*0.25) +
  
  annotation_custom(scant,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scant")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scant")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Scant")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scant")]+sign(fst$ymax[which(fst$SPECIES_code=="Scant")])*0.25) +
  
  annotation_custom(scine,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Scine")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Scine")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Scine")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Scine")]+sign(fst$ymax[which(fst$SPECIES_code=="Scine")])*0.25) +
  
  annotation_custom(spilc,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Spilc")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Spilc")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Spilc")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Spilc")]+sign(fst$ymax[which(fst$SPECIES_code=="Spilc")])*0.25) +
  
  annotation_custom(ssard,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Ssard")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Ssard")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Ssard")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Ssard")]+sign(fst$ymax[which(fst$SPECIES_code=="Ssard")])*0.25) +
  annotation_custom(styph,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Styph")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Styph")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Styph")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Styph")]+sign(fst$ymax[which(fst$SPECIES_code=="Styph")])*0.25) +
  annotation_custom(aboye,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Aboye")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Aboye")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Aboye")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Aboye")]+sign(fst$ymax[which(fst$SPECIES_code=="Aboye")])*0.25) +
  annotation_custom(dpunt,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Dpunt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Dpunt")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Dpunt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Dpunt")]+sign(fst$ymax[which(fst$SPECIES_code=="Dpunt")])*0.25) +
  annotation_custom(mmerl,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Mmerl")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Mmerl")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Mmerl")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Mmerl")]+sign(fst$ymax[which(fst$SPECIES_code=="Mmerl")])*0.25) +
  annotation_custom(lmorm,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Lmorm")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Lmorm")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Lmorm")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Lmorm")]+sign(fst$ymax[which(fst$SPECIES_code=="Lmorm")])*0.25) +
  annotation_custom(peryt,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Peryt")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Peryt")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Peryt")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Peryt")]+sign(fst$ymax[which(fst$SPECIES_code=="Peryt")])*0.25) +
  annotation_custom(gnige,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Gnige")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Gnige")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Gnige")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Gnige")]+sign(fst$ymax[which(fst$SPECIES_code=="Gnige")])*0.25) +
  annotation_custom(afall,
                    xmin = fst$xmin[which(fst$SPECIES_code=="Afall")], 
                    xmax=fst$xmax[which(fst$SPECIES_code=="Afall")],
                    ymin=fst$ymax[which(fst$SPECIES_code=="Afall")],
                    ymax=fst$ymax[which(fst$SPECIES_code=="Afall")]+sign(fst$ymax[which(fst$SPECIES_code=="Afall")])*0.25)


#p1
pdf("/Users/divco/Documents/COGEDIV/figures/fst_continuum.pdf",width=50,height=25)
print(p1)
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/fst_continuum.jpg",width=5000,height=2500,quality=100,res=100)
print(p1)
dev.off()

#scale_x_log10()
#p1


