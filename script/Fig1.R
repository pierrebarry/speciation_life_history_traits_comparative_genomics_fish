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

## Fig 1 ----
load(file="../species_identity/data/sampling.Rdata")
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

sites$X=st_coordinates(sites$geometry)[,1]
sites$Y=st_coordinates(sites$geometry)[,2]

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
  geom_sf(data = sites, size = 2, shape = 1, col=col) +
  geom_sf(data = sites, size = 3, shape = 19, col=sites$col) +
  #ggrepel::geom_label_repel(
  #   data = distinct(sites[sites$sp==i,],detail_loc,.keep_all=T),
  #   aes(label = distinct(sites[sites$sp==i,],detail_loc,.keep_all=T)$detail_loc, geometry = geometry),
  #   stat = "sf_coordinates",
  #   min.segment.length = 0
  # ) +
  #geom_sf_text_repel(data = sites[sites$sp=="i",], aes(sites[sites$sp=="i",]$X, sites[sites$sp=="i",]$Y, label=sites[sites$sp=="i",]$detail_loc), colour = "black") +
  coord_sf(xlim = c(-12.5, 10), ylim = c(32.5,47.5), expand = FALSE)+
  annotate(geom = "text", x = 5, y = 38, label = "Mediterranean \n Sea", 
           fontface = "italic", color = "grey22", size = 5.5) +
  annotate(geom = "text", x = -7.5, y = 45, label = "Atlantic \n Ocean", 
           fontface = "italic", color = "grey22", size = 5.5) +
  annotate(geom = "text", x = 5.5, y = 42.35, label = "Med-out", 
           size = 5,col=color_med_atl$Col[1],fontface=2) +
  annotate(geom = "text", x = 0.75, y = 37, label = "Med-in", 
           size = 5,col=color_med_atl$Col[2],fontface=2) +
  annotate(geom = "text", x = -8, y = 36, label = "Atl-in", 
           size = 5,col=color_med_atl$Col[3],fontface=2) +
  annotate(geom = "text", x = -3.25, y = 44.5, label = "Atl-out", 
           size = 5,col=color_med_atl$Col[4],fontface=2) +
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


map<-ggplot() +
  coord_equal(xlim = c(0, 100), ylim = c(0, 100)) +
  annotation_custom(ggplotGrob(all), xmin = 0, xmax = 100, ymin = 0, 
                    ymax = 100) +
  annotation_custom(ggplotGrob(europe), xmin = 2.5, xmax = 37.5, ymin = 83.5, 
                    ymax = 105) + 
  theme_void()
#print(map)
print(map)

ggsave(paste("/Users/divco/Documents/COGEDIV/figures/article_fig_1map.svg",sep=""),width=7.5,height=7.5,units="in")


fst<-read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx")
italic_species=c("italic('A. boyeri')",
                 "italic('A. fallax')",
                 "italic('C. galerita')",
                 "italic('C. julis')",
                 "italic('D. labrax')",
                 "italic('D. puntazzo')",
                 "italic('E. encrasicolus')",
                 "italic('G. niger')",
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
fst$italic_species=italic_species

fst$FST_LI_GA_WEIGHTED=round(fst$FST_LI_GA_WEIGHTED,2)
fst$FST_MU_FA_WEIGHTED=round(as.numeric(as.character(fst$FST_MU_FA_WEIGHTED)),2)


fst=arrange(fst,desc(FST_LI_GA_WEIGHTED))

labels=mutate(fst,SPECIES = forcats::fct_reorder(SPECIES, desc(FST_LI_GA_WEIGHTED)))$italic_species

fst[which(fst$Species_code=="Aboye"),"FST_MU_FA_WEIGHTED"]=NA
p<-fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,c(1,11:12,15)] %>%
  #filter(FST_MU_FA_WEIGHTED <0.6 ) %>%
  mutate(SPECIES = forcats::fct_reorder(SPECIES, desc(FST_LI_GA_WEIGHTED))) %>%
  melt(id.vars=c("SPECIES","italic_species")) %>%
  mutate(value = round(as.numeric(value),2)) %>%
  ggplot(aes(x=SPECIES,y=value,fill=variable))+
  geom_bar(stat='identity',position = position_dodge())+
  scale_fill_manual(name="Populations comparison",values=viridis::cividis(2,begin=0.5,end=0.9),labels=c("Between out","Between in"))+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text.x=element_blank(),
        axis.text=element_text(size=15),
        axis.title=element_text(size=30,face="bold")) +
  #xlab("Species")+
  xlab("") +
  ylab(expression(paste(F[st])))+
  geom_text(aes(label=value), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=5)+
  scale_x_discrete(labels=parse(text=c(labels)))+
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,max(fst$FST_LI_GA_WEIGHTED,na.rm=T)+max(fst$FST_LI_GA_WEIGHTED,na.rm=T)*0.285))

p_fst<-p
for (i in 1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])){
  img<-readPNG(paste("/Users/divco/Documents/COGEDIV/data/",fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$Species_code[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  
  p<-p+
    annotation_custom(g, 
                      xmin=i-0.5, 
                      xmax=i+0.5, 
                      ymin=max(fst$FST_LI_GA_WEIGHTED,na.rm=T)+max(fst$FST_LI_GA_WEIGHTED,na.rm=T)*0.1,
                      ymax=max(fst$FST_LI_GA_WEIGHTED,na.rm=T)+max(fst$FST_LI_GA_WEIGHTED,na.rm=T)*0.285)
}

load(file="data/dxy_all.Rdata")
colnames(dxy_data)[1]="Species_code"
dxy_data[7,"dxy_Mu_Fa"]=dxy_data[7,"dxy_Li_Fa"]
dxy_data[7,"da_Mu_Fa"]=dxy_data[7,"da_Li_Fa"]

fst<-merge(fst,dxy_data,on="Species_code")

fst$dxy_Li_Ga=as.numeric(fst$dxy_Li_Ga)
fst$dxy_Mu_Fa=as.numeric(fst$dxy_Mu_Fa)
fst$da_Li_Ga=as.numeric(fst$da_Li_Ga)
fst$da_Mu_Fa=as.numeric(fst$da_Mu_Fa)

summary(lm(fst$FST_LI_GA_WEIGHTED~fst$dxy_Li_Ga))
summary(lm(fst$FST_LI_GA_WEIGHTED~fst$da_Li_Ga))
summary(lm(fst$dxy_Li_Ga~fst$da_Li_Ga))
fst[1,30]="NA"

fst=arrange(fst,desc(FST_LI_GA_WEIGHTED))
labels=mutate(fst,Species_code = forcats::fct_reorder(Species_code, desc(FST_LI_GA_WEIGHTED)))$italic_species
p_da<-fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,c(1,11:12,29:30,15)] %>%
  #filter(FST_MU_FA_WEIGHTED <0.6 ) %>%
  mutate(Species_code = forcats::fct_reorder(Species_code, desc(FST_LI_GA_WEIGHTED))) %>%
  select(-FST_LI_GA_WEIGHTED,-FST_MU_FA_WEIGHTED) %>%
  melt(id.vars=c("Species_code","italic_species")) %>%
  mutate(value = round(as.numeric(value),2)) %>%
  ggplot(aes(x=Species_code,y=value,fill=variable))+
  geom_bar(stat='identity',position = position_dodge())+
  scale_fill_manual(name="Populations comparison",values=viridis::cividis(2,begin=0.5,end=0.9),labels=c("Between out","Between in"))+
  theme_classic()+
  theme(legend.position = 'bottom',
        axis.text=element_text(size=15),
        axis.title=element_text(size=30,face="bold"),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  xlab("Species")+
  ylab(expression(paste(d[a])))+
  geom_text(aes(label=value), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=5)+
  scale_x_discrete(labels=parse(text=c(labels)))+
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,1.2))

tiff(paste("/Users/divco/Documents/COGEDIV/figures/fst_da_fig2.tiff",sep=""),width=24,height=10,units="in",res=1000)
print(ggpubr::ggarrange(p,
                        p_da,
                        nrow=2,
                        ncol=1,
                        heights = c(0.45,0.55))
)
dev.off()
pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/fst_da_fig2.pdf",sep=""),width=27,height=12.5)
print(ggpubr::ggarrange(p,
                        p_da,
                        nrow=2,
                        ncol=1,
                        heights = c(0.45,0.55))
)
dev.off()
#print(ggpubr::ggarrange(p,
#                        p_da,
#                        nrow=2,
#                        ncol=1,
#                        heights = c(0.45,0.55))
#)
ggsave(paste("/Users/divco/Documents/COGEDIV/figures_paper/fst_da_fig2.svg",sep=""),width=27,height=10,units="in")

load(file="Data/PCA_list.Rdata")

pca=PCA_list$Cgale$Raw$maf0[[1]]
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
p_Cgale<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]),
                      labels=c("Atl-in","Atl-out","Med-in","Med-out")) +
  labs(title="Coryphoblennius galerita")

pca=PCA_list$Spilc$Raw$maf0[[1]]
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
p_Spilc<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]),
                      labels=c("Atl-in","Atl-out","Med-in","Med-out")) +
  labs(title="Sardina pilchardus")

pca=PCA_list$Dlabr$Raw$maf0[[1]]
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
p_Dlabr<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]),
                      labels=c("Atl-in","Atl-out","Med-in","Med-out")) +
  labs(title="Dicentrarchus labrax")

pca=PCA_list$Msurm$Raw$maf0[[1]]
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
p_Msurm<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4]),
                      labels=c("Atl-in","Atl-out","Med-in","Med-out")) +
  labs(title="Mullus surmuletus")

print(ggpubr::ggarrange(p_Cgale,p_Spilc,p_Dlabr,p_Msurm,
                        common.legend=T))
ggsave(paste("/Users/divco/Documents/COGEDIV/figures/pca_fig1.svg",sep=""),width=10,height=10,units="in")

tiff(paste("/Users/divco/Documents/COGEDIV/figures/pca_fig1.tiff",sep=""),width=10,height=10,units="in",res=2000)
print(ggpubr::ggarrange(p_Cgale,p_Spilc,p_Dlabr,p_Msurm,
                        common.legend=T))
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures/pca_fig1.pdf",sep=""),width=10,height=10)
print(ggpubr::ggarrange(p_Cgale,p_Spilc,p_Dlabr,p_Msurm,
                        common.legend=T))
dev.off()