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

## Fig 3 ----
load(file="IFB/tree/all.Rdata")

all$sp=factor(all$sp,
              levels = rev(c("Ssard","Lbude","Spilc","Msurm","Scabr","Dlabr","Hgutt","Scine","Styph","Scant")))


generation_time=data.frame(SP=c("Dlabr","Hgutt","Lbude","Msurm","Scabr","Scant","Scine","Spilc","Ssard","Styph"),
                           GENERATION=c(8.952,2.034,10.803,3.942,3.724,4.779,1.904,1.903,2.347,1.675),
                           PI=c(0.375,0.274,0.225,1.126,1.198,0.469,0.720,1.415,0.859,0.896))

labs_pop <- c("Within Atlantic",
              "Atlantic - Mediterranean",
              "Within Mediterranean")

names(labs_pop) <- levels(factor(all$POP_POP))

# Nouveaux noms d'étiquettes des facettes pour la variable `supp`
labs_species <- rev(c(
  "S. sarda",
  "L. budegassa",
  "S. pilchardus",
  "M. surmuletus",
  "S. cabrilla",
  "D. labrax",
  "H. guttulatus",
  "S. cinereus",
  "S. typhle",
  "S. cantharus"))


names(labs_species) <- levels(factor(all$sp))

p1<-ggplot(all,aes(x=DIVERGENCE))+
  geom_histogram(colour="black",
                 position='identity',
                 bins=100,
                 aes(y=..density..,fill=POP_POP),
                 alpha=0.75)+
  geom_density(alpha=0.75,size=1,aes(fill=POP_POP)) +
  facet_grid(sp~POP_POP,
             scales="free_y",
             labeller = labeller(POP_POP =labs_pop , 
                                 sp = labs_species)) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line=element_blank(),
        strip.text.x = element_text(
          size = 25, color = "black"
        ),
        strip.text.y = element_text(
          size = 15, color = "black", face = "bold.italic"
        ),
        strip.background = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"
        ),
        strip.text.y.right = element_text(angle = 0),
        legend.position = "bottom",
        axis.text.x=element_text(size=10),
        axis.title = element_text(size=25),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20)) +
  #coord_flip() +
  xlab("Time (generations)") +
  ylab("") +
  xlim(c(0,1e6)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(name="Populations",
                    label=c("Within Atlantic",
                            "Atlantic - Mediterranean",
                            "Within Mediterranean"),
                    values=c(color_med_atl[4,2],
                             viridis::viridis(1),
                             color_med_atl[1,2]))
#p1

data_fake=data.frame(X=seq(0,10,length.out=100),
                     Y=seq(0,10,length.out=100))
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  ylim(c(0,10)) +
  xlim(c(0,10))
a=0
for (sp in rev(levels(all$sp))){
  a=a+1
  img<-png::readPNG(paste("data/",sp,".png",sep=""))
  g<-grid::rasterGrob(img,interpolate=T)
  p_fake <- p_fake +
    theme_void() +
    annotation_custom(g,
                      xmin=0,
                      xmax=10,
                      ymin=a,
                      ymax=a+1) 
  
}

p_fake<-p_fake+
  ylim(c(0,11)) +
  xlim(c(0,10))

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig3_supp.pdf",sep=""),width=35/2,height=20/2)
print(ggpubr::ggarrange(
  p1,
  p_fake,
  widths = c(0.9,0.1))
)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig3_supp.jpg",sep=""),width=35/2,height=20/2,units="in",res=1500)
print(ggpubr::ggarrange(
  p1,
  p_fake,
  widths = c(0.9,0.1))
)
dev.off()


all<-all %>%
  rowwise() %>% 
  mutate(divergence_years = DIVERGENCE*generation_time[which(sp==generation_time$SP),2])

labs_pop <- c("Within Atlantic",
              "Atlantic - Mediterranean",
              "Within Mediterranean")

names(labs_pop) <- levels(factor(all$POP_POP))

# Nouveaux noms d'étiquettes des facettes pour la variable `supp`
labs_species <- rev(c(
  "S. sarda",
  "L. budegassa",
  "S. pilchardus",
  "M. surmuletus",
  "S. cabrilla",
  "D. labrax",
  "H. guttulatus",
  "S. cinereus",
  "S. typhle",
  "S. cantharus"))

names(labs_species) <- levels(factor(all$sp))

p1<-ggplot(all,aes(x=divergence_years))+
  geom_histogram(colour="black",
                 position='identity',
                 bins=100,
                 aes(y=..density..,fill=POP_POP),
                 alpha=0.75)+
  geom_density(alpha=0.75,size=1,aes(fill=POP_POP)) +
  facet_grid(sp~POP_POP,
             scales="free_y",
             labeller = labeller(POP_POP =labs_pop , 
                                 sp = labs_species)) +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line=element_blank(),
        strip.text.x = element_text(
          size = 25, color = "black"
        ),
        strip.text.y = element_text(
          size = 15, color = "black", face = "bold.italic"
        ),
        strip.background = element_rect(
          color="white", fill="white", size=1.5, linetype="solid"
        ),
        strip.text.y.right = element_text(angle = 0),
        legend.position = "bottom",
        axis.text.x=element_text(size=10),
        axis.title = element_text(size=25),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20)) +
  #coord_flip() +
  xlab("Time (years)") +
  ylab("") +
  xlim(c(0,4e6)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_manual(name="Populations",
                    label=c("Within Atlantic",
                            "Atlantic - Mediterranean",
                            "Within Mediterranean"),
                    values=c(color_med_atl[4,2],
                             viridis::viridis(1),
                             color_med_atl[1,2]))
#p1

data_fake=data.frame(X=seq(0,10,length.out=100),
                     Y=seq(0,10,length.out=100))
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  ylim(c(0,10)) +
  xlim(c(0,10))
a=0
for (sp in rev(levels(all$sp))){
  a=a+1
  img<-png::readPNG(paste("data/",sp,".png",sep=""))
  g<-grid::rasterGrob(img,interpolate=T)
  p_fake <- p_fake +
    theme_void() +
    annotation_custom(g,
                      xmin=0,
                      xmax=10,
                      ymin=a,
                      ymax=a+1) 
  
}

p_fake<-p_fake+
  ylim(c(0,11)) +
  xlim(c(0,10))

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig3.pdf",sep=""),width=35/2,height=20/2)
print(ggpubr::ggarrange(
  p1,
  p_fake,
  widths = c(0.9,0.1))
)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig3.jpg",sep=""),width=35/2,height=20/2,units="in",res=1500)
print(ggpubr::ggarrange(
  p1,
  p_fake,
  widths = c(0.9,0.1))
)
dev.off()

#tree