library(readxl)
library(tidyverse)
library(reshape2)
library(stringr)
library(png)
library(grid)
library(ggrepel)
library(ggpubr)
library(lmtest)
lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
italic_species=c("italic('A. boyeri')",
                 "italic('A. fallax')",
                 "italic('C. galerita')",
                 "italic('C. julis')",
                 "italic('D. labrax')",
                 "italic('D. puntazzo')",
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
lfh$italic_species=italic_species

tmp=lfh[,c("italic_species","Body_Size","Trophic_Level","Fec","Propagule_Size")]

colnames(tmp)[2:4]=c("Body size (cm)","Trophic level","Fecundity (eggs/day)","Propagule size (mm)")

labs <- sapply(
  as.character(tmp$italic_species), 
  function(x) parse(text = paste0(x))
)

tmp=as.data.frame(tmp)
p1<-melt(tmp[,c(1,2)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Body size (cm)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,125)) +
  scale_x_discrete(labels = labs,limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.25) 
#scale_y_discrete()

p2<-melt(tmp[,c(1,3)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Trophic level") +
  scale_y_continuous(expand=c(0,0),limits=c(0,5.5)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

tmp[,4]=round(as.numeric(tmp[,4]),2)
p3<-melt(tmp[,c(1,4)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  #facet_grid(.~variable,scale="free", switch = 'x') +
  coord_flip() +
  theme_classic() +
  scale_fill_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Fecundity (eggs/day)") +
  scale_y_log10(expand=c(0,0),limits=c(0.1,12000)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

p4<-melt(tmp[,c(1,5)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Propagule size (mm)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,22)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

df <- data.frame()
fake_plot=ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 19) +
  theme_void()

for (i in 1:nrow(lfh)){
  img<-readPNG(paste("/Users/divco/Documents/COGEDIV/data/",lfh$Species_code[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  
  if (lfh$Species_code[i]=="aStyph"){
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  } else {
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  }
  
}

fake_plot <- fake_plot + ylim(c(-0.25,18.5))

first_plot<-ggarrange(fake_plot,p1,p2,p3,p4,nrow=1,widths=c(0.1,0.27,0.19875,0.19875,0.19875))


##
tmp=lfh[,c("italic_species","Age_Mat","Lifespan","Adult_Lifespan","PLD")]

colnames(tmp)[2:4]=c("Age at maturity (years)","Lifespan (years)","Adult lifespan (years)","PLD (days)")

labs <- sapply(
  as.character(tmp$italic_species), 
  function(x) parse(text = paste0(x))
)

tmp=as.data.frame(tmp)
p1<-melt(tmp[,c(1,2)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Age at maturity (years)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,8)) +
  scale_x_discrete(labels = labs,limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.25) 
#scale_y_discrete()

p2<-melt(tmp[,c(1,3)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Lifespan (years)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,24)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

tmp[,4]=round(as.numeric(tmp[,4]),2)
p3<-melt(tmp[,c(1,4)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  #facet_grid(.~variable,scale="free", switch = 'x') +
  coord_flip() +
  theme_classic() +
  scale_fill_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Adult lifespan (years)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,17)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

p4<-melt(tmp[,c(1,5)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("PLD (days)") +
  scale_y_continuous(expand=c(0,0),limits=c(0,85)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()

df <- data.frame()
fake_plot=ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 19) +
  theme_void()

for (i in 1:nrow(lfh)){
  img<-readPNG(paste("/Users/divco/Documents/COGEDIV/data/",lfh$Species_code[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  
  if (lfh$Species_code[i]=="aStyph"){
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  } else {
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  }
  
}

fake_plot <- fake_plot + ylim(c(-0.25,18.5))

second_plot<-ggarrange(fake_plot,p1,p2,p3,p4,nrow=1,widths=c(0.1,0.27,0.19875,0.19875,0.19875))

##

##
tmp=lfh[,c("italic_species","Hermaphrodism","Parental_Care")]

colnames(tmp)[2:3]=c("Hermaphroditism","Parental Care")

labs <- sapply(
  as.character(tmp$italic_species), 
  function(x) parse(text = paste0(x))
)

tmp=as.data.frame(tmp)
p1<-melt(tmp[,c(1,2)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Hermaphroditism") +
  #scale_y_continuous(expand=c(0,0),limits=c(0,8)) +
  scale_x_discrete(labels = labs,limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.25) 
#scale_y_discrete()

p2<-melt(tmp[,c(1,3)],id.vars=c("italic_species")) %>%
  ggplot(aes(x=italic_species,y=value)) +
  #geom_segment( aes(x=italic_species, xend=italic_species, y=0, yend=value,color=value),size=1.25,alpha=0.75) +
  #geom_point(size=4, aes(color=value),alpha=0.75) + 
  geom_bar(stat='identity',aes(fill=value),alpha=0.5,col="black") +
  coord_flip() +
  theme_classic() +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(strip.placement = "outside",
        legend.position="none") +
  xlab("") +
  ylab("Parental Care") +
  #scale_y_continuous(expand=c(0,0),limits=c(0,24)) +
  scale_x_discrete(labels = rep(" ",nrow(tmp)),limits = rev) +
  geom_text(aes(label = value, y= value),  hjust = -0.5)
#scale_y_discrete()



df <- data.frame()
fake_plot=ggplot(df) + geom_point() + xlim(0, 1) + ylim(0, 19) +
  theme_void()

for (i in 1:nrow(lfh)){
  img<-readPNG(paste("/Users/divco/Documents/COGEDIV/data/",lfh$Species_code[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  
  if (lfh$Species_code[i]=="aStyph"){
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  } else {
    fake_plot<-fake_plot+
      annotation_custom(g, 
                        xmin=0, 
                        xmax=1, 
                        ymin=19-i+1,
                        ymax=19-i)
  }
  
}

fake_plot <- fake_plot + ylim(c(-0.25,18.5))

third_plot<-ggarrange(fake_plot,p1,p2,nrow=1,widths=c(0.1,0.525,0.375))


pdf(paste("/Users/divco/Documents/COGEDIV/figures/lfh.pdf",sep=""),width=25,height=20)
print(ggarrange(first_plot,
                second_plot,
                third_plot,
                nrow=3))
dev.off()

