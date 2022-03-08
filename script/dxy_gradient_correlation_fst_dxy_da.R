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

load(file="data/dxy_all.Rdata")
colnames(dxy_data)[1]="Species_code"
dxy_data[7,5]=dxy_data[7,6] ### TO RMOVE !
fst<-merge(fst,dxy_data,on="Species_code")

fst$dxy_Li_Ga=as.numeric(fst$dxy_Li_Ga)
fst$dxy_Mu_Fa=as.numeric(fst$dxy_Mu_Fa)
fst$da_Li_Ga=as.numeric(fst$da_Li_Ga)
fst$da_Mu_Fa=as.numeric(fst$da_Mu_Fa)

fst[1,30]="NA"

fst=arrange(fst,desc(FST_LI_GA_WEIGHTED))
labels=mutate(fst,Species_code = forcats::fct_reorder(Species_code, desc(FST_LI_GA_WEIGHTED)))$italic_species
p<-fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,c(1,11:12,18:19,15)] %>%
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
  ylab(expression(paste(d[XY])))+
  geom_text(aes(label=value), vjust=-0.5, color="black",
            position = position_dodge(0.9), size=5)+
  scale_x_discrete(labels=parse(text=c(labels)))+
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,3.1))


#p_<-p
for (i in 1:nrow(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,])){
  img<-readPNG(paste("/Users/divco/Documents/COGEDIV/data/",fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]$Species_code[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  
  p<-p+
    annotation_custom(g, 
                      xmin=i-0.5, 
                      xmax=i+0.5, 
                      ymin=2.59+2*0.01,
                      ymax=2.59+2*0.25)
}

a<-summary(lm(dxy_Li_Ga~FST_LI_GA_WEIGHTED,data=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,c(1,11:12,18:19,15)]))
p1<-ggplot(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,c(1,11:12,18:19,15)],
           aes(x=FST_LI_GA_WEIGHTED,
               y=dxy_Li_Ga)) +
  geom_point(alpha=0.5,size=4,col="chartreuse4") +
  geom_abline(slope=coefficients(a)[2,1],
              intercept=coefficients(a)[1,1],
              col='red',
              lty=2,
              alpha=0.5,
              lwd=1.5) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=30,face="bold")) +
  xlab(expression(F[ST])) +
  ylab(expression(d[XY])) +
  geom_text_repel(aes(label=italic_species),parse=T,col="black") +
  labs(title=paste("p-value = ",round(coefficients(a)[2,4],3),sep=""))

a<-summary(lm(dxy_Li_Ga~da_Li_Ga,data=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]))
p2<-ggplot(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,],
           aes(x=da_Li_Ga,
               y=dxy_Li_Ga)) +
  geom_point(alpha=0.5,size=4,col="chartreuse4") +
  geom_abline(slope=coefficients(a)[2,1],
              intercept=coefficients(a)[1,1],
              col='red',
              lty=2,
              alpha=0.5,
              lwd=1.5) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=30,face="bold")) +
  xlab(expression(d[a])) +
  ylab(expression(d[XY])) +
  geom_text_repel(aes(label=italic_species),parse=T,col="black") +
  labs(title=paste("p-value = ",round(coefficients(a)[2,4],3),sep=""))

a<-summary(lm(FST_LI_GA_WEIGHTED~da_Li_Ga,data=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]))
p3<-ggplot(fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,],
           aes(x=da_Li_Ga,
               y=FST_LI_GA_WEIGHTED)) +
  geom_point(alpha=0.5,size=4,col="chartreuse4") +
  geom_abline(slope=coefficients(a)[2,1],
              intercept=coefficients(a)[1,1],
              col='red',
              lty=2,
              alpha=0.5,
              lwd=1.5) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=30,face="bold")) +
  xlab(expression(d[a])) +
  ylab(expression(F[ST])) +
  geom_text_repel(aes(label=italic_species),parse=T,col="black") +
  labs(title=paste("p-value = ",round(coefficients(a)[2,4],7),sep=""))




pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/fst_dxy_da_suppfig.pdf",sep=""),width=26,height=12.5)
print(ggpubr::ggarrange(p,ggpubr::ggarrange(p1,p2,p3,labels=c("B","C","D"),nrow=1,ncol=3),
                        labels=c("A",""),nrow=2,ncol=1)
)
dev.off()