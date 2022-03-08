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

### Fig 5 ----

load(file="data/data_corr.Rdata")
tree=ape::read.tree("data/all.fa.treefile")

for (i in 3:8){
  data_corr[,i]=as.numeric(data_corr[,i])
}
data_corr <- data_corr[order(data_corr$RHO_FST,decreasing = T),] 

distance=c()
for (i in 1:nrow(data_corr)){
  distance=c(distance,cophenetic(tree)[which(data_corr$SP1[i]==row.names(cophenetic(tree))),which(data_corr$SP2[i]==colnames(cophenetic(tree)))])
}

data_corr$distance=distance

signif_Fst=c()
signif_DXY=c()

for (i in 1:nrow(data_corr)){
  if (data_corr[i,]$pvalue_FST<=0.05){
    signif_Fst=c(signif_Fst,"Signif")
  } else {
    signif_Fst=c(signif_Fst,"No")
  }
  if (data_corr[i,]$pvalue_DXY<=0.05){
    signif_DXY=c(signif_DXY,"Signif")
  } else {
    signif_DXY=c(signif_DXY,"No")
  }
}

data_corr$signif_fst=signif_Fst
data_corr$signif_dxy=signif_DXY

data_corr=data_corr[order(data_corr$R2_FST,decreasing=T),]
data_name=head(data_corr,n=6)

data_name$italic_species=data_name$italic_species=c("italic('D. puntazzo - L. mormyrus')",
                                                    "italic('D. puntazzo - S. cantharus')",
                                                    "italic('L. mormyrus - S. cantharus')",
                                                    "italic('D. labrax - D. puntazzo')",
                                                    "italic('D. labrax - L. mormyrus')",
                                                    "italic('D. labrax - S. cantharus')")



data_name<-data_name[,c(1,2,4,9,10,12)] %>%
  melt(id.vars=c("SP1","SP2","distance","signif_fst","italic_species"))

m1 <- nls(R2_FST ~ a * exp(-S * distance) + K, data=data_corr,start=list(a=3,S=40,K=-1))

data_tmp=data.frame(X=seq(min(data_corr$R2_FST),
                          max(data_corr$R2_FST),length.out=1000),
                    Y=rep(0,
                          length(seq(min(data_corr$R2_FST),max(data_corr$R2_FST),length.out=1000))))

data_tmp$Y=coefficients(summary(m1))[1]*exp(-coefficients(summary(m1))[2]*data_tmp$X) + coefficients(summary(m1))[3]

p2<-data_corr[,c(1,2,4,9,10)] %>%
  melt(id.vars=c("SP1","SP2","distance","signif_fst")) %>%
  ggplot(aes(x=distance,y=value)) +
  theme_classic() +
  theme(legend.position = 'none',
        axis.text.x=element_blank()) +
  #geom_line(aes(x=distance,y=mean),col=viridis::viridis(1),size=1) +
  geom_point(aes(col=signif_fst),size=3,alpha=0.75,shape=19) +
  #xlab("Phylogenetic distance") +
  xlab("") +
  ylab("Slope between species Fst") +
  geom_hline(yintercept=0,col="red",lty=2) +
  ylim(c(0,1)) +
  xlim(c(min(data_corr$R2_FST),
         max(data_corr$R2_FST))) +
  geom_line(data=data_tmp,aes(x=X,y=Y),size=1.5,alpha=0.75,col="gray") +
  geom_text_repel(data=data_name,aes(label=italic_species),col="black",parse=T,force=200,ylim=c(0.1,1),xlim=c(0.1,1)) +
  scale_colour_manual(name="Significant correlation",
                      values=viridis::viridis(2,begin=0.25,end=0.75),
                      labels=c("No","Yes"))  +
  ylab(expression((R^2)[F[ST]]))

p2

m1 <- lm(R2_DXY ~distance, data=data_corr)
data_tmp=data.frame(X=seq(min(data_corr$distance),
                          max(data_corr$distance),length.out=1000),
                    Y=rep(0,
                          length(seq(min(data_corr$distance),max(data_corr$distance),length.out=1000))))

data_tmp$Y=coefficients(summary(m1))[1,1] + coefficients(summary(m1))[2,1]*data_tmp$X
p3<-data_corr[,c(1,2,7,9,11)] %>%
  melt(id.vars=c("SP1","SP2","distance","signif_dxy")) %>%
  ggplot(aes(x=distance,y=value)) +
  theme_classic() +
  #geom_line(aes(x=distance,y=mean),col=viridis::viridis(1),size=1) +
  geom_point(aes(col=signif_dxy),size=3,alpha=0.75,shape=19) +
  xlab("Phylogenetic distance") +
  ylab("Slope between species Fst") +
  geom_hline(yintercept=0,col="red",lty=2) +
  ylim(c(0,0.25)) +
  xlim(c(min(data_corr$R2_FST),
         max(data_corr$R2_FST))) +
  geom_line(data=data_tmp,aes(x=X,y=Y),size=1.5,alpha=0.75,col="gray") +
  #geom_text_repel(data=data_name,aes(label=italic_species),col="black",parse=T,force=200,ylim=c(0.1,1),xlim=c(0.1,1)) +
  scale_colour_manual(name="Significant correlation",
                      values=viridis::viridis(2,begin=0.25,end=0.75),
                      labels=c("No","Yes"))  +
  ylab(expression((R^2)[d[xy]]))

p3

data_corr=data_corr[order(data_corr$R2_DXY,decreasing=T),]

tiff(paste("/Users/divco/Documents/COGEDIV/figures/article_fig_6.tiff",sep=""),width=7.5,height=7.5,units="in",res=2000)

dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig6.jpg",sep=""),width=7.5,height=7.5,units="in",res=2000)
print(ggpubr::ggarrange(p2,p3,
                        common.legend = T,
                        nrow=2,
                        ncol=1,labels=c("A","B")))
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig6.pdf",sep=""),width=7.5,height=7.5)
print(ggpubr::ggarrange(p2,p3,
                        common.legend = T,
                        nrow=2,
                        ncol=1,labels=c("A","B")))
dev.off()
