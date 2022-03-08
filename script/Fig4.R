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

## Fig 4 ----

SPECIES=c()
for (i in grep("_fst_f3_wgs_50kb.csv",list.files("data"))){
  SPECIES=c(SPECIES,strsplit(list.files("data")[i],"_")[[1]][1])
}
SPECIES=SPECIES[SPECIES!="Gnige"]

fst_all=data.frame(SP=c(NA),
                   FST_LI_GA_hudson=c(NA),
                   FST_LI_GA_wc=c(NA))

for (i in SPECIES){
  fst<-read.table(paste("data/",i,"_fst_f3_wgs_50kb.csv",sep=""),sep=",",header=T)
  fst$join=paste(fst$CHROM,"-",fst$START,"-",fst$END)
  
  fst_all=rbind(fst_all,
                data.frame(SP=i,
                           FST_LI_GA_hudson=fst[,12],
                           FST_LI_GA_wc=fst[,6]))
  
}

fst_all=fst_all[-1,]

fst<-read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx")
italic_species=c(
  "italic('A. boyeri')",
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
fst=fst[is.na(fst$FST_LI_GA)==F,]
fst=fst[fst$Species_code!="Gnige",]
library(ggridges)
library(viridis)
fst=fst[order(fst$FST_LI_GA_WEIGHTED,decreasing=T),]
fst_all$SP=factor(fst_all$SP,levels=rev(fst$Species_code))
fst_all$value=fst_all$FST_LI_GA_hudson
fst_all$variable="FST"

load(file="data/data_fd.Rdata")
data_fd=rbind(data_fd,
              data.frame(SP=SPECIES,
                         topo="Ga-Fa-Li",
                         fd=0))
data_fd=data_fd[data_fd$fd>=0 & data_fd$fd<1,]
data_fd$SP=factor(data_fd$SP,levels=rev(fst$Species_code))
data_fd$value=data_fd$fd
data_fd$variable="fd"

delta=read.table("data/Delta_Frac_16_species.txt",sep="\t")
colnames(delta)=c("SP","Delta")
delta$SP=factor(delta$SP,levels=rev(fst$Species_code))
delta$value=delta$Delta
delta$variable="delta_fst"
all_data=rbind(fst_all[,c("SP","variable","value")],
               data_fd[,c("SP","variable","value")],
               delta[,c("SP","variable","value")]
)

all_data$SP=factor(all_data$SP)
all_data$variable=factor(all_data$variable)
all_data$value=as.numeric(all_data$value)

all_data=all_data
all_data$variable=factor(all_data$variable,
                         levels=c("FST","fd","delta_fst"),
                         labels=c("F[ST]",
                                  "f[d]",
                                  "Delta[Fst]"))

all_data=rbind(all_data,
               c("Aboye","Delta[Fst]",0),
               c("Afall","Delta[Fst]",0))


my_strip_labels <- as_labeller(c(
  delta_fst = expression(Delta[Fst]),
  fd = expression(f[d]),
  FST = expression(F[ST]),
  label_parsed
))

all_data$value=as.numeric(all_data$value)
all_data$scale=factor(all_data$variable,
                      labels=c(3,1,3),
                      levels=c("F[ST]","f[d]","Delta[Fst]"))
all_data$scale=as.numeric(all_data$scale)
library('ggthemes')
p<-ggplot(all_data, aes(x = value, y = SP,fill = ..x..,height = ..ndensity..)) +
  geom_density_ridges_gradient(scale=1.5,col="white",alpha=0.5) +
  theme_classic() +
  scale_fill_viridis(begin=0.9,end=0.1,option="E") +
  labs(x=NULL) +  
  facet_wrap(
    ~variable, labeller = label_parsed,  # add labels
    strip.position = "bottom",
    ncol=3,
    scales='free') +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    legend.position = "none",
    panel.border=element_blank(), 
    axis.line=element_line()
  ) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_y_discrete(expand=c(0,0),
                   labels=parse(text=rev(c(fst$italic_species)))) +
  ylab("") 
#p

data_fake=data.frame(X=seq(0,18,length.out=100),
                     Y=seq(0,18,length.out=100))
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  ylim(c(0,18)) +
  xlim(c(0,18))
a=0
for (sp in rev(fst$Species_code)){
  a=a+1
  img<-png::readPNG(paste("data/",sp,".png",sep=""))
  g<-grid::rasterGrob(img,interpolate=T)
  p_fake <- p_fake +
    theme_void() +
    annotation_custom(g,
                      xmin=0,
                      xmax=14,
                      ymin=a,
                      ymax=a+1) 
  
}

p_fake<-p_fake+
  ylim(c(0.6,19.25)) +
  xlim(c(0,14))


p_fig4<-ggpubr::ggarrange(p_fake,p,
                          widths=c(0.1,0.9),
                          nrow=1,ncol=2)

p_fig4
#p_fig4<-ggpubr::ggarrange(p_fd,
#                          p_fig4,
#                          nrow=2,ncol=1,
#                          labels=c("A","B"),
#                          heights = c(0.33,0.66))


jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig4.jpg",sep=""),width=10,height=7.5,units="in",res=2000)
print(p_fig4)
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig4.pdf",sep=""),width=10,height=7.5)
print(p_fig4)
dev.off()



p<-ggplot(fst_all, aes(x = FST_LI_GA_hudson, y = SP,fill = ..x..)) +
  geom_density_ridges_gradient(scale=5,col="white",alpha=0.5) +
  theme_classic() + 
  scale_fill_viridis(begin=0.9,end=0.1,option="E") +
  theme(legend.position = "none") +
  xlab(expression(F[ST])) +
  ylab("") +
  xlim(c(0,1)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_y_discrete(expand=c(0,0),
                   labels=parse(text=rev(c(fst$italic_species))))

data_fake=data.frame(X=seq(0,18,length.out=100),
                     Y=seq(0,18,length.out=100))
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  ylim(c(0,18)) +
  xlim(c(0,18))
a=0
for (sp in rev(fst$Species_code)){
  a=a+1
  img<-png::readPNG(paste("data/",sp,".png",sep=""))
  g<-grid::rasterGrob(img,interpolate=T)
  p_fake <- p_fake +
    theme_void() +
    annotation_custom(g,
                      xmin=0,
                      xmax=14,
                      ymin=a,
                      ymax=a+1) 
  
}

p_fake<-p_fake+
  ylim(c(1,19)) +
  xlim(c(0,14))


fd=read.table("data/fd_simple.csv",header=T,sep=";",dec=",")
colnames(fd)[1]="Species_code"
lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
italic_species=c("italic('A. boyeri')",
                 "italic('A. fallax')",
                 "italic('C. galerita')",
                 "italic('C. julis')",
                 "italic('D. labrax')",
                 "italic('D. puntazzo')",
                 #"italic('E. encrasicolus')",
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
lfh$Fec=as.numeric(lfh$Fec)
lfh=as.data.frame(lfh)
lfh$Fec=log(lfh$Fec)
merge_data=merge(fd,lfh,on=c("Species_code"))
merge_data=merge(merge_data,fst,on=c("Species_code"))

m2<-lm(fd ~ FST_LI_GA_WEIGHTED,data=merge_data)
m1<-lm(fd ~ FST_LI_GA_WEIGHTED + I(FST_LI_GA_WEIGHTED^2),data=merge_data)
lmtest::lrtest(m2,m1)

fitted=c()
for (i in seq(min(merge_data$FST_LI_GA_WEIGHTED),max(merge_data$FST_LI_GA_WEIGHTED),by=0.0001)){
  
  a=coefficients(m1)[1]
  b=coefficients(m1)[2]
  c=coefficients(m1)[3]
  
  fitted=c(fitted,a + b*i + c*(i^(2)))
  
}

data_fitted=data.frame(X=seq(min(merge_data$FST_LI_GA_WEIGHTED),max(merge_data$FST_LI_GA_WEIGHTED),by=0.0001),
                       Y=fitted)

p_fd<-plot(ggplot(merge_data,aes(x=FST_LI_GA_WEIGHTED,y=fd)) +
             geom_line(data=data_fitted,aes(x=X,y=Y),size=1.5,alpha=0.75,col="grey") +
             geom_point(alpha=0.4,size=4,col="chartreuse4",shape=19) +
             geom_text_repel(aes(label=italic_species),col="black",parse=T) +
             theme_classic()+
             xlab(expression(F[ST])) +
             ylab(expression(italic(f[d]))) +
             
             theme(
               axis.title=element_text(size=20,face="bold"),
               axis.text=element_text(size=10)
             ) 
           
)

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/fd.pdf",sep=""),width=5,height=5)
print(p_fd)
dev.off()

p_fd_distri<-ggplot(data_fd[data_fd$fd>=0 & data_fd$fd<1,], aes(x = fd, y = SP,fill = ..x..)) +
  geom_density_ridges_gradient(scale=2.5,col="white",alpha=0.5) +
  theme_classic() + 
  scale_fill_viridis(begin=0.9,end=0.1,option="E") +
  theme(legend.position = "none",
        axis.text.y=element_blank()) +
  xlab(expression(f[d])) +
  ylab("") +
  xlim(c(0,1)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_y_discrete(expand=c(0,0),
                   labels=parse(text=rev(c(fst$italic_species))))




delta=read.table("data/Delta_Frac_16_species.txt",sep="\t")
colnames(delta)=c("SP","Delta")
delta$SP=factor(delta$SP,levels=rev(fst$Species_code))

delta=rbind(delta,
            c("Aboye",0),
            c("Afall",0))

delta$Delta=as.numeric(delta$Delta)
p_delta<-ggplot(delta, aes(x = Delta, y = SP,fill = ..x..)) +
  geom_density_ridges_gradient(scale=1.5,col="white",alpha=0.5) +
  theme_classic() + 
  scale_fill_viridis(begin=0.9,end=0.1,option="E") +
  theme(legend.position = "none",
        axis.text.y=element_blank()) +
  xlab(expression(Delta[Fst])) +
  ylab("") +
  xlim(c(0,1)) +
  scale_x_continuous(expand=c(0,0),
                     limits=c(0,1)) +
  scale_y_discrete(expand=c(0,0),
                   labels=parse(text=rev(c(fst$italic_species))))


#ggplot(data_fd[data_fd$fd>0 & data_fd$fd<1,],aes(x=fd)) +
#  geom_histogram(alpha=0.4,bins=100) +
#  facet_wrap(SP~.,scales="free") +
#  theme_classic() +
#  scale_fill_viridis_d(begin=0.1,end=0.9) +
#  scale_y_continuous(expand=c(0,0)) +
#  scale_x_continuous(expand=c(0,0))
p_fig4<-ggpubr::ggarrange(p_fake,p,p_fd_distri,p_delta,
                          widths=c(0.1,0.35,0.275,0.275),
                          nrow=1,ncol=4)

p_fig4
#p_fig4<-ggpubr::ggarrange(p_fd,
#                          p_fig4,
#                          nrow=2,ncol=1,
#                          labels=c("A","B"),
#                          heights = c(0.33,0.66))


jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig4.jpg",sep=""),width=10,height=7.5,units="in",res=2000)
print(p_fig4)
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig4.pdf",sep=""),width=10,height=7.5)
print(p_fig4)
dev.off()

