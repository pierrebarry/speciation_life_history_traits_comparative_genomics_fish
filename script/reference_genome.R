library(RColorBrewer)
library(formattable)
library(ggplot2)

#seqkit fx2tab --length --name referencegenome_Afall.fa | cut -f2

setwd("COGEDIV")

annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                                                                                 geom = ggplot2:::GeomCustomAnn,
                                                                                 inherit.aes = TRUE, params = list(grob = grob, 
                                                                                                                   xmin = xmin, xmax = xmax, 
                                                                                                                   ymin = ymin, ymax = ymax))}

color_med_atl=data.frame(Location=c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
                         Col=brewer.pal(n = 4, name = "RdBu"))

distri_length_reference_genome=vector('list',length(list.files("data/AAAA/")))
for (i in 1:length(list.files("Data/AAAA/"))){
  a<-read.table(paste("Data/AAAA/",list.files("Data/AAAA/")[i],sep=""))
  a<-a[,1]
  distri_length_reference_genome[[i]]=a
}
names(distri_length_reference_genome)=list.files("Data/AAAA/")
sp=list.files("Data/AAAA/")

length_distri=data.frame(Species=c(NA),
                         Num=c(NA),
                         length=c(NA))
for (i in 1:length(sp)){
  
  data_tmp=data.frame(Species=rep(sp[i],length(distri_length_reference_genome[[i]])),
                      Num=seq(1,length(distri_length_reference_genome[[i]])),
                      length=distri_length_reference_genome[[i]])
  
  length_distri=rbind(length_distri,
                      data_tmp)
  
}

length_distri=length_distri[-1,]

mean_length=c()
number_scaff=c()
for (i in 1:length(levels(factor(length_distri$Species)))){
  mean_length[i]=mean(length_distri$length[length_distri$Species==levels(factor(length_distri$Species))[i]])
  number_scaff[i]=length(length_distri$length[length_distri$Species==levels(factor(length_distri$Species))[i]])
}
names(mean_length)=levels(factor(length_distri$Species))
names(number_scaff)=levels(factor(length_distri$Species))

DATA_REFERENCE_GENOME=data.frame(Species=names(mean_length),
                                 Mean_length=as.vector(mean_length)/1000,
                                 Number_scaffold=as.vector(number_scaff))

ref_table=data.frame(NAME=c("0-1e+4",
                            "1e+4-1e+5",
                            "1e+5-1e+6",
                            "1e+6-1e+7",
                            "1e+7-1e+8"),
                     MIN=c(0,1e+4,1e+5,1e+6,1e+7),
                     MAX=c(1e+4,1e+5,1e+6,1e+7,1e+8))


tmp=ifelse(length_distri$length < 1e+4, "0-1e+4", length_distri$length)
tmp=ifelse(length_distri$length >= 1e+4 & length_distri$length<1e+5, "1e+4-1e+5", tmp)
tmp=ifelse(length_distri$length >= 1e+5 & length_distri$length<1e+6, "1e+5-1e+6", tmp)
tmp=ifelse(length_distri$length >= 1e+6 & length_distri$length<1e+7, "1e+6-1e+7", tmp)
tmp=ifelse(length_distri$length >= 1e+7 & length_distri$length<1e+8, "1e+7-1e+8", tmp)

length_distri$group=tmp

length_distri$Species=factor(length_distri$Species,
                             labels=c("Atherina \n boyeri",
                                      "Coryphoblennius \n galerita",
                                      "Coris \n julis",
                                      "Dicentrarchus \n labrax",
                                      "Diplodus \n puntazzo",
                                      "Engraulis \n encrasicolus",
                                      "Gobius \n niger",
                                      "Hippocampus \n guttulatus",
                                      "Lophius \n budegassa",
                                      "Lithognathus \n mormyrus",
                                      "Merluccius \n merluccius",
                                      "Mullus \n surmuletus",
                                      "Pagellus \n erythrinus",
                                      "Serranus \n cabrilla",
                                      "Spondyliosoma \n cantharus",
                                      "Symphodus \n cinereus",
                                      "Sardina \n pilchardus",
                                      "Sarda \n sarda",
                                      "Syngnathus \n typhle"))
p<-ggplot(length_distri,aes(x=length,fill=Species))+
  geom_histogram(alpha=0.5,col="darkblue",bins=200)+
  scale_color_viridis_d()+
  theme_classic()+
  xlab("Length (bp)")+
  ylab("Frequency")+
  scale_x_log10()+
  scale_y_log10()+
  facet_grid(Species ~ .)+
  theme(strip.background = element_rect(fill="white"),
        strip.text.y = element_text(size=12, color="darkblue",
                                    face="italic"))+
  scale_fill_viridis_d()

start=0
end=0.95
vv<-seq(start,end,length.out=20)
vv[19]=0.75
yy<-seq(0.98,0.03,length.out=19)
size.x=0.075

#for (i in 1:length(levels(factor(length_distri$Species)))){
#  img<-readPNG(paste("data/",levels(factor(length_distri$Species))[i],".png",sep=""))
#  #g <- rasterGrob(img, interpolate=TRUE)
#  
#  a1 = annotation_custom2(rasterGrob(img, interpolate=TRUE), xmin=1e7, xmax=1e8, ymin=100, ymax=10000, data=length_distri[1,])
#  
#  p <- p + a1
#  
#  p<-p+
#    annotation_custom(g, 
#                      xmin=1e7, 
#                      xmax=1e8, 
#                      ymin=100,
#                      ymax=1000)
#}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/ref_distri.pdf",sep=""),width=13,height=30)
print(p)
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/ref_distri.jpg",width=1300,height=3000,quality=100,res=100)
print(p)
dev.off()
