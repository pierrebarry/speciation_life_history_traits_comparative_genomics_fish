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

vcf_species=read.table("../species_identity/data/vcf_stats_species.txt",sep=";",header=F)

colnames(vcf_species)<-c("Species",
                         "nvariants",
                         "missing_genotypes",
                         "called_genotypes",
                         "homozygous_genotypes",
                         "homozygous_ref_genotypes",
                         "heterozygous_genotypes",
                         "seg_all",
                         "seg_Li",
                         "seg_Mu",
                         "seg_Fa",
                         "seg_Ga",
                         "seg_Med",
                         "seg_Atl",
                         "seg_Li_Fa",
                         "seg_Li_Ga",
                         "seg_Mu_Fa",
                         "seg_Mu_Ga",
                         "seg_Li_Mu_Fa",
                         "seg_Li_Mu_Ga",
                         "seg_Mu_Fa_Ga",
                         "singleton_all",
                         "singleton_Li",
                         "singleton_Mu",
                         "singleton_Fa",
                         "singleton_Ga",
                         "singleton_Med",
                         "singleton_Atl",
                         "singleton_Li_Fa",
                         "singleton_Li_Ga",
                         "singleton_Mu_Fa",
                         "singleton_Mu_Ga",
                         "singleton_Li_Mu_Fa",
                         "singleton_Li_Mu_Ga",
                         "singleton_Mu_Fa_Ga",
                         "fst_hudson_Li_Mu",
                         "fst_hudson_Li_Mu_se",
                         "fst_hudson_Li_Fa",
                         "fst_hudson_Li_Fa_se",
                         "fst_hudson_Li_Ga",
                         "fst_hudson_Li_Ga_se",
                         "fst_hudson_Mu_Fa",
                         "fst_hudson_Mu_Fa_se",
                         "fst_hudson_Mu_Ga",
                         "fst_hudson_Mu_Ga_se",
                         "fst_hudson_Fa_Ga",
                         "fst_hudson_Fa_Ga_se",
                         "fst_weirandcockerham_Li_Mu",
                         "fst_weirandcockerham_Li_Mu_se",
                         "fst_weirandcockerham_Li_Fa",
                         "fst_weirandcockerham_Li_Fa_se",
                         "fst_weirandcockerham_Li_Ga",
                         "fst_weirandcockerham_Li_Ga_se",
                         "fst_weirandcockerham_Mu_Fa",
                         "fst_weirandcockerham_Mu_Fa_se",
                         "fst_weirandcockerham_Mu_Ga",
                         "fst_weirandcockerham_Mu_Ga_se",
                         "fst_weirandcockerham_Fa_Ga",
                         "fst_weirandcockerham_Fa_Ga_se",
                         "f2_Li_Mu",
                         "f2_se_Li_Mu",
                         "f2_Li_Fa",
                         "f2_se_Li_Fa",
                         "f2_Li_Ga",
                         "f2_se_Li_Ga",
                         "f2_Mu_Fa",
                         "f2_se_Mu_Fa",
                         "f2_Mu_Ga",
                         "f2_se_Mu_Ga",
                         "f2_Fa_Ga",
                         "f2_se_Fa_Ga",
                         "f3_Li_Mu_Fa",
                         "f3_se_Li_Mu_Fa",
                         "f3_z_Li_Mu_Fa",
                         "f3_Li_Mu_Ga",
                         "f3_se_Li_Mu_Ga",
                         "f3_z_Li_Mu_Ga",
                         "f3_Li_Fa_Ga",
                         "f3_se_Li_Fa_Ga",
                         "f3_z_Li_Fa_Ga",
                         "f3_Mu_Li_Fa",
                         "f3_se_Mu_Li_Fa",
                         "f3_z_Mu_Li_Fa",
                         "f3_Mu_Li_Ga",
                         "f3_se_Mu_Li_Ga",
                         "f3_z_Mu_Li_Ga",
                         "f3_Mu_Fa_Ga",
                         "f3_se_Mu_Fa_Ga",
                         "f3_z_Mu_Fa_Ga",
                         "f3_Fa_Li_Mu",
                         "f3_se_Fa_Li_Mu",
                         "f3_z_Fa_Li_Mu",
                         "f3_Fa_Li_Ga",
                         "f3_se_Fa_Li_Ga",
                         "f3_z_Fa_Li_Ga",
                         "f3_Fa_Mu_Ga",
                         "f3_se_Fa_Mu_Ga",
                         "f3_z_Fa_Mu_Ga",
                         "f3_Ga_Li_Mu",
                         "f3_se_Ga_Li_Mu",
                         "f3_z_Ga_Li_Mu",
                         "f3_Ga_Li_Fa",
                         "f3_se_Ga_Li_Fa",
                         "f3_z_Ga_Li_Fa",
                         "f3_Ga_Mu_Fa",
                         "f3_se_Ga_Mu_Fa",
                         "f3_z_Ga_Mu_Fa",
                         "f4_Li_Mu_Fa_Ga",
                         "f4_se_Li_Mu_Fa_Ga",
                         "f4_z_Li_Mu_Fa_Ga",
                         "f4_Mu_Fa_Li_Ga",
                         "f4_se_Mu_Fa_Li_Ga",
                         "f4_z_Mu_Fa_Li_Ga",
                         "f4_Li_Fa_Mu_Ga",
                         "f4_se_Li_Fa_Mu_Ga",
                         "f4_z_Li_Fa_Mu_Ga",
                         "f4_Li_Ga_Mu_Fa",
                         "f4_se_Li_Ga_Mu_Fa",
                         "f4_z_Li_Ga_Mu_Fa"
)

vcf_f3=cbind(vcf_species[,str_detect(colnames(vcf_species),"f3_L")==T],
             vcf_species[,str_detect(colnames(vcf_species),"f3_M")==T],
             vcf_species[,str_detect(colnames(vcf_species),"f3_F")==T],
             vcf_species[,str_detect(colnames(vcf_species),"f3_G")==T])
vcf_f3=cbind(vcf_species[,1],vcf_f3)
colnames(vcf_f3)[1]="Species"
vcf_f3=reshape2::melt(vcf_f3,id="Species")


vcf_se_f3=cbind(vcf_species[,str_detect(colnames(vcf_species),"f3_se_L")==T],
                vcf_species[,str_detect(colnames(vcf_species),"f3_se_M")==T],
                vcf_species[,str_detect(colnames(vcf_species),"f3_se_F")==T],
                vcf_species[,str_detect(colnames(vcf_species),"f3_se_G")==T])
vcf_se_f3=cbind(vcf_species[,1],vcf_se_f3)
colnames(vcf_se_f3)[1]="Species"
vcf_se_f3=reshape2::melt(vcf_se_f3,id="Species")

vcf_f3=cbind(vcf_f3,vcf_se_f3$value)
colnames(vcf_f3)[4]="se"

vcf_z_f3=cbind(vcf_species[,str_detect(colnames(vcf_species),"f3_z_L")==T],
               vcf_species[,str_detect(colnames(vcf_species),"f3_z_M")==T],
               vcf_species[,str_detect(colnames(vcf_species),"f3_z_F")==T],
               vcf_species[,str_detect(colnames(vcf_species),"f3_z_G")==T])
vcf_z_f3=cbind(vcf_species[,1],vcf_z_f3)
colnames(vcf_z_f3)[1]="Species"
vcf_z_f3=reshape2::melt(vcf_z_f3,id="Species")

vcf_f3=cbind(vcf_f3,vcf_z_f3$value)
colnames(vcf_f3)[5]="z"

myplots <- vector('list', 19)

a=0

color_med_atl=data.frame(Location=c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
                         Col=brewer.pal(n = 4, name = "RdBu"))

italic_species=c("A. boyeri",
                 #"A. fallax",
                 "C. galerita",
                 "C. julis",
                 "D. labrax",
                 "D. puntazzo",
                 "G. niger",
                 "H. guttulatus",
                 "L. budegassa",
                 "L. mormyrus",
                 "M. merluccius",
                 "M. surmuletus",
                 "P. erythrinus",
                 "S. cabrilla",
                 "S. cantharus",
                 "S. cinereus",
                 "S. pilchardus",
                 "S. sarda",
                 "S. typhle")

vcf_f3=vcf_f3[vcf_f3$Species!="Afall",]
for (j in levels(factor(vcf_f3$Species))){
  a=a+1
  vcf_f3_sp=vcf_f3[vcf_f3$Species==j,]
  
  vcf_f3_sp$pop_admix=substr(vcf_f3_sp$variable,4,5)
  vcf_f3_sp$pop_1=substr(vcf_f3_sp$variable,7,8)
  vcf_f3_sp$pop_2=substr(vcf_f3_sp$variable,10,11)
  
  vcf_f3_sp=data.frame(pop_admix=vcf_f3_sp$pop_admix,
                       pop_1=vcf_f3_sp$pop_1,
                       pop_2=vcf_f3_sp$pop_2,
                       value=vcf_f3_sp$value,
                       se=vcf_f3_sp$se,
                       Z=vcf_f3_sp$z
  )
  
  vcf_f3_sp$pop_admix=factor(vcf_f3_sp$pop_admix,levels=c("Li","Mu","Fa","Ga")
                             ,labels=c("Gulf of Lion",
                                       "Costa Calida",
                                       "Algarve",
                                       "Bay of Biscay"))
  
  vcf_f3_sp$pop_1=factor(vcf_f3_sp$pop_1,levels=c("Li","Mu","Fa","Ga")
                         ,labels=c("Gulf of Lion",
                                   "Costa Calida",
                                   "Algarve",
                                   "Bay of Biscay"))
  
  vcf_f3_sp$pop_2=factor(vcf_f3_sp$pop_2,levels=c("Li","Mu","Fa","Ga")
                         ,labels=c("Gulf of Lion",
                                   "Costa Calida",
                                   "Algarve",
                                   "Bay of Biscay"))
  
  vcf_f3_sp$value=round(vcf_f3_sp$value,5)
  vcf_f3_sp$se=round(vcf_f3_sp$se,5)
  vcf_f3_sp$Z=round(vcf_f3_sp$Z,5)
  #datatable(vcf_f3_sp, 
  #          class = 'cell-border stripe',
  #          rownames = FALSE,
  #          filter = 'top', options = list(
  #          pageLength = 100,scrollY=500,autowidth=T,scrollX=100))
  
  vcf_f3_sp$pop_admix=as.character(vcf_f3_sp$pop_admix)
  vcf_f3_sp$pop_1=as.character(vcf_f3_sp$pop_1)
  vcf_f3_sp$pop_2=as.character(vcf_f3_sp$pop_2)
  
  
  vcf_f3_sp=vcf_f3[vcf_f3$Species==j,]
  
  vcf_f3_sp$pop_admix=substr(vcf_f3_sp$variable,4,5)
  vcf_f3_sp$pop_1=substr(vcf_f3_sp$variable,7,8)
  vcf_f3_sp$pop_2=substr(vcf_f3_sp$variable,10,11)
  
  vcf_f3_sp=data.frame(pop_admix=vcf_f3_sp$pop_admix,
                       pop_1=vcf_f3_sp$pop_1,
                       pop_2=vcf_f3_sp$pop_2,
                       value=vcf_f3_sp$value,
                       se=vcf_f3_sp$se,
                       Z=vcf_f3_sp$z
  )
  
  vcf_f3_sp$pop_admix=factor(vcf_f3_sp$pop_admix,levels=c("Li","Mu","Fa","Ga")
                             ,labels=c("Gulf of Lion",
                                       "Costa Calida",
                                       "Algarve",
                                       "Bay of Biscay"))
  
  vcf_f3_sp$pop_1=factor(vcf_f3_sp$pop_1,levels=c("Li","Mu","Fa","Ga")
                         ,labels=c("Gulf of Lion",
                                   "Costa Calida",
                                   "Algarve",
                                   "Bay of Biscay"))
  
  vcf_f3_sp$pop_2=factor(vcf_f3_sp$pop_2,levels=c("Li","Mu","Fa","Ga")
                         ,labels=c("Gulf of Lion",
                                   "Costa Calida",
                                   "Algarve",
                                   "Bay of Biscay"))
  
  vcf_f3_sp$value=round(vcf_f3_sp$value,5)
  vcf_f3_sp$se=round(vcf_f3_sp$se,5)
  vcf_f3_sp$Z=round(vcf_f3_sp$Z,5)
  
  vcf_f3_sp$pop1_pop2=paste(vcf_f3_sp$pop_admix,";",vcf_f3_sp$pop_1,"-",vcf_f3_sp$pop_2,sep="")
  
  vcf_f3_sp$pop1_pop2=factor(vcf_f3_sp$pop1_pop2,
                             levels=c(vcf_f3_sp$pop1_pop2))
  
  def=c()
  for (i in 1:nrow(vcf_f3_sp)){
    if (vcf_f3_sp$value[i]<0 & vcf_f3_sp$Z[i]<(-3)){
      def[i]="Negative"
    } else {
      def[i]="Zero"
    }
  }
  vcf_f3_sp$def=def
  
  
  p<-ggplot(vcf_f3_sp,aes(x=pop1_pop2,y=value,fill=pop_admix))+
    geom_bar(stat="identity", aes(color=def))+
    geom_errorbar(aes(ymin=value-se, ymax=value+se),width=.2,
                  position=position_dodge(.9)) +
    labs(title=italic_species[a],parse=T) +
    theme_bw() + 
    #labs(title = "",
    #     x = 'Pop1:Pop2',
    #     y= expression(f[3]),
    #     fill = "Admixed popualtion") +
    scale_fill_manual(values=color_med_atl$Col)+
    scale_color_manual(breaks=c("Negative","Zero"),values=c("chartreuse4","white"))+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle=45, vjust = 1, hjust=1),
      plot.title = element_text(hjust = 0.5,face="italic")
    ) +
    scale_x_discrete(labels=c("Gulf of Lion;Costa Calida-Algarve" = "Costa Calida-Algarve",
                              "Gulf of Lion;Costa Calida-Bay of Biscay" = "Costa Calida-Bay of Biscay",
                              "Gulf of Lion;Algarve-Bay of Biscay" = "Algarve-Bay of Biscay",
                              "Costa Calida;Gulf of Lion-Algarve" = "Gulf of Lion-Algarve",
                              "Costa Calida;Gulf of Lion-Bay of Biscay" = "Gulf of Lion-Bay of Biscay",
                              "Costa Calida;Algarve-Bay of Biscay" = "Algarve-Bay of Biscay",
                              "Algarve;Gulf of Lion-Costa Calida" = "Gulf of Lion-Costa Calida",
                              "Algarve;Gulf of Lion-Bay of Biscay" = "Gulf of Lion-Bay of Biscay",
                              "Algarve;Costa Calida-Bay of Biscay" = "Costa Calida-Bay of Biscay",
                              "Bay of Biscay;Gulf of Lion-Costa Calida" = "Gulf of Lion-Costa Calida",
                              "Bay of Biscay;Gulf of Lion-Algarve" = "Gulf of Lion-Algarve",
                              "Bay of Biscay;Costa Calida-Algarve" = "Costa Calida-Algarve"))+
    xlab("Pop1,Pop2")+
    ylab(expression(f[3]))
  
  img<-png::readPNG(paste("data/",j,".png",sep=""))
  g<-grid::rasterGrob(img,interpolate=T)
  p <- p +
    annotation_custom(g,
                      xmin=0,
                      xmax=4,
                      ymin=max(vcf_f3_sp$value)*0.95,
                      ymax=max(vcf_f3_sp$value)) 
  
  
  myplots[[a]] <- local({
    print(p)
  })
  
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/f3_plot.pdf",sep=""),width=25,height=50)
ggarrange(myplots[[1]],
          myplots[[2]],
          myplots[[3]],
          myplots[[4]],
          myplots[[5]],
          myplots[[6]],
          myplots[[7]],
          myplots[[8]],
          myplots[[9]],
          myplots[[10]],
          myplots[[11]],
          myplots[[12]],
          myplots[[13]],
          myplots[[14]],
          myplots[[15]],
          myplots[[16]],
          myplots[[17]],
          myplots[[18]],
          nrow=9,
          ncol=2,
          common.legend = T,
          legend="top")
dev.off()

