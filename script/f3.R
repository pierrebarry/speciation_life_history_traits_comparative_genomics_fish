## f3 ----

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

# Aboye ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Aboye",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Aboye_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Aboye",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Aboye_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Afall ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Afall",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Afall_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Afall",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Afall_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()

# Cgale ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Cgale",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Cgale_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Cgale",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Cgale_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Cjuli ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Cjuli",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Cjuli_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Cjuli",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Cjuli_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Dlabr ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Dlabr",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Dlabr_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Dlabr",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Dlabr_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Dpunt ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Dpunt",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Dpunt_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Dpunt",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Dpunt_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Gnige ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Gnige",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Gnige_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Gnige",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Gnige_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Hgutt ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Hgutt",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Hgutt_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Hgutt",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Hgutt_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Lbude ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Lbude",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Lbude_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Lbude",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Lbude_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Lmorm ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Lmorm",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Lmorm_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Lmorm",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Lmorm_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Mmerl ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Mmerl",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Mmerl_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Mmerl",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Mmerl_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Msurm ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Msurm",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Msurm_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Msurm",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Msurm_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Peryt ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Peryt",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Peryt_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Peryt",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Peryt_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Scabr ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scabr",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Scabr_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scabr",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Scabr_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Scant ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scant",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Scant_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scant",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Scant_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Scine ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scine",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Scine_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Scine",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Scine_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Spilc ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Spilc",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Spilc_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Spilc",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Spilc_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Ssard ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Ssard",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Ssard_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Ssard",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Ssard_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()




# Styph ----
vcf_f3_sp=vcf_f3[vcf_f3$Species=="Styph",]

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

vcf_f3_sp[1:3,1]<-cell_spec(vcf_f3_sp[1:3,1], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[4:6,1]<-cell_spec(vcf_f3_sp[4:6,1], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[7:9,1]<-cell_spec(vcf_f3_sp[7:9,1], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[10:12,1]<-cell_spec(vcf_f3_sp[10:12,1], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Gulf of Lion"),2], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Costa Calida"),2], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Algarve"),2], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_1=="Bay of Biscay"),2], color = color_med_atl$Col[4], bold = F)

vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Gulf of Lion"),3], color = color_med_atl$Col[1], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Costa Calida"),3], color = color_med_atl$Col[2], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Algarve"),3], color = color_med_atl$Col[3], bold = F)
vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3]<-cell_spec(vcf_f3_sp[which(vcf_f3_sp$pop_2=="Bay of Biscay"),3], color = color_med_atl$Col[4], bold = F)

kbl(vcf_f3_sp,col.names = c("Admixed population",
                            "Pop.1",
                            "Pop.2",
                            "f3",
                            "se",
                            "Z")
    ,escape = F,align=c(rep('c',times=6))) %>%
  kable_classic ("striped","hover", full_width = T,html_font = "Cambria") %>%
  row_spec(which(vcf_f3_sp$value<0 & vcf_f3_sp$Z<(-3)), bold = T, color = "black")%>%
  save_kable("../species_identity/figures/Styph_f3_table.pdf")

vcf_f3_sp=vcf_f3[vcf_f3$Species=="Styph",]

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
    axis.text.x = element_text(angle=45, vjust = 1, hjust=1)
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
  ylab("f3")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/Styph_f3_plot.pdf",sep=""),width=10,height=5)
print(p)
dev.off()








