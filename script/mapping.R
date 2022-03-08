load(file="Data/samtools_report.Rdata")
sp_sp=c(
  "Atherina \n boyeri",
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
  "Syngnathus \n typhle"
)
globalstats=samtools_report[[1]]
globalstats$LOCATION=factor(globalstats$LOCATION,levels=c("Li","Mu","Fa","Ga"))
percentage=c()
for (j in seq(1,nrow(globalstats[globalstats$stats=="reads mapped:" | 
                                 globalstats$stats=="reads unmapped:",]),by=2)){
  percentage[(j/2)+0.5]=globalstats[globalstats$stats=="reads mapped:" | 
                                      globalstats$stats=="reads unmapped:",][j,5]/
    (globalstats[globalstats$stats=="reads mapped:" | 
                   globalstats$stats=="reads unmapped:",][j,5]+globalstats[globalstats$stats=="reads mapped:" | 
                                                                             globalstats$stats=="reads unmapped:",][j+1,5])
}
ss=levels(factor(globalstats$SAMPLES))
tt=substr(ss,0,5)

dd=data.frame(SAMPLES=ss,
              SPECIES=tt,
              stats=rep("Percentage",length(tt)),
              LOCATION=substr(ss,6,7),
              value=percentage)
dd$LOCATION=factor(dd$LOCATION,levels=c("Li","Mu","Fa","Ga"))

percentage=c()
for (j in seq(1,nrow(globalstats[globalstats$stats=="reads mapped:" | 
                                 globalstats$stats=="reads duplicated:",]),by=2)){
  percentage[(j/2)+0.5]=globalstats[globalstats$stats=="reads mapped:" | 
                                      globalstats$stats=="reads duplicated:",][j,5]/
    (globalstats[globalstats$stats=="reads mapped:" | 
                   globalstats$stats=="reads duplicated:",][j,5]+globalstats[globalstats$stats=="reads mapped:" | 
                                                                               globalstats$stats=="reads duplicated:",][j+1,5])
}

dd$stats=rep("Mapped",length(dd$stats))

dd_inv=dd
dd_inv$value=1-dd$value
dd_inv$stats="Unmapped"

dd=rbind(dd,dd_inv)

ss=levels(factor(globalstats$SAMPLES))
tt=substr(ss,0,5)

dd_dup=data.frame(SAMPLES=ss,
                  SPECIES=tt,
                  stats=rep("Percentage_duplicated",length(tt)),
                  LOCATION=substr(ss,6,7),
                  value=1-percentage)
dd_dup$LOCATION=factor(dd_dup$LOCATION,levels=c("Li","Mu","Fa","Ga"))

annotation_custom2 <- 
  function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data){ layer(data = data, stat = StatIdentity, position = PositionIdentity, 
                                                                                 geom = ggplot2:::GeomCustomAnn,
                                                                                 inherit.aes = TRUE, params = list(grob = grob, 
                                                                                                                   xmin = xmin, xmax = xmax, 
                                                                                                                   ymin = ymin, ymax = ymax))}

labels_species=c()

for (sp in 1:length(levels(factor(globalstats[globalstats$stats=="reads mapped:",]$SPECIES)))){
  
  labels_species=c(labels_species,
                   paste(levels(factor(globalstats[globalstats$stats=="reads mapped:",]$SPECIES))[sp]," \n n = ",table(globalstats[globalstats$stats=="reads mapped:",]$SPECIES)[sp],sep=" "))
  
}

labels_species=as.vector(labels_species)

dd$SPECIES_plot=factor(dd$SPECIES,labels=c("Atherina boyeri",
                                           "Coryphoblennius galerita",
                                           "Coris julis",
                                           "Dicentrarchus labrax",
                                           "Diplodus puntazzo",
                                           "Engraulis encrasicolus",
                                           "Gobius niuger",
                                           "Hippocampus guttulatus",
                                           "Lophius budegassa",
                                           "Lithognathus mormyrus",
                                           "Merluccius merluccius",
                                           "Mullus surmuletus",
                                           "Pagellus erythrinus",
                                           "Serranus cabrilla",
                                           "Spondyliosoma cantharus",
                                           "Symphodus cinereus",
                                           "Sardina pilchardus",
                                           "Sarda sarda",
                                           "Syngnathus typhle"))


data_tmp=cbind(Species=names(tapply(dd[dd$stats=="Mapped",]$value,dd[dd$stats=="Mapped",]$SPECIES,"mean")),
               Mapped=tapply(dd[dd$stats=="Mapped",]$value,dd[dd$stats=="Mapped",]$SPECIES,"mean")
)


pdf(paste("/Users/divco/Documents/COGEDIV/figures/All_mapped.pdf",sep=""),width=25,height=10)
plot(ggplot(data=dd, aes(x=SAMPLES,y=value,fill=stats)) +
       geom_bar(stat="identity",alpha=0.25)+
       ggtitle("Reads mapped - unmapped")+
       xlab("Samples")+
       ylab("Percentage (%)")+
       facet_wrap(SPECIES_plot~.,scales='free',nrow=3)+
       coord_flip()+
       geom_vline(xintercept=seq(5.5,15.5,by=5),linetype="dashed",col="chartreuse4")+
       scale_fill_manual(values = c("blue","red"),name="")+
       theme_classic()+
       #ylim(c(0,1.9e+8))+
       theme(strip.text.x = element_text(size = 8)) +
       geom_hline(yintercept=0.1,alpha=0.4,col="black",lwd=1.25))
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/All_mapped.jpg",sep=""),width=15,height=17.5,units="in",res=1000)
plot(ggplot(data=dd, aes(x=SAMPLES,y=value,fill=stats)) +
       geom_bar(stat="identity",alpha=0.25)+
       ggtitle("Reads mapped - unmapped")+
       xlab("Samples")+
       ylab("Percentage (%)")+
       facet_wrap(SPECIES_plot~.,scales='free',ncol=4)+
       coord_flip()+
       geom_vline(xintercept=seq(5.5,15.5,by=5),linetype="dashed",col="chartreuse4")+
       scale_fill_manual(values = c("blue","red"),name="")+
       theme_classic()+
       #ylim(c(0,1.9e+8))+
       theme(strip.text.x = element_text(size = 8)) +
       geom_hline(yintercept=0.1,alpha=0.4,col="black",lwd=1.25))
dev.off()