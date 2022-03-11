# Get snp.id
.libPaths("~/R/x86_64-pc-linux-gnu-library/3.6")
library(SNPRelate)
library(ggplot2)
library(SeqArray)
library(viridis)
library(RColorBrewer)
library(progress)
#library(ggpubr)
showfile.gds(closeall=TRUE)

sp="Dlabr"
VCF_PATH=paste("/DATA/sdc1/Pierre/VCF/",sp,"/",sp,"_indel5bp_snponly.vcf.gz",sep="")

if (file.exists(paste("/DATA/sdc1/Pierre/VCF/",sp,"/",sp,".gds",sep=""))==F){
  vcf.fn <- VCF_PATH
  seqVCF2GDS(vcf.fn,paste("/DATA/sdc1/Pierre/VCF/",sp,"/",sp,".gds",sep=""))
}

# OPEN GDS AND RUN PCA
genofile <- seqOpen(paste("/DATA/sdc1/Pierre/VCF/",sp,"/",sp,".gds",sep=""))

pos<-read.gdsn(index.gdsn(genofile,"position"))
id<-read.gdsn(index.gdsn(genofile,"variant.id"))
chrom<-read.gdsn(index.gdsn(genofile,"chromosome"))
SNP<-data.frame(ID=id,
                CHROM=chrom,
                POS=pos
)

window=c(1)
window_length=10000
window_shuffle=10000
start=0
for (i in 2:nrow(SNP)){
  if (SNP$CHROM[i]!=SNP$CHROM[i-1]){
    start=0
    window[i]=window[i-1]+1
  } else {
    if ((SNP$POS[i]-start)>window_length){
      start=start+window_length
      window[i]=window[i-1]+1
    } else {
      window[i]=window[i-1]
    }
  }
}

SNP$WINDOW=factor(window)

save(SNP,file=paste(sp,"_LOCALPCA_WINDOW_DATA_",window_length,".Rdata",sep=""))

ind<-read.gdsn(index.gdsn(genofile,"sample.id"))
n_pc=4

#LOCAL_PCA=vector('list',n_pc)
#for (i in 1:length(LOCAL_PCA)){
#  LOCAL_PCA[[i]]=as.data.frame(matrix(rep(0,(length(ind)+5)*length(levels(SNP$WINDOW))),nrow=length(levels(SNP$WINDOW)),ncol=length(ind)+5))
#  colnames(LOCAL_PCA[[i]])=c("CHROM",
#                             "POS",
#                             "WINDOW",
#                             "PCA",
#                             "PERCENTAGE",
#                             ind)
#}

a=0
pb<-progress_bar$new(format=" dowloading [:bar] :current/:total (:percent) in :elapsedfull",
  total=length(levels(factor(SNP[SNP$CHROM=="LG10",]$WINDOW))),clear=FALSE)
showfile.gds(closeall=TRUE)

for (i in levels(factor(SNP[SNP$CHROM=="LG10",]$WINDOW))){
  genofile <- seqOpen(paste("/DATA/sdc1/Pierre/VCF/",sp,"/",sp,".gds",sep=""))
  a=a+1
  pca <- snpgdsPCA(genofile, num.thread=1,autosome.only = FALSE,
                   snp.id=SNP[SNP$WINDOW==i,]$ID,
                   verbose=F)
  tmp=as.data.frame(as.data.frame(matrix(rep(0,(length(ind)+5)),nrow=1,ncol=length(ind)+5)))
  colnames(tmp)=c("CHROM",
                  "POS",
                  "WINDOW",
                  "PCA",
                  "PERCENTAGE",
                  ind)
  
  for (j in 1:n_pc){
    vec<-c(as.character(SNP[SNP$WINDOW==i,]$CHROM[1]),
           round(mean(c(tail(SNP[SNP$WINDOW==i,]$POS,1),head(SNP[SNP$WINDOW==i,]$POS,1))),0),
           i,
           j,
           pca$varprop[j]*100,
           pca$eigenvect[,j])
    tmp=rbind(tmp,vec)

    #LOCAL_PCA[[j]][a,]=vec
  }
  tmp=tmp[-1,]
  showfile.gds(closeall=TRUE)
  closeAllConnections()
  
  write.table(tmp,file=paste(sp,"_LOCALPCA_",window_length,".csv",sep=""),append=T,row.names=FALSE,col.names = FALSE)

  pb$tick()
  Sys.sleep(1/length(levels(factor(SNP[SNP$CHROM=="LG10",]$WINDOW))))
}

for (j in 1:npc){
  for (i in 2:ncol(LOCAL_PCA[[j]])){
    LOCAL_PCA[[j]][,i]=as.numeric(LOCAL_PCA[[j]][,i])
  }
}

for (j in 1:npc){
  for (i in 4:ncol(LOCAL_PCA[[j]])){
    LOCAL_PCA[[j]][,i]=round(LOCAL_PCA[[j]][,i],4)
  }
}

PLOT_WINDOW=data.frame(CHROM=c(NA),
                       WINDOW=c(NA),
                       INDIV=c(NA),
                       LOCA=c(NA),
                       EIGEN=c(NA))

which_pca=2
for (i in 1:323){
  for (j in 3:ncol(LOCAL_PCA[[which_pca]])){
    PLOT_WINDOW=rbind(PLOT_WINDOW,
                      c(NA,
                        LOCAL_PCA[[which_pca]]$WINDOW[i],
                        colnames(LOCAL_PCA[[which_pca]])[j],
                        substr(colnames(LOCAL_PCA[[which_pca]])[j],6,7),
                        LOCAL_PCA[[which_pca]][i,j]))
  }
}

color_med_atl=brewer.pal(n=4,name="RdBu")
PLOT_WINDOW=PLOT_WINDOW[-1,]
PLOT_WINDOW$WINDOW=as.numeric(PLOT_WINDOW$WINDOW)
PLOT_WINDOW$EIGEN=as.numeric(PLOT_WINDOW$EIGEN)
PLOT_WINDOW$LOCA=factor(PLOT_WINDOW$LOCA,levels=c("Li","Mu","Fa","Ga"))
p1<-ggplot(PLOT_WINDOW,aes(x=WINDOW,y=EIGEN,col=LOCA))+
  geom_line(aes(group=INDIV))+
  #geom_point()+
  theme_bw()+
  scale_colour_manual(name="Location",
                      values=color_med_atl,
                      labels=c("Gulf of Lion",
                               "Costa Calida",
                               "Algarve",
                               "Bay of Biscay")) +
  ylab("Eigen value")+
  xlab("Genomic position")+
  ylim(c(-1,1))
#xlim(c(230,260))
LOCAL_PCA[[which_pca]]$WINDOW=as.numeric(LOCAL_PCA[[which_pca]]$WINDOW)
LOCAL_PCA[[which_pca]]$PERCENTAGE=as.numeric(LOCAL_PCA[[which_pca]]$PERCENTAGE)

p2<-ggplot(LOCAL_PCA[[which_pca]],aes(x=WINDOW,y=PERCENTAGE))+
  geom_line()+
  theme_bw()+
  ylab("Percentage (%)")+
  xlab("")

ggarrange(p2,p1,nrow=2,heights = c(0.2,0.8),legend="bottom")
