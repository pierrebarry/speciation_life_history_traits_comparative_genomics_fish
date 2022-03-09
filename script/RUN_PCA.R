args=(commandArgs(TRUE))
load(file="/home/labosea1/popgen_report_list.Rdata")
load(file="/home/labosea1/PCA_list.Rdata")
#### PCA
.libPaths("~/R/x86_64-pc-linux-gnu-library/3.6")
library(SNPRelate)
library(ggplot2)
library(RColorBrewer)
library(SeqArray)
library(viridis)
setwd("/home/labosea1")

showfile.gds(closeall=TRUE)

sp=args[1]
row=which(sp==names(PCA_list))

VCF_PATH=paste("/DATA/sdb1/Pierre/VCF/",sp,"/",sp,"_indel5bp_snponly.vcf.gz",sep="")

if (file.exists(paste(sp,".gds",sep=""))==F){
  vcf.fn <- VCF_PATH
  seqVCF2GDS(vcf.fn,paste(sp,".gds",sep=""))
}

# OPEN GDS AND RUN PCA
genofile <- seqOpen(paste(sp,".gds",sep=""))

#sample=pca$sample.id
#sample=sample[1:18]

genofile_ld02 <- snpgdsLDpruning(genofile,autosome.only=F)

maf_stat=seq(0,0.5,by=0.05)

pca_noprune=vector('list',length(maf_stat))
names(pca_noprune)=paste("maf",maf_stat,sep="")
for (j in 1:length(maf_stat)){
  pca <- snpgdsPCA(genofile, num.thread=5,autosome.only = FALSE,maf=maf_stat[j])
  #SnpLoad <- snpgdsPCASNPLoading(pca, genofile)
  pca_noprune[[j]]=list(pca[-2])
  #names(pca_noprune[[j]])=c("PCA","Load")
}

pca_prune=vector('list',length(maf_stat))
names(pca_prune)=paste("maf",maf_stat,sep="")
for (j in 1:length(maf_stat)){
  pca <- snpgdsPCA(genofile, num.thread=5,autosome.only = FALSE,snp.id=as.vector(unlist(genofile_ld02)),maf=maf_stat[j])
  #SnpLoad <- snpgdsPCASNPLoading(pca, genofile)
  pca_prune[[j]]=list(pca[-2])
  #names(pca_prune[[j]])=c("PCA","Load")
}

PCA_list[[row]]=list(pca_noprune,pca_prune,as.vector(unlist(genofile_ld02)))
names(PCA_list[[row]])=c("Raw","Prune","SNP_Prune")
save(PCA_list,file="PCA_list.Rdata")

popgen_report_list[[row]]$PCA=pca[-2]
save(popgen_report_list,file="/home/labosea1/popgen_report_list.Rdata")
