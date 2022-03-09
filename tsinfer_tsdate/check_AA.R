#setwd("/shared/projects/abc_fish/tsinfer/LG1A")
#setwd("/shared/projects/abc_fish/tsinfer/Dlabr")
args = commandArgs(trailingOnly=TRUE)
a<-read.table("check_AA.tsv",header=F)
colnames(a)=c("#CHROM","POS","AA","REF","ALT","AF")
cnt=0
pos=c()
freq=c()
for (i in 1:nrow(a)){
  if (a[i,]$AA!=a[i,]$REF & a[i,]$AA!=a[i,]$ALT){
    cnt=cnt+1
    pos=c(pos,a$POS[i])
    freq=c(freq,a$AF[i])
    if (a$AF[i]<0.5){
      a$AA[i]=a$REF[i]
    } else {
      a$AA[i]=a$ALT[i]
    }
  }
}

#hist(freq)

pos=c()
cnt=0
for (i in 1:nrow(a)){
  if (a[i,]$AA!=a[i,]$REF & a[i,]$AA!=a[i,]$ALT){
    cnt=cnt+1
    pos=c(pos,a$POS[i])
  }
}

write.table(a[,1:3], file = paste("ancestral_state_",args[1],"_remove_fake_AA.tsv",sep=""), sep = "\t",
            row.names = FALSE, col.names = TRUE,quote=FALSE)







