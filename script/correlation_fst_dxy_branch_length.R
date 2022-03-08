library(ggplot2)
library(viridis)
library(cowplot)
library(ggrepel)
setwd("~/COGEDIV")

data_corr=data.frame(SP1=c(NA),
                     SP2=c(NA),
                     RHO_FST=c(NA),
                     R2_FST=c(NA),
                     pvalue_FST=c(NA),
                     RHO_DXY=c(NA),
                     R2_DXY=c(NA),
                     pvalue_DXY=c(NA))

sp=c()
for (i in grep("BUSCO_",list.files("data/"))){
  
  sp=c(sp,strsplit(list.files("data/")[i],"_")[[1]][1])
  
  
}

sp=sp[sp!="Aboye"]
for (sp1 in 1:(length(sp)-1)){
  for (sp2 in ((sp1+1):length(sp))){
    
    fst_gene_1=read.csv(paste("data/",sp[sp1],"_BUSCO_stats.csv",sep=""))
    fst_gene_1$GENE=paste(fst_gene_1$CHROM,":",fst_gene_1$START,"-",fst_gene_1$END,sep="")
    dxy_gene_1=read.csv(paste("data/dxy_gene_",sp[sp1],".txt",sep=""),sep=";")
    all_join_1=merge(x=fst_gene_1,y=dxy_gene_1[dxy_gene_1$POP1=="Ga" & dxy_gene_1$POP2=="Li",],by=c("GENE"))
    
    fst_gene_2=read.csv(paste("data/",sp[sp2],"_BUSCO_stats.csv",sep=""))
    fst_gene_2$GENE=paste(fst_gene_2$CHROM,":",fst_gene_2$START,"-",fst_gene_2$END,sep="")
    dxy_gene_2=read.csv(paste("data/dxy_gene_",sp[sp2],".txt",sep=""),sep=";")
    all_join_2=merge(x=fst_gene_2,y=dxy_gene_2[dxy_gene_2$POP1=="Ga" & dxy_gene_2$POP2=="Li",],by=c("GENE"))
    
    data_gene=data.frame(ID=c(NA),
                         FST_HUDSON_LI_GA_sp1=c(NA),
                         DXY_ALL_sp1=c(NA),
                         FST_HUDSON_LI_GA_sp2=c(NA),
                         DXY_ALL_sp2=c(NA)                       
    )   
    
    for (k in 1:nrow(all_join_1)){
      if (nrow(all_join_2[all_join_2$ID==all_join_1$ID[k],])>0){
        data_gene=rbind(data_gene,
                        c(all_join_1$ID[k],all_join_1$FST_HUDSON_LI_GA[k],
                          all_join_1$DXY_ALL[k],
                          all_join_2[all_join_2$ID==all_join_1$ID[k],]$FST_HUDSON_LI_GA,
                          all_join_2[all_join_2$ID==all_join_1$ID[k],]$DXY_ALL))
      }
      
    }
    
    data_gene=data_gene[-1,]
    
    for (ttt in 2:ncol(data_gene)){
      data_gene[,ttt]=as.numeric(data_gene[,ttt])
    }
    
    ggplot(data_gene,aes(x=DXY_ALL_sp1,y=DXY_ALL_sp2)) +
      theme_classic() +
      geom_point(alpha=0.4)
    
    a<-lm(data_gene$DXY_ALL_sp1~data_gene$DXY_ALL_sp2)   
    b<-lm(data_gene$FST_HUDSON_LI_GA_sp1~data_gene$FST_HUDSON_LI_GA_sp2)   
    
    
    data_corr=rbind(data_corr,
                    c(sp[sp1],sp[sp2],
                      coefficients(b)[2],
                      summary(b)$r.squared,
                      coefficients(summary(b))[2,4],
                      coefficients(a)[2],
                      summary(a)$r.squared,
                      coefficients(summary(a))[2,4]
                    ))
    
  }
}

data_corr=data_corr[-1,]


save(data_corr,file="data/data_corr.Rdata")

data_corr <- data_corr[order(data_corr$RHO_DXY,decreasing = T),] 


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