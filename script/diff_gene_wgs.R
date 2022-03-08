library(ggplot2)
library(viridis)
library(cowplot)

setwd("~/COGEDIV")

sp=c()
for (i in grep("fst_f3_wgs",list.files("data/"))){
  
  sp=c(sp,strsplit(list.files("data/")[i],"_")[[1]][1])
  
  
}

SPECIES=sp
SPECIES=SPECIES[-c(1,14)]

#### -----
all_gene_data=data.frame(SP=c(NA),
                         GENERAL_STAT=c(NA),
                         STAT=c(NA),
                         DIFF=c(NA),
                         SD_025=c(NA),
                         SD_075=c(NA),
                         PVALUE=c(NA),
                         t=c(NA),
                         df=c(NA))

for (sp in SPECIES){
  
  ## PI
  
  pi_wgs=read.csv(paste("data/pi_",sp,".csv",sep=""),sep=";")
  pi_gene=read.csv(paste("data/pi_gene_",sp,".txt",sep=""),sep=";")
  
  info=unlist(strsplit(pi_gene$GENE,":"))
  chrom=info[seq(1,length(info),by=2)]
  position=unlist(strsplit(info[seq(2,length(info),by=2)],"-"))
  start=position[seq(1,length(position),by=2)]
  end=position[seq(2,length(position),by=2)]
  
  colnames(pi_wgs)[1]="CHROM"
  #gc_wgs=read.csv(paste("data/gc_content_",sp,".csv",sep=""),sep=",")
  #all_gc=merge(x=pi_wgs,y=gc_wgs,by=c("CHROM","START","END"))
  
  #plot(all_gc[all_gc$POP=="All",]$GC,all_gc[all_gc$POP=="All",]$PI_ALL)
  #summary(lm(all_gc[all_gc$POP=="All",]$PI_ALL~all_gc[all_gc$POP=="All",]$GC))
  
  pi_gene$chrom=chrom
  pi_gene$start=start
  pi_gene$end=end
  
  pi_gene$start=as.numeric(pi_gene$start)
  pi_gene$end=as.numeric(pi_gene$end)
  
  toremove=c()
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = nrow(pi_wgs), width = 300)
  for (row in 1:nrow(pi_wgs)){
    
    tmp=pi_gene[pi_gene$chrom==pi_wgs[row,1] & pi_gene$start>=pi_wgs[row,2] & pi_gene$end<=pi_wgs[row,3],]
    
    if (nrow(tmp)>0){
      toremove=c(toremove,row)
    }
    
    setWinProgressBar(pb, row, title=paste( round(row/nrow(pi_wgs)*100, 0),
                                            "% done"))
    
  }
  
  if (is.null(toremove)==F){
    pi_wgs=pi_wgs[-toremove,]
  }
  
  pi_gene$POP=factor(pi_gene$POP,levels=c("all","Li","Mu","Fa","Ga"),
                     labels=c("All","Li","Mu","Fa","Ga"))
  
  new_data=data.frame(CLASS=c(rep("WGS",nrow(pi_wgs)),rep("GENE",nrow(pi_gene))),
                      POP=c(pi_wgs$POP,as.character(pi_gene$POP)),
                      PI=c(pi_wgs$PI_ALL,pi_gene$PI_ALL))
  
  
  new_data$CLASS=factor(new_data$CLASS)
  new_data$POP=factor(new_data$POP)
  new_data$PI=as.numeric(new_data$PI)
  p<-ggplot(new_data,aes(x=CLASS,y=PI)) +
    geom_jitter(aes(color=CLASS),alpha=0.25) +
    geom_violin(alpha=0.1,fill="black") +
    geom_boxplot(alpha=0.1,fill="black") +
    scale_color_viridis_d() +
    theme_classic() +
    facet_wrap(.~POP,ncol=5) +
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="chartreuse4", fill="chartreuse4",alpha=0.75) +
    ylim(quantile(new_data$PI[is.na(new_data$PI)==F],probs=c(0.005,0.995)))
  
  pdf(paste("/Users/divco/Documents/COGEDIV/figures/gene_pi_",sp,".pdf",sep=""),width=20,height=5)
  print(p)
  dev.off()
  
  test<-t.test(new_data[new_data$POP=="All" & new_data$CLASS=="WGS",]$PI, 
               new_data[new_data$POP=="All" & new_data$CLASS=="GENE",]$PI, 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "pi",
                        "All",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP=="Li" & new_data$CLASS=="WGS",]$PI, 
               new_data[new_data$POP=="Li" & new_data$CLASS=="GENE",]$PI, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "pi",
                        "Li",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP=="Mu" & new_data$CLASS=="WGS",]$PI, 
               new_data[new_data$POP=="Mu" & new_data$CLASS=="GENE",]$PI, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "pi",
                        "Mu",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP=="Fa" & new_data$CLASS=="WGS",]$PI, 
               new_data[new_data$POP=="Fa" & new_data$CLASS=="GENE",]$PI, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "pi",
                        "Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP=="Ga" & new_data$CLASS=="WGS",]$PI, 
               new_data[new_data$POP=="Ga" & new_data$CLASS=="GENE",]$PI, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "pi",
                        "Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  ## DXY
  
  dxy_wgs=read.csv(paste("data/dxy_",sp,".csv",sep=""),sep=";")
  dxy_gene=read.csv(paste("data/dxy_gene_",sp,".txt",sep=""),sep=";")
  
  info=unlist(strsplit(dxy_gene$GENE,":"))
  chrom=info[seq(1,length(info),by=2)]
  position=unlist(strsplit(info[seq(2,length(info),by=2)],"-"))
  start=position[seq(1,length(position),by=2)]
  end=position[seq(2,length(position),by=2)]
  
  #colnames(dxy_wgs)[1]="CHROM"
  #gc_wgs=read.csv(paste("data/gc_content_",sp,".csv",sep=""),sep=",")
  #all_gc=merge(x=dxy_wgs,y=gc_wgs,by=c("CHROM","START","END"))
  
  #plot(all_gc[all_gc$POP1=="Li" & all_gc$POP2=="Ga",]$GC,all_gc[all_gc$POP1=="Li" & all_gc$POP2=="Ga",]$DXY_ALL)
  #summary(lm(all_gc[all_gc$POP=="All",]$DXY_ALL~all_gc[all_gc$POP=="All",]$GC))
  
  dxy_gene$chrom=chrom
  dxy_gene$start=start
  dxy_gene$end=end
  
  dxy_gene$start=as.numeric(dxy_gene$start)
  dxy_gene$end=as.numeric(dxy_gene$end)
  
  toremove=c()
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = nrow(dxy_wgs), width = 300)
  for (row in 1:nrow(dxy_wgs)){
    
    tmp=dxy_gene[dxy_gene$chrom==dxy_wgs[row,1] & dxy_gene$start>=dxy_wgs[row,2] & dxy_gene$end<=dxy_wgs[row,3],]
    
    if (nrow(tmp)>0){
      toremove=c(toremove,row)
    }
    
    setWinProgressBar(pb, row, title=paste( round(row/nrow(dxy_wgs)*100, 0),
                                            "% done"))
    
  }
  
  if (is.null(toremove)==F){
    dxy_wgs=dxy_wgs[-toremove,]
  }
  
  
  for (ee in 1:nrow(dxy_gene)){
    
    if (dxy_gene[ee,]$POP1=="Fa" & dxy_gene[ee,]$POP2=="Li"){
      dxy_gene[ee,]$POP1="Li"
      dxy_gene[ee,]$POP2="Fa"
    }
    
    if (dxy_gene[ee,]$POP1=="Fa" & dxy_gene[ee,]$POP2=="Mu"){
      dxy_gene[ee,]$POP1="Mu"
      dxy_gene[ee,]$POP2="Fa"
    }
    
    if (dxy_gene[ee,]$POP1=="Ga" & dxy_gene[ee,]$POP2=="Li"){
      dxy_gene[ee,]$POP1="Li"
      dxy_gene[ee,]$POP2="Ga"
    }
    
    if (dxy_gene[ee,]$POP1=="Ga" & dxy_gene[ee,]$POP2=="Mu"){
      dxy_gene[ee,]$POP1="Mu"
      dxy_gene[ee,]$POP2="Ga"
    }
    
  }
  new_data=data.frame(CLASS=c(rep("WGS",nrow(dxy_wgs)),rep("GENE",nrow(dxy_gene))),
                      POP1=c(dxy_wgs$POP1,as.character(dxy_gene$POP1)),
                      POP2=c(dxy_wgs$POP2,as.character(dxy_gene$POP2)),
                      dxy=c(dxy_wgs$DXY_ALL,dxy_gene$DXY_ALL))
  
  
  new_data$CLASS=factor(new_data$CLASS)
  new_data$POP1=factor(new_data$POP1)
  new_data$POP2=factor(new_data$POP2)
  
  new_data$dxy=as.numeric(new_data$dxy)
  p<-ggplot(new_data,aes(x=CLASS,y=dxy)) +
    geom_jitter(aes(color=CLASS),alpha=0.25) +
    geom_violin(alpha=0.1,fill="black") +
    geom_boxplot(alpha=0.1,fill="black") +
    scale_color_viridis_d() +
    theme_classic() +
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="chartreuse4", fill="chartreuse4",alpha=0.75) +
    facet_grid(POP1~POP2) +
    ylim(quantile(new_data$PI[is.na(new_data$dxy)==F],probs=c(0.01,0.99)))
  
  pdf(paste("/Users/divco/Documents/COGEDIV/figures/gene_dxy_",sp,".pdf",sep=""),width=7.5,height=5)
  print(p)
  dev.off()
  
  test<-t.test(new_data[new_data$POP1=="Li" & new_data$POP2=="Mu" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Li" & new_data$POP2=="Mu" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Li-Mu",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP1=="Li" & new_data$POP2=="Fa" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Li" & new_data$POP2=="Fa" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Li-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP1=="Li" & new_data$POP2=="Ga" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Li" & new_data$POP2=="Ga" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Li-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP1=="Mu" & new_data$POP2=="Fa" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Mu" & new_data$POP2=="Fa" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Mu-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP1=="Mu" & new_data$POP2=="Ga" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Mu" & new_data$POP2=="Ga" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Mu-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(new_data[new_data$POP1=="Fa" & new_data$POP2=="Ga" & new_data$CLASS=="WGS",]$dxy, 
               new_data[new_data$POP1=="Fa" & new_data$POP2=="Ga" & new_data$CLASS=="GENE",]$dxy, 
               alternative = "two.sided", 
               var.equal = FALSE)
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "dxy",
                        "Fa-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  ## FST
  
  fst_wgs=read.csv(paste("data/",sp,"_fst_f3_wgs.csv",sep=""),sep=",",header=T)
  fst_gene=read.csv(paste("data/",sp,"_BUSCO_stats.csv",sep=""))
  
  fst_wgs=fst_wgs[,c(1,2,3,10:15)]
  fst_gene=fst_gene[,c(4,5,6,18,20,22,24,26,28)]
  #f3_wgs=f3_wgs[,c(1,2,3,19:34)]
  #f3_gene=f3_gene[,c(4,5,6,42,45,47,50,53,56,59,62,65,68,71,74)]
  
  toremove=c()
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = nrow(fst_wgs), width = 300)
  for (row in 1:nrow(fst_wgs)){
    
    tmp=fst_wgs[fst_gene$CHROM==fst_wgs[row,1] & fst_gene$START>=fst_wgs[row,2] & fst_gene$END<=fst_wgs[row,3],]
    
    if (nrow(tmp)>0){
      toremove=c(toremove,row)
    }
    
    setWinProgressBar(pb, row, title=paste( round(row/nrow(fst_wgs)*100, 0),
                                            "% done"))
    
  }
  
  if (is.null(toremove)==F){
    fst_wgs=fst_wgs[-toremove,]
  }
  
  
  new_data=data.frame(CLASS=c(rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene)),
                              rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene)),
                              rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene)),
                              rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene)),
                              rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene)),
                              rep("WGS",nrow(fst_wgs)),rep("GENE",nrow(fst_gene))
  ),
  POP=c(rep("Li-Mu",nrow(fst_wgs)+nrow(fst_gene)), 
        rep("Li-Fa",nrow(fst_wgs)+nrow(fst_gene)),
        rep("Li-Ga",nrow(fst_wgs)+nrow(fst_gene)),
        rep("Mu-Fa",nrow(fst_wgs)+nrow(fst_gene)),
        rep("Mu-Ga",nrow(fst_wgs)+nrow(fst_gene)),
        rep("Fa-Ga",nrow(fst_wgs)+nrow(fst_gene))),
  fst=c(fst_wgs[,4],fst_gene[,4], 
        fst_wgs[,5],fst_gene[,5], 
        fst_wgs[,6],fst_gene[,6], 
        fst_wgs[,7],fst_gene[,7], 
        fst_wgs[,8],fst_gene[,8], 
        fst_wgs[,9],fst_gene[,9]))
  
  
  new_data$CLASS=factor(new_data$CLASS)
  new_data$POP=factor(new_data$POP)
  new_data$fst=as.numeric(new_data$fst)
  p<-ggplot(new_data,aes(x=CLASS,y=fst)) +
    geom_jitter(aes(color=CLASS),alpha=0.25) +
    geom_violin(alpha=0.1,fill="black") +
    geom_boxplot(alpha=0.1,fill="black") +
    scale_color_viridis_d() +
    theme_classic() +
    facet_wrap(.~POP,ncol=2) +
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="chartreuse4", fill="chartreuse4",alpha=0.75) +
    ylim(c(0,1))
  
  pdf(paste("/Users/divco/Documents/COGEDIV/figures/gene_fst_",sp,".pdf",sep=""),width=5,height=5)
  print(p)
  dev.off()
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Mu" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Li-Mu" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Li-Mu",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Fa" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Li-Fa" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Li-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Ga" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Li-Ga" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Li-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Mu-Fa" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Mu-Fa" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Mu-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Mu-Ga" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Mu-Ga" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Mu-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Fa-Ga" & new_data$CLASS=="WGS",]$fst), 
               as.numeric(new_data[new_data$POP=="Fa-Ga" & new_data$CLASS=="GENE",]$fst), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "FST",
                        "Fa-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  fst_gene$GENE=paste(fst_gene$CHROM,":",fst_gene$START,"-",fst_gene$END,sep="")
  
  ## f3
  
  f3_wgs=read.csv(paste("data/",sp,"_fst_f3_wgs.csv",sep=""),sep=",",header=T)
  f3_gene=read.csv(paste("data/",sp,"_BUSCO_stats.csv",sep=""))
  
  f3_wgs=f3_wgs[,c(1,2,3,16:27)]
  f3_gene=f3_gene[,c(4,5,6,42,45,47,50,53,56,59,62,65,68,71,74)]
  #f3_wgs=f3_wgs[,c(1,2,3,19:34)]
  #f3_gene=f3_gene[,c(4,5,6,42,45,47,50,53,56,59,62,65,68,71,74)]
  
  toremove=c()
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = nrow(f3_wgs), width = 300)
  for (row in 1:nrow(f3_wgs)){
    
    tmp=f3_wgs[f3_gene$CHROM==f3_wgs[row,1] & f3_gene$START>=f3_wgs[row,2] & f3_gene$END<=f3_wgs[row,3],]
    
    if (nrow(tmp)>0){
      toremove=c(toremove,row)
    }
    
    setWinProgressBar(pb, row, title=paste( round(row/nrow(f3_wgs)*100, 0),
                                            "% done"))
    
  }
  
  f3_wgs=f3_wgs[-toremove,]
  
  new_data=data.frame(CLASS=c(rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene)),
                              rep("WGS",nrow(f3_wgs)),rep("GENE",nrow(f3_gene))
  ),
  POP=c(rep("Li-Mu-Fa",nrow(f3_wgs)+nrow(f3_gene)), 
        rep("Li-Mu-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Li-Fa-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Mu-Li-Fa",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Mu-Li-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Mu-Fa-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Fa-Li-Mu",nrow(f3_wgs)+nrow(f3_gene)), 
        rep("Fa-Li-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Fa-Mu-Ga",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Ga-Li-Mu",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Ga-Li-Fa",nrow(f3_wgs)+nrow(f3_gene)),
        rep("Ga-Mu-Fa",nrow(f3_wgs)+nrow(f3_gene))
  ),
  f3=c(f3_wgs[,4],f3_gene[,4], 
       f3_wgs[,5],f3_gene[,5], 
       f3_wgs[,6],f3_gene[,6], 
       f3_wgs[,7],f3_gene[,8], 
       f3_wgs[,8],f3_gene[,7], 
       f3_wgs[,9],f3_gene[,9],
       f3_wgs[,10],f3_gene[,10], 
       f3_wgs[,11],f3_gene[,11], 
       f3_wgs[,12],f3_gene[,12], 
       f3_wgs[,13],f3_gene[,13], 
       f3_wgs[,14],f3_gene[,14], 
       f3_wgs[,15],f3_gene[,15]
  ))
  
  
  new_data$CLASS=factor(new_data$CLASS)
  new_data$POP=factor(new_data$POP)
  new_data$f3=as.numeric(new_data$f3)
  p<-ggplot(new_data,aes(x=CLASS,y=f3)) +
    geom_jitter(aes(color=CLASS),alpha=0.25) +
    geom_violin(alpha=0.1,fill="black") +
    geom_boxplot(alpha=0.1,fill="black") +
    scale_color_viridis_d() +
    theme_classic() +
    facet_wrap(.~POP,ncol=4) +
    stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="chartreuse4", fill="chartreuse4",alpha=0.75) +
    ylim(c(-1,1))
  
  pdf(paste("/Users/divco/Documents/COGEDIV/figures/gene_f3_",sp,".pdf",sep=""),width=10,height=5)
  print(p)
  dev.off()
  
  new_data=new_data[is.infinite(new_data$f3)==F,]
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Mu-Fa" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Li-Mu-Fa" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Li-Mu-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Mu-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Li-Mu-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Li-Mu-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Li-Fa-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Li-Fa-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Li-Fa-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Mu-Li-Fa" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Mu-Li-Fa" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Mu-Li-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Mu-Li-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Mu-Li-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Mu-Li-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Mu-Fa-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Mu-Fa-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Mu-Fa-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Fa-Li-Mu" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Fa-Li-Mu" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Fa-Li-Mu",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Fa-Li-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Fa-Li-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Fa-Li-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Fa-Mu-Ga" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Fa-Mu-Ga" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Fa-Mu-Ga",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Ga-Li-Mu" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Ga-Li-Mu" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Ga-Li-Mu",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Ga-Li-Fa" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Ga-Li-Fa" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Ga-Li-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  test<-t.test(as.numeric(new_data[new_data$POP=="Ga-Mu-Fa" & new_data$CLASS=="WGS",]$f3), 
               as.numeric(new_data[new_data$POP=="Ga-Mu-Fa" & new_data$CLASS=="GENE",]$f3), 
               alternative = "two.sided", 
               var.equal = FALSE)
  
  all_gene_data=rbind(all_gene_data,
                      c(sp,
                        "f3",
                        "Ga-Mu-Fa",
                        test$estimate[1] - test$estimate[2],
                        test$conf.int[1],
                        test$conf.int[2],
                        test$p.value[1],
                        test$statistic[1],
                        test$parameter[1]
                      ))
  
  f3_gene$GENE=paste(f3_gene$CHROM,":",f3_gene$START,"-",f3_gene$END,sep="")
  
}

save(all_gene_data,file="data/all_gene_data.Rdata")

### -----
load(file="data/dxy_all.Rdata")
load(file="data/all_gene_data.Rdata")

fst<-readxl::read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx")
colnames(fst)[2]="SP"
italic_species=c("italic('A. boyeri')",
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
all_gene_data=all_gene_data[-1,]

all_gene_data=merge(x=all_gene_data,y=dxy_data,by=c("SP"))
all_gene_data=merge(x=all_gene_data,y=fst,by=c("SP"))

all_gene_data$DIFF=as.numeric(all_gene_data$DIFF)

all_gene_data$da_Li_Ga=as.numeric(all_gene_data$da_Li_Ga)
all_gene_data$dxy_Li_Ga=as.numeric(all_gene_data$dxy_Li_Ga)
all_gene_data$pi_All=as.numeric(all_gene_data$pi_All)
all_gene_data$FST_LI_GA_WEIGHTED=as.numeric(all_gene_data$FST_LI_GA_WEIGHTED)
all_gene_data$FST_MU_FA_WEIGHTED=as.numeric(all_gene_data$FST_MU_FA_WEIGHTED)



m1<-lm(DIFF~FST_LI_GA,data=all_gene_data[all_gene_data$GENERAL_STAT=="FST" & all_gene_data$STAT=="Li-Ga",])
p1<-ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="FST" & all_gene_data$STAT=="Li-Ga",],aes(x=FST_LI_GA_WEIGHTED,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=FST_LI_GA_WEIGHTED, yend=0)) +
  geom_point( size=4, aes(color=FST_LI_GA_WEIGHTED)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("Fst Gulf of Lion - Bay of Biscay") +
  ylab("Fst difference Non BUSCO - BUSCO") +
  xlim(c(0,0.7)) +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 

m1<-lm(DIFF~dxy_Li_Ga,data=all_gene_data[all_gene_data$GENERAL_STAT=="dxy" & all_gene_data$STAT=="Li-Ga",])
p2<-ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="dxy" & all_gene_data$STAT=="Li-Ga",],aes(x=dxy_Li_Ga,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=dxy_Li_Ga, yend=0)) +
  geom_point( size=4, aes(color=dxy_Li_Ga)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("dxy Gulf of Lion - Bay of Biscay") +
  ylab("dxy difference Non BUSCO - BUSCO") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 

m1<-lm(DIFF~pi_All,data=all_gene_data[all_gene_data$GENERAL_STAT=="pi" & all_gene_data$STAT=="All",])
p3<-ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="pi" & all_gene_data$STAT=="All",],aes(x=pi_All,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=pi_All, yend=0)) +
  geom_point( size=4, aes(color=pi_All)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab(expression(pi)) +
  ylab("pi difference Non BUSCO - BUSCO") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 

pdf(paste("/Users/divco/Documents/COGEDIV/figures/diff_gene.pdf",sep=""),width=6.5,height=11.5)
ggarrange(p1,p2,p3,nrow=3,labels=c("A","B","C"))
dev.off()

m1<-lm(DIFF~FST_LI_GA,data=all_gene_data[all_gene_data$GENERAL_STAT=="FST" & all_gene_data$STAT=="Li-Ga",])
pdf(paste("/Users/divco/Documents/COGEDIV/figures/fst_diff_gene.pdf",sep=""),width=7.5,height=5)
p1<-ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="FST" & all_gene_data$STAT=="Li-Ga",],aes(x=FST_LI_GA_WEIGHTED,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=FST_LI_GA_WEIGHTED, yend=0)) +
  geom_point( size=4, aes(color=FST_LI_GA_WEIGHTED)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("Fst Gulf of Lion - Bay of Biscay") +
  ylab("Fst difference Non BUSCO - BUSCO") +
  xlim(c(0,0.7)) +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/fst_diff_gene.jpg",width=750,height=500,quality=100,res=100)
ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="FST" & all_gene_data$STAT=="Li-Ga",],aes(x=FST_LI_GA_WEIGHTED,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=FST_LI_GA_WEIGHTED, yend=0)) +
  geom_point( size=4, aes(color=FST_LI_GA_WEIGHTED)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("Fst Gulf of Lion - Bay of Biscay") +
  ylab("Fst difference Non coding - coding") +
  xlim(c(0,0.7)) +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()

m1<-lm(DIFF~dxy_Li_Ga,data=all_gene_data[all_gene_data$GENERAL_STAT=="dxy" & all_gene_data$STAT=="Li-Ga",])
pdf(paste("/Users/divco/Documents/COGEDIV/figures/dxy_diff_gene.pdf",sep=""),width=7.5,height=5)
ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="dxy" & all_gene_data$STAT=="Li-Ga",],aes(x=dxy_Li_Ga,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=dxy_Li_Ga, yend=0)) +
  geom_point( size=4, aes(color=dxy_Li_Ga)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("dxy Gulf of Lion - Bay of Biscay") +
  ylab("dxy difference Non BUSCO - BUSCO") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/dxy_diff_gene.jpg",width=750,height=500,quality=100,res=100)
ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="dxy" & all_gene_data$STAT=="Li-Ga",],aes(x=dxy_Li_Ga,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=dxy_Li_Ga, yend=0)) +
  geom_point( size=4, aes(color=dxy_Li_Ga)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("dxy Gulf of Lion - Bay of Biscay") +
  ylab("dxy difference Non coding - coding") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()

m1<-lm(DIFF~pi_All,data=all_gene_data[all_gene_data$GENERAL_STAT=="pi" & all_gene_data$STAT=="All",])
pdf(paste("/Users/divco/Documents/COGEDIV/figures/pi_diff_gene.pdf",sep=""),width=7.5,height=5)
ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="pi" & all_gene_data$STAT=="All",],aes(x=pi_All,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=pi_All, yend=0)) +
  geom_point( size=4, aes(color=pi_All)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("pi") +
  ylab("pi difference Non BUSCO - BUSCO") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()
jpeg("/Users/divco/Documents/COGEDIV/figures/pi_diff_gene.jpg",width=750,height=500,quality=100,res=100)
ggplot(all_gene_data[all_gene_data$GENERAL_STAT=="pi" & all_gene_data$STAT=="All",],aes(x=pi_All,y=DIFF)) +
  geom_abline(intercept=coefficients(summary(m1))[1,1],slope=coefficients(summary(m1))[2,1],alpha=1,col="grey",lwd=1) +
  geom_segment( aes(xend=pi_All, yend=0)) +
  geom_point( size=4, aes(color=pi_All)) +
  theme_classic() +
  geom_text_repel(aes(label=italic_species),col="black",parse=T) +
  geom_hline(yintercept=0,col="red",lty=2) +
  xlab("pi") +
  ylab("pi difference Non coding - coding") +
  scale_color_viridis_c(begin=0.1,end=0.9) +
  theme(legend.position="none") 
dev.off()


