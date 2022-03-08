library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(pBrackets) 
library(ggpubr)

setwd("C:/Users/divco/Documents/TWISTT/Scant/")

color_med_atl=data.frame(Location=c("Li","Mu","Fa","Ga"),
                         Col=brewer.pal(n = 4, name = "RdBu"))

lg=list.files()

data_chromo=data.frame(TOPO=c(NA),
                       VAL=c(NA),
                       CHROM=c(NA),
                       NBR_WINDOW=c(NA))

data_window=data.frame(LEFT=c(NA),
                       RIGHT=c(NA),
                       TOPO=c(NA),
                       BOTTOM=c(NA),
                       TOP=c(NA),
                       CHROM=c(NA))

myplots <- vector('list', length(lg))

LEFT_LEFT=0
for (ll in lg){
  ##
  a<-read.table(ll,header=T,sep="\t")
  topo1=sum(a$topo1,na.rm=T)
  topo2=sum(a$topo2,na.rm=T)
  topo3=sum(a$topo3,na.rm=T)
  
  data_all=data.frame(TOPO=c("topo1","topo2","topo3"),
                      VAL=c(topo1,topo2,topo3))
  
  data_all$TOPO=factor(data_all$TOPO,levels=c("topo1","topo2","topo3"),
                       labels=c("(Fa;Ga);(Li;Mu)",
                                "(Fa;Mu);(Ga;Li)",
                                "(Fa;Li);(Ga;Mu)"))
  
  data_all$CHROM=as.character(qdapRegex::ex_between(ll, "_", ".")[[1]])
  data_all$NBR_WINDOW=as.character(nrow(a))
  data_chromo=rbind(data_chromo,
                    data_all
  )
  ##
  window=1
  a<-read.table(ll,header=T,sep="\t")
  
  a$left=seq(LEFT_LEFT,nrow(a)-1+LEFT_LEFT)
  a$right=seq(LEFT_LEFT+1,nrow(a)+LEFT_LEFT)
  
  #seq(0,nrow(a),by=window)
  data_mean=data.frame(LEFT=c(NA),
                       RIGHT=c(NA),
                       TOPO=c(NA),
                       BOTTOM=c(NA),
                       TOP=c(NA))
  
  for (i in seq(LEFT_LEFT,nrow(a)+LEFT_LEFT,by=window)[-1]){
    tt<-a[a$left>=(i-window) & a$right<=i,]
    
    topo1=mean(tt$topo1)
    topo2=mean(tt$topo2)
    topo3=mean(tt$topo3)
    
    topo2=sum(topo1+topo2)
    topo3=sum(topo2+topo3)
    
    data_mean=rbind(data_mean,
                    c(i-window,i,"topo1",0,round(topo1,3)),
                    c(i-window,i,"topo2",round(topo1,3),round(topo2,3)),
                    c(i-window,i,"topo3",round(topo2,3),round(topo3,3)))
    
    
  }
  
  data_mean=data_mean[-1,]
  
  LEFT_LEFT=tail(a$right,1)
  
  data_mean$LEFT=as.numeric(data_mean$LEFT)
  data_mean$RIGHT=as.numeric(data_mean$RIGHT)
  data_mean$BOTTOM=as.numeric(data_mean$BOTTOM)
  data_mean$TOP=as.numeric(data_mean$TOP)
  data_mean$BOTTOM=data_mean$BOTTOM/10000
  data_mean$TOP=data_mean$TOP/10000
  
  #topo1 = (Fa;Ga);(Li;Mu)
  #topo2 = (Fa;Mu);(Ga;Li)
  #topo3 = (Fa;Li);(Ga;Mu)
  
  data_mean$TOPO=factor(data_mean$TOPO,levels=c("topo1","topo2","topo3"),
                        labels=c("(Fa;Ga);(Li;Mu)",
                                 "(Fa;Mu);(Ga;Li)",
                                 "(Fa;Li);(Ga;Mu)"))
  
  data_mean$CHROM=as.character(qdapRegex::ex_between(ll, "_", ".")[[1]])
  
  data_window=rbind(data_window,
                    data_mean)
  
}


data_chromo=data_chromo[-1,]
data_window=data_window[-1,]

data_chromo_all=data_chromo[1:3,]
data_chromo_all$VAL[1]=sum(data_chromo[data_chromo$TOPO=="(Fa;Ga);(Li;Mu)",]$VAL)
data_chromo_all$VAL[2]=sum(data_chromo[data_chromo$TOPO=="(Fa;Mu);(Ga;Li)",]$VAL)
data_chromo_all$VAL[3]=sum(data_chromo[data_chromo$TOPO=="(Fa;Li);(Ga;Mu)",]$VAL)
sum_all=sum(data_chromo_all$VAL,na.rm=T)
data_chromo_all$VAL=data_chromo_all$VAL/sum_all
all_plot<-ggplot(data_chromo_all,aes(x=TOPO,y=VAL,fill=TOPO))+
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  xlab("") +
  ylab("Topology Weighting (%)") +
  ylim(c(0,1)) +
  geom_text(aes(label=paste(round(VAL*100,2),"%",sep="")), hjust=-0.25, color="black",
            position = position_dodge(0.9), size=3.5) +
  ggtitle(qdapRegex::ex_between(ll, "_", ".")[[1]]) +
  theme(legend.position="right",
        plot.title = element_text(hjust = 0.5))
all_plot
chromRanges=data.frame(CHROM=c(NA),
                       XSTART=c(NA),
                       XEND=c(NA),
                       YSTART=c(NA),
                       YEND=c(NA))
for (i in levels(factor(data_window$CHROM))){
  
  chromRanges=rbind(chromRanges,
                    c(i,
                      data_window$LEFT[which(data_window$CHROM==i)[1]],
                      data_window$RIGHT[tail(which(data_window$CHROM==i),1)],
                      0,
                      1
                    ))
}
chromRanges=chromRanges[-1,]
chromRanges$XSTART=as.numeric(chromRanges$XSTART)
chromRanges$XEND=as.numeric(chromRanges$XEND)
chromRanges$YSTART=as.numeric(chromRanges$YSTART)
chromRanges$YEND=as.numeric(chromRanges$YEND)
p<-ggplot(data_window) +
  geom_rect(aes(xmin = LEFT, xmax = RIGHT, ymax = TOP, ymin=BOTTOM, fill = TOPO)) +
  xlab("") + 
  ylab("") +
  theme_classic() +
  #ggtitle(ind[u+1]) +
  theme_void() +
  geom_hline(yintercept=0.5,lty=2,col="white") +
  annotate("text", x = -50, y= 0.5, label = "0.5") +
  annotate("text", x = -50, y= 1, label = "1.0") +
  annotate("text", x = -50, y= 0.25, label = "0.25") +
  annotate("text", x = -50, y= 0.75, label = "0.75") +
  annotate("text", x = -50, y= 0, label = "0") +
  geom_rect(data = chromRanges[seq(1,nrow(chromRanges),by=2),], aes(xmin = XSTART, xmax = XEND, ymin = YSTART, ymax = YEND), col="black",alpha=0)
p#p

########

cnt=0
for (ll in lg){
  cnt=cnt+1
  a<-read.table(ll,header=T,sep="\t")
  
  #topo1 = (Fa;Ga);(Li;Mu)
  #topo2 = (Fa;Mu);(Ga;Li)
  #topo3 = (Fa;Li);(Ga;Mu)
  
  for (i in 1:nrow(a)){
    a$topo2[i]=sum(a$topo1[i]+a$topo2[i])
    a$topo3[i]=sum(a$topo2[i]+a$topo3[i])
  }
  
  a$topo1=a$topo1/10000
  a$topo2=a$topo2/10000
  a$topo3=a$topo3/10000
  a$left=seq(0,nrow(a)-1)
  a$right=seq(1,nrow(a))
  
  mdata <- melt(a, id=c("left","right")) 
  
  a$topo3 = a$topo2
  a$topo2 = a$topo1
  a$topo1 = 0
  mdata_bottom <- melt(a, id=c("left","right")) 
  
  mdata$bottom=mdata_bottom$value
  
  mdata$value=as.numeric(mdata$value)
  mdata$bottom=as.numeric(mdata$bottom)
  
  mdata$variable=factor(mdata$variable,levels=c("topo1","topo2","topo3"),
                        labels=c("(Fa;Ga);(Li;Mu)",
                                 "(Fa;Mu);(Ga;Li)",
                                 "(Fa;Li);(Ga;Mu)"))
  
  p<-ggplot(mdata) +
    geom_rect(aes(xmin = left, xmax = right, ymax = value, ymin=bottom, fill = variable)) +
    xlab("Genomic Position") + 
    ylab("Topoly weighting (%)") +
    theme_classic() +
    #scale_fill_manual(name = "Location",
    #                  labels = c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
    #                  values = brewer.pal(n = 4, name = "RdBu")) +
    theme_void() +
    geom_hline(yintercept=0.5,lty=2,col="white") +
    annotate("text", x = -250, y= 0.5, label = "0.5") +
    annotate("text", x = -250, y= 1, label = "1.0") +
    annotate("text", x = -250, y= 0.25, label = "0.25") +
    annotate("text", x = -250, y= 0.75, label = "0.75") +
    annotate("text", x = -250, y= 0, label = "0")
  #p
  
  ##
  a<-read.table(ll,header=T,sep="\t")
  topo1=sum(a$topo1,na.rm=T)
  topo2=sum(a$topo2,na.rm=T)
  topo3=sum(a$topo3,na.rm=T)
  all_topo=sum(topo1+topo2+topo3)
  
  topo1=topo1/all_topo
  topo2=topo2/all_topo
  topo3=topo3/all_topo
  
  data_all=data.frame(TOPO=c("topo1","topo2","topo3"),
                      VAL=c(topo1,topo2,topo3))
  
  data_all$TOPO=factor(data_all$TOPO,levels=c("topo1","topo2","topo3"),
                       labels=c("(Fa;Ga);(Li;Mu)",
                                "(Fa;Mu);(Ga;Li)",
                                "(Fa;Li);(Ga;Mu)"))
  
  all_plot<-ggplot(data_all,aes(x=TOPO,y=VAL,fill=TOPO))+
    geom_bar(stat='identity') +
    coord_flip() +
    theme_classic() +
    xlab("") +
    ylab("Topology Weighting (%)") +
    ylim(c(0,1)) +
    geom_text(aes(label=paste(round(VAL*100,2),"%",sep="")), hjust=-0.25, color="black",
              position = position_dodge(0.9), size=3.5) +
    ggtitle(qdapRegex::ex_between(ll, "_", ".")[[1]]) +
    theme(legend.position="right",
          plot.title = element_text(hjust = 0.5))
  
  data_all$CHROM=as.character(qdapRegex::ex_between(ll, "_", ".")[[1]])
  data_all$NBR_WINDOW=as.character(nrow(a))
  data_chromo=rbind(data_chromo,
                    data_all
  )
  ##
  window=10
  a<-read.table(ll,header=T,sep="\t")
  
  a$left=seq(0,nrow(a)-1)
  a$right=seq(1,nrow(a))
  
  #seq(0,nrow(a),by=window)
  data_mean=data.frame(LEFT=c(NA),
                       RIGHT=c(NA),
                       TOPO=c(NA),
                       BOTTOM=c(NA),
                       TOP=c(NA))
  
  for (i in seq(0,nrow(a),by=window)[-1]){
    tt<-a[a$left>=(i-window) & a$right<=i,]
    
    topo1=mean(tt$topo1)
    topo2=mean(tt$topo2)
    topo3=mean(tt$topo3)
    
    topo2=sum(topo1+topo2)
    topo3=sum(topo2+topo3)
    
    data_mean=rbind(data_mean,
                    c(i-window,i,"topo1",0,round(topo1,3)),
                    c(i-window,i,"topo2",round(topo1,3),round(topo2,3)),
                    c(i-window,i,"topo3",round(topo2,3),round(topo3,3)))
    
    
  }
  
  data_mean=data_mean[-1,]
  
  data_mean$LEFT=as.numeric(data_mean$LEFT)
  data_mean$RIGHT=as.numeric(data_mean$RIGHT)
  data_mean$BOTTOM=as.numeric(data_mean$BOTTOM)
  data_mean$TOP=as.numeric(data_mean$TOP)
  data_mean$BOTTOM=data_mean$BOTTOM/10000
  data_mean$TOP=data_mean$TOP/10000
  
  #topo1 = (Fa;Ga);(Li;Mu)
  #topo2 = (Fa;Mu);(Ga;Li)
  #topo3 = (Fa;Li);(Ga;Mu)
  
  data_mean$TOPO=factor(data_mean$TOPO,levels=c("topo1","topo2","topo3"),
                        labels=c("(Fa;Ga);(Li;Mu)",
                                 "(Fa;Mu);(Ga;Li)",
                                 "(Fa;Li);(Ga;Mu)"))
  
  p<-ggplot(data_mean) +
    geom_rect(aes(xmin = LEFT, xmax = RIGHT, ymax = TOP, ymin=BOTTOM, fill = TOPO)) +
    xlab("") + 
    ylab("") +
    theme_classic() +
    #ggtitle(ind[u+1]) +
    theme_void() +
    geom_hline(yintercept=0.5,lty=2,col="white") +
    annotate("text", x = -250, y= 0.5, label = "0.5") +
    annotate("text", x = -250, y= 1, label = "1.0") +
    annotate("text", x = -250, y= 0.25, label = "0.25") +
    annotate("text", x = -250, y= 0.75, label = "0.75") +
    annotate("text", x = -250, y= 0, label = "0")
  #p
  
  p_window=ggarrange(p_window,p,ncol=2)
  twistt_pp<-ggarrange(all_plot,p,nrow=2,heights = c(0.5,0.5),common.legend = T,legend = "bottom")
  
  myplots[[cnt]] <- local({
    twistt_pp
  })
  
  #readline(qdapRegex::ex_between(ll, "_", ".")[[1]])
  
}
print(p_window)
pdf("twistt.pdf",width=50,height=10)
print(ggarrange(myplots[[1]],
                myplots[[2]],
                ncol=2))
dev.off()


pdf("twistt.pdf",width=200,height=10)
print(ggarrange(myplots[[1]],
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
                myplots[[19]],
                myplots[[20]],
                ncol=20))
dev.off()

data_chromo=data_chromo[-1,]
data_chromo$NBR_WINDOW=as.numeric(data_chromo$NBR_WINDOW)
data_chromo_tmp=melt(data_chromo,id=c("CHROM","NBR_WINDOW","TOPO"))
ggplot(data_chromo_tmp[data_chromo_tmp$CHROM!="MT" & data_chromo_tmp!="UN",],aes(x=NBR_WINDOW ,y=value)) +
  geom_point() +
  facet_grid(. ~ TOPO) +
  theme_classic()

summary(lm(value~NBR_WINDOW,data=data_chromo_tmp[data_chromo_tmp$TOPO=="(Fa;Ga);(Li;Mu)" & data_chromo_tmp$CHROM!="MT" & data_chromo_tmp$CHROM!="UN",]))
summary(lm(value~NBR_WINDOW,data=data_chromo_tmp[data_chromo_tmp$TOPO=="(Fa;Li);(Ga;Mu)" & data_chromo_tmp$CHROM!="MT" & data_chromo_tmp$CHROM!="UN",]))
summary(lm(value~NBR_WINDOW,data=data_chromo_tmp[data_chromo_tmp$TOPO=="(Fa;Mu);(Ga;Li)" & data_chromo_tmp$CHROM!="MT" & data_chromo_tmp$CHROM!="UN",]))
