library(ggplot2)
library(ggpubr)
library(reshape2)
library(gridExtra)

plot_plot=0

SPECIES=c("Aboye","Afall","Cgale","Cjuli","Dlabr","Dpunt","Hgutt","Lbude","Lmorm","Mmerl","Msurm","Peryt","Scabr","Scant","Scine","Spilc","Ssard","Styph")

data_ABC=data.frame(SP=c(NA),
                    Format=c(NA),
                    Param=c(NA),
                    Method=c(NA),
                    Estimate=c(NA),
                    Sd_2.5=c(NA),
                    Sd_97.5=c(NA),
                    Migr=c(NA)
                    )

for (sp in SPECIES){
  
  for (dirs in list.dirs(paste("/home/labosea1/ABC/",sp,sep=""),recursive=F)){
    
    if (file.exists(paste(dirs,"/table_contrib_PCA_SS.txt",sep=""))==T){
      
      setwd(paste(dirs,"/best_model_5/",sep=""))
      text<-readLines(list.files()[grep('report',list.files())])
      param<-strsplit(text,"\t")
      
      first=strsplit(list.files()[grep('report',list.files())],"_")[[1]]
      second=strsplit(first[3],".txt")[[1]][1]
      first=first[2]
      
      format=paste(first,"_",second,sep="")
      
      for (k in 6:length(param)){
        
        if (floor(k/2)==k/2){
          method="Neural network"
        } else {
          method="Random forest"
        }
        
        yaml<-readLines(paste("../",list.files("../")[grep('config.yaml',list.files("../"))],sep=""))
        general_infos<-readLines(paste("../",list.files("../")[grep('general_infos.txt',list.files("../"))],sep=""))
        
        
        if (strsplit(yaml[grep("modeBarrier",yaml)],": ")[[1]][2]=="beta"){
          migr_method="Beta"
        } else {
          migr_method="Bimodal"
          nLoci=as.numeric(strsplit(general_infos[grep("nLoci",general_infos)],",")[[1]][2])
        }
        
        if (unlist(strsplit(param[k][[1]]," "))[1]=="nBarriersM12" | unlist(strsplit(param[k][[1]]," "))[1]=="nBarriersM21"){
          cc=c(sp,
               format,
               unlist(strsplit(param[k][[1]]," "))[1],
               method,
               as.numeric(unlist(strsplit(param[k][[1]]," "))[3])/nLoci,
               as.numeric(unlist(strsplit(param[k][[1]]," "))[2])/nLoci,
               as.numeric(unlist(strsplit(param[k][[1]]," "))[4])/nLoci,
               migr_method)
        } else {
          cc=c(sp,
               format,
               unlist(strsplit(param[k][[1]]," "))[1],
               method,
               unlist(strsplit(param[k][[1]]," "))[3],
               unlist(strsplit(param[k][[1]]," "))[2],
               unlist(strsplit(param[k][[1]]," "))[4],
               migr_method)
        }

        
        data_ABC=rbind(data_ABC,cc)
        
      }
      
    }  
    
  }
  


  
}


data_ABC=data_ABC[-1,]

data_ABC$Format=factor(data_ABC$Format)


for (i in 1:nrow(data_ABC)){
  
  if (data_ABC$Param[i]=="M12"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "m12",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                & data_ABC$Param=="N2"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                & data_ABC$Param=="N2"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                & data_ABC$Param=="N2"),7][1]) ),
           as.character(data_ABC$Migr[i])
           )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  
  if (data_ABC$Param[i]=="M21"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "m21",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                      & data_ABC$Param=="N1"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                    & data_ABC$Param=="N1"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (4 * as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                     & data_ABC$Param=="N1"),7][1]) ),
           as.character(data_ABC$Migr[i])
    )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  

  
}

for (i in 1:nrow(data_ABC)){
  
  if (data_ABC$Param[i]=="N1"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "N1/Na",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                             & data_ABC$Param=="Na"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                           & data_ABC$Param=="Na"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                            & data_ABC$Param=="Na"),7][1]) ),
           as.character(data_ABC$Migr[i])
    )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  
  if (data_ABC$Param[i]=="N2"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "N2/Na",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                             & data_ABC$Param=="Na"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                           & data_ABC$Param=="Na"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                            & data_ABC$Param=="Na"),7][1]) ),
           as.character(data_ABC$Migr[i])
    )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  
  
  
}

for (i in 1:nrow(data_ABC)){
  
  if (data_ABC$Param[i]=="Tam"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "Tam/Tsplit",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                         & data_ABC$Param=="Tsplit"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                       & data_ABC$Param=="Tsplit"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                        & data_ABC$Param=="Tsplit"),7][1]) ),
           as.character(data_ABC$Migr[i])
    )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  
  if (data_ABC$Param[i]=="Tsc"){
    
    line=c(data_ABC$SP[i],
           as.character(data_ABC$Format[i]),
           "Tsc/Tsplit",
           as.character(data_ABC$Method[i]),
           as.numeric(data_ABC$Estimate[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                         & data_ABC$Param=="Tsplit"),5][1]) ),
           as.numeric(data_ABC$Sd_2.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                       & data_ABC$Param=="Tsplit"),6][1]) ),
           as.numeric(data_ABC$Sd_97.5[i]) / (as.numeric(data_ABC[which(data_ABC$SP[i]==data_ABC$SP & data_ABC$Format[i]==data_ABC$Format & data_ABC$Method[i]==data_ABC$Method
                                                                        & data_ABC$Param=="Tsplit"),7][1]) ),
           as.character(data_ABC$Migr[i])
    )
    
    data_ABC=rbind(data_ABC,line)
    
  }
  
  
  
}

for (j in 5:7){
  data_ABC[,j]=as.numeric(data_ABC[,j])
}

data_ABC$Migr=factor(data_ABC$Migr)

data_ABC=data_ABC[data_ABC$Method=="Random forest",]
data_ABC=data_ABC[data_ABC$Format=="Li_Ga" | data_ABC$Format=="Mu_Fa",]
data_ABC=data_ABC[data_ABC$Migr=="Beta",]

pop<-ggplot(data_ABC[data_ABC$Param=="N1" | data_ABC$Param=="N2" | data_ABC$Param=="Na",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

pop_ratio<-ggplot(data_ABC[data_ABC$Param=="N1/Na" | data_ABC$Param=="N2/Na",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

time<-ggplot(data_ABC[data_ABC$Param=="Tsplit" | data_ABC$Param=="Tsc" | data_ABC$Param=="Tam",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

time_ratio<-ggplot(data_ABC[data_ABC$Param=="Tam/Tsplit" | data_ABC$Param=="Tsc/Tsplit",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

mig<-ggplot(data_ABC[data_ABC$Param=="M12" | data_ABC$Param=="M21",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

mig_geneflow<-ggplot(data_ABC[data_ABC$Param=="m12" | data_ABC$Param=="m21",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)

beta<-ggplot(data_ABC[data_ABC$Param=="shape_N_a" | data_ABC$Param=="shape_N_b",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)+
  geom_hline(yintercept = 1,col="red",lty=2)

beta_m_a<-ggplot(data_ABC[data_ABC$Param=="shape_M12_a" | data_ABC$Param=="shape_M21_a",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75)+
  geom_hline(yintercept = 1,col="red",lty=2)

beta_m_b<-ggplot(data_ABC[data_ABC$Param=="shape_M12_b" | data_ABC$Param=="shape_M21_b",],aes(x=SP,y=Estimate,fill=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_bar(position=position_dodge(width=0.9),stat='identity',alpha=0.5) +
  geom_errorbar(aes(ymin=Sd_2.5,ymax=Sd_97.5), width=0.2,position=position_dodge(.9)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_fill_viridis_d(begin = 0.25,end=0.75) +
  geom_hline(yintercept = 1,col="red",lty=2)


if (plot_plot==1){
  plot(ggarrange(ggarrange(pop,pop_ratio,ncol=2,legend='none'),
                 ggarrange(time,time_ratio,ncol=2,legend='none'),
                 ggarrange(mig,mig_geneflow,ncol=2,legend='none'),
                 beta,
                 ggarrange(beta_m_a,beta_m_b,ncol=2,legend='none')
                 ,nrow=5,common.legend = T,legend = "right"))
}


pdf("/home/labosea1/ABC/ABC_output.pdf",width=25,height=35)
plot(ggarrange(pop,
               pop_ratio,
               time,
               time_ratio,
               mig,
               mig_geneflow,
               beta,
               beta_m_a,
               beta_m_b,
               nrow=10,
               common.legend=T,
               legend="right"))
dev.off()


pdf("/home/labosea1/ABC/ABC_output.pdf",width=55/1.25,height=35/1.25)
plot(ggarrange(ggarrange(pop,pop_ratio,ncol=2,legend='none'),
               ggarrange(time,time_ratio,ncol=2,legend='none'),
               ggarrange(mig,mig_geneflow,ncol=2,legend='none'),
               beta,
               ggarrange(beta_m_a,beta_m_b,ncol=2,legend='none')
               ,nrow=5,common.legend = T,legend = "right"))
dev.off()


write.csv(data_ABC,"/home/labosea1/ABC/data_ABC.csv",row.names = F)

data_ABC=data_ABC[data_ABC$Method=="Random forest",]

pop<-ggplot(data_ABC[data_ABC$Param=="N1" | data_ABC$Param=="N2" | data_ABC$Param=="Na",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.5)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75)

pop_ratio<-ggplot(data_ABC[data_ABC$Param=="N1/Na" | data_ABC$Param=="N2/Na",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.5)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75) +
  ylab("") +
  geom_hline(yintercept = 1,col="red",lty=2)

time<-ggplot(data_ABC[data_ABC$Param=="Tsplit" | data_ABC$Param=="Tsc" | data_ABC$Param=="Tam",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75)

time_ratio<-ggplot(data_ABC[data_ABC$Param=="Tam/Tsplit" | data_ABC$Param=="Tsc/Tsplit",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75) +
  ylab("") +
  scale_y_log10()

mig<-ggplot(data_ABC[data_ABC$Param=="M12" | data_ABC$Param=="M21",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75)


mig_geneflow<-ggplot(data_ABC[data_ABC$Param=="m12" | data_ABC$Param=="m21",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75) +
  ylab("") +
  scale_y_log10()

beta<-ggplot(data_ABC[data_ABC$Param=="shape_N_a" | data_ABC$Param=="shape_N_b",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75)+
  geom_hline(yintercept = 1,col="red",lty=2)

beta_m_a<-ggplot(data_ABC[data_ABC$Param=="shape_M12_a" | data_ABC$Param=="shape_M21_a",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75)+
  geom_hline(yintercept = 1,col="red",lty=2)

beta_m_b<-ggplot(data_ABC[data_ABC$Param=="shape_M12_b" | data_ABC$Param=="shape_M21_b",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75) +
  ylab("")+
  geom_hline(yintercept = 1,col="red",lty=2)

nBarriers<-ggplot(data_ABC[data_ABC$Param=="nBarriersM12" | data_ABC$Param=="nBarriersM21",],aes(x=SP,y=Estimate,color=Migr,shape=Format)) +
  facet_wrap(.~Param,ncol=4) +
  geom_pointrange(aes(ymin=Sd_2.5,ymax=Sd_97.5),position=position_dodge(width=0.25)) +
  expand_limits(y=0) +
  theme_classic() +
  scale_colour_viridis_d(begin = 0.25,end=0.75) +
  ylab("")+
  geom_hline(yintercept = 1,col="red",lty=2)

if (plot_plot==1){
  plot(ggarrange(ggarrange(pop,pop_ratio,ncol=2,legend='none'),
                 ggarrange(time,time_ratio,ncol=2,legend='none'),
                 ggarrange(mig,mig_geneflow,ncol=2,legend='none'),
                 beta,
                 ggarrange(beta_m_a,beta_m_b,ncol=2,legend='none'),
                 nBarriers,
                 nrow=6,common.legend = T,legend = "right"))
}


pdf("/home/labosea1/ABC/ABC_output_migr.pdf",width=50,height=15)
plot(ggarrange(ggarrange(pop,pop_ratio,ncol=2,legend='none'),
               ggarrange(time,time_ratio,ncol=2,legend='none'),
               ggarrange(mig,mig_geneflow,ncol=2,legend='none'),
               beta,
               ggarrange(beta_m_a,beta_m_b,ncol=2,legend='none'),
               nBarriers,
               nrow=6,common.legend = T,legend = "right"))
dev.off()

summary(lm(data_ABC$Estimate~data_ABC$Method))

run=0

if (run==1){
  setwd("/home/labosea1/ABC/Dlabr/wv0QtYRgnH/best_model_5/")
  
  posterior<-read.csv("posterior_bestModel.txt",sep="\t",header=T)
  
  posterior %>%
    melt() %>%
    ggplot(aes(x=value)) +
    geom_density() +
    facet_wrap(.~variable,nrow=4,scales="free") +
    theme_classic()
  
  
  
  setwd("/home/labosea1/ABC/Dlabr/wv0QtYRgnH/locus_modelComp/")
  
  locus=read.csv("locus_specific_modelComp.txt",sep="\t",header=T)
  
  locus %>%
    melt(id.vars=c("dataset")) %>%
    mutate(value=parse_number(value)) %>%
    ggplot(aes(x=value)) +
    geom_density() +
    facet_wrap(.~variable,nrow=4,scales="free")
  
}
