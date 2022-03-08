## Packages ----

library(readxl)
library(tidyverse)
library(reshape2)
library(stringr)
library(png)
library(grid)
library(ggrepel)
library(ggpubr)
library(lmtest)
library(vegan)

## Life-history traits ----
lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
lfh$Fec=as.numeric(lfh$Fec)

## Genetic value ----
fst<-read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx")
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

italic_species=c()

for (i in 1:nrow(fst)){
  tmp=str_split_fixed(fst$SPECIES[i], " ",2)
  italic_species[i]=paste("italic('",substr(fst$SPECIES[i],1,1),". ",tmp[2],"')",sep="")
}

fst$italic_species=italic_species

species_plot=c()
for (i in 1:nrow(fst)){
  tmp=str_split_fixed(fst$SPECIES[i], " ",2)
  species_plot[i]=paste(tmp[1],"\n",tmp[2],sep=" ")
}

fst$species_plot=species_plot

load(file="data/dxy_all.Rdata")
dxy=dxy_data
colnames(dxy)[1]="Species_code"

genetic=merge(as.data.frame(fst),dxy,on=c("Species_code"))

abc<-read.csv("data/data_ABC.csv",header=T,sep=",")
abc=abc[abc$Method=="Random forest" & abc$Migr=="Beta" & abc$Format=="Li_Ga",]
abc=abc[,c(1,3,5)]
abc=dcast(abc, SP ~ Param)
colnames(abc)[1]="Species_code"


## Correlation ----
lfh_title=c("","","","",
            "Body size (cm)",
            "Trophic level",
            "Fecundity (eggs/day)",
            "Propagule size (mm)",
            "Age at Maturity (cm)",
            "Lifespan (years)",
            "Adult lifespan (years)",
            "Hermaphroditism",
            "Parental Care",
            "Pelagic Larval Duration (days)")

myplots <- vector('list', 10)

a=0
for (i in c(5:6,8:14)){
  
  tmp=merge(lfh[,c(3,i)],genetic[,c(1,15,11)],on=c("Species_code"))
  colnames(tmp)=c("SP","lfh","italic","genetic")
  m1<-betareg::betareg(genetic~lfh,data=tmp)
  
  tmp$mean=fitted(m1)
  wait=fitted(m1)-2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_025=wait
  wait=fitted(m1)+2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_975=wait
  
  
  
  if (coefficients(summary(m1))$mean[2,4]<=0.05){
    
    if (i==12 | i==13){
      my_comparisons <- list( c("Yes","No"))
      p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    } else {
      
      run_fit=0
      
      if (run_fit==1){
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) +
          geom_line(aes(x=lfh,y=mean),col=viridis::viridis(1),size=1) +
          geom_ribbon(aes(ymin=sd_025,
                          ymax=sd_975),
                      alpha=0.25,fill=viridis::viridis(1),col="white") 
      } else {
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
      }
      
    }
    
  } else {
    p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    
  }
  
  
  p<-p+
    geom_point(shape=19,size=2) +
    theme_classic()+
    geom_text_repel(aes(label=italic),col="black",parse=T) +
    xlab(lfh_title[i])+
    ylab("Fst Gulf of Lion - Bay of Biscay") +
    labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
    theme(plot.title = element_text(face = "italic",hjust=0.5))
  
  a=a+1
  myplots[[a]] <- local({
    print(p)
  })
  
  
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/lfh_fst_li_ga.pdf",sep=""),width=20,height=15)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/lfh_fst_li_ga.jpg",sep=""),width=1500,height=1000)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

## dxy Li-Ga ----
lfh_title=c("","","","",
            "Body size (cm)",
            "Trophic level",
            "Fecundity (eggs/day)",
            "Propagule size (mm)",
            "Age at Maturity (cm)",
            "Lifespan (years)",
            "Adult lifespan (years)",
            "Hermaphroditism",
            "Parental Care",
            "Pelagic Larval Duration (days)")

myplots <- vector('list', 10)

genetic$dxy_Li_Ga=as.numeric(genetic$dxy_Li_Ga)
a=0
for (i in c(5:6,8:14)){
  
  tmp=merge(lfh[,c(3,i)],genetic[,c(1,15,19)],on=c("Species_code"))
  colnames(tmp)=c("SP","lfh","italic","genetic")
  tmp$genetic=tmp$genetic/100
  m1<-betareg::betareg(genetic~lfh,data=tmp)
  
  tmp$mean=fitted(m1)
  wait=fitted(m1)-2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_025=wait
  wait=fitted(m1)+2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_975=wait
  
  
  
  if (coefficients(summary(m1))$mean[2,4]<=0.05){
    
    if (i==12 | i==13){
      my_comparisons <- list( c("Yes","No"))
      p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    } else {
      
      run_fit=0
      
      if (run_fit==1){
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) +
          geom_line(aes(x=lfh,y=mean),col=viridis::viridis(1),size=1) +
          geom_ribbon(aes(ymin=sd_025,
                          ymax=sd_975),
                      alpha=0.25,fill=viridis::viridis(1),col="white") 
      } else {
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
      }
      
    }
    
  } else {
    p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    
  }
  
  
  p<-p+
    geom_point(shape=19,size=2)+
    theme_classic()+
    geom_text_repel(aes(label=italic),col="black",parse=T) +
    xlab(lfh_title[i])+
    ylab("dxy Gulf of Lion - Bay of Biscay") +
    labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
    theme(plot.title = element_text(face = "italic",hjust=0.5))
  
  a=a+1
  myplots[[a]] <- local({
    print(p)
  })
  
  
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/lfh_dxy_li_ga.pdf",sep=""),width=20,height=15)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/lfh_dxy_li_ga.jpg",sep=""),width=1500,height=1000)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

## da Li-Ga ----
lfh_title=c("","","","",
            "Body size (cm)",
            "Trophic level",
            "Fecundity (eggs/day)",
            "Propagule size (mm)",
            "Age at Maturity (cm)",
            "Lifespan (years)",
            "Adult lifespan (years)",
            "Hermaphroditism",
            "Parental Care",
            "Pelagic Larval Duration (days)")

myplots <- vector('list', 10)

genetic$da_Li_Ga=as.numeric(genetic$da_Li_Ga)
a=0
for (i in c(5:6,8:14)){
  
  tmp=merge(lfh[,c(3,i)],genetic[,c(1,15,30)],on=c("Species_code"))
  colnames(tmp)=c("SP","lfh","italic","genetic")
  tmp$genetic=tmp$genetic/100
  m1<-betareg::betareg(genetic~lfh,data=tmp)
  
  tmp$mean=fitted(m1)
  wait=fitted(m1)-2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_025=wait
  wait=fitted(m1)+2*coefficients(summary(m1))$mean[2,2]
  tmp$sd_975=wait
  
  
  
  if (coefficients(summary(m1))$mean[2,4]<=0.05){
    
    if (i==12 | i==13){
      my_comparisons <- list( c("Yes","No"))
      p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    } else {
      
      run_fit=0
      
      if (run_fit==1){
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) +
          geom_line(aes(x=lfh,y=mean),col=viridis::viridis(1),size=1) +
          geom_ribbon(aes(ymin=sd_025,
                          ymax=sd_975),
                      alpha=0.25,fill=viridis::viridis(1),col="white") 
      } else {
        p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
      }
      
    }
    
  } else {
    p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    
  }
  
  
  
  p<-p+
    geom_point(shape=19,size=2)+
    theme_classic()+
    geom_text_repel(aes(label=italic),col="black",parse=T) +
    xlab(lfh_title[i])+
    ylab("da Gulf of Lion - Bay of Biscay") +
    labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
    theme(plot.title = element_text(face = "italic",hjust=0.5))
  
  a=a+1
  myplots[[a]] <- local({
    print(p)
  })
  
  
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/lfh_da_li_ga.pdf",sep=""),width=20,height=15)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/lfh_da_li_ga.jpg",sep=""),width=1500,height=1000)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

## Tsplit ----
lfh_title=c("","","","",
            "Body size (cm)",
            "Trophic level",
            "Fecundity (eggs/day)",
            "Propagule size (mm)",
            "Age at Maturity (cm)",
            "Lifespan (years)",
            "Adult lifespan (years)",
            "Hermaphroditism",
            "Parental Care",
            "Pelagic Larval Duration (days)")

myplots <- vector('list', 10)

#genetic$da_Li_Ga=as.numeric(genetic$da_Li_Ga)
a=0
for (i in c(5:6,8:14)){
  
  tmp=merge(lfh[,c(3,i)],genetic[,c(1,15,53)],on=c("Species_code"))
  colnames(tmp)=c("SP","lfh","italic","genetic")
  tmp$genetic=tmp$genetic/100
  m1<-lm(genetic~lfh,data=tmp)
  
  tmp$mean=fitted(m1)
  wait=fitted(m1)-2*coefficients(summary(m1))[2,2]
  tmp$sd_025=wait
  wait=fitted(m1)+2*coefficients(summary(m1))[2,2]
  tmp$sd_975=wait
  
  
  
  if (coefficients(summary(m1))[2,4]<=0.05){
    
    if (i==12 | i==13){
      my_comparisons <- list( c("Yes","No"))
      p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    } else {
      
      p<-ggplot(tmp,aes(x=lfh,y=genetic)) +
        geom_line(aes(x=lfh,y=mean),col=viridis::viridis(1),size=1) +
        geom_ribbon(aes(ymin=sd_025,
                        ymax=sd_975),
                    alpha=0.25,fill=viridis::viridis(1),col="white") 
    }
    
  } else {
    p<-ggplot(tmp,aes(x=lfh,y=genetic)) 
    
  }
  
  
  p<-p+
    geom_point(shape=19,size=2)+
    theme_classic()+
    geom_text_repel(aes(label=italic),col="black",parse=T) +
    xlab(lfh_title[i])+
    ylab("Tsplit Gulf of Lion - Bay of Biscay") +
    labs(title=paste("p-value = ",round(coefficients(summary(m1))[2,4],8)," ; pseudo R� = ",round(summary(m1)$r.squared,3),sep="")) +
    theme(plot.title = element_text(face = "italic",hjust=0.5))
  
  a=a+1
  myplots[[a]] <- local({
    print(p)
  })
  
  
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/lfh_tsplit_li_ga.pdf",sep=""),width=15,height=10)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/lfh_tsplit_li_ga.jpg",sep=""),width=1500,height=1000)
ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]],nrow=3,ncol=3)
dev.off()


### Slope ----
tmp=merge(lfh[,c(3,14)],genetic[,c(1,15,17)],on=c("Species_code"))

colnames(tmp)=c("SP","lfh","italic","genetic")
tmp$genetic=as.numeric(tmp$genetic)
summary(lm(genetic~lfh,data=tmp))

ggplot(tmp,aes(x=lfh,y=genetic))+
  geom_point(shape=19,size=2)+
  theme_classic()+
  geom_text_repel(aes(label=italic),col="black",parse=T) +
  #xlab("")+
  #ylab("Fst Gulf of Lion - Bay of Biscay") +
  #labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
  theme(plot.title = element_text(face = "italic",hjust=0.5))

tmp=genetic[,c(1,15,11,17)]

colnames(tmp)=c("SP","italic","fst","genetic")

tmp$fst=as.numeric(tmp$fst)
tmp$genetic=as.numeric(tmp$genetic)
summary(lm(genetic~fst,data=tmp))

tmp$habitat=c("Lagoon","No","No","No","No","Lagoon","No","No","No","No","No","No","No","Lagoon","No","No","Lagoon")
m1 <- nls(genetic ~ k*(1-exp(-fst*t)), data=tmp[tmp$habitat=="No",],start=list(k=1,t=1))

tmp_2=tmp[tmp$habitat=="No",]
tmp_2$mean=fitted(m1)
wait=fitted(m1)-2*coefficients(summary(m1))[2,2]
tmp_2$sd_025=wait
wait=fitted(m1)+2*coefficients(summary(m1))[2,2]
tmp_2$sd_975=wait

pdf("/Users/divco/Documents/COGEDIV/figures/fst_correlation.pdf",width=7.5,height=5.5)
ggplot(tmp,aes(x=fst,y=genetic))+
  geom_line(data=tmp_2,aes(x=fst,y=mean),col=viridis::viridis(1),size=1) +
  geom_point(shape=19,size=2,aes(col=habitat))+
  theme_classic()+
  geom_text_repel(aes(label=italic),col="black",parse=T) +
  ylab("Slope between Fst(Gulf of Lion;Bay of Biscay) ~ Fst(Costa Calida;Algarve)")+
  xlab("Fst Gulf of Lion - Bay of Biscay") +
  #labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
  theme(plot.title = element_text(face = "italic",hjust=0.5)) +
  geom_hline(yintercept=1.0,col="red",lty=2) +
  scale_color_viridis_d(begin=0.25,end=0.75) 
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/fst_correlation.jpg",width=750,height=550,quality=100,res=100)
ggplot(tmp,aes(x=fst,y=genetic))+
  geom_line(data=tmp_2,aes(x=fst,y=mean),col=viridis::viridis(1),size=1) +
  geom_point(shape=19,size=2,aes(col=habitat))+
  theme_classic()+
  geom_text_repel(aes(label=italic),col="black",parse=T) +
  ylab("Slope between Fst(Gulf of Lion;Bay of Biscay) ~ Fst(Costa Calida;Algarve)")+
  xlab("Fst Gulf of Lion - Bay of Biscay") +
  #labs(title=paste("p-value = ",round(coefficients(summary(m1))$mean[2,4],8)," ; pseudo R� = ",round(m1$pseudo.r.squared,3),sep="")) +
  theme(plot.title = element_text(face = "italic",hjust=0.5)) +
  geom_hline(yintercept=1.0,col="red",lty=2) +
  scale_color_viridis_d(begin=0.25,end=0.75) 
dev.off()

