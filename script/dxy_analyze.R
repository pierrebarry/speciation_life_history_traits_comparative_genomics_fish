library(readxl)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(dplyr)
library(tidyverse)

setwd("C:/Users/divco/Documents/")  

dxy_function=function(sp){
  
  dxy_data=data.frame(SP=c(NA),
                      dxy_Li_Mu=c(NA),
                      dxy_Li_Fa=c(NA),
                      dxy_Li_Ga=c(NA),
                      dxy_Mu_Fa=c(NA),
                      dxy_Mu_Ga=c(NA),
                      dxy_Fa_Ga=c(NA),
                      pi_Li=c(NA),
                      pi_Mu=c(NA),
                      pi_Fa=c(NA),
                      pi_Ga=c(NA),
                      pi_All=c(NA),
                      da_Li_Mu=c(NA),
                      da_Li_Fa=c(NA),
                      da_Li_Ga=c(NA),
                      da_Mu_Fa=c(NA),
                      da_Mu_Ga=c(NA),
                      da_Fa_Ga=c(NA))
  
  for (i in sp){
    
    print(i)
    
    ## DXY
    
    print("DXY")
    
    dxy<-read.csv(paste("COGEDIV/data/dxy_",i,".csv",sep=""),sep=";",header=T)
    
    dxy$compare=factor(paste(dxy$POP1,"-",dxy$POP2,sep=""))
    
    plot(ggplot(dxy[dxy$END-dxy$START>=9999,],aes(x=DXY_ALL)) +
           geom_density(aes(fill=compare),col="black",bins=100,alpha=0.5) +
           xlim(c(0,5)) +
           theme_classic() +
           scale_fill_viridis_d() +
           xlab("dxy (%)") +
           ylab("")) 
    
    print(paste("Gulf of Lion - Costa Calida : ",round(sum(dxy[dxy$compare=="Li-Mu",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Mu",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Gulf of Lion - Algarve : ",round(sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Gulf of Lion - Bay of Biscay : ",round(sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Costa Calida - Algarve : ",round(sum(dxy[dxy$compare=="Mu-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Fa",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Costa Calida - Bay of Biscay : ",round(sum(dxy[dxy$compare=="Mu-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Ga",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Algarve - Bay of Biscay : ",round(sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    
    ## PI
    
    print("PI")
    
    pi<-read.csv(paste("COGEDIV/data/pi_",i,".csv",sep=""),sep=";",header=T)
    
    plot(ggplot(pi[pi$END-pi$START>=9999,],aes(x=PI_ALL)) +
           geom_density(aes(fill=POP),col="black",bins=100,alpha=0.5) +
           #xlim(c(0,5)) +
           theme_classic() +
           scale_fill_viridis_d() +
           xlab("pi (%)") +
           ylab(""))
    
    print(paste("Gulf of Lion : ",round(sum(pi[pi$POP=="Li",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Li",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Costa Calida : ",round(sum(pi[pi$POP=="Mu",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Mu",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Algarve : ",round(sum(pi[pi$POP=="Fa",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Fa",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("Bay of Biscay : ",round(sum(pi[pi$POP=="Ga",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Ga",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    print(paste("All : ",round(sum(pi[pi$POP=="All",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="All",]$DENOM_ALL,na.rm=T),3),"%",sep=""))
    
    pi_li=sum(pi[pi$POP=="Li",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Li",]$DENOM_ALL,na.rm=T)
    pi_mu=sum(pi[pi$POP=="Mu",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Mu",]$DENOM_ALL,na.rm=T)
    pi_fa=sum(pi[pi$POP=="Fa",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Fa",]$DENOM_ALL,na.rm=T)
    pi_ga=sum(pi[pi$POP=="Ga",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Ga",]$DENOM_ALL,na.rm=T)
    
    ## DA
    
    print("DA")
    
    if (i!="Afall" & i!="Gnige"){
      print(paste("Gulf of Lion - Costa Calida : ",round((sum(dxy[dxy$compare=="Li-Mu",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Mu",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_mu),3),"%",sep=""))
    }
    print(paste("Gulf of Lion - Algarve : ",round((sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_fa),3),"%",sep=""))
    print(paste("Gulf of Lion - Bay of Biscay : ",round((sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_ga),3),"%",sep=""))
    if (i!="Afall" & i!="Gnige"){
      print(paste("Costa Calida - Algarve : ",round((sum(dxy[dxy$compare=="Mu-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Fa",]$DENOM_ALL,na.rm=T))-mean(pi_mu,pi_fa),3),"%",sep=""))
      print(paste("Costa Calida - Bay of Biscay : ",round((sum(dxy[dxy$compare=="Mu-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_mu,pi_ga),3),"%",sep=""))
    }
    print(paste("Algarve - Bay of Biscay : ",round((sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_fa,+pi_ga),3),"%",sep=""))
    
    ##
    
    if (i!="Afall" & i!="Gnige"){
      
      dxy_data=rbind(dxy_data,
                     c(i,
                       round(sum(dxy[dxy$compare=="Li-Mu",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Mu",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Mu-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Mu-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Li",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Li",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Mu",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Mu",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Fa",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Ga",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="All",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="All",]$DENOM_ALL,na.rm=T),3),
                       round((sum(dxy[dxy$compare=="Li-Mu",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Mu",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_mu),3),
                       round((sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_fa),3),
                       round((sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_ga),3),
                       round((sum(dxy[dxy$compare=="Mu-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Fa",]$DENOM_ALL,na.rm=T))-mean(pi_mu,pi_fa),3),
                       round((sum(dxy[dxy$compare=="Mu-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_mu,pi_ga),3),
                       round((sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_fa,+pi_ga),3)
                       
                     ))
    } else {
      
      dxy_data=rbind(dxy_data,
                     c(i,
                       round(sum(dxy[dxy$compare=="Li-Mu",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Mu",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Mu-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Mu-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Mu-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Li",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Li",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Mu",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Mu",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Fa",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Fa",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="Ga",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="Ga",]$DENOM_ALL,na.rm=T),3),
                       round(sum(pi[pi$POP=="All",]$NUM_ALL,na.rm=T)*100/sum(pi[pi$POP=="All",]$DENOM_ALL,na.rm=T),3),
                       NA,
                       round((sum(dxy[dxy$compare=="Li-Fa",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Fa",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_fa),3),
                       round((sum(dxy[dxy$compare=="Li-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Li-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_li,pi_ga),3),
                       NA,
                       NA,
                       round((sum(dxy[dxy$compare=="Fa-Ga",]$NUM_ALL,na.rm=T)*100/sum(dxy[dxy$compare=="Fa-Ga",]$DENOM_ALL,na.rm=T))-mean(pi_fa,+pi_ga),3)
                       
                     ))
    }
    
    
    if (is.na(dxy_data[1,1])==T){
      dxy_data=dxy_data[-1,]
    }
    
    print(dxy_data)
    
    
    save(dxy_data,file="COGEDIV/data/dxy_all.Rdata")
    
    
  }
  
  
  
}

list_species=c("Aboye",
               "Afall",
               "Cgale",
               "Cjuli",
               "Dlabr",
               "Dpunt",
               "Gnige",
               "Hgutt",
               "Lbude",
               "Lmorm",
               "Mmerl",
               "Msurm",
               "Peryt",
               "Scabr",
               "Scine",
               "Scant",
               "Spilc",
               "Ssard",
               "Styph")

dxy_function(sp=list_species)

load(file="COGEDIV/data/dxy_all.Rdata")

for (i in 1:nrow(dxy_data[,seq(8,11)])){
  print(dxy_data[i,1])
  print(mean(as.numeric(dxy_data[i,seq(8,11)])))
}


dxy_data=melt(dxy_data,id.vars=c("SP"))
dxy_data$value=as.numeric(dxy_data$value)

ggplot(dxy_data[str_detect(dxy_data$variable,"dxy")==T,],aes(x=variable,y=value)) +
  theme_solarized(light = TRUE, base_family = 'Inconsolata') +
  geom_bar(stat="identity",aes(fill=variable),color="black",size=1) +
  facet_grid(SP~.,scales='free') +
  scale_fill_solarized() +
  coord_flip()

ggplot(dxy_data[str_detect(dxy_data$variable,"da_Li_Ga")==T,],aes(x=SP,y=value)) +
  theme_solarized(light = TRUE, base_family = 'Inconsolata') +
  geom_bar(stat="identity",aes(fill=variable),color="black",size=1) +
  scale_fill_solarized() +
  coord_flip() +
  ylab(expression(d[a])) +
  xlab("")
