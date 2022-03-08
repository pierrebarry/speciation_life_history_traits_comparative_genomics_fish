### Packages ----
library(RColorBrewer)
library(formattable)
library(ggplot2)
library(ggrepel)
library(strex)
library(reshape2)
library(wesanderson)
library(ggridges)
library(knitr)
library(kableExtra)
library(formattable)
library(dplyr)
library(cowplot)
library(htmltools)
library(plotly)
library(ggiraph)
library(gridExtra)
library(RColorBrewer)
library(ggplot2)
library("rnaturalearth")
library(sf)
library(ggsn)
library(rgeos)
library(plotly)
library(png)
library(stringr)
library(DT)
library("readxl")
library(ggridges)
library(leaflet)
library(R.utils)
library(png)
library(readxl)
color_med_atl=data.frame(Location=c("Gulf of Lion","Costa Calida","Algarve","Bay of Biscay"),
                         Col=brewer.pal(n = 4, name = "RdBu"))
setwd("C:/Users/divco/Documents/COGEDIV/")

### Fig 7 ----
## LFH
lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
lfh$Fec=as.numeric(lfh$Fec)
lfh=as.data.frame(lfh)
lfh$Fec=log(lfh$Fec)
lfh$Propagule_Size=log(lfh$Propagule_Size)
for (i in c(5:11,14)){
  lfh[,i]=as.numeric(as.vector(lfh[,i]))
}

lfh=lfh[,c(3,seq(5,14))]

## Genetic
fst<-as.data.frame(read_excel("/Users/divco/Documents/COGEDIV/data/FST.xlsx"))
fst<-fst[,c(2,seq(9,14))]
fst<-fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]

for (i in 2:ncol(fst)){
  fst[,i]=as.numeric(fst[,i])
}

merge_data=merge(fst,lfh,on=c("Species_code"))

merge_data$proba_ongoing_out=c("Yes","Yes","No","Yes","Yes","No",NA,"Yes","Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes","No")
merge_data$introgress_in_med=c("No","No","No","No","No","Yes","No","No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")
merge_data$introgress_in_atl=c("No","No","No","No","Yes","Yes","No","No","Yes","No","No","No","No","No","No","No","No","No","No")
merge_data$introgress       =c("No","No","No","No","Yes","Yes","No","No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")

summary(betareg::betareg(FST_LI_GA_WEIGHTED~PLD,data=merge_data))

m1<-betareg::betareg(FST_LI_GA_WEIGHTED~PLD,data=merge_data)
m2<-lm(FST_LI_GA_WEIGHTED~PLD,data=merge_data)
m3<-lm(FST_LI_GA_WEIGHTED~I(1/PLD),data=merge_data)

lmtest::lrtest(m1,m3)


summary(betareg::betareg(FST_MU_FA_WEIGHTED~introgression,data=merge_data[merge_data$proba_ongoing_out=="Yes",]))


kruskal.test(merge_data$introgress,
             merge_data$Adult_Lifespan)

kruskal.test(merge_data[is.na(merge_data$introgress)==FALSE & is.na(merge_data$Species_code)==FALSE,]$introgress,
             merge_data[is.na(merge_data$introgress)==FALSE & is.na(merge_data$Species_code)==FALSE,]$Adult_Lifespan)

a <- chisq.test(  
  merge_data$Hermaphrodism,
  merge_data$introgress
)

load(file="data/dxy_all.Rdata")
dxy=dxy_data
dxy=dxy[,seq(1,18)]

for (i in 2:ncol(dxy)){
  dxy[,i]=as.numeric(dxy[,i])
}

colnames(dxy)[1]="Species_code"
merge_data=merge(merge_data,dxy,on=c("Species_code"))
summary(a<-lm(dxy_Mu_Fa~Body_Size,data=merge_data))
summary(lm(dxy_Mu_Fa~Trophic_Level,data=merge_data))
summary(lm(dxy_Mu_Fa~Adult_Lifespan,data=merge_data))
summary(lm(dxy_Mu_Fa~Lifespan,data=merge_data))
summary(lm(dxy_Mu_Fa~Propagule_Size,data=merge_data))
summary(lm(dxy_Mu_Fa~Fec,data=merge_data))
summary(lm(dxy_Mu_Fa~PLD,data=merge_data))
summary(lm(dxy_Mu_Fa~Parental_Care,data=merge_data))
summary(lm(dxy_Mu_Fa~Hermaphrodism,data=merge_data))
summary(lm(dxy_Mu_Fa~Body_Size + Trophic_Level + Lifespan + PLD ,data=merge_data))

summary(a<-lm(da_Li_Ga~Body_Size,data=merge_data))
summary(lm(da_Li_Ga~Trophic_Level,data=merge_data))
summary(lm(da_Li_Ga~Adult_Lifespan,data=merge_data))
summary(lm(da_Li_Ga~Lifespan,data=merge_data))
summary(lm(da_Li_Ga~Propagule_Size,data=merge_data))
summary(lm(da_Li_Ga~Fec,data=merge_data))
summary(lm(da_Li_Ga~PLD,data=merge_data))
summary(lm(da_Li_Ga~Parental_Care,data=merge_data))
summary(lm(da_Li_Ga~Hermaphrodism,data=merge_data))
kruskal.test(merge_data$da_Li_Ga,
             merge_data$Hermaphrodism)

summary(a<-lm(da_Mu_Fa~Body_Size,data=merge_data))
summary(lm(da_Li_Ga~Trophic_Level,data=merge_data))
summary(lm(da_Mu_Fa~Adult_Lifespan,data=merge_data))
summary(lm(da_Mu_Fa~Lifespan,data=merge_data))
summary(lm(da_Mu_Fa~Propagule_Size,data=merge_data))
summary(lm(da_Mu_Fa~Fec,data=merge_data))
summary(lm(da_Mu_Fa~PLD,data=merge_data))
summary(lm(da_Mu_Fa~Parental_Care,data=merge_data))
summary(lm(da_Mu_Fa~Hermaphrodism,data=merge_data))
kruskal.test(merge_data$da_Mu_Fa,
             merge_data$Hermaphrodism)
summary(lm(da_Li_Ga~Body_Size + Trophic_Level + Lifespan + PLD ,data=merge_data))

abc<-read.csv("data/data_ABC.csv",header=T,sep=",")
abc=abc[abc$Method=="Random forest" & abc$Migr=="Beta" & abc$Format=="Li_Ga",]
abc=abc[,c(1,3,5)]
#abc$Estimate=as.numeric(abc$Estimate)
#abc$SP=as.character(abc$SP)
#abc$Param=as.character(abc$Param)
abc=as.data.frame(dcast(abc, SP ~ Param))
colnames(abc)[1]="Species_code"
for (i in 2:5){
  for (j in 1:nrow(abc)){
    if (is.na(abc[j,i])==T){
      
      abc[j,i]=0
      
    }
    
  }
  
}

abc=rbind(abc,
          c("Gnige",rep(NA,ncol(abc)-1)))

merge_data=merge(merge_data,abc[,c(1,2:10,21)],on=c("Species_code"))

italic_species=c("italic('A. boyeri')",
                 "italic('A. fallax')",
                 "italic('C. galerita')",
                 "italic('C. julis')",
                 "italic('D. labrax')",
                 "italic('D. puntazzo')",
                 #"italic('E. encrasicolus')",
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
merge_data$italic_species=italic_species

#lfh<-read_excel("/Users/divco/Documents/COGEDIV/data/lfh.xlsx")
#lfh$Fec=as.numeric(lfh$Fec)

#merge_data=merge(merge_data,lfh,on=c("Species_code"))

merge_data$proba_ongoing_out=c("Yes","Yes","No","Yes","Yes","No",NA,"Yes","Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes","No")
merge_data$introgress_in_med=c("No","No","No","No","No","Yes",NA,"No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")
merge_data$introgress_in_atl=c("No","No","No","No","Yes","Yes",NA,"No","Yes","No","No","No","No","No","No","No","No","No","No")
merge_data$introgress=c("No","No","No","Yes","Yes","Yes","No","No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")

kruskal.test(merge_data[merge_data$Parental_Care=="No",]$PLD,
             merge_data[merge_data$Parental_Care=="No",]$introgress)
kruskal.test(merge_data$Parental_Care,
             merge_data$proba_ongoing_out)
kruskal.test(merge_data$Parental_Care,
             merge_data$introgress)

summary(lm(M12~PLD,data=merge_data))

m1<-betareg::betareg(FST_LI_GA_WEIGHTED~PLD,data=merge_data[merge_data$proba_ongoing_out=="Yes",])
p_r <- betareg::predict(m1, newdata = data.frame(PLD = seq(min(merge_data[merge_data$proba_ongoing_out=="Yes",]$PLD,na.rm=T),
                                                           max(merge_data[merge_data$proba_ongoing_out=="Yes",]$PLD,na.rm=T))), type="quantile",
                        at=c(0.025,0.5,0.975))
data_p=data.frame(PLD=seq(min(merge_data[merge_data$proba_ongoing_out=="Yes",]$PLD,na.rm=T),
                          max(merge_data[merge_data$proba_ongoing_out=="Yes",]$PLD,na.rm=T)),
                  estimate=p_r[,2],
                  predict_025=p_r[,1],
                  predict_975=p_r[,3])
p1<-ggplot() +
  geom_point(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
             aes(x=PLD,
                 y=FST_LI_GA_WEIGHTED,
                 col=proba_ongoing_out),
             size=3,
             alpha=0.75) +
  geom_line(data=data_p,
            aes(x= PLD, y= estimate),
            colour = "darkorange",alpha=0.5,size=1.25) +
  geom_ribbon(data=data_p,aes(
    x=PLD,
    ymin=predict_025, 
    ymax=predict_975),
    alpha=0.1) +
  
  theme_classic() +
  ylab(expression(F[ST])) +
  xlab("Pelagic Larval Duration (days)") +
  scale_color_manual(name="Ongoing gene flow between outer populations",
                     values=viridis::cividis(2,end=0.9)) +
  theme(legend.position = "bottom") +
  geom_text_repel(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
                  aes(x=PLD,y=FST_LI_GA_WEIGHTED,
                      label=italic_species),
                  parse=T) 

m1<-lm(dxy_Li_Ga~Body_Size,data=merge_data)
p_r <- predict.lm(m1, newdata = data.frame(Body_Size = seq(min(merge_data$Body_Size,na.rm=T),
                                                           max(merge_data$Body_Size,na.rm=T))), 
                  se.fit = T)
data_p=data.frame(Body_Size=seq(min(merge_data$Body_Size,na.rm=T),
                                max(merge_data$Body_Size,na.rm=T)),
                  estimate=p_r$fit,
                  predict_025=p_r$fit-p_r$se.fit,
                  predict_975=p_r$fit+p_r$se.fit)
p2<-ggplot() +
  geom_point(data=merge_data,
             aes(x=Body_Size,
                 y=dxy_Li_Ga,
                 col=proba_ongoing_out),
             size=3,
             alpha=0.75) +
  geom_line(data=data_p,
            aes(x= Body_Size, y= estimate),
            colour = "darkorange",alpha=0.5,size=1.25) +
  geom_ribbon(data=data_p,aes(
    x=Body_Size,
    ymin=predict_025, 
    ymax=predict_975),
    alpha=0.1) +
  
  theme_classic() +
  ylab(expression(d[XY])) +
  xlab("Body size (cm)") +
  scale_color_manual(name="Ongoing gene flow between outer populations",
                     values=viridis::cividis(2,end=0.9)) +
  theme(legend.position = "bottom") +
  geom_text_repel(data=merge_data,
                  aes(x=Body_Size,y=dxy_Li_Ga,
                      label=italic_species),
                  parse=T) 


merge_data$Tsplit=as.numeric(merge_data$Tsplit)
m1<-lm(Tsplit~Adult_Lifespan,data=merge_data[merge_data$proba_ongoing_out=="Yes",])
p_r <- predict.lm(m1, newdata = data.frame(Adult_Lifespan = seq(min(merge_data[merge_data$proba_ongoing_out=="Yes",]$Adult_Lifespan,na.rm=T),
                                                                max(merge_data[merge_data$proba_ongoing_out=="Yes",]$Adult_Lifespan,na.rm=T))), 
                  se.fit = T)
data_p=data.frame(Adult_Lifespan=seq(min(merge_data[merge_data$proba_ongoing_out=="Yes",]$Adult_Lifespan,na.rm=T),
                                     max(merge_data[merge_data$proba_ongoing_out=="Yes",]$Adult_Lifespan,na.rm=T)),
                  estimate=p_r$fit,
                  predict_025=p_r$fit-p_r$se.fit,
                  predict_975=p_r$fit+p_r$se.fit)
p3<-ggplot() +
  geom_point(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
             aes(x=Adult_Lifespan,
                 y=Tsplit,
                 col=proba_ongoing_out),
             size=3,
             alpha=0.75) +
  geom_line(data=data_p,
            aes(x= Adult_Lifespan, y= estimate),
            colour = "darkorange",alpha=0.5,size=1.25) +
  geom_ribbon(data=data_p,aes(
    x=Adult_Lifespan,
    ymin=predict_025, 
    ymax=predict_975),
    alpha=0.1) +
  
  theme_classic() +
  ylab(expression(T[split])) +
  xlab("Adult lifespan (years)") +
  scale_color_manual(name="Ongoing gene flow between outer populations",
                     values=viridis::cividis(2,end=0.9)) +
  theme(legend.position = "bottom") +
  geom_text_repel(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
                  aes(x=Adult_Lifespan,y=Tsplit,
                      label=italic_species),
                  parse=T) 


jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig7.jpg",sep=""),width=10,height=10,units="in",res=2000)
print(ggpubr::ggarrange(p1,
                        p2,
                        p3,
                        nrow=2,
                        ncol=2,
                        labels = c("A","B","C"),
                        common.legend=T))
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig7.pdf",sep=""),width=10,height=10)
print(ggpubr::ggarrange(p1,
                        p2,
                        p3,
                        labels = c("A","B","C"),
                        common.legend=T))
dev.off()


p2<-ggplot() +
  geom_point(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
             aes(x=PLD,
                 y=M21,
                 col=proba_ongoing_out),
             size=3,
             alpha=0.75) +
  theme_classic() +
  ylab(expression(d[XY])) +
  xlab("Body size (cm)") +
  scale_color_manual(name="Ongoing gene flow between outer populations",
                     values=viridis::cividis(2,end=0.9)) +
  theme(legend.position = "bottom") +
  geom_text_repel(data=merge_data[is.na(merge_data$proba_ongoing_out)==F,],
                  aes(x=PLD,y=M12,
                      label=italic_species),
                  parse=T) 

## Fig Suppmat
