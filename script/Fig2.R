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

## Fig 2 ----

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

fst=fst[is.na(fst$FST_LI_GA_WEIGHTED)==F,]

introgress_in_med=c("No","No","No","No","No","Yes","No","No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")
introgress_in_atl=c("No","No","No","No","Yes","Yes","No","No","Yes","No","No","No","No","No","No","No","No","No","No")

introgress=c("No","No","No","No","Yes","Yes","No","No","Yes","Yes","No","Yes","No","Yes","Yes","No","No","No","No")

binom.test(12, 16, alternative="greater", p=0.5)$p.value

fst$introgress_in_med=introgress_in_med
fst$introgress_in_atl=introgress_in_atl
fst$introgress=introgress
kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$introgress_in_med)
kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$introgress_in_atl)
kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$introgress)


num_yes=nrow(fst[fst$introgress_in_med=="Yes",])+nrow(fst[fst$introgress_in_atl=="Yes",])
denum_yes=nrow(fst)*2
percentage_yes=(nrow(fst[fst$introgress_in_med=="Yes",])+nrow(fst[fst$introgress_in_atl=="Yes",]))/(nrow(fst)*2)

m1<-lm(FST_LI_GA_WEIGHTED~-1+introgress_in_med,data=fst)
data_tmp_med=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                        value=c(1.4,1.8),
                        FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                             coefficients(summary(m1))[2,1]),
                        sd=c(coefficients(summary(m1))[1,2],
                             coefficients(summary(m1))[2,2])
)

m1<-lm(FST_LI_GA_WEIGHTED~-1+introgress_in_atl,data=fst)
data_tmp_atl=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                        value=c(1.3,1.7),
                        FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                             coefficients(summary(m1))[2,1]),
                        sd=c(coefficients(summary(m1))[1,2],
                             coefficients(summary(m1))[2,2])
)

p_f3<-fst[,c(1,2,11,12,15,16,17)] %>%
  melt(id.vars=c("SPECIES","Species_code","FST_LI_GA_WEIGHTED",
                 "FST_MU_FA_WEIGHTED","italic_species")) %>%
  ggplot(aes(x=FST_LI_GA_WEIGHTED,y=value)) +
  geom_jitter(aes(col=variable),alpha=0.5,size=3,height=0.1) +
  #coord_flip() +
  theme_classic()  +
  scale_colour_manual(name="Admixed population",
                      values=brewer.pal(n=4,name="RdBu")[c(1,4)],
                      labels=c("Med","Atl")) +
  xlab(expression(F[ST])) +
  ylab(expression("Presence of admixture ("*f[3]*")")) +
  xlim(c(0,0.8)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", 
           x=0.75, 
           y=2, 
           label=paste(num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  annotate(geom="text", 
           x=0.75, 
           y=1, 
           label=paste(denum_yes-num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  geom_pointrange(data=data_tmp_atl,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=brewer.pal(n=4,name="RdBu")[c(4)]) +
  geom_pointrange(data=data_tmp_med,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=brewer.pal(n=4,name="RdBu")[c(1)]) +
  labs(title="19 species")

## D
D_in_med=c(NA,NA,NA,NA,"Yes",NA,NA,"No","Yes",NA,NA,"Yes",NA,"Yes","Yes","Yes","Yes","No","No")
D_in_atl=c(NA,NA,NA,NA,"Yes",NA,NA,"No","No",NA,NA,"No",NA,"Yes","No","Yes","No","No","Yes")

fst$D_in_med=D_in_med
fst$D_in_atl=D_in_atl


kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$D_in_med)
kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$D_in_atl)

num_yes=nrow(fst[is.na(fst$D_in_med)==F & fst$D_in_med=="Yes",])+nrow(fst[is.na(fst$D_in_med)==F & fst$D_in_atl=="Yes",])
denum_yes=nrow(fst[is.na(fst$D_in_med)==F,])*2
percentage_yes=(nrow(fst[is.na(fst$D_in_med)==F & fst$D_in_med=="Yes",])+nrow(fst[is.na(fst$D_in_med)==F & fst$D_in_atl=="Yes",]))/(nrow(fst[is.na(fst$D_in_med)==F,])*2)

m1<-lm(FST_LI_GA_WEIGHTED~-1+D_in_med,data=fst)
data_tmp_med=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                        value=c(1.4,1.8),
                        FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                             coefficients(summary(m1))[2,1]),
                        sd=c(coefficients(summary(m1))[1,2],
                             coefficients(summary(m1))[2,2])
)

m1<-lm(FST_LI_GA_WEIGHTED~-1+D_in_atl,data=fst)
data_tmp_atl=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                        value=c(1.3,1.7),
                        FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                             coefficients(summary(m1))[2,1]),
                        sd=c(coefficients(summary(m1))[1,2],
                             coefficients(summary(m1))[2,2])
)
p_d<-fst[is.na(fst$D_in_atl)==F,c(1,2,11,12,15,18,19)] %>%
  melt(id.vars=c("SPECIES","Species_code","FST_LI_GA_WEIGHTED",
                 "FST_MU_FA_WEIGHTED","italic_species")) %>%
  ggplot(aes(x=FST_LI_GA_WEIGHTED,y=value)) +
  geom_jitter(aes(col=variable),alpha=0.5,size=3,height=0.1) +
  #coord_flip() +
  theme_classic()  +
  scale_colour_manual(name="Admixed population",
                      values=brewer.pal(n=4,name="RdBu")[c(1,4)],
                      labels=c("Med","Atl")) +
  xlab(expression(F[ST])) +
  ylab("Differential introgression (D)") +
  xlim(c(0,0.8)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", 
           x=0.75, 
           y=2, 
           label=paste(num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  annotate(geom="text", 
           x=0.75, 
           y=1, 
           label=paste(denum_yes-num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  geom_pointrange(data=data_tmp_atl,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=brewer.pal(n=4,name="RdBu")[c(4)]) +
  geom_pointrange(data=data_tmp_med,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=brewer.pal(n=4,name="RdBu")[c(1)]) +
  labs(title="10 species")

p_d
## proba
proba_ongoing_in=c("Yes","Yes","No","Yes","Yes","Yes",NA,"Yes","Yes","Yes","Yes","Yes","Yes","Yes","Yes","No","Yes","Yes","No")
proba_ongoing_out=c("Yes","Yes","No","Yes","Yes","No",NA,"Yes","Yes","No","Yes","Yes","Yes","Yes","No","No","Yes","Yes","No")

fst$proba_ongoing_in=proba_ongoing_in
fst$proba_ongoing_out=proba_ongoing_out

kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$proba_ongoing_in)
kruskal.test(fst$FST_LI_GA_WEIGHTED,
             fst$proba_ongoing_out)

num_yes=nrow(fst[fst$proba_ongoing_in=="Yes",])+nrow(fst[fst$proba_ongoing_out=="Yes",])
denum_yes=(nrow(fst)-1)*2
percentage_yes=(nrow(fst[fst$proba_ongoing_in=="Yes",])+nrow(fst[fst$proba_ongoing_out=="Yes",]))/((nrow(fst)-1)*2)

m1<-lm(FST_LI_GA_WEIGHTED~-1+proba_ongoing_in,data=fst)
data_tmp_in=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                       value=c(1.4,1.8),
                       FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                            coefficients(summary(m1))[2,1]),
                       sd=c(coefficients(summary(m1))[1,2],
                            coefficients(summary(m1))[2,2])
)

m1<-lm(FST_LI_GA_WEIGHTED~-1+proba_ongoing_out,data=fst)
data_tmp_out=data.frame(variable_2=c("introgression_in_atl","introgression_in_atl"),
                        value=c(1.3,1.7),
                        FST_LI_GA_WEIGHTED=c(coefficients(summary(m1))[1,1],
                                             coefficients(summary(m1))[2,1]),
                        sd=c(coefficients(summary(m1))[1,2],
                             coefficients(summary(m1))[2,2])
)
pabc<-fst[is.na(fst[,20])==F,c(1,2,11,12,15,20,21)] %>%
  melt(id.vars=c("SPECIES","Species_code","FST_LI_GA_WEIGHTED",
                 "FST_MU_FA_WEIGHTED","italic_species")) %>%
  ggplot(aes(x=FST_LI_GA_WEIGHTED,y=value)) +
  geom_jitter(aes(col=variable),alpha=0.5,size=3,height=0.1) +
  #coord_flip() +
  theme_classic()  +
  scale_colour_manual(name="Population comparison",
                      values=viridis::cividis(2,begin=0.9,end=0.5),
                      labels=c("In","Out")) +
  xlab(expression(F[ST])) +
  ylab("Ongoing gene flow between \n outer populations") +
  xlim(c(0,0.8)) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  annotate(geom="text", 
           x=0.75, 
           y=2, 
           label=paste(num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  annotate(geom="text", 
           x=0.75, 
           y=1, 
           label=paste(denum_yes-num_yes,
                       "/",
                       denum_yes,
                       sep="")) +
  geom_pointrange(data=data_tmp_in,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=viridis::cividis(2,begin=0.5,end=0.9)[2]) +
  geom_pointrange(data=data_tmp_out,
                  aes(xmin=FST_LI_GA_WEIGHTED-sd,
                      xmax=FST_LI_GA_WEIGHTED+sd,
                      x=FST_LI_GA_WEIGHTED,
                      y=value),
                  col=viridis::cividis(2,begin=0.5,end=0.9)[1])+
  labs(title="18 species")
pabc



jpeg(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig2.jpg",sep=""),width=17.5,height=4,units="in",res=2000)
print(ggpubr::ggarrange(ggpubr::ggarrange(p_f3,p_d,common.legend = T,legend="bottom",labels = c("A","B")),
                        pabc,widths = c(0.66,0.33),legend="bottom",labels=c("","C")))
dev.off()

pdf(paste("/Users/divco/Documents/COGEDIV/figures_paper/article_Fig2.pdf",sep=""),width=17.5,height=4)
print(ggpubr::ggarrange(ggpubr::ggarrange(p_f3,p_d,common.legend = T,legend="bottom",labels = c("A","B")),
                        pabc,widths = c(0.66,0.33),legend="bottom",labels=c("","C")))
dev.off()
