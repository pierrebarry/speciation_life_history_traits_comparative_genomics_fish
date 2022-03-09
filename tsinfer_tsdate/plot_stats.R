library(viridis)
library(ggExtra)
library(ggpubr)
library(plotly)

rm(list=ls())

load(file="/shared/projects/abc_fish/tree_topology/output/Dlabr/Dlabr_all_TMRCA.Rdata")
all_Dlabr=all
all_Dlabr$sp="Dlabr"
load(file="/shared/projects/abc_fish/tree_topology/output/Hgutt/Hgutt_all_TMRCA.Rdata")
all_Hgutt=all
all_Hgutt$sp="Hgutt"
load(file="/shared/projects/abc_fish/tree_topology/output/Lbude/Lbude_all_TMRCA.Rdata")
all_Lbude=all
all_Lbude$sp="Lbude"
load(file="/shared/projects/abc_fish/tree_topology/output/Scant/Scant_all_TMRCA.Rdata")
all_Scant=all
all_Scant$sp="Scant"
load(file="/shared/projects/abc_fish/tree_topology/output/Scabr/Scabr_all_TMRCA.Rdata")
all_Scabr=all
all_Scabr$sp="Scabr"
load(file="/shared/projects/abc_fish/tree_topology/output/Spilc/Spilc_all_TMRCA.Rdata")
all_Spilc=all
all_Spilc$sp="Spilc"

all=rbind(all_Dlabr,
          all_Hgutt,
          all_Lbude,
          all_Scant,
          all_Scabr,
          all_Spilc)

all=all[all$BLOCK_LENGTH>=10000,]

all <- all %>%
  mutate(POP1 = replace(POP1, POP1 == "Fa" | POP1 == "Ga", "Atl")) %>%
  mutate(POP2 = replace(POP2, POP2 == "Fa" | POP2 == "Ga", "Atl")) %>%
  mutate(POP1 = replace(POP1, POP1 == "Li" | POP1 == "Mu", "Med")) %>%
  mutate(POP2 = replace(POP2, POP2 == "Li" | POP2 == "Mu", "Med"))


all$POP_POP=paste(all$POP1,"-",all$POP2,sep="")
save(all,file="/shared/home/pbarry/all.Rdata")
p1<-ggplot(all,aes(x=DIVERGENCE))+
  geom_histogram(colour="black",
                 position='identity',
                 bins=75,
                 aes(fill=POP_POP),
                 alpha=0.75)+
  #geom_density(alpha=0.75,size=1,aes(fill=POP_POP)) +
  facet_grid(sp~POP_POP,scales="free_y") +
  theme_classic() +
  #coord_flip() +
  xlab("Time") +
  ylab("") +
  ggtitle("Distribution of TMRCA") +
  scale_fill_viridis_d()
p1
