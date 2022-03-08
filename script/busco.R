library(RColorBrewer)
library(formattable)
library(ggplot2)
library(strex)
library(reshape2)
library(wesanderson)
setwd("C:/Users/divco/Documents/COGEDIV/")

list_species=c("Aboye",
               "Afall",
               "Cgale",
               "Cjuli",
               "Dlabr",
               "Dpunt",
               "Eencr",
               "Gnige",
               "Hgutt",
               "Lbude",
               "Lmorm",
               "Mmerl",
               "Msurm",
               "Peryt",
               "Scabr",
               "Scant",
               "Scine",
               "Spilc",
               "Ssard",
               "Styph")

data=data.frame(SPECIES=c(NA),
                COMPLETE=c(NA),
                SINGLE=c(NA),
                DUPLICATED=c(NA),
                FRAGMENTED=c(NA),
                MISSING=c(NA)
                )
for (i in list_species){
  txt=readLines(paste("data/BUSCO/short_summary.specific.actinopterygii_odb10.",i,"_BUSCO.txt",sep=""))
  num=str_extract_numbers(txt[9])[[1]]
  complete=paste(num[1],".",num[2],sep="")
  single=paste(num[3],".",num[4],sep="")
  duplicated=paste(num[5],".",num[6],sep="")
  fragmented=paste(num[7],".",num[8],sep="")
  missing=paste(num[9],".",num[10],sep="")
  
  data=rbind(data,
             c(i,complete,single,duplicated,fragmented,missing))
  
}

data=data[-1,]

for (i in 2:ncol(data)){
  data[,i]=as.numeric(data[,i])
}

data$SPECIES=factor(data$SPECIES,
                    levels=c("Aboye",
                             "Afall",
                             "Cgale",
                             "Cjuli",
                             "Dlabr",
                             "Dpunt",
                             "Eencr",
                             "Gnige",
                             "Hgutt",
                             "Lbude",
                             "Lmorm",
                             "Mmerl",
                             "Msurm",
                             "Peryt",
                             "Scabr",
                             "Scant",
                             "Scine",
                             "Spilc",
                             "Ssard",
                             "Styph"),
                             labels=c("Atherina boyeri",
                                      "Alosa fallax",
                                      "Coryphoblennius galerita",
                                      "Coris julis",
                                      "Dicentrarchus labrax",
                                      "Diplodus puntazzo",
                                      "Engraulis encrasicolus",
                                      "Gobius niger",
                                      "Hippocampus guttulatus",
                                      "Lophius budegassa",
                                      "Lithognathus mormyrus",
                                      "Merluccius merluccius",
                                      "Mullus surmuletus",
                                      "Pagellus erythrinus",
                                      "Serranus cabrilla",
                                      "Spondyliosoma cantharus",
                                      "Symphodus cinereus",
                                      "Sardina pilchardus",
                                      "Sarda sarda",
                                      "Syngnathus typhle"))
p<-melt(data[,-2],id.vars=c("SPECIES")) %>%
  mutate(variable = fct_relevel(variable, "MISSING","FRAGMENTED","DUPLICATED","SINGLE")) %>%
ggplot(aes(x=SPECIES,y=value,fill=variable)) +
  geom_bar(position="stack", stat="identity",alpha=0.6) +
  coord_flip() +
  scale_fill_manual(values=rev(wes_palette(n=5, name="Zissou1"))) +
  theme_classic() +
  theme(axis.text.y = element_text(face="italic")) +
  ylab("%") +
  xlab("Species") +
  scale_x_discrete(limits = rev(levels(data$SPECIES))) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))

for (i in 1:nrow(data)){
  
  p<-p+
    annotate("text", label = paste("C:",data[i,]$COMPLETE,"%[S:",
                                   data[i,]$SINGLE,"%,D:",data[i,]$DUPLICATED,
                                   "%],F:",data[i,]$FRAGMENTED,"%,M:",
                                   data[i,]$MISSING,"%"), x = 20-i+1, y = 17.5) 
}

pdf(paste("/Users/divco/Documents/COGEDIV/figures/busco.pdf",sep=""),width=13.5,height=6)
print(p)
dev.off()

jpeg("/Users/divco/Documents/COGEDIV/figures/busco.jpg",width=1350,height=600,quality=100,res=100)
print(p)
dev.off()        
