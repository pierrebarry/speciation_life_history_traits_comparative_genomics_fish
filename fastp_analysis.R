library(ggplot2)
library(reshape2)
library(ggthemes)

read=0

# Import summary fastp file
setwd("C:/Users/pierr/Documents/PROJETS/COGEDIV/GENOMIC/fastp")
fastp_process=read.table('Summary_fastp.txt',header=T,sep=';')

# Modify the file
fastp_process=fastp_process[-1,]
fastp_process=fastp_process[c("SAMPLE",
                              "TOTAL_READS_BEFORE_FILTERING","TOTAL_BASES_BEFORE_FILTERING",
                              "GC_CONTENT_BEFORE_FILTERING","Q20_RATE_BEFORE_FILTERING",
                              "Q30_RATE_BEFORE_FILTERING",
                              "TOTAL_READS_AFTER_FILTERING","TOTAL_BASES_AFTER_FILTERING",
                              "GC_CONTENT_AFTER_FILTERING","Q20_RATE_AFTER_FILTERING",
                              "Q30_RATE_AFTER_FILTERING",
                              "PASSED_FILTER_READS","CORRECTED_READS","CORRECTED_BASES",
                              "LOW_QUALITY_READS","TOO_MANY_N_READS","TOO_SHORT_READS","LOW_COMPLEXITY_READS",
                              "DUPLICATION_RATE")]

fastp_process$TOTAL_READS_BEFORE_FILTERING=fastp_process$TOTAL_READS_BEFORE_FILTERING/1e9
fastp_process$TOTAL_BASES_BEFORE_FILTERING=fastp_process$TOTAL_BASES_BEFORE_FILTERING/1e9

fastp_process$TOTAL_READS_AFTER_FILTERING=fastp_process$TOTAL_READS_AFTER_FILTERING/1e9
fastp_process$TOTAL_BASES_AFTER_FILTERING=fastp_process$TOTAL_BASES_AFTER_FILTERING/1e9

fastp_process$PASSED_FILTER_READS=fastp_process$PASSED_FILTER_READS/100
fastp_process$CORRECTED_READS=fastp_process$CORRECTED_READS/100
fastp_process$LOW_QUALITY_READS=fastp_process$LOW_QUALITY_READS/100
fastp_process$TOO_SHORT_READS=fastp_process$TOO_SHORT_READS/100

# Read each column one by one
if (read==1){
  
  for (i in 2:ncol(fastp_process)){
    if(i==2){
      print(paste('Number of samples = ',nrow(fastp_process),sep=""))
      readline('')
      
    }
    hist(as.numeric(fastp_process[,i]),main=colnames(fastp_process)[i],xlab=colnames(fastp_process)[i],breaks=25)
    readline(colnames(fastp_process)[i])
    
  }
  
}



# Export summary histograms
d <- melt(fastp_process)

tiff(paste('fast_process_11062019.jpg',sep=" "),width = 11000, height = 6000, units = "px", res = 800)
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram(fill="dodgerblue2",alpha=0.25,col="black") +
  geom_rangeframe() + theme_minimal() 
dev.off()

# Species stats
SPECIES=c()
for (i in 1:nrow(fastp_process)){
  
  SPECIES[i]=substr(fastp_process[i,1], start = 1, stop = 5)
  
  if (substr(fastp_process[i,1], start = 1, stop = 4)=="Dlab"){
    
    SPECIES[i]="Dlabr"
  }
  
}

fastp_process=cbind(SPECIES,fastp_process)

plot(fastp_process$SPECIES,fastp_process$TOTAL_READS_BEFORE_FILTERING)
plot(fastp_process$SPECIES,fastp_process$TOTAL_BASES_BEFORE_FILTERING)
plot(fastp_process$SPECIES,fastp_process$GC_CONTENT_BEFORE_FILTERING)
plot(fastp_process$SPECIES,fastp_process$Q20_RATE_BEFORE_FILTERING)
plot(fastp_process$SPECIES,fastp_process$Q30_RATE_BEFORE_FILTERING)
plot(fastp_process$SPECIES,fastp_process$PASSED_FILTER_READS)
plot(fastp_process$SPECIES,fastp_process$TOTAL_READS_AFTER_FILTERING)
plot(fastp_process$SPECIES,fastp_process$TOTAL_BASES_AFTER_FILTERING)
plot(fastp_process$SPECIES,fastp_process$GC_CONTENT_AFTER_FILTERING)
plot(fastp_process$SPECIES,fastp_process$Q20_RATE_AFTER_FILTERING)
plot(fastp_process$SPECIES,fastp_process$Q30_RATE_AFTER_FILTERING)
plot(fastp_process$SPECIES,fastp_process$CORRECTED_READS)
plot(fastp_process$SPECIES,fastp_process$CORRECTED_BASES)
plot(fastp_process$SPECIES,fastp_process$LOW_QUALITY_READS)
plot(fastp_process$SPECIES,fastp_process$TOO_MANY_N_READS)
plot(fastp_process$SPECIES,fastp_process$TOO_SHORT_READS)
plot(fastp_process$SPECIES,fastp_process$LOW_COMPLEXITY_READS)
plot(fastp_process$SPECIES,fastp_process$DUPLICATION_RATE)

for (i in 3:ncol(fastp_process)){
  
  for (j in 3:ncol(fastp_process)){
    
    if (j>i){
      
      plot(fastp_process[,i],fastp_process[,j],xlab=colnames(fastp_process)[i],ylab=colnames(fastp_process)[j])
      print(summary(lm(fastp_process[,j]~fastp_process[,i])))
      readline("--------------------------------------------")
      
    }
    
  }
  
}
plot(fastp_process)

# Duplication rate increases with total reads/bases
# Q20/Q30 rate decreases with GC content
# Passed filter reads/Too many N reads  decreas with GC content
# Low quality reads/increases with GC content
# Duplication rate decreaseswith GC content
