SAMPLES=c()
SPECIES=c()

for (i in 1:length(list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry"))){
  if (dir.exists(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],"/Mapping",sep=""))==TRUE){
    
    for (j in 1:length(list.files(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],"/Mapping",sep="")))){
      
      if (file.exists(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",
                            list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],
                            "/Mapping/",
                            list.files(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",
                                             list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],"/Mapping",sep=""))[j],
                            "/",
                            list.files(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",
                                             list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],"/Mapping",sep=""))[j],
                            "_samtools_stats_picard.stats",
                            sep=""))==TRUE)
        SAMPLES=c(SAMPLES,
                  list.files(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",
                                   list.files("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry")[i],"/Mapping",sep=""))[j])

    }
      
  }
  
}

SPECIES=substr(SAMPLES,0,5)

globalstats=data.frame(SAMPLES=c(NA),
                       SPECIES=c(NA),
                       LOCATION=c(NA),
                       stats=c(NA),
                       value=c(NA))

readlength_data=data.frame(SAMPLES=c(NA),
                           SPECIES=c(NA),
                           LOCATION=c(NA),
                           read_length=c(NA),
                           Count=c(NA))

insertsize_data=data.frame(SAMPLES=c(NA),
                           SPECIES=c(NA),
                           LOCATION=c(NA),
                           insert_size=c(NA),
                           allpairs=c(NA))

coverage_data=data.frame(SAMPLES=c(NA),
                         SPECIES=c(NA),
                         LOCATION=c(NA),
                         Coverage=c(NA),
                         Count=c(NA))

gc_coverage_data=data.frame(SAMPLES=c(NA),
                            SPECIES=c(NA),
                            LOCATION=c(NA),
                            GC=c(NA),
                            Count=c(NA))

insertion_data=data.frame(SAMPLES=c(NA),
                          SPECIES=c(NA),
                          LOCATION=c(NA),
                          insertion=c(NA),
                          count=c(NA))

deletion_data=data.frame(SAMPLES=c(NA),
                         SPECIES=c(NA),
                         LOCATION=c(NA),
                         deletion=c(NA),
                         count=c(NA))

for (i in 1:length(SAMPLES)){
  
  flagstat<-readLines(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/Mapping/",SAMPLES[i],"/",SAMPLES[i],"_flagstat_picard.txt",sep=""))
  stats<-readLines(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/Mapping/",SAMPLES[i],"/",SAMPLES[i],"_samtools_stats_picard.stats",sep=""))
  
  # GLOBAL STATS
  sn <- grep("^SN",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      Name=c(NA),
                      Value=c(NA))
  for (j in 1:length(sn)){
    data_tmp=rbind(data_tmp,strsplit(sn[j],'\t')[[1]])
  }
  sn=data_tmp[-1,]
  for (j in 1:nrow(sn)){
    globalstats=rbind(globalstats,
                      c(SAMPLES[i],
                        substr(SAMPLES[i],0,5),
                        substr(SAMPLES[i],6,7),
                        sn$Name[j],
                        sn$Value[j]))
  }
  
  # READ LENGTH
  rl <- grep("^RL",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      read_length=c(NA),
                      count=c(NA))
  for (j in 1:length(rl)){
    data_tmp=rbind(data_tmp,strsplit(rl[j],'\t')[[1]])
  }
  rl=data_tmp[-1,]
  for (j in 1:length(rl$count)){
    readlength_data=rbind(readlength_data,
                          c(SAMPLES[i],
                            substr(SAMPLES[i],0,5),
                            substr(SAMPLES[i],6,7),
                            rl$read_length[j],
                            rl$count[j]))
  }
  
  # INSERT SIZE
  is <- grep("^IS",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      insert_size=c(NA),
                      all_pairs=c(NA),
                      inward=c(NA),
                      outward=c(NA),
                      other=c(NA))
  for (j in 1:length(is)){
    data_tmp=rbind(data_tmp,strsplit(is[j],'\t')[[1]])
  }
  is=data_tmp[-1,]
  for (j in 1:length(is$insert_size)){
    insertsize_data=rbind(insertsize_data,
                          c(SAMPLES[i],
                            substr(SAMPLES[i],0,5),
                            substr(SAMPLES[i],6,7),
                            is$insert_size[j],
                            is$all_pairs[j]))
  }
  
  # COVERAGE PLOT
  cov <- grep("^COV",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      value=c(NA),
                      Coverage=c(NA),
                      count=c(NA))
  for (j in 1:length(cov)){
    data_tmp=rbind(data_tmp,strsplit(cov[j],'\t')[[1]])
  }
  cov=data_tmp[-1,]
  for (j in 1:length(cov$count)){
    coverage_data=rbind(coverage_data,
                        c(SAMPLES[i],
                          substr(SAMPLES[i],0,5),
                          substr(SAMPLES[i],6,7),
                          cov$Coverage[j],
                          cov$count[j]))
  }
  
  # GC Coverage
  gcd <- grep("^GCD",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      GC=c(NA),
                      Unique_Sequence=c(NA),
                      X10th=c(NA),
                      X25th=c(NA),
                      X50th=c(NA),
                      X75th=c(NA),
                      X90th=c(NA))
  for (j in 1:length(gcd)){
    data_tmp=rbind(data_tmp,strsplit(gcd[j],'\t')[[1]])
  }
  gcd=data_tmp[-1,]
  for (j in 1:length(gcd$GC)){
    gc_coverage_data=rbind(gc_coverage_data,
                           c(SAMPLES[i],
                             substr(SAMPLES[i],0,5),
                             substr(SAMPLES[i],6,7),
                             gcd$GC[j],
                             gcd$Unique_Sequence[j]))
  }
  
  #INSERTION DATA
  id <- grep("^ID",stats, value=TRUE)
  data_tmp=data.frame(ID=c(NA),
                      length=c(NA),
                      insertion_count=c(NA),
                      deletion_count=c(NA))
  for (j in 1:length(id)){
    data_tmp=rbind(data_tmp,strsplit(id[j],'\t')[[1]])
  }
  id=data_tmp[-1,]
  
  for (j in 1:length(id$length)){
    insertion_data=rbind(insertion_data,
                         c(SAMPLES[i],
                           substr(SAMPLES[i],0,5),
                           substr(SAMPLES[i],6,7),
                           id$length[j],
                           id$insertion_count[j]))
  }
  for (j in 1:length(id$length)){
    deletion_data=rbind(deletion_data,
                        c(SAMPLES[i],
                          substr(SAMPLES[i],0,5),
                          substr(SAMPLES[i],6,7),
                          id$length[j],
                          id$deletion_count[j]))
  }
  
  
}

globalstats=globalstats[-1,]
globalstats$value=as.numeric(globalstats$value)

readlength_data=readlength_data[-1,]
readlength_data$read_length=as.numeric(readlength_data$read_length)
readlength_data$Count=as.numeric(readlength_data$Count)

insertsize_data=insertsize_data[-1,]
insertsize_data$insert_size=as.numeric(insertsize_data$insert_size)
insertsize_data$allpairs=as.numeric(insertsize_data$allpairs)

coverage_data=coverage_data[-1,]
coverage_data$Coverage=as.numeric(coverage_data$Coverage)
coverage_data$Count=as.numeric(coverage_data$Count)

gc_coverage_data=gc_coverage_data[-1,]
gc_coverage_data$GC=as.numeric(gc_coverage_data$GC)
gc_coverage_data$Count=as.numeric(gc_coverage_data$Count)

insertion_data=insertion_data[-1,]
insertion_data$insertion=as.numeric(insertion_data$insertion)
insertion_data$count=as.numeric(insertion_data$count)

deletion_data=deletion_data[-1,]
deletion_data$deletion=as.numeric(deletion_data$deletion)
deletion_data$count=as.numeric(deletion_data$count)

samtools_report=list(globalstats,
                     readlength_data,
                     insertsize_data,
                     coverage_data,
                     gc_coverage_data,
                     insertion_data,
                     deletion_data)

save(samtools_report,file="/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/samtools_report.Rdata")