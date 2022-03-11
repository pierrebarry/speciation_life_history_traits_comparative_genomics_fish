#bcftools stats Hgutt_indel5bp_snponly.vcf.gz > vcf_stats_Hgutt
#rtg vcfstats Hgutt_indel5bp_snponly.vcf.gz > vcf_stats_Hgutt_rtg
#vcftools --depth --gzvcf Hgutt_indel5bp_snponly.vcf.gz --out indiv_Hgutt
#vcftools --site-mean-depth --gzvcf Hgutt_indel5bp_snponly.vcf.gz --out Hgutt 
#vcftools --site-quality --gzvcf Hgutt_indel5bp_snponly.vcf.gz --out Hgutt 

SPECIES=c("Aboye",
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

verif=rep(1,length(SPECIES))
for (i in 1:length(SPECIES)){
  if (!file.exists(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/vcf_stats_",SPECIES[i],sep=""))==TRUE){
    verif[i]=0
    print(i)
  }
}

SPECIES=SPECIES[verif==1]
print(SPECIES)

## SPECIES

VCF_STATS_SPECIES=data.frame(SPECIES=c(NA),
                             NBR_SNPS=c(NA),
                             TS=c(NA),
                             TV=c(NA),
                             TS_TV=c(NA))

VCF_QUALITY_SPECIES=data.frame(SPECIES=c(NA),
                               QUAL=c(NA),
                               SNPS=c(NA),
                               TS=c(NA),
                               TV=c(NA))

VCF_SUBS_SPECIES=data.frame(SPECIES=c(NA),
                            TYPE=c(NA),
                            COUNT=c(NA))

for (i in 1:length(SPECIES)){
  
  vcf_stats<-readLines(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/vcf_stats_",SPECIES[i],sep=""))
  #vcf_stats<-readLines(paste(SPECIES[i],"/vcf_stats_",SPECIES[i],sep=""))
  
  nbr_snps <- grep("SN\t0\tnumber of SNPs:\t",vcf_stats, value=TRUE)
  nbr_snps <- as.numeric(regmatches(nbr_snps, gregexpr("[[:digit:]]+", nbr_snps))[[1]][2])
  
  tmp <- grep("TSTV\t0\t",vcf_stats, value=TRUE)
  tmp <- regmatches(tmp, gregexpr("[[:digit:].]+", tmp))[[1]]
  Ts = as.numeric(tmp[2])
  Tv = as.numeric(tmp[3])
  Ts_Tv = as.numeric(tmp[4])
  
  vec_tmp=c(SPECIES[i],nbr_snps,Ts,Tv,Ts_Tv)
  VCF_STATS_SPECIES=rbind(VCF_STATS_SPECIES,vec_tmp)
    
  quality_distri=data.frame(SPECIES=c(NA),
                            QUAL=c(NA),
                            SNPS=c(NA),
                            TS=c(NA),
                            TV=c(NA))
  
  tmp <- grep("QUAL\t0",vcf_stats, value=TRUE)
  
  for (j in 1:length(tmp)){
    tt<-as.numeric(regmatches(tmp[j], gregexpr("[[:digit:].]+", tmp[j]))[[1]])
    quality_distri=rbind(quality_distri,
                         c(SPECIES[i],
                           tt[2],
                           tt[3],
                           tt[4],
                           tt[5]))
  }
  
  quality_distri=quality_distri[-1,]
  
  VCF_QUALITY_SPECIES=rbind(VCF_QUALITY_SPECIES,
                            quality_distri)
  
  subs=data.frame(SPECIES=c(NA),
                            TYPE=c(NA),
                            COUNT=c(NA))
  
  tmp <- grep("ST\t0\t",vcf_stats, value=TRUE)
  
  for (j in 1:length(tmp)){
    tt<-as.numeric(regmatches(tmp[j], gregexpr("[[:digit:].]+", tmp[j]))[[1]])[2]
    subs=rbind(subs,
              c(SPECIES[i],
                strsplit(tmp[j],"\t")[[1]][3],
                      tt))
  }
  
  subs=subs[-1,]
  
  VCF_SUBS_SPECIES=rbind(VCF_SUBS_SPECIES,
                         subs)
}

VCF_STATS_SPECIES=VCF_STATS_SPECIES[-1,]
VCF_QUALITY_SPECIES=VCF_QUALITY_SPECIES[-1,]
VCF_SUBS_SPECIES=VCF_SUBS_SPECIES[-1,]

VCF_STATS_SPECIES$NBR_SNPS=as.numeric(VCF_STATS_SPECIES$NBR_SNPS)
VCF_STATS_SPECIES$TS=as.numeric(VCF_STATS_SPECIES$TS)
VCF_STATS_SPECIES$TV=as.numeric(VCF_STATS_SPECIES$TV)
VCF_STATS_SPECIES$TS_TV=as.numeric(VCF_STATS_SPECIES$TS_TV)

VCF_QUALITY_SPECIES$QUAL=as.numeric(VCF_QUALITY_SPECIES$QUAL)
VCF_QUALITY_SPECIES$SNPS=as.numeric(VCF_QUALITY_SPECIES$SNPS)
VCF_QUALITY_SPECIES$TS=as.numeric(VCF_QUALITY_SPECIES$TS)
VCF_QUALITY_SPECIES$TV=as.numeric(VCF_QUALITY_SPECIES$TV)

VCF_SUBS_SPECIES$COUNT=as.numeric(VCF_SUBS_SPECIES$COUNT)

VCF_SPECIES=list(VCF_STATS_SPECIES,
                 VCF_QUALITY_SPECIES,
                 VCF_SUBS_SPECIES)

save(VCF_SPECIES,file="/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/VCF_SPECIES.Rdata")

## DEPTH and QUALITY

VCF_DEPTH_QUALITY_PER_SITE=data.frame(SPECIES=NA,
                                      DEPTH=NA,
                                      QUALITY=NA)
#setwd("~/cogediv_tmp")


for (i in 1:length(SPECIES)){
  
  depth_per_site<-read.table(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/",SPECIES[i],".ldepth.mean",sep=""),header=T)
  quality_per_site<-read.table(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/",SPECIES[i],".lqual",sep=""),header=T)
 
  depth_per_site<-depth_per_site[sample(nrow(depth_per_site),nrow(depth_per_site)/1000),]
  quality_per_site<-quality_per_site[sample(nrow(quality_per_site),nrow(quality_per_site)/1000),] 
  #depth_per_site<-read.table(paste(SPECIES[i],"/",SPECIES[i],".ldepth.mean",sep=""),header=T)
  #quality_per_site<-read.table(paste(SPECIES[i],"/",SPECIES[i],".lqual",sep=""),header=T)
  
  depth_per_site=depth_per_site[,3]
  quality_per_site=quality_per_site[,3]
  
  PER_SITE=data.frame(SPECIES=rep(SPECIES[i],length(depth_per_site)),
                      DEPTH=depth_per_site,
                      QUALITY=quality_per_site)
  
  VCF_DEPTH_QUALITY_PER_SITE=rbind(VCF_DEPTH_QUALITY_PER_SITE,
                               PER_SITE)
  
}

save(VCF_DEPTH_QUALITY_PER_SITE,file="/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/VCF_DEPTH_QUALITY_PER_SITE.Rdata")



## INDIV

VCF_STATS_INDIV=data.frame(SPECIES=c(NA),
                           SAMPLES=c(NA),
                           #LOCATION=c(NA),
                           DEPTH=c(NA),
                           NBR_SNPS=c(NA),
                           SNPS_SAME_AS_REF=c(NA),
                           PERCENTAGE_SNPS_SAME_AS_REF=c(NA),
                           MISSING_GENOTYPE=c(NA),
                           PERCENTAGE_MISSING_GENOTYPE=c(NA),
                           PHASED_GENOTYPE_PERCENTAGE=c(NA),
                           PHASED_GENOTYPE_NBR=c(NA),
                           GENOTYPE_NBR=c(NA),
                           SNP_TS_TV=c(NA),
                           HET_HOM=c(NA))

for (i in 1:length(SPECIES)){
  
  vcf_stats<-readLines(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/vcf_stats_",SPECIES[i],"_rtg",sep=""))
  #vcf_stats<-readLines(paste(SPECIES[i],"/vcf_stats_",SPECIES[i],"_rtg",sep=""))
  
  indiv_depth<-read.table(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",SPECIES[i],"/VCF/indiv_",SPECIES[i],".idepth",sep=""),header=T)
  #indiv_depth<-read.table(paste(SPECIES[i],"/indiv_",SPECIES[i],".idepth",sep=""),header=T)
  
  indiv <- grep(paste("Sample Name: ",SPECIES[i],sep=""),vcf_stats)
  indiv=c(indiv,length(vcf_stats))
  
  for (j in 1:(length(indiv)-1)){
    
    sample=strsplit(vcf_stats[indiv[j]],": ")[[1]][2]
    #location=str_sub(sample,start=6,end=7)
    
    depth=indiv_depth$MEAN_DEPTH[j]
    
    tmp_indiv=vcf_stats[indiv[j]:indiv[(j+1)]]
    nbr_snps <- grep("SNPs",tmp_indiv,value=T)
    nbr_snps <- as.numeric(regmatches(nbr_snps, gregexpr("[[:digit:]]+", nbr_snps))[[1]][1])

    snps_same_as_ref <- grep("Same as reference",tmp_indiv,value=T)
    snps_same_as_ref <- as.numeric(regmatches(snps_same_as_ref, gregexpr("[[:digit:]]+", snps_same_as_ref))[[1]][1])
 
    missing_genotype <- grep("Missing Genotype",tmp_indiv,value=T)
    missing_genotype <- as.numeric(regmatches(missing_genotype, gregexpr("[[:digit:]]+", missing_genotype))[[1]][1])
 
    phased_genotype <- grep("Phased Genotypes",tmp_indiv,value=T)
    phased_genotype_percentage <- as.numeric(regmatches(phased_genotype, gregexpr("[[:digit:].]+", phased_genotype))[[1]][1])
    phased_genotype_nbr <- as.numeric(regmatches(phased_genotype, gregexpr("[[:digit:].]+", phased_genotype))[[1]][2])   
    genotype_nbr <- as.numeric(regmatches(phased_genotype, gregexpr("[[:digit:].]+", phased_genotype))[[1]][3])   
    
    percentage_snps_same_as_ref=snps_same_as_ref/genotype_nbr
    percentage_missing_genotype=1-genotype_nbr/VCF_SPECIES[[1]]$NBR_SNPS[which(SPECIES[i]==VCF_SPECIES[[1]]$SPECIES)]
      
    snp_ts_tv <- grep("SNP Transitions/Transversions",tmp_indiv,value=T)
    snp_ts_tv <- as.numeric(regmatches(snp_ts_tv, gregexpr("[[:digit:].]+", snp_ts_tv))[[1]][1])
    
    het_hom <- grep("Total Het/Hom ratio",tmp_indiv,value=T)
    het_hom <- as.numeric(regmatches(het_hom, gregexpr("[[:digit:].]+", het_hom))[[1]][1])
    
    vect<-c(SPECIES[i],
            sample,
            #location,
            depth,
            nbr_snps,
            snps_same_as_ref,
            percentage_snps_same_as_ref,
            missing_genotype,
            percentage_missing_genotype,
            phased_genotype_percentage,
            phased_genotype_nbr,
            genotype_nbr,
            snp_ts_tv,
            het_hom)
    
    VCF_STATS_INDIV=rbind(VCF_STATS_INDIV,
                          vect)
    
    
  }
  
}

VCF_STATS_INDIV=VCF_STATS_INDIV[-1,]

for (i in 3:ncol(VCF_STATS_INDIV)){
  VCF_STATS_INDIV[,i]=as.numeric(VCF_STATS_INDIV[,i])
}

save(VCF_STATS_INDIV,file="/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/VCF_STATS_INDIV.Rdata")


