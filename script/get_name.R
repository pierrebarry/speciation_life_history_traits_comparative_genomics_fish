args<-commandArgs(TRUE)
sp=args[[1]]
setwd(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",sp,"/VariantCalling",sep=""))
loca=c("Li","Mu","Fa","Ga")
num=c("1","2","3","4","5","6","7","8","9","10","11","12")
name=data.frame(sample=c(NA),path=c(NA))

for (i in 1:length(loca)){
  for (j in 1:length(num)){
    if (dir.exists(paste(sp,loca[i],num[j],sep=""))==TRUE){
      name=rbind(name,c(noquote(paste(sp,loca[i],num[j],sep="")),
                   noquote(paste("/share/tycho_poolz1/pagagnaire/COGEDIV/pbarry/",sp,"/VariantCalling/",sp,loca[i],num[j],"/",sp,loca[i],num[j],"_gvcf_first.g.vcf",sep=""))))
    }
  }
}
name=name[-1,]
name<-as.matrix(name)
write.table(name, 
            paste("name_",sp,".txt",sep=""),
            sep = "\t",
            row.names = FALSE, 
            col.names = FALSE,
	    quote=FALSE)
