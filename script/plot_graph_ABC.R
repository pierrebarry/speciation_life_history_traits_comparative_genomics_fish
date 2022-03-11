library(ggplot2)
library(ggpubr)
library(reshape2)
library(gridExtra)


### BUSCO -----
plot_plot=0

SPECIES=c("Aboye","Cgale","Cjuli","Dlabr","Dpunt","Hgutt","Lbude","Lmorm","Mmerl","Msurm","Peryt","Scabr","Scant","Scine","Spilc","Ssard","Styph")
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))

a<-read.csv(file="/home/labosea1/ABC/data_ABC.csv",header=T)
a=a[a$Migr=="Beta",]
a=a[a$Format=="Mu_Fa" | a$Format=="Li_Ga",] 
a=a[a$Method=="Random forest",]

data=data.frame(X=seq(0,max(a[a$Param=="N1" | a$Param=="N2" | a$Param=="Na",]$Estimate)*2 + max(a[a$Param=="N1" | a$Param=="N2" | a$Param=="Na",]$Estimate)/2,length.out=1000),
                          Y=seq(0,max(a[a$Param=="Tsplit",]$Estimate)+max(a[a$Param=="Tsplit",]$Estimate)/2,length.out=1000))

for (sp in SPECIES){
  
  for (pop in c("Li_Ga","Mu_Fa")){
    
    tmp<-a[a$SP==sp & a$Format==pop,]
    
    if (sp=="Dlabr" & pop=="Mu_Fa"){
      tmp=tmp[1:8,]
    }
    
    p<-ggplot(data,aes(x=X,y=Y)) + 
      theme_void() 
    
    if (strsplit(as.character(tmp$Format[1]),"_")[[1]][1]=="Li"){
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Gulf of Lion",
                 color=color_med_atl[4],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmin=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate),
          xmax=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[4]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N1",]$Estimate,0),sep=""),
                 color=color_med_atl[4],
                 fontface="bold",
                 size=2.5) 
      
    } else {
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Costa Calida",
                 color=color_med_atl[3],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmin=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate),
          xmax=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[3]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N1",]$Estimate,0),sep=""),
                 color=color_med_atl[3],
                 fontface="bold",
                 size=2.5) 
      
    }
    
    if (strsplit(as.character(tmp$Format[1]),"_")[[1]][2]=="Fa"){
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Algarve",
                 color=color_med_atl[2],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmax=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate),
          xmin=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[2]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N2",]$Estimate,0),sep=""),
                 color=color_med_atl[2],
                 fontface="bold",
                 size=2.5) 
      
    } else {
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Bay of Biscay",
                 color=color_med_atl[1],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmax=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate),
          xmin=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[1]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N2",]$Estimate,0),sep=""),
                 color=color_med_atl[1],
                 fontface="bold",
                 size=2.5) 
      
    }
    
    p<-p+
      geom_rect(xmin=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2),
                xmax=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2),
                ymin=tmp[tmp$Param=="Tsplit",]$Estimate,
                ymax=tmp[tmp$Param=="Tsplit",]$Estimate*1.5, alpha=0.1,fill="gray") +
      annotate(geom="text", 
               y=tmp[tmp$Param=="Tsplit",]$Estimate*1.5 + 50000, 
               x=(data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2))/2, 
               #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
               #label=substitute(paste(N[e],"=",e),
               #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
               label=paste("Na = ",round(tmp[tmp$Param=="Na",]$Estimate,0),sep=""),
               color="gray",
               fontface="bold",
               size=2.5) +
      geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                   y=tmp[tmp$Param=="Tsplit",]$Estimate,
                   xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                   yend=tmp[tmp$Param=="Tsplit",]$Estimate,
                   linetype="dashed") +
      annotate(geom="text", 
               y=tmp[tmp$Param=="Tsplit",]$Estimate, 
               x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
               label=paste("Tsplit = ",round(tmp[tmp$Param=="Tsplit",]$Estimate,0),sep=""),
               color="black",
               fontface="bold",
               size=2.5) 
    
    
    p<- p +
      geom_segment(x=0,
                   xend=0,
                   y=0,
                   yend=max(data$Y), color="black",arrow=arrow())
    
    if ("Tsc" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                     y=tmp[tmp$Param=="Tsc",]$Estimate,
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                     yend=tmp[tmp$Param=="Tsc",]$Estimate,
                     linetype="dashed")+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33,
                     yend=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66,
                     yend=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate, 
                 x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
                 label=paste("Tsc = ",round(tmp[tmp$Param=="Tsc",]$Estimate,0),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    } else if ("Tam" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                     y=tmp[tmp$Param=="Tam",]$Estimate,
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                     yend=tmp[tmp$Param=="Tam",]$Estimate,
                     linetype="dashed")+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tam",]$Estimate, 
                 x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
                 label=paste("Tam = ",round(tmp[tmp$Param=="Tam",]$Estimate,0),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    } else if ("M12" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    }
    
    p<-p +
      xlim(c(0,data$X[length(data$X)*1]+1000000))
    
    if ("shape_N_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_N_a",]$Estimate,
                                     tmp[tmp$Param=="shape_N_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("Ne distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.75],
                             ymax=data$Y[length(data$Y)*1])
      
    }
    
    
    
    if ("shape_M12_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_M12_a",]$Estimate,
                                     tmp[tmp$Param=="shape_M12_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("M12 distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.4],
                             ymax=data$Y[length(data$Y)*0.65])
      
    }
    
    if ("shape_M21_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_M21_a",]$Estimate,
                                     tmp[tmp$Param=="shape_M21_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("M21 distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.05],
                             ymax=data$Y[length(data$Y)*0.3])
      
    }
    
    img<-readPNG(paste("/home/labosea1/image/",as.character(tmp$SP[1]),".png",sep=""))
    g <- rasterGrob(img, interpolate=TRUE)
    
    p<-p+
      annotation_custom(g,
                        xmin=data$X[length(data$X)*0.05],
                        xmax=data$X[length(data$X)*0.35],
                        ymin=data$Y[length(data$Y)*0.95],
                        ymax=data$Y[length(data$Y)*1])
    pdf(paste("/home/labosea1/ABC/",sp,"_",pop,".pdf",sep=""),width=12.5,height=7.5)
    print(p)
    dev.off()
    
    jpeg(paste("/home/labosea1/ABC/",sp,"_",pop,".jpg",sep=""),width=12.5,height=7.5,units="in",res=1000)
    print(p)
    dev.off()
        
        
  }
  
}




### WHOLE_GENOME -----
plot_plot=0

SPECIES=c("Cgale","Cjuli")
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))

a<-read.csv(file="/home/labosea1/ABC/data_ABC.csv",header=T)
a=a[a$Migr=="Beta",]
a=a[a$Format=="Mu_Fa" | a$Format=="Li_Ga",] 
a=a[a$Method=="Random forest",]

data=data.frame(X=seq(0,max(a[a$Param=="N1" | a$Param=="N2" | a$Param=="Na",]$Estimate)*2 + max(a[a$Param=="N1" | a$Param=="N2" | a$Param=="Na",]$Estimate)/2,length.out=1000),
                Y=seq(0,max(a[a$Param=="Tsplit",]$Estimate)+max(a[a$Param=="Tsplit",]$Estimate)/2,length.out=1000))

for (sp in SPECIES){
  
  for (pop in c("Li_Ga","Mu_Fa")){
    
    tmp<-a[a$SP==sp & a$Format==pop,]
    
    if (sp=="Dlabr" & pop=="Mu_Fa"){
      tmp=tmp[1:8,]
    }
    
    p<-ggplot(data,aes(x=X,y=Y)) + 
      theme_void() 
    
    if (strsplit(as.character(tmp$Format[1]),"_")[[1]][1]=="Li"){
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Gulf of Lion",
                 color=color_med_atl[4],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmin=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate),
          xmax=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[4]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N1",]$Estimate,0),sep=""),
                 color=color_med_atl[4],
                 fontface="bold",
                 size=2.5) 
      
    } else {
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Costa Calida",
                 color=color_med_atl[3],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmin=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate),
          xmax=data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[3]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)-(tmp[tmp$Param=="N1",]$Estimate)) + (data$X[length(data$X)/2] - (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N1",]$Estimate,0),sep=""),
                 color=color_med_atl[3],
                 fontface="bold",
                 size=2.5) 
      
    }
    
    if (strsplit(as.character(tmp$Format[1]),"_")[[1]][2]=="Fa"){
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Algarve",
                 color=color_med_atl[2],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmax=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate),
          xmin=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[2]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N2",]$Estimate,0),sep=""),
                 color=color_med_atl[2],
                 fontface="bold",
                 size=2.5) 
      
    } else {
      
      p<-p+
        annotate(geom="text", 
                 y=-50000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 label="Bay of Biscay",
                 color=color_med_atl[1],
                 fontface="bold",
                 size=2.5) +
        geom_rect(
          xmax=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate),
          xmin=data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45),
          ymin=0,
          ymax=tmp[tmp$Param=="Tsplit",]$Estimate, alpha=0.1,fill=color_med_atl[1]) +
        annotate(geom="text", 
                 y=-150000, 
                 x=((data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)+(tmp[tmp$Param=="N2",]$Estimate)) + (data$X[length(data$X)/2] + (tmp[tmp$Param=="Na",]$Estimate*0.45)))/2, 
                 #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 #label=substitute(paste(N[e],"=",e),
                 #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
                 label=paste("Ne = ",round(tmp[tmp$Param=="N2",]$Estimate,0),sep=""),
                 color=color_med_atl[1],
                 fontface="bold",
                 size=2.5) 
      
    }
    
    p<-p+
      geom_rect(xmin=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2),
                xmax=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2),
                ymin=tmp[tmp$Param=="Tsplit",]$Estimate,
                ymax=tmp[tmp$Param=="Tsplit",]$Estimate*1.5, alpha=0.1,fill="gray") +
      annotate(geom="text", 
               y=tmp[tmp$Param=="Tsplit",]$Estimate*1.5 + 50000, 
               x=(data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2))/2, 
               #label=expression(paste(N[e],"=",round(tmp[tmp$Param=="N1",]$Estimate,0))),
               #label=substitute(paste(N[e],"=",e),
               #                 list(e=round(tmp[tmp$Param=="N1",]$Estimate,0))),
               label=paste("Na = ",round(tmp[tmp$Param=="Na",]$Estimate,0),sep=""),
               color="gray",
               fontface="bold",
               size=2.5) +
      geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                   y=tmp[tmp$Param=="Tsplit",]$Estimate,
                   xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                   yend=tmp[tmp$Param=="Tsplit",]$Estimate,
                   linetype="dashed") +
      annotate(geom="text", 
               y=tmp[tmp$Param=="Tsplit",]$Estimate, 
               x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
               label=paste("Tsplit = ",round(tmp[tmp$Param=="Tsplit",]$Estimate,0),sep=""),
               color="black",
               fontface="bold",
               size=2.5) 
    
    
    p<- p +
      geom_segment(x=0,
                   xend=0,
                   y=0,
                   yend=max(data$Y), color="black",arrow=arrow())
    
    if ("Tsc" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                     y=tmp[tmp$Param=="Tsc",]$Estimate,
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                     yend=tmp[tmp$Param=="Tsc",]$Estimate,
                     linetype="dashed")+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33,
                     yend=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66,
                     yend=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate, 
                 x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
                 label=paste("Tsc = ",round(tmp[tmp$Param=="Tsc",]$Estimate,0),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsc",]$Estimate-tmp[tmp$Param=="Tsc",]$Estimate*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    } else if ("Tam" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.5) - (tmp[tmp$Param=="N1",]$Estimate),
                     y=tmp[tmp$Param=="Tam",]$Estimate,
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.5) + (tmp[tmp$Param=="N2",]$Estimate),
                     yend=tmp[tmp$Param=="Tam",]$Estimate,
                     linetype="dashed")+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tam",]$Estimate, 
                 x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45) + (tmp[tmp$Param=="N2",]$Estimate) + 200000, 
                 label=paste("Tam = ",round(tmp[tmp$Param=="Tam",]$Estimate,0),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-(tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tam",]$Estimate)*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    } else if ("M12" %in% tmp$Param){
      p<-p+
        geom_segment(x=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     xend=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33, arrow=arrow(), size=tmp[tmp$Param=="M12",]$Estimate/10, color="black") + 
        geom_segment(xend=data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate*0.25),
                     x=data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate*0.45),
                     y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66,
                     yend=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66, arrow=arrow(), size=tmp[tmp$Param=="M21",]$Estimate/10, color="black")  +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.33+75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M12 = ",round(tmp[tmp$Param=="M12",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) +
        annotate(geom="text", 
                 y=tmp[tmp$Param=="Tsplit",]$Estimate-tmp[tmp$Param=="Tsplit",]$Estimate*0.66-75000, 
                 x=(data$X[length(data$X)/2]+(tmp[tmp$Param=="Na",]$Estimate/2) - (tmp[tmp$Param=="N2",]$Estimate/2) + data$X[length(data$X)/2]-(tmp[tmp$Param=="Na",]$Estimate/2) + (tmp[tmp$Param=="N1",]$Estimate/2))/2, 
                 label=paste("M21 = ",round(tmp[tmp$Param=="M21",]$Estimate,2),sep=""),
                 color="black",
                 fontface="bold",
                 size=2.5) 
    }
    
    p<-p +
      xlim(c(0,data$X[length(data$X)*1]+1000000))
    
    if ("shape_N_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_N_a",]$Estimate,
                                     tmp[tmp$Param=="shape_N_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("Ne distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.75],
                             ymax=data$Y[length(data$Y)*1])
      
    }
    
    
    
    if ("shape_M12_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_M12_a",]$Estimate,
                                     tmp[tmp$Param=="shape_M12_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("M12 distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.4],
                             ymax=data$Y[length(data$Y)*0.65])
      
    }
    
    if ("shape_M21_a" %in% tmp$Param){
      
      data_distri=data.frame(X=rbeta(10000,tmp[tmp$Param=="shape_M21_a",]$Estimate,
                                     tmp[tmp$Param=="shape_M21_b",]$Estimate))
      
      p_shape_N<-ggplot(data_distri,aes(x=X)) +
        theme_classic()+
        geom_density(fill=viridis::viridis(1),alpha=0.5) +
        scale_y_continuous(expand=c(0,0)) +
        xlab("") +
        ylab("") +
        ggtitle("M21 distribution")
      
      p<-p+annotation_custom(ggplotGrob(p_shape_N),
                             xmin=data$X[length(data$X)*1],
                             xmax=data$X[length(data$X)*1]+1000000,
                             ymin=data$Y[length(data$Y)*0.05],
                             ymax=data$Y[length(data$Y)*0.3])
      
    }
    
    img<-readPNG(paste("/home/labosea1/image/",as.character(tmp$SP[1]),".png",sep=""))
    g <- rasterGrob(img, interpolate=TRUE)
    
    p<-p+
      annotation_custom(g,
                        xmin=data$X[length(data$X)*0.05],
                        xmax=data$X[length(data$X)*0.35],
                        ymin=data$Y[length(data$Y)*0.95],
                        ymax=data$Y[length(data$Y)*1])
    pdf(paste("/home/labosea1/ABC/",sp,"_",pop,".pdf",sep=""),width=12.5,height=7.5)
    print(p)
    dev.off()
    
    
  }
  
}


