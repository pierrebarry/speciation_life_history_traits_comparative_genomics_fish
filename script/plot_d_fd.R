library(ggplot2)
library(RColorBrewer)
library(png)
library(grid)
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))

data_null=data.frame(X=seq(0,60,length.out=100),
                     Y=seq(0,50,length.out=100))

data_segment=data.frame(x1=c(10,30,20),
                        x2=c(30,50,30),
                        y1=c(10,40,25),
                        y2=c(40,10,10)
                             )

data_arrow=data.frame(x1=c(45,30),
                      x2=c(30,45),
                      y1=c(15,15),
                      y2=c(15,15))

p_base<-ggplot(data_null,aes(x=X,y=Y)) +
  theme_void() +
  geom_segment(data=data_segment,aes(x=x1,
               y=y1,
               xend=x2,
               yend=y2)) +
  geom_segment(data=data_arrow,
               aes(x=x1,
                   y=y1,
                   xend=x2,
                   yend=y2),
               arrow=arrow(length=unit(0.5,"cm"))) +
  xlim(c(7.5,52.5)) +
  ylim(c(5,40))

p_base_noarrow<-ggplot(data_null,aes(x=X,y=Y)) +
  theme_void() +
  geom_segment(data=data_segment,aes(x=x1,
                                     y=y1,
                                     xend=x2,
                                     yend=y2)) +
  xlim(c(7.5,52.5)) +
  ylim(c(5,40))

###

SPECIES=list.dirs("/home/labosea1/Introgression/DSUITE/",full.names=F,recursive = F)

myplots<-vector('list',10)

a=0
for (sp in SPECIES){
  table=read.table(paste("/home/labosea1/Introgression/DSUITE/",sp,"/Dsuite_",sp,"_BBAA.txt",sep=""),header=T)
  
  correspondance_population=data.frame(POP=c("Gulf of Lion",
                                             "Costa Calida",
                                             "Algarve",
                                             "Bay of Biscay"),
                                       SIGLE=c("Li",
                                               "Mu",
                                               "Fa",
                                               "Ga"),
                                       col=color_med_atl[c(4,3,2,1)])
  
  myplots=vector('list',4)
  for (i in 1:nrow(table)){
    
    if (as.numeric(table$p.value[i])<0.05){
      
      if (as.numeric(table$p.value[i])<1e-4){
        table$p.value[i]="< 0.0001"
      } else {
        table$p.value[i]=paste("= ",round(as.numeric(table$p.value[i]),4),sep="")
      }
      
      myplots[[i]] <- p_base +
        annotate(geom="text",x=10,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P1[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P1[i]))],fontface="bold") +
        annotate(geom="text",x=30,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P2[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P2[i]))],fontface="bold") +
        annotate(geom="text",x=50,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P3[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P3[i]))],fontface="bold") +
        labs(title=paste("D = ",round(table$Dstatistic[i],4)," (Z = ",round(table$Z.score[i],3),", p-value ",table$p.value[i],")",sep=""),
             fontface="italic") +
        theme(plot.title = element_text(hjust=0.5,face="italic"))
      
      
    } else {
      
      if (as.numeric(table$p.value[i])<1e-4){
        table$p.value[i]="< 0.0001"
      } else {
        table$p.value[i]=paste("= ",round(as.numeric(table$p.value[i]),4),sep="")
      }
      
      myplots[[i]] <- p_base_noarrow +
        annotate(geom="text",x=10,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P1[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P1[i]))],fontface="bold") +
        annotate(geom="text",x=30,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P2[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P2[i]))],fontface="bold") +
        annotate(geom="text",x=50,y=7.5,label=correspondance_population$POP[which(as.character(correspondance_population$SIGLE)==as.character(table$P3[i]))],
                 col=correspondance_population$col[which(as.character(correspondance_population$SIGLE)==as.character(table$P3[i]))],fontface="bold") +
        labs(title=paste("D = ",round(table$Dstatistic[i],4)," (Z = ",round(table$Z.score[i],3),", p-value ",table$p.value[i],")",sep=""),
             fontface="italic") +
        theme(plot.title = element_text(hjust=0.5,face="italic"))
      
      
    }
  }
  
  img <- readPNG(paste("/home/labosea1/image/",sp,".png",sep=""))
  g <- rasterGrob(img,interpolate=T)  
  
  p_image <- ggplot(data_null,aes(x=X,y=Y)) +
    theme_void() +
    annotation_custom(g,xmin=10,xmax = 20 ,ymin = 10 ,ymax=15) +
    xlim(c(10,20)) +
    ylim(c(10,15))
  
  
  pdf(paste("/home/labosea1/Introgression/DSUITE/D_fd_",sp,".pdf",sep=""),width=12.5,height=10)
  print(ggpubr::ggarrange(p_image,
                          ggpubr::ggarrange(myplots[[3]],
                                            myplots[[4]],
                                            myplots[[1]],
                                            myplots[[2]],
                                            nrow=2,
                                            ncol = 2),
                          nrow=2,
                          heights = c(0.1,0.9)) )
  dev.off()
  
  jpeg(paste("/home/labosea1/Introgression/DSUITE/D_fd_",sp,".jpg",sep=""),width=12.5,height=10,units="in",res=1000)
  print(ggpubr::ggarrange(p_image,
                          ggpubr::ggarrange(myplots[[3]],
                                            myplots[[4]],
                                            myplots[[1]],
                                            myplots[[2]],
                                            nrow=2,
                                            ncol = 2),
                          nrow=2,
                          heights = c(0.1,0.9)) )
  dev.off()
  
  a=a+1
  myplots[[a]]<-local({
    print(ggpubr::ggarrange(p_image,
                            ggpubr::ggarrange(myplots[[3]],
                                              myplots[[4]],
                                              myplots[[1]],
                                              myplots[[2]],
                                              nrow=2,
                                              ncol = 2),
                            nrow=2,
                            heights = c(0.1,0.9)) )
  
  })
  
}

pdf(paste("/home/labosea1/Introgression/DSUITE/D_fd_all.pdf",sep=""),width=50,height=50)
print(ggpubr::ggarrange(myplots[[1]],
                  myplots[[2]],
                  myplots[[3]],
                  myplots[[4]],
                  myplots[[5]],
                  myplots[[6]],
                  myplots[[7]],
                  myplots[[8]],
                  myplots[[9]],
                  myplots[[10]])
)
dev.off()
