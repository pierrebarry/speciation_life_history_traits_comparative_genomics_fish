## PCA ----

load(file="Data/PCA_list.Rdata")
data_fake=data.frame(X=seq(0,10,by=1),
                     Y=seq(0,10,by=1))
# All ----
pca=PCA_list$Aboye$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))

color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Aboye.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Aboye<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Atherina boyeri")
p_Aboye<-ggpubr::ggarrange(p_fake,p_Aboye,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Afall$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",4),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",0)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Afall.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Afall<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Alosa fallax")
p_Afall<-ggpubr::ggarrange(p_fake,p_Afall,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Cgale$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Cgale.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Cgale<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Coryphoblennius galerita")
p_Cgale<-ggpubr::ggarrange(p_fake,p_Cgale,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Cjuli$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Cjuli.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Cjuli<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Coris julis")
p_Cjuli<-ggpubr::ggarrange(p_fake,p_Cjuli,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Dlabr$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Dlabr.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Dlabr<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Dicentarchus labrax")
p_Dlabr<-ggpubr::ggarrange(p_fake,p_Dlabr,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Dpunt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",4)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Dpunt.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Dpunt<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Diplodus puntazzo")
p_Dpunt<-ggpubr::ggarrange(p_fake,p_Dpunt,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Gnige$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Gnige.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Gnige<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Gobius niger")
p_Gnige<-ggpubr::ggarrange(p_fake,p_Gnige,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Hgutt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",11),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Hgutt.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Hgutt<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Hippocampus guttulatus")
p_Hgutt<-ggpubr::ggarrange(p_fake,p_Hgutt,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Lbude$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",3)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Lbude.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Lbude<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Lophius budegassa")
p_Lbude<-ggpubr::ggarrange(p_fake,p_Lbude,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Lmorm$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Lmorm.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Lmorm<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Lithognathus mormyrus")
p_Lmorm<-ggpubr::ggarrange(p_fake,p_Lmorm,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Mmerl$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Mmerl.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Mmerl<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Merluccius merluccius")
p_Mmerl<-ggpubr::ggarrange(p_fake,p_Mmerl,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Msurm$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Msurm.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Msurm<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Mullus surmuletus")
p_Msurm<-ggpubr::ggarrange(p_fake,p_Msurm,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Peryt$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Peryt.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Peryt<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Pagellus erythrinus")
p_Peryt<-ggpubr::ggarrange(p_fake,p_Peryt,nrow=2,heights=c(0.1,0.95),legend="bottom")


pca=PCA_list$Scabr$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Scabr.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Scabr<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Serranus cabrilla")
p_Scabr<-ggpubr::ggarrange(p_fake,p_Scabr,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Scant$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Scant.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Scant<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Spondyliosoma cantharus")
p_Scant<-ggpubr::ggarrange(p_fake,p_Scant,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Scine$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Scine.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Scine<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Symphodus cinereus")
p_Scine<-ggpubr::ggarrange(p_fake,p_Scine,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Spilc$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Spilc.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Spilc<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Sardina pilchardus")
p_Spilc<-ggpubr::ggarrange(p_fake,p_Spilc,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Ssard$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Ssard.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Ssard<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Sarda sarda")
p_Ssard<-ggpubr::ggarrange(p_fake,p_Ssard,nrow=2,heights=c(0.1,0.95),legend="bottom")

pca=PCA_list$Styph$Raw$maf0.05[[1]]
data_pca=data.frame(INDIV=pca$sample.id,
                    PC1=pca$eigenvect[,1],
                    PC2=pca$eigenvect[,2],
                    PC3=pca$eigenvect[,3],
                    PC4=pca$eigenvect[,4],
                    LOCA=c(rep("Algarve",5),
                           rep("Bay of Biscay",5),
                           rep("Gulf of Lion",5),
                           rep("Costa Calida",5)))
color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
img<-png::readPNG("data/Styph.png")
g<-grid::rasterGrob(img,interpolate=T)
p_fake<-ggplot(data_fake,aes(x=X,y=Y)) +
  theme_void() +
  annotation_custom(g) +
  ylim(c(25,100)) 
p_Styph<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border=element_rect(fill="transparent"),
        plot.title=element_text(hjust=0.5,face="italic")) +
  geom_hline(yintercept=0,lty=2)+
  geom_vline(xintercept=0,lty=2)+
  geom_point(size=3,aes(col=LOCA))+
  xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
  ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
  scale_colour_manual(name="Location",
                      values=c(color_med_atl[2],
                               color_med_atl[1],
                               color_med_atl[3],
                               color_med_atl[4])) +
  labs(title="Syngnathus typhle")
p_Styph<-ggpubr::ggarrange(p_fake,p_Styph,nrow=2,heights=c(0.1,0.95),legend="bottom")

pdf(paste("/Users/divco/Documents/COGEDIV/figures/ALL_PCA.pdf",sep=""),width=20,height=20)
print(ggpubr::ggarrange(p_Aboye,
                        p_Afall,
                        p_Cgale,
                        p_Cjuli,
                        p_Dlabr,
                        p_Dpunt,
                        p_Gnige,
                        p_Hgutt,
                        p_Lbude,
                        p_Lmorm,
                        p_Mmerl,
                        p_Msurm,
                        p_Peryt,
                        p_Scabr,
                        p_Scant,
                        p_Scine,
                        p_Spilc,
                        p_Ssard,
                        p_Styph,ncol=4,nrow=5)
)
dev.off()

jpeg(paste("/Users/divco/Documents/COGEDIV/figures/ALL_PCA.jpg",sep=""),width=20,height=20,units="in",res=1000)
print(ggpubr::ggarrange(p_Aboye,
                        p_Afall,
                        p_Cgale,
                        p_Cjuli,
                        p_Dlabr,
                        p_Dpunt,
                        p_Gnige,
                        p_Hgutt,
                        p_Lbude,
                        p_Lmorm,
                        p_Mmerl,
                        p_Msurm,
                        p_Peryt,
                        p_Scabr,
                        p_Scant,
                        p_Scine,
                        p_Spilc,
                        p_Ssard,
                        p_Styph,ncol=4,nrow=5)
)
dev.off()
#Aboye
for (i in 1:2){
  for (j in 1:length(PCA_list$Aboye$Raw)){
    pca=PCA_list$Aboye[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Aboye","_pca_",names(PCA_list$Aboye)[i],"_",gsub('\\.', '-', names(PCA_list$Aboye$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Afall
for (i in 1:2){
  for (j in 1:length(PCA_list$Afall$Raw)){
    pca=PCA_list$Afall[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",4),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",0)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Afall","_pca_",names(PCA_list$Afall)[i],"_",gsub('\\.', '-', names(PCA_list$Afall$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Cgale
for (i in 1:2){
  for (j in 1:length(PCA_list$Cgale$Raw)){
    pca=PCA_list$Cgale[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Cgale","_pca_",names(PCA_list$Cgale)[i],"_",gsub('\\.', '-', names(PCA_list$Cgale$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Cjuli
for (i in 1:2){
  for (j in 1:length(PCA_list$Cjuli$Raw)){
    pca=PCA_list$Cjuli[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Cjuli","_pca_",names(PCA_list$Cjuli)[i],"_",gsub('\\.', '-', names(PCA_list$Cjuli$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Dlabr
for (i in 1:2){
  for (j in 1:length(PCA_list$Dlabr$Raw)){
    pca=PCA_list$Dlabr[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Dlabr","_pca_",names(PCA_list$Dlabr)[i],"_",gsub('\\.', '-', names(PCA_list$Dlabr$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Dpunt
for (i in 1:2){
  for (j in 1:length(PCA_list$Dpunt$Raw)){
    pca=PCA_list$Dpunt[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",4)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Dpunt","_pca_",names(PCA_list$Dpunt)[i],"_",gsub('\\.', '-', names(PCA_list$Dpunt$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Gnige
for (i in 1:2){
  for (j in 1:length(PCA_list$Gnige$Raw)){
    pca=PCA_list$Gnige[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Gnige","_pca_",names(PCA_list$Gnige)[i],"_",gsub('\\.', '-', names(PCA_list$Gnige$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Hgutt
for (i in 1:2){
  for (j in 1:length(PCA_list$Hgutt$Raw)){
    pca=PCA_list$Hgutt[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",11),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Hgutt","_pca_",names(PCA_list$Hgutt)[i],"_",gsub('\\.', '-', names(PCA_list$Hgutt$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Lbude
for (i in 1:2){
  for (j in 1:length(PCA_list$Lbude$Raw)){
    pca=PCA_list$Lbude[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",3)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Lbude","_pca_",names(PCA_list$Lbude)[i],"_",gsub('\\.', '-', names(PCA_list$Lbude$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Lmorm
for (i in 1:2){
  for (j in 1:length(PCA_list$Lmorm$Raw)){
    pca=PCA_list$Lmorm[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Lmorm","_pca_",names(PCA_list$Lmorm)[i],"_",gsub('\\.', '-', names(PCA_list$Lmorm$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Mmerl
for (i in 1:2){
  for (j in 1:length(PCA_list$Mmerl$Raw)){
    pca=PCA_list$Mmerl[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Mmerl","_pca_",names(PCA_list$Mmerl)[i],"_",gsub('\\.', '-', names(PCA_list$Mmerl$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Msurm
for (i in 1:2){
  for (j in 1:length(PCA_list$Msurm$Raw)){
    pca=PCA_list$Msurm[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Msurm","_pca_",names(PCA_list$Msurm)[i],"_",gsub('\\.', '-', names(PCA_list$Msurm$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Peryt
for (i in 1:2){
  for (j in 1:length(PCA_list$Peryt$Raw)){
    pca=PCA_list$Peryt[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Peryt","_pca_",names(PCA_list$Peryt)[i],"_",gsub('\\.', '-', names(PCA_list$Peryt$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Scabr
for (i in 1:2){
  for (j in 1:length(PCA_list$Scabr$Raw)){
    pca=PCA_list$Scabr[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Scabr","_pca_",names(PCA_list$Scabr)[i],"_",gsub('\\.', '-', names(PCA_list$Scabr$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Scant
for (i in 1:2){
  for (j in 1:length(PCA_list$Scant$Raw)){
    pca=PCA_list$Scant[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Scant","_pca_",names(PCA_list$Scant)[i],"_",gsub('\\.', '-', names(PCA_list$Scant$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Scine
for (i in 1:2){
  for (j in 1:length(PCA_list$Scine$Raw)){
    pca=PCA_list$Scine[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Scine","_pca_",names(PCA_list$Scine)[i],"_",gsub('\\.', '-', names(PCA_list$Scine$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Spilc
for (i in 1:2){
  for (j in 1:length(PCA_list$Spilc$Raw)){
    pca=PCA_list$Spilc[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Spilc","_pca_",names(PCA_list$Spilc)[i],"_",gsub('\\.', '-', names(PCA_list$Spilc$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Ssard
for (i in 1:2){
  for (j in 1:length(PCA_list$Ssard$Raw)){
    pca=PCA_list$Ssard[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Ssard","_pca_",names(PCA_list$Ssard)[i],"_",gsub('\\.', '-', names(PCA_list$Ssard$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}

#Styph
for (i in 1:2){
  for (j in 1:length(PCA_list$Styph$Raw)){
    pca=PCA_list$Styph[[i]][[j]][[1]]
    data_pca=data.frame(INDIV=pca$sample.id,
                        PC1=pca$eigenvect[,1],
                        PC2=pca$eigenvect[,2],
                        PC3=pca$eigenvect[,3],
                        PC4=pca$eigenvect[,4],
                        LOCA=c(rep("Algarve",5),
                               rep("Bay of Biscay",5),
                               rep("Gulf of Lion",5),
                               rep("Costa Calida",5)))
    
    color_med_atl=rev(brewer.pal(n=4,name="RdBu"))
    pdf(paste("/Users/divco/Documents/COGEDIV/figures/","Styph","_pca_",names(PCA_list$Styph)[i],"_",gsub('\\.', '-', names(PCA_list$Styph$Raw)[j]),".pdf",sep=""),width=7.5,height=5)
    p<-ggplot(data=data_pca,aes(x=PC1,y=PC2,label=INDIV))+
      theme_minimal()+
      theme(panel.grid=element_blank(),
            panel.border=element_rect(fill="transparent"),
            plot.title=element_text(hjust=0.5))+
      geom_hline(yintercept=0,lty=2)+
      geom_vline(xintercept=0,lty=2)+
      geom_point(size=3,aes(col=LOCA))+
      xlab(paste("PCA 1 (",round(pca$varprop[1]*100,2),"% )",sep=""))+
      ylab(paste("PCA 2 (",round(pca$varprop[2]*100,2),"% )",sep=""))+  
      scale_colour_manual(name="Location",
                          values=c(color_med_atl[2],
                                   color_med_atl[1],
                                   color_med_atl[3],
                                   color_med_atl[4]))
    print(p)
    dev.off()
    
  }
}



