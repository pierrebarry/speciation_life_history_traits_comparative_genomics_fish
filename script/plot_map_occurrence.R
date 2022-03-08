library(ggOceanMaps)
library(ggpubr)
setwd("C:/Users/divco/Documents/COGEDIV/")

myplots <- vector('list', length(grep("_occurence.csv",list.files("data/"))))

#a=0

img_map=c("Afall",
          "Aboye",
          "Cjuli",
          "Cgale",
          "Dlabr",
          "Dpunt",
          "Eencr",
          "Gnige",
          "Hgutt",
          "Lmorm",
          "Lbude",
          'Mmerl',
          "Msurm",
          "Peryt",
          "Ssard",
          "Spilc",
          "Scabr",
          "Scant",
          "Scine",
          "Styph")

title_map=c("Alosa fallax",
            "Atherina boyeri",
            "Coris julis",
            "Coryphoblennius galerita",
            "Dicentrarchus labrax",
            "Diplodus puntazzo",
            "Engraulis encrasicolis",
            "Gobius niger",
            "Hippocampus guttulatus",
            "Lithognathus mormyrus",
            "Lophius budegassa",
            'Merluccius merluccius',
            "Mullus surmuletus",
            "Pagellus erythrinus",
            "Sarda sarda",
            "Sardina pilchardus",
            "Serranus cabrilla",
            "Spondyliosoma cantharus",
            "Symphodus cinereus",
            "Syngnathus typhle")

dt <- data.frame(sp=c(NA),
                 lon = c(NA), 
                 lat = c(NA))

for (i in grep("_occurence.csv",list.files("data/"))){
  
  #img<-readPNG(paste("../Documents/COGEDIV/data/",img_map[i],".png",sep=""))
  #g <- rasterGrob(img, interpolate=TRUE)
  
  #a=a+1
  mullus<-read.csv(paste("data/",list.files("data/")[i],sep=""),header=T,sep=",")
  dt <- rbind(dt,data.frame(sp=mullus$Species,lon = mullus$Center.Long, lat = mullus$Center.Lat))
  
  
}

dt=dt[-1,]

dt[,2]=as.numeric(dt[,2])
dt[,3]=as.numeric(dt[,3])

basemap(data = dt,bathymetry = TRUE, grid.col=F,
        limits = c(-30, 45, -40, 80)
) + 
  #geom_polygon(data = transform_coord(dt), aes(x = lon, y = lat), color = "red", fill = NA) +
  geom_spatial_point(data = dt, aes(x = lon, y = lat,col=sp), 
                     fill = "red",
                     #col="red",
                     shape="square") +
  annotation_scale(location = "br") + 
  annotation_north_arrow(location = "tr", which_north = "true") +
  scale_color_viridis_d()
#annotation_custom(g, 
#                  xmin=-25, 
#                  xmax=5, 
#                  ymin=-40,
#                  ymax=0) +
#facet_wrap(.~sp,nrow=5,ncol=4)

# Set up list of images
img.list <- list()
for(i in 1:20){
  
  img<-readPNG(paste("../Documents/COGEDIV/data/",img_map[i],".png",sep=""))
  g <- rasterGrob(img, interpolate=TRUE)
  img.list[[i]] <- g
} 

a <- current.vpTree()
b <- names(a$children$layout$children)
panel.vp <- b[stringr::str_detect(b, "panel-")]
vp.img <- grid::viewport(x=unit(0.1,"npc"), y=unit(0.8,"npc"), width=unit(0.2, "npc"), just = "left")
for(i in 1:N){
  # checkout viewport for panel i
  grid::seekViewport(panel.vp[i])
  # draw my image
  grid::grid.draw(grid::grobTree(grid::rasterGrob(img.list[[i]]), vp=vp.img))
}



pdf(paste("/Users/divco/Documents/COGEDIV/figures/map.pdf",sep=""),width=15*5,height=15*5)
plot(ggarrange(
  myplots[[1]],
  myplots[[2]],
  myplots[[3]],
  myplots[[4]],
  myplots[[5]],
  myplots[[6]],
  myplots[[7]],
  myplots[[8]],
  myplots[[9]],
  myplots[[10]],
  myplots[[11]],
  myplots[[12]],
  myplots[[13]],
  myplots[[14]],
  myplots[[15]],
  myplots[[16]],
  myplots[[17]],
  myplots[[18]],
  myplots[[19]],
  myplots[[20]],
  common.legend=T
))
#plot(ggarrange(myplots[[1]],
##               myplots[[2]],
#               nrow=2))
dev.off()

