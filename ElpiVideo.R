library(jpeg)
library(ElPiGraph.R)
library(stringr)
library(rgl)
library(foreach)
library(irlba)

unzip("Metro.zip",exdir="frames")
system("ffmpeg -y -f image2 -i frames/%*.jpg -vf setpts=2*PTS initial.mp4")
system("for f in frames/*.jpg; do mv -n $f frames/$RANDOM.jpg; done;")
system("ffmpeg -y -f image2 -i frames/%*.jpg -vf setpts=2*PTS rand.mp4")


images = list.files(path = "frames",pattern=".jpg")

heigth = 640
width = 360
expr = matrix(0,ncol = heigth*width,nrow=length(images))
for (i in 1:length(images)){
  data <- as.vector(readJPEG(paste0("frames/",images[i])))
  expr[i,] = data
}

print("Perfoming PCA")
pca = prcomp_irlba(expr) 
X = pca$x

# Plot Points with rgl
plot3d(x=X[,1],y=X[,2],z=X[,3],surface=FALSE,xlab = "PC1", ylab = "PC2",zlab = "PC3",col="blue",size=7)
Sys.sleep(1)
# Position rgl plot
par3d("windowRect"= c(0,0,600,600))
rgl.viewpoint(235,35,10)
Sys.sleep(1)
# Rotate rgl plot
p=1
for (i in rev(seq(30,230,5)+5)){
  rgl.viewpoint(i,35,10)
  filename = paste0("rotation",str_pad(p, 2, pad = "0"))
  rgl.postscript(paste0(filename,".eps"),"eps")
  system(paste0("convert -density 100 ",filename,".eps -flatten ",filename,".png"))
  system(paste0("rm ",filename,".eps"))
  p=p+1
}
print("Making rotation GIF")
system("convert -delay 20 -loop 1 rotation*.png rotation.gif")
system("rm rotation*.png")

# Elpigraph with default params
EPCa = vector("list",50-2)
for (i in 3:50){
  EPCa[[i-2]]=computeElasticPrincipalCurve(X=as.matrix(X),
                                           NumNodes = i,
                                           Do_PCA = FALSE,
                                           drawAccuracyComplexity = F,
                                           drawEnergy = F,drawPCAView=F)
  
}

EPCa[[i-2]][[1]] = ExtendLeaves(X = as.matrix(X), TargetPG = EPCa[[i-2]][[1]], 
                                Mode = "QuantCentroid", ControlPar = .4,PlotSelected = F)

# Elpigraph with Trimming Radius
EPCb = vector("list",50-2)
for (i in 3:50){
  EPCb[[i-2]]=computeElasticPrincipalCurve(X=as.matrix(X),
                                           NumNodes = i,
                                           Do_PCA = FALSE,
                                           drawAccuracyComplexity = F,
                                           drawEnergy = F,drawPCAView=F,
                                           TrimmingRadius = 25)
  
}

EPCb[[i-2]][[1]] = ExtendLeaves(X = as.matrix(X), TargetPG = EPCb[[i-2]][[1]], 
                                Mode = "QuantCentroid", ControlPar = .4,PlotSelected = F)


# Add elpigraph default results
for (i in 1:length(EPCa)){
  segments3d(x=as.vector(t(cbind(EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,1],1],
                                 EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,2],1]))),
             y=as.vector(t(cbind(EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,1],2],
                                 EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,2],2]))),
             z=as.vector(t(cbind(EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,1],3],
                                 EPCa[[i]][[1]]$NodePositions[EPCa[[i]][[1]]$Edges$Edges[,2],3]))), 
             color = "red", add = TRUE,lwd=5)
  plot3d(x=EPCa[[i]][[1]]$NodePositions[,1],
         y=EPCa[[i]][[1]]$NodePositions[,2],
         z=EPCa[[i]][[1]]$NodePositions[,3],
         surface=FALSE,xlab = "PC1", ylab = "PC2",zlab = "PC3",col="black",size = 10,add=TRUE)
  
  plot3d(x=EPCa[[i]][[1]]$NodePositions[i+2,1],
         y=EPCa[[i]][[1]]$NodePositions[i+2,2],
         z=EPCa[[i]][[1]]$NodePositions[i+2,3],
         surface=FALSE,xlab = "PC1", ylab = "PC2",zlab = "PC3",col="black",size = 20,add=TRUE)
  
  filename = paste0("elpi_default",str_pad(i, 2, pad = "0"))
  rgl.postscript(paste0(filename,".eps"),"eps") 
  system(paste0("convert -density 100 ",filename,".eps -flatten ",filename,".png"))
  system(paste0("rm ",filename,".eps"))
  
  if (i != length(EPCa)){
    rgl.pop( type = "shapes" )
    rgl.pop( type = "shapes" )
  }
  rgl.pop( type = "shapes" )
}

filename = paste0("elpi_default",str_pad(i+1, 2, pad = "0"))
rgl.postscript(paste0(filename,".eps"),"eps") 
system(paste0("convert -density 100 ",filename,".eps -flatten ",filename,".png"))
system(paste0("rm ",filename,".eps"))


print("Making elpi_default GIF")
system("convert -delay 20 -loop 1 elpi_default*.png elpi_default.gif")
system("rm elpi_default*.png")
# Remove previous elpigraph results
rgl.pop( type = "shapes" )
rgl.pop( type = "shapes" )


# Add elpigraph+trimming radius results
for (i in 1:length(EPCb)){
  segments3d(x=as.vector(t(cbind(EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,1],1],
                                 EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,2],1]))),
             y=as.vector(t(cbind(EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,1],2],
                                 EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,2],2]))),
             z=as.vector(t(cbind(EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,1],3],
                                 EPCb[[i]][[1]]$NodePositions[EPCb[[i]][[1]]$Edges$Edges[,2],3]))), 
             color = "red", add = TRUE,lwd=5)
  
  plot3d(x=EPCb[[i]][[1]]$NodePositions[,1],
         y=EPCb[[i]][[1]]$NodePositions[,2],
         z=EPCb[[i]][[1]]$NodePositions[,3],
         surface=FALSE,xlab = "PC1", ylab = "PC2",zlab = "PC3",col="black",size = 10,add=TRUE)
  
  plot3d(x=EPCb[[i]][[1]]$NodePositions[i+2,1],
         y=EPCb[[i]][[1]]$NodePositions[i+2,2],
         z=EPCb[[i]][[1]]$NodePositions[i+2,3],
         surface=FALSE,xlab = "PC1", ylab = "PC2",zlab = "PC3",col="black",size = 20,add=TRUE)
  
  filename = paste0("elpi_trim",str_pad(i, 2, pad = "0"))
  rgl.postscript(paste0(filename,".eps"),"eps") 
  system(paste0("convert -density 100 ",filename,".eps -flatten ",filename,".png"))
  system(paste0("rm ",filename,".eps"))
  
  if (i != length(EPCa)){
    rgl.pop( type = "shapes" )
    rgl.pop( type = "shapes" )
  }
  rgl.pop( type = "shapes" )
}
filename = paste0("elpi_trim",str_pad(i+1, 2, pad = "0"))
rgl.postscript(paste0(filename,".eps"),"eps") 
system(paste0("convert -density 100 ",filename,".eps -flatten ",filename,".png"))
system(paste0("rm ",filename,".eps"))

print("Making elpi_trim GIF")
system("convert -delay 20 -loop 1 elpi_trim*.png elpi_trim.gif")
system("rm elpi_trim*.png")


# Following basic steps for pseudotime inference from Elpigraph documentation
ExtStruct <- EPCb[[length(EPCb)]][[1]]
Graph <- ConstructGraph(ExtStruct)
e2e = GetSubGraph(Net = Graph, Structure = 'end2end')
Root = as.numeric(e2e$Path_1)[1]
SelPaths <- e2e[sapply(e2e, function(x){any(x[c(1, length(x))] == Root)})]
PartStruct <- PartitionData(X = as.matrix(X), NodePositions = ExtStruct$NodePositions)
ProjStruct <- project_point_onto_graph(X = as.matrix(X),
                                       NodePositions = ExtStruct$NodePositions,
                                       Edges = ExtStruct$Edges$Edges,
                                       Partition = PartStruct$Partition)

AllPt <- lapply(SelPaths, function(x){
  getPseudotime(ProjStruct = ProjStruct, NodeSeq = names(x))
})
PointsPT <- apply(sapply(AllPt, "[[", "Pt"), 1, function(x){unique(x[!is.na(x)])})

neworder = sort(PointsPT,index.return=T)$ix
p=1
for (i in neworder){
  file.rename(paste0("frames/",images[i]),
              paste0("frames/",sprintf("%03d.jpg",p)))
  p=p+1
}
system("ffmpeg -y -f image2 -i frames/%*.jpg -vf setpts=2*PTS final.mp4")
unlink("frames",recursive = T)
