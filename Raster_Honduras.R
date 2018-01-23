#Libraries
library(raster)




#***Methods Raster Folders***
#According to final document, there are three types of classification.
#1. PCA_Kmeans
#2. PCA_Mclust
#3. tnse_GNG


PCA_Kmeans ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_kmeans_PCA_RedcVariables_Norm_dim11_NClu13"
PCA_Mclust ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_mclust_PCA_RedcVariables_Norm_dim11_NClu21"
tnse_GNG ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_gng_tsne_RedcVariables_Norm_per270_NClu14"


#***Weather Raster Folders***

VectorFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/SHP_File"
ElevationFile="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Elevacion/"
SoilFiles="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Suelo/"
ClimeFiles="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/Clima/WorldClim"

#***Read Raster Weather***

setwd(SoilFiles)
listFiles=list.files(pattern = "*.tif$")
soilsInfo=stack(listFiles)

setwd(ClimeFiles)
listFiles=list.files(pattern = "*.tif$")
ClimeInfo=stack(listFiles)

setwd(ElevationFile)
listFiles=list.files(pattern = "*.tif$")
ElevationInfo=stack(listFiles[1])

raster_kmeans=raster(paste0(tnse_GNG, ".tif"))

#info_raster works for getting information of raster.
#Arguments.  -name_raster.
#Return.     -table. With all information 

info_raster <- function (name_raster)
{
  raster_info  <- raster(paste0(tnse_GNG, ".tif"))
  extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(rasterkmeans,coordinatesExtract)))
  return (raster_info)
  
}