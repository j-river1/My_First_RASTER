#Libraries
library(raster)




#Read Raster
#According to final document, there are three types of classification.
#1. PCA_Kmeans
#2. PCA_Mclust
#3. tnse_GNG


PCA_Kmeans ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_kmeans_PCA_RedcVariables_Norm_dim11_NClu13"
PCA_Mclust ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_mclust_PCA_RedcVariables_Norm_dim11_NClu21"
tnse_GNG ="//dapadfs/workspace_cluster_8/AEPS/HONDURAS/ClasificacionesFinales/Raster_gng_tsne_RedcVariables_Norm_per270_NClu14"


raster_kmeans=raster(tnse_GNG)
