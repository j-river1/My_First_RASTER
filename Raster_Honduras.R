#Libraries
library(raster)
library(dplyr)

#Folders 
mainDir <- getwd()
dir.create(file.path(mainDir, "RData"), showWarnings = FALSE)

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

setwd(mainDir)

#***Read coordinatesExtract Weather***

rasterRef=ClimeInfo$bio_10
levelsWNA=which(!is.na(rasterRef[]))
coordinatesExtract=xyFromCell(rasterRef,levelsWNA)

#***Read coordinatesExtract Soil***
crsSystem=ClimeInfo@crs
#soilsInfo_Projected=projectRaster(soilsInfo, crs = crsSystem)
#save(soilsInfo_Projected,file="./RData/soilsInfo_Projected.RData")
load(file="./RData/soilsInfo_Projected.RData")




#info_raster works for getting information of raster.
#Arguments.  -name_raster.
#Return.     -table. With all information 
# menu = 1. Weather information
# menu = 2. Soil Information 


info_raster <- function (name_raster, menu)
{
  raster_info  <- raster(paste0(name_raster, ".tif"))
  extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
      
  #Put same coordinate system. Soil and weather
  load(file="./RData/soilsInfo_Projected.RData")
  
  if(name_raster == PCA_Kmeans)
  { 
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_pcaKmeans.RData")
    
    load(file="./RData/extraction_clim_pcaKmeans.RData")
    
    #**Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil.RData")
    load(file="./RData/extraction_soil.RData")
  }
  
  if(name_raster == PCA_Mclust)
  {
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_pca_Mclust.RData")
    load(file="./RData/extraction_clim_pca_Mclust.RData")
    
    #**Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil_PCAMCLUST.RData")
    
    load(file="./RData/extraction_soil_PCAMCLUST.RData")
    
   }
  
  if(name_raster == tnse_GNG)
  {
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_tnse_GNG.RData")
    load(file="./RData/extraction_clim_tnse_GNG.RData")
    
    
    
    #Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil_tnse_GNG.RData")
    
    load(file="./RData/extraction_soil_PCAMCLUST.RData")
    
  }
  
  
  if(menu == 1)
  {
      
  #Data Mean Anual 
  data_Tem_Mean_An <- data.frame(extraction%>%group_by(V68)%>%summarise(minTemMeanAnual=min(bio_1,na.rm=T),maxTemMeanAnual=max(bio_1,na.rm=T)))
  
    
  #Data Temperature Monthly Range
  data_Tem_Range_Mon <- data.frame(extraction%>%group_by(V68)%>%summarise(Mean_Monthly_Range = mean(bio_2)))
  data_Tem_Range_Mon$V68 <- NULL
    
  #Data Range Temperature Anual  Range
  data_Tem_Range_An_Range <- data.frame(extraction%>%group_by(V68)%>%summarise(Mean_Anual_Range = mean(bio_7)))
  data_Tem_Range_An_Range$V68 <- NULL
  
  
  #Data Precipitaion Anual
  data_Precip <- data.frame(extraction%>%group_by(V68)%>%summarise(Mean_Precipitation_Anual = mean(bio_12)))
  data_Precip$V68 <- NULL
  
  
  #Data Mean January
  data_tmean_jan <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_January = mean(tmean_1)))
  data_tmean_jan$V68 <- NULL
  
  
  
  #Data Mean February
  data_tmean_feb <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_February = mean(tmean_2)))
  data_tmean_feb$V68 <- NULL
  
  
  #Data Mean March 
  data_tmean_marc <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_March = mean(tmean_3)))
  data_tmean_marc$V68 <- NULL
  
  
  #Data Mean April 
  data_tmean_apri <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_April = mean(tmean_4)))
  data_tmean_apri$V68 <- NULL
  
  
  #Data Mean May 
  data_tmean_may <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_May = mean(tmean_5)))
  data_tmean_may$V68 <- NULL
  
  
  #Data Mean June 
  data_tmean_jun <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_June = mean(tmean_6)))
  data_tmean_jun$V68 <- NULL
  

  #Data Mean July 
  data_tmean_jul <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_July = mean(tmean_7)))
  data_tmean_jul$V68 <- NULL
  

  #Data Mean Agust 
  data_tmean_agus <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Agust = mean(tmean_8)))
  data_tmean_agus$V68 <- NULL
  
  
  #Data Mean Sept 
  data_tmean_sept <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Sept = mean(tmean_9)))
  data_tmean_sept$V68 <- NULL
  

  #Data Mean Oct 
  data_tmean_oct <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Oct = mean(tmean_10)))
  data_tmean_oct$V68 <- NULL
  
  
  #Data Mean Nov 
  data_tmean_Nov <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Nov = mean(tmean_11)))
  data_tmean_Nov$V68 <- NULL
  
  
  #Data Mean Dic 
  data_tmean_Dic <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Dic = mean(tmean_12)))
  data_tmean_Dic$V68 <- NULL
  
  result <- list (data_Tem_Mean_An , data_Tem_Range_Mon, data_Tem_Range_An_Range,data_Precip, data_tmean_jan, data_tmean_feb, data_tmean_marc,  data_tmean_apri, data_tmean_may , data_tmean_jun, 
                  data_tmean_jul, data_tmean_agus, data_tmean_sept, data_tmean_oct, data_tmean_Nov, data_tmean_Dic)
  
  }
  
  if(menu == 2)
  {
    #Soil
    #clay
    data_clay <- data.frame(extraction_soil%>%group_by(V5)%>%summarise(Mean_clay = mean(clay)))
    
    
    
    #sand 
    data_sand <- data.frame(extraction_soil%>%group_by(V5)%>%summarise(Mean_sand = mean(sand)))
    data_sand$V5 <- NULL
    
    #silt 
    data_silt <- data.frame(extraction_soil%>%group_by(V5)%>%summarise(Mean_slit = mean(silt)))
    data_silt$V5 <- NULL
    
    #watercapi 
    data_watercapi <- data.frame(extraction_soil%>%group_by(V5)%>%summarise(Mean_water_capi = mean(water_holding_capacity)))
    data_watercapi$V5 <- NULL
    
    result <- list (data_clay, data_sand, data_silt, data_watercapi )
    
  }
  
  #list   
  final_result <- do.call("cbind", result)
  final_result<- final_result [complete.cases(final_result), ]
  
  return (final_result)
  
}


#graphics_ombrothermic plots ombrothermic diagram
#Arguments. -Data Frame. It is composed by months and values.
#Return Ombrothermic diagram

graphics_ombrothermic <- function (months, values_temp, values_preci)
{
  #Data
  data <- data.frame(Months = months, Values_Preci = values_preci, Values_Temp = values_temp)
  
  #Graph 
  par(mar = c(5,5,2,5))
  with(data, plot(Months, Values_Preci, type="l", col="red3", ylab= "Mililitros", ylim=c(0,3)))
  
  
  
}


