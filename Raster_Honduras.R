#Libraries
library(raster)
library(dplyr)
library(ggplot2)

#Folders 
mainDir <- getwd()
dir.create(file.path(mainDir, "RData"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Ombrothermic_Plots"), showWarnings = FALSE) 
dir.create(file.path(mainDir, "Excel_Files"), showWarnings = FALSE) 
dir.create(file.path(mainDir, "Histograms_Temperature_Max_Min"), showWarnings = FALSE) 

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
ElevationInfo=stack(listFiles[2])

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
  #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
      

  
  if(name_raster == PCA_Kmeans)
  { 
    namefile <-  "PCA_Kmeans"
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_pcaKmeans.RData")
    
    load(file="./RData/extraction_clim_pcaKmeans.RData")
    
    #**Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil.RData")
    load(file="./RData/extraction_soil.RData")
    
    #**Extract Elevation
    #extraction_ele <- data.frame(cbind(extract(ElevationInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction_ele ,file="./RData/extraction_ele_PCAKmeans.RData")
    load(file="./RData/extraction_ele_PCAKmeans.RData")
  }
  
  if(name_raster == PCA_Mclust)
  {
    namefile <-  "PCA_Mclust"
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_pca_Mclust.RData")
    load(file="./RData/extraction_clim_pca_Mclust.RData")
    
    #**Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil_PCAMCLUST.RData")
    load(file="./RData/extraction_soil_PCAMCLUST.RData")
    
    #**Extract Elevation
    #extraction_ele <- data.frame(cbind(extract(ElevationInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction_ele ,file="./RData/extraction_ele_PCAMCLUST.RData")
    load(file="./RData/extraction_ele_PCAMCLUST.RData")
    
   }
  
  if(name_raster == tnse_GNG)
  {
    namefile <-  "tnse_GNG"
    #**Extract the Climate Info
    #extraction <- data.frame(cbind(extract(ClimeInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction,file="./RData/extraction_clim_tnse_GNG.RData")
    load(file="./RData/extraction_clim_tnse_GNG.RData")
    
    
    
    #Extract the Soil Info
    #extraction_soil <- data.frame(cbind(extract(soilsInfo_Projected,coordinatesExtract),extract(raster_info,coordinatesExtract))) 
    #save(extraction_soil ,file="./RData/extraction_soil_tnse_GNG.RData")
    load(file="./RData/extraction_soil_PCAMCLUST.RData")
    
        
    #**Extract Elevation
    #extraction_ele <- data.frame(cbind(extract(ElevationInfo,coordinatesExtract),extract(raster_info,coordinatesExtract)))
    #save(extraction_ele ,file="./RData/extraction_ele_tnse_GNG.RData")
    load(file="./RData/extraction_ele_tnse_GNG.RData")
  }
  
  
  if(menu == 1)
  {
  
  name_var<- "Weather"
  
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
  
  
  #Data TMean January
  data_tmean_jan <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_January = mean(tmean_1)))
  data_tmean_jan$V68 <- NULL
  
  
  
  #Data TMean February
  data_tmean_feb <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_February = mean(tmean_2)))
  data_tmean_feb$V68 <- NULL
  
  
  #Data TMean March 
  data_tmean_marc <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_March = mean(tmean_3)))
  data_tmean_marc$V68 <- NULL
  
  
  #Data TMean April 
  data_tmean_apri <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_April = mean(tmean_4)))
  data_tmean_apri$V68 <- NULL
  
  
  #Data TMean May 
  data_tmean_may <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_May = mean(tmean_5)))
  data_tmean_may$V68 <- NULL
  
  
  #Data TMean June 
  data_tmean_jun <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_June = mean(tmean_6)))
  data_tmean_jun$V68 <- NULL
  

  #Data TMean July 
  data_tmean_jul <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_July = mean(tmean_7)))
  data_tmean_jul$V68 <- NULL
  

  #Data TMean Agust 
  data_tmean_agus <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Agust = mean(tmean_8)))
  data_tmean_agus$V68 <- NULL
  
  
  #Data TMean Sept 
  data_tmean_sept <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Sept = mean(tmean_9)))
  data_tmean_sept$V68 <- NULL
  

  #Data TMean Oct 
  data_tmean_oct <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Oct = mean(tmean_10)))
  data_tmean_oct$V68 <- NULL
  
  
  #Data TMean Nov 
  data_tmean_Nov <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Nov = mean(tmean_11)))
  data_tmean_Nov$V68 <- NULL
  
  
  #Data TMean Dic 
  data_tmean_Dic <- data.frame(extraction%>%group_by(V68)%>%summarise(TMean_Dic = mean(tmean_12)))
  data_tmean_Dic$V68 <- NULL
  
  #Data PMean January
  data_pmean_jan <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_January = mean(prec_1)))
  data_pmean_jan$V68 <- NULL
  
  
  
  #Data PMean February
  data_pmean_feb <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_February = mean(prec_2)))
  data_pmean_feb$V68 <- NULL
  
  
  #Data PMean March 
  data_pmean_marc <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_March = mean(prec_3)))
  data_pmean_marc$V68 <- NULL
  
  
  #Data Mean April 
  data_pmean_apri <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_April = mean(prec_4)))
  data_pmean_apri$V68 <- NULL
  
  
  #Data Mean May 
  data_pmean_may <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_May = mean(prec_5)))
  data_pmean_may$V68 <- NULL
  
  
  #Data Mean June 
  data_pmean_jun <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_June = mean(prec_6)))
  data_pmean_jun$V68 <- NULL
  
  
  #Data Mean July 
  data_pmean_jul <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_July = mean(prec_7)))
  data_pmean_jul$V68 <- NULL
  
  
  #Data Mean Agust 
  data_pmean_agus <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_Agust = mean(prec_8)))
  data_pmean_agus$V68 <- NULL
  
  
  #Data Mean Sept 
  data_pmean_sept <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_Sept = mean(prec_9)))
  data_pmean_sept$V68 <- NULL
  
  
  #Data Mean Oct 
  data_pmean_oct <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_Oct = mean(prec_10)))
  data_pmean_oct$V68 <- NULL
  
  
  #Data Mean Nov 
  data_pmean_Nov <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_Nov = mean(prec_11)))
  data_pmean_Nov$V68 <- NULL
  
  
  #Data Mean Dic 
  data_pmean_Dic <- data.frame(extraction%>%group_by(V68)%>%summarise(PMean_Dic = mean(prec_12)))
  data_pmean_Dic$V68 <- NULL
   
  
  #Data TMin January
  data_tmin_jan <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_January = mean(tmin_1)))
  data_tmin_jan$V68 <- NULL
  
  
  
  #Data TMin February
  data_tmin_feb <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_February = mean(tmin_2)))
  data_tmin_feb$V68 <- NULL
  
  
  #Data TMin March 
  data_tmin_marc <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_March = mean(tmin_3)))
  data_tmin_marc$V68 <- NULL
  
  
  #Data TMin April 
  data_tmin_apri <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_April = mean(tmin_4)))
  data_tmin_apri$V68 <- NULL
  
  
  #Data TMin May 
  data_tmin_may <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_May = mean(tmin_5)))
  data_tmin_may$V68 <- NULL
  
  
  #Data TMin June 
  data_tmin_jun <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_June = mean(tmin_6)))
  data_tmin_jun$V68 <- NULL
  
  
  #Data TMin July 
  data_tmin_jul <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_July = mean(tmin_7)))
  data_tmin_jul$V68 <- NULL
  
  
  #Data TMin Agust 
  data_tmin_agus <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_Agust = mean(tmin_8)))
  data_tmin_agus$V68 <- NULL
  
  
  #Data TMin Sept 
  data_tmin_sept <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_Sept = mean(tmin_9)))
  data_tmin_sept$V68 <- NULL
  
  
  #Data TMin Oct 
  data_tmin_oct <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_Oct = mean(tmin_10)))
  data_tmin_oct$V68 <- NULL
  
  
  #Data TMin Nov 
  data_tmin_Nov <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_Nov = mean(tmin_11)))
  data_tmin_Nov$V68 <- NULL
  
  
  #Data TMin Dic 
  data_tmin_Dic <- data.frame(extraction%>%group_by(V68)%>%summarise(TMin_Dic = mean(tmin_12)))
  data_tmin_Dic$V68 <- NULL
  
  
  #Aca comienza
  
  
  #Data TMax January
  data_tmax_jan <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_January = mean(tmax_1)))
  data_tmax_jan$V68 <- NULL
  
  
  
  #Data TMax February
  data_tmax_feb <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_February = mean(tmax_2)))
  data_tmax_feb$V68 <- NULL
  
  
  #Data TMax March 
  data_tmax_marc <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_March = mean(tmax_3)))
  data_tmax_marc$V68 <- NULL
  
  
  #Data TMax April 
  data_tmax_apri <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_April = mean(tmax_4)))
  data_tmax_apri$V68 <- NULL
  
  
  #Data TMax May 
  data_tmax_may <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_May = mean(tmax_5)))
  data_tmax_may$V68 <- NULL
  
  
  #Data TMax June 
  data_tmax_jun <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_June = mean(tmax_6)))
  data_tmax_jun$V68 <- NULL
  
  
  #Data TMax July 
  data_tmax_jul <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_July = mean(tmax_7)))
  data_tmax_jul$V68 <- NULL
  
  
  #Data TMax Agust 
  data_tmax_agus <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_Agust = mean(tmax_8)))
  data_tmax_agus$V68 <- NULL
  
  
  #Data TMax Sept 
  data_tmax_sept <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_Sept = mean(tmax_9)))
  data_tmax_sept$V68 <- NULL
  
  
  #Data TMax Oct 
  data_tmax_oct <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_Oct = mean(tmax_10)))
  data_tmax_oct$V68 <- NULL
  
  
  #Data TMax Nov 
  data_tmax_Nov <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_Nov = mean(tmax_11)))
  data_tmax_Nov$V68 <- NULL
  
  
  #Data TMean Dic 
  data_tmax_Dic <- data.frame(extraction%>%group_by(V68)%>%summarise(TMax_Dic = mean(tmax_12)))
  data_tmax_Dic$V68 <- NULL
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  result <- list (data_Tem_Mean_An , data_Tem_Range_Mon, data_Tem_Range_An_Range,data_Precip, data_tmean_jan, data_tmean_feb, data_tmean_marc,  data_tmean_apri, data_tmean_may , data_tmean_jun, 
                  data_tmean_jul, data_tmean_agus, data_tmean_sept, data_tmean_oct, data_tmean_Nov, data_tmean_Dic, data_pmean_jan, data_pmean_feb, data_pmean_marc, data_pmean_apri, data_pmean_may,
                  data_pmean_jun, data_pmean_jul, data_pmean_agus, data_pmean_sept, data_pmean_oct, data_pmean_Nov, data_pmean_Dic,
                  data_tmin_jan, data_tmin_feb, data_tmin_marc, data_tmin_apri,  data_tmin_may, data_tmin_jun, data_tmin_jul, data_tmin_agus, data_tmin_sept,
                  data_tmin_oct, data_tmin_Nov, data_tmin_Dic, data_tmax_jan, data_tmax_feb, data_tmax_marc, data_tmax_apri,  data_tmax_may, data_tmax_jun, data_tmax_jul, data_tmax_agus, 
                  data_tmax_sept, data_tmax_oct, data_tmax_Nov, data_tmax_Dic                 
                  )
  
  }
  
  if(menu == 2)
  {
    
    name_var<- "Soil"
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
  if(menu == 3)
  {
    
    name_var<- "Elevation"
    #Elevation
    data_elevati <- data.frame(extraction_ele%>%group_by(V2)%>%summarise(Mean_Elev= mean(DEM_CholutecaCopan)))
    
    result <- data_elevati
  }
  
    
  
  
  #list   
  final_result <- do.call("cbind", result)
  final_result<- final_result [complete.cases(final_result), ]
  colnames(final_result)[1]<-"GRIDCODE"
  
  #Measure the difference
  measure_diff <- apply(final_result,2, sd)
  final_result <- rbind(final_result, measure_diff)
  final_result[nrow(final_result), 1] <- "SD"
  
  write.csv(final_result, paste0("./Excel_Files/AllVariables_",namefile,"_", name_var, ".csv" ), row.names=F)
  return (final_result)
  
}


#graphics_ombrothermic plots ombrothermic diagram
#Arguments. -Data Frame. It is composed by months and values.
#Return Ombrothermic diagram

graphics_ombrothermic <- function (method, numcluster, values_temp, values_preci)
{
  
  if(method == PCA_Kmeans)
  {
    name_method <- "PCA_Kmeans"
  }
  
  if(method == PCA_Mclust)
  {
    name_method <- "PCA_Mclust"
  }
  
  if(method == tnse_GNG)
  {
    name_method <- "tnse_GNG"
  }
  
  
  #Data
  #months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  months_aux <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  data <- data.frame(Months = seq(1:12), Values_Preci = as.numeric(values_preci), Values_Temp = as.numeric(values_temp))
  

  pdf(paste0("./Ombrothermic_Plots/Cluster_",numcluster, "_",name_method ,".pdf"))
  par(mar=c(5,5,2,5))
  matrix_grap <- matrix(nrow=1, ncol=12)
  colnames(matrix_grap) <- months_aux
  matrix_grap[1,] <- as.numeric(values_preci) 
  barplot(as.numeric(values_preci), col= "blue", names.arg= months_aux, ylim= c(0, max(values_preci)), ylab = "Mililitros", cex.names=0.8, main = paste0("Diagrama Ombrotérmico del cluster ", numcluster, "\n", name_method ), cex.main= 0.8 )
  par(new = T)
  with(data, plot(Months, Values_Temp, type="b", pch=16,  axes=F, xlab=NA, ylab=NA, cex=1.2, col= "red", ylim = c(min(Values_Temp),max(Values_Temp))))
  #ylim= c(min(Values_Temp), max(Values_Temp))
  axis(side = 4)
  mtext(side = 4, line = 3, text= 'Grados Centígrados', cex=1)
  legend("topleft",legend=c("Precipitación", "Temperatura"), lty=c(1,1), pch=c(15, 16), col=c("blue", "red"), cex = 0.8)  
  dev.off()
}


months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")


#Graph_all_station plots all clusters with ombrothermic plot
#Arguments -table with all information

graph_all_station <- function (method)
{
  
  
  values_temp <- c("TMean_January", "TMean_February", "TMean_March", "TMean_April", "TMean_May", "TMean_June", "TMean_July", "TMean_Agust", "TMean_Sept", "TMean_Oct", "TMean_Nov", "TMean_Dic")
  values_preci <- c("PMean_January", "PMean_February", "PMean_March", "PMean_April", "PMean_May", "PMean_June", "PMean_July", "PMean_Agust", "PMean_Sept", "PMean_Oct", "PMean_Nov", "PMean_Dic")
  
  
  #Graph ombrotermico
  infoRaster <- info_raster(method, 1)
  
  #Graph for each cluster
  for (i in 1:nrow(infoRaster))
  {
    graphics_ombrothermic(method, i, infoRaster[i,values_temp], infoRaster[i,values_preci])
    
    
  }
  
  
  
}


#graphics_histo_temp plots histograms for temperature_max temperature_min
#Argumetns   method. PCA_Kmean
#                    PCA_Mclustst
#                    tnse_GNG 

graphics_histo_temp <- function (method, numcluster, values_temp_min, values_temp_max)
{
  
  if(method == PCA_Kmeans)
  {
    name_method <- "PCA_Kmeans"
  }
  
  if(method == PCA_Mclust)
  {
    name_method <- "PCA_Mclust"
  }
  
  if(method == tnse_GNG)
  {
    name_method <- "tnse_GNG"
  }
  
  
  #Data
  months_aux <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  data <- data.frame(Months = seq(1:12), Values_TX = as.numeric(values_temp_max), Values_TM = as.numeric(values_temp_min))
  
  
  #pdf(paste0("./Histograms_Temperature_Max_Min/His_TXTM_Cluster_",numcluster, "_",name_method ,".pdf"))
  
  matrix_grap <- matrix(nrow=2, ncol=12)
  colnames(matrix_grap) <- months_aux
  matrix_grap[1,] <- as.numeric((values_temp_max))
  matrix_grap[2,] <- as.numeric((values_temp_min))    
  
  values_temp_max <- round(as.numeric((values_temp_max)), digits=0)
  values_temp_min <- round(as.numeric((values_temp_min)), digits=0) 
  
  Mes <- c(months_aux, months_aux)
  tempera_TX <- rep("Temperatura_Maxima", times = 12)
  tempera_TM <- rep("Temperatura_Minima", times = 12)
  both_tempe <- c(tempera_TX, tempera_TM )
  
  
  Temperatura <- c(rep(both_tempe , times = 1))
  months_aux_freq <- c(values_temp_max,values_temp_min)
  Data <- data.frame(Mes, Temperatura, months_aux_freq)
  #x[order(match(x, y))]
  Data <- Data[order(match(Data$Mes, months_aux )),]
  
  
  ggplot(Data, aes(x = Mes, y = months_aux_freq, fill = Temperatura, label = months_aux_freq)) +geom_bar(stat = "identity") + geom_text(size = 3,  position = position_stack(vjust = 0.5))
  
  #ggplot(Data, aes(x = Mes, y = months_aux_freq)) +geom_bar(aes(fill=Temperatura), stat = "identity") + geom_text(aes(label = months_aux_freq))
  
  
  ggsave(paste0("./Histograms_Temperature_Max_Min/His_TXTM_Cluster_",numcluster, "_",name_method ,".pdf"))

                                                                
  #bar <- barplot(matrix_grap, col= c("red", "blue"), names.arg= months_aux, ylab = "Grados Centigrados", cex.names=0.8, main = paste0("Histograma Temperaturas Máxima y Mínima del cluster ", numcluster, "\n", name_method ), cex.main= 0.8 )

  #text(x = bar, y = matrix_grap[1,], label = matrix_grap[1,] , pos = 3, cex = 0.8, col = "red")
  #dev.off()
}



#Graph_all_station plots all clusters with ombrothermic plot
#Arguments -table with all information


graph_all_station_TX_TM <- function (method)
{
  
  
  values_TX <- c("TMax_January", "TMax_February", "TMax_March", "TMax_April", "TMax_May", "TMax_June", "TMax_July", "TMax_Agust", "TMax_Sept", "TMax_Oct", "TMax_Nov", "TMax_Dic")
  values_TM <- c("TMin_January", "TMin_February", "TMin_March", "TMin_April", "TMin_May", "TMin_June", "TMin_July", "TMin_Agust", "TMin_Sept", "TMin_Oct", "TMin_Nov", "TMin_Dic")
  
  
  #Graph ombrotermico
  infoRaster <- info_raster(method, 1)
  
  #Graph for each cluster
  for (i in 1:nrow(infoRaster))
  {
    graphics_histo_temp (method, i, infoRaster[i,values_TM], infoRaster[i,values_TX])
    
    
  }
  
  
  
}














