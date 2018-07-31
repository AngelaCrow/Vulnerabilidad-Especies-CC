
rm(list = ls())
#--------------------------------------------------------------------

library(maptools)
library(rgbif)
library(dismo)
library(rgdal)

#Interest species
specie<-"Ambystoma mexicanum"   #Change specie

#data base general
base<-read.csv("C:/3CONABIO-IBUNAM_CC_2/bases_temporales/base_jm.csv") 
head(base)

#13 Natural protected area 
Msp<-readOGR(dsn="C:/3CONABIO-IBUNAM_CC_2/13_ANP_g.shp",layer="13_ANP_g")
projection(Msp) <- CRS("+proj=longlat")

#dir.create("C:/3CONABIO-IBUNAM_CC_2/bases_sp/bases_conanp/jm/reptiles")
setwd("C:/3CONABIO-IBUNAM_CC_2/bases_sp/bases_jm")

#Selection data by specie
bsp<-subset.data.frame(base,Especie==specie)
data<-na.omit(bsp[c("Especie","x", "y")]) 
head(data)

  #geographical records cleaning function (Luis Osorio-NicheToolBoox)
  clean_dup <- function(data,longitude,latitude,threshold=0.0){
    data <- data[!is.na(data[,longitude]),]
    dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
    dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
    return(dat_sp1@data)
  }
  
#cleaning function
  total <-clean_dup(data =data,longitude = "x",latitude = "y",threshold = 0.008333333) #threshold = 0.008333333 distancia entre registros
  
#Clean data
  sp_total<-total[,c("Especie","x", "y", "año")]
  t <-nrow(sp_total) #Total occurence data by specie

#save data clean data
  write.csv(sp_total, "E:/ejemplo_conabio/ejemplo/total_Ambystoma_velasci.csv")

#-----------------------------------------------------------
rm(list = ls())

