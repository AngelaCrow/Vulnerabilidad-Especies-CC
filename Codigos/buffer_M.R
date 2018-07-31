
#-----------------------------------------------------------------------------------------------------------------------
rm(list = ls())

library("rgdal")
library("rgeos")
library("sp")
library("corrplot")
library("NetComp")
library("raster")

#Directory
dir.create("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2")
setwd("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2")

#Data base of clean occurrence records
base1<-read.csv("E:/3.CONABIO-IBUNAM_CC_2/bases_sp/bases_finales/bases_grupos/base_aves.csv")
base<-base1[,c("Especie","x", "y")]

#Data base by specie
ejemplo<-subset(base, Especie=="Granatellus venustus")
sp<-as.character(ejemplo[1,1])
sp

#Ecoregion (Olson et al.2001)
prov<-readOGR(dsn="E:/3.CONABIO-IBUNAM_CC_2/provincias/official_teow/wwf_terr_ecos.shp",layer="wwf_terr_ecos")
projection(prov) <- CRS("+proj=longlat")

#Variables bioclimaticas del ultimo periodo 1980-2009
bios <- stack(list.files("E:/3.CONABIO-IBUNAM_CC_2/capas_tranferencias/1950_2000_bio",pattern = "*.asc$",full.names = TRUE))
#plot(bios[[1]])

#Buffer 10 km of natural Protected Area (NPA) of interes
buffer<-readOGR(dsn="E:/3.CONABIO-IBUNAM_CC_2/13ANP_buffer/13ANP_buffer.shp",layer="13ANP_buffer")
projection(buffer) <- CRS("+proj=longlat")
head(buffer, n=13)

#Specie into NPA
sps<-ejemplo
coordinates(sps) <- ~x+y
projection(sps) <- CRS("+proj=longlat")

#Over occurence data by NPA with buffer
ov <- over(sps, buffer , fn = NULL)
base_anp<-cbind(ejemplo,ov)
head(base_anp)

#Ocurrence data by specie into NPA
ANP1<-nrow(as.data.frame(subset(base_anp, OBJECTID==10)))  # Centla
ANP2<-nrow(as.data.frame(subset(base_anp, OBJECTID==6)))  #Terminos
ANP3<-nrow(as.data.frame(subset(base_anp, OBJECTID==13)))  #Tehuacan
ANP4<-nrow(as.data.frame(subset(base_anp, OBJECTID==9)))  #Monarca
ANP5<-nrow(as.data.frame(subset(base_anp, OBJECTID==7)))  #Nichupte
ANP6<-nrow(as.data.frame(subset(base_anp, OBJECTID==5)))  #Janos
ANP7<-nrow(as.data.frame(subset(base_anp, OBJECTID==12))) #San Pedro Martir
ANP8<-nrow(as.data.frame(subset(base_anp, OBJECTID==3))) #Constitucion
ANP9<-nrow(as.data.frame(subset(base_anp, OBJECTID==8))) #Mapimi
ANP10<-nrow(as.data.frame(subset(base_anp, OBJECTID==2))) #Sumidero
ANP11<-nrow(as.data.frame(subset(base_anp, OBJECTID==11))) #Ocote
ANP12<-nrow(as.data.frame(subset(base_anp, OBJECTID==4))) #Vizcaino
ANP13<-nrow(as.data.frame(subset(base_anp, OBJECTID==1))) #CADNR004

#Total datos
t <-nrow(base_anp)

#Occurende records for NPA
ANP<-cbind.data.frame(ANP1,ANP2,ANP3,ANP4,ANP5,ANP6,ANP7,ANP8,ANP9,ANP10,ANP11,ANP12,ANP13)

#Total ocurrence data into NPA
reservas<-rowSums (ANP)

# Union information by NPA
registros <-cbind.data.frame(sp,t,ANP,reservas) 

# Names
names(registros) <- c("Especie","registros", "Centla", "Terminos","Tehuacan",
                      "Monarca","Nichupte","Janos","San Pedro Martir","Constitucion",
                      "Mapimi","Sumidero","Ocote", "Vizcaino", "CADNR004", "reservas")    
#head(registros)

#Save data base
dir.create("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/registros_aves")
write.csv(registros, "E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/registros_aves/registros_Granatellus venustus.csv")

#----M selection based in occurence data with a buffer of 10 km

#Make buffer of occurence data
sp_f<-gBuffer(sps, byid=FALSE, id=NULL, width=0.08333, quadsegs=5, capStyle="ROUND",
              joinStyle="ROUND", mitreLimit=1.0)

#select the ecoregions that contain points with buffer to calculate the M
sp_M <- prov[!is.na(sp::over(prov, sp::geometry(sp_f))), ] 
info_M<-as.data.frame(sp_M)
info_M2<-cbind.data.frame(Especie="Granatellus venustus",unique(info_M$ECO_NAME))
colnames(info_M2)[2]<-"Ecoregion"
head(info_M2)

#Save M.
dir.create("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/Granatellus venustus")
setwd("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/Granatellus venustus")
writeOGR(sp_M,dsn = '.', layer = 'Granatellus venustus_M', driver = "ESRI Shapefile", overwrite=TRUE)
write.csv(info_M2,"E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/Granatellus venustus/info_M_Granatellus venustus.csv")

#Corte de variables con base a su M del ?ltimo periodo
bios_M<- raster::crop(bios, sp_M)
env <- raster::mask(bios_M, sp_M) 
#plot(env)

dir.create("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/Granatellus venustus/tiff80_09")
writeRaster(env, file.path('E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/Granatellus venustus/tiff80_09', names(env)),format="GTiff", bylayer=TRUE, driver='HFA',overwrite=TRUE)

#Variables correlations by specie
corr_bios<-layerStats(env, 'pearson', na.rm=T)
corr_matrix<-corr_bios$'pearson correlation coefficient'
dir.create("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/correlaciones_aves")
write.csv(corr_matrix, "E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/correlaciones_aves/corr_Granatellus venustus.csv")

#Only leave the variables that have more than 80% correlation
corr_mas80<-matrix_threshold(corr_matrix, threshold=0.8, minval=0, abs=TRUE, rm.na=FALSE)
corr_mas80
write.csv(corr_mas80, "E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/aves2/correlaciones_aves/corr80_Granatellus venustus.csv")

#-----------------------------------------------------------------------------------------------------------------------

rm(list = ls())

