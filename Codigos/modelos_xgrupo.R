###############################################################################################################
###############################################################################################################
#library(devtools)
#install_github("CONABIO/ENMeval")
#modelo de nicho ecologico especies CONABIO

#install.packages("rJava", repos="http://R-Forge.R-project.org")
#Sys.setenv(JAVA_HOME='C:/Program Files (x86)/Java/jre1.8.0_171') # for 64-bit version
#library(rJava)


#packageVersion("rJava")
#getRversion()

#Modified from ModelosDarwinUICN-master developed by
# - Juan M. Barrrios j.m.barrios@gmail.com
# - Angela P. Cuervo-Robayo ancuervo@gmail.com

library("rgdal", quietly = TRUE)
library("fuzzySim", quietly = TRUE)
library("ENMeval", quietly = TRUE)
library("ROCR", quietly = TRUE)
library("magrittr", quietly = TRUE)
library("readr", quietly = TRUE)
library("dplyr", quietly = TRUE)
library("tools", quietly = TRUE)
library("raster", quietly = TRUE)
library("rJava")
library("rgeos")
library(stringr)
#library("devtools")
#install_github("bobmuscarella/ENMeval@master") #se instala despues de devtools
dir()

#Location of folders with information by species for a cluster
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Please enter a single parameter (input file).\n", call. = FALSE)
} else if (length(args) == 1) {
  print(paste("Processing model for file ", args[1]))
} else {
  stop("Single parameter is needed (input file).\n", call. = FALSE)
}

inputDataFile <- args[1]
outputFolder <- inputDataFile %>%
  basename %>% 
  file_path_sans_ext %>% str_remove ("^Total_")

if (!dir.exists(outputFolder)) {
  dir.create(outputFolder, recursive = TRUE)
}


#Data by specie
sp<-read.csv(inputDataFile)
grupo<-"anfibios"
specie<-sp[,c("Especie","x","y")]
colnames(specie)<-c("Especie","Dec_Long", "Dec_Lat")
head(specie)

#Specie name
especie<-as.character(specie[1,1])

#Current period variables (1980-2009) cut with the M by species
directorio_covariables<-file.path("/LUSTRE/Genetica/SEE/cmoreno/anfibios/M", especie,"tiff10_49")
print(directorio_covariables)
env2a<-list.files(directorio_covariables, pattern="*.tif$", full.names=TRUE) #selección del conjunto de variables por especie
print(env2a)
env2<-stack(env2a)
#plot(env2)

#Future Climate Variables
#--------------------------------- 4.5

covarDataFolder_hadgem30_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp45_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_hadgem60_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp45_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_hadgem90_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp45_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi30_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp45_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi60_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp45_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi90_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp45_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd30_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp45_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd60_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp45_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd90_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp45_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_cnrm30_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp45_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_cnrm60_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp45_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_cnrm90_4.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp45_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  

#--------------------------------- 8.5
covarDataFolder_hadgem30_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp85_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_hadgem60_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp85_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_hadgem90_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/HAD_ESM_LR_bios/HAD_ESM_LR_bios/HAD_ESM_LR_rcp85_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi30_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp85_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi60_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp85_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_mpi90_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/MPI_ESM_LR_bios/MPI_ESM_LR_bios/MPI_ESM_LR_rcp85_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd30_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp85_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd60_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp85_2045_2069_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_gfd90_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/GFDL_CM3_bios/GFDL_CM3_bios/GFDL_CM3_rcp85_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_cnrm30_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp85_2015_2039_bio", pattern="*.tif$", full.names=TRUE))  
covarDataFolder_cnrm60_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp85_2045_2069_bio", pattern="*.tif", full.names=TRUE))  
covarDataFolder_cnrm90_8.5<-stack(list.files(path = "/LUSTRE/Genetica/SEE/cmoreno/capas_transferencias/CNRMCM5_bios/CNRMCM5_bios/CNRMCM5_rcp85_2075_2099_bio", pattern="*.tif$", full.names=TRUE))  

#######################################################################################################

#Variable selection by correlations and importance
sp_files <- env2a #
var_cor <- read.csv("/LUSTRE/Genetica/SEE/cmoreno/anfibios/anfibios_var.csv") #data base of species and variables selected for low correlation and biological importance
var_importantes  <- var_cor[ which(var_cor$Especie==especie), ] #select row for specie
var_importantes 

var_importantes$Especie <- gsub(x = var_importantes$Especie," ",replacement = "_")
names(var_importantes) <- c(names(var_importantes)[1:2],paste0("X_bio",1:19),"Especie")

occs <- specie
occs$Especie <-  gsub(x = occs$Especie," ",replacement = "_")

#Calibration variables for each species
capas_nicho_index <- which(occs$Especie[1]==var_importantes$Especie) 
capas_nicho_index_1 <- which(var_importantes[capas_nicho_index,] ==1)
capas_a_usar <- names(var_importantes)[capas_nicho_index_1] 
capas_a_usar

#Selection of variables to be used by species
enviromentalVariables <- env2
selectedVariables <- enviromentalVariables[[capas_a_usar]]
#plot(selectedVariables)

#M selection
Msp<-env2[[1]]
#plot(Msp)

#Future scenarios variable selection
envt<-selectedVariables  
env_hadgem30_4.5t<-covarDataFolder_hadgem30_4.5[[capas_a_usar]]
env_hadgem60_4.5t<-covarDataFolder_hadgem60_4.5[[capas_a_usar]]
env_hadgem90_4.5t<-covarDataFolder_hadgem90_4.5[[capas_a_usar]]
env_mpi30_4.5t<-covarDataFolder_mpi30_4.5[[capas_a_usar]]
env_mpi60_4.5t<-covarDataFolder_mpi60_4.5[[capas_a_usar]]
env_mpi90_4.5t<-covarDataFolder_mpi90_4.5[[capas_a_usar]]
env_gfd30_4.5t<-covarDataFolder_gfd30_4.5[[capas_a_usar]]
env_gfd60_4.5t<-covarDataFolder_gfd60_4.5[[capas_a_usar]]
env_gfd90_4.5t<-covarDataFolder_gfd90_4.5[[capas_a_usar]]
env_cnrm30_4.5t<-covarDataFolder_cnrm30_4.5[[capas_a_usar]]
env_cnrm60_4.5t<-covarDataFolder_cnrm60_4.5[[capas_a_usar]]
env_cnrm90_4.5t<-covarDataFolder_cnrm90_4.5[[capas_a_usar]]

#--------------------------------- 8.5
env_hadgem30_8.5t<-covarDataFolder_hadgem30_8.5[[capas_a_usar]]
env_hadgem60_8.5t<-covarDataFolder_hadgem60_8.5[[capas_a_usar]]
env_hadgem90_8.5t<-covarDataFolder_hadgem90_8.5[[capas_a_usar]]
env_mpi30_8.5t<-covarDataFolder_mpi30_8.5[[capas_a_usar]]
env_mpi60_8.5t<-covarDataFolder_mpi60_8.5[[capas_a_usar]]
env_mpi90_8.5t<-covarDataFolder_mpi90_8.5[[capas_a_usar]]
env_gfd30_8.5t<-covarDataFolder_gfd30_8.5[[capas_a_usar]]
env_gfd60_8.5t<-covarDataFolder_gfd60_8.5[[capas_a_usar]]
env_gfd90_8.5t<-covarDataFolder_gfd90_8.5[[capas_a_usar]]
env_cnrm30_8.5t<-covarDataFolder_cnrm30_8.5[[capas_a_usar]]
env_cnrm60_8.5t<-covarDataFolder_cnrm60_8.5[[capas_a_usar]]
env_cnrm90_8.5t<-covarDataFolder_cnrm90_8.5[[capas_a_usar]]

#cut future variables with mask by species
#Present
env<-selectedVariables  
dir.create(file.path(outputFolder, "1950_2000_bio"))
writeRaster(env,
            file.path(outputFolder, "1950_2000_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#HAD_ESM_LR rcp4.5
env_hadgem30_4.5a <- raster::crop(env_hadgem30_4.5t, Msp)
env_hadgem30_4.5 <- raster::mask(env_hadgem30_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp45_2015_2039_bio"))
writeRaster(env_hadgem30_4.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp45_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_hadgem60_4.5a <- raster::crop(env_hadgem60_4.5t, Msp)
env_hadgem60_4.5 <- raster::mask(env_hadgem60_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp45_2045_2069_bio"))
writeRaster(env_hadgem60_4.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp45_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_hadgem90_4.5a <- raster::crop(env_hadgem90_4.5t, Msp)
env_hadgem90_4.5 <- raster::mask(env_hadgem90_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp45_2075_2099_bio"))
writeRaster(env_hadgem90_4.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp45_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#MPI_ESM_LR rcp4.5
env_mpi30_4.5a <- raster::crop(env_mpi30_4.5t, Msp)
env_mpi30_4.5 <- raster::mask(env_mpi30_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp45_2015_2039_bio"))
writeRaster(env_mpi30_4.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp45_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_mpi60_4.5a <- raster::crop(env_mpi60_4.5t, Msp)
env_mpi60_4.5 <- raster::mask(env_mpi60_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp45_2045_2069_bio"))
writeRaster(env_mpi60_4.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp45_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_mpi90_4.5a <- raster::crop(env_mpi90_4.5t, Msp)
env_mpi90_4.5 <- raster::mask(env_mpi90_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp45_2075_2099_bio"))
writeRaster(env_mpi90_4.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp45_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#GFDL_CM3 rcp4.5
env_gfd30_4.5a <- raster::crop(env_gfd30_4.5t, Msp)
env_gfd30_4.5 <- raster::mask(env_gfd30_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp45_2015_2039_bio"))
writeRaster(env_gfd30_4.5,
            file.path(outputFolder, "GFDL_CM3_rcp45_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_gfd60_4.5a <- raster::crop(env_gfd60_4.5t, Msp)
env_gfd60_4.5 <- raster::mask(env_gfd60_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp45_2045_2069_bio"))
writeRaster(env_gfd60_4.5,
            file.path(outputFolder, "GFDL_CM3_rcp45_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_gfd90_4.5a <- raster::crop(env_gfd90_4.5t, Msp)
env_gfd90_4.5 <- raster::mask(env_gfd90_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp45_2075_2099_bio"))
writeRaster(env_gfd90_4.5,
            file.path(outputFolder, "GFDL_CM3_rcp45_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#GFDL_CM3 rcp4.5
env_cnrm30_4.5a <- raster::crop(env_cnrm30_4.5t, Msp)
env_cnrm30_4.5 <- raster::mask(env_cnrm30_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp45_2015_2039_bio"))
writeRaster(env_cnrm30_4.5,
            file.path(outputFolder, "CNRMCM5_rcp45_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_cnrm60_4.5a <- raster::crop(env_cnrm60_4.5t, Msp)
env_cnrm60_4.5 <- raster::mask(env_cnrm60_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp45_2045_2069_bio"))
writeRaster(env_cnrm60_4.5,
            file.path(outputFolder, "CNRMCM5_rcp45_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_cnrm90_4.5a <- raster::crop(env_cnrm90_4.5t, Msp)
env_cnrm90_4.5 <- raster::mask(env_cnrm90_4.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp45_2075_2099_bio"))
writeRaster(env_cnrm90_4.5,
            file.path(outputFolder, "CNRMCM5_rcp45_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)
#--------------------------------- 8.5
#HAD_ESM_LR rcp8.5
env_hadgem30_8.5a <- raster::crop(env_hadgem30_8.5t, Msp)
env_hadgem30_8.5 <- raster::mask(env_hadgem30_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp85_2015_2039_bio"))
writeRaster(env_hadgem30_8.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp85_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_hadgem60_8.5a <- raster::crop(env_hadgem60_8.5t, Msp)
env_hadgem60_8.5 <- raster::mask(env_hadgem60_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp85_2045_2069_bio"))
writeRaster(env_hadgem60_8.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp85_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_hadgem90_8.5a <- raster::crop(env_hadgem90_8.5t, Msp)
env_hadgem90_8.5 <- raster::mask(env_hadgem90_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "HAD_ESM_LR_rcp85_2075_2099_bio"))
writeRaster(env_hadgem90_8.5,
            file.path(outputFolder, "HAD_ESM_LR_rcp85_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#MPI_ESM_LR rcp8.5
env_mpi30_8.5a <- raster::crop(env_mpi30_8.5t, Msp)
env_mpi30_8.5 <- raster::mask(env_mpi30_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp85_2015_2039_bio"))
writeRaster(env_mpi30_8.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp85_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_mpi60_8.5a <- raster::crop(env_mpi60_8.5t, Msp)
env_mpi60_8.5 <- raster::mask(env_mpi60_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp85_2045_2069_bio"))
writeRaster(env_mpi60_8.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp85_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_mpi90_8.5a <- raster::crop(env_mpi90_8.5t, Msp)
env_mpi90_8.5 <- raster::mask(env_mpi90_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "MPI_ESM_LR_rcp85_2075_2099_bio"))
writeRaster(env_mpi90_8.5,
            file.path(outputFolder, "MPI_ESM_LR_rcp85_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#GFDL_CM3 rcp8.5
env_gfd30_8.5a <- raster::crop(env_gfd30_8.5t, Msp)
env_gfd30_8.5 <- raster::mask(env_gfd30_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp85_2015_2039_bio"))
writeRaster(env_gfd30_8.5,
            file.path(outputFolder, "GFDL_CM3_rcp85_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_gfd60_8.5a <- raster::crop(env_gfd60_8.5t, Msp)
env_gfd60_8.5 <- raster::mask(env_gfd60_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp85_2045_2069_bio"))
writeRaster(env_gfd60_8.5,
            file.path(outputFolder, "GFDL_CM3_rcp85_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_gfd90_8.5a <- raster::crop(env_gfd90_8.5t, Msp)
env_gfd90_8.5 <- raster::mask(env_gfd90_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "GFDL_CM3_rcp85_2075_2099_bio"))
writeRaster(env_gfd90_8.5,
            file.path(outputFolder, "GFDL_CM3_rcp85_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

#GFDL_CM3 rcp8.5
env_cnrm30_8.5a <- raster::crop(env_cnrm30_8.5t, Msp)
env_cnrm30_8.5 <- raster::mask(env_cnrm30_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp85_2015_2039_bio"))
writeRaster(env_cnrm30_8.5,
            file.path(outputFolder, "CNRMCM5_rcp85_2015_2039_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_cnrm60_8.5a <- raster::crop(env_cnrm60_8.5t, Msp)
env_cnrm60_8.5 <- raster::mask(env_cnrm60_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp85_2045_2069_bio"))
writeRaster(env_cnrm60_8.5,
            file.path(outputFolder, "CNRMCM5_rcp85_2045_2069_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

env_cnrm90_8.5a <- raster::crop(env_cnrm90_8.5t, Msp)
env_cnrm90_8.5 <- raster::mask(env_cnrm90_8.5a ,  Msp) 
dir.create(file.path(outputFolder, "CNRMCM5_rcp85_2075_2099_bio"))
writeRaster(env_cnrm90_8.5,
            file.path(outputFolder, "CNRMCM5_rcp85_2075_2099_bio/.tif"), 
            bylayer = T, suffix='names',
            overwrite = TRUE)

##################################################################################################### 
#### Calibration ####
# Divides your data into trainining and test data sets. 70/30 %
covarData<-specie

sampleDataPoints <- sample.int(
  nrow(covarData),
  size = floor(0.7*nrow(covarData))
)

selectedValues <- rep(0, nrow(covarData)) %>% inset(sampleDataPoints, 1)

covarData$isTrain <- selectedValues

write.csv(covarData,
          file = file.path(outputFolder, "speciesCovarDB.csv"),
          row.names = FALSE)


# MAXENT calibration
# We used ENMeval package to estimate optimal model complexity (Muscarrella et al. 2014)
# Modeling process, first separate the calibration and validation data
occsCalibracion <- covarData %>%
  as.data.frame() %>%
  dplyr::filter(isTrain == 1) %>%
  dplyr::select(Dec_Long, Dec_Lat)

occsValidacion <- covarData %>%
  as.data.frame() %>%
  dplyr::filter(isTrain == 0) %>%
  dplyr::select(Dec_Long, Dec_Lat) 

# Background
bg.df <- dismo::randomPoints(env2[[1]], n = 10000) %>% as.data.frame()

#Divide backgeound into train and test 
sample.bg <- sample.int(
  nrow(bg.df),
  size = floor(0.7*nrow(bg.df))
)
selectedValues.bg <- rep(0, nrow(bg.df)) %>% inset(sample.bg, 1)

bg.df$isTrain <- selectedValues.bg

crs.wgs84 <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
sp::coordinates(bg.df) <- c("x", "y")
sp::proj4string(bg.df) <- crs.wgs84
bg.dfbio <- raster::extract(enviromentalVariables, bg.df) 

bg.df<-as.data.frame(bg.df)
bg.dfbio <- cbind(bg.df, bg.dfbio) %>% as.data.frame()
head(bg.dfbio)


write.csv(bg.dfbio,
          file = file.path(outputFolder, "background_points.csv"),
          row.names = FALSE)

#training background
bg.df.cal <- bg.df %>%
  dplyr::filter(isTrain == 1) %>%
  dplyr::select(x, y)

# ENMeval
sp.models <- ENMevaluate(occsCalibracion, selectedVariables, bg.df.cal, RMvalues = seq(0.5, 4, 0.5),
                         fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                         method = "randomkfold", kfolds = 4, bin.output = TRUE,
                         parallel = TRUE, numCores = parallel::detectCores()-1, 
                         updateProgress = TRUE)

resultados_enmeval <- sp.models@results

resultados_enmeval2<-cbind.data.frame(especie,resultados_enmeval)

write.csv(resultados_enmeval2,
          file = file.path(outputFolder, "enmeval_results.csv"),
          row.names = FALSE)


# delta_aic <- which(resultados_enmeval$delta.AICc == 0)
modelsAIC0 <- resultados_enmeval %>%
  mutate(index = rownames(resultados_enmeval)) %>%
  filter(delta.AICc == 0) %>%
  select(index, settings) %>%
  mutate(index = as.numeric(index), settings = as.character(settings))

aic.opt <- sp.models@models[[which(sp.models@results$delta.AICc==0)]]
importa <- var.importance(aic.opt)

importa2<-cbind.data.frame(especie, importa)

write.csv(importa2, 
          file = file.path(outputFolder, "varImportance.csv"),
          row.names = FALSE)

# save species niche (raw output) model over raster 
saveRasterWithSettings <- function(models, predictions, prefix) {
  raster::writeRaster(predictions[[models["settings"]]],
                      file.path(outputFolder, paste0(prefix,
                                                     models["settings"],
                                                     ".tif")),
                      overwrite = TRUE)
}

apply(modelsAIC0, 1, saveRasterWithSettings,
      predictions = sp.models@predictions, prefix = "ENM_prediction_M_raw")

#########################################################################################################

#### Projections ####
# predict choicemodel over current climate variables
predictAndSave <- function(model, models, data, prefix, occs) {
  choicedModel <- models[[as.integer(model["index"])]]
  predictions <- dismo::predict(choicedModel, data)
  raster::writeRaster(predictions,
                      file.path(outputFolder, paste0(prefix,
                                                      "log_",
                                                      model["settings"],
                                                      ".tif")),
                      overwrite = TRUE)
  
  #Threshold prection using minimum traning (min) and 10 percentil (q10) values  
  occsValues <- raster::extract(predictions, occs)
  
  minValOcc <- min(occsValues, na.rm = TRUE)
  minValOcc2<-cbind.data.frame(especie,minValOcc)
  
  write.csv( minValOcc2, 
             file = file.path(outputFolder, "minValOcc_Lepus callotis.csv"),
             row.names = FALSE)
  
  raster::writeRaster(reclassify(predictions,
                                 c(-Inf, minValOcc, 0, minValOcc, Inf, 1)),
                      file.path(outputFolder, paste0(prefix,
                                                      "bin_min_",
                                                      model["settings"],
                                                      ".tif")),
                      overwrite = TRUE)
  
  q10ValOcc <- quantile(occsValues, 0.1, na.rm = TRUE)
  q10ValOcc1<-cbind.data.frame(especie,q10ValOcc)
  
  write.csv(q10ValOcc1, 
            file = file.path(outputFolder, "q10ValOcc.csv"),
            row.names = FALSE)
  
  
  raster::writeRaster(reclassify(predictions,
                                 c(-Inf, q10ValOcc, 0, q10ValOcc, Inf, 1)),
                      file.path(outputFolder, paste0(prefix,
                                                      "bin_q10_",
                                                      model["settings"],
                                                      ".tif")),
                      overwrite = TRUE)
}

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env, prefix = "ENM_pres",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem30_4.5, prefix = "ENM_hadgem30_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem60_4.5, prefix = "ENM_hadgem60_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem90_4.5, prefix = "ENM_hadgem90_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi30_4.5, prefix = "mpi30_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi60_4.5, prefix = "mpi60_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi90_4.5, prefix = "mpi90_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd30_4.5, prefix = "gfd30_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd60_4.5, prefix = "gfd60_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd90_4.5, prefix = "gfd90_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm30_4.5, prefix = "cnrm30_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm60_4.5, prefix = "cnrm60_4.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm90_4.5, prefix = "cnrm90_4.5",
      occs = occsCalibracion)

##########################################################################################################
apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem30_8.5, prefix = "ENM_hadgem30_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem60_8.5, prefix = "ENM_hadgem60_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_hadgem90_8.5, prefix = "ENM_hadgem90_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi30_8.5, prefix = "mpi30_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi60_8.5, prefix = "mpi60_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_mpi90_8.5, prefix = "mpi90_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd30_8.5, prefix = "gfd30_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd60_8.5, prefix = "gfd60_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_gfd90_8.5, prefix = "gfd90_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm30_8.5, prefix = "cnrm30_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm60_8.5, prefix = "cnrm60_8.5",
      occs = occsCalibracion)

apply(modelsAIC0, 1, predictAndSave,
      models = sp.models@models, data = env_cnrm90_8.5, prefix = "cnrm90_8.5",
      occs = occsCalibracion)


####ENMTest####
#source("funciones_LAE.R")
#Threslhold independent

#AUC
aucCalculator <- function(prediction, occs, bgPoints) {
  data <- rbind(occs, setNames(bgPoints, names(occs)))
  labels <- c(rep(1, nrow(occs)),
              rep(0, nrow(bgPoints)))
  scores <- raster::extract(prediction, data)
  pred <- ROCR::prediction(scores, labels)
  # perf <- performance(pred, "tpr", "fpr")
  auc <- performance(pred, "auc")@y.values[[1]]
  return(auc)
}

aucStatistcs <- function(model, models, env, occs, bgPoints) {
  result <- apply(model, 1, function(x, models, env, occs, bgPoints){
    choicedModel <- models[[as.integer(x["index"])]]
    prediction <- dismo::predict(choicedModel, env)
    auc <- aucCalculator(prediction, occs, bgPoints)
    return(c(x["settings"], auc))
  },
  models = models,
  env = env,
  occs = occs,
  bgPoints = bgPoints)
  
  result <- data.frame(
    matrix(unlist(result), nrow = nrow(model), byrow = TRUE),
    stringsAsFactors = FALSE
  )
  
  names(result) <- c("settings", "AUC")
  
  result <- result %>% mutate(AUC = as.numeric(AUC))
  
  return(result)
}


# Testing background
bg.df.test <- bg.df %>%
  dplyr::filter(isTrain == 0) %>%
  dplyr::select(x, y)

resultsAUC <- aucStatistcs(modelsAIC0, sp.models@models, env, occsValidacion, bg.df.test)
resultsAUC1<-cbind.data.frame(especie,resultsAUC)

write.csv(resultsAUC1,
          file = file.path(outputFolder, "data_auc.csv"),
          row.names = FALSE)
###############################################################################################################

