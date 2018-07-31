rm(list = ls())

#indices cambio clim?tico
#conabio

library("rgdal", quietly = TRUE)
library("readr", quietly = TRUE)
library("raster", quietly = TRUE)
library("rgeos")
library("rgl")

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#occurence data
sp<-read.csv("E:/3.CONABIO-IBUNAM_CC_2/sp_modelos/mamiferos/bases/Total_Chaetodipus_arenarius.csv")
grupo<-"mamiferos"
points<-sp[,c("Especie","x","y")]
points_xy<-as.data.frame(points[,c("x", "y")])

#specie name
sp<-as.character(points[1,1])

#---------------------------------------------------------------------------------------------
#present variables cut with M

list_bios<-list.files("E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/1950_2000_bio",pattern = "*.tif$",full.names = TRUE)
bios<-stack(list_bios)
names(bios) <- gsub("X_X_", "X_", names(bios))
Msp<-bios[[1]]

#-------------------------------------------------------------------------------------------
# Distribution potential models
modelos<-list.files("E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius",pattern = "*.tif$",full.names = TRUE)
modelo<-stack(modelos)
#plot(modelo)

############present
pres_10<-raster::subset(modelo, grep('ENM_presbin_q10_', names(modelo), value = T))
pres_min<-raster::subset(modelo, grep('ENM_presbin_min_', names(modelo), value = T))

#RCP 4.5 2015-2039
cnrm30_4.5_10<-raster::subset(modelo, grep('cnrm30_4.5bin_q10_', names(modelo), value = T))
hadgem30_4.5_10<-raster::subset(modelo, grep('ENM_hadgem30_4.5bin_q10_', names(modelo), value = T))
gfd30_4.5_10<-raster::subset(modelo, grep('gfd30_4.5bin_q10_', names(modelo), value = T))
mpi30_4.5_10<-raster::subset(modelo, grep('mpi30_4.5bin_q10_', names(modelo), value = T))

#assemble 2015-2039
stack30_4.5<-stack(cnrm30_4.5_10,gfd30_4.5_10,mpi30_4.5_10,hadgem30_4.5_10)
sum30_4.5<- calc(stack30_4.5, sum)

#Save 2015-2039
setwd("E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius")
dir.create("E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices")

#Reclassify assemble to binary 2015-2039
valor<-2
r30_4.5<-reclassify(sum30_4.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r30_4.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r30_4.5.tif', overwrite=TRUE)

#RCP 4.5 2045-2069
cnrm60_4.5_10<-raster::subset(modelo, grep('cnrm60_4.5bin_q10_', names(modelo), value = T))
hadgem60_4.5_10<-raster::subset(modelo, grep('ENM_hadgem60_4.5bin_q10_', names(modelo), value = T))
gfd60_4.5_10<-raster::subset(modelo, grep('gfd60_4.5bin_q10_', names(modelo), value = T))
mpi60_4.5_10<-raster::subset(modelo, grep('mpi60_4.5bin_q10_', names(modelo), value = T))

#assemble 2045-2069
stack60_4.5<-stack(cnrm60_4.5_10,gfd60_4.5_10,mpi60_4.5_10,hadgem60_4.5_10)
sum60_4.5<- calc(stack60_4.5, sum)

#Reclassify assemble to binary 2045-2069
r60_4.5<-reclassify(sum60_4.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r60_4.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r60_4.5.tif', overwrite=TRUE)

#RCP 4.5 2075-2099
cnrm90_4.5_10<-raster::subset(modelo, grep('cnrm90_4.5bin_q10_', names(modelo), value = T))
hadgem90_4.5_10<-raster::subset(modelo, grep('ENM_hadgem90_4.5bin_q10_', names(modelo), value = T))
gfd90_4.5_10<-raster::subset(modelo, grep('gfd90_4.5bin_q10_', names(modelo), value = T))
mpi90_4.5_10<-raster::subset(modelo, grep('mpi90_4.5bin_q10_', names(modelo), value = T))

#assemble  2075-2099
stack90_4.5<-stack(cnrm90_4.5_10,gfd90_4.5_10,mpi90_4.5_10,hadgem90_4.5_10)
sum90_4.5<- calc(stack90_4.5, sum)

#Reclassify assemble to binary 2075-2099
r90_4.5<-reclassify(sum90_4.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r90_4.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r90_4.5.tif', overwrite=TRUE)

#--------------------------------------------------
#RCP 8.5 2015-2039
cnrm30_8.5_10<-raster::subset(modelo, grep('cnrm30_8.5bin_q10_', names(modelo), value = T))
hadgem30_8.5_10<-raster::subset(modelo, grep('ENM_hadgem30_8.5bin_q10_', names(modelo), value = T))
gfd30_8.5_10<-raster::subset(modelo, grep('gfd30_8.5bin_q10_', names(modelo), value = T))
mpi30_8.5_10<-raster::subset(modelo, grep('mpi30_8.5bin_q10_', names(modelo), value = T))

#assemble 2015-2039
stack30_8.5<-stack(cnrm30_8.5_10,gfd30_8.5_10,mpi30_8.5_10,hadgem30_8.5_10)
sum30_8.5<- calc(stack30_8.5, sum)

#Reclassify assemble to binary 2015-2039
valor<-2
r30_8.5<-reclassify(sum30_8.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r30_8.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r30_8.5.tif', overwrite=TRUE)

#RCP 8.5 2045-2069
cnrm60_8.5_10<-raster::subset(modelo, grep('cnrm60_8.5bin_q10_', names(modelo), value = T))
hadgem60_8.5_10<-raster::subset(modelo, grep('ENM_hadgem60_8.5bin_q10_', names(modelo), value = T))
gfd60_8.5_10<-raster::subset(modelo, grep('gfd60_8.5bin_q10_', names(modelo), value = T))
mpi60_8.5_10<-raster::subset(modelo, grep('mpi60_8.5bin_q10_', names(modelo), value = T))

#assemble 2045-2069
stack60_8.5<-stack(cnrm60_8.5_10,gfd60_8.5_10,mpi60_8.5_10,hadgem60_8.5_10)
sum60_8.5<- calc(stack60_8.5, sum)

#Reclassify assemble to binary 2045-2069
r60_8.5<-reclassify(sum60_8.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r60_8.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r60_8.5.tif', overwrite=TRUE)

#RCP 8.5 2075-2099
cnrm90_8.5_10<-raster::subset(modelo, grep('cnrm90_8.5bin_q10_', names(modelo), value = T))
hadgem90_8.5_10<-raster::subset(modelo, grep('ENM_hadgem90_8.5bin_q10_', names(modelo), value = T))
gfd90_8.5_10<-raster::subset(modelo, grep('gfd90_8.5bin_q10_', names(modelo), value = T))
mpi90_8.5_10<-raster::subset(modelo, grep('mpi90_8.5bin_q10_', names(modelo), value = T))

#assemble 2075-2099
stack90_8.5<-stack(cnrm90_8.5_10,gfd90_8.5_10,mpi90_8.5_10,hadgem90_8.5_10)
sum90_8.5<- calc(stack90_8.5, sum)

#Reclassify assemble to binary 2075-2099
r90_8.5<-reclassify(sum90_8.5, c(-Inf, valor,0,valor, Inf,1))
writeRaster(r90_8.5,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/r90_8.5.tif', overwrite=TRUE)

#-------------------------------------------------------SENSIBILIDAD-----------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------
# CAMBIO DE ?REA

######FUNCI?N CAMBIO DE ?REA

reportaFuturo <- function(raster_presente_bin, raster_futuro_bin){
  reclass_matrix <- matrix(c(0, 1, 0, 10), ncol=2)
  offset_raster <- raster::reclassify(raster_futuro_bin, reclass_matrix)
  
  result <- raster_presente_bin + offset_raster
  
  return(list(stats=raster::freq(result, useNA='no'),
              raster_cambio=result))
}


presente<-pres_10
#  plot(presente)

#---------------------------------RCP 4.5
proyeccion <- reportaFuturo(presente, r30_4.5)
statsr30_4.5<-data.frame(proyeccion$stats)
write.csv(statsr30_4.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_30_4.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_30_4.5.tif', overwrite=TRUE)

proyeccion <- reportaFuturo(presente, r60_4.5)
statsr60_4.5<-data.frame(proyeccion$stats)
write.csv(statsr60_4.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_60_4.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_60_4.5.tif', overwrite=TRUE)

proyeccion <- reportaFuturo(presente, r90_4.5)
statsr90_4.5<-data.frame(proyeccion$stats)
write.csv(statsr90_4.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_90_4.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_90_4.5.tif', overwrite=TRUE)

#---------------------------------RCP 8.5
proyeccion <- reportaFuturo(presente, r30_8.5)
statsr30_8.5<-data.frame(proyeccion$stats)
write.csv(statsr30_8.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_30_8.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_30_8.5.tif', overwrite=TRUE)

proyeccion <- reportaFuturo(presente, r60_8.5)
statsr60_8.5<-data.frame(proyeccion$stats)
write.csv(statsr60_8.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_60_8.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_60_8.5.tif', overwrite=TRUE)

proyeccion <- reportaFuturo(presente, r90_8.5)
statsr90_8.5<-data.frame(proyeccion$stats)
write.csv(statsr90_8.5,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_90_8.5.csv")
writeRaster(proyeccion$raster_cambio,'E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_area_90_8.5.tif', overwrite=TRUE)

#tabla general
statsr30_4.5v<-t(statsr30_4.5$count)
statsr60_4.5v<-t(statsr60_4.5$count)
statsr90_4.5v<-t(statsr90_4.5$count)
statsr30_8.5v<-t(statsr30_8.5$count)
statsr60_8.5v<-t(statsr60_8.5$count)
statsr90_8.5v<-t(statsr90_8.5$count)


#' @return A list with the frequency stats for habitat gain or loss coded as
#'         follows:
#'         \itemize{
#'           \item{0 no habitat in neither two times}
#'           \item{1 habitat loss}
#'           \item{10 habitat gain}
#'           \item{11 preserved habitat}

cambios<-rbind.data.frame(statsr30_4.5v,statsr60_4.5v,statsr90_4.5v,statsr30_8.5v,statsr60_8.5v,statsr90_8.5v)
row.names(cambios)<-c("c_30_4.5","c_60_4.5","c_90_4.5","c_30_8.5","c_60_8.5","c_90_8.5")
colnames(cambios)<-c("0","1","10","11")

#porcentaje de cambio
cambios$total_pixeles<-rowSums(cambios,na.rm = TRUE)
head(cambios)
#porcentaje cambio
cambios$gain<-(cambios$"1"/(cambios$"1"+cambios$"10"))   
cambios$loss<-(cambios$"10"/(cambios$"1"+cambios$"10")) 
cambios$cambio_neto<-cambios$gain-cambios$loss
head(cambios)
write.csv(cambios,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/cambio_total")

#----------------------------------------------------------------------------
#' Function to compute the Minimum Volume covariance Matrix of an ellipsoid niche model.
#' @description Function to compute the covariance matrix, the niche centroid and volume of an
#' ellipsoid model. It uses the values of the niche variables of the ocurrences points.
#' @param data A data.frame or a matrix with the numeric values of the variables
#' that will be used to model the niche.
#' @param mve A logical value. If TRUE a minimum volume ellipsoid will be computed using
#' the function \code{\link[MASS]{cov.mve}} of the \pkg{MASS} package. If False the covariance matrix of the input data will be used.
#' @param level A numerical value specifying the proportion of the data to be
#' used to compute the ellipsoid.
#' @param vars A numeric or a string vector specifying the columns indexes/names of the variables of the input data which will be used to fit the ellipsoid model. If NULL the user will be asked to enter the indexes.
#' interactively
#' @return Returns a list containing the centroid of the ellipsoid, the covariance matrix based on
#' the input data, ellipsoid volume, semi-axis length and axis coordinates.
#' @export

cov_center <- function (data, mve = TRUE, level, vars = NULL)
{
  if (is.null(vars)) {
    nvars <- readline(prompt = "Number of variables to fit the ellipsoid model:\\n\\n")
    data <- data.frame(data)
    allvars <- names(data)
    print(nvars)
    vars <- numeric(nvars)
    cat("Select a variable form the list:\\n\\n")
    for (i in 1:dim(data)[2]) {
      cat(i, ".-", allvars[i], "\\n")
    }
    cont <- 1
    while (0 %in% vars) {
      n <- readline(prompt = "Enter an option from the above list: ")
      if (n %in% 1:dim(data)[2]) {
        vars[cont] <- as.numeric(n)
        cont <- cont + 1
      }
      else {
        cat("Option not in the list:\\n")
      }
    }
  }
  data <- data[, vars]
  if (mve) {
    
    NDquntil <- function(nD, level) {
      n <- floor(nD * level)
      if (n > nD)
        n <- nD
      return(n)
    }
    n <- NDquntil(dim(data)[1], level)
    cent_var <- MASS::cov.mve(data, quantile.used = n)
    centroid <- cent_var$center
    vari <- cent_var$cov
  }
  else {
    centroid <- colMeans(data)
    vari <- stats::cov(data)
  }
  # Ellipsoid volume
  ellip_vol <- function(n, axis_length) {
    term1 <- 2 * pi^(n/2)
    term2 <- n * gamma(n/2)
    term3 <- prod(axis_length)
    term4 <- (term1/term2) * term3
    return(term4)
  }
  
  sigmaI <- solve(vari)/stats::qchisq(level, df = dim(data)[2])
  sigIEigenSys <- eigen(sigmaI)
  sigIEval <- sigIEigenSys$values
  sigIEvec <- sigIEigenSys$vectors
  stds <- 1/sqrt(sigIEval)
  axis_length <- NULL
  for (i in 1:dim(sigmaI)[1]) {
    axis_length[i] <- stds[i] * 2
  }
  names(axis_length) <- letters[1:dim(vari)[1]]
  n <- dim(vari)[1]
  
  vol2 <- ellip_vol(n, axis_length/2)
  axis_coordinates <- list()
  for (i in 1:dim(vari)[1]) {
    assign(paste0("l", i, "_inf"),
           centroid - sigIEvec[,i] * stds[i])
    assign(paste0("l", i, "_sup"),
           centroid + sigIEvec[,i] * stds[i])
    coord_matrix <- matrix(c(eval(parse(text = paste0("l",
                                                      i, "_sup"))),
                             eval(parse(text = paste0("l", i, "_inf")))),
                           byrow = T, nrow = 2)
    colnames(coord_matrix) <- names(centroid)
    rownames(coord_matrix) <- paste0("vec_", 1:2)
    axis_coordinates[[i]] <- coord_matrix
  }
  return(list(centroid = centroid,
              covariance = vari,
              niche_volume = vol2,
              SemiAxis_length = axis_length/2,
              axis_coordinates = axis_coordinates))
}


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#selección variables
var_imp<-read.csv("E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/varImportance.csv")
var_imp_5<-subset(var_imp, percent.contribution>=3)
variable<-var_imp_5$variable

#selección de variables basados en la contribución a los modelos
var_select <-stack(bios[[variable]])
names<-names(var_select)
plot(var_select)

#puntos de M
Msp_p<-as.data.frame(rasterToPoints(Msp))
Msp_xy<-as.data.frame(Msp_p[,c("x", "y")])
Msp_xy_sub<-sample(nrow(Msp_xy), round(0.30*nrow(Msp_xy)))
Msp_xy_submuestra<-Msp_xy[Msp_xy_sub,]

#revisar proporción
pw<-nrow(Msp_xy)
pq<-nrow(Msp_xy_submuestra)
pq/pw

#variables seleccionadas en submuestra de M por especie
Msp_bios<-raster::extract(var_select, Msp_xy_submuestra)
head(Msp_bios)

#puntos presente
points_bios<-raster::extract(var_select, points_xy)
head(points_bios)
#nose puede graficar por el numero de variables (mas de 3)

#elipsiode M
p1_elips_mdata <- cov_center(data =Msp_bios,mve = TRUE,level = 0.9999, vars = names(var_select) )
p1_elipse <- rgl::ellipse3d(p1_elips_mdata$covariance,
                            centre = p1_elips_mdata$centroid)

#elipsiode sp
p2_elips_mdata <- cov_center(data =points_bios,mve = TRUE,level = 0.9999, vars = names(var_select)  )
p2_elipse <- rgl::ellipse3d(p2_elips_mdata$covariance,
                            centre = p2_elips_mdata$centroid)
#resultados
cat("El volumen del primer polígono (M) en E es:",p1_elips_mdata$niche_volume)
cat("El volumen del segundo polígono (Especie) en E es:",p2_elips_mdata$niche_volume)

#proporci?n de nicho ecol?gico
proporcion_nicho<-(p2_elips_mdata$niche_volume)/p1_elips_mdata$niche_volume
proporcion_nicho

vol<-cbind.data.frame(sp,p1_elips_mdata$niche_volume,p2_elips_mdata$niche_volume,proporcion_nicho, names)
colnames(vol)<-c("Especie","Msp","Modelo", "Proporción", "variables")
write.csv(vol,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/volumen_nicho.csv")

#plots
#par(mfrow=c(1,2))
#plot(Msp)
#plot(presente)

#--------------------------------------------------------------------------
#vegetaci?n
veg<-readOGR(dsn="E:/3.CONABIO-IBUNAM_CC_2/INEGI/usv731mgw.shp",layer="usv731mgw")
projection(veg) <- CRS("+proj=longlat")

#sobreponer los puntos por los poligonos con la capa de vegetaci?n
class(points)
coordinates(points) <- ~x+y
projection(points) <- CRS("+proj=longlat")

#seleccionar las provincias que contengan puntos
veg_points <- veg[!is.na(sp::over(veg, sp::geometry(points))), ]
re<-unique(veg_points$GRUPO_FINA)
veg_sp<-as.data.frame(re)
numero_veg<-nrow(veg_sp)
n_veg<-(cbind.data.frame(sp,numero_veg))

write.csv(veg_sp,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/veg.csv")
write.csv(n_veg,"E:/modelo_cluster/modelos_mamiferos/Chaetodipus_arenarius/indices/numeroveg.csv")
#---------------------------------------------------------------------------#---------------------------------------------------------------------------------------------
