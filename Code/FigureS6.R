### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science https://doi.org/science.adn3441
### Figure S6 ----
### Create S6a: Global historical trends (1990-2015) in mean annual temperature and for each scenario (2015-2050)
### Create S6b-e: Spatial distribution maps of absolute changes in mean annual temperature in each scenario (2015-2050)
### Created Henrique Pereira, Nov 2019
### Revised Luise Quoß, Dez 2023

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf4)

#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

#set paths
root <- '../../Data_geo/Climate_data' 
#this directory should contain the the ISIMIP2a IPSL Climate Data 
#the data can be download from Dryad, doi: https://doi.org/10.5061/dryad.3n5tb2rr6

root_figures <- '../Figures/' 

### 2 - Exploring a NetCDF file ----
# open a NetCDF file
ncname <- "tas_bced_1960_1999_ipsl-cm5a-lr_hist_rcp2p6_1901-2099_noleap_monmean"
ncfname <- paste(ncname, ".nc", sep = "")
ncin <- nc_open(file.path(root,ncfname))
print(ncin)

lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)
lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)

t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
longt<-chron(t, origin = c(tmonth, tday, tyear), out.format=c("day mon year",FALSE))

dname <- "tasAdjust"  # note: tmp means temperature (not temporary)
tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(tmp.array)

nc_close(ncin)
#close the NetCDF file

lat <- rev(lat)

#creating a matrix of the areas for the cells at different latitudes 
area<-read.csv("FigureS6_halfdegree_area_qkm.csv",header=FALSE)
area<-area[,1]
area<-matrix(rep(area,720),720,360,byrow=TRUE)

### 3 - Calculations and Plots ----
#calculates a temperature array with average values across the 12 months for yr
meanyear<-function(yr,tmp.array)
{
  t0<-1+12*(yr-1901)
  tmp.year <- tmp.array[, , t0]
  for (m in 1:11)
    tmp.year <- tmp.year + tmp.array[, , t0+m]
  tmp.year<-tmp.year/12;
  return (tmp.year-273.15)
}

#calculates a temperature array with average values for a 20-years centred in yr
meandecade<-function(yr,tmp.year.array)
{
  t0 <- yr-1900
  tmp.decade <- tmp.year.array[, , t0-9]
  for (m in -8:10)
    tmp.decade <- tmp.decade + tmp.year.array[, , t0+m]
  tmp.decade<-tmp.decade/20;
  return (tmp.decade)
}

years<-1901:2099
tmp.year.array1<-array(dim=c(nlon,nlat,length(years)))
tmp.year.mean1<-matrix(length(years))
for (t in 1:length(years))
{
  tmp.year.array1[,,t]<-meanyear(years[t],tmp.array)
  tmp.year.mean1[t]<-weighted.mean(tmp.year.array1[,,t],area,na.rm=TRUE)
}


plot(years,tmp.year.mean1,type="l")

# map of temperatures in 1901
tmp.slice <- tmp.year.array1[,,1]
tmp.slice <- tmp.slice[,ncol(tmp.slice):1]
#image(lon, lat, tmp.slice, col = rev(brewer.pal(10, "RdBu")))
grid <- expand.grid(lon = lon, lat = lat)
cutpts <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))


decades<-c(1910,1935,1960,1990,2015,2050)
tmp.decade.array1<-array(dim=c(nlon,nlat,length(decades)))
tmp.decade.mean1<-matrix(length(decades))
for (t in 1:length(decades))
{
  tmp.decade.array1[,,t]<-meandecade(decades[t],tmp.year.array1)
  tmp.decade.mean1[t]<-weighted.mean(tmp.decade.array1[,,t],area,na.rm=TRUE)
}  

plot(years[1:160],tmp.year.mean1[1:160])
lines(decades,tmp.decade.mean1,col="red")

#### 3a - SSP2.6 ----
# map of delta temperatures between 2050 and 2015

tmp.slice <- tmp.decade.array1[,,6]- tmp.decade.array1[,,5]
tmp.slice <- tmp.slice[,ncol(tmp.slice):1]
histogram(as.vector(tmp.slice),na.rm=TRUE)
image(lon, lat, tmp.slice, col = rev(brewer.pal(10, "RdBu")))
grid <- expand.grid(lon = lon, lat = lat)
min(tmp.slice,na.rm=TRUE)
max(tmp.slice,na.rm=TRUE)
cutpts <- seq (-3.0,4.5,0.75)
pdf(file.path(root_figures,"FigureS6b_TemperatureSSP2p6.pdf"))
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = length(cutpts), pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))),asp=.5,colorkey = FALSE)
dev.off()

#tmp.vec.long <- as.vector(tmp.array)
#length(tmp.vec.long)
#tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
#dim(tmp.mat)

#### 3b - SSP6.0 ----
ncname <- "tas_bced_1960_1999_ipsl-cm5a-lr_hist_rcp6p0_1901-2099_noleap_monmean"
ncfname <- paste(ncname, ".nc", sep = "")
ncin <- nc_open(file.path(root,ncfname))
tmp.array2 <- ncvar_get(ncin, dname)
nc_close(ncin)

tmp.year.array2<-array(dim=c(nlon,nlat,length(years)))
tmp.year.mean2<-matrix(length(years))
for (t in 1:length(years))
{
  tmp.year.array2[,,t]<-meanyear(years[t],tmp.array2)
  tmp.year.mean2[t]<-weighted.mean(tmp.year.array2[,,t],area,na.rm=TRUE)
}

#calculates a temperature array with average values for a 20-years centred in yr
tmp.decade.array2<-array(dim=c(nlon,nlat,length(decades)))
tmp.decade.mean2<-matrix(length(decades))
for (t in 1:length(decades))
{
  tmp.decade.array2[,,t]<-meandecade(decades[t],tmp.year.array2)
  tmp.decade.mean2[t]<-weighted.mean(tmp.decade.array2[,,t],area,na.rm=TRUE)
}  

plot(years[1:160],tmp.year.mean2[1:160])
lines(decades,tmp.decade.mean2,col="red")

# map of delta temperatures between 2050 and 2015
pdf(file.path(root_figures,"FigureS6c_TemperatureSSP6p0.pdf"))
tmp.slice <- tmp.decade.array2[,,6]- tmp.decade.array2[,,5]
tmp.slice <- tmp.slice[,ncol(tmp.slice):1]
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = length(cutpts), pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))), colorkey=FALSE, asp=.5)
dev.off()

#### 3c - SSP8.5 ----
ncname <- "tas_bced_1960_1999_ipsl-cm5a-lr_hist_rcp8p5_1901-2099_noleap_monmean"
ncfname <- paste(ncname, ".nc", sep = "")
ncin <- nc_open(file.path(root,ncfname))
tmp.array3 <- ncvar_get(ncin, dname)
nc_close(ncin)

tmp.year.array3<-array(dim=c(nlon,nlat,length(years)))
tmp.year.mean3<-matrix(length(years))
for (t in 1:length(years))
{
  tmp.year.array3[,,t]<-meanyear(years[t],tmp.array3)
  tmp.year.mean3[t]<-weighted.mean(tmp.year.array3[,,t],area,na.rm=TRUE)
}

#calculates a temperature array with average values for a 20-years centred in yr
tmp.decade.array3<-array(dim=c(nlon,nlat,length(decades)))
tmp.decade.mean3<-matrix(length(decades))
for (t in 1:length(decades))
{
  tmp.decade.array3[,,t]<-meandecade(decades[t],tmp.year.array3)
  tmp.decade.mean3[t]<-weighted.mean(tmp.decade.array3[,,t],area,na.rm=TRUE)
}  

plot(years[1:160],tmp.year.mean3[1:160])
lines(decades,tmp.decade.mean3,col="red")

# map of delta temperatures between 2050 and 2015
pdf(file.path(root_figures,"FigureS6d_TemperatureSSP8p5.pdf"))
tmp.slice <- tmp.decade.array3[,,6]- tmp.decade.array2[,,5]
tmp.slice <- tmp.slice[,ncol(tmp.slice):1]
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = length(cutpts), pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))), colorkey=FALSE, asp=.5)

dev.off()

pdf(file.path(root_figures,"FigureS6d_legend.pdf"))
tmp.slice <- tmp.decade.array3[,,6]- tmp.decade.array2[,,5]
tmp.slice <- tmp.slice[,ncol(tmp.slice):1]
levelplot(tmp.slice ~ lon * lat, data = grid, at = cutpts, cuts = length(cutpts), pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))), colorkey=list(space="bottom",title="Change in mean annual temperature (ºC)"), asp=.5)

dev.off()

#### 3d - Temporal trend plot ----
pdf(file.path(root_figures,"FigureS6a_TemperatureTrends.pdf"))

plot(years[1:160],tmp.year.mean3[1:160],xlab="Year", ylab="Mean annual temperature over land (ºC)")
lines(years[114:160],tmp.year.mean3[114:160],col="red",type="p")
lines(years[114:160],tmp.year.mean3[114:160],col="red",type="p")
lines(years[114:160],tmp.year.mean2[114:160],col="blue",type="p")
lines(years[114:160],tmp.year.mean1[114:160],col="green",type="p")
lines(decades,tmp.decade.mean3)
lines(decades[5:6],tmp.decade.mean3[5:6],col="red")
lines(decades[5:6],tmp.decade.mean2[5:6],col="blue")
lines(decades[5:6],tmp.decade.mean1[5:6],col="green")
legend(1900, 17, legend=c("RCP 2.6", "RCP 6.0", "RCP 8.5"),
       col=c("green", "blue", "red"), lty=1, cex=0.8)
text(1880,17.6,"(a)",xpd=NA,font=2)
dev.off()

lines(years,tmp.year.mean2,col="red",type="p")
lines(years,tmp.year.mean1,col="blue",type="p")

