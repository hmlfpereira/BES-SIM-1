### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figure S8 ----
### Create BD maps (intactness) per scenario, averaged over several models
### Project BES SIM 1
### Created December 2023, Luise Quoß
### Revised December 2023, Henrique Pereira
### Revised February 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(ebvcube)
library(stringr)
library(terra)
library(classInt)

#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root <- '../../Data_geo/ebv_cubes'
#directory with biodiversity model outputs as ebvcubes (they are downloaded below)

land_path <- '../../Data_geo/ne_110m_land/ne_110m_land.shp'
#this directory should contain the the land limits shapefile at 1:110m from https://www.naturalearthdata.com/downloads/110m-physical-vectors/

root_figures <- '../Figures/'
#directory where figure outputs will be saved

root_IPBES_Regions<-"../IPBES_Regions"
#directory with shapefile of IPBES regions from DataS4

root_outputs<-"../Outputs/"
#directory where other outputs and analysis will be saved

#download data
for (id in 27:28) {
  tryCatch({
    ebv_download(id, root)
  },error = function(e) {
    if(stringr::str_detect(as.character(e),'NetCDF already downloaded'))
      print(paste0('NetCDF with id ',id,' already downloaded'))
  })
}

#list all files
#list.files(root, '*.nc')
globio_path <- file.path(root, 'pereira_comcom_id27_20220405_v1.nc')
predicts_path <- file.path(root, 'pereira_comcom_id28_20231212_v2.nc')

### 2 - Import data ----
# ebv_datacubepaths(predicts_path)
predicts_1900 <- ebv_read(filepath = predicts_path,
                          datacubepath = 'scenario_1/metric_4/ebv_cube',
                          timestep = 1,
                          entity = 1, type='r')
predicts_2015 <- ebv_read(filepath = predicts_path,
                          datacubepath = 'scenario_1/metric_4/ebv_cube',
                          timestep = 2,
                          entity = 1, type='r')
predicts_2050_ssp1_lu <- ebv_read(filepath = predicts_path,
                                  datacubepath = 'scenario_1/metric_4/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')
predicts_2050_ssp3_lu <- ebv_read(filepath = predicts_path,
                                  datacubepath = 'scenario_2/metric_4/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')
predicts_2050_ssp5_lu <- ebv_read(filepath = predicts_path,
                          datacubepath = 'scenario_3/metric_4/ebv_cube',
                          timestep = 3,
                          entity = 1, type='r')

# ebv_datacubepaths(globio_path)
globio_1900 <- ebv_read(filepath = globio_path,
                          datacubepath = 'scenario_1/metric_1/ebv_cube',
                          timestep = 1,
                          entity = 1, type='r')
globio_2015 <- ebv_read(filepath = globio_path,
                          datacubepath = 'scenario_1/metric_1/ebv_cube',
                          timestep = 2,
                          entity = 1, type='r')
globio_2050_ssp1_lu <- ebv_read(filepath = globio_path,
                                datacubepath = 'scenario_1/metric_1/ebv_cube',
                                timestep = 3,
                                entity = 1, type='r')
globio_2050_ssp1_lucc <- ebv_read(filepath = globio_path,
                                  datacubepath = 'scenario_4/metric_1/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')
globio_2050_ssp3_lu <- ebv_read(filepath = globio_path,
                                datacubepath = 'scenario_2/metric_1/ebv_cube',
                                timestep = 3,
                                entity = 1, type='r')
globio_2050_ssp3_lucc <- ebv_read(filepath = globio_path,
                                  datacubepath = 'scenario_5/metric_1/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')
globio_2050_ssp5_lu <- ebv_read(filepath = globio_path,
                             datacubepath = 'scenario_3/metric_1/ebv_cube',
                             timestep = 3,
                             entity = 1, type='r')
globio_2050_ssp5_lucc <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_6/metric_1/ebv_cube',
                           timestep = 3,
                           entity = 1, type='r')

### 3 - Calculations ----
#calculate means
#stack the rasters
stack_1900 <- c(globio_1900, predicts_1900)
stack_2015 <- c(globio_2015, predicts_2015)
stack_2050_ssp1_lu <- c(globio_2050_ssp1_lu, predicts_2050_ssp1_lu)
stack_2050_ssp3_lu <- c(globio_2050_ssp3_lu, predicts_2050_ssp3_lu)
stack_2050_ssp5_lu <- c(globio_2050_ssp5_lu, predicts_2050_ssp5_lu)
#calculate mean
mean_1900 <- app(stack_1900, mean, na.rm=T)
mean_2015 <- app(stack_2015, mean, na.rm=T)
mean_2050_ssp1_lu <- app(stack_2050_ssp1_lu, mean, na.rm=T)
mean_2050_ssp3_lu <- app(stack_2050_ssp3_lu, mean, na.rm=T)
mean_2050_ssp5_lu <- app(stack_2050_ssp5_lu, mean, na.rm=T)

### 4 - Plots ----
#create colors
all <- c(as.array(mean_1900), as.array(mean_2015), 
         as.array(mean_2050_ssp1_lu), as.array(mean_2050_ssp3_lu),
         as.array(mean_2050_ssp5_lu), as.array(globio_2050_ssp1_lucc),
         as.array(globio_2050_ssp3_lucc), as.array(globio_2050_ssp5_lucc))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
rm(all)
br <- c(q[1], q[4],q[8], q[12], q[16],q[21])
br_at <- c(0.5, 3.5, 7.5, 11.5, 15.5, 20.5)

co1 <- colorRampPalette(c("#9b4a14",'#FDE6BE'))(10) 
co2 <- colorRampPalette(c('lemonchiffon1', "#0073a0"))(10)
co <- c(co1,co2)

#legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:20, y=1, matrix(1:20), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,3), cex.axis=1.5)
  box()
  title(main = 'Dimensionless score between 0 and 1', font.main = 1, cex.main=1.5, adj=0)
}

dev.copy(pdf,file.path(root_figures, 'FigureS8_legend.pdf'), 
          width=17,height=2.55)
dev.off()

#split screen----
split.screen(c(1, 2))       # split display into two columns
split.screen(c(4, 1), screen = 1) # now split the left one into 4
split.screen(c(4, 1), screen = 2) # now split the right one into 4

#map 1900
screen(3)
plot(
  mean_1900,
  breaks = q,
  main = '(a) Historical (1900)',
  col = co,
  legend = F,
  axes= F
)

#map 2015
screen(7)
plot(
  mean_2015,
  breaks = q,
  main = '(b) Historical (2015)',
  col = co,
  legend = F,
  axes= F
)

#map 2050 SSP1
screen(4)
plot(
  mean_2050_ssp1_lu,
  breaks = q,
  main = '(c) Global sustainability - LU (2050)',
  col = co,
  legend = F,
  axes= F
)

screen(8)
plot(
  globio_2050_ssp1_lucc,
  breaks = q,
  main = '(d) Global sustainability - LUCC (2050)',
  col = co,
  legend = F,
  axes= F
)

#map 2050 SSP3
screen(5)

plot(
  mean_2050_ssp3_lu,
  breaks = q,
  main = '(e) Regional rivalry - LU (2050)',
  col = co,
  legend = F,
  axes= F
)

screen(9)
plot(
  globio_2050_ssp3_lucc,
  breaks = q,
  main = '(f) Regional rivalry - LUCC (2050)',
  col = co,
  legend = F,
  axes= F
)

#map 2050 SSP5
screen(6)
plot(
  mean_2050_ssp5_lu,
  breaks = q,
  main = '(g) Fossil-fueled develop. - LU (2050)',
  col = co,
  legend = F,
  axes= F
)

screen(10)
plot(
  globio_2050_ssp5_lucc,
  breaks = q,
  main = '(h) Fossil-fueled develop. - LUCC (2050)',
  col = co,
  legend = F,
  axes= F
)

dev.copy(pdf,file.path(root_figures, 'FigureS8_intactness.pdf'),
         width=13,height=13)
dev.off()

#close all screens
close.screen(all = TRUE)

### 5 - Analysis ----
areaweight_zonal_mean<-function(raster1, regions)
{
  #produce a raster that has the area of each degree cell
  a<-cellSize(raster1)
  a <- mask(a,raster1)
  #calculate area weighted mean
  asum<-zonal(a, regions, fun="sum", na.rm=TRUE)
  w<-a
  #plot(raster1*w)
  areaw_mean<-zonal(raster1*w, regions, "sum", na.rm=TRUE)
  return(areaw_mean/asum)
}

regions<- vect(file.path(root_IPBES_Regions,"IPBES_subregions_simp_v6.shp"))
intersect <- lapply(c(mean_1900,mean_2015,mean_2050_ssp1_lu,mean_2050_ssp3_lu,mean_2050_ssp5_lu,globio_2050_ssp1_lucc,globio_2050_ssp3_lucc,globio_2050_ssp5_lucc),
                    function(x) {areaweight_zonal_mean(x,regions)}) 

df<-data.frame(as.data.frame(regions)$IPBES_sub,intersect)
colnames(df)<-c("Region","1900","2015","2050_ssp1_lu","2050_ssp3_lu","2050_ssp5_lu","2050_ssp1_lucc","2050_ssp3_lucc","2050_ssp5_lucc")
write.csv2(df,paste0(root_outputs,"RegionalIntactness.csv"), row.names = FALSE)
