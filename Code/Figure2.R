### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science https://doi.org/10.1126/science.adn3441
### Figure 2 ----
### Create BD maps (species richness) per scenario, averaged over several models
### Project BES SIM 1
### Created Oct 2023, Luise Quoß
### Revised Nov 2023, Henrique Pereira
### Revised Feb 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(ebvcube)
library(stringr)
library(terra)
library(classInt)

#set paths

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
globio_path <- ebv_download('10.25829/r7bt92', root)
predicts_path <- ebv_download('10.25829/vt7qk9', root)
iiasa_path <- ebv_download('10.25829/haq7d4', root)
csar_path <- ebv_download('10.25829/5zmy41', root)
aim_path <- ebv_download('10.25829/5wn357', root)
insight_path <- ebv_download('10.25829/h2evr2', root)

### 2 - Import data ----
# ebv_datacubepaths(predicts_path)
predicts_hist <- ebv_read(filepath = predicts_path,
                        datacubepath = 'scenario_1/metric_3/ebv_cube',
                        timestep = 2,
                        entity = 1, type='r')
predicts_2050_ssp1_lu <- ebv_read(filepath = predicts_path,
                          datacubepath = 'scenario_1/metric_3/ebv_cube',
                          timestep = 3,
                          entity = 1, type='r')
predicts_2050_ssp2_lu <- ebv_read(filepath = predicts_path,
                                  datacubepath = 'scenario_2/metric_3/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')

predicts_2050_ssp3_lu <- ebv_read(filepath = predicts_path,
                                  datacubepath = 'scenario_3/metric_3/ebv_cube',
                                  timestep = 3,
                                  entity = 1, type='r')


# ebv_datacubepaths(iiasa_path)
iiasa_hist <- ebv_read(filepath = iiasa_path,
                          datacubepath = 'scenario_1/metric_3/ebv_cube',
                          timestep = 2,
                          entity = 1, type='r')
iiasa_2050_ssp1_lu <- ebv_read(filepath = iiasa_path,
                          datacubepath = 'scenario_1/metric_3/ebv_cube',
                          timestep = 3,
                          entity = 1, type='r')
iiasa_2050_ssp2_lu <- ebv_read(filepath = iiasa_path,
                               datacubepath = 'scenario_2/metric_3/ebv_cube',
                               timestep = 3,
                               entity = 1, type='r')
iiasa_2050_ssp3_lu <- ebv_read(filepath = iiasa_path,
                               datacubepath = 'scenario_3/metric_3/ebv_cube',
                               timestep = 3,
                               entity = 1, type='r')

# ebv_datacubepaths(csar_path)
csar_hist <- ebv_read(filepath = csar_path,
                       datacubepath = 'scenario_1/metric_3/ebv_cube',
                       timestep = 2,
                      entity = 1, type='r')
csar_2050_ssp1_lu <- ebv_read(filepath = csar_path,
                               datacubepath = 'scenario_1/metric_3/ebv_cube',
                               timestep = 3,
                               entity = 1, type='r')
csar_2050_ssp2_lu <- ebv_read(filepath = csar_path,
                              datacubepath = 'scenario_2/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 1, type='r')
csar_2050_ssp3_lu <- ebv_read(filepath = csar_path,
                              datacubepath = 'scenario_3/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 1, type='r')
# ebv_datacubepaths(aim_path)
aim_hist <- ebv_read(filepath = aim_path,
                      datacubepath = 'scenario_1/metric_3/ebv_cube',
                      timestep = 2,
                      entity = 2, type='r')
aim_2050_ssp1_lu <- ebv_read(filepath = aim_path,
                              datacubepath = 'scenario_1/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 2, type='r')
aim_2050_ssp2_lu <- ebv_read(filepath = aim_path,
                              datacubepath = 'scenario_2/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 2, type='r')
aim_2050_ssp3_lu <- ebv_read(filepath = aim_path,
                              datacubepath = 'scenario_3/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 2, type='r')
aim_2050_ssp1_lucc <- ebv_read(filepath = aim_path,
                             datacubepath = 'scenario_4/metric_3/ebv_cube',
                             timestep = 3,
                             entity = 2, type='r')
aim_2050_ssp2_lucc <- ebv_read(filepath = aim_path,
                             datacubepath = 'scenario_5/metric_3/ebv_cube',
                             timestep = 3,
                             entity = 2, type='r')
aim_2050_ssp3_lucc <- ebv_read(filepath = aim_path,
                             datacubepath = 'scenario_6/metric_3/ebv_cube',
                             timestep = 3,
                             entity = 2, type='r')

# ebv_datacubepaths(insight_path)
insight_hist <- ebv_read(filepath = insight_path,
                      datacubepath = 'scenario_1/metric_3/ebv_cube',
                      timestep = 2,
                      entity = 1, type='r')
insight_2050_ssp1_lu <- ebv_read(filepath = insight_path,
                              datacubepath = 'scenario_1/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 1, type='r')
insight_2050_ssp2_lu <- ebv_read(filepath = insight_path,
                              datacubepath = 'scenario_2/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 1, type='r')
insight_2050_ssp3_lu <- ebv_read(filepath = insight_path,
                              datacubepath = 'scenario_3/metric_3/ebv_cube',
                              timestep = 3,
                              entity = 1, type='r')
insight_2050_ssp1_lucc <- ebv_read(filepath = insight_path,
                                 datacubepath = 'scenario_4/metric_3/ebv_cube',
                                 timestep = 3,
                                 entity = 1, type='r')
insight_2050_ssp2_lucc <- ebv_read(filepath = insight_path,
                                 datacubepath = 'scenario_5/metric_3/ebv_cube',
                                 timestep = 3,
                                 entity = 1, type='r')
insight_2050_ssp3_lucc <- ebv_read(filepath = insight_path,
                                 datacubepath = 'scenario_6/metric_3/ebv_cube',
                                 timestep = 3,
                                 entity = 1, type='r')

#extend aim results
aim_hist <- terra::extend(aim_hist,predicts_hist)
aim_2050_ssp1_lu <- terra::extend(aim_2050_ssp1_lu,predicts_hist)
aim_2050_ssp2_lu <- terra::extend(aim_2050_ssp2_lu,predicts_hist)
aim_2050_ssp3_lu <- terra::extend(aim_2050_ssp3_lu,predicts_hist)
aim_2050_ssp1_lucc <- terra::extend(aim_2050_ssp1_lucc,predicts_hist)
aim_2050_ssp2_lucc <- terra::extend(aim_2050_ssp2_lucc,predicts_hist)
aim_2050_ssp3_lucc <- terra::extend(aim_2050_ssp3_lucc,predicts_hist)

### 3 - Calculations ----
#historical
#stack the rasters
hist_stack <- c(predicts_hist, iiasa_hist, insight_hist, aim_hist, csar_hist) #
#calculate mean
hist_mean <- app(hist_stack, mean, na.rm=T)

#ssp1 lu
#stack the rasters
ssp1_lu_stack <- c(predicts_2050_ssp1_lu, aim_2050_ssp1_lu, 
                   iiasa_2050_ssp1_lu, csar_2050_ssp1_lu,
                   insight_2050_ssp1_lu)
#calculate mean
ssp1_lu_mean <- app(ssp1_lu_stack, mean, na.rm=T)


#ssp2 lu
#stack the rasters
ssp2_lu_stack <- c(predicts_2050_ssp2_lu, aim_2050_ssp2_lu, 
                   iiasa_2050_ssp2_lu, csar_2050_ssp2_lu,
                   insight_2050_ssp2_lu)
#calculate mean
ssp2_lu_mean <- app(ssp2_lu_stack, mean, na.rm=T)

#ssp3 lu
#stack the rasters
ssp3_lu_stack <- c(predicts_2050_ssp3_lu, aim_2050_ssp3_lu, 
                   iiasa_2050_ssp3_lu, csar_2050_ssp3_lu,
                   insight_2050_ssp3_lu)
#calculate mean
ssp3_lu_mean <- app(ssp3_lu_stack, mean, na.rm=T)


#ssp1 lucc
#stack the rasters
ssp1_lucc_stack <- c(aim_2050_ssp1_lucc, insight_2050_ssp1_lucc)
#calculate mean
ssp1_lucc_mean <- app(ssp1_lucc_stack, mean, na.rm=T)


#ssp2 lucc
#stack the rasters
ssp2_lucc_stack <- c(aim_2050_ssp2_lucc, insight_2050_ssp2_lucc)
#calculate mean
ssp2_lucc_mean <- app(ssp2_lucc_stack, mean, na.rm=T)

#ssp3 lucc
#stack the rasters
ssp3_lucc_stack <- c(aim_2050_ssp3_lucc, insight_2050_ssp3_lucc)
#calculate mean
ssp3_lucc_mean <- app(ssp3_lucc_stack, mean, na.rm=T)

### 4 - Plots ----
#prepare background
land_vec <- terra::vect(land_path)
mask <- terra::rasterize(land_vec, predicts_hist)

#calculate mean per decade
hist_mean_yrl <- (((1+(hist_mean/100))^(1/11.5))-1)*100
ssp1_lu_mean_yrl <-(((1+(ssp1_lu_mean/100))^(1/3.5))-1)*100  
ssp2_lu_mean_yrl <- (((1+(ssp2_lu_mean/100))^(1/3.5))-1)*100 
ssp3_lu_mean_yrl <- (((1+(ssp3_lu_mean/100))^(1/3.5))-1)*100 
ssp1_lucc_mean_yrl <-(((1+(ssp1_lucc_mean/100))^(1/3.5))-1)*100 
ssp2_lucc_mean_yrl <-(((1+(ssp2_lucc_mean/100))^(1/3.5))-1)*100
ssp3_lucc_mean_yrl <-(((1+(ssp3_lucc_mean/100))^(1/3.5))-1)*100   

#a) plot hist + legend ----
all <- c(as.array(hist_mean_yrl), as.array(ssp1_lu_mean_yrl), 
         as.array(ssp2_lu_mean_yrl), as.array(ssp3_lu_mean_yrl))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)

br <- c(q[1], q[4],q[6], q[9], q[11], q[14], q[16], q[18], q[20])
br_at <- c(0.5, 3.5, 5.5, 8.5, 10.5, 13.5, 15.5, 17.5, 19.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(13)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(6)
co <- c(co1,co2)

# #legend - uncomment to plot legend
# {
#   par(mfrow = c(1,1))
#   graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
#   axis(1, at= br_at, labels=round(br,2), cex.axis=1.5)
#   box()
#   title(main = expression("% spp. decade"^{-1}), font.main = 1, cex.main=2, adj=0)
# }
# 
# dev.copy(pdf,file.path(root_figures, 'figure2_legend_hist_acc_decade.pdf'),
#          width=20,height=2.9)
# dev.off()

#split screen
split.screen(c(3, 1))       # split display into two screens
split.screen(c(1, 3), screen = 2) # now split the middle one into 3
split.screen(c(1, 3), screen = 3) # now split the bottom one into 3

#map
screen(1)
plot(mask, legend = F,
     main = '(a) Historical',
     axes= F,
     col='grey93')

plot(
  hist_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)


#b) plot ssp1 lu ----
screen(4)
plot(mask, legend = F,
     main = '(b) Global Sustainability - LU',
     axes= F,
     col='grey93')

plot(
  ssp1_lu_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)


#c) plot ssp2 lu ----
screen(5)
plot(mask, legend = F,
     main = '(c) Regional rivalry - LU',
     axes= F,
     col='grey93')

plot(
  ssp2_lu_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#d) plot ssp3 lu ----
screen(6)
plot(mask, legend = F,
     main = '(d) Fossil-fueled develop. - LU',
     axes= F,
     col='grey93')

plot(
  ssp3_lu_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

# e) plot ssp1 lucc + new legend----
all <- c(as.array(ssp1_lucc_mean_yrl), 
         as.array(ssp2_lucc_mean_yrl), as.array(ssp3_lucc_mean_yrl))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)
q <- c(q[1:20], 0, q[21])

br <- c(q[1],   q[2], q[5], q[7],  q[9], q[11],
        q[15],q[18], q[21], q[22])
br_at <- c(0.5, 1.5, 4.5, 6.5, 8.5, 10.5,
           14.5, 17.5, 20.5, 21.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(20)
co <- c(co1,'lemonchiffon1')


# #legend - uncomment to plot legend
# {
#   par(mfrow = c(1,1))
#   graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
#   axis(1, at= br_at, labels=round(br,2), cex.axis=1.5)
#   box()
#   title(main = expression("% spp. decade"^{-1}), font.main = 1, cex.main=2, adj=0)
# }
# 
# dev.copy(pdf,file.path(root_figures, 'figure2_legend_lucc_acc_decade.pdf'),
#          width=20,height=2.8)
# dev.off()

screen(7)
plot(mask, legend = F,
     main = '(e) Global Sustainability - LUCC',
     axes= F,
     col='grey93')

plot(
  ssp1_lucc_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#c) plot ssp2 lucc ----
screen(8)
plot(mask, legend = F,
     main = '(f) Regional rivalry - LUCC',
     axes= F,
     col='grey93')

plot(
  ssp2_lucc_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#d) plot ssp3 lucc ----
screen(9)
plot(mask, legend = F,
     main = '(g) Fossil-fueled develop. - LUCC',
     axes= F,
     col='grey93')

plot(
  ssp3_lucc_mean_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#save as file
dev.copy(pdf,file.path(root_figures, 'Figure2_BiodiversityTrendsMaps.pdf'),
         width=16,height=10)
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
intersect <- lapply(c(hist_mean,ssp1_lu_mean,ssp2_lu_mean,ssp3_lu_mean), 
                    function(x) {areaweight_zonal_mean(x,regions)}) 
df<-data.frame(as.data.frame(regions)$IPBES_sub,intersect)
colnames(df)<-c("Region","Hist","SSP1", "SSP3", "SSP5")
write.csv2(df,paste0(root_outputs,"RegionalDeltaSS_lu_total.csv"))

intersect <- lapply(c(hist_mean_yrl,ssp1_lu_mean_yrl,ssp2_lu_mean_yrl,ssp3_lu_mean_yrl), 
                    function(x) {areaweight_zonal_mean(x,regions)}) 
df<-data.frame(as.data.frame(regions)$IPBES_sub,intersect)
colnames(df)<-c("Region","Hist","SSP1", "SSP3", "SSP5")
write.csv2(df,paste0(root_outputs,"RegionalDeltaSS_lu_decadal.csv"))

intersect <- lapply(c(hist_mean,ssp1_lucc_mean,ssp2_lucc_mean,ssp3_lucc_mean), 
                    function(x) {areaweight_zonal_mean(x,regions)}) 
df<-data.frame(as.data.frame(regions)$IPBES_sub,intersect)
colnames(df)<-c("Region","Hist","SSP1", "SSP3", "SSP5")
write.csv2(df,paste0(root_outputs,"RegionalDeltaSS_lucc_total.csv"))
