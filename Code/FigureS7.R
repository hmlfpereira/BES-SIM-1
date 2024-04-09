### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figures S7 ----
### Create BE maps (local species richness) per model for the regional rivalry scenario
### Project BES SIM 1
### Created October 2023, Luise Quoß
### Revised November 2023, Henrique Pereira
### Revised February 2024, Luise Quoß

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

#download data----
predicts_path <- ebv_download('10.25829/vt7qk9', root)
iiasa_path <- ebv_download('10.25829/haq7d4', root)
csar_path <- ebv_download('10.25829/5zmy41', root)
aim_path <- ebv_download('10.25829/5wn357', root)
insight_path <- ebv_download('10.25829/h2evr2', root)

### 2 - Import data ----
# ebv_datacubepaths(predicts_path)
predicts <- ebv_read(filepath = predicts_path,
                          datacubepath = 'scenario_2/metric_3/ebv_cube',
                          timestep = 3,
                          entity = 1, type='r')
# ebv_datacubepaths(iiasa_path)
iiasa <- ebv_read(filepath = iiasa_path,
                     datacubepath = 'scenario_2/metric_3/ebv_cube',
                     timestep = 3,
                     entity = 1, type='r')
# ebv_datacubepaths(csar_path)
csar <- ebv_read(filepath = csar_path,
                  datacubepath = 'scenario_2/metric_3/ebv_cube',
                  timestep = 3,
                  entity = 1, type='r')
# ebv_datacubepaths(aim_path)
aim <- ebv_read(filepath = aim_path,
                  datacubepath = 'scenario_2/metric_3/ebv_cube',
                  timestep = 3,
                  entity = 2, type='r')
# ebv_datacubepaths(insight_path)
insight <- ebv_read(filepath = insight_path,
                datacubepath = 'scenario_2/metric_3/ebv_cube',
                timestep = 3,
                entity = 1, type='r')

### 3 - Calculations ----
aim_ext <- terra::extend(aim,predicts)
predicts_yrl <-  (((1+(predicts/100))^(1/3.5))-1)*100
iiasa_yrl <- (((1+(iiasa/100))^(1/3.5))-1)*100  
csar_yrl <-(((1+(csar/100))^(1/3.5))-1)*100    
aim_yrl <- (((1+(aim_ext/100))^(1/3.5))-1)*100
insight_yrl <- (((1+(insight/100))^(1/3.5))-1)*100 
stack <- c(predicts_yrl, iiasa_yrl, csar_yrl, aim_yrl,insight_yrl)
model_mean <- app(stack, mean, na.rm=T)

### 4 - Plots ----
#prepare background
land_vec <- terra::vect(land_path)
mask <- terra::rasterize(land_vec, predicts)
mask <- terra::crop(mask, ext(c(-180,180,-58,90)))

#create colors
all <- c(as.array(predicts_yrl), as.array(iiasa_yrl), as.array(insight_yrl),
         as.array(csar_yrl), as.array(aim_yrl), as.array(model_mean))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)

br <- c(q[1], q[3], q[5], q[7], q[10],q[12],q[14], q[16])
br_at <- c(0.5, 2.5, 4.5, 6.5, 9.5, 11.5, 13.5, 15.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(9)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(6)
co <- c(co1,co2)

#legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2), cex.axis=1.5)
  box()
  title(main = expression("% spp. decade"^{-1}), font.main = 1, cex.main=1.5, adj=0) #
}

dev.copy(pdf,file.path(root_figures, 'FigureS7_legend_acc_decade.pdf'),
         width=20,height=3)
dev.off()

#split screen
split.screen(c(3, 1))       # split display into two screens
split.screen(c(1, 2), screen = 1) # now split the middle one into 3
split.screen(c(1, 2), screen = 2) # now split the bottom one into 3
split.screen(c(1, 2), screen = 3) # now split the bottom one into 3

#### a) map csar ----
screen(4)
plot(mask, legend = F,
     main = '(a) cSAR-iDiv',
     axes= F,
     col='grey93')

plot(
  csar_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#### b) map iiasa ----
screen(5)
plot(mask, legend = F,
     main = '(b) cSAR-IIASA-ETH',
     axes= F,
     col='grey93')

plot(
  iiasa_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#### c) map insights ----
screen(6)
plot(mask, legend = F,
     main = '(c) InSIGHTS',
     axes= F,
     col='grey93')

plot(
  insight_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#### d) map aim ----
screen(7)
plot(mask, legend = F,
     main = '(d) AIM',
     axes= F,
     col='grey93')

plot(
  aim_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#### e) map predicts ----
screen(8)
plot(mask, legend = F,
     main = '(e) PREDICTS',
     axes= F,
     col='grey93')

plot(
  predicts_yrl,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#### f) map intermodel mean ----
screen(9)
plot(mask, legend = F,
     main = '(f) Intermodel mean',
     axes= F,
     col='grey93')

plot(
  model_mean,
  breaks = q,
  add = T,
  col = co,
  legend = F,
  axes= F
)

#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS7_BiodivModelAgreement.pdf'),
         width=13,height=10)
dev.off()

#close all screens
close.screen(all = TRUE)
