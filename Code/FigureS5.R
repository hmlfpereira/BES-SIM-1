### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science https://doi.org/10.1126/science.adn3441
### Figure S5 ----
### Create distribution maps of pasture and rangeland in 1900, historical changes (1900-2015) and future changes (2015-2050) in each scenario
### Project BES SIM 1
### Created October 2023, Luise Quoß
### Revised Feb 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(ebvcube)
library(stringr)
library(terra)

##setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

#set paths
root_land <- '../../Data_geo/ne_110m_land' 
#this directory should contain the the land limits shapefile at 1:110m from https://www.naturalearthdata.com/downloads/110m-physical-vectors/

root <- '../../Data_geo/LUH2' 
#this directory should contain the the LUH2 files downloaded from https://luh.umd.edu/data.shtml
#Data used for the historical maps: LUH2 v2h Release (10/14/16)
#Data used for the scenario maps: LUH2 v2f Release (12/21/17)
#list of files
#multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc
#multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc
#multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc
#states.nc

root_figures <- '../Figures/' 
#directory where figure outputs will be saved

root_outputs <- '../Outputs/'
#directory to store other output files that can be re-used

### 2 - Import data ----
hist_path <- file.path(root, 'states.nc')
ssp1_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc')
ssp3_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc')
ssp5_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc')

data_1900 <- (terra::rast(hist_path, 'range')[[1051]] + terra::rast(hist_path, 'pastr')[[1051]])*100  
data_2015 <- (terra::rast(hist_path, 'range')[[1166]] + terra::rast(hist_path, 'pastr')[[1166]])*100  
data_2015_ssp1 <- (terra::rast(ssp1_path, 'range')[[1]] + terra::rast(ssp1_path, 'pastr')[[1]])*100
data_2050_ssp1 <- (terra::rast(ssp1_path, 'range')[[36]] + terra::rast(ssp1_path, 'pastr')[[36]])*100
data_2015_ssp3 <- (terra::rast(ssp3_path, 'range')[[1]] + terra::rast(ssp3_path, 'pastr')[[1]])*100
data_2050_ssp3 <- (terra::rast(ssp3_path, 'range')[[36]] + terra::rast(ssp3_path, 'pastr')[[36]])*100
data_2015_ssp5 <- (terra::rast(ssp5_path, 'range')[[1]] + terra::rast(ssp5_path, 'pastr')[[1]])*100
data_2050_ssp5 <- (terra::rast(ssp5_path, 'range')[[36]] + terra::rast(ssp5_path, 'pastr')[[36]])*100

### 3 - Calculations ----
# calculate change data
change_hist <- data_2015-data_1900#(data_2015-data_1900)/data_1900*100
change_2050_ssp1 <- data_2050_ssp1-data_2015_ssp1#(data_2050_ssp1-data_2015_ssp1)/data_2015_ssp1*100
change_2050_ssp3 <- data_2050_ssp3-data_2015_ssp3#(data_2050_ssp3-data_2015_ssp3)/data_2015_ssp3*100
change_2050_ssp5 <- data_2050_ssp5-data_2015_ssp5#(data_2050_ssp5-data_2015_ssp5)/data_2015_ssp5*100

# get yearly average
change_hist_yrl <- change_hist/11.5
change_2050_ssp1_yrl <- change_2050_ssp1/3.5
change_2050_ssp3_yrl <- change_2050_ssp3/3.5
change_2050_ssp5_yrl <- change_2050_ssp5/3.5

### 4 - Plots ----
#prepare background - uncomment code to produce land_mask.tif from the shapefile
#land_vec <- terra::vect(file.path(root_land, "ne_110m_land.shp"))
#mask <- terra::rasterize(land_vec, change_hist_yrl)
#mask <- terra::crop(mask, ext(c(-180,180,-58,90)))
#terra::writeRaster(mask, file.path(root_outputs,'land_mask.tif'))

mask_lu <- terra::rast(file.path(root_outputs,'land_mask.tif'))
#create colors 1
all <- c(as.array(change_hist_yrl), as.array(change_2050_ssp1_yrl), 
         as.array(change_2050_ssp3_yrl), as.array(change_2050_ssp5_yrl))
q <- quantile(all , na.rm=T,probs=seq(0, 1, 0.05))
length(q)
length(unique(q))
q <- unique(q)
br <- c(q[1], q[3], q[5], q[8], q[11], q[13], q[15])
br_at <- c(0.5, 2.5, 4.5, 7.5, 10.5, 12.5, 14.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(7)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(7)
co <- c(co1,co2)

#legend 1 - uncomment to plot legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,3), cex.axis=1.5)
  box()
  title(main = 'percentage points per decade 1900-2015 & 2015-2050', font.main = 1, cex.main=1.5, adj=0) #
}

dev.copy(pdf,file.path(root_figures, 'FigureS5_legend_change.pdf'),
         width=20,height=3.3)
dev.off()


#split screen
split.screen(c(3, 1))       # split display into three rows
split.screen(c(1, 2), screen = 1) # now split the middle one into 2
split.screen(c(1, 2), screen = 2) # now split the bottom one into 2
split.screen(c(1, 2), screen = 3) # now split the bottom one into 2

#map 1900-2015
screen(6)
plot(
  (change_hist_yrl),
  breaks = q,
  col = co,
  legend = F,
  axes= F
)
title(main = expression(bold(Delta ~ "1900-2015")), 
      font.main = 1, cex.main=1, adj=0)

#map ssp1
screen(5)
plot(
  change_2050_ssp1_yrl,
  breaks = q,
  col = co,
  legend = F,
  axes= F
)
title(main = expression(bold(Delta ~ "2015-2050 - Global sustainability")),
      font.main = 1, cex.main=1, adj=0)

#map ssp2
screen(7)
plot(
  change_2050_ssp3_yrl,
  breaks = q,
  col = co,
  legend = F,
  axes= F
)
title(main = expression(bold(Delta ~ "2015-2050 - Regional rivalry")), 
      font.main = 1, cex.main=1, adj=0)

#map ssp3
screen(9)
plot(
  change_2050_ssp5_yrl,
  breaks = q,
  col = co,
  legend = F,
  axes= F
)
title(main = expression(bold(Delta ~ "2015-2050 - Fossil fueled develop.")), 
      font.main = 1, cex.main=1, adj=0)

#create colors 2
q <- quantile(as.array(data_1900) , na.rm=T,probs=seq(0, 1, 0.05))
length(q)
length(unique(q))
q <- unique(q)
br <- c(q[1], q[4], q[7], q[10],q[12],q[15])
br_at <- c(0.5, 3.5, 6.5, 9.5, 11.5, 14.5)

co1 <- colorRampPalette(c("#9b4a14",'#FDE6BE'))(7) #"#FBD3AF", 
co2 <- colorRampPalette(c('lemonchiffon1', "#0073a0"))(7)
co <- c(co1,co2)

#map 1900
screen(4)
plot(
  (data_1900),
  breaks = q,
  col = co,
  legend = F,
  axes= F
)
title(main = expression(bold("1900")), 
      font.main = 1, cex.main=1, adj=0)

#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS5_Rangeland.pdf'),
         width=16,height=14)
dev.off()

# #legend 2 - uncomment to plot legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,1), cex.axis=1.5)
  box()
  title(main = 'distribution of land in 1900 (%)', font.main = 1, cex.main=1.5, adj=0) #
}
dev.copy(pdf,file.path(root_figures, 'FigureS5_legend_1900.pdf'),
         width=20,height=3.3)
dev.off()


#close all screens
close.screen(all = TRUE)
