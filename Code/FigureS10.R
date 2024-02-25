### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figure S10 ----
### Create BE maps per scenario for the AIM model
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

#download data
for (id in 31) {
  tryCatch({
    ebv_download(id, root)
  },error = function(e) {
    if(stringr::str_detect(as.character(e),'NetCDF already downloaded'))
      print(paste0('NetCDF with id ',id,' already downloaded'))
  })
}

aim_path <- file.path(root, 'pereira_comcom_id31_20220321_v2.nc')

### 2 - Import data ----
# ebv_datacubepaths(aim_path)
aim_s_lucc <- ebv_read(filepath = aim_path,
                     datacubepath = 'scenario_6/metric_2/ebv_cube',
                     timestep = 3,
                     entity = 2, type='r')
aim_ss_lucc <- ebv_read(filepath = aim_path,
                      datacubepath = 'scenario_6/metric_3/ebv_cube',
                      timestep = 3,
                      entity = 2, type='r')
aim_s_lucc_fd <- ebv_read(filepath = aim_path,
                       datacubepath = 'scenario_6/metric_2/ebv_cube',
                       timestep = 3,
                       entity = 1, type='r')
aim_ss_lucc_fd <- ebv_read(filepath = aim_path,
                        datacubepath = 'scenario_6/metric_3/ebv_cube',
                        timestep = 3,
                        entity = 1, type='r')

### 3 - Calculations ----
aim_s_lucc_yrl <- (((1+(aim_s_lucc/100))^(1/3.5))-1)*100
aim_ss_lucc_yrl <- (((1+(aim_ss_lucc/100))^(1/3.5))-1)*100 
aim_s_lucc_fd_yrl <-(((1+(aim_s_lucc_fd/100))^(1/3.5))-1)*100  
aim_ss_lucc_fd_yrl <- (((1+(aim_ss_lucc_fd/100))^(1/3.5))-1)*100

### 4 - Plots ----
#create colors
all <- c(as.array(aim_s_lucc_yrl), as.array(aim_ss_lucc_yrl),
         as.array(aim_s_lucc_fd_yrl), as.array(aim_ss_lucc_fd_yrl))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- c(q[1:15], 0, q[16:21])


br <- c(q[2], q[5],  q[9], q[13], q[16], q[19], q[length(q)-1])
br_at <- c(1.5, 4.5, 8.5, 12.5, 15.5, 18.5, 20.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(14)
co3 <- colorRampPalette(c("#FBD3AF","lemonchiffon1"))(3)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(6)
co <- c(co1,co3[2], co2)

#legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2), cex.axis=1.5)
  box()
  title(main = expression("% spp. decade"^{-1}), font.main = 1, cex.main=1.5, adj=0) #
}

dev.copy(pdf,file.path(root_figures, 'FigureS10_legend_decade.pdf'),
         width=15,height=2.5)
dev.off()

#split screen
split.screen(c(2, 1))       # split display into two screens
split.screen(c(1, 2), screen = 1) # now split the upper one into 2
split.screen(c(1, 2), screen = 2) # now split the lower one into 2

#map s
screen(3)
plot(
  aim_s_lucc_yrl,
  breaks = q,
  col = co,
  main = expression(paste('(a) ', Delta,'S',alpha,' - No dispersal')),
  legend = F,
  axes= F
)

#map ss
screen(4)
plot(
  aim_ss_lucc_yrl,
  breaks = q,
  main = expression(paste('(b) ', Delta,'SS',alpha,' - No dispersal')),
  col = co,
  legend = F,
  axes= F
)

#map s fd
screen(5)
plot(
  aim_s_lucc_fd_yrl,
  breaks = q,
  main = expression(paste('(c) ', Delta,'S',alpha,' - Full dispersal')),
  col = co,
  legend = F,
  axes= F
)

#map ss fd
screen(6)
plot(
  aim_ss_lucc_fd_yrl,
  breaks = q,
  main = expression(paste('(d) ', Delta,'SS',alpha,' - Full dispersal')),
  col = co,
  legend = F,
  axes= F
)

close.screen(all = TRUE)

#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS10_AIM_model.pdf'),
         width=13,height=10)
dev.off()



