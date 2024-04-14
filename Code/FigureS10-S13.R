### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050.Science https://doi.org/science.adn3441
### Figures S11-S14 ----
### Create ES maps per scenario, some per model, some averaged over several models
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
lpj_guess_path <- ebv_download('10.25829/z5v9t2', root)
lpj_path <- ebv_download('10.25829/xq7a86', root)
cable_path <- ebv_download('10.25829/ktnb68', root)
invest_path <- ebv_download('10.25829/zr4d27', root)
globio_path <- ebv_download('10.25829/vqd4s4', root)

#generic function
geomchange<-function(deltay,deltatime)
{
  ((1+deltay/100)^(1/deltatime)-1)*100
}

#maps for figure S10-----
#### a) - Import data ----

#read data from lpj-guess
# ebv_datacubepaths(lpj_guess_path)
lpj_guess_carb_s1 <- ebv_read(filepath = lpj_guess_path,
                              datacubepath = 'scenario_1/metric_3/ebv_cube',
                              entity = 1, type='r',
                              timestep=3)
lpj_guess_carb_s2 <- ebv_read(filepath = lpj_guess_path,
                              datacubepath = 'scenario_2/metric_3/ebv_cube',
                              entity = 1, type='r',
                              timestep=3)
lpj_guess_carb_s3 <- ebv_read(filepath = lpj_guess_path,
                              datacubepath = 'scenario_3/metric_3/ebv_cube',
                              entity = 1, type='r',
                              timestep=3)

#read data from lpj
# ebv_datacubepaths(lpj_path)
lpj_carb_s1 <- ebv_read(filepath = lpj_path,
                        datacubepath = 'scenario_1/metric_1/ebv_cube',
                        entity = 1, type='r',
                        timestep=3)
lpj_carb_s2 <- ebv_read(filepath = lpj_path,
                        datacubepath = 'scenario_2/metric_1/ebv_cube',
                        entity = 1, type='r',
                        timestep=3)
lpj_carb_s3 <- ebv_read(filepath = lpj_path,
                        datacubepath = 'scenario_3/metric_1/ebv_cube',
                        entity = 1, type='r',
                        timestep=3)

#read data from cable
# ebv_datacubepaths(cable_path)
cable_carb_s1 <- ebv_read(filepath = cable_path,
                          datacubepath = 'scenario_1/metric_1/ebv_cube',
                          entity = 1, type='r',
                          timestep=2)
cable_carb_s2 <- ebv_read(filepath = cable_path,
                          datacubepath = 'scenario_2/metric_1/ebv_cube',
                          entity = 1, type='r',
                          timestep=2)
cable_carb_s3 <- ebv_read(filepath = cable_path,
                          datacubepath = 'scenario_3/metric_1/ebv_cube',
                          entity = 1, type='r',
                          timestep=2)

#read data from globio
# ebv_datacubepaths(globio_path)

globio_carb_s1 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_1/metric_5/ebv_cube',
                           entity = 1, type='r')
globio_carb_s2 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_2/metric_5/ebv_cube',
                           entity = 1, type='r')
globio_carb_s3 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_3/metric_5/ebv_cube',
                           entity = 1, type='r')

#### b) - Calculations ----
#extend the extent of cable rasters - they are smaller
cable_carb_s1 <- terra::extend(cable_carb_s1,lpj_carb_s3)
cable_carb_s2 <- terra::extend(cable_carb_s2,lpj_carb_s3)
cable_carb_s3 <- terra::extend(cable_carb_s3,lpj_carb_s3)

#stack the rasters per scenario
ssp1_carb <- c(lpj_guess_carb_s1, lpj_carb_s1, cable_carb_s1, globio_carb_s1)
ssp3_carb <- c(lpj_guess_carb_s2, lpj_carb_s2, cable_carb_s2, globio_carb_s2)
ssp5_carb <- c(lpj_guess_carb_s3, lpj_carb_s3, cable_carb_s3, globio_carb_s3)
#remove all the single files from the memory
rm(globio_carb_s1,globio_carb_s2,cable_carb_s2,cable_carb_s1, lpj_carb_s2,
   lpj_carb_s1, lpj_guess_carb_s1, lpj_guess_carb_s2)

#calculate the mean
ssp1_carb_mean <- app(ssp1_carb, mean, na.rm=T)
ssp3_carb_mean <- app(ssp3_carb, mean, na.rm=T)
ssp5_carb_mean <- app(ssp5_carb, mean, na.rm=T)
#remove the stacks
rm(ssp1_carb, ssp3_carb, ssp5_carb)

#### c) - Plots ----
#prepare background
land_vec <- terra::vect(land_path)
mask <- terra::rasterize(land_vec, ssp5_carb_mean)
mask <- terra::crop(mask, ext(c(-180,180,-58,90)))

# calculate % per decade
ssp1_carb_mean <-geomchange(ssp1_carb_mean,3.5)
ssp3_carb_mean <- geomchange(ssp3_carb_mean, 3.5)
ssp5_carb_mean <- geomchange(ssp5_carb_mean, 3.5)

#colors
all <- c(as.array(ssp1_carb_mean[,,1]),as.array(ssp3_carb_mean[,,1]), as.array(ssp5_carb_mean[,,1]))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)
q <- c(q[1:6],0,q[7:21])
br <- c(q[2], q[5], q[7],  q[9], q[13], q[17], q[length(q)-1])
br_at <- c(1.5, 4.5, 6.5, 8.5, 12.5, 16.5, 20.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(6)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(15)
co <- c(co1,co2)

#legend
{
  par(mfrow = c(1,1))
  graphics::image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2),cex.axis=1.5)
  box()
  title(main = expression("% decade"^{-1}), font.main = 1, cex.main=2, adj=0)
}
dev.copy(pdf,file.path(root_figures, 'FigureS10_legend.pdf'),
         width=17,height=2.6)
dev.off()


{
  par(mfrow = c(2, 2))
  
  plot(mask, legend = F,
       main = '(a) Global sustainability',
       axes= F,
       col='grey93')
  plot(
    ssp1_carb_mean,
    breaks = q,
    add = T,
    col = co,
    legend = F,
    axes= F
  )
  
  plot(mask, legend = F,
       main = '(b) Regional rivalry',
       axes= F,
       col='grey93')
  plot(
    ssp3_carb_mean,
    breaks = q,
    add = T,
    col = co,
    legend = F,
    axes= F
  )
  
  plot(mask, legend = F,
       main = '(c) Fossil-fueled develop.',
       axes= F,
       col='grey93')
  plot(
    ssp5_carb_mean,
    breaks = q,
    add = T,
    col = co,
    legend = F,
    axes= F
  )
}

#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS10_Carbon_SSPs.pdf'),
         width=13,height=10)
dev.off()


#remove
rm(all, ssp1_carb_mean, ssp3_carb_mean)
gc()

#maps for figure S12----
#### a) - Plots ----
# calculate % per decade
globio_carb_s3 <-geomchange(globio_carb_s3,3.5)
cable_carb_s3 <- geomchange(cable_carb_s3, 3.5)
lpj_carb_s3 <- geomchange(lpj_carb_s3, 3.5)
lpj_guess_carb_s3 <- geomchange(lpj_guess_carb_s3, 3.5)

stack<-c(globio_carb_s3,cable_carb_s3,lpj_carb_s3,lpj_guess_carb_s3)
corrmat<-layerCor(stack, "pearson")
mean(corrmat$correlation[upper.tri(corrmat$correlation)])


#colors
all <- c(as.array(globio_carb_s3[,,1]),as.array(cable_carb_s3[,,1]), 
         as.array(lpj_carb_s3[,,1]), as.array(lpj_guess_carb_s3[,,1]))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)
q <- c(q[1:7],0,q[8:21])
br <- c(q[2], q[3],q[5], q[8], q[12],q[15],q[18],q[length(q)-1])
br_at <- c(1.5, 2.5, 4.5, 7.5, 11.5, 14.5, 17.5, 20.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(7)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(14)
co <- c(co1,co2)

#legend
{
  par(mfrow = c(1,1))
  image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2),cex.axis=1.5)
  box()
  title(main = expression("% decade"^{-1}), font.main = 1, cex.main=2, adj=0)
}
dev.copy(pdf,file.path(root_figures, 'FigureS12_legend.pdf'),
         width=17,height=2.6)
dev.off()

{
  par(mfrow = c(2, 2))
  plot(mask, legend = F, main = '(a) CABLE',
       axes= F,
       col='grey93')
  plot(cable_carb_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(b) GLOBIO-ES',
       axes= F,
       col='grey93')
  plot(globio_carb_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(c) LPJ',
       axes= F,
       col='grey93')
  plot(lpj_carb_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(d) LPJ-GUESS',
       axes= F,
       col='grey93')
  plot(lpj_guess_carb_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
}
#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS12_CarbonAgreement.pdf'),
         width=13,height=10)
dev.off()

#remove
rm(all, globio_carb_s3, cable_carb_s3, lpj_carb_s3, lpj_guess_carb_s3)
gc()

#maps for figure S13----
#### a) - Import data ----
cable_wood_s3 <- ebv_read(filepath = cable_path,
                          datacubepath = 'scenario_3/metric_2/ebv_cube',
                          entity = 1, type='r',
                          timestep=2)
globio_wood_s3 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_3/metric_6/ebv_cube',
                           entity = 1, type='r')
lpj_guess_wood_s3 <- ebv_read(filepath = lpj_guess_path,
                              datacubepath = 'scenario_3/metric_5/ebv_cube',
                              entity = 1, type='r',
                              timestep = 3)

stack<-c(cable_wood_s3,resample(globio_wood_s3,cable_wood_s3),resample(lpj_guess_wood_s3,cable_wood_s3))
corrmat<-layerCor(stack, "pearson")
corrmat
mean(corrmat$correlation[upper.tri(corrmat$correlation)])

#### b) - Plots ----

# calculate % per decade
globio_wood_s3 <-geomchange(globio_wood_s3,3.5)
cable_wood_s3 <- geomchange(cable_wood_s3, 3.5)
lpj_guess_wood_s3 <- geomchange(lpj_guess_wood_s3, 3.5)

#colors
all <- c(as.array(cable_wood_s3[,,1]),as.array(globio_wood_s3[,,1]), as.array(lpj_guess_wood_s3[,,1]))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)
q <- c(q[1:7], 0, q[8:21])
br <- c(q[2], q[4], q[8], q[11], q[17],q[length(q)-1])
br_at <- c(1.5,3.5,7.5, 10.5, 16.5, 20.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(7)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(14)
co <- c(co1,co2)

#legend
{
  par(mfrow = c(1,1))
  image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2),cex.axis=1.5)
  box()
  title(main = expression("% decade"^{-1}), font.main = 1,cex.main=2, adj=0)
}
dev.copy(pdf,file.path(root_figures, 'FigureS13_legend.pdf'),
         width=15,height=2.5)
dev.off()

{
  par(mfrow = c(2, 2))
  
  plot(mask, legend = F, main = '(a) CABLE POP',
       axes= F,
       col='grey93')
  plot(cable_wood_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(b) GLOBIO-ES',
       axes= F,
       col='grey93')
  plot(globio_wood_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(c) LPJ-GUESS',
       axes= F,
       col='grey93')
  plot(lpj_guess_wood_s3,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
}
#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS13_TimberAgreement..pdf'),
         width=13,height=10)
dev.off()

#remove
rm(all)
gc()

#maps for figure S11----
#### a) - Import data ----

#----carbon (4): lpj-guess, lpj, cable, globio
ssp5_carb_mean

#----food (2): lpj-guess, globio
globio_food_s3 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_3/metric_1/ebv_cube',
                           entity = 1, type='r')
lpj_guess_food_s3 <- ebv_read(filepath = lpj_guess_path,
                              datacubepath = 'scenario_2/metric_1/ebv_cube',
                              entity = 1, type='r',
                              timestep = 3)
ssp5_food <- c(globio_food_s3, lpj_guess_food_s3)
ssp5_food_mean <- app(ssp5_food, mean, na.rm=T)

#----timber (3): cable, globio, lpj guess
#extend cable
cable_wood_s3 <- terra::extend(cable_wood_s3,globio_wood_s3)

ssp5_timber <- c(cable_wood_s3,globio_wood_s3,lpj_guess_wood_s3)
ssp5_timber_mean <- app(ssp5_timber, mean, na.rm=T)

#----pollination (2): invest, globio
ebv_datacubepaths(globio_path)
invest_poll_s3 <- ebv_read(filepath = invest_path,
                           datacubepath = 'scenario_3/metric_1/ebv_cube',
                           entity = 1, type='r',
                           timestep=3)
globio_poll_s3 <- ebv_read(filepath = globio_path,
                           datacubepath = 'scenario_3/metric_2/ebv_cube',
                           entity = 1, type='r')

ssp5_poll <- c(invest_poll_s3, globio_poll_s3)
ssp5_poll_mean <- app(ssp5_poll, mean, na.rm=T)

#----nitrogen (2): lpj-guess, invest
ebv_datacubepaths(lpj_guess_path)
invest_nitro_s3 <- ebv_read(filepath = invest_path,
                            datacubepath = 'scenario_3/metric_2/ebv_cube',
                            entity = 1, type='r',
                            timestep=3)
lpj_guess_nitro_s3 <- ebv_read(filepath = lpj_guess_path,
                               datacubepath = 'scenario_3/metric_4/ebv_cube',
                               entity = 1, type='r',
                               timestep=3)
#change signal of nitrogen export -> nitrogen retention
invest_nitro_s3 <- invest_nitro_s3*-1
lpj_guess_nitro_s3 <- lpj_guess_nitro_s3*-1
ssp5_nitro <- c(invest_nitro_s3, lpj_guess_nitro_s3)
ssp5_nitro_mean <- app(ssp5_nitro, mean, na.rm=T)

#### b) - Plots ----
# calculate % per decade
ssp5_nitro_mean <-geomchange(ssp5_nitro_mean,3.5)
ssp5_poll_mean <- geomchange(ssp5_poll_mean, 3.5)
ssp5_timber_mean<- geomchange(ssp5_timber_mean, 3.5)
ssp5_carb_mean <- geomchange(ssp5_carb_mean, 3.5)
ssp5_food_mean <- geomchange(ssp5_food_mean, 3.5)

#colors
all <-c(as.array(ssp5_nitro_mean[,,1]),as.array(ssp5_poll_mean[,,1]), as.array(ssp5_timber_mean[,,1]),
        as.array(ssp5_carb_mean[,,1]), as.array(ssp5_food_mean[,,1]))
all <- as.data.frame(all)
all <- na.omit(all)
q <- classIntervals(all$all, 20, style="quantile")$brks 
q <- unique(q)
br <- c(q[2], q[4], q[8], q[13], q[17],q[length(q)-1])
br_at <- c(1.5,3.5, 7.5, 12.5, 16.5, 19.5)

co1 <- colorRampPalette(c("brown2","#FBD3AF"))(7)
co2 <- colorRampPalette(c('lemonchiffon1', "cornflowerblue"))(13)
co <- c(co1,co2)

# #legend - uncomment to plot legend
{
  par(mfrow = c(1,1))
  image(x=1:(length(q)-1), y=1, matrix(1:(length(q)-1)), col=co, axes=FALSE, xlab='', ylab='')
  axis(1, at= br_at, labels=round(br,2),cex.axis=1.5)
  box()
  title(main = expression("% decade"^{-1}), font.main = 1,cex.main=2, adj=0)
}
dev.copy(pdf,file.path(root_figures, 'FigureS11_legend.pdf'),
         width=15,height=2.5)#width=20,height=1.7)
dev.off()


{
  par(mfrow = c(3, 2))
  
  plot(mask, legend = F, main = '(a) Ecosystem Carbon',
       axes= F,
       col='grey93')
  plot(ssp5_carb_mean,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(b) Food and Feed Production',
       axes= F,
       col='grey93')
  plot(ssp5_food_mean,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(c) Timber Production',
       axes= F,
       col='grey93')
  plot(ssp5_timber_mean,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(d) Crop pollination',
       axes= F,
       col='grey93')
  plot(ssp5_poll_mean,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
  
  plot(mask, legend = F, main = '(e) Nitrogen retention',
       axes= F,
       col='grey93')
  plot(ssp5_nitro_mean,
       breaks = q,
       add = T,
       col = co,
       legend = F,
       axes= F)
}

#save as file
dev.copy(pdf,file.path(root_figures, 'FigureS11_EcoServ_SSP5.pdf'),
         width=13,height=10)
dev.off()