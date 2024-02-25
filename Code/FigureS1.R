### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figure S1 ----
### Create global historical trends (1900-2015) in land-use and projected trends for each scenario (2015-2050)
### Project BES SIM 1
### Created October 2023, Luise Quoß
### Revised Feb 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(terra)
library(ggplot2)
library(rstudioapi)
library(viridis)

#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

#set paths
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

### 2 - Import data ----
hist_path <- file.path(root, 'states.nc')
ssp1_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc')
ssp3_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc')
ssp5_path <- file.path(root, 'multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc')

### 3 - Calculations ----
#get raster area with cellSize - Mkm^2
terra::rast(hist_path)
area_raster <- cellSize(terra::rast(hist_path), unit="km") / 1000000

#process data -> diff of sum to 1900 per year
varnames <- c('primf', 'primn', 'pastr', 'secdf', 'secdn', 'range', 'crops') 

#prepare output data frame
nrow = length(varnames)*12+length(varnames)*4
data <- data.frame(matrix(NA, nrow=nrow,ncol=4))
colnames(data) <- c('group', 'variable', 'value', 'year')

#run per variable
i = 1
for (varname in varnames){
  print(varname)
  sums <- c(0)
  #get historical data
  if(varname=='crops'){
    data_weighted <- (terra::rast(hist_path, 'c3ann')[[1051]] + terra::rast(hist_path, 'c3nfx')[[1051]] +
                        terra::rast(hist_path, 'c3per')[[1051]] + terra::rast(hist_path, 'c4ann')[[1051]]+
                        terra::rast(hist_path, 'c4per')[[1051]]) * area_raster
    base <- global(data_weighted, 'sum', na.rm=T)
  }else{
    data_weighted <- rast(hist_path, varname)[[1051]] * area_raster
    base <- global(data_weighted, 'sum', na.rm=T)
  }
  for (k in seq(1061,1166,10)){
    if(varname=='crops'){
      data_1_weighted <- (terra::rast(hist_path, 'c3ann')[[k]] + terra::rast(hist_path, 'c3nfx')[[k]] +
                            terra::rast(hist_path, 'c3per')[[k]] + terra::rast(hist_path, 'c4ann')[[k]]+
                            terra::rast(hist_path, 'c4per')[[k]]) * area_raster
      new <- global(data_1_weighted, 'sum', na.rm=T) - base
    }else{
      data_1_weighted <- rast(hist_path, varname)[[k]]  * area_raster
      new <- global(data_1_weighted, 'sum', na.rm=T) - base
    }
    sums <- c(sums, new$sum)
  }
  #add to dataframe
  data[i:(i+11),'value'] <- sums
  data[i:(i+11),'year'] <- seq(1900, 2010, 10)
  data[i:(i+11),'group'] <- c(varname)
  data[i:(i+11),'variable'] <- c('hist')
  i = i+12

  #get scenario data
  sums_ssp <- c()
  for (j in seq(6,36,10)){
    #loop through scenarios
    for(sp in c('ssp1', 'ssp3', 'ssp5')){
      path <- eval(parse(text= paste0(sp,'_path')))
      #treat crops and the rest differently
      if(varname=='crops'){
        data_weighted_ssp <- (terra::rast(path, 'c3ann')[[j]] + terra::rast(path, 'c3nfx')[[j]] +
                                terra::rast(path, 'c3per')[[j]] + terra::rast(path, 'c4ann')[[j]]+
                                terra::rast(path, 'c4per')[[j]]) * area_raster
        new <- global(data_weighted_ssp, 'sum', na.rm=T) - base
      }else{
        data_weighted_ssp_1 <- rast(path, varname)[[j]] * area_raster 
        new <- global(data_weighted_ssp_1, 'sum', na.rm=T) - base
      }
      sums_ssp <- c(sums_ssp, new$sum)
    }

  }
  #add to dataframe
  data[i:(i+11),'value'] <- sums_ssp
  data[i:(i+11),'year'] <- c(rep('2020', 3),rep('2030', 3),rep('2040', 3),rep('2050', 3))
  data[i:(i+11),'group'] <- varname
  data[i:(i+11),'variable'] <- rep(c('ssp1','ssp3','ssp5'), 4)
  i = i+12


}

#add the 2010 data a second time for each scenario to connect the lines in the plot
for(ssp in c('ssp1', 'ssp3', 'ssp5')){
  part <- data[data$year=='2010',]
  part$variable <- ssp
  data <- rbind(data, part)
}

# write.csv(data, file.path(root_figures,'values_S1_area_weighted.csv'))

#read data - can be done after the part above ran at least once
# data <- read.csv(file.path(root_figures,'values_S1_area_weighted.csv'))

#divide crops by 5
data[data$group == 'crops',]$value <- (data[data$group == 'crops',]$value/5)

### 4 - Plots ----
data_hist <- data[data$variable=='hist',]
data_ssp <- data[data$variable!='hist',]


lwd = 0.6
ggplot() + 
  geom_line(data=data_hist, aes(x=year, y=value, group=group, color=group, linetype = "solid"), alpha=.8, linewidth=lwd) +
  scale_color_viridis(option="turbo",discrete=TRUE,
                      labels=c('Crops', 'Managed pasture',"Forested primary land","Non-forested primary land",
                                "Rangeland", 'Forested secondary land', 'Non-forested\nsecondary land')) +
  geom_line(data=data_ssp, aes(x=year, y=value, group=interaction(group, variable), 
                               color=group, linetype=variable), alpha=.8, linewidth=lwd) +
  scale_linetype_manual(values=c('solid',"dashed", "dotdash","dotted"),
                        labels = c('Historical', 'Global sustainability', 
                                   'Regional rivalry','Fossil-fueled develop.')) +
  geom_hline(yintercept=0, color = "black", linewidth=0.2) +
  theme_bw() +
  labs(x="Year", y=expression(paste("LU relative to 1900 in Mkm"^{2}))) +
  theme(
     strip.text.y = element_text(size = 10, colour = "black"),
     strip.text.x = element_text(size = 10, colour = "black"),
     axis.text.x=element_text(size=10),
     axis.text.y=element_text(size=10),
     legend.text=element_text(size=10),
     legend.title=element_blank(),
     legend.position = "bottom",
     legend.box="vertical"
     )+
  guides(color = guide_legend(override.aes = list(linewidth = 1.1)))

dev.copy(pdf,file.path(root_figures, 'FigureS1_LandUseTrends.pdf'),
         width = 8, height = 8)
dev.off()
