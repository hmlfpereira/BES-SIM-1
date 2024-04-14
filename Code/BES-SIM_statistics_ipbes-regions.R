### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science https://doi.org/science.adn3441
### Zonal statistics ----
### Calculate zonal statistics per IPBES region for all netCDFs
#### Output 1: Values_IPBES-regions_raw-changes.xlsx - all models: mean, sum, median, area-weighted sum and area-weighted mean
#### Output 2: Values_IPBES-regions_delta-changes.xlsx - ESS models only: sum and area-weighted sum are calculated from the absolute values
####     for those years (when they exist), by first summing all cell values for each year, and then calculating the change
### Project BES SIM 1
### Created Dec 2023, Luise Quoß
### Revised Dec 2023, Henrique Pereira
### Revised Feb 2024, Luise Quoß & Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(terra)
library(dplyr)
library(ebvcube)
library(xlsx)
library(stringr)

#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root <- '../../Data_geo/ebv_cubes'
#directory with biodiversity model outputs as ebvcubes (they are downloaded below)

root_IPBES_Regions<-"../IPBES_Regions"
#directory with shapefile of IPBES regions from DataS4

root_outputs<-"../Outputs/"
#directory where other outputs and analysis will be saved

netcdfs_path <- list.files(root, patter='*nc', full.names = T)

#functions
areaweight_mean<-function(raster1)
{
  #produce a raster that has the area of each degree cell
  a<-cellSize(raster1)
  a <- mask(a,raster1)
  #plot(a)
  #calculate area weighted mean
  asum<-global(a, "sum", na.rm=TRUE)$sum
  w<-a/asum
  #plot(raster1*w)
  areaw_mean<-global(raster1*w, "sum", na.rm=TRUE)$sum
  return(list(Area_W_Mean=areaw_mean, Area_W_Sum=areaw_mean*asum))
}

calc_delta_lq<-function(df,year0,year1,stats)
{
  (df[[stats]][df$Years==year1][[1]]-df[[stats]][df$Years==year0][[1]])/df[[stats]][df$Years==year0][[1]]*100
}

calc_delta_cube_lq<-function(es)
{
  es2<-es
  es2$Metric <- str_replace(es2$Metric," \\(.*", "")
  metrics <- unique(es2$Metric)
  scenarios <- unique(es2$Scenario)
  y1900 <- setdiff(unique(es2$Years),c("2015","2050"))
  entities <- unique(es2$Entity)
    for (m in metrics){
      if(m %in% es$Metric){
        #print(m)
      for (s in scenarios){
        absol<-subset(es2, Metric==m & Scenario==s & Entity==e & ! str_detect(Units,'%'))
        index<-which(es2$Metric==m & es2$Scenario==s & es2$Entity==e & str_detect(es2$Units,'%')) #=="Percentage (%)"
        for (i in index)
        {
          if ((!es2$Years[i] %in% c("2015","2050")) | (length(y1900)==0 & (es2$Years[i]=="2015")))
          {
            es2$Sum[i] <- 0
            es2$Area_W_Sum[i] <- 0
          } else if ((es2$Years[i] =="2015") & (!length(y1900)==0))
          {
            es2$Sum[i] <- calc_delta_lq(absol,y1900,"2015","Sum")
            es2$Area_W_Sum[i] <- calc_delta_lq(absol,y1900,"2015","Area_W_Sum")
          } else
          {
            es2$Sum[i] <- calc_delta_lq(absol,"2015","2050","Sum")
            es2$Area_W_Sum[i] <- calc_delta_lq(absol,"2015","2050","Area_W_Sum")
          }
        }
      }
      
      }else{
        print(paste0('No change calculated because no absolute values available.'))
      }
      
  }
  
  return(es2)
}

### 2 - Import data ----
ipbes_vec <- terra::vect(file.path(root_IPBES_Regions, 'IPBES_subregions_simp_v6.shp'))

### 3 - Calculations ----
#prepare result data.frame
cnames <- c('Scenario', 'LUCC', 'Model', 'Entity', 'Region','Years', 'Metric','Units', 'Sum','Mean', 'Median','Area_W_Mean','Area_W_Sum')
result <- data.frame(matrix(ncol = length(cnames), nrow = 0))
colnames(result) <- cnames

# loop through netCDFs
for (nc.i in 1:length(netcdfs_path)){ #
  nc <- netcdfs_path[nc.i]
  #get relevant netCDF info 
  cubes <- ebv_datacubepaths(nc)
  dates <- ebv_properties(nc)@temporal$dates
  entities <- ebv_properties(nc)@general$entity_names
  model <- ebv_properties(nc)@general$title
  pos<-last(str_locate_all(model,"BES-SIM ")[[1]])
  model <- str_sub(model,pos+1,-2) 
  
  cat('---- processing file ', model, ' (', nc.i, '/', length(netcdfs_path), ')\n', sep = '')
  
  #loop through cubes
  for (c.i in 1:dim(cubes)[1]){
    c.path <- cubes[c.i, 1]
    units <- ebv_properties(nc, c.path)@ebv_cube$units
    cat('-> cube ', c.path, ' (', c.i, '/', dim(cubes)[1], ')\n', sep = '')
    
    #loop through entities
    for(e in entities){
      
        #loop through ipbes regions
        for(region in unique(ipbes_vec$IPBES_sub)){
          
          #subset the vector data
          region.vec <- ipbes_vec[ipbes_vec$IPBES_sub == region,]
          
          #build result data.frame
          part <- data.frame(matrix(ncol = length(cnames), nrow = length(dates)))
          colnames(part) <- cnames
          r.i <- 1
          
          #loop through dates
          for(ts.i in 1:length(dates)){
            
            #read data
            data <- ebv_read(
              filepath = nc,
              datacubepath = c.path, 
              entity = e, 
              timestep = ts.i,
              verbose = F,
              ignore_RAM = TRUE
            )
          
          
            #extract values -> no weights + decide on touches!
            temp.raster <- terra::rasterize(region.vec, data, touches=T) #touches true to not loose the small pixels (e.g. Oceania)
            
            #mask the subset
            region.raster <- terra::mask(data, temp.raster)
            
            #calculate the metrics
            mean <- terra::global(region.raster, 'mean', na.rm=TRUE)
            sum <-global(region.raster, "sum", na.rm=TRUE)
            #median <-global(region.raster, 'median', na.rm=TRUE)
            median <- median(as.array(region.raster),na.rm=T)
            stats <-areaweight_mean(region.raster)
            Area_W_Mean <- stats$Area_W_Mean
            Area_W_Sum <- stats$Area_W_Sum
            
            #create part data.frame
            part$Scenario[r.i] <-  substr(cubes[c.i, 2],1,12) 
            part$LUCC[r.i] <- substr(cubes[c.i, 2],13,16)
            part$Model[r.i] <- model
            part$Entity[r.i] <- e
            part$Region[r.i] <- region
            part$Years[r.i] <- str_sub(as.character(dates[ts.i]), 1,4)
            part$Metric[r.i] <- cubes[c.i, 3]
            part$Units[r.i] <- units
            part$Sum[r.i] <- sum
            part$Mean[r.i] <- mean
            part$Median[r.i] <- median
            part$Area_W_Mean[r.i] <- Area_W_Mean
            part$Area_W_Sum[r.i] <- Area_W_Sum
            
            r.i <- r.i +1

          }
          

          #add data to result data.frame
          result <- rbind(result, part)
        }
    }
    
  }

}


#write xlsx
xlsx::write.xlsx(result, file.path(root_outputs, 'Values_IPBES-regions_raw-changes.xlsx'),'ipbes.stats-raw')

# calculate delta change ESS
# for each part (one model, one region, one entity, all cubes+timesteps) 

#subset-result to ESS only
result <- xlsx::read.xlsx(file.path(root_outputs, 'Values_IPBES-regions_raw-changes.xlsx'), 1)
result_ESS <- result[result$Model %in% c('GLOBIO-ES', 'InVEST', 'CABLE POP',
                                         'LPJ', 'LPJ-GUESS'),]
unique(result_ESS$Model)

#prepare result data.frame
result_delta <- data.frame(NA, matrix(ncol = dim(result_ESS)[2], nrow = dim(result_ESS)[1]))
colnames(result_delta) <- cnames

for(m in unique(result_ESS$Model)){
  print(paste0('----',m))
  for(r in unique(result_ESS$Region)){
    entities <- unique(result_ESS[result_ESS$Model == m & result_ESS$Region == r,]$Entity)
    for(e in entities){
      #get indices
      indices <- which(result_ESS$Model == m & result_ESS$Region == r & result_ESS$Entity == e)
      subset <- result_ESS[indices,]
      deltas <- calc_delta_cube_lq(subset)
      
      #fill result_delta
      result_delta[indices, ] <- deltas[,2:ncol(deltas)]
    }
  }
}

#write data delta
xlsx::write.xlsx(result_delta[,1:13], file.path(root_outputs, 'Values_IPBES-regions_raw-delta.xlsx'),'ipbes.stats-delta')

