### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Global statistics ----
### Calculate global statistics for all netCDFs
#### Output 1: Values_IPBES-regions_raw-changes.xlsx - all models: mean, sum, median, area-weighted sum and area-weighted mean
#### Output 2: Values_IPBES-regions_raw-changes.xlsx - ESS models only: mean, median, area-weighted mean and deltas of the sum of area-weighted sum
### Project BES SIM 1
### Created Nov 2023, Henrique Pereira
### Revised Dec 2023, Henrique Pereira
### Revised Feb 2024, Luise Quoß & Henrique Pereira

####1 - Initializations ----
# clear workspace
rm(list=ls())

#libraries
library(readxl)
library(stringr)
library(dplyr)
library(ebvcube)
library(terra)
library(lubridate)

#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root <- '../../Data_geo/ebv_cubes'
#directory with biodiversity model outputs as ebvcubes (they are downloaded below)

root_IPBES_Regions<-"../../Data_Geo/IPBES_Regions"
#directory with shapefile of IPBES regions from DataS4

root_outputs<-"../Outputs/"
#directory where other outputs and analysis will be saved

netcdfs_path <- list.files(root, patter='*nc', full.names = T)

#list all files
ebvfiles<-netcdfs_path
df<-NULL

####2 - Generic functions ----

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

#plot(raster1)

process_ebvcube<-function(netcdf)
{
  prop<-ebv_properties(netcdf)
  print(prop@general$title)
  cubes <- ebv_datacubepaths(netcdf)
  metrics<-unique(cubes$metric_names)
  scenarios<-unique(cubes$scenario_names)
  es<-tibble(Scenario=character(), LUCC=character(), Model=character(), Region=character(), Years=character(), Metric=character(),
             Entity=character(), Units=character(), Sum=numeric(), Mean=numeric(), Median=numeric(), 
             Area_W_Mean=numeric(), Area_W_Sum=numeric())
  for (m in metrics)
    for (s in scenarios) 
    {
      index<-which(metrics==m)[1]+(which(scenarios==s)[1]-1)*length(metrics)
      propcube <- ebv_properties(filepath = netcdf, datacubepath =  cubes$datacubepaths[index])
      timesteps <- year(propcube@temporal$dates)
      entities <- unique(propcube@general$entity_names)
      for (ent in entities)
        for (t in timesteps)
        {
          print(paste(m,s,ent,t))
          cubestats <- ebv_analyse(filepath = netcdf,
                                   datacubepath = cubes$datacubepaths[index],
                                   entity = which(ent==entities),
                                   timestep = which(t==timesteps))
          es1<-es[1,]
          es1$Scenario<-substr(s,1,11)
          es1$Years<-t
          es1$LUCC<-substr(s,13,16)
          pos<-last(str_locate_all(prop@general$title,"BES-SIM ")[[1]])[,1]
          es1$Model<-str_sub(prop@general$title,pos+8,-2) 
          es1$Region<-"Global"
          es1$Entity<-ent
          es1$Metric<-m
          es1$Units<-propcube@ebv_cube$units
          es1$Median<-cubestats$q50
          es1$Mean<-cubestats$mean
          raster1 <- ebv_read(filepath = netcdf,
                              datacubepath = cubes$datacubepaths[index],  
                              entity = which(ent==entities),
                              timestep = which(t==timesteps))
          es1$Sum<-global(raster1, "sum", na.rm=TRUE)$sum
          stats<-areaweight_mean(raster1)
          es1$Area_W_Mean <- stats$Area_W_Mean
          es1$Area_W_Sum <- stats$Area_W_Sum
          es<-rbind(es,es1) 
        }
    }
  return(es)
}

calc_delta<-function(df,year0,year1,stats)
{
  (df[[stats]][df$Years==year1]-df[[stats]][df$Years==year0])/df[[stats]][df$Years==year0]*100
}


calc_delta_cube<-function(es)
{
  es2<-es
  es2$Metric <- str_replace(es2$Metric," \\(.*", "")
  metrics <- unique(es2$Metric)
  scenarios <- unique(es2$Scenario)
  entities <- unique(es2$Entity)
  y1900 <- setdiff(unique(es$Years),c("2015","2050"))
  for (m in metrics)
    for (s in scenarios)
      for (ent in entities)
      {
        print(paste(m,s,ent))
        absol<-subset(es2, Metric==m & Scenario==s & Entity==ent & Units!="%")
        index<-which(es2$Metric==m & es2$Scenario==s &  es2$Entity==ent & es2$Units =="%")
        for (i in index)
        {
          if ((!es2$Years[i] %in% c("2015","2050")) | (length(y1900)==0 & (es2$Years[i]=="2015")))
          {
            es2$Sum[i] <- 0
            es2$Area_W_Sum[i] <- 0
          } else if ((es2$Years[i] =="2015") & (!length(y1900)==0))
          {
            es2$Sum[i] <- calc_delta(absol,y1900,"2015","Sum")
            es2$Area_W_Sum[i] <- calc_delta(absol,y1900,"2015","Area_W_Sum")
          } else
          {
            es2$Sum[i] <- calc_delta(absol,"2015","2050","Sum")
            es2$Area_W_Sum[i] <- calc_delta(absol,"2015","2050","Area_W_Sum")
          }
        }
      }
  return(es2)
}

#### 3 - Process ebv_cubes ----
for (file in ebvfiles)
{
  df1<-process_ebvcube(file)
  #for ecosystem services % stats are calculated from 
  #summing the absolute values across cells and calculating
  #the relative change of those sums, storing the results 
  #on the Sum and Area_W_Sum fields
  if (file %in% ebvfiles[7:10])
    df1<-calc_delta_cube(df1)
  df<-rbind(df,df1)
}

#### 4 - Create delta_I for GLOBIO ----
df2 <- df
netcdf <- file.path(root, "pereira_comcom_id27_20220405_v1.nc")  
prop<-ebv_properties(netcdf)
cubes <- ebv_datacubepaths(netcdf)
metrics<-unique(cubes$metric_names)
scenarios<-unique(cubes$scenario_names)

for (s in scenarios)
{
  index<-which(metrics=="Intactness, I")[1]+(which(scenarios==s)[1]-1)*length(metrics)
  propcube <- ebv_properties(filepath = netcdf, datacubepath =  cubes$datacubepaths[index])
  timesteps <- year(propcube@temporal$dates)
  ent <- unique(propcube@general$entity_names)
  for (t in timesteps[-1])
  {
    es1<-df2[1,]
    es1$Scenario<-substr(s,1,11)
    es1$Years<-t
    es1$LUCC<-substr(s,13,16)
    pos<-last(str_locate_all(prop@general$title,"BES-SIM ")[[1]])[,1]
    es1$Model<-str_sub(prop@general$title,pos+8,-2)
    es1$Region<-"Global"
    es1$Entity<-ent
    es1$Metric<-"Relatve Change in Intactness, Delta_I"
    es1$Units<-"Percentage points (%)"
    es1$Median<-NA
    es1$Sum<-NA
    es1$Area_W_Sum<-NA
    i0 <- ebv_read(filepath = netcdf,
                   datacubepath = cubes$datacubepaths[index],
                   entity = 1,
                   timestep = which(t==timesteps)-1)
    i1 <- ebv_read(filepath = netcdf,
                   datacubepath = cubes$datacubepaths[index],
                   entity = 1,
                   timestep = which(t==timesteps))
    delta_i <- (i1-i0)/i0*100
    es1$Mean <- global(delta_i, "mean", na.rm=TRUE)$mean
    es1$Area_W_Mean <-areaweight_mean(delta_i)$Area_W_Mean
    df2<-rbind(df2,es1)
  }
}
  

#### 5 - Calculate I and delta_I for PREDICTS ----

netcdf <- file.path(root, "pereira_comcom_id28_20231212_v2.nc")  

prop<-ebv_properties(netcdf)
cubes <- ebv_datacubepaths(netcdf)
metrics<-unique(cubes$metric_names)
scenarios<-unique(cubes$scenario_names)

for (s in scenarios)
{
  index<-which(metrics=="Intactness, I")[1]+(which(scenarios==s)[1]-1)*length(metrics)
  propcube <- ebv_properties(filepath = netcdf, datacubepath =  cubes$datacubepaths[index])
  timesteps <- year(propcube@temporal$dates)
  ent <- unique(propcube@general$entity_names)
  for (t in timesteps[-1])
  {
    es1<-df2[1,]
    es1$Scenario<-substr(s,1,11)
    es1$Years<-t
    es1$LUCC<-substr(s,13,16)
    pos<-last(str_locate_all(prop@general$title,"BES-SIM ")[[1]])[,1]
    es1$Model<-str_sub(prop@general$title,pos+8,-2)
    es1$Region<-"Global"
    es1$Entity<-ent
    es1$Metric<-"Relatve Change in Intactness, Delta_I"
    es1$Units<-"Percentage points (%)"
    es1$Median<-NA
    es1$Sum<-NA
    es1$Area_W_Sum<-NA
    i0 <- ebv_read(filepath = netcdf,
                   datacubepath = cubes$datacubepaths[index],
                   entity = 1,
                   timestep = which(t==timesteps)-1)
    i1 <- ebv_read(filepath = netcdf,
                   datacubepath = cubes$datacubepaths[index],
                   entity = 1,
                   timestep = which(t==timesteps))
    delta_i <- (i1-i0)/i0*100
    es1$Mean <- global(delta_i, "mean", na.rm=TRUE)$mean
    es1$Area_W_Mean <-areaweight_mean(delta_i)$Area_W_Mean
    df2<-rbind(df2,es1)
  }
}

df3 <- df2

### 6 - Write results in file
# clean dataframe
df3<-subset(df3, Scenario=="SSP1-RCP2.6" | Scenario=="SSP1xRCP2.6" | Scenario=="SSP1xRCP1.5" 
            | Scenario =="SSP1-RCP1.5" | Years!="2015")
df3<-subset(df3, Scenario=="SSP1-RCP2.6" | Scenario=="SSP1xRCP2.6" | Scenario=="SSP1xRCP1.5" 
            | Scenario =="SSP1-RCP1.5" | Years!="1900")
df4<-df3[!str_detect(df3$Units,"%") | df3$Years!="1900",]

#write csv
write.csv2(df4,file.path(root_outputs,
                         "process_all.csv"), row.names = FALSE)
