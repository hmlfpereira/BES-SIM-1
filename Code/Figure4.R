### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figure 4 ----
### Create Input data and Map with regional plots for ES and BD
### Project BES SIM 1
### Created April 2019, Ines S. Martins
### Revised Nov 2023, Henrique Pereira
### Revised Feb 20224, Henrique Pereira

#### 1 - Initializations ----

# clear workspace
rm(list=ls())

library(dplyr)
library(ggplot2)
library(sf)
library(plyr)
library(purrr)
library(igraph)
library(readr)
library(raster)

#set paths
#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root_tables<-"../Data_Tables/"
#directory with data tables with outputs of biodiversity and ecosystem service models containing
#DataS1_Biodiversity.xlsx
#DataS2_EcosystemServices.xlsx

root_IPBES_Regions<-"../../Data_Geo/IPBES_Regions"
#directory with shapefile of IPBES regions from DataS4

root_figures <- '../Figures/'
#directory where figure outputs will be saved

root_outputs<-"../Outputs/"
#directory where other outputs and analysis will be saved

#Summary function
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#### 2 - Import data ----

all_data<-read_csv(file.path(root_tables,"EcosystemServices_regional.csv"))
all_data_global<-read_csv(file.path(root_tables,"EcosystemServices_global.csv"))
#all_data_global<-all_data_global[,1:9]
all_data<-rbind(all_data,all_data_global)
all_data <- dplyr::rename(all_data, Sum = Value)

all_data_bio <- read_csv(file.path(root_tables,"Biodiversity_regional.csv"))
all_data_bio_global <- read_csv(file.path(root_tables,"Biodiversity_global.csv"))
all_data_bio_global <- all_data_bio_global[,1:11]  #last column of bio_global is not needed
all_data_bio_global <- subset(all_data_bio_global,LUCC=="LUCC")
all_data_bio <- rbind(all_data_bio [, 1:11], all_data_bio_global)
all_data_bio <- dplyr::rename(all_data_bio, Sum = Value)
all_data_bio <- dplyr::rename(all_data_bio, NCP = Family)

#### 3 - Subset and recode data ----


# Change signal
all_data$Sum<-ifelse(all_data$NCP=="Hazards", all_data$Sum*(-1),all_data$Sum)
all_data$Sum<-ifelse(all_data$NCP=="Water quality",all_data$Sum*(-1),all_data$Sum)

## add a code for NCP category of Service (M - Material R-Regulator)
all_data$NCP_cat<-ifelse(all_data$NCP =="Bioenergy" | all_data$NCP =="Food and feed"
                         | all_data$NCP =="Materials","M","R")

# subset relevant biodiversity data
all_data_bio <- subset(all_data_bio, NCP=="Sgamma" & Model %in% c("MOLSDM","BILBI","INSIGHTS"))

# calculate average for the three taxa of MOLSDM to avoid it dominates the biodiversity metrics summary
molsdm <- subset(all_data_bio, Model=="MOLSDM")
res <- molsdm %>% group_by(across(c(-Sum,-Taxa))) %>%
          dplyr::summarize(ValueMean=mean(Sum)) %>% ungroup()
res <- dplyr::rename(res, Sum = ValueMean)
res$Taxa <- do.call(paste,as.list(unique(molsdm$Taxa)))
all_data_bio<-subset(all_data_bio,  Model!="MOLSDM")
all_data_bio<-rbind(all_data_bio,res)

# merge biodiversity and ecosystem services data
all_data_bio$NCP_cat="B"
all_data<-merge(all_data,all_data_bio,all=TRUE)

# create unique column combining Model and Metric 
all_data<-transform(all_data, Model_Metric = paste(Model,Metric))

# remove historical data
all_data<-subset(all_data,Scenario!="Historical")

# drop columns not needed
#all_data<-all_data[,c(1:10,19)]

#### 4 - Calculations ----
unique(all_data$Model)
unique(all_data$Model_Metric)
all_data %>% group_by(NCP_cat)%>% 
  dplyr::summarise(n_metric= n_distinct(Model_Metric),n_model= n_distinct(Model))-> stats
stats
#write.csv(all_data,"./data_global.csv")

## Max calculation across all regions and all scenarios for each unique model metric calculation ##
max<-NULL
list<- unique(all_data$Model_Metric)
for (i in 1:length(list)){
  
  CR<-subset(all_data,Model_Metric==list[i])
  CR<-droplevels(CR)
  CR$Model<-"Multiple"
  CR1 <- max(abs(CR$Sum),na.rm = TRUE)
  CR2 <-cbind(CR[1,"Model_Metric"],as.numeric(CR1))
  colnames(CR2)<-c("Model_Metric","MAX")
  max<-rbind(max,CR2)  ##add to all dataset
}
max<-as.data.frame(max)

max$MAX<-as.numeric(as.character(max$MAX))
# Add sd column to main data
all_data1<-merge(all_data,max, by.x = c("Model_Metric"), by.y = c("Model_Metric"))

# make the SUM/max and store it in the column sum_sd
all_data1$sum_sd<-all_data1$Sum/all_data1$MAX

all_data1<-droplevels(all_data1)
final_data <- summarySE(all_data1, measurevar="sum_sd", groupvars=c("NCP_cat","Scenario","Region"),.drop=TRUE)

write_csv2(final_data,paste0(root_outputs,"final_data.csv"))
write.csv2(final_data,paste0(root_outputs,"BD_ES_regional_data_paper9.csv"))


#### 5 - Plots ####
# BD and ES data
# 3 scenarios
final_data$NCP_cat<-factor(final_data$NCP_cat, levels =  c("B", "M","R"))
#final_data$Region<-as.factor(final_data$Region)

##Correct names in data
#levels(final_data$Region) = c(levels(final_data$Region),"South-East Asia", "North-East Asia")
final_data$Region[final_data$Region=="South East Asia"]<-"South-East Asia"
final_data$Region[final_data$Region=="North East Asia"]<-"North-East Asia"
#final_data$Region<-droplevels(final_data$Region)
unique(final_data$Region)

## Shapefiles of regions

map_simp.kt<- sf::st_read(file.path(root_IPBES_Regions,"IPBES_subregions_simp_v6.shp"))

#the code below not needed if one uses saved centroid positions 
# map.kt <- sf::st_read(file.path(root_IPBES_Regions,"IPBES_subregions.shp"))
# centroids_corr <- read_delim("Figure4_centroids_correction.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)
# map.kt2 = sf::st_make_valid(map.kt)
# map.test.centroids <-sf::st_centroid(map.kt2)
# map.test.centroids1 <- as.data.frame(map.test.centroids)
# 
# map.test.centroids1 <- map.test.centroids1 %>%
#   mutate(long = unlist(map(map.test.centroids1$geometry,1)),
#          lat = unlist(map(map.test.centroids1$geometry,2)))
# 
# map.test.centroids1$x<-map.test.centroids1$long+centroids_corr$x
# map.test.centroids1$y<-map.test.centroids1$lat+centroids_corr$y
# map.test.centroids1$OBJECTID <- row.names(map.test.centroids1)
# 
# write.csv2(map.test.centroids1,"Figure4_mapcentroids.txt")

# the point fo Europe was out of place and I moved it 30 degrees to the left (now 10,5327451358187, before 40,5327451358187)

map.test.centroids1<-read.csv2("Figure4_mapcentroids.csv")
#corrected objectID and ordered to be the same as in the IPBES_subregions_simp_v6, 8.2.2024

map<-ggplot()+
  geom_sf(data = map_simp.kt, 
          fill = "grey91", colour = "grey76",size=0.2)+
  theme(legend.position = "none", rect = element_blank(),
        line = element_blank(), text = element_blank())
map

#set limits
#lim<-final_data
#lim$sumsdplus<-final_data$sum_sd+final_data$sd
#lim$sumsdminus<-final_data$sum_sd-final_data$sd

ssp<-c("SSP1","SSP3","SSP5")
ssp2<-c("SSP1xRCP2.6","SSP3xRCP6.0","SSP5xRCP8.5")
#b<-c("#D36027","#6F94CC","#059E73")
b<-c('#CC79A7','#E69F00','#F0E442')
scales::show_col(b)

#length(map.kt$IPBES_sub)
length(map_simp.kt$IPBES_sub)
length(unique(map_simp.kt$IPBES_sub))
map_simp.kt$IPBES_sub

# draws regional maps and exports maps
for (i in 1:3){
  print(ssp[i])
  
  # Select model 
  final_data1<-subset(final_data, final_data$Scenario==ssp[i] | final_data$Scenario==ssp2[i])
  final_data_global<-subset(final_data, final_data$Region=="Global" & final_data$Scenario==ssp2[i])
  final_data1<-subset(final_data1, final_data1$Region!="Global")
  ###to make in the same order that in the shapefile
  final_data1$Region<-factor(final_data1$Region, levels = unique(map_simp.kt$IPBES_sub))
  final_data1 <- arrange(final_data1,Region, factor(Region))
  #final_data1 <- arrange(final_data1,NCP_cat, factor(NCP_cat))
  
  ## Make limits ##
  lim<-final_data1
  lim$sumsdplus<-final_data1$sum_sd+final_data1$sd
  lim$sumsdminus<-final_data1$sum_sd-final_data1$sd
  # # min(lim$sumsdminus,na.rm = TRUE)
  # # max(lim$sumsdplus)
  # 
  print(Sys.time())
  
  # draws regional bar plots
  bar.testplot_list <- 
    lapply(unique(final_data1$Region), function(j) { 
      
      gt_plot <- ggplotGrob(
        ggplot(final_data1[final_data1$Region == j,], aes(x=NCP_cat, y=sum_sd, fill=NCP_cat)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
          geom_hline(yintercept=0, color = "grey")+
          geom_bar(stat="identity")+
          geom_errorbar(aes(ymin=sum_sd-se, ymax=sum_sd+se),color="grey55", width=.1)+
          ylim(min(lim$sumsdminus,na.rm = TRUE)-0.5,max(lim$sumsdplus,na.rm = TRUE)+0.5) +
          labs(x = NULL, y = TRUE)+
          scale_fill_manual(values = b)+
          #theme_minimal() +
          # theme(legend.position = "none", rect = element_blank(),
          #       line = element_blank(),
          #       aspect.ratio=4/1  # change to 1 when running for 3/4 variables
          theme(axis.text.y = element_text(size=4),
                legend.position = "none", rect = element_blank(),
                line = element_blank(),
                aspect.ratio=5  # change to 1 when running for 3/4 variables
          )
      )
      panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
      gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
    })

  unique(final_data1$Region)
  map_simp.kt$IPBES_sub
  bar_annotation_list <- lapply(1:length(unique(map_simp.kt$OBJECTID)), function(j) 
    annotation_custom(bar.testplot_list[[j]], 
                      xmin = map.test.centroids1$x[j] - 35,
                      xmax = map.test.centroids1$x[j] + 35,
                      ymin = map.test.centroids1$y[j] - 35,
                      ymax = map.test.centroids1$y[j] + 35) )
  
  # draws global bar plot
global_bar<-ggplot(final_data_global, aes(x=NCP_cat, y=sum_sd, fill=NCP_cat)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_hline(yintercept=0, color = "grey")+
    geom_bar(stat="identity")+
    geom_errorbar(aes(ymin=sum_sd-se, ymax=sum_sd+se),color="grey55", width=.1)+
    ylim(min(lim$sumsdminus,na.rm = TRUE)-0.5,max(lim$sumsdplus,na.rm = TRUE)+0.5) +
    labs(x = NULL, y = TRUE)+
    scale_fill_manual(values = b)+
    facet_wrap(Region~.)+
    #theme_minimal() +
    # theme(legend.position = "none", rect = element_blank(),
    #       line = element_blank(),
    #       aspect.ratio=4/1  # change to 1 when running for 3/4 variables
    theme(axis.text.y = element_text(size=4),
          legend.position = "none", rect = element_blank(),
          line = element_blank(),
          aspect.ratio=4/1)# change to 1 when running for 3/4 variables
  
  result_plot <- Reduce(`+`, bar_annotation_list, map)
  
  savePlot <- function(myPlot, X) {
    pdf(X, width = 10.93, height = 4.97, paper="a4r")
    print(myPlot)
    dev.off()
  }
  
  path <- file.path(root_figures,paste0("Figure4_",ssp[i], "_Regions_8",".pdf"))
  savePlot(result_plot,path)
  path2 <- file.path(root_figures,paste0("Figure4_",ssp[i], "_Global_8",".pdf"))
  savePlot(global_bar,path2)
}

result_plot

