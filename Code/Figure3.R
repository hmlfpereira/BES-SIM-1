### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science https://doi.org/10.1126/science.adn3441
### Create input data and plots for global ecosystem service metrics
### Project BES SIM 1
### Created September 2018, Ines S. Martins 
### Revised Nov 2023, Henrique Pereira
### Revised Feb 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

#load packages
library(ggplot2)
library(dplyr)
library(Rmisc)
library(ggpubr)
library(ggpattern)
library(rlang)

#set paths
#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root_tables<-"../Data_Tables/"
#directory with data tables with outputs of biodiversity and ecosystem service models containing
#DataS1_Biodiversity.xlsx
#DataS2_EcosystemServices.xlsx

root_figures <- '../Figures/'
#directory where figure outputs will be saved

root_outputs<-"../Outputs/"
#directory where other outputs and analysis will be saved

# auxiliary function to save plots #
savePlot <- function(myPlot, X) {
  pdf(X, width = 11.69, height = 8.27)
  print(myPlot)
  dev.off()
}

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
  #datac <- dplyr :: rename(datac, !!measurevar := mean)
  datac <-  plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

### 2 - Import data ----

all_data<-read.csv(file.path(root_tables,"EcosystemServices_global.csv"))

### 3 - Subset and recode data ----

all_data$in_fig<-all_data$Value

## add a code for NCP category of Service (M - Material R-Regulator)
all_data$NCP_cat<-ifelse(all_data$NCP =="Bioenergy" | all_data$NCP =="Food and feed"
                         | all_data$NCP =="Materials","M","R")

# select only change data and only LUCC
all_data<-subset(all_data, (Region=="Global" & Units == "%" & Years == "2015-2050" & LUCC == "LUCC")|
         (Region=="Global" & Units == "%" & Years == "1920-2015" & LUCC == "LUCC")|
         (Region=="Global" & Units == "%" & Years == "1900-2015" & LUCC == "LUCC"))

# recode scenario
all_data$Scenario<-as.character(all_data$Scenario)

### 4 - Calculations ----

#Calculate decadal change rates
first<- as.numeric(substr(all_data$Years, 0, 4))
last<- as.numeric(substr(all_data$Years, 6, 10))
int<-last-first
#all_data$in_fig<-(all_data$in_fig/int)
all_data$in_fig<-((1+all_data$in_fig/100)^(10/int)-1)*100

# Change signal
all_data$in_fig<-ifelse(all_data$NCP=="Hazards", all_data$in_fig*(-1),all_data$in_fig)
all_data$in_fig<-ifelse(all_data$NCP=="Water quality",all_data$in_fig*(-1),all_data$in_fig)

# create unique identifier combining model and metric
all_data$Model_Metric<-paste(all_data$Model,all_data$Metric,sep=" ")

### 5 - Make plots ----
#### Prepare plots----
# Calculate two sets of summaries
# 1- model metrics with historical
# 2 - model metrics without historical or model metrics with historical for scenarios
MM_with_hist <-unique(subset(all_data,Scenario=="Historical")$Model_Metric)
data1<-subset(all_data,(Model_Metric %in% MM_with_hist))
data2<-subset(all_data,!(Model_Metric %in% MM_with_hist) |
                (Model_Metric %in% MM_with_hist)&(Scenario!="Historical"))

# calculate summaries
plot_data1<- summarySE(data1, measurevar="in_fig", 
                       groupvars=c("NCP","NCP_cat","Scenario","Region"),.drop=TRUE)
plot_data2<- summarySE(data2, measurevar="in_fig", 
                       groupvars=c("NCP","NCP_cat","Scenario","Region"),.drop=TRUE)

write.csv2(rbind(plot_data1, plot_data2),file.path(root_outputs,"Global_NCP_Model_Metric.csv"))

#model metrics with historical data in light
#create placeholders NCPs without historical data
NCP_with_hist <-unique(subset(all_data,Scenario=="Historical")$NCP)
plot_data11<-subset(plot_data2,!(NCP %in% NCP_with_hist))
plot_data11$in_fig<-0
plot_data11$se<-NA
plot_data111<-subset(plot_data11,Scenario=="SSP1xRCP2.6")
plot_data1<-rbind(plot_data1,plot_data11,plot_data111)
plot_data1$Color<-"light"

#model metrics without historical data in dark
#create placeholders for NCPs without historical data for historical bars
plot_data22<-subset(plot_data2, Scenario=="SSP1xRCP2.6")
plot_data22$Scenario<-"Historical"
plot_data22$in_fig<-0
plot_data22$se<-NA
plot_data22[plot_data22$NCP=="Water quality",]<-
  subset(plot_data1,NCP=="Water quality")
plot_data2<-rbind(plot_data2,plot_data22)
plot_data2$Color<-"dark"

# merge both NCP's bar groups
plot_data<-rbind(plot_data1,plot_data2)

# sort data and arrange levels
plot_data<-arrange(plot_data,NCP_cat,NCP,Scenario)
plot_data<-droplevels(plot_data)
plot_data$NCP<-factor(plot_data$NCP, levels=unique(plot_data$NCP), ordered = TRUE)

#all_data1<-all_data
#all_data1$Scenario <- as.factor(all_data1$Scenario)
#plot_data$Scenario<-as.factor(all_data$Scenario)

#### Plotting ----

#create place holders for empty historical points
row1<-all_data[1,]
row1$Scenario="Historical"
row1$NCP="Hazards"
row1$in_fig=NA
all_data1<-rbind(all_data,row1)
row1$NCP="Soils"
row1$in_fig=NA
all_data1<-rbind(all_data1,row1)
row1$NCP="Detrimental organisms"
row1$in_fig=NA
all_data1<-rbind(all_data1,row1)
unique(all_data$NCP)


#b<-c("#56B4E9","#009E73","#D55E00","#FCBF10")
b<-rev(c("#F2C45F", "#B5520F","#00681C","#56B4E9"))

P1<-ggplot() +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_hline(yintercept=0, color = "grey")+
  geom_bar(data = subset(plot_data, Color=="light"),
           aes(x=NCP, y=in_fig, fill=Scenario), stat="identity",
           
           position = "dodge", alpha=0.4
  ) +  #alpha=0.4
  geom_bar(data= subset(plot_data, Color=="dark"),
           aes(x=NCP, y=in_fig, fill=Scenario), stat="identity",
           position = "dodge", alpha=1.0) +
  # geom_errorbar(data=plot_data, aes(x=NCP,ymin=in_fig-se, ymax=in_fig+se, fill=Scenario),
  #             position = position_dodge(0.9),color="black", width=.3)+
  geom_point(data=all_data1, 
             aes(y=in_fig,shape=Model, x=NCP, group = Scenario), position=position_dodge(1.0),size=3) +
  scale_shape_manual(values = 8:12) +
  #scale_size_manual(values = rep(10,5)) +
  labs(x="NCP", y="% change per decade")+
  scale_fill_manual(values = rev(b))+
  geom_text(data=subset(plot_data1,
                        plot_data1$Scenario=="SSP5xRCP8.5"),
            aes(x=NCP, y=in_fig+2, label=NCP),check_overlap = FALSE, nudge_y = 0.2,size = 3)+
  ylim(min(plot_data$in_fig)*1.2,max(plot_data$in_fig)*1.1) +
  theme_bw() +
  theme(strip.text= element_text(size = 15, colour = "black"),
        axis.text.x = element_blank(),
        axis.text.y=element_text(size=10),
        legend.title=element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.position = c(0.8, 0.8))

P1

savePlot(P1,file.path(root_figures,"Figure3_EcosystemServicesMetrics.pdf"))

