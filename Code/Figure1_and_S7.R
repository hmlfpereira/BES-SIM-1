### Pereira et al. (2024). Global trends and scenarios for terrestrial biodiversity and ecosystem services from 1900-2050. Science.
### Figure 1 ----
### Create input data and plots for global biodiversity metrics
### Project BES SIM 1
### Created April 2019, Isabel Rosa & Ines S. Martins
### Revised Nov 2023, Henrique Pereira
### Revised Dec 2023, Henrique Pereira
### Revised Feb 2024, Henrique Pereira

### 1 - Initializations ----
# clear workspace
rm(list=ls())

# load packages
library(ggplot2)
library(Rmisc)
library(grid)
library(gridExtra)
library(dplyr)
library(stringr)

# set working directory
#setting working directory to the current file source location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  #only works R studio
#alternatively setwd(getSrcDirectory(function(){})[1])

root_tables<- "../Data_Tables/"
#directory with data tables with outputs of biodiversity and ecosystem service models containing
#DataS1_Biodiversity.xlsx
#DataS2_EcosystemServices.xlsx

root_figures<- "../Figures/"
#directory where figure outputs will be saved

root_outputs<- "../Outputs/"
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
  print(data)
  print(datac)
  datac <- rename(datac, !!measurevar := mean)
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# auxiliary function to save plots
savePlot <- function(myPlot, X) {
  pdf(X, width = 11.69, height = 8.27)
  print(myPlot)
  dev.off()
}

# function to calculate geometric change per time

geomchange<-function(deltay,deltatime)
{
  ((1+deltay/100)^(1/deltatime)-1)*100
}

b<-c("#56B4E9","#009E73","#D55E00","#FCBF10")
#b<-c("#56B4E9","#D55E00","#009E73","#FCBF10")

### 2 - Import data ----
data_all<-read.csv(file.path(root_tables,"Biodiversity_global.csv"))

# data for Figure 1
#Use only units in %
data2<-subset(data_all,Units %in% c("%"))

#rename scenarios
data2$Scenario<-str_replace_all(data2$Scenario,
                                  c("SSP1xRCP2.6"="Global sustainability",
                                    "SSP3xRCP6.0"="Regional rivalry",
                                    "SSP5xRCP8.5"="Fossil fuel developm."))

### 3 - Calculations ----
# normalize to decadal change ranges
data2$DecadalValue<-data2$Value
data2$DecadalValue[data2$Scenario=="Historical"]<-
  geomchange(data2$Value[data2$Scenario=="Historical"],11.5)
#  data2$Value[data2$Scenario=="Historical"]/115
data2$DecadalValue[data2$Scenario!="Historical"]<-
  geomchange(data2$Value[data2$Scenario!="Historical"],3.5)
#  LU_data$Value[LU_data$Scenario!="Historical"]/35

# select the non-diversity weighted metrics
data2 <- subset(data2,Family!="SSalpha")
# select Years
data2<-subset(data2, Years %in% c("1900-2014","1900-2015","2015-2050"))
# add the LUCC historical data point to Insights by using the LU value
#newrow<-subset(data2,Model=="INSIGHTS"&Scenario=="Historical")
#newrow$LUCC<-"LUCC"
#data2<-rbind(data2,newrow)

# data for Figure S7 for plotting different taxa
data3 <-  subset(data2, (Model=="AIM" & (Family=="Hgamma" | Family=="Salpha") & 
                           Years %in% c("1900-2015","2015-2050")) |
                   (Model=="cSAR-IIASA" & Family=="Salpha" & Years %in% c("1900-2015","2015-2050")))

# Select only "All taxa" and only relevant models
data2 <- subset(data2, (Model=="AIM" & Taxa=="All") |
                  (Model=="cSAR-iDiv" & Taxa=="Birds") |
                  (Model=="cSAR-IIASA" & Taxa=="All") |
                  (Model=="BILBI" & Taxa=="Plants") |
                  (Model %in% c("INSIGHTS","PREDICTS","GLOBIO")))
unique(cbind(data2$Model, data2$Taxa))

# remove cSAR-IIASA gamma 
#data2 <- subset(data2, (Model!="cSAR-IIASA" | Family!="Sgamma")) 

# select only NoDispersal (only relevant for AIM)
data2<-subset(data2, !str_detect(data2$Metric,"full dispersal"))


# summarize across models for each metric
tgc <- summarySE(data2, measurevar="DecadalValue", groupvars=c("Scenario","Family","LUCC"))

### 4 - Plots ----
#### a) Figure 1----

#reorder factor levels
LU_data<-data2
LU_data$Family<-as.factor(LU_data$Family)
levels(LU_data$Family)
LU_data$Family <-factor(LU_data$Family,levels(LU_data$Family)[c(2,1,3,4)]) 
levels(LU_data$Family)

tgc$Family<-as.factor(tgc$Family)
tgc$Family<-factor(tgc$Family,levels(tgc$Family)[c(2,1,3,4)]) 
write.csv2(tgc,file.path(root_outputs,"Figure1_intermodel.csv"))

# tgc$LUCC[tgc$LUCC=="LU"]<-"Land Use"
# tgc$LUCC[tgc$LUCC=="LUCC"]<-"Land Use and Climate Change"


tgc$Scenario <- factor(tgc$Scenario, levels = c("Fossil fuel developm.","Regional rivalry",
                                    "Global sustainability","Historical"))
#tgc$Scenario <- factor(tgc$Scenario, levels = c("SSP5xRCP8.5","SSP3xRCP6.0","SSP1xRCP2.6","Historical"))

LU_data$Scenario <- factor(LU_data$Scenario, levels = c("Fossil fuel developm.","Regional rivalry",
                                               "Global sustainability","Historical"))
#LU_data$Scenario <- factor(LU_data$Scenario, levels = c("SSP5xRCP8.5","SSP3xRCP6.0","SSP1xRCP2.6","Historical"))

plot_global<-ggplot(tgc, aes(y=DecadalValue, x=Family, group = Scenario)) +
  geom_col(aes(fill = Scenario),position = "dodge") +
  scale_shape_manual(values = 0:6) +
  geom_point(data=LU_data,aes(y=DecadalValue,shape=Model, x=Family, group = Scenario),position=position_dodge(0.9)) +
  #geom_text(aes(label = N, y = 0.1), color = "black", position = position_dodge(0.9),vjust = 0)+
  scale_fill_manual(values = b)+
  #geom_errorbar(aes(ymin=DecadalValue-se, ymax=DecadalValue+se), width=.2, position=position_dodge(0.9))+
  scale_colour_manual(values = b)+
  coord_flip()+ 
  scale_y_continuous(name= "Relative change per decade (%)") +
  scale_x_discrete(name= "Biodiversity metrics", 
                   labels=c(expression(""*Delta*"I"[alpha]*""),expression(""*Delta*"H"[gamma]*""),expression(""*Delta*"S"[alpha]*""),expression(""*Delta*"S"[gamma]*""))) +
  # scale_x_discrete(name= "Intermodel biodiversity metrics", 
  #                  labels=c(expression(paste("Biodiversity Intactness (I"[alpha],")")),expression(paste("Global mean Habitat extent (H"[gamma],")"),expression(paste("Local mean Habitat extent (H"[alpha],")"),expression("S"[alpha]))) +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=12), axis.title.x=element_text(size=12))+
  facet_wrap(~LUCC)+
  theme_bw()+
  theme(#panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position =  c(0.1, 0.6),
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12, colour = "black"),
    strip.text.x = element_blank(),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12))

plot_global +
  guides(fill = guide_legend(reverse = TRUE))

#save plot
path <- paste(root_figures,"Figure1_BiodiversityMetrics.pdf",sep="")
ggsave(file=path,plot_global  +
         guides(fill = guide_legend(reverse = TRUE)), width = 11.69, height = 11.27 * 2/3)

#### b) Figure S7----
# add plot of AIM zgamma by taxa
# add plot of cSAR IIASA gamma by taxa
data4 <- subset(data3, Family %in% c("Sgamma","Hgamma","Salpha"))
data4 <- subset(data4,!Model=="AIM" | Taxa == "All" | str_detect(Metric, "no dispersal"))
data4 <- subset(data4,!Model=="cSAR-IIASA" | Taxa != "All") 
data4$Taxa[str_detect(data4$Metric,"full dispersal") & data4$Taxa=="All"] <- "All with full dispersal"
tgc3<-subset(tgc,  Family %in% c("Hgamma","Salpha"))
data4$Scenario <- factor(data4$Scenario, levels = c("Fossil fuel developm.","Regional rivalry",
                                                      "Global sustainability","Historical"))

plot_global2<-ggplot(tgc3, aes(y=DecadalValue, x=Family, group = Scenario)) +
  geom_col(aes(fill = Scenario), position = "dodge") +
  scale_shape_manual(values = c(0,15,1:6)) +
  geom_point(data=data4,aes(y=DecadalValue,shape=Taxa, color=Model, x=Family, size=Taxa,
                            group = Scenario),position=position_dodge(0.9)) +
  scale_size_manual(values = c(3,3,rep(2,5))) + 
  #geom_text(aes(label = N, y = 0.1), color = "black", position = position_dodge(0.9),vjust = 0)+
  scale_fill_manual(values = b)+
  #geom_errorbar(aes(ymin=DecadalValue-se, ymax=DecadalValue+se), width=.2, position=position_dodge(0.9))+
  scale_colour_manual(values = c("Black","Grey45"))+
  coord_flip()+ 
  scale_y_continuous(name= "Relative change per decade (%)") +
  scale_x_discrete(name= "Intermodel biodiversity metrics", 
                   labels=c(expression(""*Delta*"H"[gamma]*""),expression(""*Delta*"S"[alpha]*""))) +
  # scale_x_discrete(name= "Intermodel biodiversity metrics (mean +- s.e.)", 
  #                  labels=c(expression(paste("Biodiversity Intactness (I"[alpha],")")),expression(paste("Global mean Habitat extent (H"[gamma],")"),expression(paste("Local mean Habitat extent (H"[alpha],")"),expression("S"[alpha]))) +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=12), axis.title.x=element_text(size=12))+
  facet_wrap(~LUCC)+
  theme_bw()+
  theme(#panel.border = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position =  c(0.1, 0.5),
    strip.background = element_blank(),
    strip.text.y = element_text(size = 12, colour = "black"),
    strip.text.x = element_blank(),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12))

print(plot_global2)  +
  guides(fill = guide_legend(reverse = TRUE))
path <- paste(root_figures,"FigureS7_BiodiversityMetrics.pdf",sep="")
ggsave(file=path,plot_global2  +
         guides(fill = guide_legend(reverse = TRUE)), width = 11.69, height = 1/2 * 11.27)

# calculate coeeficient of variation
tgc2 <- na.omit(tgc) %>% mutate(cv=se/DecadalValue)

