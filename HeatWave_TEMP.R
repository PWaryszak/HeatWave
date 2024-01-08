#Load R Packages:
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lme4)) install.packages('lme4')
if (!require(lmerTest)) install.packages('lmerTest')
if (!require(gridExtra)) install.packages('gridExtra')
if (!require( scales)) install.packages('scales')

library(tidyverse)
library(lme4)
library(lmerTest)
library(gridExtra)
library(scales)


#LOAD DATA (Heat Wave Experiment):
# Data frame for overlaid graph:
getwd() #Put all data in this directory.
HOBO_temp_data_1 <- read.csv("HOBO_R_data")
head(HOBO_temp_data_1,n=2)
#            date_time Air_temp HOBO Treatment
#1 2022-03-02 14:24:00    13.40  HB1    Heated
#2 2022-03-02 14:25:00    14.06  HB1    Heated

#write.csv(HOBO_temp_data_1, "HOBO_R_DATA.csv", row.names = F) #Re-write R_data into excel spreadsheet
#HOBO_temp_data_1 <- read.csv("HOBO_R_data.csv") # read-in spreadsheet instead. Same Data, different format


dim(HOBO_temp_data_1)#351828      4
length(unique(HOBO_temp_data_1$HOBO)) #27 HOBO loggers, Suz used only 16

HOBO_temp_data_1$Treatment[HOBO_temp_data_1$Treatment== "CC"]<- "Chamber only"  #CC = Chamber Control

#Filter out HOBO loggers used in Susanna Venn's heatwave experiment:
hobo_match <- read_excel("MASTER_HeatWave_HoboData.xlsx", sheet = "LoggerID_Freya2Venn") #Use matched spreadsheet where Venn HOBO with Freya's HOBO


HOBO_temp_data_2 <- HOBO_temp_data_1 %>% #To filter out 16 out 27 HOBO loggers used in Susanna's heat wave experiment:
  filter(HOBO %in% hobo_match$HOBO)

unique(HOBO_temp_data_2$HOBO)
length(unique(HOBO_temp_data_2$HOBO)) #16 - it should be 16 YAY!

HOBO_temps <- HOBO_temp_data_2 %>% mutate(
  Day = 
    {case_when(
      date_time >'2022-03-02 08:00:00'& date_time <'2022-03-03 08:00:00'~ "1",
      date_time >='2022-03-03 08:00:00'& date_time <'2022-03-04 08:00:00'~ "2",
      date_time >='2022-03-04 08:00:00'& date_time <'2022-03-05 08:00:00'~ "3",
      date_time >='2022-03-05 08:00:00'& date_time <'2022-03-06 08:00:00'~ "4",
      date_time >='2022-03-06 08:00:00'& date_time <'2022-03-07 08:00:00'~ "5",
      date_time >='2022-03-07 08:00:00'& date_time <'2022-03-08 08:00:00'~ "6",
      date_time >='2022-03-08 08:00:00'& date_time <'2022-03-09 08:00:00'~ "7"
    )}) %>%       #adding day number of experiment 
  filter(!(HOBO== "HB2"|
             HOBO == "HB5"))%>% #filter out HB2 and HB5 as the heater in block 8 died
  spread(
    HOBO, Air_temp  ) %>%               # creating wide df, averaging across all 24 HOBO loggers ([,4:28])
  filter(
    date_time>'2022-03-02 14:00:00'& 
      date_time <'2022-03-08 07:30:00')%>%  #filtering out to days between 2-8 Mar
  mutate(
    average =
      rowMeans(.[,4:16], na.rm = T)) %>% #adding averages across treatments 
  
  .[,c(1:3,19)] %>%  # filtering out raw data to only leave averages
  
  mutate(
    time =
      format(as.POSIXct(.$date_time,format="%Y-%m-%d %H:%M:%S"),format='%H:%M:%S'))  %>%
  filter(Day== "1"|
           Day== "3"|
           Day == "6") %>%  # filtering out days 1, 3, and 6 only 
  
  mutate(
    target = 
      case_when(
        time >='00:00:00'& time <'08:00:00'~ 22, 
        time >='08:00:00'& time <'20:00:00'~ 32 ,
        time >='20:00:00'& time <'23:59:59'~ 22))%>%  #adding in corresponding target temperatures
  mutate(
    date=
      case_when(
        time >='08:00:00'& time <'23:59:59'~ "2022-03-03",
        time >='00:00:00'& time <'08:00:00'~ "2022-03-04")) %>%
  mutate(
    date_time_chr=
      paste(date, time))%>%
  mutate(
    time_3=
      as.POSIXct(date_time_chr,format="%Y-%m-%d %H:%M:%S")) ## My very convoluted way at getting the right time stamp for the graphs
#need to filter out 7.30 am -8am on day 3 to account for weather effects 


HOBO_temps$time_2 <- as.POSIXct(HOBO_temps$time,format="%H:%M:%S")###This changes the date to the current date of when the code was run, problem for annotating graphs later, need to figure out a work around 


HOBO_temps2 <- HOBO_temps %>%
  filter(!(Day=="3" &
             Treatment=="Heated" &
             date_time>"2022-03-05 06:30:00")) %>%
  filter(!(Day=="6" &
             Treatment=="Heated" &
             date_time>"2022-03-08 06:30:00")) ### filtering out 6.30am-8am hours on days 3 and 6 due to weird heating effects and weather skewing the smoothed averages


#Prep for graph formatting #####

### labels to get right time stamps in the graph
my_labels<- c("08:00:00", "14:00:00", "20:00:00","02:00:00","08:00:00","06:00:00")
my_labels<- c("08:00", "14:00", "20:00","02:00","08:00","06:00")

### adding levels to Treatment variable to get Legend in preferred order:
unique(HOBO_temps2$Treatment)
#HOBO_temps2$Treatment <- factor(HOBO_temps2$Treatment, levels = c("Heated", "Chamber only", "Control"))

#Graph HEAT WAVE TEMP:============  
ggplot(HOBO_temps2, 
       aes(x=time_3, 
           y= average,
           color= Treatment , 
           linetype = Day )) +
  geom_line(alpha=0)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs="tp", k =5), se = F) + # k=5 is my current fave but could adjust depending on desired smoothing effect \
  scale_color_manual(values = c( "#009E70","#D55E00"))+
  scale_linetype_manual(values=c("dotted","dashed","solid"))+
  theme_bw()+
  
  ggtitle("")+ #Heat Wave Experiment
  
  annotate("rect",
           xmin = as.POSIXct("2022-03-03 20:00:00", format= "%Y-%m-%d %H:%M:%S")  ,
           xmax = as.POSIXct("2022-03-04 08:00:00", format= "%Y-%m-%d %H:%M:%S"),
           ymin = -Inf, ymax = Inf,
           fill = alpha("white", .17))+    # to get grey shading in to show night-time hours use "grey" instead of "white
  
  labs(y = (bquote(Air~Temperature~("\u00B0"~C))))+ #this created a sqaure=#xlab = expression("Temperature " ( degree*C))
  
  scale_x_datetime(name = "Time (hh:mm)", 
                   breaks = date_breaks("2 hours"),
    labels = date_format("%H:%M"),               
    #labels = my_labels , #edit away if you want to change lables on x-axis
                   expand = c(0,0))+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
    
        plot.margin = margin(1,1,1.5,1, "cm"), #control the white space around the plot,fits the 22:00 in
        
        legend.position = c(.9, .75),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"))


ggsave(file= "HeatWave_Temperature_Plot_Simpler2200.jpg", width = 13, height = 7)



#Supplementary Graph HEAT WAVE TEMP with GAM on:============  
#This figure can go to supplementary to show the temp variation before smoothing effet of GAM:

ggplot(HOBO_temps2, 
       aes(x=time_3, 
           y= average,
           color= Treatment , 
           linetype = Day )) +
  geom_line(alpha=0.3)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs="tp", k =5), se = F) + # k=5 is my current fave but could adjust depending on desired smoothing effect \
  scale_color_manual(values = c( "#009E70","#D55E00"))+
  scale_linetype_manual(values=c("dotted","dashed","solid"))+
  theme_bw()+
  
  ggtitle("")+ #Heat Wave Experiment
  
  annotate("rect",
           xmin = as.POSIXct("2022-03-03 20:00:00", format= "%Y-%m-%d %H:%M:%S")  ,
           xmax = as.POSIXct("2022-03-04 08:00:00", format= "%Y-%m-%d %H:%M:%S"),
           ymin = -Inf, ymax = Inf,
           fill = alpha("grey", .17))+    # to get grey shading in to show night-time hours use "grey" instead of "white
  
  labs(y = (bquote(Air~Temperature~("\u00B0"~C))))+ #this created a sqaure=#xlab = expression("Temperature " ( degree*C))
  
  scale_x_datetime(name = "Time (hh:mm)", 
                   breaks = date_breaks("2 hours"),
    labels = date_format("%H:%M"),               
    #labels = my_labels , #edit away if you want to change lables on x-axis
                   expand = c(0,0))+
  theme(axis.text.x=element_text(vjust=0.5,size=16, color="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20,vjust=-0.5),
        
        legend.position = c(.9, .75),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"))

