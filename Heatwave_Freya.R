#Load Libraries and Notes:
library(tidyverse)
library(readxl)

#Here are the dates that Virginia and I took our measurements.
#Day 0: 2nd March
#Day 3: 5th March
#Day 6: 8th March



# Creating right formatted dataframe==========
#for overlaid graph
# Data frame ###

HOBO_temp_data_1 <- read.csv("HOBO_R_data")
head(HOBO_temp_data_1)
dim(HOBO_temp_data_1)#351828      4
length(unique(HOBO_temp_data_1$HOBO)) #27 HOBO loggers

HOBO_temp_data_1$Treatment[HOBO_temp_data_1$Treatment== "CC"]<- "Chamber only"

HOBO_temps <- HOBO_temp_data_1 %>% mutate(
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
    HOBO, Air_temp
  ) %>%               # creating wide df
  filter(
    date_time>'2022-03-02 14:00:00'& 
      date_time <'2022-03-08 07:30:00')%>%  #filtering out to days between 2-8 Mar
  mutate(
    average =
      rowMeans(.[,4:28], na.rm = T))%>%     #adding averages across treatments 
  .[,c(1:3,29)]%>%  # filtering out raw data to only leave averages
  mutate(
    time =
      format(as.POSIXct(.$date_time,format="%Y-%m-%d %H:%M:%S"),format='%H:%M:%S'))  %>%
  filter(Day== "2"|
           Day== "3"|
           Day == "6") %>%  # filtering out days 2, 3, and 6 only 
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



HOBO_temps2$Treatment[HOBO_temps2$Treatment== "CC"]<- "Chamber only"
HOBO_temps2$Treatment[HOBO_temps2$Treatment== "Actively Heated"]<- "Heated"

#Prep for graph formatting #####

### labels to get right time stamps in the graph
my_labels<- c("08:00:00", "14:00:00", "20:00:00","02:00:00","08:00:00","06:00:00")

### adding levels to Treatment variable to get Legend in preferred order
HOBO_temps2$Treatment <- factor(HOBO_temps2$Treatment, levels = c("Heated", "Chamber only", "Control"))

#Graph ####  
ggplot(HOBO_temps2, 
       aes(x=time_3, 
           y= average,
           color= Treatment , 
           linetype = Day )) +
  geom_line(alpha=0.3)+
  geom_smooth(method = "gam", formula = y ~ s(x, bs="tp", k =5), se = F) + # k=5 is my current fave but could adjust depending on desired smoothing effect \
  geom_line(y= HOBO_temps2$target, color= "black", size = 0.75)+
  scale_color_manual(values = c( "#D55E00", "#E69F00","#009E73", "black"))+
  theme_bw()+
  
  ggtitle("Heat Wave Experiment (black line indicates the target temperature)")+
  
  annotate("rect",
           xmin = as.POSIXct("2022-03-03 20:00:00", format= "%Y-%m-%d %H:%M:%S")  ,
           xmax = as.POSIXct("2022-03-04 08:00:00", format= "%Y-%m-%d %H:%M:%S"),
           ymin = -Inf, ymax = Inf,
           fill = alpha("gray", .17))+    # to get grey shading in to show night-time hours
  
    labs(y = (bquote(Air~Temperature~(degree~C))))+
  scale_x_datetime(name = "Time (hh:mm:ss)", 
                   #breaks = date_breaks("6 hours"),
                   labels = my_labels ,
                   expand = c(0,0))+
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_text(face="bold"))


#Repduce the boxplot==========
dim(HOBO_temps2)#12722    10
names(HOBO_temps2)

ggplot(HOBO_temps2, aes(x = Day, y = average)) +
  geom_boxplot()+
  scale_y_continuous(limits = c(15,40))+
  labs (x = "Heat Wave Day", y = "Tempereature (C)") +
  facet_wrap(.~Treatment)+
  
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 22,  color = "black"),
        axis.title.y=element_text(size=24),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 24, face = "bold",hjust=0.5),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

