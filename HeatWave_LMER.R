#LOAD and explore Hotham heat  DATA (Heat Wave Experiment):=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(sjPlot)
library(sjstats)
library(sjmisc)
library(ggpmisc)


#Load snow prepared and cured in CombineData.R file:
#setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/HeatWave")
heat <- read.csv("hotham_heat.csv") 
unique(heat$PD_MD) #PD = PreDawn Potential and MD = MidDay Potential
#NOTE: MD is missing. might need mergin from HeataveData!!!!

names(heat)
#Columns explained:
#RWC = relative water content = (Fresh.wt - Dry.wt) / (Turgid wt - Dry.wt) * 100
#LDMC = Leaf dry matter content = Dry wt / Turgid wt

summary(heat$RWC)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#-21.21   85.45   94.48   90.41  100.00  162.50 

unique(heat$Genus) #6 focus plants were: 
#"Aciphylla"     "Carex"         "Grevillea"     "Leptorhynchos" "Pimelea"       "Poa"  

#Entire Data (Day 0, 3, 6):
summary( lmer(RWC ~ Treatment  +(1|Genus)+(1|Block), data=heat))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 



#RWC Stats:===========
#The heat treatment was applied on Day 0 after Virginia took  midday readings -
#we waited until then before switching on the heating. 
#So the Day 0 has no matching response in heat treatment- we decided to remove  it prior analysis:
#hence: data = heat[ heat$Day != 0,])

#Stats Table - 
mod1 <-( lmer(RWC ~ Treatment  +(1|Genus)+(1|Block), data=heat[ heat$Day != 0,]))
tab_model(mod1)
#(Intercept)          	93.23  	85.82 – 100.64	<0.001
#Treatment [heatwave]	  -7.13   -13.96 – -0.30	0.041

7.13/ 93.23*100

#Plot Day/Genus versus Relative Water Content (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=RWC, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Relative Water Content", fill= "Treatment: ")+
  scale_fill_manual(values = c("darkgreen", "pink"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

#ggsave(file= "RWC_Genus_Plot.jpg", width = 8, height = 6)


#LT50_versus_RWC PLOT=========
heat2 <- heat %>% filter(Day != 0)
summary( lmer(LT50 ~ RWC                *as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))
summary( lmer(LT50 ~ WaterPotential_Mpa *as.factor(Day)  +(1|Genus)+(1|Block), data=heat2))


ggplot(data = heat2, aes(x=RWC , y=LT50  )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "RWC",y = "LT50", color = "Genus:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "bottom",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)


#Water Potential Stats =====
heat2 <- heat %>% filter(Day != 0)

summary( lmer(-WaterPotential_bar ~ Treatment+ as.factor(Day)+SoilMoisture_percent +(1|Genus)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
mod2 <-( lmer(LT50 ~ Treatment  + as.factor(Day) + (1|Genus)+(1|Block), data=heat2))
tab_model(mod2)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Plot Day/Genus versus Lethal Temperature 50 (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=LT50, fill=Treatment )) + #, color= AreaType
  geom_violin()+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "LT50", fill= "Treatment: ")+
  scale_fill_manual(values = c("darkgreen", "brown"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#LT50 Stats =====
heat2 <- heat %>% filter(Day != 0)
heat2$WP <- -heat2$WaterPotential_bar

summary( lmer(LT50 ~ Treatment *  as.factor(Day) +(1|Genus)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table:
mod2 <-( lmer(LT50 ~ Treatment+ as.factor(Day) + WaterPotential_bar+ (1|Genus)+(1|Block), data=heat2))
tab_model(mod2)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Plot Day/Genus versus Lethal Temperature 50 (%):
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=LT50, fill=Treatment )) + #, color= AreaType
  geom_boxplot()+
  #facet_wrap(.~Genus)+
  labs(x = "Day",y = "LT50", fill= "Treatment: ")+
  scale_fill_manual(values = c("darkgreen", "red"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))



#Soil Moisture =========
#Stats Table:
mod3 <-( lmer(SoilMoisture_percent ~ Treatment  +(1|Genus)+(1|Block), data=heat2))
tab_model(mod3)
summary(mod3)
#                          Estimate Std. Error df    t value Pr(>|t|)    
#(Intercept)               38.379      1.126  12.768  34.078 6.42e-14 ***
# Heat_TreatmentTreatment   -5.168     1.159 171.160  -4.458 1.49e-05 ***

mod4 <-( lmer(LT50 ~  as.factor(Day) +Genus+(1|Block), data=heat2))
tab_model(mod4)
Genus [Carex]	3.12
Genus [Poa]	2.45
Genus [Grevillea]	2.31
Genus [Pimelea]	1.02
Aciphylla (Intercept)	46.90
Genus [Leptorynchos]	-0.07

summary(lm(SoilMoisture_percent ~ as.factor(Day), data = heat))
  
#Focus on Poa and soil moisture:
ggplot(data = heat, aes(x= as.factor(Day), y=SoilMoisture_percent )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color = Genus))+
  #facet_grid(Genus~.)+
  labs(x = "Field Heat Wave (Days)",y = "Soil Moisture (%)", color = "Genus: ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = c(.5,.8),               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "yellow"),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#Focus on Poa and PD
heat$Poa <- ifelse(heat$Genus == "Poa", "Poa", "Shrub")

ggplot(data = heat, aes(x= as.factor(Day), y=WaterPotential_Mpa )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2,aes( color = Poa))+
  #facet_grid(Genus~.)+
  labs(x = "Day",y = "PD (MPa)", color = "Plant:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#Soil moisture versus WP
ggplot(data = heat2, aes(x= WaterPotential_bar, y=SoilMoisture_percent )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Water Potential",y = "Soil Moisture (%)", color = "Genus name:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
stat_fit_glance(method = "lm",
                label.x =  c(0.5,0),
                method.args = list(formula = y ~ x),
                mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                              stat(r.squared), stat(p.value))),parse = TRUE)


#LT50 versus PreDawn Water Potential PLOT=========
heat3 <- heat2 %>%
  filter(PD_MD == "PD")

#Plot Day/Genus versus  WaterPotential_Mpa:
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=-WaterPotential_Mpa, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Water Potential (MPa)", fill= "Treatment: ")+
  scale_fill_manual(values = c("darkgreen", "pink"))+
  theme_classic()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "top",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))

ggsave(file= "WP_Genus_Plot.jpg", width = 8, height = 6)


ggplot(data = heat3, aes(x= -WaterPotential_bar, y=LT50 )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Water Potential",y = "LT50", color = "Genus name:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)


#LT50 versus LDMC PLOT=========
ggplot(data = heat2, aes(x= WaterPotential_bar, y=LDMC )) + #, color= AreaType
  geom_point(size=2,aes( color =Genus))+
  stat_smooth(method = "lm", col = "black")+
  
  facet_grid(Day~Treatment)+
  labs(x = "Water Potential",y = "LDMC", color = "Genus name:  ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "right",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))+
  
  stat_fit_glance(method = "lm",
                  label.x =  c(0.5,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),parse = TRUE)


