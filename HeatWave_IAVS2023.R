#LOAD and explore Hotham heat  DATA (Heat Wave Experiment):=========
library(tidyverse)
library(readxl)
library(sjPlot)
library(sjstats)
library(sjmisc)


heat <- read_excel("MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") #Get clean CSM

#Create Life Form column and turn dots into shapes based on Life Formn:
unique(heat$Genus)#"Aciphylla" and Leptorynchos = Forbs |   "Carex"&,"Poa" = Grass        "Pimelea" ,"Grevillea" = shrub
heat$Forms <- ifelse(heat$Genus == "Aciphylla" | heat$Genus == "Leptorynchos", "Forb", heat$Genus  )
heat$Form2 <- ifelse(heat$Forms == "Carex"| heat$Genus == "Poa", "Grass", heat$Forms  )
heat$Form <- ifelse(heat$Form2 == "Pimelea" | heat$Genus == "Grevillea", "Shrub", heat$Form2  )
heat$Form <- as.factor(heat$Form)
heat$LT50_Freeze <- as.numeric(as.character((heat$LT50_Freeze)))

heat2 <- heat %>% filter(Day != 0) #Ignore Day 0 as only control present there.
str(heat2)

#PLOT soil moisture:===========
ggplot(data = heat, aes(x= as.factor(Day), y=SoilMoisture_percent )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(size=2.5,aes( color = Genus))+
  facet_grid(.~Treatment)+
  labs(x = "Field Heat Wave (Days)",y = "Soil Moisture (%)", color = "Genus: ")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position =  "top", #c(.5,.8),               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.key = element_rect( fill = "white", color = "black"),
        legend.box.background = element_rect(fill = "yellow"),
        legend.box.margin = margin(6, 6, 6, 6),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))



#Plot Day/Genus versus Relative Water Content:=============
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=RWC, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Relative Water Content", fill= "Treatment: ")+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
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

ggsave(file= "IAVS2023_RWC_Genus_Plot.jpg", width = 8, height = 6)



#Plot Day/Genus versus Water Potential :=============
ggplot(data = heat[ heat$Day != 0,], aes(x= as.factor(Day),y=-WaterPotential_Mpa, fill=Treatment )) + #, color= AreaType
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(.~Genus)+
  labs(x = "Day",y = "Relative Water Content", fill= "Treatment: ")+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
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



ggsave(file= "IAVS2023_WP_Genus_Plot.jpg", width = 8, height = 6)




#Boxlot LT50_Heat:==========
#Day 3 & 6 Control Vs Heatwave treatment:
ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = LT50_Heat, fill=Treatment)) +
  geom_boxplot()+
  geom_jitter(aes(shape = Form),size=3)+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Days of Heat Wave", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.grid.minor.x = element_blank())

ggsave(file= "IAVS2023_HeatAcrossDays_LifeForms_Plot.jpg", width = 8, height = 6)



#Day 3 & 6 Control Vs Heatwave treatment across 3 life forms:
ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = LT50_Heat, fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form,color=Genus),size=3)+
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Days of Heat Wave", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("")+
  facet_grid(.~Form)+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
         strip.text = element_text(face = "bold", size = 14),
        panel.grid.minor.x = element_blank())

ggsave(file= "IAVS2023_HeatAcrossDays_LifeForms_Species_Plot.jpg", width = 8, height = 6)




#Plot FREEZE on Day 3,6:==========
ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = as.numeric(LT50_Freeze), fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter()+
  scale_fill_manual(values = c('#deebf7','#3182bd'))+
  labs (fill= "Treatment: ", x = "Days of Heat Wave", y = (bquote(LT50~Freeze~Tolerance~("\u00B0"~C)))) +
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.grid.minor.x = element_blank())

ggsave(file= "IAVS2023_FreezeAcrossDays_Plot.jpg", width = 8, height = 6)

ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = as.numeric(LT50_Freeze), fill=Treatment)) +
  geom_boxplot()+
  geom_jitter(aes(shape = Form),size=3)+
  scale_fill_manual(values = c('#deebf7','#3182bd'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Days of Heat Wave", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("")+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        panel.grid.minor.x = element_blank())

ggsave(file= "IAVS2023_FreezeAcrossDays_LifeForm_Plot.jpg", width = 8, height = 6)


#Day 3 & 6 Control Vs Heatwave treatment across 3 life forms:
ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = LT50_Freeze, fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form,color=Genus),size=3)+
  scale_fill_manual(values = c('#deebf7','#3182bd'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Days of Heat Wave", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("")+
  facet_grid(.~Form)+
  #scale_y_continuous(limits = c(-23,-10))+
  theme_bw()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
         strip.text = element_text(face = "bold", size = 14),
        panel.grid.minor.x = element_blank())

ggsave(file= "IAVS2023_LT50_Freeze_AcrossDays_LifeForms_Species_Plot.jpg", width = 8, height = 6)


#LT50 HEAT 4 Results=========
heat_summary <- heat %>% group_by(Day) %>%
  summarise(AV = mean(LT50, na.rm=T))
heat_summary 

summary( lmer(LT50_Heat ~ Treatment *  as.factor(Day) +Form+(1|Genus)+(1|Block), data=heat2))
##############Estimate Std. Error  df          t value Pr(>|t|)    
#(Intercept)               95.020      3.118   9.213  30.471 1.45e-10 ***
# Heat_Treatment  -7.385      2.565 233.000  -2.879  0.00436 ** 

#Stats Table LT50_Heat :
mod2 <-( lmer(LT50_Heat ~ Treatment+ as.factor(Day) + (1|Genus)+(1|Block), data=heat2))
tab_model(mod2)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Stats Table LT50_Freeze :
mod2 <-( lmer(LT50_Freeze ~ Treatment+ as.factor(Day) + (1|Genus)+(1|Block), data=heat2))
tab_model(mod2)
tab_model(mod2,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table


#Stats Table LT50_Heat +Form :
mod3 <- lmer(LT50_Heat ~ Treatment +  as.factor(Day) +Form +(1|Genus)+(1|Block), data=heat2)
tab_model(mod3)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table

#Stats Table LT50_Freeze +Form :
mod3 <- lmer(LT50_Freeze ~ Treatment +as.factor(Day) + Form + (1|Genus) + (1|Block), data=heat2,na.action = na.omit)
tab_model(mod3)
tab_model(mod3,show.icc = FALSE,show.stat =T,show.re.var=F) #Simpler table
