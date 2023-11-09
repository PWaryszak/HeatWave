#LOAD Hotham heat  DATA (Heat Wave Experiment):=========
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(ggpmisc)) install.packages('ggpmisc')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(readxl)) install.packages('readxl')
if (!require(gridExtra)) install.packages('gridExtra')

library(tidyverse)
library(ggpmisc)
library (ggpubr)
library(readxl)
library(gridExtra)
library(scales)

#FINAL DATA=========
heat <- read_excel("MASTER_HeatWave_Data.xlsx", sheet = "Heat_Freeze_CSM_DATA") #Get clean CSM
dim(heat) #127  31

#Create Life Form column and turn dots into shapes based on Life Formn:
unique(heat$Genus)#"Aciphylla" and Leptorynchos = Forbs |   "Carex"&,"Poa" = Grass        "Pimelea" ,"Grevillea" = shrub
heat$Forms <- ifelse(heat$Genus == "Aciphylla" | heat$Genus == "Leptorynchos", "Forb", heat$Genus  )
heat$Form2 <- ifelse(heat$Forms == "Carex"| heat$Genus == "Poa", "Grass", heat$Forms  )
heat$Form <- ifelse(heat$Form2 == "Pimelea" | heat$Genus == "Grevillea", "Shrub", heat$Form2  )
heat$Form <- as.factor(heat$Form)
heat$LT50_Freeze <- as.numeric(as.character((heat$LT50_Freeze)))



#PLOT Table LT50_Heat & LT50_Freeze :==========
#LT50_Heat Plot
min(heat$LT50_Heat, na.rm=T)# 44.24843
max(heat$LT50_Heat, na.rm=T)#51.13175


h <- ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = LT50_Heat, fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form),size=3)+
  
  scale_y_continuous(limits = c(44,52), labels = ~ paste0(.x, "°"), breaks = c( 44, 46, 48, 50, 52))+
  
  scale_fill_manual(values = c('#fee0d2','#de2d26'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Heat Day", y = (bquote(LT50~Heat ~Tolerance~("\u00B0"~C)))) +
  ggtitle("a)")+
  theme_minimal()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = c(.5, .17),
          legend.direction="horizontal",
          legend.box.margin = margin(10, 10, 10, 10),
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 16),
          legend.box.background = element_rect(color = "black", size = 1),   # Add a box around the legend

        panel.grid.minor.x = element_blank(),
    plot.title = element_text(size=20, lineheight=1.8, face="bold"))

h



#LT50_Freeze Plot:
min(heat$LT50_Freeze, na.rm=T)#-22.82627
max(heat$LT50_Freeze, na.rm=T)#-10.9

f<-ggplot(data = heat[ heat$Day != 0,], aes(x = as.factor(Day), y = as.numeric(LT50_Freeze), fill=Treatment)) +
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(aes(shape = Form),size=3)+

  scale_y_continuous(limits = c(-23,-10), labels = ~ paste0(.x, "°"), breaks = c(-22, -18, -14, -10)) +
  

  scale_fill_manual(values = c('#deebf7','#3182bd'))+
  labs (shape = "Life Form: ",fill= "Treatment: ", x = "Heat Day", y = (bquote(LT50~Freeze ~Tolerance~("\u00B0"~C)))) +
  ggtitle("b)")+
  # guides(shape="none")+
  theme_minimal()+
  theme(axis.text.y=element_text(size=24 , color = "black"),
        axis.text.x = element_text(size = 24,  color = "black"),
        axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = c(.5, .17),
          legend.direction="horizontal",
          legend.box.margin = margin(10, 10, 10, 10),
         legend.text = element_text(size = 14),
         legend.title = element_text(size = 16),
          legend.box.background = element_rect(color = "black", size = 1),   # Add a box around the legend

        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size=20, lineheight=1.8, face="bold"))

f 
 
#Combined PLOTS====
p_plots_horizon  <- ggarrange(h,f, ncol=2) #,labels = c("a)", "b)"),label.x = 0,label.y = 1)
p_plots_horizon

ggsave(p_plots_horizon, filename = "Fig02_LT50_Heat_Freeze4.jpg", width = 8800, height = 5437, units = "px", dpi = 600)
