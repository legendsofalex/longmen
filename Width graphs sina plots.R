library(readxl)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(ggforce)##load in packages


alldata <- read_excel("all widths.xlsx")###read in appropriately laid out excel file
#priwis is its own category of data (post re-inoculation with induced sample)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
################################################# O-antigen restored sina plot grow up 3 ##########################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
# Reshape data from wide to long format for O-Antigen restored columns
data_long <- alldata %>%
  pivot_longer(cols = c(oar300i, oar300ui, oar90i, oar90ui,oarpriwis),
               names_to = "Condition",
               values_to = "Width")
# Convert condition names to more informative factor levels (with line breaks for display)

data_long$Condition <- factor(data_long$Condition,
                              levels = c("oarpriwis","oar300i", "oar300ui", "oar90i", "oar90ui"),
                              labels = c("PRIWIS","210 min +\n 0.1 mM IPTG", "210 min", "0 min +\n 0.1 mM IPTG", "0 min"))

#set colours for fill and outline
customcolours  <- c("210 min +\n 0.1 mM IPTG" = "#00a393",
                   "210 min"        = "#de0286",
                   "PRIWIS"= "#6625b0",
                   "0 min +\n 0.1 mM IPTG"  = "#00a393",
                   "0 min"         = "#de0286")
custom_colours <- c("210 min +\n 0.1 mM IPTG" = "#007367",
                   "210 min"        = "#8a0053",
                   "PRIWIS" ="#400d7a",
                   "0 min +\n 0.1 mM IPTG"  = "#007367",
                   "0 min"         = "#8a0053")
# Create the plot for O-Antigen restored samples

OAR <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +

  geom_sina(maxwidth = 1.05, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custom_colours) +
  scale_color_manual(values=customcolours)+
  coord_cartesian(ylim=c(0,2.8))+
  
  # Apply plot themes and style   
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,30, by=0.5))+
# Add title and axis labels 
  labs(title = expression(bold("OAR +"~ bolditalic(" secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
# Add statistical comparisons between groups with custom annotations, generated from mann-whitney U-test files
  geom_signif(comparisons = list(c("PRIWIS","210 min"),c("210 min +\n 0.1 mM IPTG","PRIWIS"),c("210 min +\n 0.1 mM IPTG","210 min"),c("210 min +\n 0.1 mM IPTG","0 min +\n 0.1 mM IPTG"), 
                                 c("0 min","0 min +\n 0.1 mM IPTG"), c("0 min","210 min")),
              annotations=c("****","****","****","N.S","****","****"),
              y_position = c(1.8,1.6,2,2.2,2.6,2.4),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)

#display the plot
print(OAR) 
#save the plot as a PNG
ggsave("OARpriwis_Violin_plot_width.png", plot=OAR, width=10,height= 10)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
################################################ rfaC knock-out grow up 3 sina plot ###############################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

datalong <- alldata %>%
  pivot_longer(cols = c(rfac300i, rfac300ui, rfac90i, rfac90ui,rfacpriwis),
               names_to = "Condition",
               values_to = "Width")

datalong$Condition <- factor(datalong$Condition,
                             levels = c("rfacpriwis","rfac300i", "rfac300ui", "rfac90i", "rfac90ui"),
                             labels = c("PRIWIS","210 min +\n 0.1 mM IPTG", "210 min", "0 min +\n 0.1 mM IPTG", "0 min"))


custom___colours <- c("210 min +\n 0.1 mM IPTG" = "#00a393",
                     "210 min"        = "#de0286",
                     "PRIWIS"= "#6625b0",
                     "0 min +\n 0.1 mM IPTG"  = "#00a393",
                     "0 min"         = "#de0286")
custom__colours <- c("210 min +\n 0.1 mM IPTG" = "#007367",
                    "210 min"        = "#8a0053",
                    "PRIWIS" ="#400d7a",
                    "0 min +\n 0.1 mM IPTG"  = "#007367",
                    "0 min"         = "#8a0053")


rfac <- ggplot(datalong, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custom__colours) +
  scale_color_manual(values=custom___colours)+
  coord_cartesian(ylim=c(0,2.8))+
  
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,35, by=0.5))+
  
  labs(title = expression(bold(bolditalic("∆rfaC + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("PRIWIS","210 min"),c("210 min +\n 0.1 mM IPTG","PRIWIS"),c("210 min +\n 0.1 mM IPTG","210 min"),c("210 min +\n 0.1 mM IPTG","0 min +\n 0.1 mM IPTG"), 
                                 c("0 min","0 min +\n 0.1 mM IPTG"), c("0 min","210 min")),
              annotations=c("****","****","N.S","**","****","***"),
              y_position = c(1.8,1.6,2,2.2,2.6,2.4),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)


print(rfac) 


ggsave("RFACpriwis_Violin_plot_width.png", plot=rfac, width=10,height= 10)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################### Brauns lipoprotien dificient sina plot grow up 1 ###########################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

data_long <- alldata %>%
  pivot_longer(cols = c(lpp300i, lpp300ui, lpp90i, lpp90ui),
               names_to = "Condition",
               values_to = "Width")

data_long$Condition <- factor(data_long$Condition,
                              levels = c("lpp300i", "lpp300ui", "lpp90i", "lpp90ui"),
                              labels = c("300 min +\n 0.1 mM IPTG", "300 min", "90 min +\n 0.1 mM IPTG", "90 min"))


custo_colours <- c("300 min +\n 0.1 mM IPTG" = "#00a393",
                  "300 min"        = "#de0286",
                  "90 min +\n 0.1 mM IPTG"  = "#00a393",
                  "90 min"         = "#de0286")
custocolours <- c("300 min +\n 0.1 mM IPTG" = "#007367",
                 "300 min"        = "#8a0053",
                 "90 min +\n 0.1 mM IPTG"  = "#007367",
                 "90 min"         = "#8a0053")


lpp <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolours) +
  scale_color_manual(values=custo_colours)+
  coord_cartesian(ylim=c(0,2.6))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold(bolditalic("∆lpp + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("300 min +\n 0.1 mM IPTG","300 min"),c("300 min +\n 0.1 mM IPTG","90 min +\n 0.1 mM IPTG"), 
                                 c("90 min","90 min +\n 0.1 mM IPTG"), c("90 min","300 min")),
              annotations=c("**","****","N.S","****"),
              y_position = c(1.8,2,2.4,2.2),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)



print(lpp) 


ggsave("LPP_Violin_plot_width.png", plot=lpp, width=10,height= 10)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
############################################### BW25113 grow up 1 sina plot #######################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

data_long <- alldata %>%
  pivot_longer(cols = c(bw300i, bw300ui, bw100i, bw100ui),
               names_to = "Condition",
               values_to = "Width")

data_long$Condition <- factor(data_long$Condition,
                              levels = c("bw300i", "bw300ui", "bw100i", "bw100ui"),
                              labels = c("300 min +\n 0.1 mM IPTG", "300 min", "100 min +\n 0.1 mM IPTG", "100 min"))


custo_colorsbw <- c("300 min +\n 0.1 mM IPTG" = "#00a393",
                  "300 min"        = "#de0286",
                  "100 min +\n 0.1 mM IPTG"  = "#00a393",
                  "100 min"         = "#de0286")
custocolorsbw <- c("300 min +\n 0.1 mM IPTG" = "#007367",
                 "300 min"        = "#8a0053",
                 "100 min +\n 0.1 mM IPTG"  = "#007367",
                 "100 min"         = "#8a0053")


bw <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolorsbw) +
  scale_color_manual(values=custo_colorsbw)+
  coord_cartesian(ylim=c(0,2.6))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold("BW24113"~bolditalic(" + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("300 min +\n 0.1 mM IPTG","300 min"),c("300 min +\n 0.1 mM IPTG","100 min +\n 0.1 mM IPTG"), 
                                 c("100 min","100 min +\n 0.1 mM IPTG"), c("100 min","300 min")),
              annotations=c("****","****","****","****"),
              y_position = c(1.8,2,2.4,2.2),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)



print(bw) 

ggsave("BW_Violin_plot_width.png", plot=bw, width=10,height= 10)
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
############################################## Brauns lipoprotien dificeient grow up 3 ############################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

data_long <- alldata %>%
  pivot_longer(cols = c(lpp300i3, lpp300ui3, lpp90i3, lpp90ui3),
               names_to = "Condition",
               values_to = "Width")

data_long$Condition <- factor(data_long$Condition,
                              levels = c("lpp300i3", "lpp300ui3", "lpp90i3", "lpp90ui3"),
                              labels = c("300 min +\n 0.1 mM IPTG", "300 min", "90 min +\n 0.1 mM IPTG", "90 min"))


custo_colours <- c("300 min +\n 0.1 mM IPTG" = "#00a393",
                  "300 min"        = "#de0286",
                  "90 min +\n 0.1 mM IPTG"  = "#00a393",
                  "90 min"         = "#de0286")
custocolours <- c("300 min +\n 0.1 mM IPTG" = "#007367",
                 "300 min"        = "#8a0053",
                 "90 min +\n 0.1 mM IPTG"  = "#007367",
                 "90 min"         = "#8a0053")


lpp3 <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolours) +
  scale_color_manual(values=custo_colours)+
  coord_cartesian(ylim=c(0,2.6))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold(bolditalic("∆lpp + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("300 min +\n 0.1 mM IPTG","300 min"),c("300 min +\n 0.1 mM IPTG","90 min +\n 0.1 mM IPTG"), 
                                 c("90 min","90 min +\n 0.1 mM IPTG"), c("90 min","300 min")),
              annotations=c("N.S","****","****","****"),
              y_position = c(1.8,2,2.4,2.2),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)


print(lpp3) 


ggsave("LPP3_Violin_plot_width.png", plot=lpp3, width=10,height= 10)
###################################################################################################################################
###################################################################################################################################
################### Randomised brauns lipoprotien grow up 1 and 3 #################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

data_long <- alldata %>%
  pivot_longer(cols = c(lpp300ir, lpp300uir, lpp90ir, lpp90uir,lpppriwis),
               names_to = "Condition",
               values_to = "Width")

data_long$Condition <- factor(data_long$Condition,
                              levels = c("lpppriwis","lpp300ir", "lpp300uir", "lpp90ir", "lpp90uir"),
                              labels = c("PRIWIS","210 min +\n 0.1 mM IPTG", "210 min", "0 min +\n 0.1 mM IPTG", "0 min"))


custo_colours <- c("210 min +\n 0.1 mM IPTG" = "#00a393",
                  "210 min"        = "#de0286",
                  "PRIWIS" = "#6c32ad",
                  "0 min +\n 0.1 mM IPTG"  = "#00a393",
                  "0 min"         = "#de0286")
custocolours <- c("210 min +\n 0.1 mM IPTG" = "#007367",
                 "210 min"        = "#8a0053",
                 "PRIWIS"="#400d7a",
                 "0 min +\n 0.1 mM IPTG"  = "#007367",
                 "0 min"         = "#8a0053")


lppr <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  #geom_violin(trim = FALSE, size= 1, bw= 0.2) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolours) +
  scale_color_manual(values=custo_colours)+
  coord_cartesian(ylim=c(0,2.8))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold(bolditalic("∆lpp + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("PRIWIS","210 min"),c("210 min +\n 0.1 mM IPTG","PRIWIS"),c("210 min +\n 0.1 mM IPTG","210 min"),c("210 min +\n 0.1 mM IPTG","0 min +\n 0.1 mM IPTG"), 
                                 c("0 min","0 min +\n 0.1 mM IPTG"), c("0 min","210 min")),
              annotations=c("****","****","N.S","N.S","****","****"),
              y_position = c(1.8,1.6,2,2.2,2.6,2.4),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)


print(lppr) 


ggsave("LPPrpriwis_Violin_plot_width.png", plot=lppr, width=10,height= 10)




#################################################################################################################
#################################################################################################################
######################################### BW25113 grow up 2 sina plot ##########################################
#################################################################################################################
#################################################################################################################
data_long <- alldata %>%
  pivot_longer(cols = c(bw330i, bw330ui, bw120i, bw120ui),
               names_to = "Condition",
               values_to = "Width")

data_long$Condition <- factor(data_long$Condition,
                              levels = c("bw330i", "bw330ui", "bw120i", "bw120ui"),
                              labels = c("210 min +\n 0.1 mM IPTG", "210 min", "0 min +\n 0.1 mM IPTG", "0 min"))


custo_colorsbw <- c("210 min +\n 0.1 mM IPTG" = "#00a393",
                    "210 min"        = "#de0286",
                    "0 min +\n 0.1 mM IPTG"  = "#00a393",
                    "0 min"         = "#de0286")
custocolorsbw <- c("210 min +\n 0.1 mM IPTG" = "#007367",
                   "210 min"        = "#8a0053",
                   "0 min +\n 0.1 mM IPTG"  = "#007367",
                   "0 min"         = "#8a0053")


bw <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +
  #geom_violin(trim = FALSE, size= 1, bw= 0.2) +
  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolorsbw) +
  scale_color_manual(values=custo_colorsbw)+
  coord_cartesian(ylim=c(0,2.6))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold("BW24113"~bolditalic(" + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("210 min +\n 0.1 mM IPTG","210 min"),c("210 min +\n 0.1 mM IPTG","0 min +\n 0.1 mM IPTG"), 
                                 c("0 min","0 min +\n 0.1 mM IPTG"), c("0 min","210 min")),
              annotations=c("****","****","****","****"),
              y_position = c(1.8,2,2.4,2.2),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)

print(bw) 

ggsave("BW_Violin_plot_width.png", plot=bw, width=10,height= 10)

##################################################################################################################
##################################################################################################################
##################################################################################################################
############################### randomised BW25113 grow up 1 and 2 ###############################################
##################################################################################################################
##################################################################################################################
##################################################################################################################

data_long <- alldata %>%
  pivot_longer(cols = c(bw330ir, bw330uir, bw120ir, bw120uir,bwpriwis),
               names_to = "Condition",
               values_to = "Width")


data_long$Condition <- factor(data_long$Condition,
                              levels = c("bwpriwis","bw330ir", "bw330uir", "bw120ir", "bw120uir"),
                              labels = c("PRIWIS","210 min +\n 0.1 mM IPTG", "210 min", "0 min +\n 0.1 mM IPTG", "0 min"))


custo_colours <- c("210 min +\n 0.1 mM IPTG" = "#00a393",
                  "210 min"        = "#de0286",
                  "PRIWIS" = "#6c32ad",
                  "0 min +\n 0.1 mM IPTG"  = "#00a393",
                  "0 min"         = "#de0286")
custocolours <- c("210 min +\n 0.1 mM IPTG" = "#007367",
                 "210 min"        = "#8a0053",
                 "PRIWIS"="#400d7a",
                 "0 min +\n 0.1 mM IPTG"  = "#007367",
                 "0 min"         = "#8a0053")


bwr <- ggplot(data_long, aes(x = Condition, y = Width, fill = Condition, color= Condition)) +

  geom_sina(maxwidth = 0.95, size = 1.2, alpha = 0.8)+
  
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
               geom = "errorbar", width=0.1, color= "black", size= 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 5, fill = "black") +
  scale_fill_manual(values = custocolours) +
  scale_color_manual(values=custo_colours)+
  coord_cartesian(ylim=c(0,2.8))+
  
  theme_bw(base_size = 24) +
  theme(legend.position = "none",
        panel.border= element_rect(size=3),
        axis.ticks= element_line(size=1.5),
        axis.title.y= element_text(family ="sans", face="bold"),
        axis.text.x= element_text(family="sans",size=24),
        axis.text.y= element_text(family="sans",size=24),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#d9d9d9"),
        plot.title =element_text(family="sans", face="bold", hjust=0.5, size=30)) +
  scale_y_continuous(breaks=seq(0,45, by=0.5))+
  
  labs(title = expression(bold("BW25113"~bolditalic(" + secA E. coli")~" widths")),
       x = "   ",
       y ="Width (µm)")+
  geom_signif(comparisons = list(c("PRIWIS","210 min"),c("210 min +\n 0.1 mM IPTG","PRIWIS"),c("210 min +\n 0.1 mM IPTG","210 min"),c("210 min +\n 0.1 mM IPTG","0 min +\n 0.1 mM IPTG"), 
                                 c("0 min","0 min +\n 0.1 mM IPTG"), c("0 min","210 min")),
              annotations=c("****","N.S","N.S","****","N.S","****"),
              y_position = c(1.8,1.6,2,2.2,2.6,2.4),
              tip_length = 0.03,
              map_signif_level = TRUE, color= "black",textsize=8,size=1)



print(bwr)


ggsave("bwrpriwis_Violin_plot_width.png", plot=bwr, width=10,height= 10)