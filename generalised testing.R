##################################################################################
###################~~~~~~~~~generalised~~~~~~~~~##################################
##################################################################################
##################################################################################
# load in packages
library(tidyverse)
library(readxl)
library(dplyr)
##################################################################################
#####################~~~remove blanks~~~##########################################
##################################################################################
#read in data from excel
alldata<-read_xlsx("all lengths.xlsx")
alldata<-read_xlsx("all widths.xlsx")# i alternate which file i load in by running the single line for the data set i want
summary(alldata)
rfac90ui<-na.omit(alldata$rfac90ui)
rfac90i<-na.omit(alldata$rfac90i)
rfac300ui<-na.omit(alldata$rfac300ui)
rfac300i<-na.omit(alldata$rfac300i)
oar300ui<-na.omit(alldata$oar300ui)
oar300i<-na.omit(alldata$oar300i)
oar90ui<-na.omit(alldata$oar90ui)
oar90i<-na.omit(alldata$oar90i)
bw300ui<-na.omit(alldata$bw300ui)
bw300i<-na.omit(alldata$bw300i)
bw100ui<-na.omit(alldata$bw100ui)
bw100i<-na.omit(alldata$bw100i)
lpp300ui<-na.omit(alldata$lpp300ui)
lpp300i<-na.omit(alldata$lpp300i)
lpp90ui<-na.omit(alldata$lpp90ui)
lpp90i<-na.omit(alldata$lpp90i)
lpp300ui3<-na.omit(alldata$lpp300ui3)
lpp300i3<-na.omit(alldata$lpp300i3)
lpp90ui3<-na.omit(alldata$lpp90ui3)
lpp90i3<-na.omit(alldata$lpp90i3)
lpppriwis<-na.omit(alldata$lpppriwis)
bwpriwis<-na.omit(all$databwpriwis)
data<- alldata
##################################################################################
##############################shapiro test########################################
##################################################################################
shapiro.test(alldata$rfac300i)#not normal, width normal
shapiro.test(alldata$rfac300ui)#not normal, width normal
shapiro.test(alldata$rfac90i)#not normal, width normal
shapiro.test(alldata$rfac90ui)#not normal, width normal
shapiro.test(alldata$oar300i)#not normal,width normal
shapiro.test(alldata$oar300ui)#not normal
shapiro.test(alldata$oar90i)#not normal
shapiro.test(alldata$oar90ui)#not normal
shapiro.test(alldata$bw300i)#not normal
shapiro.test(alldata$bw300ui)#not normal, width normal
shapiro.test(alldata$bw100i)#not normal
shapiro.test(alldata$bw100ui)#not normal
shapiro.test(alldata$lpp300i)#not normal,width normal
shapiro.test(alldata$lpp300ui)#not normal
shapiro.test(alldata$lpp90i)#not normal
shapiro.test(alldata$lpp90ui)#not normal
shapiro.test(alldata$lpp300i3)# not normal
shapiro.test(alldata$lpp300ui3)#not normal
shapiro.test(alldata$lpp90i3)#not normal
shapiro.test(alldata$lpp90ui3)#not normal
shapiro.test(alldata$lpppriwis)#not normal, not normal
shapiro.test(alldata$bwpriwis)#not normal, not normal

