##################################################################################
###################~~~~mann whitney u-test~~~~####################################
##################################################################################
##################################################################################
# load in packages
library(effsize)
library(tidyverse)
library(readxl)
library(dplyr)


#################################################################################################
###########################################~~~width~~~###########################################
#################################################################################################
alldata<-read_xlsx("all widths.xlsx")
summary(alldata)
data<- alldata
########################################### #looks to see what data sets are normally distributed
shapiro.test(alldata$rfac300i)#not normal
shapiro.test(alldata$rfac300ui)#not normal
shapiro.test(alldata$rfac120i)#normal
shapiro.test(alldata$rfac120ui)#normal
shapiro.test(alldata$oar300i)#normal
shapiro.test(alldata$oar300ui)#not normal
shapiro.test(alldata$oar90i)#not normal
shapiro.test(alldata$oar90ui)#not normal
shapiro.test(alldata$lpp300i3)#not normal
shapiro.test(alldata$lpp300ui3)#not normal
shapiro.test(alldata$lpp90i3)#not normal
shapiro.test(alldata$lpp90ui3)#not normal
shapiro.test(alldata$lpppriwis)#not normal

###################################################################################
###################################~~RfaC grow 3~~#################################
###################################################################################

#########################
#300v300 rfac############
#########################
rfac300<-wilcox.test(alldata$"rfac300ui",alldata$"rfac300i",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(rfac300$p.value)

###########################
#####cliff delta ef########
rfac300ui<-na.omit(alldata$rfac300ui)
rfac300i<-na.omit(alldata$rfac300i)
cd<-cliff.delta(rfac300ui,rfac300i)
print(cd)

########################
#####120v120 rfac#######
########################
rfac120<-t.test(alldata$"rfac120ui",alldata$"rfac120i",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(rfac120$p.value)
#####################
rfac120ui<-na.omit(alldata$rfac120ui)
rfac120i<-na.omit(alldata$rfac120i)
m1<-mean(rfac120ui)
m2<-mean(rfac120i)
sd1 <- sd(rfac120ui)
sd2 <- sd(rfac300i)
n1 <- length(rfac120ui)
n2 <- length(rfac120i)
pool <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
md <- m2 - m1
d<- md / pool
print(d) ##carries out cohens-D effect size test with pooled standard deviation


#########################
#####120v300 i rfac######
#########################
rfaci<-wilcox.test(alldata$"rfac120i",alldata$"rfac300i",
                   alternative = "two.sided", paired = FALSE, 
                   exact = FALSE, correct = FALSE)
print(rfaci$p.value)
#########################
cd<-cliff.delta(rfac120i,rfac300i)
print(cd)

########################
####120v300 ui rfac#####
########################
rfacui<-wilcox.test(alldata$"rfac120ui",alldata$"rfac300ui",
                    alternative = "two.sided", paired = FALSE, 
                    exact = FALSE, correct = FALSE)
print(rfacui$p.value)
#########################
cd<-cliff.delta(rfac120ui,rfac300ui)
print(cd)

##################################################################################
################################~~OAR grow up 3~~#################################
##################################################################################

#######################
#######300v300 oar#####
#######################
oar300<-wilcox.test(alldata$"oar300ui",alldata$"oar300i",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(oar300$p.value)
########################
oar300ui<-na.omit(alldata$oar300ui)
oar300i<-na.omit(alldata$oar300i)
cd<-cliff.delta(oar300ui,oar300i)
print(cd)

###########################
#########90v90 oar#########
###########################
oar90<-wilcox.test(alldata$"oar90ui",alldata$"oar90i",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(oar90$p.value)
##################################
oar90ui<-na.omit(alldata$oar90ui)
oar90i<-na.omit(alldata$oar90i)
cd<-cliff.delta(oar90ui,oar90i)
print(cd)

#########################
#######90v300 i oar######
#########################
oari<-wilcox.test(alldata$"oar90i",alldata$"oar300i",
                  alternative = "two.sided", paired = FALSE, 
                  exact = FALSE, correct = FALSE)
print(oari$p.value)
################################
cd<-cliff.delta(oar90i,oar300i)
print(cd)

#########################
#######90v300 ui oar#####
#########################
oarui<-wilcox.test(alldata$"oar90ui",alldata$"oar300ui",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(oarui$p.value)
###############################
cd<-cliff.delta(oar90ui,oar300ui)
print(cd)
######################################
#######300iv300 ui oar grow up 2######
######################################
oar2ui<-na.omit(alldata$oar300ui2)
oar2i<-na.omit(alldata$oar300i2)
oarui<-wilcox.test(alldata$"oar300ui2",alldata$"oar300i2",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(oarui$p.value)
###############################
cd<-cliff.delta(oar2i,oar2ui)
print(cd)
##################################################################################
################################~~Lpp~~###########################################
##################################################################################

#######################
#######300v300 Lpp#####
#######################
lpp300<-wilcox.test(alldata$"lpp300ui",alldata$"lpp300i",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lpp300$p.value)
##################################
lpp300ui<-na.omit(alldata$lpp300ui)
lpp300i<-na.omit(alldata$lpp300i)
cd<-cliff.delta(lpp300ui,lpp300i)
print(cd)

#############################
#########90v90 lpp###########
#############################
lpp90<-wilcox.test(alldata$"lpp90ui",alldata$"lpp90i",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(lpp90$p.value)
##################################
lpp90ui<-na.omit(alldata$lpp90ui)
lpp90i<-na.omit(alldata$lpp90i)
cd<-cliff.delta(lpp90ui,lpp90i)
print(cd)

###########################
#######90v300 i lpp########
###########################
lppi<-wilcox.test(alldata$"lpp90i",alldata$"lpp300i",
                  alternative = "two.sided", paired = FALSE, 
                  exact = FALSE, correct = FALSE)
print(lppi$p.value)
################################
cd<-cliff.delta(lpp90i,lpp300i)
print(cd)

#########################
#######90v300 ui lpp#####
#########################
lppui<-wilcox.test(alldata$"lpp90ui",alldata$"lpp300ui",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(lppui$p.value)
#################################
cd<-cliff.delta(lpp90ui,lpp300ui)
print(cd)
##################################################################################
################################~~Lpp3~~##########################################
##################################################################################

#######################
#######300v300 Lpp#####
#######################
lpp3003<-wilcox.test(alldata$"lpp300ui3",alldata$"lpp300i3",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(lpp3003$p.value)
##################################
lpp300ui3<-na.omit(alldata$lpp300ui3)
lpp300i3<-na.omit(alldata$lpp300i3)
cd<-cliff.delta(lpp300ui3,lpp300i3)
print(cd)

#############################
#########90v90 lpp###########
#############################
lpp903<-wilcox.test(alldata$"lpp90ui3",alldata$"lpp90i3",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lpp903$p.value)
##################################
lpp90ui3<-na.omit(alldata$lpp90ui3)
lpp90i3<-na.omit(alldata$lpp90i3)
cd<-cliff.delta(lpp90ui3,lpp90i3)
print(cd)

###########################
#######90v300 i lpp########
###########################
lppi3<-wilcox.test(alldata$"lpp90i3",alldata$"lpp300i3",
                   alternative = "two.sided", paired = FALSE, 
                   exact = FALSE, correct = FALSE)
print(lppi3$p.value)
################################
cd<-cliff.delta(lpp90i3,lpp300i3)
print(cd)

#########################
#######90v300 ui lpp#####
#########################
lppui3<-wilcox.test(alldata$"lpp90ui3",alldata$"lpp300ui3",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppui3$p.value)
#################################
cd<-cliff.delta(lpp90ui3,lpp300ui3)
print(cd)




#######################
#######300v300 Lpp#####
#######################
lpp300r<-wilcox.test(alldata$"lpp300uir",alldata$"lpp300ir",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(lpp300r$p.value)
##################################
lpp300uir<-na.omit(alldata$lpp300uir)
lpp300ir<-na.omit(alldata$lpp300ir)
cd<-cliff.delta(lpp300uir,lpp300ir)
print(cd)

#############################
#########90v90 lpp###########
#############################
lpp90r<-wilcox.test(alldata$"lpp90uir",alldata$"lpp90ir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lpp90r$p.value)
##################################
lpp90uir<-na.omit(alldata$lpp90uir)
lpp90ir<-na.omit(alldata$lpp90ir)
cd<-cliff.delta(lpp90uir,lpp90ir)
print(cd)

###########################
#######90v300 i lpp########
###########################
lppir<-wilcox.test(alldata$"lpp90ir",alldata$"lpp300ir",
                   alternative = "two.sided", paired = FALSE, 
                   exact = FALSE, correct = FALSE)
print(lppir$p.value)
################################
cd<-cliff.delta(lpp90ir,lpp300ir)
print(cd)

#########################
#######300 ui vs PRIWIS lpp#####
#########################
lppuir<-wilcox.test(alldata$"lpppriwis",alldata$"lpp300uir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppuir$p.value)
#################################
lpppriwis<-na.omit(alldata$lpppriwis)
lpp300uir<-na.omit(alldata$lpp300uir)

cd<-cliff.delta(lpppriwis,lpp300uir)
print(cd)
#########################
#######300 i vs PRIWIS lpp#####
#########################
lppir<-wilcox.test(alldata$"lpppriwis",alldata$"lpp300ir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppir$p.value)
#################################
lpppriwis<-na.omit(alldata$lpppriwis)
lpp300ir<-na.omit(alldata$lpp300ir)
cd<-cliff.delta(lpppriwis,lpp300ir)
print(cd)
###########################
#######90v300 ui lpp########
###########################
lppuir<-wilcox.test(alldata$"lpp90uir",alldata$"lpp300uir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppuir$p.value)
#################################
cd<-cliff.delta(lpp90uir,lpp300uir)
print(cd)

#####################################################################################
#####################################################################################
### BW25113 randomised grow up 1 and 2###############################################
#####################################################################################

bw300ui<-na.omit(alldata$bw330uir)
bw300i<-na.omit(alldata$bw330ir)
bw100ui<-na.omit(alldata$bw120uir)
bw100i<-na.omit(alldata$bw120ir)
bw300ui2<-na.omit(alldata$bw300ui2)
bw300i2<-na.omit(alldata$bw300i2)
bw100ui2<-na.omit(alldata$bw100ui2)
bw100i2<-na.omit(alldata$bw100i2)
bwpriwis<-na.omit(alldata$bwpriwis)
#######################
#######330v330 oar#####
#######################
bw330<-wilcox.test(alldata$"bw330uir",alldata$"bw330ir",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(bw330$p.value)
##################################

cd<-cliff.delta(bw300ui,bw300i)
print(cd)

#############################
######120v120 bw25113########
#############################
bw120<-wilcox.test(alldata$"bw120uir",alldata$"bw120ir",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(bw120$p.value)
##################################

cd<-cliff.delta(bw100ui,bw100i)
print(cd)

###########################
#######100v300 i bw########
###########################
bwi<-wilcox.test(alldata$"bw120ir",alldata$"bw330ir",
                 alternative = "two.sided", paired = FALSE,
                 exact = FALSE, correct = FALSE)
print(bwi$p.value)
################################
cd<-cliff.delta(bw100i,bw300i)
print(cd)

#########################
#######120v330 ui bw#####
#########################
bwui<-wilcox.test(alldata$"bw120uir",alldata$"bw330uir",
                  alternative = "two.sided", paired = FALSE,
                  exact = FALSE, correct = FALSE)
print(bwui$p.value)
################################
cd<-cliff.delta(bw100ui,bw300ui)
print(cd)
#########################
#######PRIWISv330 ui bw#####
#########################
bwui<-wilcox.test(alldata$"bwpriwis",alldata$"bw330uir",
                  alternative = "two.sided", paired = FALSE,
                  exact = FALSE, correct = FALSE)
print(bwui$p.value)
################################
cd<-cliff.delta(bwpriwis,bw300ui)
print(cd)
#########################
#######PRIWISv330 i bw#####
#########################
bwui<-wilcox.test(alldata$"bwpriwis",alldata$"bw330ir",
                  alternative = "two.sided", paired = FALSE,
                  exact = FALSE, correct = FALSE)
print(bwui$p.value)
################################
cd<-cliff.delta(bwpriwis,bw300i)
print(cd)



######################################################################################
######################################################################################
########################## BW25113 grow up 2 #########################################
######################################################################################
######################################################################################
bw300ui<-na.omit(alldata$bw300ui)
bw300i<-na.omit(alldata$bw300i)
bw100ui<-na.omit(alldata$bw100ui)
bw100i<-na.omit(alldata$bw100i)
bw300ui2<-na.omit(alldata$bw300ui2)
bw300i2<-na.omit(alldata$bw300i2)
bw100ui2<-na.omit(alldata$bw100ui2)
bw100i2<-na.omit(alldata$bw100i2)
#######################
#######300v300 oar#####
#######################
bw300<-wilcox.test(alldata$"bw300ui",alldata$"bw300i",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(bw300$p.value)
##################################

cd<-cliff.delta(bw300ui,bw300i)
print(cd)

#############################
######100v100 bw25113########
#############################
bw100<-wilcox.test(alldata$"bw100ui",alldata$"bw100i",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(bw100$p.value)
##################################

cd<-cliff.delta(bw100ui,bw100i)
print(cd)

###########################
#######100v300 i bw########
###########################
bwi<-wilcox.test(alldata$"bw100i",alldata$"bw300i",
                 alternative = "two.sided", paired = FALSE, 
                 exact = FALSE, correct = FALSE)
print(bwi$p.value)
################################
cd<-cliff.delta(bw100i,bw300i)
print(cd)

#########################
#######100v300 ui bw#####
#########################
bwui<-wilcox.test(alldata$"bw100ui",alldata$"bw300ui",
                  alternative = "two.sided", paired = FALSE, 
                  exact = FALSE, correct = FALSE)
print(bwui$p.value)
################################
cd<-cliff.delta(bw100ui,bw300ui)
print(cd)
