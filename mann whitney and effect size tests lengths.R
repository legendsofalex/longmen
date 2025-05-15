##################################################################################
###################~~~~mann whitney u-test~~~~####################################
##################################################################################
##################################################################################
# load in packages
library(effsize)
library(tidyverse)
library(readxl)
library(dplyr)
##################################################################################
############################~~~length~~~##########################################
##################################################################################
#read in data from excel
alldata<-read_xlsx("all lengths.xlsx")

summary(alldata)
data<- alldata
##create data set that is randomized, they will be in separate columns, i combined like for like columns in excel manually
#and added it manually to the main file
set.seed(63)#set seed so can go back to the same randomization 
random_datas<-data.frame( 
  bw300i1r=sample(na.omit(alldata$bw330i),250, replace=FALSE),
  bw300ui1r=sample(na.omit(alldata$bw330ui),250, replace=FALSE),
  bw90i1r=sample(na.omit(alldata$bw120i),250, replace=FALSE),
  bw90ui1r=sample(na.omit(alldata$bw120ui),250, replace=FALSE),
  bw300i3r=sample(na.omit(alldata$bw330i2),250, replace=FALSE),
  bw300ui3r=sample(na.omit(alldata$bw330ui2),250, replace=FALSE),
  bw90i3r=sample(na.omit(alldata$bw120i2),250, replace=FALSE),
  bw90ui3r=sample(na.omit(alldata$bw120ui2),250, replace=FALSE))

set.seed(42) ##create data set that is randomized, they will be in separate columns, i combined like for like columns in excel manually
random_data<-data.frame( #and added it manually to the main file
  lpp300i1r=sample(na.omit(alldata$lpp300i),349, replace=FALSE),
  lpp300ui1r=sample(na.omit(alldata$lpp300ui),349, replace=FALSE),
  lpp90i1r=sample(na.omit(alldata$lpp90i),349, replace=FALSE),
  lpp90ui1r=sample(na.omit(alldata$lpp90ui),349, replace=FALSE),
  lpp300i3r=sample(na.omit(alldata$lpp300i3),349, replace=FALSE),
  lpp300ui3r=sample(na.omit(alldata$lpp300ui3),349, replace=FALSE),
  lpp90i3r=sample(na.omit(alldata$lpp90i3),349, replace=FALSE),
  lpp90ui3r=sample(na.omit(alldata$lpp90ui3),349, replace=FALSE))
# I used the same set seed for lengths and widths so that the data pulled from both were from the same cells.
###################################################################################
######################## rfaC knock-out grow up 3 #################################
###################################################################################

##############################
######300v300 rfac############
##############################
rfac300<-wilcox.test(alldata$"rfac300ui",alldata$"rfac300i",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)# this function carries out the mann-whitney U-test inside the wilcox fucntion
#display result
print(rfac300$p.value)

############################
######cliff delta ef########
############################
rfac300ui<-na.omit(alldata$rfac300ui)
rfac300i<-na.omit(alldata$rfac300i)
cd<-cliff.delta(rfac300ui,rfac300i)
print(cd)

###########################
#########90 v90 rfac#######
###########################
rfac90<-wilcox.test(alldata$"rfac90ui",alldata$"rfac90i",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(rfac90$p.value)
####################################
rfac90ui<-na.omit(alldata$rfac90ui)
rfac90i<-na.omit(alldata$rfac90i)
cd<-cliff.delta(rfac90ui,rfac90i)
print(cd)


###########################
########90v300 i rfac######
###########################
rfaci<-wilcox.test(alldata$"rfac90i",alldata$"rfac300i",
                   alternative = "two.sided", paired = FALSE, 
                   exact = FALSE, correct = FALSE)
print(rfaci$p.value)
#########################
cd<-cliff.delta(rfac90i,rfac300i)
print(cd)

############################
#########90v300 ui rfac#####
############################
rfacui<-wilcox.test(alldata$"rfac90ui",alldata$"rfac300ui",
                    alternative = "two.sided", paired = FALSE, 
                    exact = FALSE, correct = FALSE)
print(rfacui$p.value)
#####################################
cd<-cliff.delta(rfac90ui,rfac300ui)
print(cd)
#########################
#######300 i vs PRIWIS rfac#####
#########################
rfac300ui<-na.omit(alldata$rfac300ui)
rfac300i<-na.omit(alldata$rfac300i)
rfacpriwis<-na.omit(alldata$rfacpriwis)
rfacpriwis<-wilcox.test(alldata$"rfacpriwis",alldata$"rfac300i",
                        alternative = "two.sided", paired = FALSE,
                        exact = FALSE, correct = FALSE)
print(rfacpriwis$p.value)
#################################

cd<-cliff.delta(rfacpriwis,rfac300i)
print(cd)
#########################
#######300 ui vs PRIWIS rfac#####
#########################
rfacpriwis<-wilcox.test(alldata$"rfacpriwis",alldata$"rfac300ui",
                        alternative = "two.sided", paired = FALSE,
                        exact = FALSE, correct = FALSE)
print(rfacpriwis$p.value)
#################################

cd<-cliff.delta(rfacpriwis,rfac300ui)
print(cd)

##################################################################################
########################## O-Antigen restored grow up 3 ##########################
##################################################################################

#######################
#######300v300 oar#####
#######################
oar300<-wilcox.test(alldata$"oar300ui",alldata$"oar300i",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(oar300$p.value)
##################################
oar300ui<-na.omit(alldata$oar300ui)
oar300i<-na.omit(alldata$oar300i)
cd<-cliff.delta(oar300ui,oar300i)
print(cd)

#############################
#########90v90 oar###########
#############################
oar90<-wilcox.test(alldata$"oar90ui",alldata$"oar90i",
                   alternative = "two.sided", paired = FALSE,
                   exact = FALSE, correct = FALSE)
print(oar90$p.value)
##################################
oar90ui<-na.omit(alldata$oar90ui)
oar90i<-na.omit(alldata$oar90i)
cd<-cliff.delta(oar90ui,oar90i)
print(cd)

###########################
#######90v300 i oar########
###########################
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
#################################
cd<-cliff.delta(oar90ui,oar300ui)
print(cd)
#########################
#######300 i vs PRIWIS oar#####
#########################
oar300ui<-na.omit(alldata$oar300ui)
oar300i<-na.omit(alldata$oar300i)
oarpriwis<-na.omit(alldata$oarpriwis)
oarpriwis<-wilcox.test(alldata$"oarpriwis",alldata$"oar300i",
                       alternative = "two.sided", paired = FALSE,
                       exact = FALSE, correct = FALSE)
print(oaripriwis$p.value)
#################################

cd<-cliff.delta(oarpriwis,oar300i)
print(cd)
#########################
#######300 ui vs PRIWIS oar#####
#########################
oaripriwis<-wilcox.test(alldata$"oarpriwis",alldata$"oar300ui",
                        alternative = "two.sided", paired = FALSE,
                        exact = FALSE, correct = FALSE)
print(oaripriwis$p.value)
#################################

cd<-cliff.delta(oarpriwis,oar300ui)
print(cd)
##################################################################################
################300iv300 ui oar grow up 2#########################################
##################################################################################
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
###################### Brauns lipoprotein deficient grow up 1 ####################
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

##################################################################################
###################### Brauns lipoprotein deficient grow up 3 ####################
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


##################################################################################
##################################################################################
############ Brauns lipoprotein deficient grow up 1 and 3 randomised##############
##################################################################################
#randomised data set, generated above

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
#######90v300 ui lpp#####
#########################
lppuir<-wilcox.test(alldata$"lpp90uir",alldata$"lpp300uir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppuir$p.value)
#################################
cd<-cliff.delta(lpp90uir,lpp300uir)
print(cd)
#########################
#######300 ui vs PRIWIS lpp#####
#########################
lppuir<-wilcox.test(alldata$"lpppriwis",alldata$"lpp300uir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppuir$p.value)
#################################
cd<-cliff.delta(lpp90uir,lpp300uir)
print(cd)
#########################
#######300 i vs PRIWIS lpp#####
#########################
lppuir<-wilcox.test(alldata$"lpppriwis",alldata$"lpp300ir",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(lppuir$p.value)
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
########################
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
###############################can we combine???######################################
######################################################################################
######################################################################################
#comparing if data that was collected slightly differently is similar enough to be combined

#rfac2 vs 3 300
rfac300<-wilcox.test(alldata$"rfac300i2",alldata$"rfac300i",
                     alternative = "two.sided", paired = FALSE,
                     exact = FALSE, correct = FALSE)
print(rfac300$p.value)

############################
######cliff delta ef########
############################
rfac300i2<-na.omit(alldata$rfac300i2)
rfac300i<-na.omit(alldata$rfac300i)
cd<-cliff.delta(rfac300i2,rfac300i)
print(cd)
#######cant combine

rfac90<-wilcox.test(alldata$"rfac120i2",alldata$"rfac90i",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(rfac90$p.value)
####################################
rfac90i<-na.omit(alldata$rfac90i)
rfac120i2<-na.omit(alldata$rfac120i2)
cd<-cliff.delta(rfac90i,rfac120i2)
print(cd)
############## cant combine

oar300<-wilcox.test(alldata$"oar300i",alldata$"oar300i2",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(oar300$p.value)
####################################
oar300i<-na.omit(alldata$oar300i)
oar300i2<-na.omit(alldata$oar300i2)
cd<-cliff.delta(oar300i,oar300i2)
print(cd)
####################################
bw300ui<-na.omit(alldata$bw300ui)
bw300i<-na.omit(alldata$bw300i)
bw100ui<-na.omit(alldata$bw100ui)
bw100i<-na.omit(alldata$bw100i)
bw300ui2<-na.omit(alldata$bw300ui2)
bw300i2<-na.omit(alldata$bw300i2)
bw100ui2<-na.omit(alldata$bw100ui2)
bw100i2<-na.omit(alldata$bw100i2)


bw300<-wilcox.test(alldata$"bw300i",alldata$"bw300i2",
                    alternative = "two.sided", paired = FALSE,
                    exact = FALSE, correct = FALSE)
print(bw300$p.value)
#sig dif
####################################

cd<-cliff.delta(bw300i,bw300i2)
print(cd)





