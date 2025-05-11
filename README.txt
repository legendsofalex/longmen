Data analysis of length and width measurements taken using MicroeJ- ImageJ plug-in.

11/05/2025||| legendsofalex
under MIT license (see LICENSE)
this repository contains all the code used for handling the data I collected 
over the course of this project. it should be be noted that the raw data will 
NOT be provided however a mock file to display headers will be included for 
completeness. 

this file is broken down into the follow sections
How to use this project- information on installation
Outlining excel documents
Outlining R scripts
credits

################ how to use this project #######################
~~~~~~~~~~~~~~ information on installation ~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
inorder to run the R scripts you will need to install R Studio
head on over to https://cran.rstudio.com/ and follow the hyperlinks
for the type of operating system you're running and install R. 
for this project I used R version 4.4.3 (2025-28-02)

in addition to this some packages need to be installed, these can be
done within R studio using 
      install.packages(ggplot2)
      install.packages(readxl)
      install.packages(effsize)
      install.packages(tidyverse)
      install.packages(ggforce)
      install.packages(dplyr)
these are same packages that i have loaded in, in each R script

inorder to utilise the excel files you will need to install excel
via https://www.microsoft.com/en-gb/microsoft-365/excel or utilise
google sheets for free online and then dowload the files as XLSX
files


################ outlining excel documents #####################
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All data used in scripts is split across two files containing Width
measurements and length measurements. These files contain just the 
data collected optimised for use in scripts, taken from larger files.

in the case of randomised data sets these were generated in R, exported, 
combined, then imported to the length/width file. This is not a 
stream-lined or convenient approach but was just what was done.

files are titled:

they are headered with shorthand for strain corrosponding minutes 
induction state and which data collection they are from, if not taken 
from the most recent or "r" if they are the combinded randomised data set
Eg. bw300ui2 would be BW25113 strain of E. coli of measurements taken 
from the un-induced 300 minute post-inoculation sample during grow-up 2.
if there is no number that indicates grow-up 3 (most up-to-date)

################### outlining R scripts ########################
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Longmen is the working R file for all subsequent scripts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sina graphs were generated in 
  -"Width graphs sina plots.R"
  -"Length graphs sina plots.R"
lppr,bwr,rfac,oar dataframes are what generated the graphs used in
the document.

statistical testing was done in
  -"mann whitney and effect size tests lengths.R"
  -"mann whitney and effect size tests widths.R"
these files also generated the randomised combined data sets used in
graph generating
  -"generalised testing.R"
in here was carried out shapiros and variance tests.

further information in using the scripts can be found within the file 
itself.
######################### Credits ##############################
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data was collected in collaboration with Dr Christoph G. Baumann
at The University of York