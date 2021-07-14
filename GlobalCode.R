
# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science databases
# The columns will need to contain:
# Year; Title; Source.title; Authors; AuthorID; DE; DES; EID; SO; DT

library(dplyr)
library(stringr)
library(tidyr)
library(hablar)
library(ggplot2)
library(maps)
library(countrycode)
library(RColorBrewer)
library(bibliometrix)
library(tidyverse)
library(plotly)
library(gridExtra)
library(extrafont)

#############################################################
#####                      Function                     #####
#############################################################

source("Functions/SearchAndReplace.R")
source("Functions/Diacritics.R")
source("Functions/PartialMatch.R")
source("Functions/RemoveDuplicate.R")


#############################################################
#####                Folder & Files                     #####
#############################################################

# set extension and Citation
extension <- ".TXT"
cit.path.XPS <- "XPS/"

# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.

#############################################################
#####           Keyword Figures Range set               #####
#############################################################
# the number of keywords (top most frequent) appearing in the figure
number <- 50   # target number of keywords appearing in the keyword figure for MergeDataset
number2 <- 28   # target number of keywords appearing in the keyword figure for IFSMS

maximum <- 57  # maximum number of keywords appearing in the keyword figure
maximum2 <- 30  # maximum number of keywords appearing in the keyword figure

#############################################################
#####                       Codes                       #####
#############################################################

# This codes can be run subsequently or independently as each create necessary outputs for the next codes.  

# This R script is the first step to merge exported data from Scopus and Web of Sciences (BibTex format)
# This Script allows to merge the file and exclude records that are not relevant to the field of research
#source("Code/Merger_1_Exclusion.R")
#source("Code/Merger_5_Keywords Analysis.R")

# source("code/Merger_2_KeywordsAnalysis.R")
# The .txt/.csv export is used for the Bibliometric analysis in the following R scripts:

# This R script is the create the keyword figure based on the combined Scopus and Web of Sciences datasets
# This code can be run independe


