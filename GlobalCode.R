###########################################################################
#
# Research trends in forensic science: Fibre and Databases
#
# Leverhulme Research Centre for Forensic Science

# Virginie Galais, Holly Fleming, Hervé Ménard and Niamh Nic Daéid

# Website: https://github.com/LRCFS/
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

###########################################################################

# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science databases
# The columns will need to contain:
# Year; Title; Source.title; Authors; AuthorID; DE; DES; EID; SO; DT
library(plyr)
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
extensionTXT <- ".TXT"
extensionBIB <- ".bib"
extensionCSV <- ".csv"


# where the generated figures are saved, create folder if not existing
Results.dir <- "Results/"
dir.create(file.path(Results.dir),recursive = TRUE) # will create folder if not already there.

#############################################################
#####           Keyword Figures Range set               #####
#############################################################
# the number of keywords (top most frequent) appearing in the figure
numberKeywordDataset <- 73   # target number of keywords appearing in the keyword figure for combined dataset
numberKeywordIfsms <- 24   # target number of keywords appearing in the keyword figure for IFSMS

numberMaxKeywordDataset <- 75  # maximum number of keywords appearing in the keyword figure
numberMaxKeywordIfsms <- 25  # maximum number of keywords appearing in the keyword figure

#############################################################
#####           Source Title Figures Range set          #####
#############################################################
# the number of keywords (top most frequent) appearing in the figure
numberTitle <- 15   # target number of keywords appearing in the keyword figure for MergeDataset
numberTitleMax <- 18   # target number of keywords appearing in the keyword figure for IFSMS

#############################################################
#####                       Codes                       #####
#############################################################

# These codes can be run subsequently or independently as each create necessary outputs for the next codes.

# This R script is the first step to merge exported data from Scopus and Web of Sciences (BibTex format) and allows to merge the file and exclude records that are not relevant to the field of research
source("Code/Merger_1_Exclusion.R")
# This Script allows to compare references from Scopus, Web of Science and the IFSMS reports
source("Code/Merger_2_Comparison of dataset.R")
# This Script allows to compare authors from Scopus, Web of Science and the IFSMS reports
source("Code/Merger_3_Authors Analysis.R")
# This R script is the create the keyword figure based on the combined Scopus and Web of Sciences datasets, and the IFSMS reports
source("Code/Merger_4_Keywords Analysis.R")