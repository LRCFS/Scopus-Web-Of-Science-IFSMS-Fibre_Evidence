# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                     To read                       #####
#############################################################
# This R script is the first step to merge exported data from Scopus and Web of Sciences (BibTex format)
# This Script allows to merge the file and exclude records that are not relevant to the field of research
# The .txt/.csv exported at the end is the one which will be use for the Bibliometric analysis in the following R script:
# Merger_2_Keywords Analysis.R
# Merger_3_Authors Analysis.R
# Merger_2_Document and Country.R

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated from Scopus and Web Of Science
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

#############################################################
#####                      Function  1/4                #####
#############################################################

#### Function to search and replace ####

# Include function to duplicate {Marc Schwartz (via MN) on http://r.789695.n4.nabble.com/replace-values-in-data-frame-td803416.html}
gsr <- function(Source, Search, Replace) 
{ 
  if (length(Search) != length(Replace)) 
    stop("Search and Replace Must Have Equal Number of Items\n") 
  
  Changed <- as.character(Source) 
  
  for (i in 1:length(Search)) 
  { 
    cat("Replacing: ", Search[i], " With: ", Replace[i], "\n") 
    Changed <- replace(Changed, Changed == Search[i], Replace[i]) 
  } 
  
  cat("\n") 
  
  Changed 
}

#############################################################
#####                      Function  2/3                #####
#############################################################

# function to replace accented characters with unaccented equivalents 
# adapted from https://stackoverflow.com/questions/15253954/replace-multiple-letters-with-accents-with-gsub
removeDiacritics <- function(string) {
  chartr(
    "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝàáâãäåçèéêëìíîïðñòóôõöøùúûüýÿ",
    "SZszYAAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyy", 
    string
  )
}

#############################################################
#####                      Function  3/4                #####
#############################################################

# Function, adapted from https://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/

# Function to check for duplicate and partial match between database imports. The genreted list can then be used to make the appropriate changes to the lists

#Data loaded in from downloaded files as:
##PercentageUsingTheNet
##ccode

##Here's where the algorithm starts...
##I'm going to generate a signature from country names to reduce some of the minor differences between strings
##In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
##So for example, United Kingdom would become kingdomunited
##We might also remove stopwords such as 'the' and 'of'.

# Separator <- list(c("\\. |\\.| | \\& |\\: | \\ - |\\-|\\ -|\\- |\\-"))

signature=function(x){
#  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
   sig=paste(sort(unlist(strsplit(x," "))),collapse = '')
#  sig=paste(x)
#  sig=paste(x, sep =" ", collapse = " ")
  sig=x
  return(sig)
}

number = 4.0

partialMatch=function(x,y,levDist = number){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  
  return(matched)
}

#############################################################
#####                   Function  4/4                   #####
#############################################################


rem_dup_word <- function(x){
  paste(unique(trimws(unlist(strsplit(x,split="\\, ",fixed=F,perl=T)))),collapse = 
          " ")
}


#############################################################
#####                  Global variable                  #####
#############################################################

Wos.path = 'Wos/December 2020/'    #Web of Science folder
Sco.path = 'Scopus/December 2020/' #Scopus folder

#############################################################
#####         Data loading Individual citation list     #####
#############################################################

# set extension and folder
extension <- ".bib"
Wos <- Sys.glob(paste(Wos.path, "*", extension, sep = ""))
Sco <- Sys.glob(paste(Sco.path, "*", extension, sep = ""))


#############################################################
#####                    Data loading                   #####
#############################################################

# Using bibtex files instead of csv. This is to resolve avoid one loading issue with Web of Science; incorrect file format and coding.
# This also works for Scopus. Some information is lost such as the EID not available in the bibtex export. 
# Keeping the Scopus csv also offers access to a greater range of entries.

Scopus <- convert2df(Sco,dbsource = "scopus",format = "bibtex")
WebofScience <- convert2df(Wos,dbsource = "isi",format = "bibtex")

# For the purpose of the analysis, each records from 2020 are removed from the current dataset
Scopus <- Scopus[Scopus$PY != "2020" & Scopus$PY != "2021", ]
WebofScience <- WebofScience[WebofScience$PY != "2020" & WebofScience$PY != "2021", ]


#############################################################
#####     Comparison and merging of the two datasets    #####
#############################################################
#####    Each dataset are imported separately using common column labels.
#####    To keep the original data, the columns are remained ending with "S" for Scopus and "W" for Web of Science
#####    To remove duplicate, some correction is needed on the Title

#####             For the results from Scopus           #####
# Select column label $PY, $TI,  $SO, $AU, $DE, $ID, $C1, $DI, url, $SO, $DT, $
ScopusReducedDataset <- Scopus %>%
  select(PY,AU,TI,DE,ID,C1,DI,url,SO,DT)
# transform the years into numeric
ScopusReducedDataset$PY <- as.numeric(ScopusReducedDataset$PY)


####  for rebuilding EID  ####
#### If added need to be included later in merge #####

# ScopusReducedDataset$EID <- sub("http.*.eid=", "", ScopusReducedDataset$url)
# ScopusReducedDataset$EID <- sub("&.*", "", ScopusReducedDataset$EID)

#####    Rename the Keywords lists 
names(ScopusReducedDataset)[names(ScopusReducedDataset)=="ID"] <- "IDS"
names(ScopusReducedDataset)[names(ScopusReducedDataset)=="DE"] <- "DES"
names(ScopusReducedDataset)[names(ScopusReducedDataset)=="C1"] <- "C1S"

#####        For the results from Web of Science        #####
# Select column label $PY, $TI,  $SO, $AU, $DE, $ID, $C1, $DI
WebOfScienceReducedDataset <- WebofScience %>%
  select(PY,AU,TI,DE,ID,C1,DI,SO,DT)
# removing the extra "." in the affiliation after the country
WebOfScienceReducedDataset$C1  <- as.character(gsub("\\.","\\",WebOfScienceReducedDataset$C1))
# Removing each ";" in double in the DE column
WebOfScienceReducedDataset$DE  <- as.character(gsub(";;",";",WebOfScienceReducedDataset$DE))
# removing the extra "NA NA" in the AU column, replace by "" 
#WebOfScienceReducedDataset$AU  <- as.character(gsub("NA NA","", WebOfScienceReducedDataset$AU))

#####    Rename the Keywords lists
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="ID"] <- "IDW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="DE"] <- "DEW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="C1"] <- "C1W"
# removing entries with no year
WebOfScienceReducedDataset <- WebOfScienceReducedDataset[!is.na(WebOfScienceReducedDataset$PY),]
WebOfScienceReducedDataset <- WebOfScienceReducedDataset %>%
  distinct()

#####_________Search for match by title between the two datasets_________##########
# To generate a list of titles with a partial match for external check

matches=partialMatch(WebOfScienceReducedDataset$TI,ScopusReducedDataset$TI)

aggregate(matches$pass, by=list(matches$pass), FUN=length)

PartialExport <- matches %>% filter(pass == "Partial")

# The PartialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
write.table(PartialExport, file = "PartialExport_December.txt", quote = F, sep="\t", row.names = F)

# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title generated in Web of Science will be used to correct the one in Scopus
# Note this does not mean all the title from Web of Science are correct !!!
# Missed duplicate or partial can be added as well. This is the case here with one extra correction added
# "Chang KH, Yew CH, Abdullah AFL. Study on..." changed to "Study on the behaviors of gunshot..."
# Applying the title correction does not necessary mean there will not be further partial match is the function is rerun on the new (corrected) Scopus dataframe. 
TitleCorrection <- read.csv("PartialTitleCorrected_December.txt", sep = "\t", header = TRUE)
TitleCorrection <- as.data.frame(TitleCorrection)

ScopusReducedDataset$TICorrected <- gsr(as.character(ScopusReducedDataset$TI),as.character(TitleCorrection$raw.y),as.character(TitleCorrection$raw.x.Corrected))

#####_________Search for authors between the two datasets_________##########
##### to generate a correction list of authors
##### The recombined list should be the same length as the original minus any duplicates. Duplicates must be checked.


      # 1) SCOPUS

# Split Column "AU" with the separator ";" and place it in AuthorListScopusExtended
AuthorListScopus <- ScopusReducedDataset %>%
  select(PY,TICorrected,AU,DES,IDS,C1S,DI,SO,DT)
# rename TICorrected column
names(AuthorListScopus)[names(AuthorListScopus)=="TICorrected"] <- "TI"

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListScopusExtended <- AuthorListScopus %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# export to correct the author names
#write.table(AuthorListScopusExtended, "Authors to correct Scopus_December.txt", sep = "\t")

# read the corrected list of "Authors" and combine it to the original list.
# Correction from both Scopus and Web of Science - see line 332 to export the authors names from Web of Science)
AuthorCorrected <- read.csv("Authors Name Corrected_December.txt", sep="\t", header=TRUE)
AuthorListScopusExtended$AuthorsCor <- gsr(AuthorListScopusExtended$Authors,AuthorCorrected$name, as.character(AuthorCorrected$Name.corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
# The corrected list should be the same lenght as the original version. Duplicates will be removed and they will need to be checked individually.
ScopusReducedDatasetCorrected <- AuthorListScopusExtended %>%
  group_by(PY,TI,AU,DES,IDS,C1S,DI,SO,DT) %>%
  summarise(Authors = paste(Authors, collapse = ";"), AuthorCorrected = paste(AuthorsCor, collapse = ";")) %>%
  ungroup()

# Duplicate check on the output:
DupeScopus <- ScopusReducedDatasetCorrected %>%
  find_duplicates(PY,TI,AU)

# in this case found that 1 distinct records have different affiliation, 2 records don't have the same keywords (IDS), the last one have different affiliation and Source
# correcting the one with the keywords
ScopusReducedDatasetCorrected <- ScopusReducedDatasetCorrected %>%
  mutate(IDS = strsplit(as.character(IDS), ";", ))%>%
  unnest(IDS) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(PY,AU,TI,DES,C1S,DI,SO,DT,Authors,AuthorCorrected) %>%
  summarise(IDS = sort(paste(IDS, collapse= ";")))%>%
  ungroup()

DupeScopus2 <- ScopusReducedDatasetCorrected %>%
  find_duplicates(PY,TI,AU)

# correcting the one which have different affiliations
ScopusReducedDatasetCorrected <- ScopusReducedDatasetCorrected %>%
  mutate(C1S = strsplit(as.character(C1S), ","))%>%
  unnest(C1S) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(PY,AU,TI,DES,IDS,DI,SO,DT,Authors,AuthorCorrected) %>%
  summarise(C1S = sort(paste(C1S, collapse= ",")))%>%
  ungroup()

DupeScopus3 <- ScopusReducedDatasetCorrected %>%
  find_duplicates(PY,TI,AU)

# still have an issue with 2 of them, just delete the row (the reference with keyword was chosen for one, the source from Europe for the second one)
# order by authors name
ScopusReducedDatasetCorrected <- arrange(ScopusReducedDatasetCorrected, AU)
# row to delete is 556 and 1168
ScopusReducedDatasetCorrected <-ScopusReducedDatasetCorrected[-556,]
ScopusReducedDatasetCorrected <-ScopusReducedDatasetCorrected[-1168,]

DupeScopus4 <- ScopusReducedDatasetCorrected %>%
  find_duplicates(PY,TI,AU)



      # 2) WEB OF SCIENCE

# Split Column "AU" with the separator ";" and place it in AuthorListWebOfScienceExtended
AuthorListWebOfScience <- WebOfScienceReducedDataset %>%
  select(PY,TI,AU,DEW,IDW,C1W,DI,SO,DT)

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListWebOfScienceExtended <- AuthorListWebOfScience %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# export to correct the author names
#write.table(AuthorListWebOfScienceExtended, "Authors to correct WoS.txt", sep = "\t")

# read the corrected list of "Authors" and combine it to the original list
# Correction apply to Scopus and Web of Science was combined
AuthorListWebOfScienceExtended$AuthorsCor <- gsr(AuthorListWebOfScienceExtended$Authors,AuthorCorrected$name, as.character(AuthorCorrected$Name.corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
WebOfScienceReducedDatasetCorrected <- AuthorListWebOfScienceExtended %>%
  group_by(PY,TI,AU,DEW,IDW,C1W,DI,SO,DT) %>%
  summarise(Authors = paste(Authors, collapse = ";"), AuthorCorrected = paste(AuthorsCor, collapse = ";")) %>%
  ungroup()

DupeWebOfScience <- WebOfScienceReducedDatasetCorrected %>%
  find_duplicates(PY,TI,AU)


#####_________Make some correction to Document Type_________##########
# As Web of Science and Scopus don't have the same way to define the name of the document types, some corrections are needed
# It has been chose to use the Document Type from Scopus to correct the one on Web Of Science

WebOfScienceReducedDatasetCorrected$DT <- gsub("ARTICLE; BOOK CHAPTER","BOOK CHAPTER",WebOfScienceReducedDatasetCorrected$DT)
WebOfScienceReducedDatasetCorrected$DT <- gsub("ARTICLE; PROCEEDINGS PAPER","PROCEEDINGS",WebOfScienceReducedDatasetCorrected$DT)
WebOfScienceReducedDatasetCorrected$DT <- gsub("PROCEEDINGS PAPER","PROCEEDINGS",WebOfScienceReducedDatasetCorrected$DT)


#####_________Search for match by Source between the two datasets_________##########
##### to generate a list of titles with a partial match for external check

matches2=partialMatch(WebOfScienceReducedDatasetCorrected$SO,ScopusReducedDatasetCorrected$SO)

aggregate(matches2$pass, by=list(matches2$pass), FUN=length)

PartialExport2 <- matches2 %>% filter(pass == "Partial")

# The PartialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
write.table(PartialExport2, file = "PartialExport_Source_December.txt", quote = F, sep="\t", row.names = F)

# Correction to the Journal can be applied at this stage. This can be done in Notepad++, Excel etc.
# The Source generated in Scopus is use to correct the one in WoS
# The \& symbol present in the source from Web of science must be corrected as well
SourceCorrection <- read.csv("PartialExport_SourceCorrected.txt", sep = "\t", header = TRUE)
SourceCorrection <- as.data.frame(SourceCorrection)

WebOfScienceReducedDatasetCorrected$SOCorrected <- gsr(as.character(WebOfScienceReducedDatasetCorrected$SO), as.character(SourceCorrection$raw.x), as.character(SourceCorrection$raw.y))
WebOfScienceReducedDatasetCorrected <- WebOfScienceReducedDatasetCorrected%>%
  select(PY,AU,DEW,IDW,C1W,DI,SOCorrected,DT,Authors,AuthorCorrected,TI)
# rename SOCorrected column
names(WebOfScienceReducedDatasetCorrected)[names(WebOfScienceReducedDatasetCorrected)=="SOCorrected"] <- "SO"


########################################################
#####        To combine both dataset into one      #####
########################################################

# Combining the two dataset
DatabaseOutputTemp <- bind_rows(WebOfScienceReducedDatasetCorrected, ScopusReducedDatasetCorrected)
DatabaseOutputTemp <- as.data.frame(DatabaseOutputTemp)

DatabaseOutput <- DatabaseOutputTemp %>%
  select(TI,PY,AuthorCorrected,DEW,IDW,DES,IDS,C1W,C1S,DI,SO,DT)

# To summarise by removing duplicate, grouping by Title and year. 
CombinedDataset <- DatabaseOutput %>%
  group_by(TI, PY) %>% summarise(AU = rem_dup_word(paste(AuthorCorrected[!is.na(AuthorCorrected)], collapse="; ")), 
                                DOI = rem_dup_word(paste(DI[!is.na(DI)], collapse=", ")),
                                DEW = paste(DEW[!is.na(DEW)], collapse=";"), 
                                IDW = paste(IDW[!is.na(IDW)], collapse=";"), 
                                DES = paste(DES[!is.na(DES)], collapse=";"),
                                IDS = paste(IDS[!is.na(IDS)], collapse=";"),
                                C1W = paste(C1W[!is.na(C1W)], collapse=","),
                                C1S = paste(C1S[!is.na(C1S)], collapse=","),
                                DT = rem_dup_word(paste(DT[!is.na(DT)], collapse=", ")),
                                SO = rem_dup_word(paste(SO[!is.na(SO)], collapse=", "))) %>% ungroup()

# In the Authors column, because the number of author is sometimes different between Scopus and Web of Science, a correction must be done
CombinedDataset <- CombinedDataset %>%
  mutate(AU = strsplit(as.character(AU), ";"))%>%
  unnest(AU) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(TI,PY,DEW,IDW,DES,IDS,C1W,C1S,DOI,SO,DT) %>%
  summarise(AU = sort(paste(AU, collapse= ";")))%>%
  ungroup()
CombinedDataset$AU[CombinedDataset$AU==""]<-NA

# Add some correction to SO
# This error is particular to the dataset and need to be checked
SourceCorrection2 <- read.csv("SourceCorrected2.txt", sep = "\t", header = TRUE)
SourceCorrection2 <- as.data.frame(SourceCorrection2)
CombinedDataset$SOCorrected <- gsr(as.character(CombinedDataset$SO), as.character(SourceCorrection2$raw.x), as.character(SourceCorrection2$raw.y))


# Filling Affiliations columns empty string with NA
 CombinedDataset$C1S[CombinedDataset$C1S==""]<-NA
 CombinedDataset$C1W[CombinedDataset$C1W==""]<-NA

# Scopus affiliation set as default for all the records
# If Scopus has no affiliations entries, WEb of Science is used as altenative including no entries.
TempScopusAffiliations <- CombinedDataset[!is.na(CombinedDataset$C1S),] %>%
  mutate(C1 = C1S)

TempWebOfSAffiliations <- CombinedDataset[is.na(CombinedDataset$C1S),] %>%
  mutate(C1 = C1W)

CombinedDataset <- rbind(TempScopusAffiliations,TempWebOfSAffiliations)


# Check for duplicate in the combined datasets
DupeCombinedDataset <- CombinedDataset %>%
  find_duplicates(TI)

# The Authors' keywords (i.e. DEW amd DES) between the two lists should be the same, however it is not the case and some correction will be needed.
# The user can decide to use the original list of keywords given by the databases instead, adding "S" or "W" to "DE"
# The merged list (i.e. DEW +DES) is more exhautive than the individual one as for some records as the keywords do not appear in both lists
# It is best to remove  the white space present with the ";"
CombinedDataset <- transform(CombinedDataset, DE=paste(DEW, DES, sep="; "))
CombinedDataset$DE  <- as.character(gsub("; ",";",CombinedDataset$DE))
CombinedDataset <- CombinedDataset %>%
  select(TI,PY,AU,DOI,DEW,IDW,DES,IDS,C1W,C1S,C1,DT,SOCorrected,C1,DE,DOI)
names(CombinedDataset)[names(CombinedDataset)=="SOCorrected"] <- "SO"

# In the DE column, because the order of the author's keyword is sometimes different between Scopus and Web of Science, a correction has been done
# It has been chosen to remove each keywords that were duplicated in the column DE
CombinedDataset <- CombinedDataset %>%
  mutate(DE = strsplit(as.character(DE), ";"))%>%
  unnest(DE) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(TI,PY,AU,DEW,IDW,DES,IDS,C1W,C1,C1S,DOI,SO,DT) %>%
  summarise(DE = sort(paste(DE, collapse= ";")))%>%
  ungroup()


#write.table(CombinedDataset, file = "ScopWos merge_December.txt", sep = "\t", row.names = F)

#######################################################################
#####                       EXCLUSION LIST                        #####
#######################################################################
# This section is to narowing down the CombinedDataset
# The exclusion list is generated by filtering CombinedDataset based on a Journal, Keywords and Title
# One of the following options must be chosen

#   Author Keywords only
 names(CombinedDataset) <- sub("DE","AIK", names(CombinedDataset))

#   Database Scopus Keywords only 
# names(CombinedDataset) <- sub("DES","AIK", names(CombinedDataset))

#   Database WEb of Science Keywords only
# names(CombinedDataset) <- sub("DEW","AIK", names(CombinedDataset))

#   Index and Database Keywords
# Combine Columns DE, DES and DEW, place in Column name "AIK" and remove original columns
 #CombinedDataset <- CombinedDataset %>%
  #unite("AIK", DE, IDS, IDW, sep = ";", remove = TRUE)

#######################################################################
#####          Creating an exclusion list based on Journal        #####
#######################################################################

# Journal list from scopus is available at https://www.scopus.com/sources.uri?zone=TopNavBar&origin=searchbasic
# read the export *.csv forensic Journal list from Scopus, separation "\t", and place it in data.frame "ScopusForensicJournalList"
ScopusForensicJournalList <- read.csv("Scopus_Journal list_Forensic and Justice_21-04-20.txt", sep="\t", header=TRUE)

# Creating a list of keywords for exclusion
# The keywords should be chosen in relation to the research areas to be excluded
removejournal.list <- paste(c("Medicine", 
                              "medicine", 
                              "Genetics", 
                              "genetics", 
                              "Psychology",
                              "Nursing",
                              "Psychiatry",
                              "Neuropsychology",
                              "Social",
                              "Digital",
                              "Toxicology",
                              "Radiology"), collapse = '|')
removejournallist <- as.data.frame(removejournal.list)

# Remove from "ScopusForensicJournalList" every Journal with keywords from "removejournal.list" and place it in a new list "InclusionJournals"
InclusionJournals <- ScopusForensicJournalList %>%
  filter(!grepl(removejournal.list, ScopusForensicJournalList$Source.title))
InclusionJournalsReduced <- InclusionJournals %>%
  select(Source.title)
# Upper case "Source.title" in "InclusionJournals"
InclusionJournalsReduced$Source.title <- toupper(InclusionJournals$Source.title)

# Creating a list of documents included, based on Source.title
InclusionDataSet <-subset(CombinedDataset,SO %in% InclusionJournalsReduced$Source.title)

# Creating a list of documents excluded, based on Source.title
ExclusionDataSet <- setdiff(CombinedDataset,InclusionDataSet)

#######################################################################
#####           Testing the exclusion and inclusion list          #####
#######################################################################
### Definition
# True Positive : documents declared relevant to the research subject, and they are relevant
# False positive : documents declared relevant to the research subject, while they are not relevant
# True negative : documents declared not relevant to the research subject, and they are not relevant
# False negative : documents declared not relevant to the research subject, while they are relevant

# 1) Inclusion List
# Creating a list of keywords relevant to the field of research
Keyword.list <- paste(c("FIBRE","FIBER",
                        "CLOTHING", 
                        "TEXTILE",
                        "TEXTILE FIBRE", "TEXTILE FIBER",
                        "DYE", "COLOUR", "COLOR"), collapse = '|')

# Creating a new list of document which don't have any of the keywords from Keyword.list
FalsePositiveList <- InclusionDataSet[-grep(Keyword.list, InclusionDataSet$AIK), ]

# False positive (FP) documents - documents in InclusionDataSet which can be not relevant (based on Keyword.list)
FPdocument  <- as.numeric(count(FalsePositiveList))

# True Positive (TP) documents - documents in InclusionDataSet which are relevant (based on Keyword.list)
TruePositiveList <- InclusionDataSet[grep(Keyword.list, InclusionDataSet$AIK), ]
TPdocument  <- as.numeric(count(TruePositiveList))

# Total number of documnet in the inclusion list
Includeddocument <- as.numeric(count(InclusionDataSet))

# % of FP in the exlusion list
FPdocument/Includeddocument*100

# Verification - Is FPdocument + TPdocument = Includeddocument ?
test1 <- FPdocument + TPdocument
ifelse(Includeddocument == test1, "Correct", "Not correct")
V1 <- as.data.frame(ifelse(Includeddocument == test1, "Correct", "Not correct"))


# 2) Exclusion List
# Creating a new list of document which have, at least, one of the keywords from Keyword.list
FalseNegativeList <- ExclusionDataSet[grep(Keyword.list, ExclusionDataSet$AIK), ]

# False negative (FN) documents - documents in ExclusionDataSet which can be relevant (based on Keyword.list)
FNdocument  <- as.numeric(count(FalseNegativeList))

# True Negative documents - documents in ExclusionDataSet which are not relevant (based on Keyword.list)
TrueNegativeList <- ExclusionDataSet[-grep(Keyword.list, ExclusionDataSet$AIK), ]
TNdocument  <- as.numeric(count(TrueNegativeList))

# Total number of documnet in the excluded list
Excludeddocument <- as.numeric(count(ExclusionDataSet))

# Number of FN in the exlusion list
FNdocument/Excludeddocument*100

# Verification - Is FNdocument + TNdocument = Excludeddocument ?
test2 <- FNdocument + TNdocument
ifelse(Excludeddocument == test2, "Correct", "Not correct")
V2 <- as.data.frame(ifelse(Excludeddocument == test2, "Correct", "Not correct"))

# 3) Recreating the dataset after the first cleaning
# creating a new dataset "ScopusCleanedData" with the True positive documents from InclusionDataSetBis and False negative documents from ExclusionDataSetBis
CombinedDataset2 <- rbind(TruePositiveList, FalseNegativeList)

#######################################################################
#####     Second cleaning of the dataset based on AIKeywords      #####
#######################################################################

# 1) Cleaning the inclusion List
# The keywords should be chosen in relation to the research areas to be excluded
# Remove from inclusion list every document with particular keywords in it
removeKeywords.list <- paste(c("GUNSHOT RESIDUE", "GSR", "GUNSHOT","FIREAMRS", 
                               "DNA", 
                               "FINGERPRINT", 
                               "DRUG",
                               "DOCUMENT",
                               "AXONAL", "HISTOLOGY",
                               "OPTICAL FIBRE", "OPTICAL FIBER"), collapse = '|')
removeKeywordslist <- as.data.frame(removeKeywords.list)

# Creating a new list of document which don't have any of the keywords from removeKeywords.list
InclusionDataSetBis <- CombinedDataset2 %>%
  filter(!grepl(removeKeywords.list, CombinedDataset2$AIK))

# Eclusion List bis
ExclusionDataSetBis <- setdiff(CombinedDataset2,InclusionDataSetBis)

# testing the new inclusion list with the previous"Keyword.list"
FalsePositiveListBis <- InclusionDataSetBis[-grep(Keyword.list, InclusionDataSetBis$AIK), ]

# New calcul of False positive (FP) (based on the previous Ketword.list)
FPdocumentBis  <- as.numeric(count(FalsePositiveListBis))

# New calcul of True positive (TP) (based on the previous Ketword.list)
TruePositiveListBis <- InclusionDataSetBis[grep(Keyword.list, InclusionDataSetBis$AIK), ]
TPdocumentBis  <- as.numeric(count(TruePositiveListBis))

# Total number of document in the inclusion list
IncludeddocumentBis <- as.numeric(count(InclusionDataSetBis))

# % of FP in the exlusion list
FPdocumentBis/IncludeddocumentBis*100

# Verification - Is FPdocumentBis + TPdocumentBis = IncludeddocumentBis ?
test3 <- FPdocumentBis + TPdocumentBis
ifelse(IncludeddocumentBis == test3, "Correct", "Not correct")
V3 <- as.data.frame(ifelse(IncludeddocumentBis == test3, "Correct", "Not correct"))

#######################################################################
#####        Third cleaning of the dataset based on Title         #####
#######################################################################
# The aim of this part is to include earlier articles about fibre, excluded in the first place because they did not provide keywords
# the keyword.list and removeKeyword.list from the first cleaning will be use on the title from the exclusion lisst
# These articles will be indentify in the first Exclusion dataset (ExclusionDataSet)

# 1) Remove from ExclusionDataSet references which have keywords from the fibre forensic field in the Title
# Creating a new list of document which have, at least, one of the keywords from Keyword.list in the title
FalseNegativeListBis <- ExclusionDataSet[grep(Keyword.list, ExclusionDataSet$TI), ]

# False negative (FNBis) documents - documents in ExclusionDataSet which can be relevant (based on Keyword.list)
FNdocumentBis  <- as.numeric(count(FalseNegativeListBis))

# True Negative documents - documents in ExclusionDataSet which are not relevant (based on Keyword.list)
TrueNegativeListBis <- ExclusionDataSet[-grep(Keyword.list, ExclusionDataSet$TI), ]
TNdocumentBis  <- as.numeric(count(TrueNegativeListBis))

# Total number of document in the excluded list
Excludeddocumentbis <- as.numeric(count(ExclusionDataSet))

# Number of FN in the exlusion list
FNdocumentBis/Excludeddocumentbis*100

# Verification - Is FNdocumentBis + TNdocumentBis = Excludeddocumentbis ?
test4 <- FNdocumentBis + TNdocumentBis
ifelse(Excludeddocumentbis == test4, "Correct", "Not correct")
V4 <- as.data.frame(ifelse(Excludeddocumentbis == test4, "Correct", "Not correct"))

# 2) Remove from the new FalseNegative list references which have keywords from other field in the Title
# Creating a new list of document which don't have any of the keywords from removeKeywords.list
FinalFalseNegativelist <- FalseNegativeListBis %>%
  filter(!grepl(removeKeywords.list, FalseNegativeListBis$TI))

# FinalTrueNegative list
FinalTrueNegativelist <- setdiff(FalseNegativeListBis,FinalFalseNegativelist)
X <- rbind(FinalTrueNegativelist, TrueNegativeListBis)
FinalTrueNegativelist <- X

#######################################################################
#####                Creating the new dataset                     #####
#######################################################################

# creating a new dataset "ScopusCleanedData" with the True positive documents from InclusionDataSetBis
CombinedDataset3 <- rbind(TruePositiveListBis, FinalFalseNegativelist) %>% distinct()

#######################################################################
#####              Overview of all the verifications              #####
#######################################################################
Verifications <- V1
names(Verifications) <- c("Verifications")

Verifications[2,1] <- V2
Verifications[3,1] <- V3
rownames(Verifications)[rownames(Verifications)=="1"] <- "FPdocument + TPdocument = Includeddocument ?"
rownames(Verifications)[rownames(Verifications)=="2"] <- "FNdocument + TNdocument = Excludeddocument ?"
rownames(Verifications)[rownames(Verifications)=="3"] <- "FPdocumentBis + TPdocumentBis = IncludeddocumentBis ?"

show(Verifications)

#######################################################################
#####                     EXPORT FINAL DATA                       #####
#######################################################################

write.table(CombinedDataset3, file = "Merger_Dataset_Final_December.txt", sep = "\t", row.names = F)

#############################################################
#####                 General information               #####
#############################################################
# This section is to generate a table with general information about the dataset

#####______________Number of document______________##########
# From Scopus
NDScop <- data.frame(nrow(Scopus))
names(NDScop) <- c("Number")
GF <- NDScop
rownames(GF)[rownames(GF)=="1"] <- "Number of document from Scopus"

#from WoS
NDWoS <- data.frame(nrow(WebofScience))
GF[2,1] <- NDWoS
rownames(GF)[rownames(GF)=="1"] <- "Number of document from Web of Science"

# From Merge before exclusion
ND <- data.frame(nrow(CombinedDataset))
GF[3,1] <- ND
rownames(GF)[rownames(GF)=="1"] <- "Total of document before exclusion"

# From Merge after exclusion
NDex <- data.frame(nrow(CombinedDataset3))
GF[4,1] <- NDex

rownames(GF)[rownames(GF)=="1"] <- "Total of document after exclusion"

#####______________Keywords and Journal______________##########
#after exclusion
#Number of different Journals (NDS) 
NDS <- data.frame(table(CombinedDataset3$SO, exclude = ""));NDS
GF[5,1] <- nrow(NDS)
rownames(GF)[rownames(GF)=="5"] <- "Number of different journals"

# Number of Authors Keywords before correction
X <- CombinedDataset3 %>% 
  select(AIK) %>% 
  mutate(AIK = strsplit(as.character(AIK), ";")) %>% 
  unnest(AIK) %>%
  mutate_if(is.character, str_trim) #calculating the total number of keywords
GF[6,1] <-nrow(X)
rownames(GF)[rownames(GF)=="6"] <- "Total number of Authors Keywords"
NK <- data.frame(table(X, exclude = ""));NK
GF[7,1] <-nrow(NK)
rownames(GF)[rownames(GF)=="7"] <- "Distinct Authors Keywords"

#####______________Document type______________##########
# For Scopus
# Change the name of the type of document
# read the corrected list of "document" and combine it to the original list
DocumentCorrected <- read.csv("Document Type Name Corrected_ScopWoS.txt", sep="\t", header=TRUE)
Scopus$Document.TypeC <- gsr(Scopus$DT, DocumentCorrected$name, as.character(DocumentCorrected$Name.Corrected))

# Count the number of time each document type appear
DTScop <- data.frame(table(Scopus$Document.TypeC, exclude = ""))
DTScop <- data.frame(table(Scopus$Document.TypeC, exclude = NA));DTScop
names() <- c("Document Type", "Count")

# For WoS
# Change the name of the type of document
WebofScience$Document.TypeC <- gsr(WebofScience$DT, DocumentCorrected$name, as.character(DocumentCorrected$Name.Corrected))

# Count the number of time each document type appear
DTWoS <- data.frame(table(WebofScience$Document.TypeC, exclude = ""))
DTWoS <- data.frame(table(WebofScience$Document.TypeC, exclude = NA));DTWoS
names(DTWoS) <- c("Document Type", "Count")

#####______________Exportation______________##########
# To export the first table (General Information)
#write.table(GF, file = "General Information_ScopWoS_December.csv", quote = F, sep = "\t", row.names = F)

#To export the second table (Document Type)
#write.table(DTScop, file = "Document type_Scopus_December.csv", quote = F, sep = "\t", row.names = F)
w#rite.table(DTWoS, file = "Document type_Wos_December.csv", quote = F, sep = "\t", row.names = F)


##################################################################################
#####              Comparison Scopus/Web Of Science all year                 #####
##################################################################################
# This section is to overview the differences between Scopus and Web of science in terms of number of articles
# Creating 2 new dataset to not overwrite the data 
WebOfScience2 <- WebOfScienceReducedDatasetCorrected
Scopus2 <- ScopusReducedDatasetCorrected

# Label each row with the name of the database from where it came from 
Scopus2$Coder <- "Scopus"
WebOfScience2$Coder <- "WebOfScience"

      # 1) Creating a list or document present in Web of Science but not in Scopus
# Creating a list from Scopus Title
ScopusTitleList <- Scopus2 %>%
  select(TI)

# List of records from Web of Science that are not in the Scopus database (Title based)
WoSExclusive <- subset(WebOfScience2,!(TI %in% ScopusTitleList$TI))
# List of records from WoS that are in the Scopus database (Title based)
WoSNotExclusive <- subset(WebOfScience2,TI %in% ScopusTitleList$TI)

### Check that WoSExclusive + WoSNotExclusive = WebOfScience2 ###
X <- as.numeric(count(WoSExclusive)); X
Y <- as.numeric(count(WoSNotExclusive)); Y
Z <- as.numeric(count(WebOfScience2)); Z
ifelse(X+Y == Z, "Correct", "Not correct")

      # 2) Calculating the % of record present in WoS but not in Scopus
# Total number of record in the excluded list
NumberofexcludedDocument <- as.numeric(count(WoSExclusive)) # same thing as X above, just with another name

# Number Total of record on WoS
TotalWoS <- as.numeric(count(WebOfScience2)) # same thing as Z above, just with another name

# % of record from WoS present in Scopus
NumberofexcludedDocument/TotalWoS*100 

      # 3) Creating a list or document present in Scopus but not in Interpol
# Creating a list from Scopus Title
WoSTitleList <- WebOfScience2 %>%
  select(TI)

# List of records from Scopus that are not in the Interpol database (TI based)
ScopusExclusive <- subset(Scopus2,!(TI %in% WoSTitleList$TI))
# List of records from Scopus that are in the Interpol database (TI based)
ScopusNotExclusive <- subset(Scopus2,TI %in% WoSTitleList$TI)

### Check that ScopusExclusive + ScopusNotExclusive = Scopus2 ###
A <- as.numeric(count(ScopusExclusive)); A
B <- as.numeric(count(ScopusNotExclusive)); B
C <- as.numeric(count(Scopus2)); C
ifelse(A+B == C, "Correct", "Not correct")

      # 4) Calculating the % of record present in Scopus but not in Interpol
# Total number of record in the excluded list
ScopusNumberofexcludedDocument <- as.numeric(count(ScopusExclusive)) # same thing as A above, just with another name

# Number Total of record on Scopus
TotalScopus <- as.numeric(count(Scopus2)) # same thing as C above, just with another name

# % of record from Scopus present in Interpol
ScopusNumberofexcludedDocument/TotalScopus*100

### Check that WoSNotExclusive = ScopusNotExclusive  ###
Y <- as.numeric(count(WoSNotExclusive)); Y
B <- as.numeric(count(ScopusNotExclusive)); B
ifelse(Y == B, "Correct", "Not correct")

# there is 3 differences between Y and B
# ADVANCING THE FORENSIC ANALYSIS OF DYED FIBERS BY TIME-OF-FLIGHT MASS SPECTROMETRY is in double in Scopus because one is a conference paper, the other one is an article
# APPLICATION OF DYE ANALYSIS IN FORENSIC FIBRE AND TEXTILE EXAMINATION: CASE EXAMPLES is in double in WoS for the same reason as above
# FORENSIC SCIENCE, APPLICATIONS OF RAMAN SPECTROSCOPY TO FIBER ANALYSIS is in double in Scopus because they don't have the same IDS (one entry don't have one)
# these file must be deleted manually, the code to remove these will be add at the begining
Scopus2 <- arrange(Scopus2, TI)
WebOfScience2 <- arrange(WebOfScience2, TI)
# to delete them
#Scopus2 <-Scopus2[-125,] #for the first one
#WebOfScience2 <-WebOfScience2[-112,] #for the second one
#Scopus2 <-Scopus2[-666,] #for the third one