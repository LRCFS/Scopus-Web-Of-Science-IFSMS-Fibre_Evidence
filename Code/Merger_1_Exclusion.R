#############################################################
#####                     To read                       #####
#############################################################
# This R script is the first step to merge exported data from Scopus and Web of Sciences (BibTex format)
# This script allows to merge the file and exclude records that are not relevant to the field of research
# This script also allows to import data from the IFSMS reports and make some corrections to the dataset
# The .txt/.csv exported at the end is the one which will be use for the Bibliometric analysis in the following R script:
# Merger_2_Comparison of dataset.R
# Merger_3_Authors Analysis.R
# Merger_4_Keywords Analysis.R

#############################################################
#####                  Global variable                  #####
#############################################################

Wos.path = 'InputData/Wos/Nov2021/'    #Web of Science folder
Sco.path = 'InputData/Scopus/Nov2021/' #Scopus folder
ifsms.path = 'InputData/IFSMS/' #IFSMS folder

#############################################################
#####         Data loading Individual citation list     #####
#############################################################

# set extension and folder

Wos <- Sys.glob(paste(Wos.path, "*", extensionBIB, sep = ""))
Sco <- Sys.glob(paste(Sco.path, "*", extensionBIB, sep = ""))

# specify the type of file we want to import (.csv) for IFSMS
#files <- list.files(path = ifsms.path,pattern="*.csv") ; files

IFSMSfiles <- list.files(ifsms.path, pattern=extensionCSV, full.names=TRUE)

#############################################################
#####                    Data loading                   #####
#############################################################

# Using bibtex files instead of csv. This is to resolve avoid one loading issue with Web of Science; incorrect file format and coding.
# This also works for Scopus. Some information is lost such as the EID not available in the bibtex export. 
# Keeping the Scopus csv also offers access to a greater range of entries.

Scopus <- convert2df(Sco,dbsource = "scopus",format = "bibtex")
WebofScience <- convert2df(Wos,dbsource = "isi",format = "bibtex")

IFSMS <- ldply(IFSMSfiles, read_csv)

names(IFSMS)[5] <- c("Source.title")
names(IFSMS)[17] <- c("AKeywords")
names(IFSMS)[20] <- c("Document.Type")

# Removing every year after 2020
Scopus <- filter(Scopus, PY<2021)
WebofScience <- filter(WebofScience, PY<2021)

##############################################
#####     Merging of the two datasets    #####
##############################################
#####    Each dataset are imported separately using common column labels.
#####    To keep the original data, the columns are remained ending with "S" for Scopus
#####                                                                and "W" for Web of Science
#####    To remove duplicate, some correction is needed on the Title


########################################
#####             Scopus           #####
########################################

# Select column label $PY, $TI,  $SO, $AU, $DE, $ID, $C1, $DI, $SO, $DT, $
ScopusReducedDataset <- Scopus %>%
  select(PY,AU,TI,DE,ID,C1,DI,SO,DT)
# Label each row with the name of the database from where it came from 
ScopusReducedDataset$Coder <- "Scopus"

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


#############################
##### Title correction ##### 
#############################

# load correction list
CorrectionTitle <- read.csv("CorrectionLists/PartialTitleCorrected.txt", sep = "\t", header = TRUE)
CorrectionTitle <- as.data.frame(CorrectionTitle)
ScopusReducedDataset$TICorrected <- gsr(as.character(ScopusReducedDataset$TI),as.character(CorrectionTitle$raw.y),as.character(CorrectionTitle$raw.x.Corrected))

# summarise the corrected information
ScopusReducedDatasetTIcor <- ScopusReducedDataset %>%
  select(PY,TICorrected,AU,DES,IDS,C1S,DI,SO,DT,Coder)
# rename TICorrected column
names(ScopusReducedDatasetTIcor)[names(ScopusReducedDatasetTIcor)=="TICorrected"] <- "TI"


#############################
##### Author correction ##### 
#############################

# Split Column "AU" with the separator ";" and place it in ScopusReducedDatasetTIcorExtended
ScopusReducedDatasetTIcorExtended <- ScopusReducedDatasetTIcor %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# load correction list
# Correction list apply to Scopus and Web of Science
CorrectionAuthor <- read.csv("CorrectionLists/AuthorsNameCorrected.txt", sep="\t", header=TRUE)
ScopusReducedDatasetTIcorExtended$AuthorsCor <- gsr(ScopusReducedDatasetTIcorExtended$Authors,CorrectionAuthor$name, as.character(CorrectionAuthor$Name.corrected))

# summarise the corrected information
# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
# The corrected list should be the same length as the original version. Duplicates will be removed and they will need to be checked individually.
ScopusReducedDatasetTIAUcor <- ScopusReducedDatasetTIcorExtended %>%
  select(PY,TI,AuthorsCor,DES,IDS,C1S,DI,SO,DT,Coder) %>%
  dplyr::group_by(PY,TI,DES,IDS,C1S,DI,SO,DT,Coder) %>%
  dplyr::summarise(AU = paste(AuthorsCor, collapse = ";")) %>%
  ungroup()

# Apply some correction to previous list
ScopusReducedDatasetTIAUcor$AU <- gsub(", JR"," JR",ScopusReducedDatasetTIAUcor$AU)
ScopusReducedDatasetTIAUcor$AU <- gsub(", II","",ScopusReducedDatasetTIAUcor$AU)
ScopusReducedDatasetTIAUcor$AU <- gsub(", III, ",", ",ScopusReducedDatasetTIAUcor$AU)
ScopusReducedDatasetTIAUcor$AU <- gsub(",",";", ScopusReducedDatasetTIAUcor$AU)

##################################
##### Affiliation correction ##### 
##################################

# Split Column "C1S" with the separator ";" and place it in ScopusReducedDatasetTIAUcorExtended
ScopusReducedDatasetTIAUcorExtended <- ScopusReducedDatasetTIAUcor %>% 
  mutate(Affiliation = strsplit(as.character(ScopusReducedDatasetTIAUcor$C1S), ";"))%>% 
  unnest(Affiliation) %>%
  mutate_if(is.character, str_trim)

# load correction list
CorrectionAffiliation <- read.csv("CorrectionLists/AffiliationCorrections.txt", sep="\t", header=TRUE)
ScopusReducedDatasetTIAUcorExtended$AffiliationCorrected <- gsr(ScopusReducedDatasetTIAUcorExtended$Affiliation, CorrectionAffiliation$Origin, CorrectionAffiliation$Corrected)

# summarise the corrected information
# list may not be the same length as the original one 
ScopusReducedDatasetTIAUC1Scor <- ScopusReducedDatasetTIAUcorExtended %>%
  select(PY,TI,AU,DES,IDS,DI,SO,DT,Coder,AffiliationCorrected) %>%
  dplyr::group_by(PY,TI,AU,DES,IDS,DI,SO,DT,Coder) %>%
  dplyr::summarise(C1S = paste(AffiliationCorrected[!is.na(AffiliationCorrected)], collapse = ";"))

ScopusReducedDatasetTIAUC1Scor$C1S[ScopusReducedDatasetTIAUC1Scor$C1S==""] <- NA

#############################
#####  keyword merging  ##### 
#############################

# correcting identical records with different keywords lists (either missing or complementary); removing duplicates
# list may not be the same length as the original one

ScopusReducedDatasetTIAUC1SIDScor <- ScopusReducedDatasetTIAUC1Scor %>%
  mutate(IDS = strsplit(as.character(IDS), ";", ))%>%
  unnest(IDS)%>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  dplyr::group_by(PY,TI,AU,DES,DI,SO,DT,C1S,Coder)%>%
  dplyr::summarise(IDS = paste(IDS[!is.na(IDS)], collapse= ";"))%>%
  ungroup()

ScopusReducedDatasetTIAUC1SIDScor$IDS[ScopusReducedDatasetTIAUC1SIDScor$IDS==""] <- NA

#############################
##### source correction ##### 
#############################

# Correction to the Journal can be applied at this stage. This can be done in Notepad++, Excel etc.
# The Source generated in Scopus is use to correct the one in WoS
# The \& symbol present in the source from Web of science must be corrected as well
CorrectionSource <- read.csv("CorrectionLists/SourceCorrected.txt", sep = "\t", header = TRUE)
CorrectionSource <- as.data.frame(CorrectionSource)

ScopusReducedDatasetTIAUC1SIDScor$SOCorrected <- gsr(as.character(ScopusReducedDatasetTIAUC1SIDScor$SO), as.character(CorrectionSource$raw.x), as.character(CorrectionSource$raw.y))
ScopusReducedDatasetTIAUC1SIDScor$SOCorrected  <- as.character(gsub(": ","-",ScopusReducedDatasetTIAUC1SIDScor$SOCorrected))
ScopusReducedDatasetTIAUC1SIDSSOCor <- ScopusReducedDatasetTIAUC1SIDScor %>%
  select(PY,TI,AU,DES,IDS,C1S,DI,SOCorrected,DT,Coder)

# rename SOCorrected column
names(ScopusReducedDatasetTIAUC1SIDSSOCor)[names(ScopusReducedDatasetTIAUC1SIDSSOCor)=="SOCorrected"] <- "SO"

####################################
##### document type correction ##### 
####################################

# load correction list
DocumentCorrected <- read.csv("CorrectionLists/DocumentCorrection.txt", sep = "\t", header = TRUE)
DocumentCorrected <- as.data.frame(DocumentCorrected)
ScopusReducedDatasetTIAUC1SIDSSOCor$DTCorrected <- gsr(as.character(ScopusReducedDatasetTIAUC1SIDSSOCor$DT),as.character(DocumentCorrected$name),as.character(DocumentCorrected$Name.Corrected))

# summarise the corrected information
ScopusReducedDatasetTIAUC1SIDSSODTcor <- ScopusReducedDatasetTIAUC1SIDSSOCor %>%
  select(PY,TI,AU,DES,IDS,DI,SO,C1S,Coder,DTCorrected)

# rename DTCorrected column
names(ScopusReducedDatasetTIAUC1SIDSSODTcor)[names(ScopusReducedDatasetTIAUC1SIDSSODTcor)=="DTCorrected"] <- "DT"

rm(ScopusReducedDatasetTIAUC1SIDSSOCor)
rm(ScopusReducedDatasetTIAUC1SIDScor)
rm(ScopusReducedDatasetTIAUC1Scor)
rm(ScopusReducedDatasetTIAUcorExtended)
rm(ScopusReducedDatasetTIAUcor)
rm(ScopusReducedDatasetTIcorExtended)
rm(ScopusReducedDatasetTIcor)


########################################
#####       Web of Science         #####
########################################

#####        For the results from Web of Science        #####
# Select column label $PY, $TI,  $SO, $AU, $DE, $ID, $C1, $DI, $SO, $DT
WebOfScienceReducedDataset <- WebofScience %>%
  select(PY,AU,TI,DE,ID,C1,DI,SO,DT)
# Label each row with the name of the database from where it came from 
WebOfScienceReducedDataset$Coder <- "WebOfScience"

# removing the extra "." in the affiliation after the country
WebOfScienceReducedDataset$C1  <- as.character(gsub("\\.","\\",WebOfScienceReducedDataset$C1))
# Removing each ";" in double in the DE column
WebOfScienceReducedDataset$DE  <- as.character(gsub(";;",";",WebOfScienceReducedDataset$DE))

#####    Rename the Keywords lists
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="ID"] <- "IDW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="DE"] <- "DEW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="C1"] <- "C1W"

##############################
##### Authors correction ##### 
##############################

# Split Column "AU" with the separator ";" and place it in WebOfScienceReducedDatasetExtended
WebOfScienceReducedDatasetExtended <- WebOfScienceReducedDataset %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# export to correct the author names
#write.table(WebOfScienceReducedDatasetExtended, "Authors to correct WoS.txt", sep = "\t")

# read the corrected list of "Authors" and combine it to the original list
# Correction apply to Scopus and Web of Science was combined
WebOfScienceReducedDatasetExtended$AuthorsCor <- gsr(WebOfScienceReducedDatasetExtended$Authors,CorrectionAuthor$name, as.character(CorrectionAuthor$Name.corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
WebOfScienceReducedDatasetAUCor <- WebOfScienceReducedDatasetExtended %>%
  select(PY,TI,AuthorsCor,DEW,IDW,C1W,DI,SO,DT,Coder) %>%
  dplyr::group_by(PY,TI,DEW,IDW,C1W,DI,SO,DT,Coder) %>%
  dplyr::summarise(AU = paste(AuthorsCor, collapse = ";")) %>%
  ungroup()

DupeWebOfScience <- WebOfScienceReducedDatasetAUCor %>%
  find_duplicates(PY,TI)

# Apply some correction to previous list
WebOfScienceReducedDatasetAUCor$AU <- gsub(", JR"," JR",WebOfScienceReducedDatasetAUCor$AU)
WebOfScienceReducedDatasetAUCor$AU <- gsub(", II","",WebOfScienceReducedDatasetAUCor$AU)
WebOfScienceReducedDatasetAUCor$AU <- gsub(", III, ",", ",WebOfScienceReducedDatasetAUCor$AU)
WebOfScienceReducedDatasetAUCor$AU <- gsub(",",";", WebOfScienceReducedDatasetAUCor$AU)

#############################
##### source correction ##### 
#############################

# Correction to the Journal can be applied at this stage. This can be done in Notepad++, Excel etc.
# The Source generated in Scopus is use to correct the one in WoS
# The \& symbol present in the source from Web of science must be corrected as well
CorrectionSource <- read.csv("CorrectionLists/SourceCorrected.txt", sep = "\t", header = TRUE)
CorrectionSource <- as.data.frame(CorrectionSource)

WebOfScienceReducedDatasetAUCor$SOCorrected <- gsr(as.character(WebOfScienceReducedDatasetAUCor$SO), as.character(CorrectionSource$raw.x), as.character(CorrectionSource$raw.y))
WebOfScienceReducedDatasetAUSOCor <- WebOfScienceReducedDatasetAUCor %>%
  select(PY,AU,DEW,IDW,C1W,DI,SOCorrected,DT,TI,Coder)

# rename SOCorrected column
names(WebOfScienceReducedDatasetAUSOCor)[names(WebOfScienceReducedDatasetAUSOCor)=="SOCorrected"] <- "SO"

####################################
##### document type correction ##### 
####################################

# load correction list
WebOfScienceReducedDatasetAUSOCor$DTCorrected <- gsr(as.character(WebOfScienceReducedDatasetAUSOCor$DT),as.character(DocumentCorrected$name),as.character(DocumentCorrected$Name.Corrected))

# summarise the corrected information
WebOfScienceReducedDatasetAUSODTcor <- WebOfScienceReducedDatasetAUSOCor %>%
  select(PY,TI,AU,DEW,IDW,C1W,DI,SO,Coder,DTCorrected)

# rename DTCorrected column
names(WebOfScienceReducedDatasetAUSODTcor)[names(WebOfScienceReducedDatasetAUSODTcor)=="DTCorrected"] <- "DT"


rm(WebOfScienceReducedDatasetAUSOCor)
rm(WebOfScienceReducedDatasetAUCor)
rm(WebOfScienceReducedDatasetExtended)

###############################
#####       IFSMS         #####
###############################

# select the column of interest
IFSMS <- IFSMS %>%
  select(Authors, Title, Year, AKeywords, Source.title, DOI, Document.Type, Link, Coder)

# remove duplicate within each individual IFSMS reports
IFSMS <- IFSMS %>%
  distinct()

####################################
##### document type correction ##### 
####################################

# load correction list
DocumentCorrectedIFSMS <- read.csv("CorrectionLists/DocumentCorrectionIFSMS.txt", sep = "\t", header = TRUE)
DocumentCorrectedIFSMS <- as.data.frame(DocumentCorrectedIFSMS)
IFSMS$DTCorrected <- gsr(as.character(IFSMS$Document.Type),as.character(DocumentCorrectedIFSMS$name),as.character(DocumentCorrectedIFSMS$Name.Corrected))

# summarise the corrected information
IFSMS <- IFSMS %>%
  select(Authors, Title, Year, AKeywords, Source.title, DOI, DTCorrected, Link, Coder)

# rename DTCorrected column
names(IFSMS)[names(IFSMS)=="DTCorrected"] <- "Document.Type"

############################
##### Authors correction ### 
############################

# Upper case "Authors" in "IFSMS"
IFSMS$Authors <- toupper(IFSMS$Authors)

# in column Authors, replace "," by ";" and remove all the "."
IFSMS$Authors <- gsub(",",";",IFSMS$Authors)
IFSMS$Authors <- gsub("\\.","", IFSMS$Authors)

# Split Column "Authors" with the separator ";" and place it in IFSMSDatasetExtended
IFSMSDatasetExtended <- IFSMS %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# read the corrected list of "Authors" and combine it to the original list
# Correction apply to Scopus and Web of Science was combined
IFSMSDatasetExtended$AuthorsCor <- gsr(IFSMSDatasetExtended$Authors,CorrectionAuthor$name, as.character(CorrectionAuthor$Name.corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
IFSMSDatasetExtendedAUCor <- IFSMSDatasetExtended %>%
  select(Year,Title, AuthorsCor,AKeywords,Source.title,DOI,Document.Type,Link,Coder) %>%
  dplyr::group_by(Year,Title,AKeywords,Source.title,DOI,Document.Type,Link,Coder) %>%
  dplyr::summarise(Authors = paste(AuthorsCor, collapse = ";")) %>%
  ungroup()

# Apply some correction to previous list
IFSMSDatasetExtendedAUCor$Authors <- gsub(", JR"," JR",IFSMSDatasetExtendedAUCor$Authors)
IFSMSDatasetExtendedAUCor$Authors <- gsub(", II","",IFSMSDatasetExtendedAUCor$Authors)
IFSMSDatasetExtendedAUCor$Authors <- gsub(", III, ",", ",IFSMSDatasetExtendedAUCor$Authors)
IFSMSDatasetExtendedAUCor$Authors <- gsub(",",";", IFSMSDatasetExtendedAUCor$Authors)

#################################################################
#####        To combine Scopus and WOS datasets into one    #####
#################################################################

# Combining the two dataset
DatabaseOutputTemp <- bind_rows(ScopusReducedDatasetTIAUC1SIDSSODTcor, WebOfScienceReducedDatasetAUSODTcor)
DatabaseOutputTemp <- as.data.frame(DatabaseOutputTemp)


# To summarise by removing duplicate, grouping by Title and year
# doing so may lead to what appears to be duplicates from within the same database, e.g. two records with the same title and year in Scopus
# such records can be found using the "Coder" column but no further actions are taken on these records
# it should that if "duplicates" are found, ";;" may appear in keywords lists.

CombinedDataset <- DatabaseOutputTemp %>%
  dplyr::group_by(TI, PY) %>% dplyr::summarise(AU = paste(unique(AU), collapse=";"), 
                                DOI = rem_dup_word(paste(DI[!is.na(DI)], collapse=", ")),
                                DEW = paste(DEW[!is.na(DEW)], collapse=";"), 
                                IDW = paste(IDW[!is.na(IDW)], collapse=";"), 
                                DES = paste(DES[!is.na(DES)], collapse=";"),
                                IDS = paste(IDS[!is.na(IDS)], collapse=";"),
                                C1W = paste(C1W[!is.na(C1W)], collapse=","),
                                C1S = paste(C1S[!is.na(C1S)], collapse=","),
                                Coder = paste(Coder[!is.na(Coder)], collapse=","),
                                DT = paste(unique(DT), collapse=";"),
                                SO = paste(unique(SO), collapse=";")
                                ) %>% ungroup()

####################################
##### document type correction ##### 
####################################
# Some references duplicated in WOS and Scopus have different DT.
# The choice made here is to keep the DT from scopus and remove the one from WOS
# split column DT in to DTScopus and DTWoS based on ";"
CombinedDatasetExtended <- CombinedDataset %>%
  separate(DT, c("DTScopus", "DTWoS"), ";")

# summarise the corrected information
CombinedDataset <- CombinedDatasetExtended %>%
  select(PY,TI,AU,DEW,IDW,DES,IDS,DTScopus,C1W,C1S,SO,Coder,DOI)

# rename DTCorrected column
names(CombinedDataset)[names(CombinedDataset)=="DTScopus"] <- "DT"


rm(CombinedDatasetExtended)


#######################################################################
#####                       EXCLUSION LIST                        #####
#######################################################################
# This section is to narowing down the CombinedDataset
# The exclusion list is generated by filtering CombinedDataset based on a Journal, Keywords and Title

#####_________Correction on Authors' keywords_________##########
# The Authors' keywords (i.e. DEW and DES) between the two lists should be the same, however it is not the case and some correction will be needed.
# The user can decide to use the original list of keywords given by the databases instead, adding "S" or "W" to "DE"
# The merged list (i.e. DEW +DES) is more exhaustive than the individual one as for some records as the keywords do not appear in both lists
# It is best to remove the white space present with the ";"
CombinedDataset <-  transform(CombinedDataset, DE=paste(DEW,DES, sep="; "))

# remove leading and trailing "; " in the combined list 
CombinedDataset$DE <- gsub('^; |; $', '', CombinedDataset$DE)

# remove extra columns (DEW and DES), 

CombinedDatasetReduced <- CombinedDataset %>%
  select(TI,PY,AU,DOI,DE,IDW,IDS,C1W,C1S,Coder,DT,SO)

# replace all empty in DE with NA
 CombinedDatasetReduced$DE[CombinedDatasetReduced$DE==""]<-NA

# split keywords (DE) to remove duplicates

CombinedDatasetNarrow <- CombinedDatasetReduced %>%
  mutate(DE = strsplit(as.character(DE), "; ", )) %>%
  unnest(DE) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  dplyr::group_by(TI,PY,AU,DOI,IDW,IDS,C1W,C1S,Coder,DT,SO) %>%
  dplyr::summarise(DE = sort(paste(DE[!is.na(DE)], collapse= ";")))

# remove extra dataframe
rm(CombinedDatasetReduced)

# rename keyword column to "AK" (From Sobreira's code)
names(CombinedDatasetNarrow) <- sub("DE","AK", names(CombinedDatasetNarrow))

CombinedDatasetNarrow <- as.data.frame(CombinedDatasetNarrow)

#######################################################################
#####          Creating an exclusion list based on Journal        #####
#######################################################################

# Journal list from scopus is available at https://www.scopus.com/sources.uri?zone=TopNavBar&origin=searchbasic
# read the export *.csv forensic Journal list from Scopus, separation "\t", and place it in data.frame "ScopusForensicJournalList"
ScopusForensicJournalList <- read.csv("Lists/Scopus_Journal_list_Forensic_and_Justice_21-04-20.txt", sep="\t", header=TRUE)

# Creating a list of keywords for exclusion
# The keywords should be chosen in relation to the research areas to be excluded

removejournal.list <- read.delim("Lists/Scopus_Discipline_Exclusion.txt", sep="\t", header=TRUE)

removejournal.list <- removejournal.list %>%
  summarise(Discipline = sort(paste(Discipline, collapse= "|")))

# Remove from "ScopusForensicJournalList" every Journal with keywords from "removejournal.list" and place it in a new list "InclusionJournals"
InclusionJournals <- ScopusForensicJournalList %>%
  filter(!grepl(removejournal.list, ScopusForensicJournalList$Source.title))
  

InclusionJournalsReduced <- InclusionJournals %>%
  select(Source.title)

# Upper case "Source.title" in "InclusionJournals"
InclusionJournalsReduced$Source.title <- toupper(InclusionJournals$Source.title)

# Creating a list of documents included, based on Source.title
InclusionDataSet <-subset(CombinedDatasetNarrow,SO %in% InclusionJournalsReduced$Source.title)

# Creating a list of documents excluded, based on Source.title
ExclusionDataSet <- setdiff(CombinedDatasetNarrow,InclusionDataSet)


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
FalsePositiveList <- InclusionDataSet[-grep(Keyword.list, InclusionDataSet$AK), ]
# False positive (FP) documents - documents in InclusionDataSet which can be not relevant (based on Keyword.list)
FPdocument  <- as.numeric(nrow(FalsePositiveList))

# True Positive (TP) documents - documents in InclusionDataSet which are relevant (based on Keyword.list)
TruePositiveList <- InclusionDataSet[grep(Keyword.list, InclusionDataSet$AK), ]
TPdocument  <- as.numeric(nrow(TruePositiveList))

# Total number of documents in the inclusion list
Includeddocument <- as.numeric(nrow(InclusionDataSet))

# % of FP in the exlusion list
FPdocument/Includeddocument*100

# Verification - Is FPdocument + TPdocument = Includeddocument ?
test1 <- FPdocument + TPdocument
ifelse(Includeddocument == test1, "Correct", "Not correct")
V1 <- as.data.frame(ifelse(Includeddocument == test1, "Correct", "Not correct"))


# 2) Exclusion List
# Creating a new list of document which have, at least, one of the keywords from Keyword.list
FalseNegativeList <- ExclusionDataSet[grep(Keyword.list, ExclusionDataSet$AK), ]

# False negative (FN) documents - documents in ExclusionDataSet which can be relevant (based on Keyword.list)
FNdocument  <- as.numeric(nrow(FalseNegativeList))

# True Negative documents - documents in ExclusionDataSet which are not relevant (based on Keyword.list)
TrueNegativeList <- ExclusionDataSet[-grep(Keyword.list, ExclusionDataSet$AK), ]
TNdocument  <- as.numeric(nrow(TrueNegativeList))

# Total number of documnet in the excluded list
Excludeddocument <- as.numeric(nrow(ExclusionDataSet))

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
#####     Second cleaning of the dataset based on AKeywords      #####
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
                               "OPTICAL FIBRE", "OPTICAL FIBER", "MEMBRANE"), collapse = '|')
removeKeywordslist <- as.data.frame(removeKeywords.list)

# Creating a new list of document which don't have any of the keywords from removeKeywords.list
InclusionDataSetBis <- CombinedDataset2 %>%
  filter(!grepl(removeKeywords.list, CombinedDataset2$AK))

# Eclusion List bis
ExclusionDataSetBis <- setdiff(CombinedDataset2,InclusionDataSetBis)

# testing the new inclusion list with the previous"Keyword.list"
FalsePositiveListBis <- InclusionDataSetBis[-grep(Keyword.list, InclusionDataSetBis$AK), ]

# New calcul of False positive (FP) (based on the previous Ketword.list)
FPdocumentBis  <- as.numeric(nrow(FalsePositiveListBis))

# New calcul of True positive (TP) (based on the previous Ketword.list)
TruePositiveListBis <- InclusionDataSetBis[grep(Keyword.list, InclusionDataSetBis$AK), ]
TPdocumentBis  <- as.numeric(nrow(TruePositiveListBis))

# Total number of document in the inclusion list
IncludeddocumentBis <- as.numeric(nrow(InclusionDataSetBis))

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
FNdocumentBis  <- as.numeric(nrow(FalseNegativeListBis))

# True Negative documents - documents in ExclusionDataSet which are not relevant (based on Keyword.list)
TrueNegativeListBis <- ExclusionDataSet[-grep(Keyword.list, ExclusionDataSet$TI), ]
TNdocumentBis  <- as.numeric(nrow(TrueNegativeListBis))

# Total number of document in the excluded list
Excludeddocumentbis <- as.numeric(nrow(ExclusionDataSet))

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
MergerOriginalData <- rbind(TruePositiveListBis, FinalFalseNegativelist) %>% distinct()

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

write.table(WebOfScienceReducedDatasetAUSODTcor, file = paste0(Results.dir,"Result_WebOfScience_CorrectedDataset.txt"), sep = "\t", row.names = F)
write.table(ScopusReducedDatasetTIAUC1SIDSSODTcor, file = paste0(Results.dir,"Result_Scopus_CorrectedDataset.txt"), sep = "\t", row.names = F)
write.table(MergerOriginalData, file = paste0(Results.dir,"Result_Merger_Dataset.txt"), sep = "\t", row.names = F)
write.table(CombinedDataset, file = paste0(Results.dir,"Result_MergerExclusion_Dataset.txt"), sep = "\t", row.names = F)
write.table(IFSMS, file = paste0(Results.dir,"Result_IFSMS_Dataset.txt"), sep = "\t", row.names = F)
