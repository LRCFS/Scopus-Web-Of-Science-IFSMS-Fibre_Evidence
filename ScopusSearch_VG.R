# To clean the Global environment
rm(list=ls()) 

#######################################################################
#####                      File requirement                       #####
#######################################################################
# The files to be imported is generated from Scopus.
# The columns will need to contain:
#   Year; Title; Source.title; Authors; AuthorID; Author.Keywords; Index.Keywords; EID

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#######################################################################
#####                           Function                          #####
#######################################################################

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

#######################################################################
#####                         Data loading                        #####
#######################################################################

# set working directory

# read the export *.csv document from Scopus, separation ",", and place it in data.frame "ScopusOriginal"
ScopusOriginalData <- read.csv("Scopus_FibreAndForensic_18-05-2020.txt", sep="\t", header=TRUE)

# rename some of the columns to remove special characters or encoding
colnames(ScopusOriginalData)[colnames(ScopusOriginalData)=="Author.s..ID"] <- "AuthorID"

# Select column label $Year, $Title,  $Source.title, $Author.Keywords, $Index.Keyword
ScopusReducedDataSet <- ScopusOriginalData %>%
  select(Year,Title,Source.title,Authors,AuthorID,Author.Keywords,Index.Keywords, Cited.by, Access.Type, Document.Type)


#######################################################################
#####          Select one of the following three options          #####
#######################################################################

#   Author Keywords only
 #names(ScopusReducedDataSet) <- sub("Author.Keywords","AIKeywords", names(ScopusReducedDataSet))

#   Index Keywords only
 #names(ScopusReducedDataSet) <- sub("Index.Keywords","AIKeywords", names(ScopusReducedDataSet))

#   Index and Author Keywords
# Combine Columns Author.Keywords and Index.Keywords and place in Column name "AIKeywords" and remove original columns
 ScopusReducedDataSet <- ScopusReducedDataSet %>%
  unite("AIKeywords", Author.Keywords, Index.Keywords,sep = ";", remove = TRUE)


#######################################################################
#####       Creating an exclusion list based on Source.title      #####
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

# Creating a list of documents included, based on Source.title
InclusionDataSet <-subset(ScopusReducedDataSet,Source.title %in% InclusionJournalsReduced$Source.title)

# Creating a list of documents excluded, based on Source.title
ExclusionDataSet <- setdiff(ScopusReducedDataSet,InclusionDataSet)


#######################################################################
#####           Testing the exclusion and inclusion list          #####
#######################################################################
    #Definition
# True Positive : documents declared relevant to the research subject, and they are relevant
# False positive : documents declared relevant to the research subject, while they are not relevant
# True negative : documents declared not relevant to the research subject, and they are not relevant
# False negative : documents declared not relevant to the research subject, while they are relevant

  # 1) Inclusion List
# Creating a list of keywords relevant to the field of research
Keyword.list <- paste(c("FIBRE", 
                        "FIBER",
                        "CLOTHING", 
                        "TEXTILE"), collapse = '|')

# Creating a new list of document which don't have any of the keywords from Keyword.list
InclusionDataSet$AIKeywords <- toupper(InclusionDataSet$AIKeywords)
FalsePositiveList <- InclusionDataSet[-grep(Keyword.list, InclusionDataSet$AIKeywords), ]

# False positive (FP) documents - documents in InclusionDataSet which can be not relevant (based on Keyword.list)
FPdocument  <- as.numeric(count(FalsePositiveList))

# True Positive (TP) documents - documents in InclusionDataSet which are relevant (based on Keyword.list)
TruePositiveList <- InclusionDataSet[grep(Keyword.list, InclusionDataSet$AIKeywords), ]
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
ExclusionDataSet$AIKeywords <- toupper(ExclusionDataSet$AIKeywords)
FalseNegativeList <- ExclusionDataSet[grep(Keyword.list, ExclusionDataSet$AIKeywords), ]

# False negative (FN) documents - documents in ExclusionDataSet which can be relevant (based on Keyword.list)
FNdocument  <- as.numeric(count(FalseNegativeList))

# True Negative documents - documents in ExclusionDataSet which are not relevant (based on Keyword.list)
TrueNegativeList <- ExclusionDataSet[-grep(Keyword.list, ExclusionDataSet$AIKeywords), ]
TNdocument  <- as.numeric(count(TrueNegativeList))

# Total number of documnet in the excluded list
Excludeddocument <- as.numeric(count(ExclusionDataSet))

# Number of FN in the exlusion list
FNdocument/Excludeddocument*100

# Verification - Is FNdocument + TNdocument = Excludeddocument ?
test2 <- FNdocument + TNdocument
ifelse(Excludeddocument == test2, "Correct", "Not correct")
V2 <- as.data.frame(ifelse(Excludeddocument == test2, "Correct", "Not correct"))

#######################################################################
#####     Second cleansing of the dataset based on AIKeywords     #####
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
                              "OPTIC"), collapse = '|')
removeKeywordslist <- as.data.frame(removeKeywords.list)

# Creating a new list of document which don't have any of the keywords from removeKeywords.list
InclusionDataSetBis <- InclusionDataSet %>%
  filter(!grepl(removeKeywords.list, InclusionDataSet$AIKeywords))

# testing the new inclusion list with the previous"Keyword.list"
FalsePositiveListBis <- InclusionDataSetBis[-grep(Keyword.list, InclusionDataSetBis$AIKeywords), ]

# New calcul of False positive (FP) (based on the previous Ketword.list)
FPdocumentBis  <- as.numeric(count(FalsePositiveListBis))

# New calcul of True positive (TP) (based on the previous Ketword.list)
TruePositiveListBis <- InclusionDataSetBis[grep(Keyword.list, InclusionDataSetBis$AIKeywords), ]
TPdocumentBis  <- as.numeric(count(TruePositiveListBis))

# Total number of document in the inclusion list
IncludeddocumentBis <- as.numeric(count(InclusionDataSetBis))

# % of FP in the exlusion list
FPdocumentBis/IncludeddocumentBis*100

# Verification - Is FPdocumentBis + TPdocumentBis = IncludeddocumentBis ?
test3 <- FPdocumentBis + TPdocumentBis
ifelse(IncludeddocumentBis == test3, "Correct", "Not correct")
V3 <- as.data.frame(ifelse(IncludeddocumentBis == test3, "Correct", "Not correct"))
  
      # 2) Cleaning the exclusion List
# Remove from Exclusion list each document with particular keywords in it
addKeywords.list <- paste(c("TEXTILE",
                            "CLOTHING"), collapse = '|')
addKeywordslist <- as.data.frame(addKeywords.list)

# Creating a new list of document which don't have any of the keywords from addKeywords.list
ExclusionDataSetbis <- ExclusionDataSet %>%
  filter(!grepl(addKeywords.list, ExclusionDataSet$AIKeywords))

# testing the new exlusion list with the previous "Keyword.list"
FalseNegativeListBis <- ExclusionDataSetbis[grep(Keyword.list, ExclusionDataSetbis$AIKeywords), ]

# New calcul of False positive (FP) (based on the previous keyword.list)
FNdocumentBis  <- as.numeric(count(FalseNegativeListBis))

# New calcul of True negative (FP) (based on the previous keyword.list)
TrueNegativeListBis <- ExclusionDataSetbis[-grep(Keyword.list, ExclusionDataSetbis$AIKeywords), ]
TNdocumentBis  <- as.numeric(count(TrueNegativeListBis))

# Total number of documnet in the excluded list
Excludeddocumentbis <- as.numeric(count(ExclusionDataSetbis))

# Number of FN in the exlusion list
FNdocumentBis/Excludeddocumentbis*100

# Verification - Is FNdocumentBis + TNdocumentBis = Excludeddocumentbis ?
test4 <- FNdocumentBis + TNdocumentBis
ifelse(Excludeddocumentbis == test4, "Correct", "Not correct")
V4 <- as.data.frame(ifelse(Excludeddocumentbis == test4, "Correct", "Not correct"))

  # 3) Are FN and FP in relation to Forensic ?
# If the number of False positive and False negative is still high after, it is important that these to are in relation to the field of research
Forensic.list <- paste(c("FORENCIC"), collapse = '|')
Forensiclist <- as.data.frame(Forensic.list)

# Creating a new list of document which have any of the keywords from Forensic.list
FalsePositiveListTer <- FalsePositiveListBis %>%
  filter(!grepl(Forensic.list, FalsePositiveListBis$AIKeywords))
FalseNegativeListTer <- FalseNegativeListBis %>%
  filter(!grepl(Forensic.list, FalseNegativeListBis$AIKeywords))

FPdocumentTer  <- as.numeric(count(FalsePositiveListTer))
FNdocumentTer  <- as.numeric(count(FalseNegativeListTer))

# Verification - if FPdocumentTer = FPdocumentBis then all False positive document in the InclusionDataSetBis are in relation with to Forensic
ifelse(FPdocumentTer == FPdocumentBis, "Correct", "Not correct")
V5 <- as.data.frame(ifelse(FPdocumentTer == FPdocumentBis, "Correct", "Not correct"))
# Verification - if FNdocumentTer = FNdocumentBis then all False negative document in the ExclusionDataSetBis are in relation with to Forensic
ifelse(FNdocumentTer == FNdocumentBis, "Correct", "Not correct")
V6 <- as.data.frame(ifelse(FNdocumentTer == FNdocumentBis, "Correct", "Not correct"))

#######################################################################
#####                Creating the new dataset                     #####
#######################################################################

# creating a new dataset "ScopusCleanedData" with the True positive documents from InclusionDataSetBis and False negative documents from ExclusionDataSetBis
ScopusCleanedData <- rbind(TruePositiveListBis, FalseNegativeListBis)

#######################################################################
#####              Overview of all the verifications              #####
#######################################################################
Verifications <- V1
names(Verifications) <- c("Verifications")

Verifications[2,1] <- V2
Verifications[3,1] <- V3
Verifications[4,1] <- V4
Verifications[5,1] <- V5
Verifications[6,1] <- V6

rownames(Verifications)[rownames(Verifications)=="1"] <- "FPdocument + TPdocument = Includeddocument ?"
rownames(Verifications)[rownames(Verifications)=="2"] <- "FNdocument + TNdocument = Excludeddocument ?"
rownames(Verifications)[rownames(Verifications)=="3"] <- "FPdocumentBis + TPdocumentBis = IncludeddocumentBis ?"
rownames(Verifications)[rownames(Verifications)=="4"] <- "FNdocumentBis + TNdocumentBis = Excludeddocumentbis ?"
rownames(Verifications)[rownames(Verifications)=="5"] <- "FPdocumentTer = FPdocumentBis ?"
rownames(Verifications)[rownames(Verifications)=="6"] <- "FNdocumentTer = FNdocumentBis ?"

show(Verifications)

#######################################################################
#####                       Data cleansing                        #####
#######################################################################

#Split Column "AIKeywords" in row by the separator ";", remove leading white space to generate list
ScopusKeywordList <- ScopusCleanedData %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIKeywords" in "ScopusKeywordList" and save in dataframe
# Extract list of "AIkeywords" and remove duplicate
ScopusKeywordList$AIKeywords <- toupper(ScopusKeywordList$AIKeywords)
KeywordList <- ScopusKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

# Most cited keywords before correction - Exact a list of keywords to apply corrections
KeywordListCount <- aggregate(KeywordList$AIKeywords, by=list(Freq=KeywordList$AIKeywords), FUN=length)
names(KeywordListCount) <- c("Keywords","Count")
#write.csv(KeywordListCount,"Keywords to correct.csv")

# Correction of the keywords
KeywordsCorrected <- read.csv("Corrected Keywords List.csv", sep=",", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)
ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$Keywords.corrected))

# Number of different keywords after correction
KeywordList2 <- ScopusKeywordList %>%
  select(KeywordsCorrected)
Keyword2 <- KeywordList2 %>%
  distinct()

#######################################################################
#####                    Data analysis - Keywords                 #####
#######################################################################

#Count to number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(ScopusReducedDataSet$Year));PublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
ScopusKeywordListTemp1 <- ScopusKeywordList  %>%
  select(Year,Title,Source.title,KeywordsCorrected) %>%
  distinct()
ScopusKeywordListTemp2 <-ScopusKeywordListTemp1[complete.cases(ScopusKeywordListTemp1), ]
sum(is.na(ScopusKeywordListTemp2$KeywordsCorrected))

ScopusKeywordYearCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Year=ScopusKeywordListTemp2$Year, Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)
ScopusKeywordTotalCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)

### ScopusKeywordListTemp5 <- aggregate(ScopusKeywordListTemp2, by=list(ScopusKeywordListTemp2$Year), FUN=length)

# narrowing range for plot
ScopusKeywordNarrowRangeGraph <- subset(ScopusKeywordTotalCount,x>15)

SubsetKeywordNarrowRangeGraph <-subset(ScopusKeywordYearCount,Rtitle %in% ScopusKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
SubsetKeywordNarrowRangeGraph$x <- as.numeric(SubsetKeywordNarrowRangeGraph$x)


#######################################################################
#####                       Keyword Graph                         #####
#######################################################################

# Create a new variable from incidence
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,5,10,15,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10","11-15","15-20"))

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,5,10,15,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10","11-15","15-20")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# ScopusKeywordList$WYear <- gsr(ScopusKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1[, 1], list(GraphTemp1$KeywordsCorrected), min)

GraphTemp1$graphorder <- as.numeric(gsr(GraphTemp1$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p <- ggplot(GraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1975,1985,1995,2005,2015))+
  scale_fill_manual(values=c("#d53e4f","#f46d43","#fdae61","#fee08b","#d5ee52","#77c86c","#66afc6","#ddf1da"),na.value = "grey90")+
  #coord_fixed()+
  theme_bw(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p)
#ggsave("AIKeywords_Trend.png", p, width = 9, height = 8, units = "in", dpi=150)

#######################################################################
#####                 Data analysis - Authors                     #####
#######################################################################

# This section is looks at Authors to generate a table:
#                                   the total number of authors and publications per year,
#                                   the number of publications per author,
#                                   the first year an author published,
#                                   the number of new author  


# Creating a first list of authors
AuthorList <- ScopusCleanedData %>%
  select(Year,Title,AuthorID,Authors)

# Apply some correction to the list
AuthorList$Authors <- gsub(", Jr"," Jr",AuthorList$Authors)
AuthorList$Authors <- gsub(", II","",AuthorList$Authors)
AuthorList$Authors <- gsub(", III, ",", ",AuthorList$Authors)
AuthorList$Authors <- gsub(",",";",AuthorList$Authors)

#Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListExtended <- AuthorList %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)
colnames(AuthorListExtended)[colnames(AuthorListExtended)=="AuthorID"] <- "AuthorIDList"


# Creating a second list of authors ID
AuthorIDList <- ScopusCleanedData %>%
  select(Year,Title,Authors,AuthorID)

#Split Column "AuthorID" in row by the separator ";", remove leading white space to generate list
AuthorIDListExtended <- AuthorIDList %>% 
  mutate(AuthorID = strsplit(as.character(AuthorID), ";"))%>% 
  unnest(AuthorID) %>%
  mutate_if(is.character, str_trim)

#####______________Apply corrections______________##########

# Combine the two list AuthorListExtendedTest and AuthorIDListExtended to associate ID number to each authors
ToCorrect <- data.frame(cbind(Id=AuthorIDListExtended$AuthorID, name= AuthorListExtended$Authors))
# export ToCorrect to apply correction
#write.table(ToCorrect, file = "Authors-ID to correct.csv", quote = F, sep = ",", row.names = F)

    # Apply correction to author's name
# It can happen that an author's name is written in two different ways.
# This need to be homogenized (a methodology to correct authors name effectively is provide in - still need to be written)
AuthorCorrected <- read.csv("Authors Name Corrected.txt", sep="\t", header=TRUE)
AuthorListExtended$AuthorsCor <- gsr(AuthorListExtended$Authors,AuthorCorrected$name,as.character(AuthorCorrected$Name.corrected))

    # Apply correction of author's ID
# It can happen that an author has multiple ID. This must be corrected so an author has a unique ID
# It is also possible that two different authors has different ID but the same name
# This need to be homogenized (a methodology to correct authors ID effectively is provide in - still need to be written)
AuthorIDCorrected <- read.csv("Authors ID Corrected.txt", sep="\t", header=TRUE)
AuthorIDListExtended$AuthorsIDCor <- gsr(AuthorIDListExtended$AuthorID,AuthorIDCorrected$Id,as.character(AuthorIDCorrected$Id.Corrected))

# Combine the two list AuthorListExtendedTest and AuthorIDListExtended to associate ID number to each authors after correction
FinalAuthorList <- data.frame(cbind(Id=AuthorIDListExtended$AuthorsIDCor, name= AuthorListExtended$AuthorsCor)) %>%
  na.omit(FinalAuthorList, cols="Id")

# Export the list for relabelling on gephi
#write.table(FinalAuthorList, file = "Scopus_RelabellingList.csv", quote = F, sep = ",", row.names = F)

#####___________________________________________##########

AuthorCountPaper <- aggregate(AuthorListExtended$AuthorsCor, list(AuthorListExtended$AuthorsCor), FUN=length)
names(AuthorCountPaper) <- c("Author","Frequency")
AuthorIDCountPaper <- aggregate(AuthorIDListExtended$AuthorsIDCor, list(AuthorIDListExtended$AuthorsIDCor), FUN=length)
names(AuthorIDCountPaper) <- c("Author","Frequency")

# Number of AuthorsCor per year
NumberAuthorYear <- aggregate(AuthorListExtended$AuthorsCor,list(AuthorListExtended$Year), FUN=length)
names(NumberAuthorYear) <- c("Year","Author")
NumberAuthorIDYear <- aggregate(AuthorIDListExtended$AuthorsIDCor,list(AuthorIDListExtended$Year), FUN=length)
names(NumberAuthorIDYear) <- c("Year","Author")

# List of "Author" with one publication only
AuthorCountSinglePaper <- subset(AuthorCountPaper,Frequency<2)
AuthorCountSinglePaperReduced <-subset(AuthorListExtended,AuthorsCor %in% AuthorCountSinglePaper$Author)
AuthorIDCountSinglePaper <- subset(AuthorIDCountPaper,Frequency<2)
AuthorIDCountSinglePaperReduced <-subset(AuthorIDListExtended,AuthorsIDCor %in% AuthorIDCountSinglePaper$Author) 

# List of "Author" with one publication only and their publication "Year"
YearNewAuthorSinglePaper <- aggregate(AuthorCountSinglePaperReduced$AuthorsCor,list(AuthorCountSinglePaperReduced$Year), FUN=length) 
names(YearNewAuthorSinglePaper) <- c("Year","Single Author")
YearNewAuthorIDSinglePaper <- aggregate(AuthorIDCountSinglePaperReduced$AuthorsIDCor,list(AuthorIDCountSinglePaperReduced$Year), FUN=length)
names(YearNewAuthorIDSinglePaper) <- c("Year","Single Author")

# List of Authors with multiple publications only
AuthorCountMultiplePaper <- subset(AuthorCountPaper,Frequency>1)
AuthorCountMultiplePaperReduced <- subset(AuthorListExtended,AuthorsCor %in% AuthorCountMultiplePaper$Author)
AuthorIDCountMultiplePaper <- subset(AuthorIDCountPaper,Frequency>1)
AuthorIDCountMultiplePaperReduced <- subset(AuthorIDListExtended,AuthorsIDCor %in% AuthorIDCountMultiplePaper$Author) #if correction Authors= Authors

# List of "Authors" with multiple output and their first "Year"
AuthorFirstAppearanceMultipleEntry<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$AuthorsCor), min)
names(AuthorFirstAppearanceMultipleEntry) <- c("Author","Year")
AuthorIDFirstAppearanceMultipleEntry<- aggregate(AuthorIDCountMultiplePaperReduced$Year, list(AuthorIDCountMultiplePaperReduced$AuthorsIDCor), min)
names(AuthorIDFirstAppearanceMultipleEntry) <- c("Author","Year")

# List of "Authors" with multiple output and their last "Year"
AuthorMultipleOutputLastYear<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$AuthorsCor), max)
names(AuthorMultipleOutputLastYear) <- c("Author","Year")
AuthorIDMultipleOutputLastYear<- aggregate(AuthorIDCountMultiplePaperReduced$Year, list(AuthorIDCountMultiplePaperReduced$AuthorsIDCor), max)
names(AuthorIDMultipleOutputLastYear) <- c("Author","Year")

#####__________________Gephi Plots - Authors_________________#####

 #generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
#ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(Year,Title) %>%
  #summarise(AuthorCorrected = paste(AuthorsCor, collapse = ";"))
#ListAuthor <- as.data.frame(ListAuthor)

ListAuthorID <- AuthorIDCountMultiplePaperReduced %>% group_by(Year,Title) %>%
  summarise(AuthorIDCorrected = paste(AuthorID, collapse = ";"))
ListAuthorID <- as.data.frame(ListAuthorID)

#GephiAuthor <- ListAuthor %>%
  #select(AuthorCorrected)
#names(GephiAuthor) <- c("Author")

GephiAuthorID <- ListAuthorID %>%
  select(AuthorIDCorrected)
names(GephiAuthorID) <- c("AuthorID")

#Export to Gephi plot - Year;Title;Authors
#write.table(GephiAuthor, file = "GephiAuthor.csv", quote = F, sep = "\t", row.names = F)
#write.table(GephiAuthorID, file = "GephiAuthorID.csv", quote = F, sep = "\t", row.names = F)

#Export to Gephi plot - Authors;Year
#write.table(AuthorMultipleOutputLastYear, file = "GephiListAuthorLastYear.csv", quote = F, sep = "\t", row.names = F)


#######################################################################
#####                     General information                     #####
#######################################################################
 
#Number of document (ND)
ND <- data.frame(nrow(ScopusCleanedData))
names(ND) <- c("Information")
GF <- ND
GF[1,1] <- ND
rownames(GF)[rownames(GF)=="1"] <- "Total number of document"

#Number of different sources (NDS)
NDS <- data.frame(table(ScopusCleanedData$Source.title, exclude = ""));NDS
GF[2,1] <-nrow(NDS)
rownames(GF)[rownames(GF)=="2"] <- "Number of different sources"

# Average number of citation per document (ACD)
ACD <- sum(ScopusCleanedData$Cited.by, na.rm=T) #number of citation in total
ACD <- ACD/ND # calcul of the mean
ACD <- format(ACD, digit=3)
GF[3,1] <- ACD 
rownames(GF)[rownames(GF)=="1"] <- "Average number of citations/documents"

# Number of  keywords (NK)
X <- ScopusCleanedData %>% 
  select(AIKeywords) %>% 
  mutate(AIKeywords = strsplit(as.character(AIKeywords), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)
NK <- data.frame(table(X, exclude = ""));NK
GF[4,1] <-nrow(NK)
rownames(GF)[rownames(GF)=="4"] <- "Keywords"

# Number of Open Access document (OAD)
ScopusCleanedData$Access.Type <- toupper(ScopusCleanedData$Access.Type)
OAD <- data.frame(length(grep("OPEN ACCESS", ScopusCleanedData$Access.Type)))
GF[5,1] <- OAD
rownames(GF)[rownames(GF)=="1"] <- "Open access documents"

# Pourcentage of open access document (POAD)
POAD <- (OAD/ND)*100
POAD <- format(POAD, digit=2)
GF[6,1] <- POAD
rownames(GF)[rownames(GF)=="1"] <- "% of Open access document"

# List of "Author" with one publication only
ACSP <- data.frame(sum(AuthorCountSinglePaper$Frequency))
options("digits"=1)
GF[8,1] <- ACSP
rownames(GF)[rownames(GF)=="8"] <- "Authors with one publication only"

# List of Authors with multiple publications only
ACMP <- data.frame(sum(AuthorCountMultiplePaper$Frequency))
options("digits"=1)
GF[9,1] <- ACMP
rownames(GF)[rownames(GF)=="1"] <- "Authors with multiple publication only"

#Total number of authors
NOA <- data.frame(ACSP+ACMP)
GF[7,1] <- NOA
rownames(GF)[rownames(GF)=="7"] <- "Total number of authors"

# Number of new author per year (NAY)
NAY <- AuthorFirstAppearanceMultipleEntry
NAY <- data.frame(table(NAY$Year));NAY
NAYmean <- mean(NAY$Freq)
NAYmean <- round(NAYmean,2)
NAYmedian <- median(NAY$Freq)
NAYmedian <- round(NAYmedian,2)
GF[10,1] <- NAYmean
rownames(GF)[rownames(GF)=="10"] <- "Average value of new authors per year"
GF[11,1] <- NAYmedian
rownames(GF)[rownames(GF)=="11"] <- "Median value of new authors per year"

# Document Types (DT)
DT <- data.frame(table(ScopusCleanedData$Document.Type, exclude = ""));DT
DT2 <- data.frame(DT[,-1])
rownames(DT2) <- DT$`Var1`
colnames(DT2)[colnames(DT2)=="DT....1."] <- "Information"
GeneralInformationFinal <- rbind(GF,DT2)

# To export data (replace the correct date)
#write.table(GeneralInformationFinal, file = "General Information_Scopus.csv", quote = F, sep = "\t", row.names = F)

#######################################################################
#####                     Document analysis                       #####
#######################################################################
library(extrafont)
#font_import()
#loadfonts(device = "win")

      # 1) document per year
# create a new data.frame of the number of document published each year
document <- ScopusCleanedData %>%
  select(Authors,Title, Year, Cited.by)

#Count to number of time the same year is repeated in the "document$Year" and save in a data.frame "Year" 
year <- data.frame(table(document$Year));year
year$Var1 <- as.numeric(as.character(year$Var1))

# To caculate the AAGR
#write.csv(year,"Scopus AAGR.csv")

# To add years with no document
DFfilled <- year %>%
  complete(Var1 = 1968:2020,
           fill = list(Freq = 0)) %>%
  as.data.frame()
DFfilled

# to plot the graph
a <-ggplot(data = DFfilled, aes(x =Var1, y = Freq)) +
  geom_line(color="black")+
  ggtitle(label = "Documents published in Scopus per year", 
          subtitle="(with 'Fibre' and 'Forensic' as keywords)")+
  ylim(0,45)+
  labs(x="Year", y="Number of documents")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, family ="Arial"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, family="Arial"),
        axis.text.y= element_text(size= 10, family ="Arial"),
        axis.text.x= element_text(size= 10, angle= 90, vjust= 0.6, family ="Arial"),
        axis.title.x = element_text(colour="black",size = 10, family ="Arial"),
        axis.title.y = element_text(colour="black", size= 10, family ="Arial"))
show(a)
#ggsave("Document_per_year_21-05-2020.png", a, width = 9, height = 6, units = "in", dpi=150)


      # 2) Most Cited document ###
# Create new dataframe with only the first 15 articles
MCD <- ScopusCleanedData[order(-ScopusCleanedData$Cited.by),]
Top15MostCiteddocument <- slice(MCD, 1:15)

#plot the graph
b <- ggplot(data = Top15MostCiteddocument, aes(x = reorder(Title,-Cited.by),y = Cited.by, fill = Cited.by)) +
  geom_bar(stat='identity', position = "stack") +
  coord_flip() +
  ggtitle(label = "Top 15 of Most cited document", 
          subtitle = "(with Fibre and Forensic as keywords in Scopus)")+
  labs(x="Authors",y="Number of Citations")+
  geom_text(aes(label=Cited.by), vjust = 0.5, hjust = 1.3, color = "black", size =3.5, family ="Arial") +
  scale_fill_gradient(low = "gray90", high = "gray64")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 100))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, family ="Arial"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, family="Arial"),
        axis.text.y= element_text(size= 10, family ="Arial"),
        axis.text.x= element_text(size= 10, angle= 90, vjust= 0.6, family ="Arial"),
        axis.title.x = element_text(colour="black",size = 10, family ="Arial"),
        axis.title.y = element_text(colour="black", size= 10, family ="Arial"))
show(b)
#ggsave("Most_cited_document_21-05-2020.png", b, width = 13, height = 7, units = "in", dpi=150)


      # 3) Most Cited journal
# Create new dataframe with only the first 15 articles before the cleansing
MCJ <- data.frame(table(ScopusCleanedData$Source.title, exclude = ""));MCJ
names(MCJ) <- c("Title","Frequence")
MCJ2 <- MCJ[order(-MCJ$Frequence),]
Top15MostCitedJournal <- slice(MCJ2, 1:15)

#plot the graph
c <- ggplot(data = Top15MostCitedJournal, aes(x = reorder(Title,-Frequence),y = Frequence, fill = Frequence)) +
  geom_bar(stat='identity', position = "stack" ) +
  coord_flip() +
  ggtitle("Top 15 of Most cited document", subtitle = "(with Fibre and Forensic as keywords in Scopus)")+
  labs(x="Authors",y="Number of Citations")+
  geom_text(aes(label=Frequence), vjust=0.5, hjust=1.3, color="black", size=3.5) +
  scale_fill_gradient(low = "gray90", high = "gray64")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, size = 12, family ="Arial"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, family="Arial"),
        axis.text.y= element_text(size= 10, family ="Arial"),
        axis.text.x= element_text(size= 10, angle= 90, vjust= 0.6, family ="Arial"),
        axis.title.x = element_text(colour="black",size = 10, family ="Arial"),
        axis.title.y = element_text(colour="black", size= 10, family ="Arial"))
show(c)
#ggsave("Most_cited_journal_21-05-2020.png", c, width = 13, height = 7, units = "in", dpi=150)
