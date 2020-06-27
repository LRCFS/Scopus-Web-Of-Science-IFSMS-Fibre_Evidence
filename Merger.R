#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported is generated from Scopus.
# The columns will need to contain:
#   Year; Title; Source.title; Authors; AuthorID; DE; DES; EID


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

Wos.path = 'Wos/'    #WEb of Science folder
Sco.path = 'Scopus/' #Scopus folder

#############################################################
#####       Data loading Individual citation list   #####
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
WebofScience<-convert2df(Wos,dbsource = "isi",format = "bibtex")


#############################################################
#####     Comparison and merging of the two datasets    #####
#############################################################

#####    Each dataset are imported separately using common column labels.
#####    To keep the original data, the columns are remained ending with "S" for Scopus and "W" for Web of Science
#####    To remove duplicate, some correction is needed on the Title

#####             For the results from Scopus           #####
# Select column label $PY, $TI,  $SO, $AU, $DE, $ID, $C1, $DI
ScopusReducedDataset <- Scopus %>%
  select(PY,AU,TI,DE,ID,C1,DI,url)
# transfor the year into numeric
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
  select(PY,AU,TI,DE,ID,C1,DI)
# removing the extra "." in the affiliation after the country
WebOfScienceReducedDataset$C1  <- as.character(gsub("\\.","\\",WebOfScienceReducedDataset$C1))

#####    Rename the Keywords lists
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="ID"] <- "IDW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="DE"] <- "DEW"
names(WebOfScienceReducedDataset)[names(WebOfScienceReducedDataset)=="C1"] <- "C1W"
# removing entries with no year
WebOfScienceReducedDataset <- WebOfScienceReducedDataset[!is.na(WebOfScienceReducedDataset$PY),]
WebOfScienceReducedDataset <- WebOfScienceReducedDataset %>%
  distinct()

########################################################
##### Search for match by title between the two datasets
##### to generate a list of titles with a partial match for external check

matches=partialMatch(WebOfScienceReducedDataset$TI,ScopusReducedDataset$TI)

aggregate(matches$pass, by=list(matches$pass), FUN=length)

PartialExport <- matches %>% filter(pass == "Partial")

# The PArtialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
#write.table(PartialExport, file = "PartialExport.txt", quote = F, sep="\t", row.names = F)

# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title generated in Web of Science will be used to correct the one in Scopus
# Note this does not mean all the title from Web of Science are correct !!!
# Missed duplicate or partial can be added as well. This is the case here with one extra correction added
# "Chang KH, Yew CH, Abdullah AFL. Study on..." changed to "Study on the behaviors of gunshot..."
# Applying the title correction does not necessary mean there will not be further partial match is the function is rerun on the new (corrected) Scopus dataframe. 
TitleCorrection <- read.csv("PartialTitleCorrected_VG.txt", sep = "\t", header = TRUE)
TitleCorrection <- as.data.frame(TitleCorrection)

ScopusReducedDataset$TICorrected <- gsr(as.character(ScopusReducedDataset$TI),as.character(TitleCorrection$raw.y),as.character(TitleCorrection$raw.x.Corrected))

########################################################
##### Search for authors between the two datasets
##### to generate a correction list
##### The recombined list should be the same length as the original minus any duplicates. Duplicates must be checked.

##### Scopus #####

# Split Column "AU" with the separator ";" and place it in AuthorListScopusExtended
AuthorListScopus <- ScopusReducedDataset %>%
  select(PY,TICorrected,AU,DES,IDS,C1S,DI)
# rename TICorrected column
names(AuthorListScopus)[names(AuthorListScopus)=="TICorrected"] <- "TI"

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListScopusExtended <- AuthorListScopus %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# export to correct the author names
write.table(AuthorListScopusExtended, "Authors to correct.txt", sep = "\t")

# read the corrected list of "Authors" and combine it to the original list
AuthorCorrected <- read.csv("Authors Name Corrected.txt", sep="\t", header=TRUE)
AuthorListScopusExtended$AuthorsCor <- gsr(AuthorListScopusExtended$Authors,AuthorCorrected$Name,as.character(AuthorCorrected$Name.Corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
# The corrected list should be the same lenght as the original version. Duplicates will be removed and they will need to be checked individually.
ScopusReducedDatasetCorrected <- AuthorListScopusExtended %>%
  group_by(PY,TI,AU,DES,IDS,C1S,DI) %>%
  summarise(Authors = paste(Authors, collapse = ";"), AuthorCorrected = paste(AuthorsCor, collapse = ";"))

# Duplicate check on the output:
DupeScopus <- ScopusReducedDataset %>%
  find_duplicates(PY,TI,AU)

# in this case found that 2 distinct records have different affiliation, 1 record don't have the same keywords, the last one have different keywords and DOI
# correcting the one with the keywords
ScopusReducedDataset <- ScopusReducedDataset %>%
  mutate(IDS = strsplit(as.character(IDS), ";", ))%>%
  unnest(IDS) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(PY,AU,TI,DES,C1S,DI) %>%
  summarise(IDS = sort(paste(IDS, collapse= ";")))%>%
  ungroup()

DupeScopus2 <- ScopusReducedDataset %>%
  find_duplicates(PY,TI,AU)

# correcting the one which have different affiliations
ScopusReducedDataset <- ScopusReducedDataset %>%
  mutate(C1S = strsplit(as.character(C1S), ","))%>%
  unnest(C1S) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(PY,AU,TI,DES,IDS,DI) %>%
  summarise(C1S = sort(paste(C1S, collapse= ",")))%>%
  ungroup()

DupeScopus3 <- ScopusReducedDataset %>%
  find_duplicates(PY,TI,AU)

# still have an issue with the last one, just delete the row (the reference with keyword was chosen)
# order by authors name
ScopusReducedDataset <- arrange(ScopusReducedDataset, AU)
# row to delete is 1200
#ScopusReducedDataset <-ScopusReducedDataset[-1200,]

##### Web of Science #####

# Split Column "AU" with the separator ";" and place it in AuthorListWebOfScienceExtended
AuthorListWebOfScience <- WebOfScienceReducedDataset %>%
  select(PY,TI,AU,DEW,IDW,C1W,DI)

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListWebOfScienceExtended <- AuthorListWebOfScience %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# read the corrected list of "Authors" and combine it to the original list
AuthorListWebOfScienceExtended$AuthorsCor <- gsr(AuthorListWebOfScienceExtended$Authors,AuthorCorrected$Name,as.character(AuthorCorrected$Name.Corrected))

# generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list

WebOfScienceReducedDatasetCorrected <- AuthorListWebOfScienceExtended %>%
  group_by(PY,TI,AU,DEW,IDW,C1W,DI) %>%
  summarise(Authors = paste(Authors, collapse = ";"), AuthorCorrected = paste(AuthorsCor, collapse = ";"))

DupeWebOfScience <- WebOfScienceReducedDataset %>%
  find_duplicates(PY,TI,AU)

########################################################
#####         To combine both lists into one       #####
########################################################

DatabaseOutputTemp <- bind_rows(WebOfScienceReducedDatasetCorrected, ScopusReducedDatasetCorrected)
DatabaseOutputTemp <- as.data.frame(DatabaseOutputTemp)

DatabaseOutput <- DatabaseOutputTemp %>%
  select(TI,PY,AuthorCorrected,DEW,IDW,DES,IDS,C1W,C1S,DI)

# To summarise by removing duplicate, grouping by Title and year. 
CombinedDataset <- DatabaseOutput %>%
  group_by(TI,PY) %>% summarise(AU = rem_dup_word(paste(AuthorCorrected[!is.na(AuthorCorrected)], collapse=", ")), DOI = rem_dup_word(paste(DI[!is.na(DI)], collapse=", ")), DEW = paste(DEW[!is.na(DEW)], collapse=", "), IDW = paste(IDW[!is.na(IDW)], collapse=", "), DES = paste(DES[!is.na(DES)], collapse=", "),IDS = paste(IDS[!is.na(IDS)], collapse=", "),C1W = paste(C1W[!is.na(C1W)], collapse=", "),C1S = paste(C1S[!is.na(C1S)], collapse=", "))

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
  find_duplicates(PY,TI,AU)

# The Authors' keywords (i.e. DEW amd DES) between the two lists should be the same, however it is not the case and some correction will be needed.
# The user can decide to use the original list of keywords given by the databases instead, adding "S" or "W" to "DE"
# The merged list (i.e. DEW +DES) is more exhautive than the individual one as for some records as the keywords do not appear in both lists
# It is best to remove  the white space present with the ";"
CombinedDataset <- transform(CombinedDataset, DE=paste(DEW, DES, sep="; "))
CombinedDataset$DE  <- as.character(gsub("; ",";",CombinedDataset$DE))

# Count the number of references with DES, DEW, IDS and IDW as well as their % to the total number of references
Totalref <- data.frame(nrow(CombinedDataset))
CountDES <- data.frame(table(CombinedDataset$DES, exclude = ""));CountDES
CountDEW <- data.frame(table(CombinedDataset$DEW, exclude = ""));CountDEW
CountIDS <- data.frame(table(CombinedDataset$IDS, exclude = ""));CountIDS
CountIDW <- data.frame(table(CombinedDataset$IDW, exclude = ""));CountIDW

KeywordTable_1 <- data.frame(nrow(CountDES))
KeywordTable_1[2,1] <- nrow(CountDEW)
KeywordTable_1[3,1] <- nrow(CountIDS)
KeywordTable_1[4,1] <- nrow(CountIDW)

rownames(KeywordTable_1)[rownames(KeywordTable_1)=="1"] <- "Author keywords Scopus"
rownames(KeywordTable_1)[rownames(KeywordTable_1)=="2"] <- "Author keywords WoS"
rownames(KeywordTable_1)[rownames(KeywordTable_1)=="3"] <- "Index keywords Scopus"
rownames(KeywordTable_1)[rownames(KeywordTable_1)=="4"] <- "Index keywords WoS"

KeywordTable_1[1,2] <- (KeywordTable_1[1,1]/Totalref)*100
KeywordTable_1[2,2] <- (KeywordTable_1[2,1]/Totalref)*100
KeywordTable_1[3,2] <- (KeywordTable_1[3,1]/Totalref)*100
KeywordTable_1[4,2] <- (KeywordTable_1[4,1]/Totalref)*100

KeywordTable_1 <- rownames_to_column(KeywordTable_1)
names(KeywordTable_1) <- c("Keywords", "Count", "%")

#Export to text file
#write.table(KeywordTable_1, file = "Merger_KeywordTable_1.csv", sep = ",", row.names = F)

#############################################################
#####                    Countries                      #####
#############################################################

# get city/country data
data(world.cities)

# replace "United States" with USA & "United Kingdom" with UK.
aff.lst <- gsub("UNITED STATES$", "USA", CombinedDataset$C1, perl = TRUE)  # toupper as affiliations in capital
aff.lst <- gsub("UNITED KINGDOM$", "UK", aff.lst, perl = TRUE)             # toupper as affiliations in capital
# replace ';' with ',' as multiple affiliations are separated with ';'
# but that doesn't fit with the strsplit()
aff.lst <- gsub(";", ",", aff.lst)
# split fields by ", "
splt.lst <- sapply(aff.lst, strsplit, split = ", ", USE.NAMES = FALSE)
# extract fields which match a known city making sure that diacritics aren't a problem...
city.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% toupper(world.cities$name))]) # toupper as affiliations in capital
# ... or country
# cntry.lst <- lapply(splt.lst, function(x)x[which(removeDiacritics(x) %in% toupper(world.cities$country.etc))]) # toupper as affiliations in capital
# this version only returns unique instances of countries per publication
cntry.lst <- lapply(splt.lst, function(x)unique(x[which(x %in%  toupper(world.cities$country.etc))]))  # toupper as affiliations in capital

## generate plot of papers per country
threshold <- 5
cntry.dat <- data.frame(Country = removeDiacritics(unlist(cntry.lst)), stringsAsFactors = FALSE)

# define continent for each country
cntry.dat$Continent <- countrycode(sourcevar = cntry.dat[, "Country"],
                            origin = "country.name",
                            destination = "continent")

# get countries under threshold
other.dat <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  summarise(Count = n()) %>% 
  filter(Count <= threshold)
# aggregate counts as 'Others'
other.dat <- data.frame(Country = "Others", Continent = "Other", Count = sum(other.dat$Count))

# Collate counts for countries over threshold
cntry.dat <- cntry.dat %>% 
  group_by(Country, Continent) %>% 
  summarise(Count = n()) %>% 
  filter(Count > threshold)
# order by count
cntry.dat$Country <- reorder(cntry.dat$Country, +cntry.dat$Count)
# add in 'Others'
cntry.dat <- rbind(other.dat, data.frame(cntry.dat))

# plot
p <- ggplot(cntry.dat, aes(x=Country, y=Count, fill=Continent)) + 
  geom_col() +
  scale_fill_manual(values = c("gray", brewer.pal(6, "Set1")), breaks = c("Americas", "Asia", "Europe", "Oceania","Africa", "Other")) +
  xlab('Country Affiliation') +
  ylab('Total Papers') +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(family = "Palatino"))
show(p)
ggsave("Fig2_CountryCounts.png", p, width = 8, height = 6, units = "in", dpi=150)

#############################################################
#####                     Keywords                      #####
#############################################################

# This section looks at Keywords

#############################################################

#############################################################
#####        Select one of the following options        #####
#############################################################

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

#############################################################

#Split Column "AIK" in row by the separator ";", remove leading white space to generate list
ScopusKeywordList <- CombinedDataset %>% 
  mutate(AIKeywords = strsplit(as.character(AIK), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIK" in "KeywordList" and save in dataframe
# Extract list of "AIK" and remove duplicate
ScopusKeywordList$AIKeywords <- toupper(ScopusKeywordList$AIKeywords)
KeywordList <- ScopusKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

# Most cited keywords before correction - Extract a list of keywords to apply corrections
KeywordListCount <- aggregate(KeywordList$AIKeywords, by=list(Freq=KeywordList$AIKeywords), FUN=length)
names(KeywordListCount) <- c("Keywords","Count")
write.csv(KeywordListCount,"Keywords to correct.csv")

#############################################################
#####                  Data cleansing                   #####
#############################################################

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.

#read the corrected list of keywords and combine it to the original list
KeywordsCorrected <- read.csv("KeywordsCorrection.txt", sep="\t", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)
ScopusKeywordList$KeywordsCorrected <- gsr(as.character(ScopusKeywordList$AIKeywords),as.character(KeywordsCorrected$AIKeywords),as.character(KeywordsCorrected$CorrectedAIKeywords))


#############################################################
#####               Data analysis - Keywords            #####
#############################################################


#Count to number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(CombinedDataset$PY));PublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
ScopusKeywordListTemp1 <- ScopusKeywordList  %>%
  select(PY,TI,KeywordsCorrected) %>%
  distinct()
names(ScopusKeywordListTemp1) <- c("Year","Title","KeywordsCorrected")

ScopusKeywordListTemp2 <-ScopusKeywordListTemp1[complete.cases(ScopusKeywordListTemp1), ]
sum(is.na(ScopusKeywordListTemp2$KeywordsCorrected))

ScopusKeywordYearCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Year=ScopusKeywordListTemp2$Year, Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)
ScopusKeywordTotalCount <- aggregate(ScopusKeywordListTemp2$Year, by=list(Rtitle=ScopusKeywordListTemp2$KeywordsCorrected), FUN=length)
# ScopusKeywordListTemp5 <- aggregate(ScopusKeywordListTemp2, by=list(ScopusKeywordListTemp2$Year), FUN=length)

# narrowing range for plot
ScopusKeywordNarrowRangeGraph <- subset(ScopusKeywordTotalCount,x>30)

SubsetKeywordNarrowRangeGraph <-subset(ScopusKeywordYearCount,Rtitle %in% ScopusKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
SubsetKeywordNarrowRangeGraph$x <- as.numeric(SubsetKeywordNarrowRangeGraph$x)

#############################################################
#####                      GRAPH                        #####
#############################################################

# Create a new variable from incidence
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,5,10,20,30,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10","11-20","21-30",">30"))

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,5,10,20,30,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10","11-20","21-30",">30")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# KeywordList$WYear <- gsr(KeywordList$Year,year$Var1,1/year$Freq)
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
  theme_grey(base_size=8)+
  theme(text = element_text(family = "Palatino"),
        legend.position="right",legend.direction="vertical",
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

ggsave("Fig5_KeywordTrend.png", p, width = 6, height = 8, units = "in", dpi=150)

#############################################################
#####                     Authors                       #####
#############################################################

# This section is looks at Authors to generate a table:
#                                   the total number of authors and publications per year,
#                                   the number of publications per author,
#                                   the first year an author published,
#                                   the number of new author  

#####______________Data analysis - Authors______________#####

#Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListExtended <- CombinedDataset %>% 
  mutate(Authors = strsplit(as.character(AU), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)

# Considering only the publications with listed authors instead of the number of publications per year: i.e. PublicationYear
PublicationYearWithAuthors <- aggregate(AuthorListExtended$Authors,list(AuthorListExtended$TI,AuthorListExtended$PY), FUN=length)
names(PublicationYearWithAuthors) <- c("TI","Year","Frequency")

PublicationYearWithAuthorsSummary <- data.frame(table(PublicationYearWithAuthors$Year))
names(PublicationYearWithAuthorsSummary) <- c("Year","PublicationsAuthors")
PublicationYearWithAuthorsSummary$Year <- as.numeric(as.character(PublicationYearWithAuthorsSummary$Year))


AuthorCountPaper <- aggregate(AuthorListExtended$Authors, list(AuthorListExtended$Authors), FUN=length)
names(AuthorCountPaper) <- c("Author","Frequency")

# Number of Authors per year
NumberAuthorYear <- aggregate(AuthorListExtended$Authors,list(AuthorListExtended$PY), FUN=length)
names(NumberAuthorYear) <- c("Year","Author")

# List of "Author" with one publication only
AuthorCountSinglePaper <- subset(AuthorCountPaper,Frequency<2)
AuthorCountSinglePaperReduced <-subset(AuthorListExtended,Authors %in% AuthorCountSinglePaper$Author)

# List of "Author" with one publication only and their publication "Year"
YearNewAuthorSinglePaper <- aggregate(AuthorCountSinglePaperReduced$Authors,list(AuthorCountSinglePaperReduced$PY), FUN=length)
names(YearNewAuthorSinglePaper) <- c("Year","Single Author")

# List of Authors with multiple publications only
AuthorCountMultiplePaper <- subset(AuthorCountPaper,Frequency>1)
AuthorCountMultiplePaperReduced <- subset(AuthorListExtended,Authors %in% AuthorCountMultiplePaper$Author)

# List of "Authors" with multiple output and their first "Year"
AuthorFirstAppearanceMultipleEntry<- aggregate(AuthorCountMultiplePaperReduced$PY, list(AuthorCountMultiplePaperReduced$Authors), min)
names(AuthorFirstAppearanceMultipleEntry) <- c("Author","Year")

# List of "Authors" with multiple output and their last "Year"
 AuthorMultipleOutputLastYear<- aggregate(AuthorCountMultiplePaperReduced$PY, list(AuthorCountMultiplePaperReduced$Authors), max)
 names(AuthorMultipleOutputLastYear) <- c("Id","Year")  # "Id" is essential for ammending on Gephi import

#####__________________Gephi Plots - Authors_________________#####

 #generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(PY,TI) %>%
  summarise(AuthorCorrected = paste(Authors, collapse = ";"))
ListAuthor <- as.data.frame(ListAuthor)

GephiAuthor <- ListAuthor %>%
  select(AuthorCorrected)
names(GephiAuthor) <- c("Author")

#Export to Gephi plot - Year;Title;Authors
write.table(GephiAuthor, file = "GephiAuthor.csv", quote = F, sep = "\t", row.names = F)

#Export to Gephi plot - Authors;Year
write.table(AuthorMultipleOutputLastYear, file = "GephiListAuthorLastYear.csv", quote = F, sep = "\t", row.names = F)

#####__________________Table 1 Output____________________#####

# List of new "Authors" and their first "Year" of appearance
AuthorFirstAppearance<- aggregate(AuthorListExtended$PY, list(AuthorListExtended$Authors), min)
names(AuthorFirstAppearance) <- c("Author","Year")

YearNewAuthor <- aggregate(AuthorFirstAppearance$Author,list(AuthorFirstAppearance$Year), FUN=length)
names(YearNewAuthor) <- c("Year","New Authors")
YearOutput <- Reduce(merge, list(NumberAuthorYear,PublicationYear,PublicationYearWithAuthorsSummary,YearNewAuthor))
YearTableOutput <- merge(YearOutput, YearNewAuthorSinglePaper, by="Year", all = T)
YearTableOutput$Ratio <- round(YearOutput$Author/YearOutput$Publications, 1)
YearTableOutput$`New Author Percentage` <- round(YearOutput$`New Authors`/YearOutput$Author*100, 1)

#Export to text file for Latex import
write.table(YearTableOutput, file = "Table1_full.txt", sep = " & ", row.names = F)

