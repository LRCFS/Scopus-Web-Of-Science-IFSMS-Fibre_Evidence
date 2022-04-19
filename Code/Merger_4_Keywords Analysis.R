#############################################################
#####                     To read                       #####
#############################################################
# This R script is the second step after the merge of exported data from Scopus and Web of Sciences, and the importation of IFSMS data
# This script allows a bibliometric analysis on the Keywords 
# The choice of the keywords to analyse (Authors Keywords, Index Keywords, Both) is done in the first script (Merger_1_Exclusion.R)

#############################################################
#####                  Data loading                   #####
#############################################################

# read the export *.csv document from Merger, separation "\t", and place it in data.frame "MergerOriginalData"
MergerOriginalData <- read.csv(paste0(Results.dir,"Result_Merger_Dataset.txt"), sep="\t", header=TRUE)
IFSMS <- read.csv(paste0(Results.dir,"Result_IFSMS_Dataset.txt"), sep="\t", header=TRUE)

# replace all empty in DE with NA 
MergerOriginalData$AK[MergerOriginalData$AK==""]<-NA
IFSMS$AKeywords[IFSMS$AKeywords==""]<-NA

### information about reference without author's keywords, before removing them
# Number of references with no keywords provided by the authors (AK)
sum(is.na(MergerOriginalData$AK))
# dataframe with references with no AK
NoAK <- MergerOriginalData[is.na(MergerOriginalData$AK),]
# Type of document in NoAK
CountDT <- data.frame(table(NoAK$DT, exclude = ""));CountDT

# Number of references with no keywords provided by the authors (AKeywords)
sum(is.na(IFSMS$AKeywords))
# dataframe with references with no AKeywords
IFSMSNoAK <- IFSMS[is.na(IFSMS$AKeywords),]
# Type of document in IFSMSNoAK
IFSMSCountDT <- data.frame(table(IFSMSNoAK$Document.Type, exclude = ""));IFSMSCountDT

rm(NoAK)
rm(CountDT)
rm(IFSMSNoAK)
rm(IFSMSCountDT)

# remove entry with Na in column AK and AKeywords
MergerOriginalData <- MergerOriginalData %>% drop_na(AK)
IFSMS <- IFSMS %>% drop_na(AKeywords)

#############################################################
#####                  Data cleansing                   #####
#############################################################

###############################################
#####             MergerDataset           #####
###############################################

#Split Column "AK" in row by the separator ";", remove leading white space to generate list
MergeDataKeywordList <- MergerOriginalData %>% 
  mutate(AKeywords = strsplit(as.character(AK), ";")) %>% 
  unnest(AKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AK" in "KeywordList" and save in dataframe
# Extract list of "AK" and remove duplicate
MergeDataKeywordList$AKeywords <- toupper(MergeDataKeywordList$AKeywords)
MergeKeywordList <- MergeDataKeywordList %>%
  select(AKeywords)
MergeKeyword <- MergeKeywordList %>%
  distinct()

# Most cited keywords before correction - Extract a list of keywords to apply corrections
MergeKeywordListCount <- aggregate(MergeKeywordList$AKeywords, by=list(Freq=MergeKeywordList$AKeywords), FUN=length) #first row could have the number of null entries for the column AKeywords
names(MergeKeywordListCount) <- c("Keywords","Count")

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
# The list of corrected Keywords from Sobreira et al. was added to correction of the top 100 keywords from MergeKeywordListCount
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrection.txt", sep="\t", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)
KeywordsCorrected$Keywords <- as.character(KeywordsCorrected$Keywords)
KeywordsCorrected$KeywordsCorrected <- as.character(KeywordsCorrected$KeywordsCorrected)
KeywordsCorrected$Acronyms <- as.character(KeywordsCorrected$Acronyms)

KeywordsCorrected$Simplified <- gsub("\\s*\\([^\\)]+\\)\\s*$","",as.character(KeywordsCorrected$Acronyms))

# replace blanck with "NA"
KeywordsCorrected$Simplified[KeywordsCorrected$Simplified==""]<-0


for (i in 1:nrow(KeywordsCorrected)) { # for-loop over rows
  if (is.na(KeywordsCorrected[i,4])) {
    i <- i+1
  }
  else {
  m <- KeywordsCorrected[i,4]
    if (m == "0") {
      KeywordsCorrected[i,4] <- KeywordsCorrected[i,3]
    }
  }
}


# for full keywords
# MergeDataKeywordList$KeywordsCorrected <- gsr(as.character(MergeDataKeywordList$AKeywords),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$KeywordsCorrected))
# or for use of acronyms or long keywords
MergeDataKeywordList$KeywordsCorrected <- gsr(as.character(MergeDataKeywordList$AKeywords),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$Simplified))

#######################################
#####             IFSMS           #####
#######################################

#Split Column "AKeywords" in row by the separator ";", remove leading white space to generate list
IFSMSKeywordList <- IFSMS %>% 
  mutate(AK = strsplit(as.character(AKeywords), ";")) %>% 
  unnest(AK) %>%
  mutate_if(is.character, str_trim)

# Upper case "AK" in "IFSMSKeywordList" and save in dataframe
# Extract list of "AK" and remove duplicate
IFSMSKeywordList$AK <- toupper(IFSMSKeywordList$AK)
IFSMSKeywordListAK <- IFSMSKeywordList %>%
  select(AK)
IFSMSKeywordAK <- IFSMSKeywordListAK %>%
  distinct()

# for full keywords
# IFSMSKeywordList$KeywordsCorrected <- gsr(as.character(IFSMSKeywordList$AK),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$KeywordsCorrected))
# or for use of acronyms or long keywords
IFSMSKeywordList$KeywordsCorrected <- gsr(as.character(IFSMSKeywordList$AK),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$Simplified))

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

###############################################
#####             MergerDataset           #####
###############################################

#####__________________Average number of keywords per year_________________#####
#Count the number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(MergerOriginalData$PY));PublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
MergeDataKeywordListTemp1 <- MergeDataKeywordList  %>%
  select(PY,TI,KeywordsCorrected)
names(MergeDataKeywordListTemp1) <- c("Year","Title","KeywordsCorrected")

# Removing every year after 2019
#MergeDataKeywordListTemp1$Year <- as.numeric(as.character(MergeDataKeywordListTemp1$Year))
#MergeDataKeywordListTemp1 <- filter(MergeDataKeywordListTemp1, Year<2019)

MergeDataKeywordListTemp2 <- MergeDataKeywordListTemp1[complete.cases(MergeDataKeywordListTemp1), ]
sum(is.na(MergeDataKeywordListTemp2$KeywordsCorrected))

#############################################################
#####               Keyword trend graph                 #####
#############################################################

MergeDataKeywordYearCount <- aggregate(MergeDataKeywordListTemp2$Year, by=list(Year=MergeDataKeywordListTemp2$Year, Rtitle=MergeDataKeywordListTemp2$KeywordsCorrected), FUN=length)
MergeDataKeywordTotalCount <- aggregate(MergeDataKeywordListTemp2$Year, by=list(Rtitle=MergeDataKeywordListTemp2$KeywordsCorrected), FUN=length)

# MergeDataKeywordListTemp5 <- aggregate(MergeDataKeywordListTemp2, by=list(MergeDataKeywordListTemp2$Year), FUN=length)

Count <- numberKeywordDataset

# narrowing range for plot
DatasetKeywordNarrowRangeGraph <- top_n(MergeDataKeywordTotalCount, Count)

# count the number of rows, hence the number of keywords in figure
a <- nrow(DatasetKeywordNarrowRangeGraph)

while (a>numberMaxKeywordDataset) {
  Count <- Count-1
  DatasetKeywordNarrowRangeGraph <- top_n(MergeDataKeywordTotalCount, Count)
  a <- nrow(DatasetKeywordNarrowRangeGraph)
}

SubsetKeywordNarrowRangeGraph <-subset(MergeDataKeywordYearCount,Rtitle %in% DatasetKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
SubsetKeywordNarrowRangeGraph$x <- as.numeric(SubsetKeywordNarrowRangeGraph$x)

#####______________Graph for the top keywords - Figure 2 ______________##########
# Create a new variable from incidence
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,5,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10"))

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,5,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10")))  %>%
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
  #  labs(x="",y="",title="Keywords found in fibre publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2020))+
  scale_fill_manual(values=c("#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF"),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=20)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=18,colour=textcol),
        axis.text.y=element_text(size=18,vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12)) + 
  labs(caption = "Principal component analysis (PCA), Hollow fiber-based liquid-phase microextraction (HF-LPME)\nGas chromatography–mass spectrometry (GC-MS), Fourier-transform infrared spectroscopy (FT-IR)\nHigh-performance liquid chromatography (HPLC),Scanning electron microscope (SEM)")
show(p)

#ggplotly(p)
ggsave("KeywordTrend_ScopWoS.png", p, width = 14, height = 19, units = "in", dpi=600, path = "Results")

#######################################
#####             IFSMS           #####
#######################################

#####__________________Average number of keywords per year_________________#####
#Count the number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
IFSMSPublicationYear<- data.frame(table(IFSMS$Year));IFSMSPublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
IFSMSKeywordListTemp1 <- IFSMSKeywordList  %>%
  select(Year,Title,KeywordsCorrected)
names(IFSMSKeywordListTemp1) <- c("Year","Title","KeywordsCorrected")

IFSMSKeywordListTemp2 <- IFSMSKeywordListTemp1[complete.cases(IFSMSKeywordListTemp1), ]
sum(is.na(IFSMSKeywordListTemp2$KeywordsCorrected))

#############################################################
#####               Keyword trend graph                 #####
#############################################################

IFSMSKeywordYearCount <- aggregate(IFSMSKeywordListTemp2$Year, by=list(Year=IFSMSKeywordListTemp2$Year, Rtitle=IFSMSKeywordListTemp2$KeywordsCorrected), FUN=length)
IFSMSKeywordTotalCount <- aggregate(IFSMSKeywordListTemp2$Year, by=list(Rtitle=IFSMSKeywordListTemp2$KeywordsCorrected), FUN=length)
# IFSMSKeywordListTemp5 <- aggregate(IFSMSKeywordListTemp2, by=list(IFSMSKeywordListTemp2$Year), FUN=length)

Count <- numberKeywordIfsms

# narrowing range for plot
IFSMSDatasetKeywordNarrowRangeGraph <- top_n(IFSMSKeywordTotalCount, Count)

# count the number of rows, hence the number of keywords in figure
b <- nrow(IFSMSDatasetKeywordNarrowRangeGraph)

while (b>numberMaxKeywordIfsms ) {
  Count <- Count-1
  IFSMSDatasetKeywordNarrowRangeGraph <- top_n(IFSMSKeywordTotalCount, Count)
  b <- nrow(IFSMSDatasetKeywordNarrowRangeGraph)
}

IFSMSDatasetKeywordNarrowRangeGraph <-subset(IFSMSKeywordYearCount,Rtitle %in% IFSMSDatasetKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
IFSMSDatasetKeywordNarrowRangeGraph$x <- as.numeric(IFSMSDatasetKeywordNarrowRangeGraph$x)

#####______________Graph for keywords - Figure 6 ______________##########
# Create a new variable from incidence
IFSMSDatasetKeywordNarrowRangeGraph$Incidenceweight <- cut(IFSMSDatasetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,3,max(IFSMSDatasetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10"))

IFSMSGraphTemp1 <- IFSMSDatasetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,3,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# KeywordList$WYear <- gsr(KeywordList$Year,year$Var1,1/year$Freq)
IFSMSGraphTemp2 <- aggregate(IFSMSGraphTemp1[, 1], list(IFSMSGraphTemp1$KeywordsCorrected), min)

IFSMSGraphTemp1$graphorder <- as.numeric(gsr(IFSMSGraphTemp1$KeywordsCorrected,IFSMSGraphTemp2$Group.1,IFSMSGraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p2 <- ggplot(IFSMSGraphTemp1,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in fibre publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF"),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=25)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p2)
#ggplotly(p)
ggsave("KeywordTrend_IFSMS.png", p2, width = 14, height = 10, units = "in", dpi=500, path = "Results")
