#############################################################
#####                     To read                       #####
#############################################################
# This R script is the second step after the merge Scopus and Web of Sciences (BibTex format)
# This script allows a bibliometric analysis on the Keywords 
# The choice of the keywords to analyse (Authors Keywords, Index Keywords, Both) is done in the previous script

# read the export *.csv document from Merger, separation "\t", and place it in data.frame "MergerOriginalData"
MergerOriginalData <- read.csv(paste0(Results.dir,"Result_Merger_Dataset.txt"), sep="\t", header=TRUE)

# replace all empty in DE with NA
MergerOriginalData$AIK[MergerOriginalData$AIK==""]<-NA


#############################################################
#####                  Data cleansing                   #####
#############################################################

#Split Column "AIK" in row by the separator ";", remove leading white space to generate list
MergeDataKeywordList <- MergerOriginalData %>% 
  mutate(AIKeywords = strsplit(as.character(AIK), ";")) %>% 
  unnest(AIKeywords) %>%
  mutate_if(is.character, str_trim)

# Upper case "AIK" in "KeywordList" and save in dataframe
# Extract list of "AIK" and remove duplicate
MergeDataKeywordList$AIKeywords <- toupper(MergeDataKeywordList$AIKeywords)
KeywordList <- MergeDataKeywordList %>%
  select(AIKeywords)
Keyword <- KeywordList %>%
  distinct()

# Most cited keywords before correction - Extract a list of keywords to apply corrections
KeywordListCount <- aggregate(KeywordList$AIKeywords, by=list(Freq=KeywordList$AIKeywords), FUN=length) #first row could have the number of null entries for the column AIKeywords
names(KeywordListCount) <- c("Keywords","Count")

#Correction to the keywords can be applied at this stage. This can be done in Notepad++, Excel etc. The ultimate order of the list must be kept so it can be binded to the orignial data.
#read the corrected list of keywords and combine it to the original list
# The list of corrected Keywords from Sobreira et al. was added to correction of the top 100 keywords from KeywordListCount
KeywordsCorrected <- read.csv("CorrectionLists/KeywordsCorrection.txt", sep="\t", header=TRUE)
KeywordsCorrected <- as.data.frame(KeywordsCorrected)


# for full keywords
# MergeDataKeywordList$KeywordsCorrected <- gsr(as.character(MergeDataKeywordList$AIKeywords),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$KeywordsCorrected))
# or for use of acronyms or long keywords
MergeDataKeywordList$KeywordsCorrected <- gsr(as.character(MergeDataKeywordList$AIKeywords),as.character(KeywordsCorrected$Keywords),as.character(KeywordsCorrected$Acronyms))

#############################################################
#####               Data analysis - Keywords            #####
#############################################################

# Number of references with no keywords provided by the authors (AIK)
sum(is.na(MergerOriginalData$AIK))
# dataframe with references with no AIK
NoAIK <- MergerOriginalData[is.na(MergerOriginalData$AIK),]
# Type of document in NoAIK
CountDT <- data.frame(table(NoAIK$DT, exclude = ""));CountDT

#####__________________Average number of keywords per year_________________#####
#Count the number of time the same year is repeated in the "ScopusKeywordList$Year" and save in a data.frame "Year" 
PublicationYear<- data.frame(table(MergerOriginalData$PY));PublicationYear
names(PublicationYear) <- c("Year","Publications")

#count the number of keywords per title paper 
MergeDataKeywordListTemp1 <- MergeDataKeywordList  %>%
  select(PY,TI,KeywordsCorrected)
names(MergeDataKeywordListTemp1) <- c("Year","Title","KeywordsCorrected")

MergeDataKeywordListTemp2 <- MergeDataKeywordListTemp1[complete.cases(MergeDataKeywordListTemp1), ]
sum(is.na(MergeDataKeywordListTemp2$KeywordsCorrected))

#############################################################
#####               Keyword trend graph                 #####
#############################################################

MergeDataKeywordYearCount <- aggregate(MergeDataKeywordListTemp2$Year, by=list(Year=MergeDataKeywordListTemp2$Year, Rtitle=MergeDataKeywordListTemp2$KeywordsCorrected), FUN=length)
MergeDataKeywordTotalCount <- aggregate(MergeDataKeywordListTemp2$Year, by=list(Rtitle=MergeDataKeywordListTemp2$KeywordsCorrected), FUN=length)
# MergeDataKeywordListTemp5 <- aggregate(MergeDataKeywordListTemp2, by=list(MergeDataKeywordListTemp2$Year), FUN=length)

Count <- number
# narrowing range for plot
DatasetKeywordNarrowRangeGraph <- top_n(MergeDataKeywordTotalCount, Count)

# count the number of rows, hence the number of keywords in figure
a <- nrow(DatasetKeywordNarrowRangeGraph)

while (a>maximum) {
  Count <- Count-1
  DatasetKeywordNarrowRangeGraph <- top_n(MergeDataKeywordTotalCount, Count)
  a <- nrow(DatasetKeywordNarrowRangeGraph)
}

SubsetKeywordNarrowRangeGraph <-subset(MergeDataKeywordYearCount,Rtitle %in% DatasetKeywordNarrowRangeGraph$Rtitle)
#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
SubsetKeywordNarrowRangeGraph$x <- as.numeric(SubsetKeywordNarrowRangeGraph$x)

#####______________Graph for keywords with a frequency >=5 ______________##########
# Create a new variable from incidence
SubsetKeywordNarrowRangeGraph$Incidenceweight <- cut(SubsetKeywordNarrowRangeGraph$x,
                                                     breaks = c(-1,0,1,2,5,10,max(SubsetKeywordNarrowRangeGraph$x,na.rm=T)),
                                                     labels=c("0","1","2","3-5","6-10","11-20"))

GraphTemp1 <- SubsetKeywordNarrowRangeGraph %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,5,10,max(x,na.rm=T)),
                         labels=c("0","1","2","3-5","6-10","11-20")))  %>%
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
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF"),na.value = "grey90")+
  #coord_fixed()+
  theme_grey(base_size=16)+
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
show(p)
#ggplotly(p)
ggsave("KeywordTrend_ScopWoS.png", p, width = 13, height = 20, units = "in", dpi=500, path = "Results")
