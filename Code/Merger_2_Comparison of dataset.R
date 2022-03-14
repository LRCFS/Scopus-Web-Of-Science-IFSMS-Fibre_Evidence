#############################################################
#####                     To read                       #####
#############################################################
# This R script is the second step after the merge of exported data from Scopus and Web of Sciences, and the importation of IFSMS data
# This script allows to compare Scopus, Web of Science and the IFSMS reports (Journals and document type)
# It can be choosen to work with the combined dataset before the exclusion process (CombinedDataset)
# Or it can be choosen to work with the combined dataset after the exclusion process (MergerOriginalData)

#############################################################
#####                    Data loading                   #####
#############################################################
# read the export *.csv document from Merger_1_Exclusion, separation "\t", and place it in data.frame "MergerOriginalData"
MergerOriginalData <- read.csv(paste0(Results.dir,"Result_Merger_Dataset.txt"), sep="\t", header=TRUE)
CombinedDataset <- read.csv(paste0(Results.dir,"Result_MergerExclusion_Dataset.txt"), sep="\t", header=TRUE)
IFSMS <- read.csv(paste0(Results.dir,"Result_IFSMS_Dataset.txt"), sep="\t", header=TRUE)
ScopusReducedDatasetTIAUC1SIDSSODTcor <- read.csv(paste0(Results.dir,"Result_Scopus_CorrectedDataset.txt"), sep="\t", header=TRUE)
WebOfScienceReducedDatasetAUSODTcor <- read.csv(paste0(Results.dir,"Result_WebOfScience_CorrectedDataset.txt"), sep="\t", header=TRUE)


##################################################################################
#####              Comparison Scopus/Web Of Science all year                 #####
##################################################################################
 
#####______________creation of dataset______________##########
# In the previous dataset, the column Coder can be used to calculate the percentage of articles both present in Scopus and in WoS
# A selection between CombinedDataset or MergerOriginalData must be done here and will be applied for the rest of the code
# Scopus not exclusive = Web of science not exclusive
ScopWosnotexclusive <- CombinedDataset[CombinedDataset$Coder == "Scopus,WebOfScience"|CombinedDataset$Coder == "WebOfScience,Scopus"|CombinedDataset$Coder == "Scopus,Scopus,WebOfScience"|CombinedDataset$Coder == "Scopus,WebOfScience,WebOfScience", ]
Scopusexclusive <- CombinedDataset[CombinedDataset$Coder == "Scopus"|CombinedDataset$Coder == "Scopus,Scopus", ]
WoSexclusive <- CombinedDataset[CombinedDataset$Coder == "WebOfScience", ]

#####______________Analysis______________##########
# Count the number of references in each data.frame
countWoSexclusive <- as.numeric(count(WoSexclusive));countWoSexclusive
countScopusexclusive <- as.numeric(count(Scopusexclusive));countScopusexclusive
countScopWosnotexclusive <- as.numeric(count(ScopWosnotexclusive));countScopWosnotexclusive

Total <- (countWoSexclusive+countScopusexclusive+countScopWosnotexclusive);Total
#pourcentage of article in WoS only
X <- ((countWoSexclusive/Total) * 100); X
#pourcentage of article in Scop only
Y <- ((countScopusexclusive/Total) * 100); Y
# pourcentage of articles shared with both databases
Z <- ((countScopWosnotexclusive/Total) * 100); Z
 
#################################################
##### Journals in Scopus and Web Of Science ##### 
#################################################

# Count the number of time each Journals appear in  Scopus
JournalsScopus <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$SO, exclude = ""))
JournalsScopus <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$SO, exclude = NA));JournalsScopus
names(JournalsScopus) <- c("Journals", "CountScopus")

# Count the number of time each Journals appear only in the Scopus output and not shared with WoS
JournalsScop <- data.frame(table(Scopusexclusive$SO, exclude = ""))
JournalsScop <- data.frame(table(Scopusexclusive$SO, exclude = NA));JournalsScop
names(JournalsScop) <- c("Journals", "CountScopus")

# Count the number of time each Journals appear in Web Of Science 
JournalWoScience <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$SO, exclude = ""))
JournalWoScience <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$SO, exclude = NA));JournalWoScience
names(JournalWoScience) <- c("Journals", "CountWoS")

# Count the number of time each Journals appear only in Web Of Science and not shared in Scopus
JournalWoS <- data.frame(table(WoSexclusive$SO, exclude = ""))
JournalWoS <- data.frame(table(WoSexclusive$SO, exclude = NA));JournalWoS
names(JournalWoS) <- c("Journals", "CountWoS")

# Count the number of time each Journals appear with shared references between Scopus and WoS
JournalsCombinedDataset <- data.frame(table(MergerOriginalData$SO, exclude = ""))
JournalsCombinedDataset <- data.frame(table(MergerOriginalData$SO, exclude = NA));JournalsCombinedDataset
names(JournalsCombinedDataset) <- c("Journals", "Count")

# this table combined the results
JournalWoSAll <- left_join(JournalsCombinedDataset,JournalWoS)
JournalTotal <- left_join(JournalWoSAll,JournalsScop)

# select the most counted journals in either Scopus, WoS or in both datasets (using numberTitle in Global)
# for Scopus
TopJournalsScop <- top_n(JournalTotal, numberTitle, CountScopus) 
a <- as.numeric(nrow(TopJournalsScop))
b <- numberTitle

while (a>numberTitleMax) {
  b <- b-1
  TopJournalsScop <- top_n(JournalTotal, b, CountScopus)
  a <- as.numeric(nrow(TopJournalsScop))
}

# for WoS
TopJournalsWoS <- top_n(JournalTotal, numberTitle, CountWoS) 
a <- as.numeric(nrow(TopJournalsWoS))
b <- numberTitle

while (a>numberTitleMax) {
  b <- b-1
  TopJournalsWoS <- top_n(JournalTotal, b, CountWoS)
  a <- as.numeric(nrow(TopJournalsWoS))
}

# in both Scopus and WoS
TopJournalsScopusWoS <- top_n(JournalTotal, numberTitle, Count)
a <- as.numeric(nrow(TopJournalsScopusWoS))
b <- numberTitle

while (a>numberTitleMax) {
  b <- b-1
  TopJournalsScopusWoS <- top_n(JournalTotal, b, Count)
  a <- as.numeric(nrow(TopJournalsScopusWoS))
}

# combine all the results
# Scopus and Wos only
TopJournalScopusOrWosOnly <- rbind(TopJournalsScop,TopJournalsWoS)
# including Scopus Wos common
TopJournal <-rbind(TopJournalScopusOrWosOnly,TopJournalsScopusWoS)
TopJournal <- TopJournal %>%
  distinct()

# add a column total count per journal
TopJournal$Total <- rowSums(TopJournal[ , c(2:4)], na.rm=TRUE)
names(TopJournal) <- c("Journals", "ScopWos","WebofScienceExclusive", "ScopusExclusive", "Total")

# Select top 12 journal in TOPJournal$Total
TopJournal <- top_n(TopJournal, 12, Total)

# Make some modification on the name of Journals that are too long (if needed)
TopJournal$Journals <- gsub("PROCEEDINGS OF SPIE - THE INTERNATIONAL SOCIETY FOR OPTICAL ENGINEERING","PROC. SPIE - INT. SOC. OPT. ENG.", TopJournal$Journals)
TopJournal$Journals <- gsub("ITCANDDC: 5TH INTERNATIONAL TEXTILE, CLOTHING AND DESIGN CONFERENCE 2010, BOOK OF PROCEEDINGS: MAGIC WORLD OF TEXTILES",
                                    "5TH ITCANDDC 2010", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF CHROMATOGRAPHY B-ANALYTICAL TECHNOLOGIES IN THE BIOMEDICAL AND LIFE SCIENCES","JOURNAL OF CHROMATOGRAPHY B", TopJournal$Journals)

# take difference of reference counts
# and make long
forOverlapPlotTemp3 <- gather(TopJournal, Database, Frequency, ScopWos:ScopusExclusive, factor_key=TRUE)

# GRAPH - Figure 1
plotoverlap <- ggplot(forOverlapPlotTemp3, aes(x = reorder(Journals,-Total), y = Frequency, fill = Database)) + 
  geom_bar(position = position_dodge(0.8),
           stat = "identity",
           width =0.8) + 
  coord_flip() + 
  geom_text(aes(label = ifelse( Frequency>0, Frequency , ""), y = Frequency + 0.05), position = position_dodge(0.8), hjust = 0, colour="black", size=6) +
  labs(y= "Number of documents", x="")+
  scale_fill_manual(labels = c('Scopus and WOS', 'WOS only', 'Scopus only'), values = brewer.pal(3, 'Paired')[1:3]) + 
  theme_bw(base_size = 20)+
  theme(panel.grid.major.y = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=20)
  )

show(plotoverlap)
ggsave("Journals.png", plotoverlap, width = unit(16.5, 'in'), height = unit(15, 'in'), dpi=300, path = "Results")

######################################################
##### Document type in Scopus and Web Of Science ##### 
######################################################

# Count the number of time each document type appear in Scopus, after corrections
DocumentTypeScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$DT, exclude = ""))
DocumentTypeScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$DT, exclude = NA));DocumentTypeScop
names(DocumentTypeScop) <- c("DocumentType", "Count")

# Count the number of time each document type appear in Web of Science
DocumentTypeWoS <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$DT, exclude = ""))
DocumentTypeWoS <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$DT, exclude = NA));DocumentTypeWoS
names(DocumentTypeWoS) <- c("DocumentType", "Count")

# Count the number of time each document type appear in Combined Dataset
DocumentTypeCombined <- data.frame(table(CombinedDataset$DT, exclude = ""))
DocumentTypeCombined <- data.frame(table(CombinedDataset$DT, exclude = NA));DocumentTypeCombined
names(DocumentTypeCombined) <- c("DocumentType", "Count")

# Count the number of time each document type appear in MergerOriginalData
DocumentTypeMerger <- data.frame(table(MergerOriginalData$DT, exclude = ""))
DocumentTypeMerger <- data.frame(table(MergerOriginalData$DT, exclude = NA));DocumentTypeMerger
names(DocumentTypeMerger) <- c("DocumentType", "Count")

# Count the number of time each document type appear in ScopWosnotexclusive
DocumentTypedup <- data.frame(table(ScopWosnotexclusive$DT, exclude = ""))
DocumentTypedup <- data.frame(table(ScopWosnotexclusive$DT, exclude = NA));DocumentTypedup
names(DocumentTypedup) <- c("DocumentType", "Count")

# Count the number of time each document type appear in ScopWosnotexclusive (duplicates between Scopus and Web of Science)
DocumentTypeIFSMS <- data.frame(table(IFSMS$Document.Type, exclude = ""))
DocumentTypeIFSMS <- data.frame(table(IFSMS$Document.Type, exclude = NA));DocumentTypeIFSMS
names(DocumentTypeIFSMS) <- c("DocumentType", "Count")

# Exportation - data for Table 1
# Create a blank row between Scopus and Web of Science results, after corrections
DocumentTypeScop[nrow(DocumentTypeScop)+1,] <- NA
DocumentTypeWoS[nrow(DocumentTypeWoS)+1,] <- NA
DocumentTypeCombined[nrow(DocumentTypeCombined)+1,] <- NA
DocumentTypeMerger[nrow(DocumentTypeMerger)+1,] <- NA
DocumentTypedup[nrow(DocumentTypedup)+1,] <- NA
DocumentTypeScopWoS <- bind_rows(DocumentTypeScop, DocumentTypeWoS, DocumentTypeCombined, DocumentTypeMerger,DocumentTypedup, DocumentTypeIFSMS)
write.table(DocumentTypeScopWoS, file = paste0(Results.dir,"Result_Document type_ScopWoS_Table 1.txt"), sep = "\t", row.names = F)

#####################################################################
#####              Scopus/Web of Science/ IFSMS                 #####
#####################################################################

# Data from ScopWos and WoS after exclusion : MergerOriginalData
MergerOriginalDataTemp <- MergerOriginalData %>%
  select(AU, TI, PY, SO, DT, Coder)
CombinedDatasetDataTemp <- CombinedDataset %>%
  select(AU, TI, PY, SO, DT, Coder)

# Label each row with the database it come from 
MergerOriginalDataTemp$Coder2 <- "ScopWosexclusion"
CombinedDatasetDataTemp$Coder2 <- "ScopWos"
IFSMS$Coder2 <- "Interpol"
IFSMS$Title <- toupper(IFSMS$Title)

####______Creating a list of document present in Interpol but not in ScopWos______####
# Creating a list from ScopWos Title
ScopWosTitleList <- MergerOriginalDataTemp %>%
  select(TI)
ScopWosTitleList2 <- CombinedDatasetDataTemp %>%
  select(TI)

# List of records from Interpol that are in the MergerOriginalData (Title based)
IFSMSNotexclusive <- subset(IFSMS,Title %in% ScopWosTitleList$TI)

# List of records from Interpol that are not in the MergerOriginalData (Title based)
IFSMSexclusive <- setdiff(IFSMS,IFSMSNotexclusive)

# List of records from Interpol that are in the CombinedDataset (Title based)
IFSMSNotexclusive2 <- subset(IFSMS,Title %in% ScopWosTitleList2$TI)

# List of records from Interpol that are not in the CombinedDataset (Title based)
IFSMSexclusive2 <- setdiff(IFSMS,IFSMSNotexclusive2)

####______Calculating the % of record present in Interpol but not in ScopWos______####
# Total number of record in the excluded list
CountIFSMSexclusive <- as.numeric(count(IFSMSexclusive));CountIFSMSexclusive
CountIFSMSexclusive2 <- as.numeric(count(IFSMSexclusive2));CountIFSMSexclusive2

# Number Total of record on Interpol
TotalIFSMS <- as.numeric(count(IFSMS))

# % of record from Interpol present in ScopWos
CountIFSMSexclusive/TotalIFSMS*100 

# In IFSMSNotexclusive, how many are exclusive to WoS ?
# Creating a list from IFSMS Title
IFSMSTitleList <- IFSMSNotexclusive %>%
  select(Title)

#####______________Graph______________##########
# to not overwrite data
ScopusCorrected <- ScopusReducedDatasetTIAUC1SIDSSODTcor
WoSCorrected <- WebOfScienceReducedDatasetAUSODTcor

ScopusCorrected <- ScopusCorrected %>%
  select(PY,TI,DT,Coder)
WoSCorrected <- WoSCorrected %>%
  select(PY,TI,DT,Coder)

ScopusCorrected <- subset(ScopusCorrected, !PY <1999 & !PY > 2018)
WoSCorrected <- subset(WoSCorrected, !PY <1999 & !PY > 2018)

IFSMSTemp <- IFSMS %>%
  select(Year,Title,Document.Type,Coder2)

# Change the column name
names(IFSMSTemp) <- c("PY", "TI", "DT", "Coder")

#Count to number of time the same year is repeated in the "document$Year" and save in a data.frame "Year" 
yearScopus <- data.frame(table(ScopusCorrected$PY));yearScopus
yearScopus$Var1 <- as.numeric(as.character(yearScopus$Var1))
names(yearScopus) <- c("Year","Total")
yearScopus$Coder <- "Scopus"

yearWoS <- data.frame(table(WoSCorrected$PY));yearWoS
yearWoS$Var1 <- as.numeric(as.character(yearWoS$Var1))
names(yearWoS) <- c("Year","Total")
yearWoS$Coder <- "WebOfScience"


yearIFSMS <- data.frame(table(IFSMSTemp$PY));yearIFSMS
yearIFSMS$Var1 <- as.numeric(as.character(yearIFSMS$Var1))
names(yearIFSMS) <- c("Year","Total")
yearIFSMS$Coder <- "IFSMS"

#to plot
toplot <- data.frame(rbind(yearScopus,yearWoS,yearIFSMS))


# Graph - Figure 5
plot <- ggplot(data=toplot, aes(x=Year, y=Total, color=Coder)) +
  geom_line(aes(linetype=Coder), size=0.8)+
  scale_linetype_manual(values=c("solid","solid", "solid"))+
  scale_color_manual(values=c("gray65", "black", "dodgerblue3"))+
  xlab('Year') +
  ylab('Documents') +
  scale_x_continuous(breaks=c(1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  theme_classic(base_family = "Arial", base_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",size=1, linetype="solid", colour="grey80"))
show(plot)
ggsave("Result_ScopWoSIFSMS_DocTrend.png", plot, width = 11, height = 8, units = "in", dpi=500, path = "Results")
