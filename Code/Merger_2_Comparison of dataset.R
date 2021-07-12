#############################################################
#####                     To read                       #####
#############################################################
# This R script is for blablablabla
# It can be choosen to work with the combined dataset before the exclusion process (CombinedDataset)
# Or it can be choosen to work with the combined dataset after the exclusion process (MergerOriginalData)

##################################################################################
#####              Comparison Scopus/Web Of Science all year                 #####
##################################################################################
 
#####______________creation of dataset______________##########
# In the previous dataset, the column Coder can be used to calculate the pourcentage of articles both present in Scopus and in WoS
# A selection between CombinedDataset or MergerOriginalData must be doner here and will be applied for the rest of the code
# Scopus not exclusive = Web of science not exclusive
ScopWosnotexclusive <- CombinedDataset[CombinedDataset$Coder == "Scopus,WebOfScience"|CombinedDataset$Coder == "WebOfScience,Scopus"|CombinedDataset$Coder == "Scopus,Scopus,WebOfScience"|CombinedDataset$Coder == "Scopus,WebOfScience,WebOfScience", ]
Scopusexclusive <- CombinedDataset[CombinedDataset$Coder == "Scopus"|CombinedDataset$Coder == "Scopus,Scopus", ]
WoSexclusive <- CombinedDataset[CombinedDataset$Coder == "WebOfScience", ]
# To calculate the pourcentage of possibles errors
#forErrors <- rbind(ScopWosnotexclusive,Scopusexclusive,WoSexclusive)
#errorpourcentage <- setdiff(CombinedDataset,forErrors)
 
 
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

# Count the number of time each Journals appear in Scopus, after corrections
JournalsScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$SO, exclude = ""))
JournalsScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSSODTcor$SO, exclude = NA));JournalsScop
names(JournalsScop) <- c("Journals", "Count")

# Count the number of time each Journals appear in Web Of Science, after corrections
JournalWoS <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$SO, exclude = ""))
JournalWoS <- data.frame(table(WebOfScienceReducedDatasetAUSODTcor$SO, exclude = NA));JournalWoS
names(JournalWoS) <- c("Journals", "Count")

# Count the number of time each Journals appear in Web Of Science, after corrections
JournalDup <- data.frame(table(ScopWosnotexclusive$SO, exclude = ""))
JournalDup <- data.frame(table(ScopWosnotexclusive$SO, exclude = NA));JournalDup
names(JournalDup) <- c("Journals", "Count")

# Select the top 20 in each Dataset
TopJournalsScop <- top_n(JournalsScop, 15, Count)
TopJournalsScop <- TopJournalsScop[order(-TopJournalsScop$Count),]
names(TopJournalsScop) <- c("Title","Frequency")

TopJournalsWoS <- top_n(JournalWoS, 15, Count)
TopJournalsWoS <- TopJournalsWoS[order(-TopJournalsWoS$Count),]
names(TopJournalsWoS) <- c("Title","Frequency")

TopJournalsDup <- top_n(JournalDup, 15, Count)
TopJournalsDup <- TopJournalsDup[order(-TopJournalsDup$Count),]
names(TopJournalsDup) <- c("Title","Frequency")

# create a dataframe with data from Scopus and Web Of Science
TopJournalsScop$Coder <- "Scopus"
TopJournalsWoS$Coder <- "WebOfScience"
TopJournalScopWoS <- rbind(TopJournalsScop, TopJournalsWoS)

TopJournalScopWoS$Frequency <- with(TopJournalScopWoS, ifelse(Coder == "Scopus", -Frequency, Frequency))

Plot <- ggplot(data = TopJournalScopWoS,  aes(x = Title, y = Frequency, fill = Coder)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs)+
  scale_fill_brewer(palette = "Pastel1") +
  labs(x="", y="Number of references")+
  theme_bw()
Plot
#ggplotly(Plot)
ggsave("Journals.png", Plot, width = 12, height = 10, units = "in", dpi=300, path = "Results")

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

# Count the number of time each document type appear in CombinedDataset
DocumentTypeMerger <- data.frame(table(CombinedDataset$DT, exclude = ""))
DocumentTypeMerger <- data.frame(table(CombinedDataset$DT, exclude = NA));DocumentTypeMerger
names(DocumentTypeMerger) <- c("DocumentType", "Count")

# Count the number of time each document type appear in ScopWosnotexclusive
DocumentTypedup <- data.frame(table(ScopWosnotexclusive$DT, exclude = ""))
DocumentTypedup <- data.frame(table(ScopWosnotexclusive$DT, exclude = NA));DocumentTypedup
names(DocumentTypedup) <- c("DocumentType", "Count")

# Count the number of time each document type appear in ScopWosnotexclusive (duplicates between Scopus and Web of Science)
DocumentTypeIFSMS <- data.frame(table(IFSMS$Document.Type, exclude = ""))
DocumentTypeIFSMS <- data.frame(table(IFSMS$Document.Type, exclude = NA));DocumentTypeIFSMS
names(DocumentTypeIFSMS) <- c("DocumentType", "Count")

# Exportation
# Create a blank row between Scopus and Web of Science results, after corrections
DocumentTypeScop[nrow(DocumentTypeScop)+1,] <- NA
DocumentTypeWoS[nrow(DocumentTypeWoS)+1,] <- NA
DocumentTypeCombined[nrow(DocumentTypeCombined)+1,] <- NA
DocumentTypeMerger[nrow(DocumentTypeMerger)+1,] <- NA
DocumentTypedup[nrow(DocumentTypedup)+1,] <- NA
DocumentTypeScopWoS <- bind_rows(DocumentTypeScop, DocumentTypeWoS, DocumentTypeCombined, DocumentTypeMerger,DocumentTypedup, DocumentTypeIFSMS)
#write.table(DocumentTypeScopWoS, file = paste0(Results.dir,"Result_Document type_ScopWoS.txt"), sep = "\t", row.names = F)

#####################################################################
#####              Scopus/Web of Science/ IFSMS                 #####
#####################################################################

# Data from ScopWos and WoS after exclusion : MergerOriginalData
MergerOriginalDataTemp <- MergerOriginalData %>%
  select(AU, TI, PY, SO, DT, Coder)

# Label each row with the database it come from 
MergerOriginalDataTemp$Coder2 <- "ScopWos"
IFSMS$Coder2 <- "Interpol"
IFSMS$Title <- toupper(IFSMS$Title)

####______Creating a list of document present in Interpol but not in ScopWos______####
# Creating a list from ScopWos Title
ScopWosTitleList <- MergerOriginalDataTemp %>%
  select(TI)

# List of records from Interpol that are in the MergerOriginalData (Title based)
IFSMSNotexclusive <- subset(IFSMS,Title %in% ScopWosTitleList$TI)

# List of records from Interpol that are not in the MergerOriginalData (Title based)
IFSMSexclusive <- setdiff(IFSMS,IFSMSNotexclusive)


####______Calculating the % of record present in Interpol but not in ScopWos______####
# Total number of record in the excluded list
CountIFSMSexclusive <- as.numeric(count(IFSMSexclusive));CountIFSMSexclusive

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


# GRAPH
plot <- ggplot(data=toplot, aes(x=Year, y=Total, color=Coder)) +
  geom_line(aes(linetype=Coder), size=0.8)+
  scale_linetype_manual(values=c("solid","solid", "dashed"))+
  scale_color_manual(values=c("black", "darkblue", "grey50"))+
  xlab('Year') +
  ylab('Documents') +
  scale_x_continuous(breaks=c(1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  theme_classic(base_family = "Arial", base_size = 20)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="white",size=1, linetype="solid", colour="grey80"))
plot
ggsave("Result_ScopWoSIFSMS_DocTrend.png", plot, width = 11, height = 8, units = "in", dpi=500, path = "Results")
