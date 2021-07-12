#############################################################
#####                     To read                       #####
#############################################################
# This R script is for blablablabla

#################################################################################
#####              Comparison Scopus/Web Of Science all year                 #####
##################################################################################
 
#####______________creation of dataset______________##########
# In the previous dataset, the column Coder can be used to calculate the pourcentage of articles both present in Scopus and in WoS
#before of after the exclusion process (CombinedDataset or MergerOriginalData)
#Scopus not exclusive = Web of science not exclusive
ScopWosnotexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus,WebOfScience"|MergerOriginalData$Coder == "WebOfScience,Scopus"|MergerOriginalData$Coder == "Scopus,Scopus,WebOfScience"|MergerOriginalData$Coder == "Scopus,WebOfScience,WebOfScience", ]
Scopusexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus"|MergerOriginalData$Coder == "Scopus,Scopus", ]
WoSexclusive <- MergerOriginalData[MergerOriginalData$Coder == "WebOfScience", ]
# To calculate the pourcentage of possibles errors
#forErrors <- rbind(ScopWosnotexclusive,Scopusexclusive,WoSexclusive)
#errorpourcentage <- setdiff(MergerOriginalData,forErrors)
 
 
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
JournalsScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSDTcor$SO, exclude = ""))
JournalsScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSDTcor$SO, exclude = NA));JournalsScop
names(JournalsScop) <- c("Journals", "Count")

# Count the number of time each Journals appear in Web Of Science, after corrections
JournalWoS <- data.frame(table(WebOfScienceReducedDatasetAUDTSODTcor$SO, exclude = ""))
JournalWoS <- data.frame(table(WebOfScienceReducedDatasetAUDTSODTcor$SO, exclude = NA));JournalWoS
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

# create a dataframe with data from Scopus and Web Of Science
TopJournalsScop$Coder <- "Scopus"
TopJournalsWoS$Coder <- "WebOfScience"
TopJournalScopWoS <- rbind(TopJournalsScop, TopJournalsWoS)

TopJournalScopWoS$Frequency <- with(TopJournalScopWoS, ifelse(Coder == "Scopus", Frequency, Frequency))

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
DocumentTypeScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSDTcor$DT, exclude = ""))
DocumentTypeScop <- data.frame(table(ScopusReducedDatasetTIAUC1SIDSDTcor$DT, exclude = NA));DocumentTypeScop
names(DocumentTypeScop) <- c("DocumentType", "Count")

# Count the number of time each document type appear in Web of Science
DocumentTypeWoS <- data.frame(table(WebOfScienceReducedDatasetAUDTSODTcor$DT, exclude = ""))
DocumentTypeWoS <- data.frame(table(WebOfScienceReducedDatasetAUDTSODTcor$DT, exclude = NA));DocumentTypeWoS
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

# Exportation
# Create a blank row between Scopus and Web of Science results, after corrections
DocumentTypeScop[nrow(DocumentTypeScop)+1,] <- NA
DocumentTypeWoS[nrow(DocumentTypeWoS)+1,] <- NA
DocumentTypeCombined[nrow(DocumentTypeCombined)+1,] <- NA
DocumentTypeMerger[nrow(DocumentTypeMerger)+1,] <- NA
DocumentTypeScopWoS <- bind_rows(DocumentTypeScop, DocumentTypeWoS, DocumentTypeCombined, DocumentTypeMerger,DocumentTypedup)
write.table(DocumentTypeScopWoS, file = paste0(Results.dir,"Result_Document type_ScopWoS.txt"), sep = "\t", row.names = F)