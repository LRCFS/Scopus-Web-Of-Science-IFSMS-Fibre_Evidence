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

# # or for the reduced dataset for fibre/textile in forensic science
# ScopWosnotexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus,WebOfScience"|MergerOriginalData$Coder == "WebOfScience,Scopus"|MergerOriginalData$Coder == "Scopus,Scopus,WebOfScience"|MergerOriginalData$Coder == "Scopus,WebOfScience,WebOfScience", ]
# Scopusexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus"|MergerOriginalData$Coder == "Scopus,Scopus", ]
# WoSexclusive <- MergerOriginalData[MergerOriginalData$Coder == "WebOfScience", ]

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
JournalsCombinedDataset <- data.frame(table(ScopWosnotexclusive$SO, exclude = ""))
JournalsCombinedDataset <- data.frame(table(ScopWosnotexclusive$SO, exclude = NA));JournalsCombinedDataset
names(JournalsCombinedDataset) <- c("Journals", "Count")

# this table combined the results
JournalWoSAll <- full_join(JournalsCombinedDataset,JournalWoS)
JournalTotal <- full_join(JournalWoSAll,JournalsScop)

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
TopJournal$Journals <- gsub("PROCEEDINGS OF SPIE - THE INTERNATIONAL SOCIETY FOR OPTICAL ENGINEERING","PROC. SPIE -\nINT. SOC. OPT. ENG.", TopJournal$Journals)
TopJournal$Journals <- gsub("AMERICAN JOURNAL OF FORENSIC MEDICINE AND PATHOLOGY","AMERICAN JOURNAL\nOF FORENSIC MEDICINE\nAND PATHOLOGY", TopJournal$Journals)
TopJournal$Journals <- gsub("FORENSIC SCIENCE INTERNATIONAL","FORENSIC SCIENCE\nINTERNATIONAL", TopJournal$Journals)
TopJournal$Journals <- gsub("APPLIED SPECTROSCOPY","APPLIED\nSPECTROSCOPY", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF CHROMATOGRAPHY A","JOURNAL OF\nCHROMATOGRAPHY A", TopJournal$Journals)
TopJournal$Journals <- gsub("ARCHIV FUR KRIMINOLOGIE","ARCHIV FUR\nKRIMINOLOGIE", TopJournal$Journals)
TopJournal$Journals <- gsub("Z ZAGADNIEN NAUK SADOWYCH","Z ZAGADNIEN\nNAUK SADOWYCH", TopJournal$Journals)
TopJournal$Journals <- gsub("ZEITSCHRIFT FR RECHTSMEDIZIN","ZEITSCHRIFT\nFR RECHTSMEDIZIN", TopJournal$Journals)
TopJournal$Journals <- gsub("SCIENCE AND JUSTICE","SCIENCE\nAND JUSTICE", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF FORENSIC SCIENCES","JOURNAL OF\nFORENSIC SCIENCES", TopJournal$Journals)
TopJournal$Journals <- gsub("INTERNATIONAL JOURNAL OF LEGAL MEDICINE","INTERNATIONAL\nJOURNAL\nOF LEGAL MEDICINE", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF THE FORENSIC SCIENCE SOCIETY","JOURNAL\nOF THE FORENSIC\nSCIENCE SOCIETY", TopJournal$Journals)
TopJournal$Journals <- gsub("CHINESE JOURNAL OF FORENSIC MEDICINE","CHINESE JOURNAL OF\nFORENSIC MEDICINE", TopJournal$Journals)
TopJournal$Journals <- gsub("ITCANDDC: 5TH INTERNATIONAL TEXTILE, CLOTHING AND DESIGN CONFERENCE 2010, BOOK OF PROCEEDINGS: MAGIC WORLD OF TEXTILES",
                            "5TH ITCANDDC 2010", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF CHROMATOGRAPHY B-ANALYTICAL TECHNOLOGIES IN THE BIOMEDICAL AND LIFE SCIENCES","JOURNAL OF CHROMATOGRAPHY B", TopJournal$Journals)

# take difference of reference counts
# and make long
# run for full list
forOverlapPlotTempFullList <- gather(TopJournal, Database, Frequency, ScopWos:ScopusExclusive, factor_key=TRUE)

# add a list for facet_wrap when combining the two figures
forOverlapPlotTempFullList$list <- c("Complete Citation databases")

# GRAPH - Figure 1
plotoverlap <- ggplot(forOverlapPlotTempFullList , aes(x = reorder(Journals,-Total), y = Frequency, fill = Database)) + 
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

##################################################################################
#####                            Reduced Dataset                             #####
##################################################################################

# # or for the reduced dataset for fibre/textile in forensic science
ScopWosnotexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus,WebOfScience"|MergerOriginalData$Coder == "WebOfScience,Scopus"|MergerOriginalData$Coder == "Scopus,Scopus,WebOfScience"|MergerOriginalData$Coder == "Scopus,WebOfScience,WebOfScience", ]
Scopusexclusive <- MergerOriginalData[MergerOriginalData$Coder == "Scopus"|MergerOriginalData$Coder == "Scopus,Scopus", ]
WoSexclusive <- MergerOriginalData[MergerOriginalData$Coder == "WebOfScience", ]

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
JournalsCombinedDataset <- data.frame(table(ScopWosnotexclusive$SO, exclude = ""))
JournalsCombinedDataset <- data.frame(table(ScopWosnotexclusive$SO, exclude = NA));JournalsCombinedDataset
names(JournalsCombinedDataset) <- c("Journals", "Count")

# this table combined the results
JournalWoSAll <- full_join(JournalsCombinedDataset,JournalWoS)
JournalTotal <- full_join(JournalWoSAll,JournalsScop)


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
TopJournal$Journals <- gsub("PROCEEDINGS OF SPIE - THE INTERNATIONAL SOCIETY FOR OPTICAL ENGINEERING","PROC. SPIE -\nINT. SOC. OPT. ENG.", TopJournal$Journals)
TopJournal$Journals <- gsub("AMERICAN JOURNAL OF FORENSIC MEDICINE AND PATHOLOGY","AMERICAN JOURNAL\nOF FORENSIC MEDICINE\nAND PATHOLOGY", TopJournal$Journals)
TopJournal$Journals <- gsub("FORENSIC SCIENCE INTERNATIONAL","FORENSIC SCIENCE\nINTERNATIONAL", TopJournal$Journals)
TopJournal$Journals <- gsub("APPLIED SPECTROSCOPY","APPLIED\nSPECTROSCOPY", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF CHROMATOGRAPHY A","JOURNAL OF\nCHROMATOGRAPHY A", TopJournal$Journals)
TopJournal$Journals <- gsub("ARCHIV FUR KRIMINOLOGIE","ARCHIV FUR\nKRIMINOLOGIE", TopJournal$Journals)
TopJournal$Journals <- gsub("Z ZAGADNIEN NAUK SADOWYCH","Z ZAGADNIEN\nNAUK SADOWYCH", TopJournal$Journals)
TopJournal$Journals <- gsub("ZEITSCHRIFT FR RECHTSMEDIZIN","ZEITSCHRIFT\nFR RECHTSMEDIZIN", TopJournal$Journals)
TopJournal$Journals <- gsub("SCIENCE AND JUSTICE","SCIENCE\nAND JUSTICE", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF FORENSIC SCIENCES","JOURNAL OF\nFORENSIC SCIENCES", TopJournal$Journals)
TopJournal$Journals <- gsub("INTERNATIONAL JOURNAL OF LEGAL MEDICINE","INTERNATIONAL\nJOURNAL\nOF LEGAL MEDICINE", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF THE FORENSIC SCIENCE SOCIETY","JOURNAL\nOF THE FORENSIC\nSCIENCE SOCIETY", TopJournal$Journals)
TopJournal$Journals <- gsub("CHINESE JOURNAL OF FORENSIC MEDICINE","CHINESE JOURNAL OF\nFORENSIC MEDICINE", TopJournal$Journals)
TopJournal$Journals <- gsub("ITCANDDC: 5TH INTERNATIONAL TEXTILE, CLOTHING AND DESIGN CONFERENCE 2010, BOOK OF PROCEEDINGS: MAGIC WORLD OF TEXTILES",
                            "5TH ITCANDDC 2010", TopJournal$Journals)
TopJournal$Journals <- gsub("JOURNAL OF CHROMATOGRAPHY B-ANALYTICAL TECHNOLOGIES IN THE BIOMEDICAL AND LIFE SCIENCES","JOURNAL OF CHROMATOGRAPHY B", TopJournal$Journals)

# take difference of reference counts
# and make long
# run for reduce list
forOverlapPlotTempReduceList <- gather(TopJournal, Database, Frequency, ScopWos:ScopusExclusive, factor_key=TRUE)
# names(forOverlapPlotTempReduceList)[4] <- c("Frequency")
# names(forOverlapPlotTempReduceList)[2] <- c("Total")

forOverlapPlotTempReduceList$list <- c("Fibre as evidence type")

# extract the top list of journals from the full list 
forOverlapPlotTempFullListNarrow <- forOverlapPlotTempFullList %>%
  select(Journals) %>%
  distinct()

forOverlapPlotTempReduceListNarrow <-subset(forOverlapPlotTempReduceList,Journals %in% forOverlapPlotTempFullListNarrow$Journals)


# combine the two lists
OrderTitleTemp <- full_join(forOverlapPlotTempFullList,forOverlapPlotTempReduceListNarrow)


# GRAPH - Figure 1
plotoverlap <- ggplot(OrderTitleTemp , aes(x = reorder(Journals,-Total), y = Frequency, fill = Database)) + 
  geom_bar(position = position_dodge(0.8),
           stat = "identity",
           width =0.8) + 
  facet_wrap( ~list)+
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
ggsave("Journals_Full_Reduced.png", plotoverlap, width = unit(16.5, 'in'), height = unit(15, 'in'), dpi=300, path = "Results")
