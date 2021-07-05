#####_________Search for match by title between the two datasets_________##########
# To generate a list of titles with a partial match for external check

#matches=partialMatch(WebOfScienceReducedDataset$TI,ScopusReducedDataset$TI)

#aggregate(matches$pass, by=list(matches$pass), FUN=length)

#PartialExport <- matches %>% filter(pass == "Partial")

# The PartialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
#write.table(PartialExport, file = "PartialExport_December.txt", quote = F, sep="\t", row.names = F)

# Correction to the title can be applied at this stage. This can be done in Notepad++, Excel etc.
# The title generated in Web of Science will be used to correct the one in Scopus
# Note this does not mean all the title from Web of Science are correct !!!
# Missed duplicate or partial can be added as well
# Applying the title correction does not necessary mean there will not be further partial match is the function is rerun on the new (corrected) Scopus dataframe. 




#####_________Search for match by Source between the two datasets_________##########
##### to generate a list of sources with a partial match for external check

#matches2=partialMatch(WebOfScienceReducedDatasetAUCor$SO,ScopusReducedDatasetCorrected$SO)

#aggregate(matches2$pass, by=list(matches2$pass), FUN=length)

#PartialExport2 <- matches2 %>% filter(pass == "Partial")

# The PartialExport can be written to a table and further processed manually using fo example Notepad++, Excel, etc.
write.table(PartialExport2, file = "PartialExport_SourceTest.txt", quote = F, sep="\t", row.names = F)



# Duplicate check on the output:
DupeScopus2 <- ScopusReducedDatasetTIAUC1SIDScor %>%
  find_duplicates(PY,TI)







#####_________Second step : correction on false duplicates_________##########
# Issues with some references in the Coder column. Some references will have :
# "Scopus,Scopus" -> this mean that 2 references are identify as duplicates in Scopus and need to be check manually
# "WebOfScience,WebOfScience" -> this mean that 2 references are identify as duplicates in Web of Science and need to be check manually
# "Scopus,WebOfScience,WebOfScience" -> this mean that 3 references are identify as one unique ref and need to be check manually
# "Scopus,Scopus,WebOfScience" -> this mean that 3 references are identify as one unique ref and need to be check manually
# As a example in this dataset, THE EVIDENTIAL VALUE OF BLACK COTTON FIBRES published in 2001 in present in Web of Science (ARTICLE) and in Scopus (ARTICLE and CONFERENCE PAPER)
# Only the document ARTICLE in Scopus and Web of Science are duplicates, the CONFERENCE PAPER in Scopus is an exclusive reference
# This is due to a missing entry in the DI column (DOI) for the CONFERENCE PAPER in Scopus which is therefore considered as the same as the ARTICLE
# This can be fixed manually by including again in the CombinedDataset the references
WebOfScienceReducedDatasetCorrected <- WebOfScienceReducedDatasetCorrected[order(WebOfScienceReducedDatasetCorrected$TI),]
WebOfScienceReducedDatasetCorrected <- WebOfScienceReducedDatasetCorrected %>%
  select(PY,TI,Authors,AuthorCorrected,DI,DEW,IDW,C1W,SO,DT,Coder)

ScopusReducedDatasetCorrected <- ScopusReducedDatasetCorrected[order(ScopusReducedDatasetCorrected$TI),]
ScopusReducedDatasetCorrected <- ScopusReducedDatasetCorrected %>%
  select(PY,TI,Authors,AuthorCorrected,DI,DES,IDS,C1S,SO,DT,Coder)

#write.table(WebOfScienceReducedDatasetCorrected, file = "WebOfScienceReducedDatasetCorrected.txt", quote = F, sep="\t", row.names = F)
#write.table(ScopusReducedDatasetCorrected, file = "ScopusReducedDatasetCorrected.txt", quote = F, sep="\t", row.names = F)

# Delete previous entries with "Scopus,Scopus", "WebOfScience,WebOfScience", "Scopus,WebOfScience,WebOfScience" or "Scopus,Scopus,WebOfScience"
CombinedDataset<-CombinedDataset[!(CombinedDataset$Coder=="Scopus,Scopus" |
                                     CombinedDataset$Coder=="WebOfScience,WebOfScience" |
                                     CombinedDataset$Coder=="Scopus,WebOfScience,WebOfScience" |
                                     CombinedDataset$Coder=="Scopus,Scopus,WebOfScience"),]
# Add entries again
RecreatedEntries <- read.csv("Entries recreated.txt", sep = "\t", header = TRUE)
RecreatedEntries <- RecreatedEntries %>%
  select(TI,PY,AU,DOI,DEW,IDW,DES,IDS,C1W,C1S,Coder,DT,SO)

CombinedDataset <- rbind(RecreatedEntries,CombinedDataset)

#####_________Correction on DT_________##########
# Differences may remain between Web of Science and Scopus (i.e. "Article" in Scopus and "Review" in WoS)
# when two references are found both in Scopus and in WoS, the document type provided by Scopus is used
# split column DT based on ";"
CombinedDataset2 <- CombinedDataset %>%
  separate(DT, c("DT", "DT2"), ";") 
# the column DT correspond to the DT from Scopus when the coder is "Scopus" and "Scopus,WebOfScience",
# the column DT correspond to the DT from WoS when the coder is "WebOfScience",
# the column DT2 correspond to the DT from WoS when the coder is "Scopus,WebOfScience"

# Remove DT2 
CombinedDataset <- CombinedDataset2 %>%
  select(PY,TI,AU,DOI,DEW,DES,IDW,IDS,C1W,C1S,SO,DT,Coder)

#####_________Correction on SO_________##########
# in the column SO, So provided by Scopus and So provided by WoS are join by ";" when two references are found in both database
# In that situation, So provided by Scopus is used
#split column SO based on ";"
CombinedDataset3 <- CombinedDataset %>%
  separate(SO, c("SO", "SO2"), ";")
# the column SO correspond to the SO from Scopus when the coder is "Scopus" and "Scopus,WebOfScience",
# the column SO correspond to the SO from WoS when the coder is "WebOfScience",
# the column SO2 correspond to the SO from WoS when the coder is "Scopus,WebOfScience"

# Remove SO2 
CombinedDataset <- CombinedDataset3 %>%
  select(PY,TI,AU,DOI,DEW,DES,IDW,IDS,C1W,C1S,SO,DT,Coder)

#####_________Correction on AU_________##########
# In the Authors column, because the number of authors is sometimes different between Scopus and Web of Science, a correction must be done
CombinedDataset <- CombinedDataset %>%
  mutate(AU = strsplit(as.character(AU), ";"))%>%
  unnest(AU) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(TI,PY,DEW,IDW,DES,IDS,C1W,C1S,DOI,SO,DT,Coder) %>%
  summarise(AU = sort(paste(AU, collapse= ";")))%>%
  ungroup()
CombinedDataset$AU[CombinedDataset$AU==""]<-NA


#####_________Correction on Affiliation_________##########
# Filling Affiliations columns empty string with NA
CombinedDataset$C1S[CombinedDataset$C1S==""]<-NA
CombinedDataset$C1W[CombinedDataset$C1W==""]<-NA
# If Scopus has no affiliations entries, WEb of Science is used as altenative including no entries.
TempScopusAffiliations <- CombinedDataset[!is.na(CombinedDataset$C1S),] %>%
  mutate(C1 = C1S)

TempWebOfSAffiliations <- CombinedDataset[is.na(CombinedDataset$C1S),] %>%
  mutate(C1 = C1W)

CombinedDataset <- rbind(TempScopusAffiliations,TempWebOfSAffiliations)


# Check for duplicate in the combined datasets
DupeCombinedDataset <- CombinedDataset %>%
  find_duplicates(TI) # if duplicates in DupeCombinedDataset does not have the same year, they are not duplicates

#####_________Correction on Authors' keywords_________##########
# The Authors' keywords (i.e. DEW and DES) between the two lists should be the same, however it is not the case and some correction will be needed.
# The user can decide to use the original list of keywords given by the databases instead, adding "S" or "W" to "DE"
# The merged list (i.e. DEW +DES) is more exhautive than the individual one as for some records as the keywords do not appear in both lists
# It is best to remove  the white space present with the ";"
CombinedDataset <- transform(CombinedDataset, DE=paste(DEW, DES, sep="; "))
CombinedDataset$DE  <- as.character(gsub("; ",";",CombinedDataset$DE))
CombinedDataset <- CombinedDataset %>%
  select(TI,PY,AU,DOI,DEW,IDW,DES,IDS,C1W,C1S,C1,DT,SO,C1,DE,DOI,Coder)

# In the DE column, because the order of the author's keyword is sometimes different between Scopus and Web of Science, a correction has been done
# It has been chosen to remove each keywords that were duplicated in the column DE
CombinedDataset <- CombinedDataset %>%
  mutate(DE = strsplit(as.character(DE), ";"))%>%
  unnest(DE) %>%
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  group_by(TI,PY,AU,DEW,IDW,DES,IDS,C1W,C1,C1S,DOI,SO,DT,Coder) %>%
  summarise(DE = sort(paste(DE, collapse= ";")))%>%
  ungroup()

# To export the Data before the exclusion process
write.table(CombinedDataset, file = "ScopWos merge_December.txt", sep = "\t", row.names = F)



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
