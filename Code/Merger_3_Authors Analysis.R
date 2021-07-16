#############################################################
#####                     To read                       #####
#############################################################
# This R script is the third step after merging Scopus and Web of Sciences (BibTex format)
# This script allows a bibliometric analysis on the authors


#######################################################################
#####                         Data loading                        #####
#######################################################################

# read the export *.csv document from Merger, separation "\t", and place it in data.frame "MergerOriginalData"
MergerOriginalData <- read.csv(paste0(Results.dir,"Result_Merger_Dataset.txt"), sep="\t", header=TRUE)
IFSMS <- read.csv(paste0(Results.dir,"Result_IFSMS_Dataset.txt"), sep="\t", header=TRUE)


#########################################################################
#####                         Merger Dataset                        #####
#########################################################################
#############################################################
#####                  Data cleansing                   #####
#############################################################

# Replace each "," with ";" in MergerOriginalData
AuthorList <- MergerOriginalData %>%
  select(PY,TI,AU,C1S)
# Change the column name
names(AuthorList) <- c("Year", "Title", "Authors", "Affiliation")

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListExtended <- AuthorList %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)%>%
  distinct()

# Remove entry that have no authors name
AuthorListExtended <- AuthorListExtended[-grep("NA NA", AuthorListExtended$Authors),]

# Count to number of time the same year is repeated in the "AuthorListExtended$Year" and save in a data.frame "Year" 
PublicationYear <- aggregate(AuthorList$Authors,list(AuthorList$Year), FUN=length)
names(PublicationYear) <- c("Year","Publications")

# add missing year to PublicationYear
DFfilledPublicationYear <- PublicationYear %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
PublicationYear <- DFfilledPublicationYear

# Count to number of time the same author is repeated in "AuthorListExtended$Authors" and save it in "AuthorCountPaper"
AuthorCountPaper <- aggregate(AuthorListExtended$Authors, list(AuthorListExtended$Authors), FUN=length)
names(AuthorCountPaper) <- c("Author","Frequency")

# Number of Authors per year
NumberAuthorYear <- aggregate(AuthorListExtended$Authors,list(AuthorListExtended$Year), FUN=length)
names(NumberAuthorYear) <- c("Year","Author")

# add missing year to NumberAuthorYear
DFfilledNumberAuthorYear <- NumberAuthorYear %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
NumberAuthorYear <- DFfilledNumberAuthorYear

# List of "Author" with one publication only
AuthorCountSinglePaper <- subset(AuthorCountPaper,Frequency<2)
AuthorCountSinglePaperReduced <-subset(AuthorListExtended,Authors %in% AuthorCountSinglePaper$Author)
sum(AuthorCountSinglePaper$Frequency)

# List of "Author" with one publication only and their publication "Year"
YearNewAuthorSinglePaper <- aggregate(AuthorCountSinglePaperReduced$Authors,list(AuthorCountSinglePaperReduced$Year), FUN=length)
names(YearNewAuthorSinglePaper) <- c("Year","Single Author")

# add missing year to NumberAuthorYear
DFfilledYearNewAuthorSinglePaper <- YearNewAuthorSinglePaper %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
YearNewAuthorSinglePaper <- DFfilledYearNewAuthorSinglePaper

# List of Authors with multiple publications only
AuthorCountMultiplePaper <- subset(AuthorCountPaper,Frequency>1)
AuthorCountMultiplePaperReduced <- subset(AuthorListExtended, Authors %in% AuthorCountMultiplePaper$Author)
sum(AuthorCountMultiplePaper$Frequency)

# List of "Authors" with multiple output and their first "Year"
AuthorFirstAppearanceMultipleEntry<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$Authors), min)
names(AuthorFirstAppearanceMultipleEntry) <- c("Author","Year")

# List of "Authors" with multiple output and their last "Year"
AuthorMultipleOutputLastYear<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$Authors), max)
names(AuthorMultipleOutputLastYear) <- c("Author","Year")

##########################################################
#####               Gephi Plots - Authors            #####
##########################################################

#generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(Year,Title) %>%
  summarise(AuthorCorrected = paste(Authors, collapse = ";"))
ListAuthor <- as.data.frame(ListAuthor)

GephiAuthor <- ListAuthor %>%
  select(AuthorCorrected)
names(GephiAuthor) <- c("Author")

#Export to Gephi plot - Year;Title;Authors
#write.table(GephiAuthor, file = paste0(Results.dir,"GephiAuthor.csv"), sep = ",", row.names = F, quote = F)

#Export to Gephi plot - Authors;Year
names(AuthorMultipleOutputLastYear) <- c("Id","Year")
#write.table(AuthorMultipleOutputLastYear, file = paste0(Results.dir,"GephiAuthor_Last_Year.csv"), sep = ";", row.names = F)

############################################################
#####               Data analysis - Authors            #####
############################################################

#####__________________Authors Table/New authors____________________#####

# List of new "Authors" and their first "Year" of appearance
AuthorFirstAppearance<- aggregate(AuthorListExtended$Year, list(AuthorListExtended$Authors), min)
names(AuthorFirstAppearance) <- c("Author","Year")

# New authors
YearNewAuthor <- aggregate(AuthorFirstAppearance$Author,list(AuthorFirstAppearance$Year), FUN=length)
# Add years in which no authors published
DFfilledNewauthors <- YearNewAuthor %>%
  complete(Group.1 = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
YearNewAuthor <- DFfilledNewauthors
names(YearNewAuthor) <- c("Year","New Authors")

YearOutput <- Reduce(merge, list(NumberAuthorYear,PublicationYear,YearNewAuthor))
YearTableOutput <- merge(YearOutput, YearNewAuthorSinglePaper, by="Year", all = T)
YearTableOutput$Ratio <- round(YearOutput$Author/YearOutput$Publications, 1)
YearTableOutput$`New Author Percentage` <- round(YearOutput$`New Authors`/YearOutput$Author*100, 1)

#Replace NA by 0
YearTableOutput[is.na(YearTableOutput)] <- 0

#Export to text file for Latex import
#write.table(YearTableOutput, file = "Authors_table.csv", sep = ",", row.names = F)

# GRAPH
Meanauthors <- mean(YearTableOutput$Ratio)

TotalAuthorsplot <- ggplot(YearTableOutput, aes(x=Year, y=Ratio))+
  geom_line()+ 
  geom_point()+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  scale_linetype_manual(values=c("solid"))+
  scale_color_manual(values=c("black"))+
  labs(x="Year", y="Average number of authors \n per document")+
  theme_classic(base_family = "Arial", base_size = 12)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"))+
  geom_hline(yintercept=Meanauthors, linetype="dashed", color = "blue", size=0.5)
show(TotalAuthorsplot)
#ggplotly(TotalAuthorsplot)
ggsave("Average_Author_Per_Year_ScopWoS.png", TotalAuthorsplot, width = 8, height = 3.5, units = "in", dpi=200, path = "Results")


################################################################
#####                         IFSMS                        #####
################################################################
#############################################################
#####                  Data cleansing                   #####
#############################################################

# Replace each "," with ";" in MergerOriginalData
AuthorListIFSMS <- IFSMS %>%
  select(Year, Title, Authors)
# Change the column name
names(AuthorListIFSMS) <- c("Year", "Title", "Authors")

# Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListIFSMSExtended <- AuthorListIFSMS %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)%>%
  distinct()

# Number of references with no keywords provided by the authors (AK)
sum(is.na(AuthorListIFSMSExtended$Authors))
# Remove entry that have no authors name
AuthorListIFSMSExtended <- AuthorListIFSMSExtended %>% drop_na(Authors)

# Count to number of time the same year is repeated in the "AuthorListIFSMSExtended$Year" and save in a data.frame "Year" 
PublicationYear <- aggregate(AuthorListIFSMS$Authors,list(AuthorListIFSMS$Year), FUN=length)
names(PublicationYear) <- c("Year","Publications")

# add missing year to PublicationYear
DFfilledPublicationYear <- PublicationYear %>%
  complete(Year = 1999:2018,
           fill = list(Freq = 0)) %>%
  as.data.frame()
PublicationYear <- DFfilledPublicationYear

# Count to number of time the same author is repeated in "AuthorListIFSMSExtended$Authors" and save it in "AuthorCountPaper"
AuthorCountPaper <- aggregate(AuthorListIFSMSExtended$Authors, list(AuthorListIFSMSExtended$Authors), FUN=length)
names(AuthorCountPaper) <- c("Author","Frequency")

# Number of Authors per year
NumberAuthorYear <- aggregate(AuthorListIFSMSExtended$Authors,list(AuthorListIFSMSExtended$Year), FUN=length)
names(NumberAuthorYear) <- c("Year","Author")

# add missing year to NumberAuthorYear
DFfilledNumberAuthorYear <- NumberAuthorYear %>%
  complete(Year = 1999:2018,
           fill = list(Freq = 0)) %>%
  as.data.frame()
NumberAuthorYear <- DFfilledNumberAuthorYear

# List of "Author" with one publication only
AuthorCountSinglePaper <- subset(AuthorCountPaper,Frequency<2)
AuthorCountSinglePaperReduced <-subset(AuthorListIFSMSExtended,Authors %in% AuthorCountSinglePaper$Author)
sum(AuthorCountSinglePaper$Frequency)

# List of "Author" with one publication only and their publication "Year"
YearNewAuthorSinglePaper <- aggregate(AuthorCountSinglePaperReduced$Authors,list(AuthorCountSinglePaperReduced$Year), FUN=length)
names(YearNewAuthorSinglePaper) <- c("Year","Single Author")

# add missing year to NumberAuthorYear
DFfilledYearNewAuthorSinglePaper <- YearNewAuthorSinglePaper %>%
  complete(Year = 1999:2018,
           fill = list(Freq = 0)) %>%
  as.data.frame()
YearNewAuthorSinglePaper <- DFfilledYearNewAuthorSinglePaper

# List of Authors with multiple publications only
AuthorCountMultiplePaper <- subset(AuthorCountPaper,Frequency>1)
AuthorCountMultiplePaperReduced <- subset(AuthorListIFSMSExtended, Authors %in% AuthorCountMultiplePaper$Author)
sum(AuthorCountMultiplePaper$Frequency)

# List of "Authors" with multiple output and their first "Year"
AuthorFirstAppearanceMultipleEntry<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$Authors), min)
names(AuthorFirstAppearanceMultipleEntry) <- c("Author","Year")

# List of "Authors" with multiple output and their last "Year"
AuthorMultipleOutputLastYear<- aggregate(AuthorCountMultiplePaperReduced$Year, list(AuthorCountMultiplePaperReduced$Authors), max)
names(AuthorMultipleOutputLastYear) <- c("Author","Year")

##########################################################
#####               Gephi Plots - Authors            #####
##########################################################

#generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(Year,Title) %>%
  summarise(AuthorCorrected = paste(Authors, collapse = ";"))
ListAuthor <- as.data.frame(ListAuthor)

GephiAuthor <- ListAuthor %>%
  select(AuthorCorrected)
names(GephiAuthor) <- c("Author")

#Export to Gephi plot - Year;Title;Authors
#write.table(GephiAuthor, file = paste0(Results.dir,"GephiAuthor.csv"), sep = ",", row.names = F, quote = F)

#Export to Gephi plot - Authors;Year
names(AuthorMultipleOutputLastYear) <- c("Id","Year")
#write.table(AuthorMultipleOutputLastYear, file = paste0(Results.dir,"GephiAuthor_Last_Year.csv"), sep = ";", row.names = F)

############################################################
#####               Data analysis - Authors            #####
############################################################

#####__________________Authors Table/New authors____________________#####

# List of new "Authors" and their first "Year" of appearance
AuthorFirstAppearance<- aggregate(AuthorListIFSMSExtended$Year, list(AuthorListIFSMSExtended$Authors), min)
names(AuthorFirstAppearance) <- c("Author","Year")

# New authors
YearNewAuthor <- aggregate(AuthorFirstAppearance$Author,list(AuthorFirstAppearance$Year), FUN=length)
# Add years in which no authors published
DFfilledNewauthors <- YearNewAuthor %>%
  complete(Group.1 = 1999:2018,
           fill = list(Freq = 0)) %>%
  as.data.frame()
YearNewAuthor <- DFfilledNewauthors
names(YearNewAuthor) <- c("Year","New Authors")

YearOutput <- Reduce(merge, list(NumberAuthorYear,PublicationYear,YearNewAuthor))
YearTableOutput <- merge(YearOutput, YearNewAuthorSinglePaper, by="Year", all = T)
YearTableOutput$Ratio <- round(YearOutput$Author/YearOutput$Publications, 1)
YearTableOutput$`New Author Percentage` <- round(YearOutput$`New Authors`/YearOutput$Author*100, 1)

#Replace NA by 0
YearTableOutput[is.na(YearTableOutput)] <- 0

#Export to text file for Latex import
#write.table(YearTableOutput, file = "Authors_table.csv", sep = ",", row.names = F)

# GRAPH
Meanauthors <- mean(YearTableOutput$Ratio)

TotalAuthorsplot <- ggplot(YearTableOutput, aes(x=Year, y=Ratio))+
  geom_line()+ 
  geom_point()+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  scale_linetype_manual(values=c("solid"))+
  scale_color_manual(values=c("black"))+
  labs(x="Year", y="Average number of authors \n per document")+
  theme_classic(base_family = "Arial", base_size = 12)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"))+
  geom_hline(yintercept=Meanauthors, linetype="dashed", color = "blue", size=0.5)
show(TotalAuthorsplot)
#ggplotly(TotalAuthorsplot)
ggsave("Average_Author_Per_Year_IFSMS.png", TotalAuthorsplot, width = 8, height = 3.5, units = "in", dpi=200, path = "Results")
