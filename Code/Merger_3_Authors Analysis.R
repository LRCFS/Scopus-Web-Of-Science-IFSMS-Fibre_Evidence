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
#####__________________Authors per year _________________#####

# 1) Plot number of authors per year (Total, Single and multiple authors)

# create new data.frame of the number of Authors contributed to publications
Authors <- NumberAuthorYear
names(Authors) <- c("Year","Freq")

# create new data.frame of the number of Single Authors contributed to publications 
SingleAuthors <- AuthorCountSinglePaperReduced %>%
  select(Year, Authors)

# create new data.frame of the number of Multiple Authors contributed to publications
MultipleAuthors <- AuthorCountMultiplePaperReduced %>%
  select(Year, Authors) 

#Count to number of time the same year is repeated in each of the previous data.frame 
SingleAuthorsyear <- data.frame(table(SingleAuthors$Year));SingleAuthorsyear
SingleAuthorsyear$Var1 <- as.numeric(as.character(SingleAuthorsyear$Var1))
names(SingleAuthorsyear) <- c("Year","Freq")

MultipleAuthorsyear <- data.frame(table(MultipleAuthors$Year));MultipleAuthorsyear
MultipleAuthorsyear$Var1 <- as.numeric(as.character(MultipleAuthorsyear$Var1))
names(MultipleAuthorsyear) <- c("Year","Freq")

# Add years in which no authors published
DFfilledauthors <- Authors %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
DFfilledauthors

DFfilledSingleauthors <- SingleAuthorsyear %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
DFfilledSingleauthors

DFfilledMultipleauthors <- MultipleAuthorsyear %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
DFfilledMultipleauthors

#  GRAPH number of authors per year
# Label each row  
DFfilledauthors$Coder <- "Total Authors"
DFfilledSingleauthors$Coder <- "With unique publication"
DFfilledMultipleauthors$Coder <- "With multiple publication"

# Create a new dataframe by combining DFfilledauthors, DFfilledSingleauthors, DFfilledMultipleauthors
ForAuthorsplot <- as.data.frame(rbind(DFfilledauthors, DFfilledMultipleauthors, DFfilledSingleauthors))

#  GRAPH
Authorsplot <- ggplot(ForAuthorsplot, aes(x=Year, y=Freq, color=Coder))+
  geom_line(aes(linetype=Coder, color=Coder), size =1)+ 
  scale_linetype_manual(values=c("solid","solid", "dashed"))+
  scale_color_manual(values=c("black", "darkblue", "grey50"))+
  labs(x="Year", y="Number of authors")+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  theme_classic(base_family = "Arial", base_size = 12)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"))
show(Authorsplot)
#ggsave("Authors-document-year.png", Authorsplot, width = 7, height = 5, units = "in", dpi=200, path = "Results")

#graph with just the total
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
ggplotly(TotalAuthorsplot)
ggsave("Average_Author_Per_Year_ScopWoS.png", TotalAuthorsplot, width = 8, height = 3.5, units = "in", dpi=200, path = "Results")

################################################################
#####                         IFSMS                        #####
################################################################
#############################################################
#####                  Data cleansing                   #####
#############################################################