# To clean the Global environment
rm(list=ls()) 

#############################################################
#####                     To read                       #####
#############################################################
# This R script is the third step after merging Scopus and Web of Sciences (BibTex format)
# This script allows a bibliometric analysis on the authors

#############################################################
#####                 File requirement                  #####
#############################################################
# The files to be imported are generated by the first code Merger_1.R

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
library(plotly)
library(extrafont)
library(ggpubr)

#######################################################################
#####                           Function                          #####
#######################################################################

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

#######################################################################
#####                         Data loading                        #####
#######################################################################

# set working directory

# read the export *.csv document from Merger, separation "\t", and place it in data.frame "MergerOriginalData"
MergerOriginalData <- read.csv("Result_Merger_Dataset.txt", sep="\t", header=TRUE)


#######################################################################
#####                             Authors                         #####
#######################################################################

# This section is looks at Authors to generate a table:
#                                   the total number of authors and publications per year,
#                                   the number of publications per author,
#                                   the first year an author published,
#                                   the number of new author  

# Replace every "," with ";" in MergerOriginalData
AuthorList <- MergerOriginalData %>%
  select(PY,TI,AU,C1)
# Change the column name
names(AuthorList) <- c("Year", "Title", "Authors", "Affiliation")

# Apply some correction to both previous list
AuthorList$Authors <- gsub(", JR"," JR",AuthorList$Authors)
AuthorList$Authors <- gsub(", II","",AuthorList$Authors)
AuthorList$Authors <- gsub(", III, ",", ",AuthorList$Authors)
AuthorList$Authors <- gsub(",",";", AuthorList$Authors)

#Split Column "Authors" in row by the separator ";", remove leading white space to generate list
AuthorListExtended <- AuthorList %>% 
  mutate(Authors = strsplit(as.character(Authors), ";"))%>% 
  unnest(Authors) %>%
  mutate_if(is.character, str_trim)%>%
  distinct()

#####____________________________##########
#Count to number of time the same year is repeated in the "AuthorListExtended$Year" and save in a data.frame "Year" 
PublicationYear <- aggregate(AuthorList$Authors,list(AuthorList$Year), FUN=length)
names(PublicationYear) <- c("Year","Publications")

# add missing year to PublicationYear
DFfilledPublicationYear <- PublicationYear %>%
  complete(Year = 1967:2019,
           fill = list(Freq = 0)) %>%
  as.data.frame()
PublicationYear <- DFfilledPublicationYear

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

#####__________________Gephi Plots - Authors_________________#####

#generate collapse corrected list of "Authors" by year and title from Authors with multiple papers list
ListAuthor <- AuthorCountMultiplePaperReduced %>% group_by(Year,Title,Affiliation) %>%
  summarise(AuthorCorrected = paste(Authors, collapse = ";"))
ListAuthor <- as.data.frame(ListAuthor)

GephiAuthor <- ListAuthor %>%
  select(AuthorCorrected)
names(GephiAuthor) <- c("Author")

#Export to Gephi plot - Year;Title;Authors
write.table(GephiAuthor, file = "Result_GephiAuthor.csv", quote = F, sep = "\t", row.names = F)

#Export to Gephi plot - Authors;Year
names(AuthorMultipleOutputLastYear) <- c("Id","Year")
write.table(AuthorMultipleOutputLastYear, file = "Result_Gephi List Author Last Year.csv", quote = F, sep = "\t", row.names = F)


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
write.table(YearTableOutput, file = "Resutl_Authors_table.csv", sep = ",", row.names = F)

# GRAPH
YearNewAuthorplot <- ggplot(data=YearTableOutput, aes(x=Year, y=`New Authors`))+
  geom_smooth(formula = y ~ x, method = 'lm', se=F, color="red", size=0.5)+
  geom_line(data=YearTableOutput, aes(x=Year, y=`New Authors`))+ geom_point(data=YearTableOutput, aes(x=Year, y=`New Authors`))+
  labs(x="Year", y="Number of new authors")+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  theme_classic(base_family = "Arial", base_size = 12)+
  theme(legend.position = c(0.05, 0.8),
        legend.background = element_rect(fill = "grey95"),
        legend.title = element_blank(),)
show(YearNewAuthorplot)
ggplotly(YearNewAuthorplot)
#ggsave("YearNewAuthorplot.png", YearNewAuthorplot_December, width = 7, height = 3, units = "in", dpi=200, path = "Results")

# GRAPH
Median2 <- median(YearTableOutput$`New Author Percentage`)
YearNewAuthorplotpercent <- ggplot()+
  geom_hline(yintercept=Median2, linetype="dashed", color = "red", size=0.5)+
  geom_line(data=YearTableOutput, aes(x=Year, y=`New Author Percentage`))+ geom_point(data=YearTableOutput, aes(x=Year, y=`New Author Percentage`))+
  labs(x="Year", y="Number of new authors (%)")+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
  theme_classic(base_family = "Arial", base_size = 12)+
  theme(legend.position = c(0.05, 0.8),
        legend.background = element_rect(fill = "grey95"),
        legend.title = element_blank(),)
show(YearNewAuthorplotpercent)
ggplotly(YearNewAuthorplotpercent)

plot1 <- ggarrange(YearNewAuthorplot, YearNewAuthorplotpercent,labels = c("A", "B"),ncol = 1, nrow = 2,legend = "none")
plot1
#ggsave("NewAuthors_year.png", plot1, width = 7, height = 6, units = "in", dpi=200, path = "Results")

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
ggsave("Result_Average authorPerYear_ScopWoS.png", TotalAuthorsplot, width = 8, height = 3.5, units = "in", dpi=200, path = "Results-2021")

# Calculate the average number of authors (all type of authors)
means <- aggregate(Freq ~ Coder, ForAuthorsplot, mean)
means$Freq <- round(means$Freq,2)
sd1 <- aggregate(Freq ~ Coder, ForAuthorsplot, sd)
sd1$Freq <- round(sd1$Freq,1)

# Basic statistic about the different dataset
summary(Authors$Freq)
summary(SingleAuthorsyear$Freq)
summary(MultipleAuthorsyear$Freq)


#####__________________Authors per document and per year _________________#####
# Create a new dataframe with column Year, Title and Authors
Authorperdocument <- AuthorListExtended %>% group_by(Year,Title) %>%
  summarise(AuthorCorrected = paste(Authors, collapse = ";"))
Authorperdocument <- as.data.frame(Authorperdocument)

# Combine column Year, Affiliation and Title with the separator ";"
Authorperdocument$Title <- paste(Authorperdocument$Year, Authorperdocument$Title, sep = ";")
Authorperdocument <- as.data.frame(Authorperdocument) %>% 
  select(Title, AuthorCorrected)

#Split Column "Authors" in row by the separator ";", remove leading white space to generate list
Authorperdocument <- Authorperdocument %>% 
  mutate(AuthorCorrected = strsplit(as.character(AuthorCorrected), ";"))%>% 
  unnest(AuthorCorrected) %>%
  mutate_if(is.character, str_trim)
names(Authorperdocument) <- c("Title","AuthorCorrected")

# Number of Authors per Title
Authorperdocument <- aggregate(Authorperdocument$AuthorCorrected,list(Authorperdocument$Title), FUN=length)
names(Authorperdocument) <- c("Title","Frequency")

# Separate Column Title in to "Year" and "Title" by the separator ";" 
Authorperdocument <- separate(data = Authorperdocument, col = Title, into = c("Year", "Title"), sep = ";")

# Calculate the average number of authors per document and per year
means2 <- aggregate(Frequency ~  Year, Authorperdocument, mean)
Median2 <- aggregate(Frequency ~  Year, Authorperdocument, median)

#  GRAPH
jitter <- position_jitter(width = 0.2, height =0)
Authorperdocumentplot <-ggplot() +
  geom_point(data =Authorperdocument, aes(x =Year, y = Frequency), color = "black", shape=1, size=1, position= jitter)+
  geom_point(data =means2, aes(x =Year, y = Frequency), color = "darkred", shape=17, size=2)+  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
  geom_point(data =Median2, aes(x =Year, y = Frequency), color = "blue", shape=18, size=2)+  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
  labs(x="Year", y="Authors per documents")+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
show(Authorperdocumentplot)
ggsave("Authorperdocumentplot.png", Authorperdocumentplot, width = 6, height = 4, units = "in", dpi=200, path = "Results-2021")

Authorperdocumentboxplot <-ggplot() +
  geom_boxplot(data =Authorperdocument, aes(x =Year, y = Frequency), outlier.colour= "red", outlier.shape = 8, color = "black", shape=1, size=0.5)+
  labs(x="Year", y="Authors per documents")+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
show(Authorperdocumentboxplot)
ggsave("Authorperdocumentboxplot_December.png", Authorperdocumentboxplot, width = 6, height = 4, units = "in", dpi=200, path = "Results-2021")

p3 <- ggarrange(Authorperdocumentplot, Authorperdocumentboxplot,labels = c("A", "B"),ncol = 1, nrow = 2,legend = "none")
show(p3)
#ggsave("Authorperdocument combine_December.png", p3, width = 6, height = 6, units = "in", dpi=200, path = "Results")

# OTHER GRAPH
Authorperdocumentmix <-ggplot() +
  geom_boxplot(data =Authorperdocument, aes(x =Year, y = Frequency), outlier.colour= "red", outlier.shape = 8, color = "black", shape=1, size=0.5)+
  geom_point(data =Authorperdocument, aes(x =Year, y = Frequency), color = "black", shape=1, size=1, position= jitter)+
  geom_point(data =means2, aes(x =Year, y = Frequency), color = "darkred", shape=17, size=1)+  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
  labs(x="Year", y="Authors per documents")+
  theme_bw(base_family = "Arial", base_size = 12)+
  theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
show(Authorperdocumentmix)
#ggsave("Authorperdocument mix_December.png",Authorperdocumentmix, width = 6, height = 4, units = "in", dpi=200, path = "Results")

#####__________________Authors per document and per year split_________________##### STILL WORKinG ON IT

Oneauthorperdocument <- filter(Authorperdocument, Frequency==1)
Oneauthorperdocument <- aggregate(Oneauthorperdocument$Frequency,list(Oneauthorperdocument$Year), FUN=length)
names(Oneauthorperdocument) <- c("Year", "Freq")
functionx <- function(x){
  group1 <- filter(x, Year <= 1970)
  group2 <- filter(x, Year >= 1970 & Year <= 1979)
  group3 <- filter(x, Year >= 1980 & Year <= 1989)
  group4 <- filter(x, Year >= 1990 & Year <= 1999)
  group5 <- filter(x, Year >= 2000 & Year <= 2009)
  group6 <- filter(x, Year >= 2010 & Year <= 2019)
  
  sum1 <- sum(group1$Freq)
  sum2 <- sum(group1$Freq)
  sum3 <- sum(group1$Freq)
  sum4 <- sum(group1$Freq)
  sum5 <- sum(group1$Freq)
  sum6 <- sum(group1$Freq)
  
  return(group1)
}

functionx(Oneauthorperdocument)

Twoauthorperdocument <- filter(Authorperdocument, Frequency==2)
Twoauthorperdocument <- aggregate(Twoauthorperdocument$Frequency,list(Twoauthorperdocument$Year), FUN=length)
names(Twoauthorperdocument) <- c("Year", "Freq")

Threeauthorperdocument <- filter(Authorperdocument, Frequency==3)
Threeauthorperdocument <- aggregate(Threeauthorperdocument$Frequency,list(Threeauthorperdocument$Year), FUN=length)
names(Threeauthorperdocument) <- c("Year", "Freq")

Foureauthorperdocument <- filter(Authorperdocument, Frequency==4)
Foureauthorperdocument <- aggregate(Foureauthorperdocument$Frequency,list(Foureauthorperdocument$Year), FUN=length)
names(Foureauthorperdocument) <- c("Year", "Freq")

Fiveeauthorperdocument <- filter(Authorperdocument, Frequency==5)
Fiveeauthorperdocument <- aggregate(Fiveeauthorperdocument$Frequency,list(Fiveeauthorperdocument$Year), FUN=length)
names(Fiveeauthorperdocument) <- c("Year", "Freq")

Morethanfoureauthorperdocument <- filter(Authorperdocument, Frequency>5)
Morethanfoureauthorperdocument <- aggregate(Morethanfoureauthorperdocument$Frequency,list(Morethanfoureauthorperdocument$Year), FUN=length)
names(Morethanfoureauthorperdocument) <- c("Year", "Freq")

#####________________Most prolific authors_______________#####
# Create new dataframe with only the first top 15 articles
YearMultipleAuthor <- aggregate(AuthorFirstAppearance$Author,list(AuthorFirstAppearance$Year), FUN=length)
MostProlificAuthor <- top_n(AuthorCountMultiplePaper, 15,Frequency)
MostProlificAuthor <- MostProlificAuthor[order(-MostProlificAuthor$Frequency),]
names(MostProlificAuthor) <- c("Authors", "Publication")

#Export to text file
#write.table(MostProlificAuthor, file = "MostProlificAuthor_Table_December.csv", sep = ",", row.names = F)