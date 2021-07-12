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

# to correct keywords
#write.csv(KeywordListCount,"Result_Keywords to correct.csv")

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

# #####________________References with keywords________________#####
# # This part allows to calculate the number of references with keywords provided, from Scopus, from Web of Science and both Scopus+WoS
# # Count the number of references with DES, DEW, IDS, IDW and AIK as well as their % to the total number of references
# Totalref <- data.frame(nrow(MergerOriginalData))
# CountDES <- data.frame(table(MergerOriginalData$AIKS, exclude = ""));CountDES
# CountDEW <- data.frame(table(MergerOriginalData$AIKW, exclude = ""));CountDEW
# CountIDS <- data.frame(table(MergerOriginalData$IDS, exclude = ""));CountIDS
# CountIDW <- data.frame(table(MergerOriginalData$IDW, exclude = ""));CountIDW
# CountAIK <- data.frame(table(MergerOriginalData$AIK, exclude = ""));CountAIK
# 
# KeywordTable_1 <- data.frame(sum(CountDES$Freq))
# KeywordTable_1[2,1] <- sum(CountDEW$Freq)
# KeywordTable_1[3,1] <- sum(CountIDS$Freq)
# KeywordTable_1[4,1] <- sum(CountIDW$Freq)
# KeywordTable_1[5,1] <- sum(CountAIK$Freq)
# 
# rownames(KeywordTable_1)[rownames(KeywordTable_1)=="1"] <- "Author keywords Scopus"
# rownames(KeywordTable_1)[rownames(KeywordTable_1)=="2"] <- "Author keywords WoS"
# rownames(KeywordTable_1)[rownames(KeywordTable_1)=="3"] <- "Index keywords Scopus"
# rownames(KeywordTable_1)[rownames(KeywordTable_1)=="4"] <- "Index keywords WoS"
# rownames(KeywordTable_1)[rownames(KeywordTable_1)=="5"] <- "Authors keywords Scopus+WoS distinct"
# 
# KeywordTable_1 <- rownames_to_column(KeywordTable_1)
# names(KeywordTable_1) <- c("Keywords", "Count")

#Export to text file
#write.table(KeywordTable_1, file = "Result_Keyword Table_ScopWoS.csv", sep = ",", row.names = F)

# # references with no AIK
# RefNoAIK <- filter(MergerOriginalData, AIK=="")
# ScopWosnotexclusive <- RefNoAIK[RefNoAIK$Coder == "Scopus,WebOfScience"|RefNoAIK$Coder == "WebOfScience,Scopus", ]
# Scopusexclusive <- RefNoAIK[RefNoAIK$Coder == "Scopus", ]
# WoSexclusive <- RefNoAIK[RefNoAIK$Coder == "WebOfScience", ]
# 
# # Count 
# countWoSexclusive <- as.numeric(count(WoSexclusive));countWoSexclusive
# countScopusexclusive <- as.numeric(count(Scopusexclusive));countScopusexclusive
# countScopWosnotexclusive <- as.numeric(count(ScopWosnotexclusive));countScopWosnotexclusive
# Total <- (countWoSexclusive+countScopusexclusive+countScopWosnotexclusive);Total
# 
# # Type of document mostly without AIK
# CountDT <- data.frame(table(RefNoAIK$DT, exclude = ""));CountDT

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

# #count the number of time each keywords appear each year 
# MergeDataKeywordYearCount <- aggregate(MergeDataKeywordListTemp2$Year, by=list(Year=MergeDataKeywordListTemp2$Year, Rtitle=MergeDataKeywordListTemp2$KeywordsCorrected), FUN=length)
# 
# #count the number of keywords per year 
# MergeDataKeywordYearCountBis <- aggregate(MergeDataKeywordYearCount$x, list(MergeDataKeywordYearCount$Year), FUN=sum)
# names(MergeDataKeywordYearCountBis) <- c("Year","Freq")
# 
# # Add in each MergerOriginalData a column with the number of document each year
# # create a new data.frame of the number of document published each year
# Year <- MergerOriginalData %>%
#   select(PY)
# 
# #Count to number of time the same year is repeated in the "Year" and save in a data.frame "year" 
# year <- data.frame(table(Year$PY));year
# year$Var1 <- as.numeric(as.character(year$Var1))
# names(year) <- c("Year","Freq")
# #add in year the missing year
# DFfilledYear <- year %>%
#   complete(Year = 1967:2019,
#            fill = list(Freq = 0)) %>%
#   as.data.frame()
# year <- DFfilledYear
# 
# TableAKeywords <- merge(MergeDataKeywordYearCountBis, year, by="Year", all = F)
# 
# # Calculate the average number of keywords per document and per year
# MeanAK <- data.frame(MeanAK=TableAKeywords$Freq.x/TableAKeywords$Freq.y)
# MeanAK <- round(MeanAK, 2)
# 
# # Combine the calculated mean "MeanA/IK" with the rest of the table "TableA/IKeywords" and add a Coder
# FinalTableAKeywords <- cbind(TableAKeywords, MeanAK)
# FinalTableAKeywords$Coder <- "Authors Keywords"
# 
# #Calculate the mean of the mean for each variable
# MeanAK2 <- mean(FinalTableAKeywords$MeanAK)
# MeanAK2 <- round(MeanAK2, 2)
# 
# #  GRAPH
# KeywordsPerYear <- ggplot(FinalTableAKeywords, aes(x=Year, y=MeanAK))+
#   geom_line()+ 
#   geom_point()+
#   scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015,2019))+
#   scale_linetype_manual(values=c("solid"))+
#   scale_color_manual(values=c("black"))+
#   labs(x="Year", y="Average number of keywords \n per document")+
#   theme_classic(base_family = "Arial", base_size = 12)+
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.background = element_rect(fill="grey95",size=1, linetype="solid", colour="grey80"))+
#   geom_hline(yintercept=MeanAK2, linetype="dashed", color = "blue", size=0.5)
# show(KeywordsPerYear)
# ggplotly(KeywordsPerYear)
# 
# #To save the graph
# ggsave("Average Keywords per year_ScopWoS.png", KeywordsPerYear, width = 8, height = 3.5, units = "in", dpi=300, path = "Results-2021")
# 
# #####__________________Keywords per document and per year _________________#####
# # Create a new dataframe with column Year, Title and Authors
# Keywordsperdocument <- MergeDataKeywordListTemp1 %>% group_by(Year,Title) %>%
#   summarise(KeywordsCorrected = paste(KeywordsCorrected, collapse = ";"))
# Keywordsperdocument <- as.data.frame(Keywordsperdocument)
# 
# # Combine column Year and Title with the separator ";"
# Keywordsperdocument$Title <- paste(Keywordsperdocument$Year, Keywordsperdocument$Title, sep = ";")
# Keywordsperdocument <- as.data.frame(Keywordsperdocument) %>% 
#   select(Title, KeywordsCorrected)
# 
# #Split Column "KeywordsCorrected" in row by the separator ";", remove leading white space to generate list
# Keywordsperdocument <- Keywordsperdocument %>% 
#   mutate(KeywordsCorrected = strsplit(as.character(KeywordsCorrected), ";"))%>% 
#   unnest(KeywordsCorrected) %>%
#   mutate_if(is.character, str_trim)
# Keywordsperdocumentbis <- filter(Keywordsperdocument, KeywordsCorrected=="NA")
# Keywordsperdocumentfinal <- setdiff(Keywordsperdocument, Keywordsperdocumentbis)
# 
# # Number of Keywords per Title
# Keywordsperdocumentfinal <- aggregate(Keywordsperdocumentfinal$KeywordsCorrected,list(Keywordsperdocumentfinal$Title), FUN=length)
# names(Keywordsperdocumentfinal) <- c("Title","Frequency")
# 
# # Separate Column Title in to "Year" and "Title" by the separator ";" 
# Keywordsperdocumentfinal <- separate(data = Keywordsperdocumentfinal, col = Title, into = c("Year", "Title"), sep = ";")
# 
# # Calculate the average number of authors per document and per year
# means2 <- aggregate(Frequency ~  Year, Keywordsperdocumentfinal, mean)
# 
# #  GRAPH
# jitter <- position_jitter(width = 0.2, height =0)
# Keywordsperdocumentplot <-ggplot() +
#   geom_point(data =Keywordsperdocumentfinal, aes(x =Year, y = Frequency), color = "black", shape=1, size=1, position= jitter)+
#   geom_point(data =means2, aes(x =Year, y = Frequency), color = "darkred", shape=17, size=2)+  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
#   labs(x="Year", y="Keywords per documents")+
#   theme_bw(base_family = "Arial", base_size = 12)+
#   theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
# show(Keywordsperdocumentplot)
# ggsave("Keywords per document plot_ScopWoS.png", Keywordsperdocumentplot, width = 7, height = 5, units = "in", dpi=200, path = "Results-2021")
# 
# Keywordsperdocumentboxplot <- ggplot() +
#   geom_boxplot(data =Keywordsperdocumentfinal, aes(x =Year, y = Frequency), outlier.colour= "red", outlier.shape = 8, color = "black", shape=1, size=0.5)+
#   labs(x="Year", y="Keywords per documents")+
#   theme_bw(base_family = "Arial", base_size = 12)+
#   theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
# show(Keywordsperdocumentboxplot)
# ggsave("Keywords per document boxplot_ScopWoS.png", Keywordsperdocumentboxplot, width = 6, height = 4, units = "in", dpi=200, path = "Results-2021")
# 
# p3 <- ggarrange(Keywordsperdocumentplot, Keywordsperdocumentboxplot,labels = c("A", "B"),ncol = 1, nrow = 2,legend = "none")
# show(p3)
# ggsave("Keywords per document combined_ScopWoS.png", p3, width = 6, height = 6, units = "in", dpi=200, path = "Results-2021")
# 
# # OTHER GRAPH
# Keywordsperdocumentmix <-ggplot() +
#   geom_boxplot(data =Keywordsperdocumentfinal, aes(x =Year, y = Frequency), outlier.colour= "red", outlier.shape = 8, color = "black", shape=1, size=0.5)+
#   geom_point(data =Keywordsperdocumentfinal, aes(x =Year, y = Frequency), color = "black", shape=1, size=1, position= jitter)+
#   geom_point(data =means2, aes(x =Year, y = Frequency), color = "darkred", shape=17, size=2)+  geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
#   geom_line(data = means2, aes(x =Year, y = Frequency, group=1), color = "darkred")+
#   labs(x="Year", y="Keywords per documents")+
#   theme_bw(base_family = "Arial", base_size = 12)+
#   theme(axis.text.x= element_text(angle= 90, vjust= 0.5))
# show(Keywordsperdocumentmix)
# ggsave("Keywords per document mix_ScopWoS.png",Keywordsperdocumentmix, width = 6, height = 4, units = "in", dpi=200, path = "Results-2021")

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
ggsave("KeywordTrend_ScopWoS.png", p, width = 13, height = 20, units = "in", dpi=500, path = "Results-2021")




#####______________Graph for Techniques analysis only______________##########

#Create a new variable of MergeKeywordNarrowRangeGraph to not overwrite data
MergeDataKeywordNarrowRangeGraph2 <-subset(MergeDataKeywordYearCount,Rtitle %in% DatasetKeywordNarrowRangeGraph$Rtitle)

#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
MergeDataKeywordNarrowRangeGraph2$x <- as.numeric(MergeDataKeywordNarrowRangeGraph2$x)

# Create a list of techniques to include
techniques.list <- paste(c("MICROSPECTROPHOTOMETRY (MSP)",
                           "INFRARED SPECTROSCOPY (IR)",
                           "FOURIER-TRANSFORM INFRARED SPECTROSCOPY (FT-IR)",
                           "SPECTROPHOTOMETRY",
                           "MICROSCOPY",
                           "MASS SPECTROMETRY (MS)",
                           "SCANNING ELECTRON MICROSCOPY (SEM)",
                           "PRINCIPAL COMPONENT ANALYSIS (PCA)",
                           "THIN-LAYER CHROMATOGRAPHY (TLC)",
                           "RAMAN",
                           "CAPILLARY ELECTROPHORESIS (CE)",
                           "GAS CHROMATOGRAPHY–MASS SPECTROMETRY (GC–MS)",
                           "HOLLOW-FIBRE LIQUID-PHASE MICROEXTRACTION (HF-LPME)",
                           "POLARIZED LIGHT MICROSCOPY",
                           "SPECTROSCOPY",
                           "HIGH-PERFORMANCE LIQUID CHROMATOGRAPHY (HPLC)"), collapse = ';')
TechniquesList <- as.data.frame(techniques.list)

# #Split Column "techniques.list" in row by the separator ";", remove leading white space to generate list
# TechniquesList <- TechniquesList %>% 
#   mutate(techniques.list = strsplit(as.character(techniques.list), ";")) %>% 
#   unnest(techniques.list) %>%
#   mutate_if(is.character, str_trim)
# 
# #total count of the keywords
# TechniqueListCount <- aggregate(TechniquesList$x, list(TechniquesList$Rtitle), sum)

# Select from "MergeDataKeywordNarrowRangeGraph2" every keywords from "techniques.list" and place it in a new list "TechniqueList"
TechniqueList <-subset(MergeDataKeywordNarrowRangeGraph2,Rtitle %in% TechniquesList$techniques.list)

# Rename some of the techniques that are too long
# read the list of techniques'abreviations and combine it to TechniqueList
TechniqueCorrected <- read.csv("Techniques's abreviations_ScopWoS.txt", sep="\t", header=TRUE)
TechniqueList$Rtitle2 <- gsr(TechniqueList$Rtitle,TechniqueCorrected$name, as.character(TechniqueCorrected$Name.Corrected))
TechniqueList <- TechniqueList %>% select("Year", "Rtitle2", "x")
names(TechniqueList) <- c("Year", "Rtitle", "x")


#### plot second graph
# Create a new variable from incidence
TechniqueList$Incidenceweight <- cut(TechniqueList$x,
                                     breaks = c(-1,0,1,2,3,4,max(TechniqueList$x,na.rm=T)),
                                     labels=c("0","1","2","3","4","5"))

GraphTemp1Bis <- TechniqueList %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,3,4,max(x,na.rm=T)),
                         labels=c("0","1","2","3","4","5")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolKeywordList$WYear <- gsr(InterpolKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1Bis[, 1], list(GraphTemp1Bis$KeywordsCorrected), min)

GraphTemp1Bis$graphorder <- as.numeric(gsr(GraphTemp1Bis$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))
GraphTemp1Bis
# assign text colour
textcol <- "black"


# further modified ggplot
p1 <- ggplot(GraphTemp1Bis,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0),labels = function(x) str_wrap(x, width = 30))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,200,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#BD0026", "#F03B20", "#FD8D3C", "#FECC5C","lightgoldenrod1"),na.value = "grey90")+
  coord_fixed()+
  theme_bw(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p1)
#ggplotly(p1)
ggsave("Techniques Keyword Trend_ScopWoS.png", p1,  width = 6, height = 6, units = "in", dpi=150, path = "Results-2021")

#####______________Graph for Textile analysis only______________##########

#Create a new variable of ScopusKeywordNarrowRangeGraph to not overwrite data
MergeDataKeywordNarrowRangeGraph3 <-subset(MergeDataKeywordYearCount,Rtitle %in% MergeDataKeywordYearCount$Rtitle)

#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
MergeDataKeywordNarrowRangeGraph3$x <- as.numeric(MergeDataKeywordNarrowRangeGraph3$x)

# Create a list of techniques to include
Textiles.list <- paste(c("WOOL",
                         "ACRYLIC",
                         "VISCOSE",
                         "POLYESTER",
                         "COTTON",
                         "SYNTHETIC FIBRE","POLYETHYLENE"), collapse = ';')
Textileslist <- as.data.frame(Textiles.list)

#Split Column "Textiles.list" in row by the separator ";", remove leading white space to generate list
Textileslist <- Textileslist %>% 
  mutate(Textiles.list = strsplit(as.character(Textiles.list), ";")) %>% 
  unnest(Textiles.list) %>%
  mutate_if(is.character, str_trim)

# Select from "ScopusKeywordNarrowRangeGraph3" every keywords from "Textiles.list" and place it in a new list "TechniqueList"
Textileslist <-subset(MergeDataKeywordNarrowRangeGraph3,Rtitle %in% Textileslist$Textiles.list)

# Total count of the keywords
TextileslistCount <- aggregate(Textileslist$x, list(Textileslist$Rtitle), sum)
sum(TextileslistCount$x)

#### plot second graph
# Create a new variable from incidence
Textileslist$Incidenceweight <- cut(Textileslist$x,
                                    breaks = c(-1,0,1,2,3,max(Textileslist$x,na.rm=T)),
                                    labels=c("0","1","2","3","4"))

GraphTemp1Ter <- Textileslist %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,2,3,max(x,na.rm=T)),
                         labels=c("0","1","2","3","4")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolKeywordList$WYear <- gsr(InterpolKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1Ter[, 1], list(GraphTemp1Ter$KeywordsCorrected), min)

GraphTemp1Ter$graphorder <- as.numeric(gsr(GraphTemp1Ter$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p2 <- ggplot(GraphTemp1Ter,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,200,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#08519C","#3182BD","#6BAED6","#9ECAE1","#C6DBEF","#EFF3FF"),na.value = "grey90")+
  coord_fixed()+
  theme_bw(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
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
#ggplotly(p2)
ggsave("Textile Keyword Trend_SopWoS.png", p2, width = 6, height = 6, units = "in", dpi=500, path = "Results-2021")


#####______________Graph for Transfer/Persistence analysis only______________##########

#Create a new variable of ScopusKeywordNarrowRangeGraph to not overwrite data
MergeDataKeywordNarrowRangeGraph4 <-subset(MergeDataKeywordYearCount,Rtitle %in% MergeDataKeywordYearCount$Rtitle)

#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
MergeDataKeywordNarrowRangeGraph4$x <- as.numeric(MergeDataKeywordNarrowRangeGraph4$x)

# Create a list of techniques to include
Transfer.list <- paste(c("TRANSFER",
                         "FIBRE TRANSFER",
                         "TRANSFERENCE", "TRANSFERRING",
                         "SECONDARY TRANSFER",
                         "DIFFERENTIAL TRANSFER", "	FIBRE TRANSFER AND RETENTION","FIBRE: TRANSFER AND DISPERSAL",
                         "INTERPRETATION OF FIBRE TRANSFER", "PRIMARY TRANSFER SIMULATION", "RELEVANCE OF TRANSFER",
                         "RELEVANCE OF TRANSFER MATERIALS", "SCENT TRANSFER UNIT (STU-100)", "TRANSFER SIMULATION	",
                         "TRANSFER UNIT (STU-100)",
                         "	PERSISTENCE","TRANSFER PERSISTENCE", "FIBRE PERSISTENCE"), collapse = ';')
Transferlist <- as.data.frame(Transfer.list)

#Split Column "Transfer.list" in row by the separator ";", remove leading white space to generate list
Transferlist <- Transferlist %>% 
  mutate(Transfer.list = strsplit(as.character(Transfer.list), ";")) %>% 
  unnest(Transfer.list) %>%
  mutate_if(is.character, str_trim)

# Select from "ScopusKeywordNarrowRangeGraph3" every keywords from "Transfer.list" and place it in a new list "TechniqueList"
Transferlist <-subset(MergeDataKeywordNarrowRangeGraph4,Rtitle %in% Transferlist$Transfer.list)

# Total count of the keywords
TransferlistCount <- aggregate(Transferlist$x, list(Transferlist$Rtitle), sum)

#### plot second graph
# Create a new variable from incidence
Transferlist$Incidenceweight <- cut(Transferlist$x,
                                    breaks = c(-1,0,1,max(Transferlist$x,na.rm=T)),
                                    labels=c("0","1","2"))

GraphTemp1Transfer <- Transferlist %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,max(x,na.rm=T)),
                         labels=c("0","1","2")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolKeywordList$WYear <- gsr(InterpolKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1Transfer[, 1], list(GraphTemp1Transfer$KeywordsCorrected), min)

GraphTemp1Transfer$graphorder <- as.numeric(gsr(GraphTemp1Transfer$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p3 <- ggplot(GraphTemp1Transfer,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1975,1985,1995,2005,2015))+
  scale_fill_manual(values=c("#000099","#3366ff","#99ccff"),na.value = "grey90")+
  coord_fixed()+
  theme_grey(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p3)
#ggplotly(p3)
ggsave("Transfer Keyword Trend_December_SopWoS.png", p3, width = 8, height = 3, units = "in", dpi=150, path = "Results-2021")


#####______________Graph for Colour analysis only______________##########

#Create a new variable of ScopusKeywordNarrowRangeGraph to not overwrite data
MergeDataKeywordNarrowRangeGraph5 <-subset(MergeDataKeywordYearCount,Rtitle %in% MergeDataKeywordYearCount$Rtitle)

#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
MergeDataKeywordNarrowRangeGraph5$x <- as.numeric(MergeDataKeywordNarrowRangeGraph5$x)

# Create a list of techniques to include
Colour.list <- paste(c("DYE",
                       "COLOUR",
                       "REACTIVE DYE",
                       "PIGMENTS",
                       "DISPERSE DYES"), collapse = ';')
Colourlist <- as.data.frame(Colour.list)

#Split Column "Colour.list" in row by the separator ";", remove leading white space to generate list
Colourlist <- Colourlist %>% 
  mutate(Colour.list = strsplit(as.character(Colour.list), ";")) %>% 
  unnest(Colour.list) %>%
  mutate_if(is.character, str_trim)

# Select from "ScopusKeywordNarrowRangeGraph3" every keywords from "Transfer.list" and place it in a new list "TechniqueList"
Colourlist <-subset(MergeDataKeywordNarrowRangeGraph5,Rtitle %in% Colourlist$Colour.list)


#### plot second graph
# Create a new variable from incidence
Colourlist$Incidenceweight <- cut(Colourlist$x,
                                    breaks = c(-1,0,1,max(Colourlist$x,na.rm=T)),
                                    labels=c("0","1","2"))

GraphTemp1Transfer <- Colourlist %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,max(x,na.rm=T)),
                         labels=c("0","1","2")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolKeywordList$WYear <- gsr(InterpolKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1Transfer[, 1], list(GraphTemp1Transfer$KeywordsCorrected), min)

GraphTemp1Transfer$graphorder <- as.numeric(gsr(GraphTemp1Transfer$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p4 <- ggplot(GraphTemp1Transfer,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,200,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#9E9AC8","#6A51A3"),na.value = "grey90")+
  coord_fixed()+
  theme_bw(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p4)
#ggplotly(p4)
ggsave("Colour Keyword Trend_December_SopWoS.png", p4, width = 6, height = 6, units = "in", dpi=300, path = "Results-2021")

#####______________Graph for Bayesian analysis only______________##########

#Create a new variable of ScopusKeywordNarrowRangeGraph to not overwrite data
MergeDataKeywordNarrowRangeGraph6 <-subset(MergeDataKeywordYearCount,Rtitle %in% MergeDataKeywordYearCount$Rtitle)

#Reduced <- subset(Condensed, SummaryKeywords$weight>0.007)
MergeDataKeywordNarrowRangeGraph6$x <- as.numeric(MergeDataKeywordNarrowRangeGraph6$x)

# Create a list of techniques to include
Bayesian.list <- paste(c("INTERPRETATION",
                       "LIKELYHOOD RATIO",
                       "BAYESIAN APPROACH",
                       "STATISTICS",
                       "EVIDENTIAL VALUE"), collapse = ';')
Bayesianlist <- as.data.frame(Bayesian.list)

#Split Column "Bayesian.list" in row by the separator ";", remove leading white space to generate list
Bayesianlist <- Bayesianlist %>% 
  mutate(Bayesian.list = strsplit(as.character(Bayesian.list), ";")) %>% 
  unnest(Bayesian.list) %>%
  mutate_if(is.character, str_trim)

# Select from "ScopusKeywordNarrowRangeGraph3" every keywords from "Transfer.list" and place it in a new list "TechniqueList"
Bayesianlist <-subset(MergeDataKeywordNarrowRangeGraph6,Rtitle %in% Bayesianlist$Bayesian.list)


#### plot second graph
# Create a new variable from incidence
Bayesianlist$Incidenceweight <- cut(Bayesianlist$x,
                                  breaks = c(-1,0,1,max(Bayesianlist$x,na.rm=T)),
                                  labels=c("0","1","2"))

GraphTemp1Transfer <- Bayesianlist %>%
  # convert state to factor and reverse order of levels
  mutate(KeywordsCorrected=factor(Rtitle,levels=rev(sort(unique(Rtitle))))) %>%
  # create a new variable from count
  mutate(countfactor=cut(x,breaks=c(-1,0,1,max(x,na.rm=T)),
                         labels=c("0","1","2")))  %>%
  # change level order
  mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
# InterpolKeywordList$WYear <- gsr(InterpolKeywordList$Year,year$Var1,1/year$Freq)
GraphTemp2 <- aggregate(GraphTemp1Transfer[, 1], list(GraphTemp1Transfer$KeywordsCorrected), min)

GraphTemp1Transfer$graphorder <- as.numeric(gsr(GraphTemp1Transfer$KeywordsCorrected,GraphTemp2$Group.1,GraphTemp2$x))

# assign text colour
textcol <- "black"

# further modified ggplot
p5 <- ggplot(GraphTemp1Transfer,aes(x=Year,y=reorder(KeywordsCorrected,graphorder),fill=countfactor))+
  geom_tile(colour="white",size=0.2)+
  guides(fill=guide_legend(title="Count"))+
  #  labs(x="",y="",title="Keywords found in gunshot residue publication")+
  labs(x="Year",y="",title="")+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(breaks=c(1965,1970,1975,1980,1985,1990,1995,200,2005,2010,2015,2019))+
  scale_fill_manual(values=c("#006D2C","#A1D99B"),na.value = "grey90")+
  coord_fixed()+
  theme_bw(base_size=8)+
  theme(legend.position="right",legend.direction="vertical",
        legend.title=element_text(colour=textcol),
        legend.margin=margin(grid::unit(0,"cm")),
        legend.text=element_text(colour=textcol,size=7),
        legend.key.height=grid::unit(0.8,"cm"),
        legend.key.width=grid::unit(0.2,"cm"),
        axis.text.x=element_text(size=8,colour=textcol),
        axis.text.y=element_text(vjust=0.2,colour=textcol),
        axis.ticks=element_line(size=0.4),
        plot.background=element_blank(),  # element_rect(fill, colour, size, linetype, color))
        panel.border=element_blank(),
        plot.margin=margin(0.7,0.4,0.1,0.2,"cm"),
        plot.title=element_text(colour=textcol,hjust=0,size=12))
show(p5)
#ggplotly(p5)
ggsave("bayesian Keyword Trend_SopWoS.png", p5, width = 6, height = 6, units = "in", dpi=500, path = "Results-2021")

#####______________2D matrix______________##########
# create a list with the keywords with a frequency >5
TopKeywords <- GraphTemp1 %>%
  select(Rtitle)

# Remove duplicates to create the list of the top keywords
TopKeywordsList <- TopKeywords %>% distinct()
names(TopKeywordsList) <- c("TopKeywords")

# Export to Excel to create the correlation Table
#write.table(TopKeywordsList, file = "TopKeywordsList_AKeywords.csv", quote = F, sep = ",", row.names = F)
#write.table(MergeDataKeywordListTemp2, file = "MergeKeywordListTemp2_Akeywords.csv", quote = F, sep = ";", row.names = F)

# Import correlation Table
MatrixKeyword <- read.csv("Matrix AKeywords sup5 - step2.txt", sep="\t", header=T, check.names=FALSE)

# transform the column with numbers into num. the number of row must be specified and adapt to the dataset
MatrixKeywordbis <- as.data.frame(sapply(MatrixKeyword[,2:67], function(x) as.numeric(x)))
Matrix <- cbind(Title=MatrixKeyword$Title, MatrixKeywordbis)
Matrixbis <- rowsum(Matrix[,2:67], Matrix$Title)

# remove from Matrixbis keywords that are not realevant 
# for Index+ authors keywords mixed
#MatrixFinal <- subset(Matrixbis, select=-c(ARTICLE, PRIORITY.JOURNAL, REVIEW, CONFERENCE.PAPER, HUMAN, FEMALE, MALE,ANIMAL, NONHUMAN))
# for authors keywords only
MatrixFinal <- Matrixbis

# Create the matrix
Matriceplot <- cor(MatrixFinal, method = "spearman")

# Plot the matix
corrplot(Matriceplot, method = "color", type = "upper", tl.col = "black", tl.cex=0.5, col = brewer.pal(n = 11, name = "RdBu"), diag = F)

####################### Plot the matrix with ggplot ##############################
# the matrix need to be melt first ( package reshape2 require)
melted_matrice <- melt(Matriceplot)
head(melted_matrice)

# Get only the upper triangle
get_upper_tri <- function(Matriceplot){
  Matriceplot[lower.tri(Matriceplot)]<- NA
  return(Matriceplot)
}
upper_tri <- get_upper_tri(Matriceplot)
upper_tri
# Get only the lower triangle
get_lower_tri<-function(Matriceplot){
  Matriceplot[lower.tri(Matriceplot)] <- NA
  return(Matriceplot)
}

lower_tri <- get_lower_tri(Matriceplot)
lower_tri

# choose upper or lower triangle there
melted_matrice <- melt(lower_tri, na.rm = TRUE)

ggplotmatrix <- ggplot(data = melted_matrice, aes(x= reorder(Var2, desc(Var2)), Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low="darkblue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1), space = "Lab", name="Spearman\nCorrelation") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(vjust = 1, size = 12, hjust = 1,))+
  labs(y="", x = "")
show(ggplotmatrix)
ggplotly(ggplotmatrix)

# to export the table
#write.table(melted_matrice, file = "Matrix_table sup5.csv", sep = ",", row.names = F)


######################### test count  #########################
testmatrix <- MergeDataKeywordList %>%
  select(TI,KeywordsCorrected)
testmatrix <- testmatrix %>% group_by(TI) %>%
  summarise(KeywordsCorrected = paste(KeywordsCorrected, collapse = ";"))

#Split Column "KeywordsCorrected" in row by the separator ";", remove leading white space to generate list
X <- lapply(testmatrix$KeywordsCorrected, function(x){unlist(strsplit(x, split = ";"))})
unlist(strsplit(testmatrix$KeywordsCorrected[1:2], split = ";"))

countMat<- matrix(0, length(unique(unlist(strsplit(testmatrix$KeywordsCorrected, split = ";")))),
                  length(unique(unlist(strsplit(testmatrix$KeywordsCorrected, split = ";")))))
colnames(countMat) <- unique(unlist(strsplit(testmatrix$KeywordsCorrected, split = ";")))
rownames(countMat) <- unique(unlist(strsplit(testmatrix$KeywordsCorrected, split = ";")))

addCounts <- function(keywordString){
  keywordsSep<-unlist(strsplit(keywordString, split = ";"))
  countMat[keywordsSep,keywordsSep] <<- countMat[keywordsSep,keywordsSep]+1
}

n<- lapply(testmatrix$KeywordsCorrected, addCounts)
rm(n)
topWordCount <- countMat[TopKeywordsList$TopKeywords,TopKeywordsList$TopKeywords]
fibreCounts <- countMat["FIBRE",TopKeywordsList$TopKeywords]

# Plot the matix
coul <- colorRampPalette(brewer.pal(8, "Oranges"))(8)
heatmap(topWordCount, Colv = NA, Rowv = NA, revC = F, col = coul)