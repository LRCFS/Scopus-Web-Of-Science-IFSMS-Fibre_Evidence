
library(matrixStats)

# create temporary test dataset, can be replaced by importing a dataset in the appropriate format
# FA is for reference and FN for sample

Filter <- c('FA','FN','FA','FN')
Date <- c('20220205','20220205','20220206','20220206','20220207','20220207','20220208','20220208')
Experiment <- c('1','1','1','1','2','2','2','2')
Filtration <- c('Ndata','Ndata','Ndata','Sample')

datatest <- cbind(Filter,Date,Experiment,Filtration)
datatest <- as.data.frame(datatest)

# note first set of 4 masses based on actual measurments, the last remaning 4 randomly created

mass1 <- c(1360.1,	1360.3,	1361.0,	1428.1, 1359.1,	1360,	1361.2,	1428.4)
mass2 <- c(1360.2,	1360.2,	1361.2,	1428.2,1360.3,	1360.1,	1361.3,	1428.6)
mass3 <- c(1360.2,	1360.3,	1361.2,	1428.1,1360,	1360.4,	1361.4,	1428.8)
mass4 <- c(1360.3,	1360.2,	1361.1,	1428.1,1359.9,	1360.2,	1361,	1428.6)
mass5 <- c(1360.1,	1360.4,	1361.0,	1428.0,1360.2,	1360,	1360.8,	1428.5)

massAll <- cbind(mass1,mass2,mass3,mass4,mass5)

datatest <- cbind(datatest,massAll)

# dataset now created

###### Data processing ######

# mean for each rows (selected columns)
datatest$meanValue <- rowMeans(subset(datatest, select = c(mass1: mass5)), na.rm = TRUE)

# standard deviation for each row (selected columns)
datatest$standardDeviation <- rowSds(as.matrix(datatest[,c(5,6,7,8,9)]))

# number of replicate measurements
datatest$Number <- rowSums( !is.na(datatest[,5:9]))

# SEM per row
datatest$SEM <- datatest$standardDeviation/sqrt(datatest$Number)

# create an identifier
datatest$ID <- datatest$Filter

# split out each set using the ID
p1 = datatest %>% filter(ID == "FA") %>% select(Experiment,meanValue,SEM)
p2 = datatest %>% filter(ID == "FN") %>% select(Experiment,meanValue,SEM)

# and merge
merge.dat = data.frame(p2[,c("Experiment")], p1$meanValue, p2$meanValue, p1$SEM, p2$SEM)
names(merge.dat) = c("Experiment","P1","P2","SEM1", "SEM2")

# add the filter ID
merge.dat <- cbind(Filter,merge.dat)

# Calculate difference and U.F
datatest2 <- merge.dat %>% mutate(Diff =  P2 -P1, U.F = sqrt((SEM1*SEM1)+(SEM2*SEM2)))

# split out each set by reshaping
p3 <- datatest2 %>%
  select(Filter, Experiment, Diff, U.F)
p4 <- reshape(p3, idvar = "Experiment", timevar = "Filter", direction = "wide")
p4 <- data.frame(p4)

# Calculate difference and U.F
p4$Wf <- p4$Diff.FN-p4$Diff.FA
p4$U.C <- sqrt((p4$U.F.FA*p4$U.F.FA)+ (p4$U.F.FN*p4$U.F.FN))


  