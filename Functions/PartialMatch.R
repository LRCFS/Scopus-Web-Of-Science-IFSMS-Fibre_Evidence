# Function, adapted from https://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
# Function to check for duplicate and partial match between database imports. The generated list can then be used to make the appropriate changes to the lists

#Data loaded in from downloaded files as:
##PercentageUsingTheNet
##ccode

##Here's where the algorithm starts...
##I'm going to generate a signature from country names to reduce some of the minor differences between strings
##In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
##So for example, United Kingdom would become kingdomunited
##We might also remove stopwords such as 'the' and 'of'.

# Separator <- list(c("\\. |\\.| | \\& |\\: | \\ - |\\-|\\ -|\\- |\\-"))

signature=function(x){
  #  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  sig=paste(sort(unlist(strsplit(x," "))),collapse = '')
  #  sig=paste(x)
  #  sig=paste(x, sep =" ", collapse = " ")
  sig=x
  return(sig)
}

FunctionPartialMatchNumber = 4.0

partialMatch=function(x,y,levDist = FunctionPartialMatchNumber){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  
  return(matched)
}