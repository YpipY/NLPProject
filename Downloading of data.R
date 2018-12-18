### Downloading the needed books from the Gutenberg corpus
## Installing and loading packages
install.packages('devtools')
install.packages('Rtools')
library(devtools)
library(rJava)
devtools::install_github("bnosac/RDRPOSTagger")
devtools::install_github("ropensci/tokenizers") # If it does not install, see if it downloads Rtools and try to install again
library(RDRPOSTagger)
library(tokenizers)

install.packages('pacman')
library(pacman)
p_load(tidyverse,gutenbergr,tidyverse,qdapDictionaries,gsubfn,stringi,rENA,plyr,Metrics,caret,magrittr,lmerTest,boot,e1071,ROCR,pROC,OptimalCutpoints)

# Creates an folder for the data and the books, if one does not exist
dir.create(file.path(getwd(), "Books"), showWarnings = FALSE)
dir.create(file.path(getwd(), "Centroids"), showWarnings = FALSE)

## Setting up the varables and needed metadata to loop though the corpus and download the needed book
metadata<-gutenberg_metadata
nA=0
nS=1
nF=1
nP=1
nH=1
t1=1
t=1

## Looping though the metadata
for (i in (1:50000)){
  # Stops the search once 200 books are found
  if(nA==200) {
    break 
  }
  # Getting the book number
  t1<-metadata[i,1]
  t<-t1$gutenberg_id
  # Only enclude english books
  if (metadata[i,5] == 'en'){
    # Only include intries with text
    if(metadata[i,8] == TRUE){
      # Don't include if does not have a genre
      if(is.na(metadata[i,6]) == FALSE){
        # Include 50 science fiction books
        if(metadata[i,6] == 'Science Fiction'){
          if(nS<=50){
            # Downloading the specified text 
            book<-gutenberg_download(t, strip = T, meta_fields = c("title", 'author', 'gutenberg_bookshelf', 'language'))
            write.csv(book, file = paste(getwd(),'/Books/',t,'.csv',sep=''))
            # Increase the counters
            nS=nS+1
            nA=nA+1
          }
        # Include 50 fantasy books
        }
        if(metadata[i,6] == 'Fantasy'){
          if(nF<=50){
            # Downloading the specified text 
            book<-gutenberg_download(t, strip = T, meta_fields = c("title", 'author', 'gutenberg_bookshelf', 'language'))
            write.csv(book, file = paste(getwd(),'/Books/',t,'.csv',sep=''))
            # Increase the counters
            nF=nF+1
            nA=nA+1
          }
        # Include 50 philosophy books
        }
        if(metadata[i,6] == 'Philosophy'){
          if(nP<=50){
            # Downloading the specified text 
            book<-gutenberg_download(t, strip = T, meta_fields = c("title", 'author', 'gutenberg_bookshelf', 'language'))
            write.csv(book, file = paste(getwd(),'/Books/',t,'.csv',sep=''))
            # Increase the counters
            nP=nP+1
            nA=nA+1
          }
        # Include 50 historical fiction books
        }
        if(metadata[i,6] == 'Historical Fiction'){
          if(nH<=50){
            # Downloading the specified text 
            book<-gutenberg_download(t, strip = T, meta_fields = c("title", 'author', 'gutenberg_bookshelf', 'language'))
            write.csv(book, file = paste(getwd(),'/Books/',t,'.csv',sep=''))
            # Increase the counters
            nH=nH+1
            nA=nA+1
          }
        }
      }
    } 
  }
}

# Setting up a list of the dataframes containing the books downloaded
listData = list.files(paste(getwd(),'/Books',sep=''))
books = lapply(paste('Books/',listData,sep=''), read.csv)

### Creating a dataframe of cooccurrence at sentence level for each book

## Looping though the books
# This can take about a day when running through 200 books
# Don't mind the warrnings
for (b in c(1:length(books))){
  # Getting the necessary meta data
  id=as.character(books[[b]][1,2])
  title=as.character(books[[b]][1,4])
  author=as.character(books[[b]][1,5])
  genre=as.character(books[[b]][1,6])
  
  # Getting the text
  text=paste(books[[b]][,3],collapse = ' ')
  # Cleaning the text
  text=gsub('-',' ', text)
  text=gsub('"',' ', text)
  text=gsub('_',' ', text)
  text=gsub('\\\\',' ', text)
  text=gsub('[*]',' ', text)
  text=gsub('[?]',' ', text)
  text=gsub('[!]',' ', text)
  text=gsub('[;]',' ', text)
  text=gsub('[,]',' ', text)
  text=gsub('\\.{4}',' ', text)
  text=gsub('[(]',' ', text)
  text=gsub('[)]',' ', text)
  text=gsub('[:]',' ', text)
  text=gsub("[']",' ', text)
  text=tolower(text)
  
  # Splitting the text into sentences
  textsplit = strsplit(text, "[.]")  
  
  # Creating a dataframe for the matrix of cooccurrence
  df = data.frame(matrix(ncol = 21, nrow = length(textsplit[[1]])))
  colnames(df) = paste(c("id","title","author","genre","sentence","DET","NOUN","VERB","ADJ",'PROPN','ADP','AUX','CONJ','INTJ','NUM','PRON','SCONJ','PUNCT','PART','SYM','X'))
  
  # Inserting the book metadata
  df$id = id
  df$title = title
  df$author = author
  df$genre = genre
  df$sentence = c(1:length(textsplit[[1]]))
  df[is.na(df)] <- 0
  
  # Setting up the parts of speech annotator 
  unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
  
  # Loop through the sentences and write the amount of POS of each category for each sentence
  for (i in c(1:length(textsplit[[1]]))){
    # Annotating parts of speech
    unipostags <- rdr_pos(unipostagger, textsplit[[1]][[i]])
    # Saving the amounts
    if (length(unipostags$pos)>1){
      for (a in c(1:length(unipostags$pos))){
        df[[unipostags$pos[a]]][i] = df[[unipostags$pos[a]]][i] + 1
      }
    }
    # Print text so we can see how far the processing is
    print(paste("Processing sentence: ",i," of", length(textsplit[[1]]), " of book", b))
  }
  
  # Changing the dataformate for ENA
  df$id=as.factor(id)
  df$title=as.factor(title)
  df$author=as.factor(author)
  df$genre=as.factor(genre)
  
  # Creating the cooccurrence vectors from the cooccurrence matrix
  accum = ena.accumulate.data(
    units = df[,c("title","sentence")],
    conversation = df[,c("sentence","id")],
    metadata = df[,c("author","genre")],
    codes = df[,c("DET","NOUN","VERB","ADJ",'PROPN','ADP','AUX','CONJ','INTJ','NUM','PRON','SCONJ')],
    window.size.back = 1
  )
  
  # Runing the ENA analysis
  set = ena.make.set(enadata = accum, dimensions = 2)
  
  # Saving the centroids and some of the cooccurrence data
  centroids = data.frame(matrix(ncol = 7, nrow = length(textsplit[[1]])))
  colnames(centroids) = paste(c("id","title","author","genre","sentence","x","y"))
  centroids$id = id
  centroids$title = title
  centroids$author = author
  centroids$genre = genre
  centroids$sentence = c(1:length(textsplit[[1]]))
  centroids$x = set$centroids[,1]
  centroids$y = set$centroids[,2]
  centroids = cbind(centroids,select(df,"DET","NOUN","VERB","ADJ",'PROPN','ADP','AUX','CONJ','INTJ','NUM','PRON','SCONJ'))
  
  # Saving the dataframe of the centroids as a csv 
  write.csv(centroids, file = paste(getwd(),'/Centroids/',id,'.csv',sep=''))
}

### Ploting some of the ENA data for demonstration purposes
# Creating a plot of the each sentence position in the ENA vector space
# Can take a while
set = ena.make.set(enadata = accum, dimensions = 2)
plot = ena.plot(set)
sentence.points = set$points.rotated[set$enadata$units$sentence > 0,]
plot = ena.plot.points(plot, points = sentence.points)
print(plot)

# Creating a plot of the ENA network
plot = ena.plot(set)
unitNames = set$enadata$units
sentence = unitNames$sentence > 0  
sentencepoints = set$points.rotated[sentence,]
plot = ena.plot.group(plot, sentencepoints, labels = "1",
                      colors = "red", confidence.interval = "box")
sentencelineweights = set$line.weights[sentence,]
sentencelineweightsmean = colMeans(sentencelineweights)
plot = ena.plot.network(plot, network = sentencelineweightsmean)
print(plot)

### Simple statical messures
## Not very comprehensive
# Loading the data
listData = list.files(paste(getwd(),'/centroids',sep=''))
centroids = lapply(paste('centroids/',listData,sep=''), read.csv)
centroids = rbind.fill(centroids)

# Making the folds
predictors<- rep(NA, nrow(centroids))

centroidsP = centroids
levels(centroidsP$genre) <- c("Other", "Philosophy", "Other", "Other")

Folds<-createFolds(unique(centroidsP$id), k=5, list=T, returnTrain=F)
Folds<-lapply(Folds, function(x) unique(centroidsP$id)[x])

# quad model

# Lopping through the folds
for (i in Folds) {
  # Getting the data for the folds
  trainFold = dplyr::filter(centroidsP, !id %in% i)
  testFold = dplyr::filter(centroidsP, id %in% i)
  # Making the classifier 
  classifier = glm(genre ~ x + y, data= trainFold, family="binomial")
 
  pTest <- inv.logit(predict(classifier, newdata = testFold, allow.new.levels=TRUE))
  
  predictors[centroidsP$id %in% i] = pTest
}
predictors =as.numeric(predictors)
# Making ROC curve
rocCurve <- roc(response = centroidsP$genre, predictor= predictors)
auc(rocCurve)
ci(rocCurve)
plot(rocCurve, legacy.axes=TRUE)
?pROC
median(predictors)

optimal.cutpoint.CB <- optimal.cutpoints(X = elas ~ status, tag.healthy = 0,
                                             methods = "CB", data = elas, pop.prev = NULL, categorical.cov = "gender",
                                             control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)

# Checking the predictions
predictors[predictors > 0.2] = "Philosophy"
predictors[predictors <= 0.2] = "Other"

confusionMatrix(data = as.factor(predictors), reference = as.factor(centroidsP$genre), positive = "Philosophy")
