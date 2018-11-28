### Downloading the needed books from the Gutenberg corpus
## Loadind packages
install.packages('pacman')
library(pacman)
p_load(tidyverse,gutenbergr,tidyverse)

??gutenbergr

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
            write.csv(book, file = paste(getwd(),'/Data/',t,'.csv',sep=''))
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
            write.csv(book, file = paste(getwd(),'/Data/',t,'.csv',sep=''))
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
            write.csv(book, file = paste(getwd(),'/Data/',t,'.csv',sep=''))
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
            write.csv(book, file = paste(getwd(),'/Data/',t,'.csv',sep=''))
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
listData = list.files(paste(getwd(),'/Data',sep=''))
books = lapply(paste('Data/',listData,sep=''), read.csv)

View(books[[1]])


test1=paste(test$text, collapse = '')
library(dplyr)
count(metadata,gutenberg_bookshelf == 'Historical Fiction')
test1
