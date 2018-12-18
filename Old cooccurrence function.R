### Creating a dataframe of cooccurrence at sentence level for each book
#getting the necessary meta data
book1=books[[1]]
id=as.character(books[[1]][1,2])
title=as.character(books[[1]][1,4])
author=as.character(books[[1]][1,5])
genre=as.character(books[[1]][1,6])

text=paste(books[[1]][,3],collapse = ' ')
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
text

textsplit = strsplit(text, "[.]")  

text1=gsub('\\.',' ', text)
text1=tolower(text1)
textfull=strsplit(text1,' ')
utext=unique(unlist(textfull, use.names = FALSE))
is.word  = function(x) x %in% GradyAugmented
utext=utext[is.word(utext)]

df = data.frame(matrix(ncol = length(utext)+5, nrow = length(textsplit[[1]])))
colnames(df) = paste(c("id","title","author","genre","sentence",utext))

df$id = id
df$title = title
df$author = author
df$genre = genre
df$sentence = c(1:length(textsplit[[1]]))

for (i in c(1:length(textsplit[[1]]))){
  sentence = strsplit(textsplit[[1]][[i]], " ") 
  sentence=sentence[[1]]
  sentence=sentence[is.word(sentence)]
  if (length(sentence)>1){
    for (a in c(1:length(sentence))){
      df[[sentence[a]]][i] = 1
    }
  }
  print(paste("Processing sentence: ",i," of", length(textsplit[[1]])))
}

df[is.na(df)] <- 0

df$id=as.factor(id)
df$title=as.factor(title)
df$author=as.factor(author)
df$genre=as.factor(genre)

accum = ena.accumulate.data(
  units = df[,c("title","sentence")],
  conversation = df[,c("sentence","id")],
  metadata = df[,c("author","genre")],
  codes = df[,utext],
  window.size.back = 1
)
