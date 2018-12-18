# Installing and loading wordnet
# Will require wordnet, download available form here: https://wordnet.princeton.edu/download/current-version
install.packages('wordnet')
library(wordnet)
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1") 
setDict("C:/Program Files (x86)/WordNet/2.1/dict")