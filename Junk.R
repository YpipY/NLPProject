# Old junk I might still need
textsplit[[1]][[1]]
sentence=strsplit(textsplit[[1]][[1]], " ")  
sentence=sentence[[1]]
sentence=sentence[is.word(sentence)]

textsplit[[1]]

acq <- "Gulf Applied Technologies Inc said it sold its subsidiaries engaged in pipeline and terminal operations for 12.2 mln dlrs. The company said the sale is subject to certain post closing adjustments, which it did not explain. Reuter."

unipostag_types <- c("ADJ" = "adjective", "ADP" = "adposition", "ADV" = "adverb", "AUX" = "auxiliary", "CONJ" = "coordinating conjunction", "DET" = "determiner", "INTJ" = "interjection", "NOUN" = "noun", "NUM" = "numeral", "PART" = "particle", "PRON" = "pronoun", "PROPN" = "proper noun", "PUNCT" = "punctuation", "SCONJ" = "subordinating conjunction", "SYM" = "symbol", "VERB" = "verb", "X" = "other")

sentences <- tokenize_sentences(acq, simplify = TRUE)
sentences

unipostagger <- rdr_model(language = "English", annotation = "UniversalPOS")
unipostags <- rdr_pos(unipostagger, textsplit[[1]][[4]])
unipostags

nrow(filter(unipostags,pos=='NOUN'))

unipostags$word.type <- unipostag_types[unipostags$word.type]

unipostags$pos

length(unipostags$pos)
nrow(unipostags)
unipostags$pos[2]
df[[unipostags$pos[1]]][2]
df[[unipostags$pos[1]]][2] = df[[unipostags$pos[1]]][2] + 1