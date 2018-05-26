setwd("D:/hk01/")

library("NLP")
library("tm")
library("rJava")
library("Rwordseg")
library("tmcn")
library("RColorBrewer")
library("wordcloud")
library("jiebaRD")
library("jiebaR")
library("devtools")

#loading data
test.data <- read.csv("offsite-tagging-test-set (1).csv")
training.data <- read.csv("offsite-tagging-training-set (1).csv")
training.data$text <- as.character(training.data$text)

#cleaning
#?M???j?p?g?^??,?Ʀr,???I?Ÿ?,???N?r??,space
for (num in 1:nrow(training.data)) {
  clean.data <-  function(x) {
    gsub("[[:punct:]]|[[:space:]]|[A-Za-z]|[0-9]|. ::|?U", "", x)
  }
  training.data$text[num] <- clean.data(training.data$text[num])
}

#write.csv(training.data,"training_data.csv")

# add new words that should not be  separated
sentence <- worker()
insertWords(toTrad(iconv(c("???y"),
                         "big5", "UTF-8"), rev = TRUE))
#?W?[?H?W????
segment.options(isNameRecognition = TRUE)
segmentCN(segment.words)
#?M????????
stop.text <- readLines("stop.txt")#???????w
document.stop.words <- c(NULL)
for (num in 1:length(stop.text))
{
  document.stop.words[num] <- stop.text[num]
}
#print(document.stop.words)

d.corpus <- Corpus(VectorSource(training.data$text))

num.end <- nrow(training.data)
d.corpus <- tm_map(d.corpus[1:num.end], segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) == "n"]
    #num.of.noun <- length(w[names(w) == "n"])#???h?֭ӦW??
    #precent.of.noun <- length(w[names(w) == "n"])/length(w)# ?W???????y?? %
  })
  unlist(noun)
  print(noun)
})

#?D?X?U???J??TF-IDF ??:?Y?r?J?b?Y???󤤪????n??
tdm <-  TermDocumentMatrix(d.corpus,
                           control = list(wordLengths = c(2, Inf), stopwords = document.stop.words))
inspect(tdm)

#Concatenate a vector of strings/character
library("wordcloud")
mat.tdm <- as.matrix(tdm)
mat.total <-  sort(rowSums(mat.tdm), decreasing = TRUE)
source.data <-
  data.frame(word = names(mat.total), freq = mat.total)

#graphic
wordcloud(
  source.data$word,
  source.data$freq,
  min.freq = 5,
  random.order = F,
  ordered.colors = F,
  colors = rainbow(length(row.names(mat.tdm)))
)
