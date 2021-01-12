
# Natural Language Processing
# Master in Data Science
# Santiago Monteso Fernandez

### INTRODUCTION ###

# This document show how to perform some analyses of PDF documents at a word level
# in order to identify the topic and key words of each document and possible copies.

# The code is based on the following web pages:

# - https://rpubs.com/rgcmme/IS-HO1
# - https://data.library.virginia.edu/reading-pdf-files-into-r-for-text-mining/
# - https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf

# for the sake of simplicity some comments have been kept.

### PREPARATION ###

# Check working directory #
getwd()
setwd("C:/Users/santi/Desktop/upm_data_science/course_20_21/semester_1/intelligent_systems/practicas/procesado_lenguaje")

# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")

## Load needed libraries ##
library(tm)
library(ggplot2)
library(wordcloud)
# the library RWeka only works if both RStudio and Java are 64 bits
library(RWeka)
library(reshape2)

#### READING PDFs ####

# Library to read PDFs
library(pdftools)

# Vector of PDF files names 
files <- list.files(pattern = "pdf$")
files

# Function "pdf_text." for extracting text 
data <- lapply(files, pdf_text)

# This creates a list object with five elements, one for each document
length(data)

# Number of pages of each PDF file. 
lapply(data, length)
# text in R variable
data

# Create a corpus, which is basically a database for text, using the library "tm" 
corpus <- Corpus(URISource(files),
               readerControl = list(reader = readPDF,language = "eng"))

# writeCorpus(corpus)

## Inspect raw corpus without transformations ##
# how many entries there are in our corpus
length(corpus)
# Taking a look at the corpus
summary(corpus)
# Inspecting the first entry (document + metadata)
inspect(corpus[1])
# Taking a closer look at the first entry
inspect(corpus[[1]])
# Taking a closer look at the first entry metadata
meta(corpus[[1]])
# Taking a closer look at an element of the first entry metadata
meta(corpus[[1]])$id
meta(corpus[[1]])$language     


### CREATE A RAW TERM DOCUMENT MATRIX ###

# create a TMD
tdm = TermDocumentMatrix(corpus)
# take a look at the summary of the TDM (sparsity)
tdm
# How many terms have been identified in the TDM?
length(dimnames(tdm)$Terms)
# How frequently do those terms appear? 
freq=rowSums(as.matrix(tdm))
#head(freq,10)
#tail(freq,10)
# If we plot those frequencies ordered, we can see how the corpus behaves following Zipf’s law.
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")
# And we can analyse the ten most frequent terms 
# as well as check that 11240 terms out of 29924 only appear once in our corpus.
# Ten most frequent terms
tail(sort(freq),n=10)
# Ten least frequent terms
head(sort(freq),n=10)
# Number of terms only appearing once
sum(freq == 1)


### CREATE A TDM AFTER APPLYING TRANSFORMATIONS TO THE CORPUS ###

# We can see the different transformations that can be applied to a document
getTransformations()
## Create a TDM with transformations ##
# Let’s create another term document matrix but now after applying transformations to our document.
tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = T,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))
# Let’s take a look at the summary of the new TDM.
tdm
# How many terms have been identified in the TDM?
length(dimnames(tdm)$Terms)
# We can see how many terms have been identified in the TDM using the length function again.
length(dimnames(tdm)$Terms)
head(dimnames(tdm)$Terms,10)
tail(dimnames(tdm)$Terms,10)
# How frequently do those terms appear? Let’s sum the content of all terms (i.e., rows)
# and see the frequency of the terms just shown.
freq=rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)
# We can plot those frequencies ordered again.
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")
# And we can analyse the ten most frequent terms as well as check that 6388 terms out of 19064 only appear once in our corpus.
# Ten most frequent terms
tail(sort(freq),n=10)
# Ten least frequent terms
head(sort(freq),n=10)
# Number of terms only appearing once
sum(freq == 1)

# We can also show the most frequent terms and their frequencies in a bar plot.
freq=rowSums(as.matrix(tdm))
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")


## Make an association analysis ##

# We can make the analysis of what words are more frequently associated with others.
# Let’s analyse those terms frequently associated with “star”.
asoc.term = as.data.frame(findAssocs(tdm,"switch", 0.8))
asoc.term$names <- rownames(asoc.term) 
asoc.term
# We can also put them in a bar graph.
ggplot(asoc.term, aes(reorder(names,switch), switch)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Correlation") +
  ggtitle("\"switch\" associations")

## Create a word-document frequency graph ##

# Now let’s make a word-document frequency graph that shows in a graphical way the frequency of terms in documents.
# The first thing that we need to do, since we have a highly sparse TDM, 
# is to remove sparse terms using the removeSparseTerms function.
tdm.small = removeSparseTerms(tdm,0.1)
dim(tdm.small)
tdm.small
# This way, instead of 19063 terms we have only the 28 terms that are more frequent in the corpus.
# We can clearly see how our new TDM is less sparse.
inspect(tdm.small[1:10,])
# We create a matrix were we count all the appearances of terms in the documents.
matrix.tdm = melt(as.matrix(tdm.small), value.name = "count")
head(matrix.tdm)
# And we plot the word-document frequency graph. 
# The grey color means that the term does not appear in the document. 
# Besides, a stronger red color indicates a higher frequency.
ggplot(matrix.tdm, aes(x = Docs, y = Terms, fill = log10(count))) +
  geom_tile(colour = "white") +
  scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  ylab("Terms") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

## Create a word cloud ##

# Let’s choose a nice range of blue colors for the wordcloud. 
# You can invoke the display.brewer.all function to see the whole palette.
# Let’s also set the random number generator seed to some value (this way, we will always get the same word cloud).
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
set.seed(1234)
# Due to some issue with the newest versions of the tm package (0.7 and 0.7-1) 
# in order to create n-grams VCorpus must be used instead of Corpus. 
# Another option to solve the problem is to go back to version 0.6-2 of the tm package.
corpus.ngrams = VCorpus(URISource(files),
       readerControl = list(reader = readPDF,language = "eng"))
tdm.unigram = TermDocumentMatrix(corpus.ngrams,
                         control=list(stopwords = T,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))
# Now we extract the frequency of each term
freq = sort(rowSums(as.matrix(tdm.unigram)), decreasing = T)
# Finally, we invoke the wordcloud function to make the wordcloud with those terms that appear at least 400 times.
word.cloud=wordcloud(words=names(freq), freq=freq,
                     min.freq=400, random.order=F, colors=pal)



## Create a bigram wordcloud ##

# To create a bigram wordcloud, we apply transformations to the original corpus. 
corpus.ngrams = tm_map(corpus.ngrams,removePunctuation)
corpus.ngrams = tm_map(corpus.ngrams,removeNumbers)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(corpus.ngrams,
                                control = list (tokenize = BigramTokenizer))
# We extract the frequency of each bigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
# And we plot the wordcloud.
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
# We could also plot the most frequent bigrams in a bar graph.
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")

## Create a trigram wordcloud ##

# To create a trigram wordcloud, the approach is the same but this time we tell the n-gram tokenizer to find trigrams.
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram = TermDocumentMatrix(corpus.ngrams,
                                 control = list(tokenize = TrigramTokenizer))
# We extract the frequency of each trigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.trigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)
# And we plot the wordcloud.
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)
# We could also plot the most frequent trigrams in a bar graph.
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent trigrams")







