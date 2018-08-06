rm(list=ls())

# MF Offer Document Similarity Algorithm Starts Here ......


# Upload the consolidated csv file consisting of multiple SID text

ic1 <- read.csv("C:/Users/debapriyag@icrakpo.com/Desktop/Book1.csv")
attach(ic1)

# Create a generic vector using list function for the uploaded csv file

rm(a)
a<-list()
for (i in 1:nrow(ic1)){
  a[[i]]<-ic1[i,6]    #column 6 is freezed for text comparison
}
length(a)

#STEP 2: Define the query doc & create a corpus

# 2(a) Naming the docs
doc.list<-a
(n.docs <- length(doc.list))
(names(doc.list)<-paste0("", c(1:n.docs)))


#STEP 2(b): Defining the query 

query<-(a[1])  #5th row (fund name) will act as the reference against which other rows (funds) will be compared..

# STEP 2(c): Creating a corpus with docs & query

library(tm)  # load text mining package

my.docs <- VectorSource(c(doc.list, query))
my.docs$Names <- c(names(doc.list), "query")
my.corpus <- Corpus(my.docs)
my.corpus

#my.corpus[[2]]
# STEP 3: Generate Term Document Matrix with different options (like stemming, weighting, removing numbers or punctuations)
## A document term matrix is simply a matrix with documents as the rows and terms as the columns
## and a count of the frequency of words as the cells of the matrix.

ic1.tdm <- TermDocumentMatrix(my.corpus,
                              control = list(removePunctuation = TRUE,
                                             stopwords = TRUE, stemming=TRUE, 
                                             tolower =TRUE, removeNumbers=FALSE,
                                             stripWhitespace=TRUE, skipWords=TRUE, 
                                             wordLengths = c(3,Inf)))
# Inspecting the Term Document Matrix (no of rows should be changed with new dataset)

inspect(ic1.tdm[1:6, 1:6])
term.doc.matrix <- as.matrix(ic1.tdm)


# STEP 3(a): DEFINE WEIGHTING FUNCTION
get.tf.idf.weights <- function(tf.vec, df) {
  weight = rep(0, length(tf.vec))
  weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(n.docs/df)
  weight
}

# STEP 3(b): GET WEIGHTS OF TERM DOC MATRIX
get.weights.per.term.vec <- function(tfidf.row) {
  term.df <- sum(tfidf.row[1:n.docs] > 0)
  tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df)
  return(tf.idf.vec)
}

#dim(term.doc.matrix)


# STEP 4: ADD WEIGHTS AS PER THE WEIGHTING FUNCTION 
tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec))


# STEP 5: DEFINE COLUMN NAMES
colnames(tfidf.matrix) <- colnames(term.doc.matrix)

# tfidf.matrix[0:3, ]

# STEP 6: SCALE THE WEIGHTED MATRIX
tfidf.matrix <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
tfidf.matrix[0:4, ]

# STEP 7: DOT PRODUCT OF 'ALL DOCS' vs 'QUERY DOC' 
query.vector <- tfidf.matrix[, (n.docs + 1)]
tfidf.matrix <- tfidf.matrix[, 1:n.docs]
doc.scores <- t(query.vector) %*% tfidf.matrix


# STEP 8: STORE THE SCORES IN A DATAFRAME
results.df <- data.frame(doc = names(doc.list), score = t(doc.scores))#, text = unlist(doc.list))
results.df <- results.df[order(results.df$doc), ]

# VIEW THE RESULTS
options(width = 2000)
pr <- print(results.df, row.names = FALSE, right = TRUE, digits = 2)

results1 <- data.frame(ID,Title, Abstract, results.df$score)
write.csv(results1, "C:/Users/debapriyag@icrakpo.com/Desktop/Results.csv",row.names = FALSE)


# MF offer document similarity algorithm ends here =====================================

# We can also find associations with a word, after specifying a correlation limit

#findAssocs(ic1.tdm, 'long', .6)

