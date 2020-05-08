library(tidytext)
library(tidyverse)
library(tidygraph)
library(tm)
library(SnowballC)
library(randomForest)
#Read in the dataset
posting <- read.csv("fake_job_postings.csv")

#Start the cleaning process
#Examine dataset
str(posting)

#Examine count ratio of fraudulent cases
posting %>% 
  group_by(fraudulent) %>% 
  count()

#We have a large class imbalance
#We must be careful to ensure that our splitting criteria is random enough

#Data Cleaning
corpus <- Corpus(VectorSource(posting$description))

inspect(corpus[1:5])

#Convert to lower cast
corpus <- tm_map(corpus, tolower)

#Remove Punctuationa and inspect
corpus <- tm_map(corpus, removePunctuation)

inspect(corpus[1:5])

#Remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords(kind = "en"))

#Stem document
corpus <- tm_map(corpus, stemDocument)

library(wordcloud)

#Create a wordcloud of clean document
#wordcloud(corpus,min.freq = 0,random.order = FALSE)


#Create document frequency
freq <- DocumentTermMatrix(corpus)
freq

#remove sparsity
freq_sparsed <- removeSparseTerms(freq, sparse = 0.995)

freq_sparsed

#Convert cleaned document to a df
df <- as.data.frame(as.matrix(freq_sparsed))


#Give unique names to colnames
colnames(df) <- make.names(colnames(df))


#Add the fraudelent colum
df$fradulent <- posting$fraudulent


#Remove duplicate column names
colnames(df) <-  make.unique(colnames(df), sep = "_")

#Training the dataset

#Create train and test dataset
library(caTools)
library(caret)


#Convert target variable to factor type
df$fradulent <- as.factor(df$fradulent)

#Split
set.seed(2020, sample.kind = "Rounding")
test_index <- createDataPartition(y = df$fradulent, times = 1, p = 0.1, list= FALSE)
train_set <- df[-test_index, ]
validation <- df[test_index, ]

#Check the split ratio of the target variable
table(train_set$fradulent)
table(validation$fradulent)

library(caret)
#Get a sample number
n <- 1000

#Cross validation contro;
control<- trainControl(method = "cv", number = 5, verboseIter = TRUE)

#Number of trees for each train
grid <-data.frame(mtry = c(1, 5, 10, 25, 50, 100))

#create a sample from n number from dataset
index <- sample(nrow(train_set), n)

#subset data
rf_train_data <- train_set[index, ]

#Train random forest model
subset_train_rf <- train(fradulent ~ ., method = "rf", data = rf_train_data, ntree = 150, trControl = control, tuneGrid = grid)

#Cross validation contro;
control<- trainControl(method = "cv", number = 2, verboseIter = TRUE)

#Tune parameter
#Tuningparameters
grid <- data.frame(mtry = c(100))

#train entire dataset
#Training the model
train_rf <- train(fradulent ~ ., method = "rf", data = train_set, ntree = 150, trControl = control, tuneGrid = grid)
