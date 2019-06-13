### Final Project Introduction
#The purpose of this project (CYO) is to create an algorithm for recommending classes for users based on other programs they have completed and the scores they received. I will be using a subset of modified data to protect the actual users infomation. I will use the recommenderlab for making my recommendation and use different models to identify the best model with the lowest error acuracy.

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(useful)) install.packages("useful", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
library(dplyr)
library(stringr)
library(tidyverse)
library(caret)
library(data.table)
library(useful)
library(lubridate)
library(recommenderlab)

#Initial data variables

# Get lastest data available from OSHA 
if(!file.exists("OnlineStudentHistory.csv")){
    download.file("https://github.com/Roberttheachiever/OTIMarketing/OnlineStudentHistory.csv", destfile = "OnlineStudentHistory.csv")}
  
Student_data <- read.csv(file.path("OnlineStudentHistory.csv"))
#review structure of dataset
nrow(Student_data)
ncol(Student_data)
names(Student_data)
str(Student_data)
summary(Student_data)
class(Student_data)

unique_studentIds<-unique(Student_data$StudentID)
       
students <- sample(unique(Student_data$StudentID),100)

student_matrix <- Student_data %>% 
  select(StudentID,CourseID,Rating) %>%
  mutate(Rating = 1) %>%
  spread(CourseID, Rating) %>%
  as.matrix() 

#student_matrix

#Plot StudentData for the count of courses rated:  

Student_data %>% 
  dplyr::count(CourseID) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Course Rating Counts")  

#Plot StudentData for the count of Student ratings:  

Student_data %>% 
  dplyr::count(StudentID) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Student Rating Counts")  

#Plot StudentData for the count of ratings:  

Student_data %>% 
  dplyr::count(Rating) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Ratings Counts")  

summary(Student_data)  
names(Student_data)

### Preparing Data:   
#Clean data remove Student that have only taking 1 course, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
keep_StudentID <- Student_data %>%
  dplyr::count(StudentID) %>%
  filter(n > 1) %>%
  pull(StudentID)

Student_data1 <- Student_data %>% filter(StudentID %in% keep_StudentID)  

summary(Student_data1)  

#Convert to realRatingMatrix
#1. I used dcast.data.table to cast the data.frame as a table  
#2. I used sprintf to convert MovieId's, and UserId's to chr  
#3. I used corner to view a small sample of the data to verify conversion  
#4. I then convert the data ta a matrix and then a realRatingMatrix  

Students_2 <- dcast(StudentID ~ CourseID, data = Student_data1, value.var = "Rating")
dim(Students_2)
class(Students_2)
#view data
require(useful)
corner(Students_2)

#change rownames
rownames(Students_2) <- sprintf("Student_%s",Students_2$StudentID)  
Students_2$StudentID <- NULL  
corner(Students_2)  

#change column Names
colnames(Students_2) <- sprintf("Course_%s",colnames(Students_2))  
corner(Students_2)  

#convert to matrix  
Students_2 <- as.matrix(Students_2)  
dim(Students_2)  
class(Students_2)  

#convert to realRatingMatrix
Students_3 <- as(Students_2 , "realRatingMatrix")  
class(Students_3)  
str(Students_3)  

####Review the prepared dataset  

#Review ratings distribution
hist(getRatings(Students_3), main = "Distribution of Ratings")

#The Distribution of Ratings shows we have 10 different possible ratings from 70 to 100

#```{r Normalized Distribution of Ratings, echo=FALSE }
hist(getRatings(normalize(Students_3)),breaks = 100,main = "Normalized Distribution of Ratings")  

#```{r Visual image of distribution of first 500 users, echo=FALSE}
image(Students_3[1:500,],main = "Visual image of distribution of first 500 users")  

#```{r Avg Ratings of Prepared data, eval=FALSE, include=FALSE}
boxplot(rowMeans(Students_3),main = "Avg Ratings of Prepared data")  

model_data = Students_3
#Create model data, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
dim(model_data)

#Number of Rating remaining in Model Data  
nratings(model_data)  

#### Model dataset Distribution of Ratings  

#Plot the Model Data Course ratings
hist(getRatings(model_data), main = "Distribution of Ratings after removing Outliers")   

#*Note rating of 70 is the most popular.*
  
#image of prepared model dataset, echo=FALSE}
image(as(model_data,"matrix"), main = "Visual image of Rating distribution - Model Data")  

#*Note Prepared Model data appears to be well ditributed. *
  
#  Analyze number of course ratings per user:
table(rowCounts(model_data)) 

#*Note that 11619 have rated 2 classes, we have a few Outlier who have rated 6 classes, will keep for now performance ihas not been a issue
  
#### Create Modeling datasets  

#Create recommender sets, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
set.seed(1)
which_train <- sample( x = c(TRUE, FALSE), size = nrow(model_data), replace = TRUE, prob = c(0.8, 0.2))
head(which_train)
rec_data_train <- model_data[which_train]
rec_data_test <- model_data[!which_train]

#Divide the prepared model data set into train and test sets, 80/20 respectively.  
dim(rec_data_train)`  
dim(rec_data_test)`  


#### Build Models Options 

#Get list of recommender models available
recommender_models <-recommenderRegistry$get_entries(dataType = "realRatingMatrix")  

#show model details, eval=FALSE, include=FALSE}
lapply(recommender_models,"[[","description")   
recommender_models  #Description, references and parameteres of models

#I will use various models then compare prediction accuracies to determine the best algorithim. The available models I will first use a user-based collaborative filtering algorithm (UBCF), then item-based collaborative filtering (IBCF) and item popularity algorithms (POPULAR).

#### User Based Collborative Filtering (UBCF) Model
#Collaborative filtering uses algorithims to filter users ratings to make personalized recommendations from similiar users (definition from whatis.techtarget.com/definition/collaborative-filtering).

#Build User Based Collabrotive model
ubcf_model <- Recommender(data = rec_data_train, method = "UBCF")  
ubcf_model  

# Using the UBCF recommendations 
# Get recommender test set, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
# note UBCF model is a lazy learner technic that must access all data in order to make a prediction
n_recommended <- 10
ubcf_predicted <- predict(object = ubcf_model, newdata = rec_data_test, n = n_recommended)
ubcf_list <- sapply(ubcf_predicted@items, function(x){colnames(model_data)[x]})

#List of recommendation movies for test set users 7 thru 10: 
ubcf_list[7:10]  

#Total number of recommendations by users in test set  
number_of_items = sort(unlist(lapply(ubcf_list, length)), decreasing = TRUE)

table(number_of_items)

#Note that 0, hmmm


#### Create an evaluators scheme:

items_to_keep <- 2
rating_threshold <- 5
n_fold <- 5
eval_sets <- evaluationScheme(data = model_data, method = "cross-validation", train 
                              = percentage_training, given = items_to_keep, goodRating = rating_threshold, k = n_fold)
eval_sets  

#Create evaluation datasets using cross-validation method, keeping **`r items_to_keep`** items and **`r n_fold`** folds with rating threshold of **`r rating_threshold`**  using the recommenderLab evaluationScheme function.  

#Get size of evaluation sets, echo=FALSE}
size_sets <- sapply(eval_sets@runsTrain, length)   
cat("Sizes of Evaluation Sets:\t", size_sets, "\n")   
getData(eval_sets, "train")

#3 sets will be used:
#train = training set  
#known = test set used to build recommendations   
#unknown = test set to test the recommendations   

#Create UBCF Recommender 
#```{r Evaluate UBCF known, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
model_to_evaluate <-"UBCF"
model_paramenter <- NULL
eval_recommender <- Recommender(data = getData(eval_sets, "train"), method = model_to_evaluate, parameter = model_paramenter)
eval_recommender  

#Calculate the UBCF predictions for known test set  
items_to_recommend <- 2
eval_prediction <- predict(object = eval_recommender, newdata = getData(eval_sets, "known"), n = items_to_recommend, type = "ratings")
eval_prediction 

#Calculate the prediction accuracy for each user in unknown test set :
#  ```{r Calculate the prediction accuracy UBCF, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = TRUE)
head(eval_accuracy)

#Calculate the overall avgerages in unknown test set: 
apply(eval_accuracy,2,mean)

#Calculate the overall accuracy given in unknown test set:
  
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction, data = getData(eval_sets, "unknown"), byUser = FALSE)
eval_accuracy

#Note the overall RMSE and the accuracy are good.  

#Using a precicion recall plot to predict accuracy with confusion matrix for known test set  

#```{r Use prediction recall to predict accuarcy, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
results <- evaluate(x = eval_sets, method = model_to_evaluate, n = seq(10, 100, 10))   

#Evaluate the result with confusion matrix   

#```{r Show Confusion Matrix, echo=FALSE}  
head(getConfusionMatrix(results)[[1]])  
# note first 4 columns cotain the True False Positives

#Sum up the UBCF TP, FP, FN, TN indexes and plot:  
  
#sum up the indexes 
columns_to_sum <- c("TP","FP","FN","TN")  
indicies_summed <- Reduce("+", getConfusionMatrix(results))[,columns_to_sum]  
indicies_summed    

#*Note: it is difficult to visulize the data provided unless the results are plotted.*
  #Create UBCF Receiver operating characteristic (ROC) plot  

#```{r Create UBCF ROC plot, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
plot(results, annotate = TRUE, main = "UBCF - ROC Curve")    

#Note plot shows the relation ship between TPR and FPR  
#At 30 the TPR is close to 0.7 and the FPR is less than 0.4 is good  
#At 40 the TPR is close to 0.7 but the FPR is greater than 0.4 is not as good  

#Plot UBCF Precision/recall to verify accuracy  

#```{r Plot UBCF Precision recall, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}

plot(results, "prec/rec",annotate = 1, main = "UBCF - Precision/recall")   
