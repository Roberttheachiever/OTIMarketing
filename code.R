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

