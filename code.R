### Final Project Introduction
The purpose of this project (CYO) is to create an algorithm for recommending classes for users based on other programs they have completed and the scores they received. I will be using a subset of modified data to protect the actual users infomation. I will use the recommenderlab for making my recommendation and use different models to identify the best model with the lowest error acuracy.

```{r Install Packages and libraries, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
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

```
```{r Load Data, include=FALSE}
cat("Movielens Preptime Times:")  
system.time({
  # MovieLens 10M dataset:
  if(!file.exists("OnlineStudentHistoryml-10m.csv")){
    download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", destfile = "ml-10m.zip")