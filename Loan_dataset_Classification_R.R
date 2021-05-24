### Calling all necessary libraries


library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(psych)
library(visualize)
library(Amelia)
library(plotly)


### Reading the loan dataset from csv




loanDF1 <- read.csv("loan_test.csv")
loanDF2 <- read.csv("loan_train.csv")

####  Merging test and train sets

loandDF <- merge(loanDF1,loanDF2, index_col=0)


### removing the indexes

loanDF <- loanDF [-c(1,2)]


#### change the dateformats of effective_date and due_date




