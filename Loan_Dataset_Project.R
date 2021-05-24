### Calling all necessary libraries
### After installing them using install.package("xxxxx")


library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(psych)
library(visualize)
library(Amelia)
library(plotly)
library(fastDummies)
library(corrplot)
library(corrgram)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)

### Reading the loan dataset from csv

loanDF1 <- read.csv("loan_test.csv")
loanDF2 <- read.csv("loan_train.csv")


####  Merging test and train sets

loanDF <- rbind(loanDF1,loanDF2)

### Data cleaning

## 1.Removing index and unnecessary 'X' in front of the dataframe
loanDF <- loanDF[-c(1,2)]



#### 2.Changing effective and due dates in timeseries format


loanDF$effective_date <- strptime(as.character(loanDF$effective_date), "%m/%d/%Y")
loanDF$effective_date <- format(loanDF$effective_date, "%Y-%m-%d")


loanDF$due_date <- strptime(as.character(loanDF$due_date), "%m/%d/%Y")
loanDF$due_date <- format(loanDF$due_date, "%Y-%m-%d")


#### 3. Estimate the day of the week for the loan due dates. (0 is Sunday, 6 is Saturday)

loanDF['duedayofWeek']  <-  as.POSIXlt(loanDF$due_date)$wday
loanDF['dayofWeek']     <- as.POSIXlt(loanDF$effective_date)$wday

loanDF$diff_in_days<- difftime(loanDF$due_date ,loanDF$effective_date , units = c("days"))



#### 4. Create dummy columns for Gender, Education, etc. 

##### Education, loanstatus and Gender

loanDF$loan_status <- factor(loanDF$loan_status)
loanDF$education  <- factor(loanDF$education)
loanDF$Gender     <- factor(loanDF$Gender)


##### 5. remove redundant columns (education, gender, effective date, due date)


loanDF <- loanDF[-c(4,5)]


#### 6. converting "diff_in_days" column to numeric. 

loanDF$diff_in_days <- as.numeric(loanDF$diff_in_days)


#### Some descriptive statistics

summary(loanDF$Principal)
summary(loanDF$terms)
summary(loanDF$age)

dplyr::count(loanDF, Gender, sort = TRUE)
dplyr::count(loanDF, loan_status, sort = TRUE)


#### 

#### Plotting some exploratory data analyses

### 1. simple scatter plot of loan "terms" vs "principal amount" owed 

# step 1. choose x (groupedby gender) and y (grouped by same gender), followed by
## then add color, size of the font, and X and Y-labels. 
# step 2. Repeat the same step for the other gender ( male in this instance)

plot(loanDF$terms [loanDF$Gender == 'male'],
     loanDF$Principal [loanDF$Gender == 'male'], col = 'red',
     xlab = "Terms of loan in months", ylab = "Loan Principal",
     cex =2, font.lab =2)
points(loanDF$terms [loanDF$Gender == 'female'],
       loanDF$Principal [loanDF$Gender == 'female'], 
       col = 'green',cex =2, font.lab =2)



### 2. simple scatter plot of loan "age" vs "principal amount" owed. some inference
### that can be obtained if the loan borrowers borrow late or early. 


plot(loanDF$age [loanDF$Gender == 'male'],
     loanDF$Principal [loanDF$Gender == 'male'], col = 'red',
     xlab = "Age of borrower in months", ylab = "Loan Principal",
     cex =2, font.lab =2)
points(loanDF$age [loanDF$Gender == 'female'],
       loanDF$Principal [loanDF$Gender == 'female'], 
       col = 'green',cex =2, font.lab =2)

### There is no correlation or pattern of the borrower with respect to the age. 


### 3. Pair plot to see if there is any visible correlation among the features

pairs(~ loanDF$Principal + loanDF$terms + loanDF$age
      +loanDF$dayofWeek)



### 4. Comparative hist and boxplots on terms, principals, due of loans based on Gender

par(mfrow=c(3,2))


hist(loanDF$Principal[loanDF$Gender == "male"], main="Male", 
     breaks = c(300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900,950, 1000),
     xlim = c(300,1000), ylim = c(0,150),
     xlab = 'Principal (in $)', ylab = "Count")

hist(loanDF$Principal[loanDF$Gender == "female"], main="Female", 
     breaks = c(300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900,950, 1000),
     xlim = c(300,1000), ylim = c(0,150),
     xlab = 'Principal (in $)', ylab = "Count")

hist(loanDF$terms[loanDF$Gender == "male"], main="Male", 
     breaks = c(5, 10, 15, 20, 25, 30),
     xlim = c(5, 30), ylim = c(0,150),
     xlab = 'Terms of loan', ylab = "Count")

hist(loanDF$terms[loanDF$Gender == "female"], main="Female", 
     breaks = c(5, 10, 15, 20, 25, 30),
     xlim = c(5, 30), ylim = c(0,150),
     xlab = 'Terms of loan', ylab = "Count")


hist(loanDF$age[loanDF$Gender == "male"], main="Male", 
     breaks = c(15:55),
     xlim = c(15, 55), ylim = c(0,30),
     xlab = 'Age of loanseekers', ylab = "Count")

hist(loanDF$age[loanDF$Gender == "female"], main="Female", 
     breaks = c(15:55),
     xlim = c(15, 55), ylim = c(0,20),
     xlab = 'Age of loanseekers', ylab = "Count")


#### 5. Histogram plot to evalute age of borrowers vs principal owed. 

pl <- ggplot(loanDF,aes(x=Principal))
pl + geom_histogram(binwidth=10,aes(fill=..count..)) + xlab('Age of Borrower')+ ylab('Counts')



#### 6. Stacked histogram ggplot to evaluate age of borrowers segregated by Age

age <- ggplot(loanDF,aes(age)) + geom_bar(aes(fill=factor(Gender)),alpha=0.5)

age +   theme_classic()  + ylab("count") 

age + theme(plot.title = element_text(color="red", size=14, face="bold.italic"),
      axis.title.x = element_text(color="blue", size=14, face="bold"),
      axis.title.y = element_text(color="#993333", size=14, face="bold"),
  legend.position="bottom")

## sample data is too small ( N= 400). so no relationship could be established. 

##### 7. correlation between diff_in_days vs principal. 

diff_in_days<- ggplot(loanDF, aes(x = diff_in_days, y = Principal)) 
diff_in_days + geom_point(aes(color=factor(Gender)), ,size=4,alpha=0.6)


# No correlation between principal due and loan due date. 
# That is, do the borrowers wait till the last minute to pay it off was not clearly established. 
# perhaps it can be argued that most borrowers wait till the last 30 days to pay off the loan. 


##### 8. correlation between age and diff_in_days

# do older people pay it off early than younger folks. 

diff_in_days<- ggplot(loanDF, aes(x = age, y = diff_in_days)) 
diff_in_days + geom_point(aes(color=factor(Gender)), size=4,alpha=0.6)


##### 9. correlation plot among continuous variables. 


corrgram(loanDF,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

corr.loanDF <- data.frame(loanDF$age, loanDF$Principal, 
                            loanDF$terms, loanDF$diff_in_days, 
                            loanDF$duedayofWeek, loanDF$dayofWeek)

#### Baseline machine learning algorithms
#### 1. Logistic Regression



#### Esablishing logistic model

log.model <- glm(formula=loan_status ~ . , family = binomial(link='logit'),data = loanDF)

summary(log.model)


#### splitting date into train/test data

set.seed(4321)   # for reproducibility

split = sample.split(loanDF$loan_status, SplitRatio = 0.70)

loanDF.train = subset(loanDF, split == TRUE)
loanDF.test = subset(loanDF, split == FALSE)


#### Prediction using Test cases

final.log.model <- glm(formula=loan_status ~ . , family = binomial(link='logit'),data = loanDF.train)
summary(final.log.model)


fitted.probabilities <- predict(final.log.model,newdata=loanDF.test,type='response')
summary(fitted.probabilities)


fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)

misClasificError <- mean(fitted.results != loanDF.test$loan_status)


table(loanDF.test$loan_status, fitted.probabilities > 0.5)
#### The table indicates reasonably high true predictions (and less false predictions)

#### 2. Decision Tree Algorithm

loan_tree <- rpart(loan_status ~ . , method='class', data= loanDF)

printcp(loan_tree)


plot(loan_tree, uniform=TRUE, main="Loan Tree")
text(loan_tree, use.n=TRUE, all=TRUE)

# Visualization from rpart is generally bad, instead can be plotted in rpart.plot

prp(loan_tree)


#### 3. Random Forest Algorithm

loan.rftree <- randomForest(loan_status ~ .,   data=loanDF)

print(loan.rftree)   # view results


importance(loan.rftree) # importance of each predictor




