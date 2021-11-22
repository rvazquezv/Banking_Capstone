###############################################################################
###############################################################################

## ML algorithm for Harvardx Capstone Course, Choose Your Own project
## Version: 1.0
## Author: Ruben Vázquez del Valle
## Comments: 10M version of movielens dataset has been prepared by edx and splitted in two datasets:
##           - 
##           This script takes xxx minutes in Intel(R) Core i7 8565U CPU @ 1.8GHzs (4 CPUs) 16 GB RAM 
##                         and xxx minutes in Intel(R) Core i7 5500U CPU @ 2.4GHzs (4 CPUs) 8  GB RAM 
##
##
## INTERNET CONEXION NEEDED TO RUN SUCCESFULLY THE SCREEN TO download source date
###############################################################################
###############################################################################


###############################################################################
###############################################################################
##
## Libraries needed for the project
##
###############################################################################
###############################################################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(fastDummies)) install.packages("fastDummies", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")



library(tidyverse)
library(caret)
library(data.table)
library(rvest)
library(httr)
library(ggcorrplot)
library(fastDummies)
library(e1071)
library(rpart)
library(randomForest)


###############################################################################
###############################################################################
##        Functions specifically created for the project
###############################################################################
###############################################################################

## substrRight function to substract n last characters on a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


## substrLeft function to substract n first characters on a string
substrLeft <- function(x, n){
  substr(x, 1, n)
}



## Standarize function to standarize values on a numeric vector
Standarize <- function(x){
  (x-mean(x))/sd(x)
}




################################################################################################################
# Download source data set, and split it between training, test and validation sets (final hold-out test set)
################################################################################################################

# Note: this process could take a couple of minutes


# URL with original dataset to be downloaded
url<-"https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#"
url2<-"https://archive.ics.uci.edu/ml/machine-learning-databases/00222/"
  
  
file<-"bank-additional.zip"

dfile<-paste(url2,file,sep="")


# Create input folder on working directory to download source files
mydir<-getwd()
inputdir<-paste(mydir,"/Input",sep="")
dir.create(path=inputdir,mode="0777")

# Download source files, unzip them and erase temp files
dl <- tempfile()
download.file(dfile,dl)
untar(dl,exdir=inputdir)
rm(dl)


#Looking for downloading directory
dirs<-list.dirs()
idx_dirs<-grep(paste("/Input",substrLeft(file,15),sep="/"),dirs)
dw_dir<-dirs[idx_dirs]

#List files tp download
dw_files<-list.files(path=dw_dir)

#Download n all files and save them into Input_file_n  tibbles
for(i in (1:length(dw_files))){
exp1 <- expression(paste("Input_file",as.character(i),sep="_"))                                  # Create name of the tibble expression
exp2<- expression(read_delim(file=paste(dw_dir,dw_files[i],sep="/"),delim=";",col_names=TRUE))   # Create read_delim expression
z<-paste(eval(exp1),exp2,sep="<-")                                                               # Create assignation expression as a string
eval(parse(text=z))                                                                              # Evaluate expression
}


#rm(Input_file_1,Input_file_2,Input_file_3)



###############################################################################
###############################################################################
##
## Source Data exploratory analysis
##
###############################################################################
###############################################################################


#Confirming Input_file_3 is a subset of Input_file_1 while anti_join() return all rows from x without a match in y.
# Thus if previous antijoin is 0 means one set is contained in the other one
nrow(anti_join(Input_file_3,Input_file_1,by=NULL))




head(Input_file_1)
summary(Input_file_1)


#Splitting columns between numerical and categorical
idx_isc<-which(sapply(Input_file_1,is.character)==TRUE)

colnames(Input_file_1[idx_isc])

# Get values for character columns
for(i in (1:length(idx_isc))){
  #The idea is building for each character column the sentence that will cast it to a factor column. For instance:
  # Input_file_1$job<-factor(Input_file_1$job)
  # Instead of building one by one and taking advantage of having them stored in idx_isc, I will build the expression.
  exp1 <- expression(paste("Input_file_1",colnames(Input_file_1[idx_isc])[i],sep="$"))       # Input_file_1$**** expression
  exp2<-expression(factor(eval(parse(text=eval(exp1)))))                                     # factor(Input_file_1$****) expression
  exp3<-expression(levels(factor(eval(parse(text=eval(exp1))))))                             # levels(factor(Input_file_1$****)) expression
  cat(colnames(Input_file_1[idx_isc])[i],"possible values are :",eval(exp3),"\n")
  z<-paste(eval(exp1),exp2,sep="<-")                                                         # Create a<-factor(a) expression 
  eval(parse(text=z))    
}  

###############################################################################
###############################################################################
##
## Partitioning source data
##
###############################################################################
###############################################################################



# Validation set will be 20% of Source data according to Pareto principle
set.seed(1978, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = Input_file_1$y, times = 1, p = 0.2, list = FALSE)
bankraw<- Input_file_1[-test_index,]
Validation <- Input_file_1[test_index,]



head(bankraw)
summary(bankraw)



head(Validation)
summary(Validation)



###############################################################################
##
##        Preprocessing, cleaning, wrangling and creating partitions
##        
###############################################################################

## Selecting a random seed to allow replicability
set.seed(1978, sample.kind="Rounding")


## Creating training a testing partitions on bankraw dataset 

test_index <- createDataPartition(y = bankraw$y, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- bankraw[-test_index,]
test_set <- bankraw[test_index,]



str(train_set)

## Bivariate Analysis on numerical variables  
colnames_num<-colnames(train_set[-idx_isc])
length(colnames_num)


for(i in (1:length(colnames_num))){
  print(    
    train_set%>%ggplot(aes(eval(parse(text=eval(colnames_num[i]))))) +
    geom_histogram() +
    facet_wrap(~y) +
    ggtitle(paste("Bivariate Analysis on variable",colnames_num[i])) +
    xlab(colnames_num[i]) +
    labs(caption = paste("Figure ",i))
  )
}


## Bivariate Analysis on categorical variables  
  
colnames_fac<-colnames(train_set[idx_isc])
n<-length(colnames_fac)-1
  
  for(i in (1:n)){
    print(    
      train_set%>%ggplot(aes(y)) +
        geom_bar() +
        facet_wrap(~eval(parse(text=eval(colnames_fac[i])))) +  
        ggtitle(paste("Bivariate Analysis on variable",colnames_fac[i])) +
        labs(caption = paste("Figure ",10+i))
    )
  }
  

## Multivariate Analysis on numerical variables  
  
  
Y<-as.numeric(train_set$y)
M<-cbind(train_set[-idx_isc],Y)
MC<-cor(M)
ggcorrplot(MC)+
  ggtitle("Pearson's Correlation between numerical variables")+
  labs(caption = "Figure 21")

MCS<-cor(M,method = "spearman")

ggcorrplot(MCS)+
  ggtitle("Spearman's Correlation between variables")+
  labs(caption = "Figure 21.b")




## Multivariate Analysis on categorical variables  

new_train_set<-train_set%>%select(colnames_fac) %>% 
               dummy_cols(., select_columns = c("job","marital","education","contact","month","poutcome"))

Y<-as.numeric(train_set$y)
MC1<-cbind(new_train_set[,12:35],Y)
MC2<-cbind(new_train_set[,36:50],Y)

MCC1<-cor(MC1)
ggcorrplot(MCC1)+
  ggtitle("Pearson's Correlation between categorical variables")+
  labs(caption = "Figure 22")



MCC2<-cor(MC2)
ggcorrplot(MCC2)+
  ggtitle("Pearson's Correlation between categorical variables")+
  labs(caption = "Figure 23")






## Selecting principal variables, correlation greater than an arbitrary threshold

idx1<-which(abs(MC[,11])>=0.3)

idx2<-which(abs(MCC1[,25])>=0.3)

idx3<-which(abs(MCC2[,16])>=0.3)

## Taking off dependent variable from independent variables list

fidx1<-grepl("Y",names(idx1))

fidx2<-grepl("Y",names(idx2))

fidx3<-grepl("Y",names(idx3))

## Storing principal variables into final_vars vector

final_vars<-c(names(idx1[!fidx1]),names(idx2[!fidx2]),names(idx3[!fidx3]))

final_vars

###############################################################################
##
##        Modelling
##      
###############################################################################


##        I.) BASELINE 

predict_baseline<- sample(c("no"), length(test_set$y), replace = TRUE) %>%
  factor(levels = levels(test_set$y))

confusionMatrix(predict_baseline,test_set$y)$overall["Accuracy"]

confusionMatrix(predict_baseline,test_set$y)$byClass["Balanced Accuracy"]


##        II.) LOGISTIC REGRESSION

train_glm<-train(y~.,method="glm",data=train_set)
predict_glm<-predict(train_glm,test_set)

confusionMatrix(predict_glm,test_set$y)$overall["Accuracy"]


##        III.) K NEAREST NEIGHBOURS

train_knn<-train(y~.,method="knn",data=train_set)
predict_knn<-predict(train_knn,test_set)

confusionMatrix(predict_knn,test_set$y)$overall["Accuracy"]



##        IV.) DECISSION TREES

train_rpart<-train(y~.,method="rpart",tuneGrid=data.frame(cp=seq(0,0.05,len=25)),data=train_set)
y_hat_rpart<-predict(train_rpart,test_set)

confusionMatrix(y_hat_rpart,test_set$y)$overall["Accuracy"]


##        V.) RANDOM FOREST

nodesize<-seq(11,31,10)
acc<-sapply(nodesize,function(ns){
train(y~.,method="rf",tuneGrid=data.frame(mtry=2),nodesize=ns,data=train_set)$results$Kappa
})

train_rf<-train(y~.,method="rf",tuneGrid=data.frame(mtry=2),nodesize=nodesize[which.max(acc)],data=train_set)
y_hat_rf<-predict(train_rf,test_set)

confusionMatrix(y_hat_rf,test_set$y)$overall["Accuracy"]








z<-train_set%>%select(duration,pdays,euribor3m,nr.employed,poutcome,y)

z_test<-test_set%>%select(duration,pdays,euribor3m,nr.employed,poutcome,y)





##        II.) LOGISTIC REGRESSION

z_glm<-train(y~.,method="glm",data=z)
predict_glm_z<-predict(z_glm,z_test)

confusionMatrix(predict_glm_z,test_set$y)$overall["Accuracy"]


##        III.) K NEAREST NEIGHBOURS

z_knn<-train(y~.,method="knn",data=z)
predict_knn_z<-predict(z_knn,z_test)

confusionMatrix(predict_knn_z,test_set$y)$overall["Accuracy"]



##        IV.) DECISSION TREES

train_rpart<-train(y~.,method="rpart",tuneGrid=data.frame(cp=seq(0,0.05,len=25)),data=z)
y_hat_rpart<-predict(train_rpart,z_test)

confusionMatrix(y_hat_rpart,test_set$y)$overall["Accuracy"]




##        V.) RANDOM FOREST

nodesize<-seq(1,51,10)
acc<-sapply(nodesize,function(ns){
  train(y~.,method="rf",tuneGrid=data.frame(mtry=2),nodesize=ns,data=z)$results$Kappa
})

train_rf_z<-train(y~.,method="rf",tuneGrid=data.frame(mtry=2),nodesize=nodesize[which.max(acc)],data=z)
y_hat_rf_z<-predict(train_rf_z,test_set)

confusionMatrix(y_hat_rf,test_set$y)$overall["Accuracy"]



