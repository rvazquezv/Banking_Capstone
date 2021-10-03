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
if(!require(genefilter)) install.packages("genefilter", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(rvest)
library(httr)
library(genefilter)

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


                                                                        


#Confirming Input_file_3 is a subset of Input_file_1 while anti_join() return all rows from x without a match in y.
# Thus if previous antijoin is 0 means one set is contained in the other one
nrow(anti_join(Input_file_3,Input_file_1,by=NULL))




head(Input_file_2)
summary(Input_file_2)



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

#Build an index with all character columns in the dataset that will need to become factor
idx_isc<-which(sapply(train_set,is.character)==TRUE)

# Get the columns to be converted to factor and split from the others that do not need to change
df1<-train_set[,idx_isn]
df2<-train_set[,-idx_isn]

l<-sapply(df1,factor)


which(idx_isn==TRUE)

for(i in (1:ncol(x))){
  train_set[,i]<-ifelse(idx_isn[[i]],train_set[,i],factor(train_set[,i]))
}  


str(a)

summary(train_set)
summary(y)



###############################################################################
##
##        Modelling
##        I.) BASELINE
###############################################################################



tt<-colttests(train_set[,1:20],train_set[,21])

x<-as.matrix(train_set[,1:20])
y<-factor(train_set$y)

tt <- colttests(x, y)

tt$p.value

ind <- which(tt$pvalue <= 0.01)


train_set$job<-factor(train_set$job)



fit <- lm(y ~ ., data = train_set)