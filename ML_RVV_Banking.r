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

library(tidyverse)
library(caret)
library(data.table)
library(rvest)
library(httr)


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
exp2<- expression(read_delim(file=paste(dw_dir,dw_files[i],sep="/"),delim=";",col_names=TRUE))  # Create read_delim expression
z<-paste(eval(exp1),exp2,sep="<-")                                                               # Create assignation expression as a string
eval(parse(text=z))                                                                             # Evaluate expression
}


#rm(Input_file_1,Input_file_2,Input_file_3)

#Confirming Input_file_3 is a subset of Input_file_1 while anti_join() return all rows from x without a match in y.
# Thus if previous antijoin is 0 means one set is contained in the other one
nrow(anti_join(Input_file_3,Input_file_1,by=NULL))



head(Input_file_2)
summary(Input_file_2)

