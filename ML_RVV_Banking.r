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

dfile<-paste(url2,"bank-additional.zip",sep="")


# Create input folder on working directory to download source files
mydir<-getwd()
inputdir<-paste(mydir,"/Input",sep="")
dir.create(path=inputdir,mode="0777")

# Download source files, unzip them and erase temp files
dl <- tempfile()
download.file(dfile,dl)
untar(dl,exdir=inputdir)
rm(dl)

dirs<-list.dirs()
idx_dirs<-grep("/Input",dirs)
dirs[idx_dirs]

dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)


dir <- system.file(paste(substrLeft(file,15),".csv",sep=""),mustWork=TRUE)