# Amunategui - Pipeline
source('~/R Work/GDAwithR/code/StartLibraries.R')

# Auto readers
# doc 12 -----------------------------------------------------------------------------
readLines('http://math.ucdenver.edu/RTutorial/titanic.txt', n=10)
Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt', 
                              sep='\t',
                              header=TRUE)
str(Titanic_dataset)
# without conversions to factors
Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt', 
                              sep='\t', 
                              header=TRUE, 
                              stringsAsFactors=FALSE)
str(Titanic_dataset)
# 
readLines('https://www.umass.edu/statdata/statdata/data/actg320.dat', n=10)
# pass column names to the read.table statement
actg320_colnames <- c('id','time','censor','time_d','censor_d','treatment',
'treatment_group','strat2','sex','raceth','ivdrug','hemophil','karnof','cd4',
'priorzdv','age')
actg320 <- read.table('https://www.umass.edu/statdata/statdata/data/actg320.dat',
                      col.names = actg320_colnames)
head(actg320)

# read.csv
Iris_dataset <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/iris/bezdekIris.data', 
                         header=FALSE)
head(Iris_dataset)

# preapering for readr
mix_dataset <- data.frame(
  id=c(10,20,30,40,50),
  gender=c('male','female','female','male','female'),
  some_date=c('2012-01-12','2012-01-12','2012-12-01','2012-05-30','2013-12-12'),
  value=c(12.34, 32.2, 24.3, 83.1, 8.32),
  outcome=c(1,1,0,0,0))
write.csv(mix_dataset, './data/mix_dataset.csv', row.names = FALSE)
# read it back and check types
mix_dataset <- read.csv('./data/mix_dataset.csv', stringsAsFactors = FALSE)
str(mix_dataset)
# date is read as character

# readr
library(readr)
mix_dataset <- readr::read_csv('./data/mix_dataset.csv')
str(mix_dataset)
# define the types
mix_dataset <- read_csv('./data/mix_dataset.csv', col_types='ncDni')
str(mix_dataset)

# fread - Fastest of readers
readLines('./data/mix_dataset.csv', n=10)
library(data.table)
mix_dataset <- fread('./data/mix_dataset.csv', showProgress=TRUE, data.table=FALSE)
str(mix_dataset)
# read only a subset of columns
mix_dataset <- fread(
  './data/mix_dataset.csv',
  data.table = FALSE,
  select = c('value', 'outcome')
)
head(mix_dataset)

# Automating data exploration: dates
# doc 13 -----------------------------------------------------------------------------
source('~/R Work/GDAwithR/code/PipelineFunctions.R')
path_and_file_name <- './data/mix_dataset.csv'
# quick peek at top lines
print(readLines(path_and_file_name, n=5))
# format the dates
mix_dataset <- read.csv(path_and_file_name, stringsAsFactor=FALSE)
head(Fix_Date_Features(mix_dataset))

# Automating data exploration: raw text
# doc 14 -----------------------------------------------------------------------------
source('~/R Work/GDAwithR/code/PipelineFunctions.R')
Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt', 
                              sep='\t', 
                              header=TRUE, 
                              stringsAsFactors=FALSE)
head(Titanic_dataset)
# count words in Name
Titanic_dataset_temp <- Titanic_dataset
Titanic_dataset_temp$Word_Count <- sapply(strsplit(Titanic_dataset_temp$Name, " "), 
                                          length)
head(Titanic_dataset_temp$Word_Count)
# count characters in Name
Titanic_dataset_temp <- Titanic_dataset
Titanic_dataset_temp$Character_Count <- nchar(as.character(Titanic_dataset_temp$Name))
head(Titanic_dataset_temp$Character_Count)
# Get the first word of Name
Titanic_dataset_temp <- Titanic_dataset
Titanic_dataset_temp$First_Word <- sapply(strsplit(as.character(Titanic_dataset_temp$Name), " "), `[`, 1)
head(Titanic_dataset_temp$First_Word)
# What features/variables are candidates to Categorization
Titanic_dataset_temp <- Get_Free_Text_Measures(data_set = Titanic_dataset, 
                                               features_to_ignore = c())
str(Titanic_dataset_temp)

# Automating data exploration: factors
# doc 15 -----------------------------------------------------------------------------
source('~/R Work/GDAwithR/code/PipelineFunctions.R')

# binaryzing data - making dummy features
Titanic_dataset <- read.table('http://math.ucdenver.edu/RTutorial/titanic.txt', 
                              sep='\t', 
                              header=TRUE, 
                              stringsAsFactors=FALSE)
head(Titanic_dataset)
str(Titanic_dataset)
dim(Titanic_dataset)

# Sex and PClass are candidates to binarization
str(Titanic_dataset)
Titanic_dataset_temp <- Binarize_Features(data_set = Titanic_dataset, 
                                          features_to_ignore = c('Name'))
str(Titanic_dataset_temp)
Titanic_dataset_temp <- Binarize_Features(data_set = Titanic_dataset, 
                                          features_to_ignore = c('Name'),
                                          leave_out_one_level = TRUE)
str(Titanic_dataset_temp)
# Or use caret::dummyVars
