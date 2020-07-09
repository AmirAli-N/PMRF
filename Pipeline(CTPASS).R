library(data.table)
library(utils)
library(dplyr)
library(textstem)
library(tm)
library(qdap)
library(iterators)
library(itertools)
setwd("C:/Users/anasr/Desktop/AHCMT lab pc/")
######
######
#pipeline for library expenditure summary tables (CTPASS)
######
######
file.names=list.files(pattern = "/CTPASS/Labor ExpenditureSummary/*.csv") #create a list of lbr_exp_sum_csv file names
files=lapply(file.names, fread) #read the files in the file.names list
df=rbindlist(files, use.names = TRUE, fill=TRUE) #bind the dataframes by column names

##extract distinct values of PEC, PECT, Task, and SB1 columns
PEC_class.names=unique(df$`PEC Description`)
PECT_class.names=unique(df$`PECT Description`)
Task.names=unique(df$`Task Description`)
SB1.names=unique(df$`SB1 Program Description`)

##change column types from string to numeric when neccessary
df=type.convert(df) #from utils library, changes data type as fit.

#check whether the conversion was successful
is.numeric(df$FY)
is.numeric(df$Dist)
is.numeric(df$Element)
is.numeric(df$PEC)
is.numeric(df$PECT)
is.numeric(df$Task)
is.numeric(df$`Sub Task`)
is.numeric(df$`Tot Hrs`)
is.numeric(df$`Tot $`)

######
#type conversion for columns Tot Hrs and Tot $ may not be successful because those numeric include ',' in string format
#notice that some also have '()'
######

#replace ',', '(', and ')' by empty sring
df$`Tot Hrs`=sub(",","",df$`Tot Hrs`, fixed=TRUE)
df$`Tot Hrs`=sub("(","",df$`Tot Hrs`, fixed=TRUE)
df$`Tot Hrs`=sub(")","",df$`Tot Hrs`, fixed=TRUE)

df$`Tot $`=sub(",","",df$`Tot $`,fixed=TRUE)
df$`Tot $`=sub("(","",df$`Tot $`,fixed=TRUE)
df$`Tot $`=sub(")","",df$`Tot $`,fixed=TRUE)

#convert Tot Hrs and Tot $ to numeric values and check for success
df$`Tot Hrs`=as.numeric(df$`Tot Hrs`)
is.numeric(df$`Tot Hrs`)
df$`Tot $`=as.numeric(df$`Tot $`)
is.numeric(df$`Tot $`)

#pec.df=df[,c("PEC", "PEC Description","PECT", "PECT Description", "Task", "Task Description", "SB1 Program", "SB1 Program Description")]
pec.df=df[,c("PEC", "PEC Description")]
pec.df=pec.df[!duplicated(pec.df)]

######
######
#pipeline for linking PEC decriptions and Activity Descriptions
#uses PEC description table, pec.df
######
######

act.df=fread(file='/IMMS/activity+description.csv') #read the activity-description file
act.df$ACTIVITY=gsub("Activities", "", act.df$ACTIVITY) #replaces the word 'Activities' by an empty string

######
#this part adds major activity names (in the first column) to the description column
#each major activity has a number of minor activities
#the row with the major activity has an empty string in the description column
######
table_names.ind=which(act.df$DESCRIPTION=="") #find the indexes where major activity names are stored
table_name.iter=iter(table_names.ind) #create an iterator based on table_names.ind
table_name.iter=ihasNext(table_name.iter) #create a wrapper that checks wheter the iterator has a next element
start=nextElem(table_name.iter) #create a start variable equal to the first element in the iterator
while(hasNext(table_name.iter)){ #as long as the iterator has not reached its last element
  finish=nextElem(table_name.iter) #finish is the index of next element in the iterator
  for (i in (start+1):(finish-1)){
    #adds major activity name to minor activity name
    act.df$DESCRIPTION[i]=paste(act.df$ACTIVITY[start], act.df$DESCRIPTION[i], sep = " ")
  }
  start=finish
}

######
#the below function lowers the case, removes punctuations, removes extra spaces, and removes stopwords
######
prepair.func <-function(str){
  str=tolower(str) #change every word to lower case
  str=gsub("\\s*\\([^\\)]+\\)","",as.character(str)) #replace every " (...) " with an empty string
  str=gsub("[[:punct:]]", " ", as.character(str)) #replace punctuatations with a single space
  str=gsub("\\s+"," ",as.character(str)) #reduce multiple spaces between words with a single space
  str=str[!str %in% stopwords("english")]  #remove stopwords from the string
}

#run both description columns through the prepair function
pec.df$`PEC Description`=prepair.func(pec.df$`PEC Description`)
act.df$DESCRIPTION=prepair.func(act.df$DESCRIPTION)

######
#this part checks the spelling of the each description column, the spell checking is interactive. check_spelling_interactive
#identify some words, and asks how to replace them.
######
spell_checked.words=check_spelling_interactive(pec.df$`PEC Description`)
preprocessed(spell_checked.words) #create a list of misspelled words and their replacement
fixit=attributes(spell_checked.words)$correct #call the correction function of the spell_checked.words
pec.df$`PEC Description`=fixit(pec.df$`PEC Description`) #correct the misspelled words

spell_checked.words=check_spelling_interactive(act.df$DESCRIPTION)
preprocessed(spell_checked.words)
fixit=attributes(spell_checked.words)$correct
act.df$DESCRIPTION=fixit(act.df$DESCRIPTION)

#remove major activity names.
act.df=act.df[-table_names.ind,]

##########
##########
#pipeline for similarity score between PEC clasess and Activity codes
##########
##########

###use these two lines if line 59 is uncommented
###pec_descr.df=pec.df[,c("PEC", "PEC Description")]
###pec_descr.df=pec_descr.df[!duplicated(pec_descr.df)]

pec_descr.df=pec.df
act_descr.df=act.df

#each unique PEC class is scored against all the acitivities
pec_act.score=lapply(pec_descr.df$`PEC Description`, function(x) {
  x=strsplit(x, ' ')[[1]] #create a list of all the words in PEC description
  x=lemmatize_strings(x) #use the stem of the word
  score.list=lapply(act_descr.df$DESCRIPTION, function(y){
    y=strsplit(y, ' ')[[1]] #create a list of all the words in Activity code description
    y=lemmatize_strings(y) #use the stem of the word
    common_words=x[x %in% y] #extract common words between two activities
    score=length(common_words)/sqrt(length(y)*length(x)) #evaluate the score
    return (score)
  })
  return(unlist(score.list))
})

pec_act_score.mat=matrix(unlist(pec_act.score), nrow=length(pec_act.score), ncol=length(pec_act.score[[1]]), byrow = TRUE) #create a matrix of the scores with PEC class rows and Activity code columns
row.names(pec_act_score.mat)=pec_descr.df$PEC #assign row names
colnames(pec_act_score.mat)=c(act_descr.df$ACTIVITY) #assign column names

ordered.ind=t(apply(pec_act_score.mat, 1, function(x) order(x, decreasing = TRUE))) #order the scores from largest to smallest
ordered_activity.code=t(apply(ordered.ind, 1, function(x) colnames(pec_act_score.mat)[x])) #extract the ordered activity codes

suggestion_len=10 #sets the length of activity codes suggestion for each PEC class
#create a list of suggestions as a data.frame with the following columns: PEC class, PEC description, suggested Activity codes
pec_act_suggestions.lst=lapply(pec_descr.df$PEC, function(x) as.data.frame(cbind(c(x, rep("", suggestion_len-1)), c(pec_descr.df$`PEC Description`[which(pec_descr.df$PEC==x)], rep("", suggestion_len-1)),ordered_activity.code[toString(x), 1:10])))
pec_act_suggestions.df=rbindlist(pec_act_suggestions.lst) #merge the list of data.frames by rows
colnames(pec_act_suggestions.df)=c("PEC", "PEC_description", "Suggested_Activity_Code") #assign column names to data.frame
#create a list of Activity description for each suggested Activity code
pec_act_suggestions.description=unlist(lapply(pec_act_suggestions.df$Suggested_Activity_Code, function(x) act_descr.df$DESCRIPTION[which(act_descr.df$ACTIVITY==x)[1]]))
pec_act_suggestions.df=cbind(pec_act_suggestions.df, pec_act_suggestions.description) #bind the Activity suggestions to the data.frame

#######
#print results
#######

fwrite(pec_act_suggestions.df, file="/CTPASS/pec_activity_suggestion.csv", append=FALSE, sep=',') #print the data.frame containing PEC class, PEC description, Acitivity code, Activity description
fwrite(df, file="/CTPASS/CTPASS.budget_hours.csv", append=FALSE, sep=',') #print the original .csv merging the labor expenditure summaries