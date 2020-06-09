library(tabulizer) #library for extracting tables from standard pdf files
library(data.table) #library for fast reading of .csv and .txt files
library(shiny)
library(miniUI)
##################
##Table extraction
##################
#file names can be iterated over by uppercase letters, e.g., <filename>=IMMS Manual Volume II - A Family.pdf
for (i in toupper(letters)){
    file.address=paste("C:/Users/anasr/Desktop/IMMS_Manual_2013+/IMMS Manual Volume II -", i, "Family.pdf", sep=" ")
    #catches errors when the file does not exist
    res=tryCatch(
                #assign(str, value) creates a variable named <str> by <value> 
                assign  (
                            paste(i, ".tables", sep=""),
                            #extract tables from the given pages of the .pdf file
                            extract_tables(file=file.address, pages=c(1,2,3,4,5,6), output="data.frame", encoding="UTF-8")
                        ),
                #if file does not exist, continue
                error=function(e) return("error")
            )
    if (inherits(res, "error")){
        next
    }
}
##########################################################
## NOTICE: tables from familis K,G,W are not read correctly
## K,G only headers are read
## W the first two columns are combined
##########################################################
## POSSIBLE FIX for K,G family ###########################
##########################################################
G_areas=locate_areas("C:/Users/anasr/Desktop/IMMS_Manual_2013+/IMMS Manual Volume II - G Family.pdf", pages=1)
K_areas=locate_areas("C:/Users/anasr/Desktop/IMMS_Manual_2013+/IMMS Manual Volume II - K Family.pdf", pages=c(1,2))
K.tables=extract_tables(file="C:/Users/anasr/Desktop/IMMS_Manual_2013+/IMMS Manual Volume II - K Family.pdf", pages=c(1,2), guess=FALSE, area=K_areas, output="data.frame", encoding="UTF-8")
G.tables=extract_tables(file="C:/Users/anasr/Desktop/IMMS_Manual_2013+/IMMS Manual Volume II - G Family.pdf", pages=1, guess=FALSE, area=G_areas, output="data.frame", encoding="UTF-8")
#########################################################

##################
##Table names
##################
#read table names as a data
table.names.df=fread(file="C:/Users/anasr/Desktop/IMMS_Manual_2013+/table_names_lowerCase.csv", sep=",", header=TRUE)
##################
##Combine tables of the same family into one big table with corresponding names
##################
for (i in toupper(letters)){
    #cathces error when the name does not exist
    temp_tab_lst=tryCatch(
        #treats the string in paste() as a variable name
        get(paste(i, ".tables", sep="")),
        error=function(e) e
    )
    if (inherits(temp_tab_lst, "error")){
        next
    } else{
        #Each family has different number of tables. There may be multiple NA values in table.names.df. To find out the right number of tables in each family, unique names (which excludes multiple NAs) are counted.
        #the value is subtracted by 2 because of TASK ACTIVITIES tables and NA values for all families except "C" where there is no NA values.
        if(i=="C"){
            length_tab_lst=length(unique(table.names.df[[i]]))-1
        }else{
            length_tab_lst=length(unique(table.names.df[[i]]))-2
        }
        temp_df=data.frame(matrix(NA, ncol=2, nrow=0))
        colnames(temp_df)=c("ACTIVITY", "DESCRIPTION")
        for (j in 1:length_tab_lst){
            table.name=table.names.df[[i]][j]
            temp_df=rbind(temp_df, c(table.name, NA), temp_tab_lst[[j]][1:2])
        }
        fwrite(temp_df, file=paste("C:/Users/anasr/Desktop/IMMS_Manual_2013+/Activities.csv"), append=TRUE, sep=",", eol="\n")
        #fwrite(temp_df, file=paste("C:/Users/anasr/Desktop/IMMS_Manual_2013+/", i, "Activities.csv", sep=" "), append=FALSE, sep=",", eol="\n")
    }
}

######################################################
## NOTICE: "W" tables are printed the wrong way
##         "Y" tables are note printed
######################################################