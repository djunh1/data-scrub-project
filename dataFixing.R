
library("stringr")
library("data.table")
library(dplyr)
setwd("C:/Users/gongshow/Documents/R Projects/Paggen Data")

bData <- read.csv("C:/Users/gongshow/Documents/R Projects/Paggen Data/bData_version1.csv",
                  stringsAsFactors=FALSE, header=TRUE)

#This part will look at the Name_2 column and perform the following:
#  * Check Name_2 column for multiple names
#  * Split the names and add a new row with the same data, except for the name
#    in Name_2 shall be the second name from this data element
#  * Order the Data frame in alphabetical order by the company name

bData$NAME_2A = as.character(lapply(strsplit(as.character(bData$NAME_2), split="/"), "[", 1))
bData$Name_2B = as.character(lapply(strsplit(as.character(bData$NAME_2), split="/"), "[", 2))


for(i in 1:nrow(bData)){
        if(!is.na(bData[i, 28])){
             bData <- rbind(bData, bData[i ,])
             bData[i ,4] <- bData[i,27]
             bData[nrow(bData) , 4] <- bData[i,28] 
             }
}
bData <- arrange(bData, as.character(bData$NAME_1))
bData$NAME_2A <-NULL
bData$Name_2B <-NULL

#This will add a new row for each person called to attention zHv
for(i in 1:nrow(bData)){
        if(grepl( "zHv", bData$NAME_3[i]) ==TRUE)
        {
               if(grepl(bData$NAME_3[i], bData$NAME_3[i+1] )){
                       bData$NAME_3[i+1] <- NA
               }
               bData<- rbind(bData, bData[i,])
               bData[nrow(bData), 4] <- bData[i,5]
                }        
}

bData <- arrange(bData, as.character(bData$NAME_1))

#If an email field has two emails, split the emails and create a new row 
# with the new email and idential information

bData$email_2A = as.character(lapply(strsplit(as.character(bData$EMAIL), split=";"), "[", 1))
bData$email_2B = as.character(lapply(strsplit(as.character(bData$EMAIL), split=";"), "[", 2))

for(i in 1:nrow(bData)){
        if(!is.na(bData[i, 28])){
                if(grepl(bData$email_2B[i],bData$email_2B[i+1])){
                        #bData$email_2B[i+1] <- NA
                        #bData[i+1,20] <-bData[i,27]
                        
                }
                if(grepl(bData$email_2B[i],bData$email_2B[i+2])){
                        #bData$email_2B[i+2] <- NA
                        #bData[i+2,20] <-bData[i,27]
                        
                }
                
                
                bData <- rbind(bData, bData[i ,])
                #bData[i ,20] <- bData[i,27]
                bData[nrow(bData) , 20] <- bData[i,20] 
                }
}

bData$email_2A <-NULL
bData$email_2B <-NULL
bData <- arrange(bData, as.character(bData$NAME_1))

#Takes the 5 digit number from rows telephon 2 and/or telephone 3 and places them into
# Column Kunde

for(i in 1:nrow(bData)){
        if(grepl("K-[0-9]{5}", bData$TELEFON_2[i])){
           bData$Kunde[i]<- str_extract_all(bData$TELEFON_2[i],"K-[0-9]{5}")
        } 
        if(grepl("K-[0-9]{5}", bData$TELEFON_4[i])){
           bData$Kunde[i]<- str_extract_all(bData$TELEFON_4[i],"K-[0-9]{5}")
        }  
        if(grepl("K-[0-9]{5}", bData$TELEFAX_2[i])){
           bData$Kunde[i]<- str_extract_all(bData$TELEFAX_2[i],"K-[0-9]{5}")
        } 
}
#Splits out the "K-" so only the number remains
bData$Kunde <- gsub('K-','',bData$Kunde)


#Collapses the list into single cells (needed to export the data to CSV)
bData$Kunde <- sapply(bData$Kunde,FUN = paste,collapse = ";")

#Takes the 4 digit number from the phone/ fax columns and places the number into
#The interresent cell
for(i in 1:nrow(bData)){
        if(grepl("I-[0-9]{4,5}", bData$TELEFON_2[i])){ 
                bData[i,14]<- str_extract_all(bData$TELEFON_2[i],"I-[0-9]{4,5}")
         }
         
        else if(grepl("I-[0-9]{4,5}", bData$TELEFON_4[i])){ 
                bData[i,14]<- str_extract_all(bData$TELEFON_4[i],"I-[0-9]{4,5}")     
        }  
        else if(grepl("I-[0-9]{4,5}", bData$TELEFAX_2[i])){ 
                bData[i,14]<- str_extract_all(bData$TELEFAX_2[i],"I-[0-9]{4,5}")
        } 
}

#Removes the I- so only the number remains
bData$Interessent <- gsub('I-','',bData$Interessent)

#Output the data to CSV
write.csv(bData, file="paggenData", row.names=FALSE)




