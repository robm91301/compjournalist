library("gmodels")
library("dplyr")
options(width=200)

# unzip the data file
unzip("data.zip")
# read the data in
df <- read.table("arrest_data.csv", header=TRUE,sep=",",stringsAsFactors=FALSE, as.is=TRUE)
# the columns our for testing
#  [1] "Report.ID"                "Arrest.Date"             
#  [3] "Time"                     "Area.ID"                 
# [5] "Area.Name"                "Reporting.District"      
# [7] "Age"                      "Sex.Code"                
# [9] "Descent.Code"             "Charge.Group.Code"       
#[11] "Charge.Group.Description" "Arrest.Type.Code"        
#[13] "Charge"                   "Charge.Description"      
#[15] "Address"                  "Cross.Street"            
#[17] "Location"

# put the data in a form I can use
df$Days<- weekdays(as.Date(df$Arrest.Date,"%m/%d/%Y"))
dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
df$Days <- factor(df$Days, levels=dayLabs)
genderLabs<-c("M","F")
df$Sex.Code<- factor(df$Sex.Code,levels=genderLabs)
# remove anything without a description
df <- df[!(df$Charge.Group.Description== ""), ]
# convert times into to hour buckets
df$Time <- round(df$Time/100)*100

# make this field more useful.....not sure what an infraction or dependent is yet.
df$Arrest.Type.Code <- recode(df$Arrest.Type.Code , "I" =  "Infraction" , "D" = "Dependent" , "F" = "Felony" , "M" = "Misdemeanor" , "O" = "Other")
# change codes into human reable races
df$Descent.Code <- recode(df$Descent.Code, "A" = "Other Asian" , "B" = "Black", "C" = "Chinese", "D" = "Cambodian",  "F" = "Filipino", "G" =  "Guamanian", "H" = "Hispanic/Latin/Mexican", "I" = "American Indian/Alaskan Native",  "J" = "Japanese", "K" = "Korean", "L" = "Laotian", "O" = "Other", "P" = "Pacific Islander", "S" = "Samoan",  "U" = "Hawaiian", "V" = "Vietnamese", "W" = "White", "X" = "Unknown", "Z" = "Asian Indian")
# do crosstab on gender
CrossTable(x=df$Sex.Code, y=df$Days, dnn=c("Gender","Days"),prop.r=FALSE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE, max.width=120)
y = xtabs(~ Arrest.Type.Code + Days + Sex.Code , df)
print(y)
y = xtabs(~ Descent.Code + Days + Arrest.Type.Code + Sex.Code , df)
print(y)
