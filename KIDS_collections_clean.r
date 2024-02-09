# ASGT '24


library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(odbc)
library(fuzzyjoin)
library(data.table)


# KIDS_clean <-function(file,)
  
  # file type IN EXIT, TEST, ASGT
  
  # if grade = 'IT', K-12 Kansas At-Risk Program Participation (AM or D39 = NULL) is not valid for this grade level.
  #Preschool-Aged At-Risk Program Participation (BF or D58= NULL) is not valid for this grade level.
  
  
  getwd()
setwd('C:/Users/jonathan.wilson/Downloads')
extract <- read.delim("KIDS_Collection01 - 2024-02-08T080558.762.txt",
                      sep = "\t",
                      colClasses = "character",
                      skip = 1,
                      header = FALSE) %>%
  mutate_all(as.character)


# for asgt
extract_clean <- extract %>%
  filter(!is.na(V1),
         V2 != '8421',
  ) %>%
  mutate(
    V58 = ifelse(V10 == "IT", "", V58),
    V39 = ifelse(V10 == "IT|UG", "", V39),
    V91 = ifelse(row_number() != n(), "X", V91)
    
  )


#   # type
type <- extract_clean[2, 1]

# smallest<- min(x$column_name)
# largest<- max(x$column_name)
timestamp <- Sys.time() %>% format(format = "%m%d%y%H%M")


filename<- paste0(type,".USD500", timestamp,".txt")





# this gets/ slices the header row
head <- read.delim("KIDS_Collection01 - 2024-02-08T080558.762.txt",
                   sep = "\t",
                   colClasses = "character",
                   header = FALSE) %>%
  slice(1)

batch<- head[,1] %>%
  substr(start = 24, stop = 33)

z<- rbind(head, extract_clean)

footer <-paste0('TT ', batch," ",nrow(z))

z[nrow(z), 1] <- footer
type <- z[2, 1]

filename<- paste0(type,".USD500", timestamp,".txt")


write.table(z,
            file =filename,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE
)



# note this is for exit only
# EXITS #####################################



extract <- read.delim("KIDS_Collection01 - 2024-02-08T160034.444.txt",
                      sep = "\t", 
                      colClasses = "character",
                      skip = 1,
                      header = FALSE) %>%
  mutate_all(as.character)


extract_clean <- extract %>%
  filter(!is.na(V1),
         V2 != '8421',
         V27 != '23') %>%
  mutate(
    V58 = ifelse(V10 == "IT", "", V58),
    V39 = ifelse(V10 == "IT|UG", "", V39),
    V91 = ifelse(row_number() != n(), "X", V91),
    V30 = ifelse(V30 == "0.00" & V10 %in% c("09", "10", "11", "12", "UG"), "25.00", V30),
    V29 = ifelse(V30 == "0.00" & V10 %in% c("09", "10", "11", "12", "UG"), "0.00", V29)
  )
# this slices the header row
head <- read.delim("KIDS_Collection01 - 2024-02-08T160034.444.txt",
                   sep = "\t", 
                   colClasses = "character",
                   header = FALSE) %>%
  slice(1)

batch<- head[,1] %>% substr(start = 24, stop = 33)

z<- rbind(head, extract_clean)

footer <-paste0('TT ', batch," ",nrow(z))

z[nrow(z), 1] <- footer
type <- z[2, 1]

# smallest<- min(x$column_name)
# largest<- max(x$column_name)
timestamp <- Sys.time() %>% format(format = "%m%d%y%H%M")


filename<- paste0(type,".USD500", timestamp,".txt")


write.table(z,
            file =filename,
            sep = "\t",
            row.names = FALSE,
            col.names = FALSE)
