###############################################
#
# project: Compare tariff prices by Monitor
#          from  16/17 to 17/18
#    date: 08/12/16
# details: Cleans raw data containing
#         hrg prices
#
###############################################

###---set up libraries and paths ---###

.libPaths("P:/User/Amanda.Teo/R/Packages")
library(XLConnect)

main.path <- "C:/Users/Amanda.Teo/Desktop/Tariffs-17-18/Scripts-Analytics"
raw.data <- "Data-Raw"
clean.data <- "Data-Processed"

###---clean 16/17 prices---###

#read in data
df.1617 <- readWorksheetFromFile(file.path(main.path, 
                                           raw.data, 
                                           "1617_Annex_A_national_prices_and_national_tariff_workbook.xlsx"),
                                 sheet = "1 APC & OPROC",
                                 rownames = NULL,
                                 startRow = 4,
                                 endCol = 14)

#add column names
colnames(df.1617) <- c("hrgcode", "hrg", "tariff.op", "tariff.dcel", "tariff.dc", "tariff.el",
                       "trim.el", "tariff.nel", "trim.nel","tariff.overtrim", 
                       "nelshortst", "perc.nelshortst","tariff.nelshortst")

#convert tariff from character to numeric
tariffs <- df.1617[, grep("tariff", colnames(df.1617))]

tariffs.new <- lapply( tariffs, function(y) { #function that for each column
  if (is.numeric(y) == TRUE) return(y) #checks if numeric, and if not
  temp <- gsub("[,]", "", y) #remove commas
  temp <- gsub("[-]", 0, temp) #change - to NA
  temp <- as.numeric(temp) #convert column to numeric
  return(temp)
})

tariffs.new <-do.call(cbind, tariffs.new)
df.1617 <- data.frame(df.1617[, !(names(df.1617) %in% names(tariffs))], tariffs.new)

#save data
write.csv(df.1617, file.path(main.path, clean.data, "HRG_Tariffs_1617.csv"), 
          row.names = FALSE,
          na = "NA")

###--- clean 17/18 prices ---###

#load raw data
df.1718 <- readWorksheetFromFile(file.path(main.path, 
                                           raw.data, 
                                           "1718_Annex_B1_-_national_tariff_workbook.xlsx"),
                                 sheet = "1a APC & OPROC 17.18",
                                 rownames = NULL,
                                 startRow = 3,
                                 startCol = 2,
                                 endCol = 14)

#add column names
names(df.1718) <- c("hrgcode", "hrg", "tariff.op", "tariff.dcel", "tariff.dc", "tariff.el",
                    "trim.el", "tariff.nel", "trim.nel","tariff.overtrim", 
                    "nelshortst", "perc.nelshortst","tariff.nelshortst")

#convert tariff from character to numeric
tariffs <- df.1718[, grep("tariff", names(df.1718))]

tariffs.new <- lapply(tariffs, function(y) {
  if (is.numeric(y) == TRUE) return(y)
  y <- gsub("[,]", "", y)
  y <- gsub("[-]", 0, y)
  y <- as.numeric(y)
  return(y)
})

tariffs.new <- do.call(cbind, tariffs.new)
df.1718 <- data.frame(df.1718[, !(names(df.1718) %in% names(tariffs))], tariffs.new)

#save file
write.csv(df.1718, file.path(main.path, clean.data, "HRG_Tariffs_1718.csv"),
          row.names = FALSE,
          na = "NA")
