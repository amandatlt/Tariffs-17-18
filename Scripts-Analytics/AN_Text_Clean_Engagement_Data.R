##################################################
#
# Project: Unstructured text analysis of 2017/18 
#         tariff consultation
# Date: 14/12/16
#
##################################################

###--- PACKAGES AND LIBRARIES ---###

.libPaths("P:/User/Amanda.Teo/R/Packages")
library(curl)
library(XLConnect)

###--- LOAD DATA ---###

tmp <- tempfile()
curl_download("https://improvement.nhs.uk/uploads/documents/Annex_A1_-_engagement_annex.xlsx", tmp)
txt <- readWorksheetFromFile(tmp, sheet = "3 - Policy proposal feedback")

###--- CLEAN DATA ---###

#remove first four rows and first column
txt <- txt[-(1:4),-(1)]

#create group header in row 1
fillcol <- function(x) {
  for (i in 1:(length(x)-1)) {
  word <- x[i]
  if(is.na(x[i+1]) == TRUE) x[i+1] <- x[i]
  }
  return(x)
}
txt[1,] <- fillcol(txt[1,])

#drop unimportant groups
dropgrp <- c("Best Practice Tariffs", "The method","National variations and local prices", "Outside of scope")
txt <- txt[, !(txt[1,] %in% dropgrp)]

#name columns
currency_names <- paste0("curr_", c("twoyr_sup", "twoyr_com", 
                                    "hrg4_sup", "hrg4_com", 
                                    "scope_sup", "scope_com",
                                    "op_sup", "op_com", 
                                    "mat_sup", "mat_com", 
                                    "device_sup", "device_com",
                                    "drugs_sup", "drugs_com", 
                                    "fcom"))

impact_names <- c("assess_other_com", "assess_equality_com")
finalcom_names <- c("final_com", "engage_further_com")
demog_names <- paste0("demo_", c("org", "org_type", "org_other"))

col.names <- c(currency_names, impact_names, finalcom_names, demog_names)
names(txt) <- col.names #check that the names match information in the first three rows
txt <- txt[-(1:3),]

#drop entries where org type not indicated
txt <- txt[is.na(txt$demo_org_type) == FALSE, ]

#tidy org type
txt[,"demo_org_type"] <- ifelse(txt[,"demo_org_type"]== "Other (please specify)", 
                                 txt$demo_org_other, 
                                 txt$demo_org_type)
  #types of organisation
    # NHS Provider
    # Independent Provider
    # Professional Body
    # Consultancy
    # Pharmaceutical Company
    # Commissioners
    #Charity

txt[grep("NHS Provider.*(acute|multiple settings)",txt$demo_org_type), "demo_org_type"] <- "NHS provider acute"

#aggregate support from 5 categories to 3
sup.cols <- grep("_sup$", names(txt))
txt[, sup.cols] <- lapply(txt[,sup.cols], function(x) replace(x, grep("Neither", x), "Neutral"))
txt[, sup.cols] <- lapply(txt[,sup.cols], function(x) replace(x, grep("oppose", x), "Oppose"))
txt[, sup.cols] <- lapply(txt[,sup.cols], function(x) replace(x, grep("support", x), "Support"))
