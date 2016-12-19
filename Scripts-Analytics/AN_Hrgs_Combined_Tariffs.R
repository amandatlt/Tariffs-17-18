###############################################
#
# project: Compare tariff prices by Monitor
#          from  16/17 to 17/18
#    date: 08/12/16
# details: Creates base data frame for scatter
#          plot in Shiny    
#
###############################################

rm(list = ls())

###---LIBRARIES AND PATHS ---###

.libPaths("P:/User/Amanda.Teo/R/Packages")
library(XLConnect)
library(dplyr)
library(ggvis)
library(stringr)

main.path <- "C:/Users/Amanda.Teo/Desktop/Tariffs-17-18/Scripts-Analytics"
raw.data <- "Data-Raw"
clean.data <- "Data-Processed"

###--- LOAD AND CLEAN HRP MAP DATA ---###

#load data
map <- readWorksheetFromFile(file.path(main.path, 
                                       raw.data, 
                                       "HRG4__201516_Reference_Costs_Grouper_Roots_v1.0.xlsx"),
                             sheet = "HRG Roots")

#keep 11/12 and 14/15 labels (corresponds to the 16/17 and 17/18 tariffs)
map <- map[,grep("11.12|14.15|Root", names(map))]

#rename dataframe
names(map) <- c("root", "hrgcode.1112", "hrg.1112", "hrgcode.1415", "hrg.1415")

#remove all rows with NA in all hrg related columns
map <- map[(rowSums(is.na(map[, grep("hrg", names(map)) ])) != 4), ]

#remove those with root titled "various" or "new"
map <- map[!(map$root == "Various" | map$root == "New"),]

###--- CREATE 14/15 SUBSET ---###

#take out 14/15 hrgs
map_1415 <- map[,grep("root|1415", names(map))]

#tidy
map_1415 <- map_1415[is.na(map_1415$hrgcode.1415) != TRUE,]
map_1415$root <- gsub(" ", "", map_1415$root)

#split up roots (b/c some hrgs have more than 1 root)
root.split <- strsplit(map_1415$root, "/")
maxlen <- max(sapply(root.split, length))
root.split <- lapply(root.split, function(x) {
  x <- c(x, rep(NA,maxlen - length(x)))
})
root.split <- do.call(rbind, root.split)
colnames(root.split) <- paste("split", 1:maxlen, sep = ".")
map_1415 <- data.frame(map_1415, root.split, stringsAsFactors = FALSE)

rm(root.split)

#reshape wide dataset into long (i = hrg and j = no. of roots)
map_1415 <- reshape(map_1415,
                    varying = names(map_1415)[grep("split", names(map_1415))],
                    direction = "long",
                    v.names = "subroot",
                    timevar = "subroot.lvl")

#tidy
map_1415 <- map_1415[!is.na(map_1415$subroot),]
map_1415 <- map_1415[order(map_1415$hrgcode.1415, map_1415$subroot.lvl),]

###--- CREATE 11/12 SUBSET ---###

#take out 17/18 hrgs
map_1112 <- map[,grep("root|1112", names(map))]

#tidy
map_1112 <- map_1112[is.na(map_1112$hrgcode.1112) != TRUE,]
map_1112$root <- gsub(" ", "", map_1112$root)

#split up roots (b/c some hrgs have more than 1 root)
root.split <- strsplit(map_1112$root, "/")
maxlen <- max(sapply(root.split, length))
root.split <- lapply(root.split, function(x) {
  x <- c(x, rep(NA,maxlen - length(x)))
})
root.split <- do.call(rbind, root.split)
colnames(root.split) <- paste("split", 1:maxlen, sep = ".")
map_1112 <- data.frame(map_1112, root.split, stringsAsFactors = FALSE)

rm(root.split)

#reshape wide dataset into long (i = hrg and j = no. of roots)
map_1112 <- reshape(map_1112,
                    varying = names(map_1112)[grep("split", names(map_1112))],
                    direction = "long",
                    v.names = "subroot",
                    timevar = "subroot.lvl")

#tidy
map_1112 <- map_1112[!is.na(map_1112$subroot),]
map_1112 <- map_1112[order(map_1112$hrgcode.1112, map_1112$subroot.lvl),]

###--- TIDY THE TWO MAPS ---###

map_1112$id <- NULL
map_1415$id <- NULL

###--- CREATE LIST OF 14/15 HRGS THAT MAP TO 11/12 HRGS ---###

#merge 14/15 hrgs with 11/12 hrgs
df.1 <- merge(map_1415, map_1112, 
                   by = "subroot",
                   suffixes = c(".1415", ".1112"),
                   all = TRUE)

#remove if hrgcode for 1415 is missing
df.1 <- df.1[!is.na(df.1$hrgcode.1415),]

#remove duplicates
df.1 <- df.1[order(df.1$hrg.1415, df.1$hrg.1112),]
df.1 <- df.1[!(df.1$subroot.lvl.1112 > 1 & df.1$subroot.lvl.1415 > 1), ]

#remove 11/12 hrgs that have roots mapping into a subset (as opposed to a superset) for 14/15
df.1$rtsize.1415 <- str_count(df.1$root.1415, "/")
df.1$rtsize.1112 <- str_count(df.1$root.1112, "/")
df.1 <- df.1[!(df.1$rtsize.1112 > df.1$rtsize.1415), ]
df.1[, c("subroot",
              "rtsize.1415", 
              "rtsize.1112",
              "subroot.lvl.1112",
              "subroot.lvl.1415")] <- list(NULL)

#tidy - rename var and create year variable
df.1$year <- 2017
names(df.1) <- gsub(".1415", "", names(df.1))

###--- CREATE LIST OF 11/12 HRGS THAT MAP TO 11/12 HRGS ---###

#create copy of 11/12 data
df.2 <- map_1112
names(df.2)[names(df.2) == "root"] <- c("root.1112")

#tidy
df.2[, grep("subroot", names(df.2))] <- list(NULL)
attr(df.2, "reshapeLong") <- NULL #remove attributes left from reshaping earlier

#create variable clones of 11/12 variables
for(i in names(df.2)) {
  varname <- gsub(".1112", "", i)
  df.2[[varname]] <- df.2[[i]]
}

#create year variable
df.2$year <- 2016

###--- MAIN DF WITH 14/15 AND 11/12 HRGS MAPPED BACK TO 11/12 HRGS---###

#append the two together
df <- rbind(df.1, df.2)
df <- df[order(df$hrgcode.1112, df$year),]

#create 11/12 hrg id
df$hrgid.1112 <- paste(df$hrgcode.1112, df$hrg.1112, sep = "-")

#create psuedoid - required for display
df$altID <- seq_len(NROW(df))

###-- MERGE ON PRICES ---###

#load data
price.1718 <- read.csv(file.path(main.path, clean.data, "HRG_Tariffs_1718.csv"),
                       stringsAsFactors = FALSE)
price.1617 <- read.csv(file.path(main.path, clean.data, "HRG_Tariffs_1617.csv"),
                       stringsAsFactors = FALSE)

#add year variable
price.1718$year <- 2017
price.1617$year <- 2016

#form price data frame
prices <- rbind(price.1617, price.1718)
rm(list =c("price.1718", "price.1617"))

#tidy price data
prices[,grep("trim|nelshortst", names(prices))] <- list(NULL)
prices$hrg <- NULL

#merge prices with main DF
full.df <- merge(df, prices, by =c("hrgcode", "year"), all = FALSE)
full.df <- full.df[order(full.df$hrgcode.1112, full.df$year),]

### IDENTIFY CCS AND AGE GRP FOR EACH HRG ---###

#identify age
full.df$age.pos <- regexpr("([0-9]+[0-9]*[^,]+year+.*)", full.df$hrg)
full.df$age <- ifelse(full.df$age != -1, regmatches(full.df$hrg, full.df$age.pos), NA)
full.df$age <- gsub("(,|\\swith)+.*", "", full.df$age)

#identify ccs
full.df$cc.pos <- regexpr("with\\w*\\s(\\w*\\s)?CC.*", full.df$hrg) 
    # the above says: search for with or without, then search for at most one more word, then search for CC
full.df$cc <- ifelse(full.df$cc.pos != -1, regmatches(full.df$hrg, full.df$cc.pos), NA)

#tidy
full.df[, grep("pos", names(full.df))] <- list(NULL)

###-- DISPLAY RESULTS ---###

#display, for a particular 11/12 hrg and setting, the prices for 16/17 and the prices for 17/18
#setting is dc tariff
#test hrg: HA71B-Major Elbow and Lower Arm Procedures for Trauma, with CC
#test hrg: EB10Z-Actual or Suspected Myocardial Infarction

#create tooltip
hrg_tooltip <- function(x) {
  
  if(is.null(x)) return(NULL)
  if(is.null(x$altID)) return(NULL)

  #pick out all hrgs linked to the chosen 11/12 hrg
  hrgs <- full.df[full.df$altID == x$altID,]
  
  #create label
  hrglabel <- paste(hrgs$hrgcode, hrgs$hrg, sep = "-")
  rootlabel <- paste0("Root: ", hrgs$root)
  paste0("<b>", hrglabel, "</b><br>", rootlabel, "<br>")
  
}

#set properties
props(yr = ~year, tariff = ~tariff.dcel)

#visualise!
full.df %>% 
  filter(hrgid.1112 == "HA71B-Major Elbow and Lower Arm Procedures for Trauma, with CC") %>%
  ggvis(x = ~year, y = ~tariff.dc) %>%
  layer_points(size := 50, size.hover := 200, fillOpacity := 0.5, key := ~altID) %>%
  add_tooltip(hrg_tooltip, "hover") %>%
  scale_numeric("x", domain =c(2015, 2018), round = TRUE)


  #set_option to change width and height of a plot in pxels, but only useful in shiny app

