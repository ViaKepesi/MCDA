# load packages to read and manipulate data
options(java.parameters = "-Xmx4g")
if (!require('XLConnect')) install.packages('XLConnect'); library('XLConnect')
if (!require('readr')) install.packages('readr'); library('readr')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('reshape')) install.packages('reshape'); library('reshape')
if (!require('rlang')) install.packages('rlang'); library('rlang')

# set working directory
working.main.directory <-  "C:\\Users\\malejin\\Documents\\Rworkinglibrary\\_beadando_TSZDM"
setwd(working.main.directory)
getwd()

# read data and check structure
cardata <- readWorksheetFromFile(file = "cardata.xlsx", sheet = "Final")
ls.str(cardata )

# manipulate data (change row names, transpose)
rownames(cardata) <- cardata[, 1]
cardata <- cardata[, -1]
cardata <- as.data.frame(t(cardata))
head(cardata)
ls.str(cardata )

# parse data to numbers
cardata$`PRICE WITH GOV SUPPORT` <- parse_number(as.character(cardata$`PRICE WITH GOV SUPPORT`), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$`HP` <- parse_number(as.character(cardata$`HP`), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'ENGINE'<- parse_number(as.character(cardata$'ENGINE'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'CONSUMPTION L PER KM' <- parse_number(as.character(cardata$'CONSUMPTION L PER KM'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'TRUNK CAPACITY IN L' <- parse_number(as.character(cardata$'TRUNK CAPACITY IN L'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'LENGTH' <- parse_number(as.character(cardata$'LENGTH'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'SPEED GEAR TYPE' <- parse_number(as.character(cardata$'SPEED GEAR TYPE'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)
cardata$'NUMBER OF SEATS' <- parse_number(as.character(cardata$'NUMBER OF SEATS'), locale = locale("hu", decimal_mark = ",", grouping_mark = " "), trim_ws = TRUE)

# check data structure and data
ls.str(cardata)
head(cardata)
#somehow column ENGINE became duplicated and a 10th column was created as factorm this needs to be removed
ncol(cardata)
cardata[,10]<-NULL
ncol(cardata)
head(cardata)

#install stargazer
if (!require('stargazer')) install.packages('stargazer'); library('stargazer')
#print cardata
stargazer(cardata, type = 'text', summary = FALSE)

# rank data for each columns
cardata$CAR.TYPES <- rank(cardata$`CAR TYPES`, ties.method = "average")
cardata$PRICE.WITH.GOV.SUPPORT <- rank(cardata$`PRICE WITH GOV SUPPORT`, ties.method = "average")
cardata$HORSE.POWER <- rank(cardata$`HP`, ties.method = "average")
cardata$ENGINE.CAPACITY <- rank(-cardata$'ENGINE', ties.method = "average")
cardata$CONSUMPTION.L.PER.KM<- rank(cardata$'CONSUMPTION L PER KM', ties.method = "average")
cardata$TRUNK.CAPACITY.IN.L<- rank(cardata$'TRUNK CAPACITY IN L', ties.method = "average")
cardata$CAR.LENGTH<- rank(cardata$'LENGTH', ties.method = "average")
cardata$SPEED.GEAR.TYPE<- rank(cardata$'SPEED GEAR TYPE', ties.method = "average")
cardata$NUMBER.OF.SEATS<- rank(cardata$'NUMBER OF SEATS', ties.method = "average")
cardata$IS.DIESEL<- rank(cardata$'DIESEL', ties.method = "average")

stargazer(cardata, type = 'text', summary = FALSE)
cardata


# keep only ranks and original data separate
ncol(cardata)
head(cardata[, c(1:9)])
head(cardata[, c(10:18)])

cardata.Rank <- cardata[, c(10:18)]
cardata <- cardata[, c(1:9)]

stargazer(cardata, type = 'text', summary = FALSE)
stargazer(cardata.Rank, type = 'text', summary = FALSE)

# convert from wide to long format
cardata.Rank$CAR.TYPES <- rownames(cardata)
cardata.Rank.molten <- melt(cardata.Rank, id.vars = "CAR.TYPES")
ls.str(cardata.Rank.molten)
cardata.Rank.molten$CAR.TYPES
length(cardata.Rank.molten$CAR.TYPES)
cardata.Rank.molten$variable
length(cardata.Rank.molten$variable)
cardata.Rank.molten$value
length(cardata.Rank.molten$value)

typeof(cardata.Rank.molten)
head(cardata.Rank.molten)


# insert line breaks for better plotting
cardata.Rank.molten$CAR.TYPES <- gsub(".", "\n", cardata.Rank.molten$CAR.TYPES, fixed = TRUE)
cardata.Rank.molten$variable <- gsub("Rank.", "", cardata.Rank.molten$variable, fixed = TRUE)
cardata.Rank.molten$variable <- gsub(".", "\n", cardata.Rank.molten$variable, fixed = TRUE)
cardata.Rank.molten$variable

#in order to keep the original order of variables we need to transform it to factor. I user am alteration that creates a news column which I remove after ploting in order to avoid any bug in later code.
unique(cardata.Rank.molten$variable)
cardata.Rank.molten$variable_f=factor(cardata.Rank.molten$variable, levels=unique(cardata.Rank.molten$variable))
head(cardata.Rank.molten)

# plot ranks 
ggplot(cardata.Rank.molten, aes(x =variable_f, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point(position = position_dodge(width = 0.05)) +
  geom_line(position = position_dodge(width = 0.05)) +
  geom_text(data = cardata.Rank.molten[cardata.Rank.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2,  check_overlap = TRUE, size=2) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  labs(x = NULL, y = "rank") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

cardata.Rank.molten$variable_f<-NULL 
head(cardata.Rank.molten)
# calculate an overall rank by summing all ranks and rank the sums

cardata.Rank$Overall.Rank <-  rank(rowSums(subset(cardata.Rank, select = -c(CAR.TYPES)), dims = 1), ties.method = "average")

# convert from wide to long format
cardata.Rank.molten <- melt(cardata.Rank, id.vars = "CAR.TYPES")
ls.str(cardata.Rank.molten)
cardata.Rank.molten

# insert line breaks for better plotting
cardata.Rank.molten$CAR.TYPES <- gsub(".", "\n", cardata.Rank.molten$CAR.TYPES, fixed = TRUE)
cardata.Rank.molten$variable <- gsub("Rank.", "", cardata.Rank.molten$variable, fixed = TRUE)
cardata.Rank.molten$variable <- gsub(".", "\n", cardata.Rank.molten$variable, fixed = TRUE)
cardata.Rank.molten$variable <-  factor(cardata.Rank.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", "ENGINE\nCAPACITY", 
                                                                    "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE", "NUMBER\nOF\nSEATS","IS\nDIESEL","Overall\nRank"))
cardata.Rank.molten
# plot ranks
ggplot(cardata.Rank.molten, aes(x = variable, y = value, colour =CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line(position = position_dodge(width = 0.05)) +
  geom_text(data = cardata.Rank.molten[cardata.Rank.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2,  check_overlap = TRUE, size=2) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", "ENGINE\nCAPACITY", 
                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE", "NUMBER\nOF\nSEATS","IS\nDIESEL","Overall\nRank"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  labs(x = NULL, y = "<-- rank - lower is better") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  geom_point(data = cardata.Rank.molten[cardata.Rank.molten$variable == "Overall\nRank", ], size = 5)



# comparing alternatives by normalisation
ls.str(cardata)

# store row and column names as they will be dropped at matrix calculations
cardata.rownames <- rownames(cardata)
cardata.colnames <- colnames(cardata)

# convert factors to number (integer)
cardata$DIESEL
cardata$DIESEL <- as.integer(cardata$DIESEL)
ls.str(cardata)

cardata$ENGINE <- as.integer(cardata$DIESEL)
ls.str(cardata)

#somehow engine still remained there as factor, it needs to be removed. See line 36, I have removed it once..
cardata[,3]<-NULL

# normalise the data frames using the four different normalisation methods

norm1 <- function(x) {  
    x / max(x)}

cardata.norm1 <- as.data.frame(sapply(cardata, norm1))
cardata.norm1

norm2 <- function(x) {  
  (x - min(x)) / (max(x) - min(x))}

cardata.norm2 <- as.data.frame(sapply(cardata, norm2))
cardata.norm2

norm3 <- function(x) {  
  x / sum(x)}

cardata.norm3 <- as.data.frame(sapply(cardata, norm3))
cardata.norm3

norm4 <- function(x) {  
  x / sqrt(sum(x^2))}

cardata.norm4 <- as.data.frame(sapply(cardata, norm4))
cardata.norm4

# take direction in consideration: smaller numbers are better for some criteria (marked as -1)
cardata.Direction <- c(-1, 1, 1, -1, 1, 1, 1, -1, 1) # set the direction as a vector
#price,consumption and diesel are marked as minus, in these cases I think lower is better
# define function to change direction
change.direction <- function(x, y) {
  ifelse(y == -1, 1 + x * y, x)
}

### norm1 

# apply "change.direction" function and restore column and rownames
cardata.norm1.dir <- as.data.frame(t(apply(cardata.norm1, MARGIN = 1, FUN = change.direction, y =  cardata.Direction)))
rownames(cardata.norm1.dir) <- cardata.rownames
colnames(cardata.norm1.dir) <- c("PRICE.WITH.GOV.SUPPORT", "HORSE.POWER",  
                                   "CONSUMPTION.L.PER.KM", "TRUNK.CAPACITY.IN.L","CAR.LENGTH","SPEED.GEAR.TYPE","IS.DIESEL", "NUMBER.OF.SEATS","ENGINE.CAPACITY")

cardata.norm1.dir
# calculate overall value by summing normalised values
cardata.norm1.dir$Overall <- (cardata.norm1.dir$PRICE.WITH.GOV.SUPPORT + 
                                      cardata.norm1.dir$HORSE.POWER + 
                                      cardata.norm1.dir$CONSUMPTION.L.PER.KM + 
                                      cardata.norm1.dir$TRUNK.CAPACITY.IN.L + 
                                      cardata.norm1.dir$CAR.LENGTH+
                                      cardata.norm1.dir$SPEED.GEAR.TYPE+
                                      cardata.norm1.dir$IS.DIESEL+
                                      cardata.norm1.dir$NUMBER.OF.SEATS+
                                      cardata.norm1.dir$ENGINE.CAPACITY)/9
cardata.norm1.dir$Overall 
# convert from wide to long format
cardata.norm1.dir$CAR.TYPES <- rownames(cardata.norm1.dir)
cardata.norm1.dir$CAR.TYPES
head(cardata.norm1.dir)
cardata.norm1.dir.molten <- melt(cardata.norm1.dir, id.vars = "CAR.TYPES")

cardata.norm1.dir.molten
# insert line breaks for better plotting
cardata.norm1.dir.molten$CAR.TYPES <- gsub(".", "\n", cardata.norm1.dir.molten$CAR.TYPES, fixed = TRUE)
cardata.norm1.dir.molten$variable <- gsub(".", "\n", cardata.norm1.dir.molten$variable, fixed = TRUE)
cardata.norm1.dir.molten$variable <-  factor(cardata.norm1.dir.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))

# plot ranks
ggplot(cardata.norm1.dir.molten, aes(x = variable, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line() +
  geom_text(data = cardata.norm1.dir.molten[cardata.norm1.dir.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2,check_overlap = TRUE, size=2) +
  geom_text(data = cardata.norm1.dir.molten[cardata.norm1.dir.molten$variable == "Overall", ], hjust = -0.2, vjust = 0.2, check_overlap = TRUE, size=2) +
  labs(x = NULL, y = "value higher is better ->", title = "normalised as y = x / max(x)") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

### norm2 

# apply "change.direction" function and restore column and rownames
cardata.norm2.dir <- as.data.frame(t(apply(cardata.norm2, MARGIN = 1, FUN = change.direction, y =  cardata.Direction)))
rownames(cardata.norm2.dir) <- cardata.rownames
colnames(cardata.norm2.dir) <- c("PRICE.WITH.GOV.SUPPORT", "HORSE.POWER",  
                                 "CONSUMPTION.L.PER.KM", "TRUNK.CAPACITY.IN.L","CAR.LENGTH","SPEED.GEAR.TYPE","IS.DIESEL", "NUMBER.OF.SEATS","ENGINE.CAPACITY")


cardata.norm2.dir

# calculate overall value by summing normalised values
cardata.norm2.dir$Overall <- (cardata.norm2.dir$PRICE.WITH.GOV.SUPPORT + 
                                cardata.norm2.dir$HORSE.POWER + 
                                cardata.norm2.dir$CONSUMPTION.L.PER.KM + 
                                cardata.norm2.dir$TRUNK.CAPACITY.IN.L + 
                                cardata.norm2.dir$CAR.LENGTH+
                                cardata.norm2.dir$SPEED.GEAR.TYPE+
                                cardata.norm2.dir$IS.DIESEL+
                                cardata.norm2.dir$NUMBER.OF.SEATS+
                                cardata.norm2.dir$ENGINE.CAPACITY)/9
cardata.norm2.dir$Overall 

# convert from wide to long format
cardata.norm2.dir$CAR.TYPES <- rownames(cardata.norm2.dir)
cardata.norm2.dir.molten <- melt(cardata.norm2.dir, id.vars = "CAR.TYPES")


# insert line breaks for better plotting
cardata.norm2.dir.molten$CAR.TYPES <- gsub(".", "\n", cardata.norm2.dir.molten$CAR.TYPES, fixed = TRUE)
cardata.norm2.dir.molten$variable <- gsub(".", "\n", cardata.norm2.dir.molten$variable, fixed = TRUE)
cardata.norm2.dir.molten$variable <-  factor(cardata.norm2.dir.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                                                           "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))

# plot ranks
ggplot(cardata.norm2.dir.molten, aes(x = variable, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line() +
  geom_text(data = cardata.norm2.dir.molten[cardata.norm2.dir.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2, check_overlap = TRUE, size=2) +
  geom_text(data = cardata.norm2.dir.molten[cardata.norm2.dir.molten$variable == "Overall", ], hjust = -0.2, vjust = 0.2, check_overlap = TRUE, size=2) +
  labs(x = NULL, y = "value higher is better ->", title = "normalised as y = (x - min(x)) / (max(x) - min(x))") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                           "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  


### norm3 

# apply "change.direction" function and restore column and rownames
cardata.norm3.dir <- as.data.frame(t(apply(cardata.norm3, MARGIN = 1, FUN = change.direction, y =  cardata.Direction)))
rownames(cardata.norm3.dir) <- cardata.rownames
colnames(cardata.norm3.dir) <- c("PRICE.WITH.GOV.SUPPORT", "HORSE.POWER",  
                                 "CONSUMPTION.L.PER.KM", "TRUNK.CAPACITY.IN.L","CAR.LENGTH","SPEED.GEAR.TYPE","IS.DIESEL", "NUMBER.OF.SEATS","ENGINE.CAPACITY")


cardata.norm3.dir

# calculate overall value by summing normalised values
cardata.norm3.dir$Overall <- (cardata.norm3.dir$PRICE.WITH.GOV.SUPPORT + 
                                cardata.norm3.dir$HORSE.POWER + 
                                cardata.norm3.dir$CONSUMPTION.L.PER.KM + 
                                cardata.norm3.dir$TRUNK.CAPACITY.IN.L + 
                                cardata.norm3.dir$CAR.LENGTH+
                                cardata.norm3.dir$SPEED.GEAR.TYPE+
                                cardata.norm3.dir$IS.DIESEL+
                                cardata.norm3.dir$NUMBER.OF.SEATS+
                                cardata.norm3.dir$ENGINE.CAPACITY)/9
cardata.norm3.dir$Overall 

# convert from wide to long format
cardata.norm3.dir$CAR.TYPES <- rownames(cardata.norm3.dir)
cardata.norm3.dir.molten <- melt(cardata.norm3.dir, id.vars = "CAR.TYPES")


# insert line breaks for better plotting
cardata.norm3.dir.molten$CAR.TYPES <- gsub(".", "\n", cardata.norm3.dir.molten$CAR.TYPES, fixed = TRUE)
cardata.norm3.dir.molten$variable <- gsub(".", "\n", cardata.norm3.dir.molten$variable, fixed = TRUE)
cardata.norm3.dir.molten$variable <-  factor(cardata.norm3.dir.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                                                           "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))

# plot ranks
ggplot(cardata.norm3.dir.molten, aes(x = variable, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line() +
  geom_text(data = cardata.norm3.dir.molten[cardata.norm3.dir.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2, check_overlap = TRUE, size=2) +
  geom_text(data = cardata.norm3.dir.molten[cardata.norm3.dir.molten$variable == "Overall", ], hjust = -0.2, vjust = 0.2, check_overlap = TRUE, size=2) +
  labs(x = NULL, y = "value higher is better ->", title = "normalised as y =  x / sum(x)") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

  

### norm4 

# apply "change.direction" function and restore column and rownames
cardata.norm4.dir <- as.data.frame(t(apply(cardata.norm4, MARGIN = 1, FUN = change.direction, y =  cardata.Direction)))
rownames(cardata.norm4.dir) <- cardata.rownames
colnames(cardata.norm4.dir) <- c("PRICE.WITH.GOV.SUPPORT", "HORSE.POWER",  
                                 "CONSUMPTION.L.PER.KM", "TRUNK.CAPACITY.IN.L","CAR.LENGTH","SPEED.GEAR.TYPE","IS.DIESEL", "NUMBER.OF.SEATS","ENGINE.CAPACITY")


cardata.norm4.dir

# calculate overall value by summing normalised values
cardata.norm4.dir$Overall <- (cardata.norm4.dir$PRICE.WITH.GOV.SUPPORT + 
                                cardata.norm4.dir$HORSE.POWER + 
                                cardata.norm4.dir$CONSUMPTION.L.PER.KM + 
                                cardata.norm4.dir$TRUNK.CAPACITY.IN.L + 
                                cardata.norm4.dir$CAR.LENGTH+
                                cardata.norm4.dir$SPEED.GEAR.TYPE+
                                cardata.norm4.dir$IS.DIESEL+
                                cardata.norm4.dir$NUMBER.OF.SEATS+
                                cardata.norm4.dir$ENGINE.CAPACITY)/9
cardata.norm4.dir$Overall 

# convert from wide to long format
cardata.norm4.dir$CAR.TYPES <- rownames(cardata.norm4.dir)
cardata.norm4.dir.molten <- melt(cardata.norm4.dir, id.vars = "CAR.TYPES")


# insert line breaks for better plotting
cardata.norm4.dir.molten$CAR.TYPES <- gsub(".", "\n", cardata.norm4.dir.molten$CAR.TYPES, fixed = TRUE)
cardata.norm4.dir.molten$variable <- gsub(".", "\n", cardata.norm4.dir.molten$variable, fixed = TRUE)
cardata.norm4.dir.molten$variable <-  factor(cardata.norm4.dir.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                                                           "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))

# plot ranks
ggplot(cardata.norm4.dir.molten, aes(x = variable, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line() +
  geom_text(data = cardata.norm4.dir.molten[cardata.norm4.dir.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2,check_overlap = TRUE, size=2) +
  geom_text(data = cardata.norm4.dir.molten[cardata.norm4.dir.molten$variable == "Overall", ], hjust = -0.2, vjust = 0.2, check_overlap = TRUE, size=2) +
  labs(x = NULL, y = "value higher is better ->", title = "normalised as y =  x / sqrt(sum(x^2))") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  

###########################

# Establishing the weights of decision criteria
# the entropy method

if (!require('DescTools')) install.packages('DescTools'); library('DescTools')

# calculate weights

dispersion <- vector()

for(i in 1:ncol(cardata.norm1)){
  dispersion[i] <- 1 - DescTools::Entropy(as.matrix(cardata.norm1[,i]), base = 10)
}

dispersion <- data.frame(dispersion)
dispersion
dispersion$weights <- dispersion$dispersion / sum(dispersion$dispersion)
dispersion$weights
sum(dispersion$weights)

# apply weights
cardata.norm1.dir$Overall <- colSums(  t(cardata.norm1.dir[, 1:9]) * dispersion$weights  )  
cardata.norm2.dir$Overall <- colSums(  t(cardata.norm2.dir[, 1:9]) * dispersion$weights  )  
cardata.norm3.dir$Overall <- colSums(  t(cardata.norm3.dir[, 1:9]) * dispersion$weights  )  
cardata.norm4.dir$Overall <- colSums(  t(cardata.norm4.dir[, 1:9]) * dispersion$weights  )  


# plot weighted 
# convert from wide to long format
cardata.norm3.dir$CAR.TYPES <- rownames(cardata.norm3.dir)
cardata.norm3.dir.molten <- melt(cardata.norm3.dir, id.vars = "CAR.TYPES")


# insert line breaks for better plotting
cardata.norm3.dir.molten$CAR.TYPES <- gsub(".", "\n", cardata.norm3.dir.molten$CAR.TYPES, fixed = TRUE)
cardata.norm3.dir.molten$variable <- gsub(".", "\n", cardata.norm3.dir.molten$variable, fixed = TRUE)
cardata.norm3.dir.molten$variable <-  factor(cardata.norm3.dir.molten$variable, levels = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                                                           "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))

# plot ranks
ggplot(cardata.norm3.dir.molten, aes(x = variable, y = value, colour = CAR.TYPES, group = CAR.TYPES, label = CAR.TYPES)) +
  geom_point() +
  geom_line() +
  geom_text(data = cardata.norm3.dir.molten[cardata.norm3.dir.molten$variable == "PRICE\nWITH\nGOV\nSUPPORT", ], hjust = 1.2, vjust = -0.2,  check_overlap = TRUE, size=2) +
  geom_text(data = cardata.norm3.dir.molten[cardata.norm3.dir.molten$variable == "Overall", ], hjust = -0.2, vjust = 0.2,  check_overlap = TRUE, size=2) +
  labs(x = NULL, y = "value higher is better ->", title = "normalised as y =  x / sum(x)") +
  theme_bw() +
  theme(legend.position = "none", panel.grid.minor = element_blank()) +
  scale_x_discrete(expand = c(0.3, 0, 0.1, 0), breaks = c("PRICE\nWITH\nGOV\nSUPPORT", "HORSE\nPOWER", 
                                                          "CONSUMPTION\nL\nPER\nKM", "TRUNK\nCAPACITY\nIN\nL","CAR\nLENGTH","SPEED\nGEAR\nTYPE","IS\nDIESEL","NUMBER\nOF\nSEATS","ENGINE\nCAPACITY","Overall"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  



######################
# the eigenvalue methods
# ahp analytic hierarchy process
if (!require('ahp')) install.packages('ahp'); library('ahp')
if (!require('shinythemes')) install.packages('shinythemes'); library('shinythemes')
if (!require("shinyAce")) install.packages("shinyAce"); library("shinyAce")
if (!require("shinyjs")) install.packages("shinyjs"); library("shinyjs")

# RunGUI requires a number of shiny packages !
RunGUI()

cardata.ahp <- Load(ahpFile = "cardataahp_v3.ahp")
cardata.ahp

Calculate(cardata.ahp)
Analyze(cardata.ahp)
AnalyzeTable(cardata.ahp)
Visualize(cardata.ahp)

# topsis method
if (!require('topsis')) install.packages('topsis'); library('topsis')

cardata.norm3

#these weights were calculated by ahp
cardata.weights <- c(0.248, 0.063, 0.201, 0.146, 0.017, 0.041, 0.030, 0.176, 0.078 )
cardata.impacts <- c("-", "+", "-", "+", "-", "+","-","+","+")

cardata.topsis <- topsis(decision = as.matrix(cardata.norm3), weights = cardata.weights, impacts = cardata.impacts)

cardata.topsis
cardata.norm3.dir[, 11]

cardata.topsis <- cbind(cardata.topsis, cardata.norm3.dir[, 11])
cardata.topsis

colnames(cardata.topsis)[4] <- "Bidder"


#cardata.topsis$score_rounded<-round(cardata.topsis$score,digits=4)

ggplot(data=cardata.topsis, aes(x=Bidder, y=score, fill=Bidder)) +
  geom_bar(stat="identity", color="black")+theme_minimal()+
  geom_text(aes(label = rank), vjust = 5, size = 3, colour="White")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Scores of Bidders")+
  xlab("Bidders")+
  ylab("Score")


