#################################
#                               #
# DATA EXTRACTION: 18/09/2019   #
# START DATA POINT: 03/06/2019  #
# FINAL DATA POINT: 17/09/2019  #
#                               #
#################################


setwd("C:/Users/ROMEST/Downloads/Performance/")

options(scipen=999)

# read excel file
library(readxl)
# split column from excel import
library("tidyr")


################################
#                              #
#     PERFORMANCE CARRELLO     #
#                              #
################################


preferiti_performance_excel <- read_excel("preferiti_2_nodi.xlsx")
data <- as.data.frame(preferiti_performance_excel)

# separate column
data <- separate(data, time, into = c("empty", "time"), sep = " (?=[^ ]+$)")
data <- data[,-2]
names(data)[4] <- "avg_response"
names(data)[1] <- "date"
# add column for unique ID
data$id <- seq.int(nrow(data))
# remove comma from number
data$avg_response <- as.numeric(gsub(",","", data$avg_response))
# add column for month extraction
data$new <- data$date
# split new column for month extraction
data <- separate(data, new, into = c("year", "month", "day"), sep = "-")


# check for visible outliers
plot(data$date, data$avg_response, main = "Performance preferiti",
     xlab = "Data", ylab = "Tempo risposta medio (3 ore), in millisecondi")

# extract outliers rows for investivation
outliers <- data[which(data$avg_response > 200000),]
final_data <- data[-which(data$avg_response > 200000), ]

# create subset data for each month
june <- subset(final_data, month == "06")
july <- subset(final_data, month == "07")
august <- subset(final_data, month == "08")
september <- subset(final_data, month == "09")

# create variables for custom X axis (lid is where to show the mark, lab are the labels)
lid <- c(1, 218, 466, 714)
lab <- c("Giu", "Lug", "Ago", "Set")



#plot(final_data$date, final_data$avg_response, type = "l", main = "Performance Catalogo",
#    xlab = "Data", ylab = "Tempo risposta medio (3 ore), in millisecondi")

plot(final_data$id, final_data$avg_response, type = "l", main = "Performance preferiti",
     xlab = "Data", ylab = "Tempo risposta medio (3 ore), in millisecondi", xaxt = "n")
axis(side = 1, at = lid, labels = lab)

# check situation before IT fix (june/july) vs after IT fix (august/september)
# create data frame
before <- rbind(june, july)
after <- rbind(august, september)


# TIME OF THE DAY
# 10 SECONDS
# BEFORE
# extract rows where avg response > 10 sec
b10s <- before[which(before$avg_response > 10000), ]
# show distribution
table(b10s$time)
# AFTER
# extract rows where avg response > 10 sec
a10s <- after[which(after$avg_response > 10000), ]
# show distribution
table(a10s$time)

# 5 SECONDS
# BEFORE
# extract rows where avg response > 5 sec
b5s <- before[which(before$avg_response > 5000), ]
# show distribution
table(b5s$time)
# AFTER
# extract rows where avg response > 5 sec
a5s <- after[which(after$avg_response > 5000), ]
# show distribution
table(a5s$time)

# 2.5 SECONDS
# BEFORE
# extract rows where avg response > 2.5 sec
b2.5s <- before[which(before$avg_response > 2500), ]
# show distribution
table(b2.5s$time)
nrow(before[which(before$avg_response > 2500), ])
# AFTER
# extract rows where avg response > 2.5 sec
a2.5s <- after[which(after$avg_response > 2500), ]
# show distribution
table(a2.5s$time)
nrow(after[which(after$avg_response > 2500), ])

# 1.5 SECONDS
# BEFORE
# extract rows where avg response > 1.5 sec
b1.5s <- before[which(before$avg_response > 1500), ]
# show distribution
table(b1.5s$time)
nrow(before[which(before$avg_response > 1500), ])
# AFTER
# extract rows where avg response > 1.5 sec
a1.5s <- after[which(after$avg_response > 1500), ]
# show distribution
table(a1.5s$time)
nrow(after[which(after$avg_response > 1500), ])




sum(table(b10s$time))/nrow(before)
sum(table(a10s$time))/nrow(after)
