library(dplyr)
library(ggplot2)

setwd("/Users/sandraezidiegwu/Documents/Data Science/SFO Customer Survey 2015/")
surv.data <- read.xlsx("sfo cust_sat.xlsx", 1, colIndex = c(5,6,7,9,14,16,48,49,83,84,85), header = TRUE)
surv.data$DEPTIME <- format(surv.data$DEPTIME, "%H:%M:%S")

american <- subset(surv.data, AIRLINE == 8)
united <- subset(surv.data, AIRLINE == 33)
american.neg <- subset(american, Q8COM1 %in% 401:410)
united.neg <- subset(united, Q8COM1 %in% 401:410)

hist(american.neg$Q8COM1, col = "blue")
hist(united.neg$Q8COM1, col = "blue")

boxplot(Q8COM1 ~ DESTINATION, american.neg, col = american.neg$Q8COM1)
boxplot(Q8COM1 ~ DESTINATION, united.neg, col = united.neg$Q8COM1)

plot(Q8COM1 ~ DESTINATION, united.neg, col = united.neg$Q8COM1, pch = 17, xaxt = 'n', yaxt = 'n')
axis(1, united.neg$DESTINATION)
axis(2, united.neg$Q8COM1)
axis(4, united.neg$Q8COM1)
abline(h = united.neg$Q8COM1, v = united.neg$DESTINATION, col = "gray", lty = 3)

top.amer <- aggregate(american.neg$Q8COM1, by = list(american.neg$DESTINATION), FUN = length)
names(top.amer) <- c("Destination", "No. of Complaints")
top.amer$Destination[1:nrow(top.amer)] <- c("Chicago-O'Hare", "Dalas-Ft.Worth", "New York-JFK")
top.amer

top.amer1 <- aggregate(american.neg$DESTINATION, by = list(american.neg$Q8COM1), FUN = length)
top.amer1[,3] <- c("Need more airline customer service staff", "Baggage claim too slow/difficult to find/too far away from flights","Check-in staff not there/not enough staff at check-in during early morning hours")
names(top.amer1) <- c("Complaints", "Frequency", "Complaint Decription")
top.amer1

top.neg <- aggregate(united.neg$Q8COM1, by = list(united.neg$DESTINATION), FUN = length)
names(top.neg) <- c("Destination", "No. of Complaints")
top.neg$Destination[1:nrow(top.neg)] <- c("Arcata, CA", "Denver", "Eugene, OR", "Honolulu", "Newark", "Palm Springs", "Salt Lake City")
top.comp <- top.neg[order(-top.neg$'No. of Complaints'),][1:3,]
top.comp

top.neg1 <- aggregate(united.neg$DESTINATION, by = list(united.neg$Q8COM1), FUN = length)
top.neg1[,3] <- c("Tell airlines to be on time more often", "Tell airlines to communicate better about delays/changes", "Need more airline customer service staff", "Airline customer service staff were rude/not helpful", "Baggage claim too slow/difficult to find/too far away from flights", "Allow more time for connecting flights/cut transfer to next flight too close", "Need a live person as you get off plane to help with connecting flights/provide directions")
names(top.neg1) <- c("Complaints", "Frequency", "Complaint Description")
top.comp1 <- top.neg1[order(-top.neg1$'Frequency'),][1:3,]
top.comp1
