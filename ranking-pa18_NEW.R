library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

#testing out a ranking process to see if it could work
x <- rnorm(10, mean = 100, sd=3)
y <- rnorm(10, mean = 100, sd=3)

test <- rbind(x,y)

test

rank(x)
rank(y)

t(apply(test, 1, rank))
#rank appears to score the LOWEST value as #1
#####

#Now to the real data
# https://www.census.gov/mycd/?st=42&cd=18   #2016 ACS-1 by CD


pa_all <- read_csv("Pennsylvania_All_Districts.csv", 
                                       col_types = cols(`District 01 MOE` = col_skip(), 
                                                        `District 02 MOE` = col_skip(), 
                                                        `District 03 MOE` = col_skip(),
                                                        `District 04 MOE` = col_skip(), 
                                                        `District 05 MOE` = col_skip(), 
                                                        `District 06 MOE` = col_skip(),
                                                        `District 07 MOE` = col_skip(), 
                                                        `District 08 MOE` = col_skip(), 
                                                        `District 09 MOE` = col_skip(), 
                                                        `District 10 MOE` = col_skip(),
                                                        `District 11 MOE` = col_skip(), 
                                                        `District 12 MOE` = col_skip(), 
                                                        `District 13 MOE` = col_skip(),
                                                        `District 14 MOE` = col_skip(), 
                                                        `District 15 MOE` = col_skip(), 
                                                        `District 16 MOE` = col_skip(),
                                                        `District 17 MOE` = col_skip(), 
                                                        `District 18 MOE` = col_skip()
                                                     
                                                        )
                                       )
# View(pa_all)
str(pa_all)

colnames(pa_all) <- c("topic", "subject", "title", "dist01", "dist02", "dist03","dist04","dist05","dist06","dist07","dist08",
                      "dist09","dist10","dist11","dist12","dist13","dist14","dist15","dist16","dist17","dist18")

#some columns came as chr, convert to numeric
pa_all$dist01 <- as.numeric(pa_all$dist01)
pa_all$dist02 <- as.numeric(pa_all$dist02)
pa_all$dist14 <- as.numeric(pa_all$dist14)



numbersonly <- pa_all[4:21]
categories <- pa_all[1:3]
numbersonly

rankings <- round(t(apply(numbersonly, 1, rank)))
rankings

c <- cbind(numbersonly, rankings)
cc <- cbind(categories, c)  

str(cc)
# View(cc)

temp <- t(cc[199,])
write.csv(temp, "temp_medincome.csv")


#pulling out just the ranking for PA-18th
names(cc)
cc[ , c(1,2,3,21,39)]

#new object with just the results
pa18only <- cc[ , c(1,2,3,21,39)]


pa18only$topic <- as.factor(pa18only$topic)
pa18only$subject <- as.factor(pa18only$subject)
pa18only$title <- as.factor(pa18only$title)
colnames(pa18only) <- c("topic", "subject", "title", "value", "ranking")
head(pa18only, 10)
str(pa18only)

# View(pa18only)

write.csv(pa18only, "pa18_demosandrank.csv")


##trying out some charts
#note: str_detect below looks for "contains" a string pattern


#age breakdown
byage <- pa18only %>% 
  filter(subject == "Sex and Age", 
         str_detect(title, "years"),
         str_detect(title, "Median") == FALSE
         )

ggplot(byage, aes(x=title, y=value)) +
  geom_col() +
  coord_flip()
  

