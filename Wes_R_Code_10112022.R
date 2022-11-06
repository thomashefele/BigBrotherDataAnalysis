# Load data management packages 
# Load packages
library(readxl)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)
library(kableExtra)
library(psych)
library(labelled)
library(tidyverse)
library(stringr)
```
# Set R working directory and import match history detail and SOR data from computer
# Jen: Update the filepaths and filenames to whatever they are on your computer
setwd("/Users/jenkoide/Desktop/")
sor.df <- read_excel("SOR_2014_to_2018.xlsx", guess_max = min(900000, n_max = NULL))
mhd.df <- read_excel("MatchHistoryDetail_2014_to_2018.xlsx", guess_max = min(362000, n_max = NULL))

# Import data codes
codes.df <- read_excel("BBBS_data_codes.xlsx")

# SOR variables
sor.varnames <- subset(codes.df, select = c(variable),
                       subset = codes.df$source_file=='SOR_2014_to_2018.xlsx')
sor.varnames <- as.vector(sor.varnames$variable)
colnames(sor.df) <- sor.varnames

# MHD variables
mhd.varnames <- subset(codes.df, select = c(variable),
                       subset = codes.df$source_file=='MatchHistoryDetail_2014_to_2018.xlsx')
mhd.varnames <- as.vector(mhd.varnames$variable)
colnames(mhd.df) <- mhd.varnames

# Join match history detail and SOR into one dataset
data.filter<- inner_join(mhd.df, sor.df, by = "match_id")

# Convert SOR survey completion date from character to date
data.filter$sor_codate <- as.Date(data.filter$sor_codate, "%m/%d/%Y", tz = "Europe/London")

data.filter$test_date <- as.Date(data.filter$sor_codate, format = '%Y-%m-%d')

min(data.filter$test_date)

###create big and little dfs
data.filter.b <- filter(data.filter, !is.na(sorb1))
data.filter.l <- filter(data.filter, !is.na(sorl1))


# Create the variables for t1 & t2
# Jen: It's a lot of typing... sorry!
# Grab the SOR at time 1 for sorb1
data.filter.b <- data.filter.b %>%
  group_by(match_id) %>%
  filter(test_date==min(test_date))

temp<-data.filter.b%>%group_by(match_id)%>% summarize(n=n()) 
unique(temp$n)

data.filter.b <- data.filter.b %>% distinct()


data.filter.l <- data.filter.l %>%
  group_by(match_id) %>%
  filter(test_date==min(test_date))

temp<-data.filter.l%>%group_by(match_id)%>% summarize(n=n()) 
unique(temp$n)

data.filter.b <- data.filter.b %>% distinct()

###One person
data.filter.b <- data.filter.b %>%
  group_by(match_id) %>%
  arrange(test_date) %>%
  slice(1L)


data.filter.l <- data.filter.l %>%
  group_by(match_id) %>%
  arrange(test_date) %>%
  slice(1L)


data.filter.joined<- inner_join(data.filter.b, data.filter.l, by = "match_id")

#Follow this pattern for the rest of the SOR columns
View(data.filter.2)

###############################################################################

# Jen's df
data.filter.joined <- data.filter.joined %>% 
  select(c(match_id, length.x, bage.x, bgender.x, brace.x, beduc.x, sor_codate.x, sorb1.x, sorb2.x, sorb3.x, sorb4.x, sorb5.x, sorb6.x, sorb7.x,
           sorb8.x, sorb9.x, sorb10.x, sorb11.x, sorb12.x, sorb13.x, sorb14.x, sorb15.x, lage.y, lgender.y, lrace.y, lage.y))
View(data.filter.joined)

colnames(data.filter.joined)



# Export a data frame using write_csv() 
write.csv(data.filter.joined, "datafilter_10192022", row.names = FALSE, na = "-999", quote = TRUE)