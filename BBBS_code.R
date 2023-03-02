#necessary libraries
library(readxl)
library(tidyverse)

#import files from computer and read out to code, replace each file variable with respective file pathway on computer
file_SOR <- readline("Enter SOR 2014-2018 file pathway: ")
file_MH <- readline("Enter Match History Detail 2014-2018 file pathway: ")
data_SOR <- read_excel(file_SOR, sheet= 1)
data_MH <- read_excel(file_MH, sheet= 1)
data_sorb <- data.frame(data_SOR[,1:17])
data_sorl <- data.frame(data_SOR[,1:2],data_SOR[,18:27])

#filter data by completed surveys into little and big data frames. Rows with incomplete surveys for each respective survey removed
sorb_by_compl <- data_sorb[complete.cases(data_sorb),]
sorl_by_compl <- data_sorl[complete.cases(data_sorl),]

#remove rows where the match id only occurs once (i.e. survey was only completed one time)
n_sorb <- count(sorb_by_compl, `Match.ID`)
freqb <- filter(n_sorb, n > 1)
sorb_by_freq <- merge(sorb_by_compl, freqb, by= "Match.ID")
sorb_by_freq <- sorb_by_freq[,-c(18)]

n_sorl <- count(sorl_by_compl, `Match.ID`)
freql <- filter(n_sorl, n > 1)
sorl_by_freq <- merge(sorl_by_compl, freql, by= "Match.ID")

#fetch first survey and survey at desired time later, if such a time exists
dates <- function(data, t_goal, p_m, b_or_l) {
  #set upper and lower bounds and retrieve row dimensional
  t_l <- t_goal - p_m
  t_h <- t_goal + p_m
  R <- nrow(data)
  #establish dataframe to be returned
  new_data <- data.frame()
  #initialize
  track <- c()
  del <- c()
  start_date <- ""
  start_match <- ""
  start_sorb <- data.frame()
  start_sorl <- data.frame()
  pot_dates <- c()
  min_del <- c()
  init <- 1
  
  if (b_or_l == "b") {
    for (i in 1:R) {
      if (i == 1) {
        start_match <- data[i,1]
        start_date <- data[i,2]
        start_sorb <- data[i,3:17]
   
      } else if (data[i-1,1] == data[i,1]) {
        del <- difftime(data[i,2], start_date, units="weeks")
        
        if (del >= t_l && del <= t_h) {
          pot_dates <- append(pot_dates, data[i,2])
          min_del <- append(min_del, abs(del-t_goal))
        }
        
        if (i == R) {
          if (is.null(pot_dates)) {
          } else {
            ord <- order(min_del)
            end_date <- pot_dates[ord[1]]
            new_line <- data.frame(`Match ID`= start_match, `Start Date`= start_date, `SORB 1`= start_sorb, `End Date`= end_date,
                                   `SORB 2`= data[init+ord[1],3:17])
            new_data <- rbind(new_data, new_line)
          }
          break
        }
      } else if (data[i-1,1] != data[i,1]) {
        if (i == R) {
          break
        } else if (is.null(pot_dates)) {
        } else {
          ord <- order(min_del)
          end_date <- pot_dates[ord[1]]
          
          new_line <- data.frame(`Match ID`= start_match, `Start Date`= start_date, `T1`= start_sorb, `End Date`= end_date,
                                 `T2`= data[init+ord,3:17])
          new_data <- rbind(new_data, new_line)
        }
        #re-initialize
        start_match <- data[i,1]
        start_date <- data[i,2]
        start_sorb <- data[i,3:17]
        pot_dates <- c()
        min_del <- c()
        init <- i
      }
      
      if (i == R) {
        if (is.null(pot_dates)) {
        } else {
          ord <- order(min_del)
          end_date <- pot_dates[ord[1]]
          new_line <- data.frame(`Match ID`= start_match, `Start Date`= start_date, `T1`= start_sorb, `End Date`= end_date,
                                 `T2`= data[init+ord[1],3:17])
          new_data <- rbind(new_data, new_line)
        }
      }
      cat(paste("Completion %:", round(100*(i/R)), "% \r"))
    }
  } else if (b_or_l == "l") {
    for (i in 1:R) {
      if (i == 1) {
        start_match <- data[i,1]
        start_date <- data[i,2]
        start_sorl <- data[i,3:12]
        
      } else if (data[i-1,1] == data[i,1]) {
        del <- difftime(data[i,2], start_date, units="weeks")
        
        if (del >= t_l && del <= t_h) {
          pot_dates <- append(pot_dates, data[i,2])
          min_del <- append(min_del, abs(del-t_goal))
        }
        
        if (i == R) {
          if (is.null(pot_dates)) {
          } else {
            ord <- order(min_del)
            end_date <- pot_dates[ord[1]]
            new_line <- data.frame(`Match ID`= start_match, `Start Date`= start_date, `T1`= start_sorl, `End Date`= end_date,
                                   `T2`= data[init+ord[1],3:12])
            new_data <- rbind(new_data, new_line)
          }
          break
        }
      } else if (data[i-1,1] != data[i,1]) {
        if (i == R) {
          break
        } else if (is.null(pot_dates)) {
        } else {
          ord <- order(min_del)
          end_date <- pot_dates[ord[1]]
          
          new_line <- data.frame(`Match ID`= start_match, `Start Date`= start_date, `T1`= start_sorl, `End Date`= end_date,
                                 `T2`= data[init+ord[1],3:12])
          new_data <- rbind(new_data, new_line)
        }
        #re-initialize
        start_match <- data[i,1]
        start_date <- data[i,2]
        start_sorl <- data[i,3:12]
        pot_dates <- c()
        min_del <- c()
        init <- i
      }
      cat(paste("Completion %:", round(100*(i/R)), "% \r"))
    }
  }
  return(new_data)
}

#filter by completion dates
options(warn= -1)
target <- as.numeric(readline("Enter target time interval in number of weeks: "))
var <- as.numeric(readline("Enter variation/error from target date in number of weeks: "))
sorb_by_date <- dates(sorb_by_freq, target, var, "b")
sorl_by_date <- dates(sorl_by_freq, target, var, "l")

#convert text response to SOR.B.15 to numeric response
for (i in 1:nrow(sorb_by_date)) {
  txt_1 <- sorb_by_date[i,9]
  txt_2 <- sorb_by_date[i,25]
    
  sorb_by_date[i,9] <- switch(txt_1, "1. I usually decide how we?ll spend our time together."= 1, 
                      "2. My Little usually decides how we?ll spend our time together."= 2, 
                      "3. I get ideas from my Little then we decide together."= 3,
                      "4. The agency case manager outlines how we will spend our time together."= 4,
                      "5. Someone else (like a teacher or parent) decides how we?ll spend our time together." = 5)

  sorb_by_date[i,25] <- switch(txt_2, "1. I usually decide how we?ll spend our time together."= 1, 
                      "2. My Little usually decides how we?ll spend our time together."= 2, 
                      "3. I get ideas from my Little then we decide together."= 3,
                      "4. The agency case manager outlines how we will spend our time together."= 4,
                      "5. Someone else (like a teacher or parent) decides how we?ll spend our time together." = 5)
}

#merge with Match History data frame
merge_sorb <- merge(sorb_by_date, data_MH, by= "Match.ID")
merge_sorl <- merge(sorl_by_date, data_MH, by= "Match.ID")

#change race parameters as desired
race_change <- function(r_list, idx, pos) {
  r <- "Other"
  boo <- 0
  race <- c("White", "Black", "Hispanic")
  multi_r <- "Multi-Race"
  
  if (length(grep(multi_r, r_list[idx,pos], ignore.case= TRUE)) != 0) {
    r <- "Multiracial"
  } else if (length(grep("White", r_list[idx,pos], ignore.case= TRUE)) != 0) {
    r <- "White"
  } else if (length(grep("Black", r_list[idx,pos], ignore.case= TRUE)) != 0) {
    r <- "Black"
  } else if (length(grep("Hispanic", r_list[idx,pos], ignore.case= TRUE)) != 0) {
    r <- "Hispanic"
  }
  
  return(r)
}

#test if little and big race/ethnicity matches; return a column vector of Boolean values (i.e. 1 = yes, 0 = no)
race_match <- function(data, b_or_l, idx1, idx2) {
  R <- nrow(data)
  yn <- c()
  
  if (b_or_l == "b") {
    for (i in 1:R) {
      if (identical(data[i,idx1], data[i,idx2])) {
        yn <- append(yn, 1)
      } else {
        yn <- append(yn, 0)
      }
    }
  } else if (b_or_l == "l") {
    for (i in 1:R) {
      if (identical(data[i,idx1], data[i,idx2])) {
        yn <- append(yn, 1)
      } else {
        yn <- append(yn, 0)
      }
    }
  }
  return (yn)
}

Rb <- nrow(merge_sorb)
Rl <- nrow(merge_sorl)
tb_l <- c()
tl_l <- c()

for (i in 1:Rb){
  tb_l <- append(tb_l, difftime(merge_sorb[i,18], merge_sorb[i,2], units= "weeks"))
  
  for (j in list(36, 40))
    merge_sorb[i,j] <- race_change(merge_sorb, i, j)
}

for (i in 1:Rl){
  tl_l = append(tl_l, difftime(merge_sorl[i,13], merge_sorl[i,2], units= "weeks"))
  
  for (j in list(26, 30))
    merge_sorl[i,j] <- race_change(merge_sorl, i, j)
}

final_sorb <- cbind(merge_sorb, `Racial/Ethnic Match:`= round(race_match(merge_sorb, "b", 36, 40)), `Length of Match:`= tb_l)
final_sorl <- cbind(merge_sorl, `Racial/Ethnic Match:`= round(race_match(merge_sorl, "l", 26, 30)), `Length of Match:`= tl_l)

#export to csv files
write_csv(final_sorb, "SORB_formatted.csv")
write_csv(final_sorl, "SORL_formatted.csv")
