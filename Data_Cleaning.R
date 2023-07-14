library(tidyverse)
library(readxl)
RI_scores_2223 <- read_excel("RI_scores_2223.xlsx")
View(RI_scores_2223)

#Names for scores in df are "1", "2", etc. which is confusing for arithmetic operations.
colnames(RI_scores_2223)[8:11] <- c("Level1", "Level2", "Level3", "Level4")

#Make a column with the number of meets and exceeds in each categorty (which means students scored 3 or 4), then delete the columns with individual levels
RI_scores_2223 <- RI_scores_2223 %>% mutate(M_E = Level3 + Level4) %>% select(-c("Level1", "Level2", "Level3", "Level4"))

#We also don't need "Group Type" since we have "Group Name"
RI_scores_2223 <- RI_scores_2223 %>% select(-c("GroupType"))

#Now we only group the data for charter districts available to students in Providence (had we not already done the PVD district, we would include 28 in the districts vector)
districts <- c(41, 48, 68, 53, 52, 69, 51, 43, 55, 59, 58, 83, 61, 54, 81, 7, 63, 42, 62, 64)
charter_scores_2223 <- RI_scores_2223 %>% filter(DistCode %in% districts)

#NAs mean there were no students that met or exceeded standards, so replace those with 0
charter_scores_2223[is.na(charter_scores_2223)] <- 0

#We also only want (for now) all students and ELL students - then we need to change to non-ELL and ELL
charter_scores_2223 <- charter_scores_2223 %>% filter(GroupName == "All" | GroupName == "ELL")

#The comparison we really want to do is non-ELL vs ELL, as opposed to All vs. ELL.
#We accomplish this in two steps. First we crate a function that , given a school code, creates a df with ELL, non-ELL scores and totals.
#Second, we bind those dfs together.

#The advantage of the separate dfs for each school is that we can control the row numbers to perform the needed operations

#Not all schools tested Science, so the function first has to determine if the school had science test scores or not. Then does its thang
new_row <- function(code){
  df = charter_scores_2223 %>% filter(SchCode == code)
  if ("Science" %in% df[3,]){      #some schools didn’t assess science
    non_T_ELA = df[1,7]-df[4,7]   #extracting the df makes this works
    non_T_Math = df[2,7] - df[5,7]
    non_T_Sci = df[3,7] - df[6,7]
    non_ELA = df[1,8] - df[4,8]
    non_Math = df[2,8] - df[5,8]
    non_Sci = df[3,8] - df[6,8]
    non_row_ELA <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "ELA", TotalCount = non_T_ELA, M_E = non_ELA)
    non_row_Math <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Math", TotalCount = non_T_Math, M_E = non_Math)
    non_row_Sci <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Science", TotalCount = non_T_Sci, M_E = non_Sci)
    df <- df %>% rbind(non_row_ELA, non_row_Math, non_row_Sci)
    df <- df %>% filter(GroupName != "All")
    }
    else {
    non_T_ELA = df[1,7]-df[3,7]
    non_T_Math = df[2,7] - df[4,7]
    non_ELA = df[1,8] - df[3,8]
    non_Math = df[2,8] - df[4,8]
    non_row_ELA <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "ELA", TotalCount = non_T_ELA, M_E = non_ELA)
    non_row_Math <- data.frame(DistCode = df$DistCode[1], DistName = df$DistName[1], SchCode = df$SchCode[1], SchName = df$SchName[1], GroupName = "Non-ELL", TestSubject = "Math", TotalCount = non_T_Math, M_E = non_Math)
    df <- df %>% rbind(non_row_ELA, non_row_Math)
    df <- df %>% filter(GroupName != "All")
    }
    return(df)
}

#test this
new_row(28609)

#Next, to start building our final data frame, we initialize an empty df with the desired column names
colnames <- colnames(charter_scores_2223)
charters_non_vs_ELL <- data.frame(matrix(nrow = 0, ncol = length(colnames)))
colnames(charters_non_vs_ELL) = colnames

#Because the school codes are strings, but we want to iterate through them, we create a numerica vector of school codes
v <- charter_scores_2223$SchCode
v <- as.numeric(v)
v <- unique(v)  #There is one entry for each test subject and group

#Make a new df using a loop - we iterate over the school codes, for each school code we apply the new_rows function to generate a temporary df with non-ELL and ELL, then add that df to the non-vs-ell df initialized earlier.
i <- 1
while (i <= 26){
  df <- new_row(v[i])
  charters_non_vs_ELL <- rbind(charters_non_vs_ELL, df)
  i <- i+1
}
