library(tidyverse)
library(png)
library(patchwork)

#The df PVD_non_vs_ELL contains test scores for all students in a school and for ELL students

#Define ell to be the vector with schools with desired bilingual programs

ell <- c(28103, 28134, 28153, 28157, 28144)

#28103 is Leviton Dual Language School which has dual language programs
#28134 is Frank D. Spaziano El School which has dual language programs
#28153 is William D'Abate El School which has developmental bilingual programs
#28157 is Lillian Feinstein El School which has transitional bilingual programs
#28144 is Gilbert Stuart Middle School, has "dual language immersion"

#Create data with state scores
state <- c(0.041037795, 0.369080983, 0.052497981, 0.296199854, 0.030647986, 0.343284716)

#pull es icon for schools with bilingual programs
es <- readPNG("ES.png")

#This function wrangles the dataframe to make a new dataframe with the percentage of students meeting/exceeding expectations for non-ELL and ELL students in ELA, returning a new dataframe.
ELA_scores <- function(code){
  df <- PVD_non_vs_ELL %>% filter(SchCode == code, TestSubject == "ELA") 
  df <- df %>% select(-c("Level1", "Level2", "Level3", "Level4", "Percent_M_E"))
  df[is.na(df)] <- 0
  df[nrow(df) + 1,] <- list(df$SchCode[1], df$SchName[1], "Non-ELL", df$TestSubject[1], df$Number_1[1] - df$Number_1[2], df$Number_2[1] - df$Number_2[2], df$Number_3[1] - df$Number_3[2], df$Number_4[1] - df$Number_4[2], df$TotalCount[1] - df$TotalCount[2])
  df <- df %>% mutate(Percent_M_E = (Number_3 + Number_4)/TotalCount)
  df <- df[-1, ]
  df_ELA <- df
  return(df_ELA)
}

#Do the same with math

Math_scores <- function(code){
  df <- PVD_non_vs_ELL %>% filter(SchCode == code, TestSubject == "Math")
  df <- df %>% select(-c("Level1", "Level2", "Level3", "Level4", "Percent_M_E"))
  df[is.na(df)] <- 0
  df[nrow(df) + 1,] <- list(df$SchCode[1], df$SchName[1], "Non-ELL", df$TestSubject[1], df$Number_1[1] - df$Number_1[2], df$Number_2[1] - df$Number_2[2], df$Number_3[1] - df$Number_3[2], df$Number_4[1] - df$Number_4[2], df$TotalCount[1] - df$TotalCount[2])
  df <- df %>% mutate(Percent_M_E = (Number_3 + Number_4)/TotalCount)
  df <- df[-1, ]
  df_Math <- df
  return(df_Math)
}

#Do the same with science

Sci_scores <- function(code){
  df <- PVD_non_vs_ELL %>% filter(SchCode == code, TestSubject == "Science")
  df <- df %>% select(-c("Level1", "Level2", "Level3", "Level4", "Percent_M_E"))
  df[is.na(df)] <- 0
  df[nrow(df) + 1,] <- list(df$SchCode[1], df$SchName[1], "Non-ELL", df$TestSubject[1], df$Number_1[1] - df$Number_1[2], df$Number_2[1] - df$Number_2[2], df$Number_3[1] - df$Number_3[2], df$Number_4[1] - df$Number_4[2], df$TotalCount[1] - df$TotalCount[2])
  df <- df %>% mutate(Percent_M_E = (Number_3 + Number_4)/TotalCount)
  df <- df[-1, ]
  df_Sci <- df
  return(df_Sci)
}

#Generate base graph for a given school coming from a dataframe

base_graph <- function(df){
  graph <- df %>% ggplot(aes(fill=GroupName, x = TestSubject, y = Percent_M_E)) + geom_bar(position = "dodge", stat = "identity") + labs(x = "Subject", y = "Percent Meets or Exceeds", title = paste(df$SchName[1], "Test Scores")) + theme(legend.title = element_blank())
  return(graph)
}

StudentType <- c("Statewide ELL", "Statewide NonELL")
#linetype <- c(2,1)

#Code to add state data
add_state <- function(graph){
  graph <- graph + geom_segment(aes(x = 0.55, y = state[1], xend = 1, yend = state[1], linetype = "dashed")) + geom_segment(aes(x = 1, y = state[2], xend = 1.45, yend = state[2], linetype = "solid")) + geom_segment(aes(x = 1.55, y = state[3], xend  = 2, yend=state[3]),  linetype = 2) + geom_segment(aes(x=2, y = state[4], xend = 2.45, yend=state[4]), linetype = 1) + geom_segment(aes(x = 2.55, y = state[5], xend = 3, yend = state[5]),  linetype = 2) + geom_segment(aes(x=3, y=state[6], xend = 3.45, yend=state[6]), linetype = 1) 
  graph <- graph + scale_linetype_manual(values = c("dashed", "solid"), labels = StudentType)
  return(graph)
}

#Function to generate graph with state data and icon for bilingual progams


# Make graph for a given school based on code
graph <- function(df, code){
  graph <- base_graph(df)
  graph <- add_state(graph)
  if(code %in% ell)
    graph <- graph + inset_element(grid::rasterGrob(es), -0.1, 1.05, 0, 1.15)
  else
    graph <- graph
  return(graph)
}

#Given school code, create the graph

score_comps <- function(code){
  df_ELA <- ELA_scores(code)
  df_Math <- Math_scores(code)
  df_Sci <- Sci_scores(code)
  df <- rbind(df_ELA, df_Math, df_Sci)
  graph <- graph(df, code)
  return(graph)
}

#test
score_comps(28103)

#test
score_comps(28130)



