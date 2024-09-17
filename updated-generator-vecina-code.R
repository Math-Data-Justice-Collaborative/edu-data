install.packages("tidyverse")
install.packages("readxl")
install.packages("grDevices")
install.packages("png")
install.packages("patchwork")
install.packages("stringr")

library(tidyverse)
library(readxl)
library(grDevices)
library(png)
library(patchwork)
library(stringr)

#First we get the data.
#The data comes from reportcard.ride.ri.gov
#Website: https://reportcard.ride.ri.gov/ 
# Select “CustomView/Download Data: Select Datasets” and hit “Go”
# Select year
#Click on “download button” from corner of “Assessments” block which is in the first row, second column

#Delete the "Participation" sheet and just keep the "Performance" sheet

#Delete the district level data - these are those rows that do not have specific school names or school codes. 
#There should be about 4000 to 5000 rows to delete, they will be at the beginning.
#Save as "RI_scores.xlsx"
#go to the bottom of the sheet and cut (don't delete) the state level data, paste into a new sheet.

#Look only at the rows with GroupName = "ELL" and GroupName = "Not ELL"

#For the state level averages, For each exam subject, and only for non-ELL and ELL students, 
#have Excel calculate the percent of 3s and 4s by summing the columns "Level3_Percent" and "Level4_Percent".
#This gives you the percentage of those who met or exceeded state standards.
#For ELL and NonELL, put the percentage for meets/exceeds for the ELA, Math, and Science subjects as indicated below:
ELL_ELA <- 5.57 #insert percentage
NonELL_ELA <- 39.25 #insert percentage
ELL_Math <- 6.82 #insert percentage
NonELL_Math <- 32.28 #insert percentage
ELL_Science <- 3.20 #insert percentage
NonELL_Science <- 33.96 #insert percentage

#We put the state results into a vector:
state <- c(ELL_ELA, NonELL_ELA, ELL_Math, NonELL_Math, ELL_Science, NonELL_Science)

#While we are at it, let's set up a few other things we'll need.
#First, we want the labels for ELL and NonELL students in the state data.
StudentType <- c("Statewide ELL", "Statewide NonELL")

#We also will want the icon for schools with bilingual programs.
es <- readPNG("ES.png")

#Finally, we make a vector containing the school codes for schools with bilingual education
ell <- c(26601, 69601, 28103, 28134, 28153, 28157, 28144)

#26601 is International Charter School
#69601 is Nuestro Mundo Charter School
#28103 is Leviton Dual Language School which has dual language programs
#28134 is Frank D. Spaziano El School which has dual language programs
#28153 is William D'Abate El School which has developmental bilingual programs
#28157 is Lillian Feinstein El School which has transitional bilingual programs
#28144 is Gilbert Stuart Middle School, has "dual language immersion"

#Now we get the school-by-school results

#Pull out the sheet labeled "School Performance" as a separate spreadsheet, title it "RI_scores".
#Import it.

RI_scores <- read_excel("RI_scores.xlsx")

RI_scores[is.na(RI_scores)] <- 0


#Make a column with the percentage of meets and exceeds in each categorty (which means students scored 3 or 4), then delete the columns with individual levels
RI_scores <- RI_scores %>% mutate(Percent_M_E = 100*(Level3_Percent + Level4_Percent)) %>% select(-c("Level1_Count", "Level2_Count", "Level3_Count", "Level4_Count"))

#We keep only those columns that we need. Note that we need DistCode to filter to schools available to PVD
RI_scores <- RI_scores %>% select(c(DistCode, SchCode, SchName, TestSubject, GroupName, Total_Count, Percent_M_E))
#RI_scores <- RI_scores %>% select(-c("GroupType"))



#Now we only group the data for the PVD and charter districts available to students in Providence 
districts <- c(28, 41, 48, 68, 53, 52, 69, 51, 43, 55, 59, 58, 83, 61, 54, 81, 7, 63, 42, 62, 64)
PVD_scores <- RI_scores %>% filter(DistCode %in% districts)

#We no longer need the DistCode column.
PVD_scores <- PVD_scores %>% select(-c(DistCode))

#We want to limit to ELL and Not ELL students. The other GroupNames are other distinctions that we aren't looking at.
PVD_scores <- PVD_scores %>% filter(GroupName == "ELL" | GroupName == "Not ELL")


# Create three rows to add zero ELL students at the compass school. 

#En el futuro, we will need to rewrite this so that any schools without ELL rows (or Not ELL rows) can be populated.

#We do this by creating a new dataframe, then bind the new dataframe to PVD_scores. We have to do this for each row (for each test)
new_row <- data.frame(
  SchCode = 23601 ,
  SchName = "The Compass School" ,
  GroupName = "ELL",
  TestSubject = "ELA",
  Total_Count = 0,
  Percent_M_E = 0
)

PVD_scores <- rbind(PVD_scores, new_row)

new_row <- data.frame(
  SchCode = 23601 ,
  SchName = "The Compass School" ,
  GroupName = "ELL",
  TestSubject = "Math",
  Total_Count = 0,
  Percent_M_E = 0
)

PVD_scores <- rbind(PVD_scores, new_row)

new_row <- data.frame(
  SchCode = 23601 ,
  SchName = "The Compass School" ,
  GroupName = "ELL",
  TestSubject = "Science",
  Total_Count = 0,
  Percent_M_E = 0
)

PVD_scores <- rbind(PVD_scores, new_row)

##Now we make school names consistent

#function to change school names. We create a new dataframe with only the desired school, reassign the name, then delete the former rows from PVD_scores and then bind the new df
name_change <- function(number, name){
  temp <- PVD_scores %>% filter(SchCode == number)
  temp$SchName <- name
  PVD_scores <- PVD_scores %>% filter(!(PVD_scores$SchCode == number))
  PVD_scores <- rbind(PVD_scores, temp)
}

#Now we call the function to make the name changes 
PVD_scores <- name_change(number = 28198, name = "Times2 Middle and High School")
PVD_scores <- name_change(number = 28609, name = "Achievement First Providence Mayoral Academy Elementary")
PVD_scores <- name_change(number = 28614, name = "Achievement First Iluminar Mayoral Academy Elementary")
PVD_scores <- name_change(number = 28615, name = "Achievement First Providence Mayoral Academy Middle")
PVD_scores <- name_change(number = 41604, name = "Achievement First Iluminar Mayoral Academy Middle")


##Some schools didn't offer science assessments. We want the graph to still have the "science" label on the x-axis where the bars WOULD go if the test were administered. The following code accomplishes this.
for (code in PVD_scores$SchCode){  #loop over each school code
  s <- sum(PVD_scores$SchCode == code) #this gives us the total number of rows for a given school
  if (s == 4){  #if the number of rows is 4, they only offered ELA and math, but 6 if they also offered science.
    temp <- PVD_scores %>% filter(SchCode == code) #the strategy is to create a new dataframe with the new rows for science, put in 0s, and bind back to PVD_scores
    new_row <- data.frame(
      SchCode = code,
      SchName = temp$SchName[1],
      TestSubject = "Science",
      GroupName = "ELL",
      Total_Count = 0,
      Percent_M_E = 0
    )
    temp <- rbind(temp, new_row)
    second_row <- data.frame(
      SchCode = code,
      SchName = temp$SchName[1],
      TestSubject = "Science",
      GroupName = "Not ELL",
      Total_Count = 0,
      Percent_M_E = 0
    )
    temp <- rbind(temp, second_row)
    PVD_scores <- PVD_scores %>% filter(PVD_scores$SchCode != code)
    PVD_scores <- rbind(PVD_scores, temp)
  }
}

#The data is now cleaned, let's start building the graphs.
#We build the graph by first building the base graph - the bar graphs with the results.
#Then we add state data.
#We conclude by making the title and adding the icon for bilingual programs when appropriate.
#In each of these, the call will be with df = PVD_scores.

#Generate base graph for a given school coming from a dataframe
base_graph <- function(df, code){
  temp <- df %>% filter(SchCode == code)
  graph <- temp %>% ggplot(aes(fill=GroupName, x = TestSubject, y = Percent_M_E)) + geom_bar(position = "dodge", stat = "identity") + labs(x = "Test Subject", y = "Meets or Exceeds (%)", title = str_wrap(temp$SchName[1])) + theme(legend.title = element_blank())
  return(graph)
}

#Code to add state data
add_state <- function(graph){
  graph <- graph + geom_segment(aes(x = 0.55, y = state[1], xend = 1, yend = state[1], linetype = "dashed")) + geom_segment(aes(x = 1, y = state[2], xend = 1.45, yend = state[2], linetype = "solid")) + geom_segment(aes(x = 1.55, y = state[3], xend  = 2, yend=state[3]),  linetype = 2) + geom_segment(aes(x=2, y = state[4], xend = 2.45, yend=state[4]), linetype = 1) + geom_segment(aes(x = 2.55, y = state[5], xend = 3, yend = state[5]),  linetype = 2) + geom_segment(aes(x=3, y=state[6], xend = 3.45, yend=state[6]), linetype = 1) 
  graph <- graph + scale_linetype_manual(values = c("dashed", "solid"), labels = StudentType)
  return(graph)
}

#Function to generate graph with state data and icon for bilingual progams
make_graph <- function(df, code){
  graph <- base_graph(df, code)
  graph <- add_state(graph)
  if(code %in% ell)
    #    graph <- graph + inset_element(grid::rasterGrob(es), -0.1, 1.05, 0, 1.15)
    graph <- graph + inset_element(grid::rasterGrob(es), -0.1, 0.99, -0.03, 1.06)
  else
    graph <- graph
  return(graph)
}

#We conclude by generating the graphs and depositing them to a local folder.

#Because the school codes are strings, but we want to iterate through them, we create a numerical vector of school codes
PVD_scores$SchCode <- as.numeric(PVD_scores$SchCode)
v <- PVD_scores$SchCode
v <- unique(v)  #There is one entry for each test subject and group


#We create a loop to go through the vector v of school codes, generate the graph, and save the graph
i <- 1
while (i < length(v)+1) {
  temp <- PVD_scores %>% filter(SchCode == v[i]) #We first need to generate the file name
  name <- temp$SchCode[1]  #This is part of the file name
  path <- "C:\\Users\\piercev1\\Documents\\Research\\VECINA\\EdGraphs\\"  #This path will have to be changed to whatever path is used by the user.
  filename <- paste(path, name, ".png", sep = "")
  p <- make_graph(PVD_scores, v[i])
  ggsave(filename = filename, p)
  i <- i+1}