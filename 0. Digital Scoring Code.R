##########################################
#06/17/24
#Author: Carl Romer
#This file downloads data from O*NET and creates the digitalization scores
#As described in https://www.brookings.edu/wp-content/uploads/2017/11/mpp_2017nov15_digitalization_full_report.pdf
#Pages 10-20
#Data from: https://www.onetcenter.org/database.html#individual-files
##########################################
##########################################
#Setup
##########################################
library(tidyverse)
setwd('V:/Carl/AI/06.24 Digital Scoring Code')
##########################################
#Data Importation 2024
##########################################

knowledge_data2024 <- readxl::read_excel('Data/Knowledge 2024.xlsx',
                                    sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #Get rid of data that is missing
         !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
         ) %>% 
  select(-c(Level, Importance))

activities_data2024 <- readxl::read_excel('Data/Work Activities 2024.xlsx',
                                      sheet = 1) %>% 
  filter(`Element Name` == 'Working with Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2024 <-inner_join(activities_data2024, #Join 2 datasets
                                   knowledge_data2024) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

############################
#2023
############################
knowledge_data2023 <- readxl::read_excel('Data/Knowledge 2023.xlsx',
                                     sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
filter(!is.na(Importance), #Get rid of data that is missing
       !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

activities_data2023 <- readxl::read_excel('Data/Work Activities 2023.xlsx',
                                      sheet = 1) %>% 
  filter(`Element Name` == 'Working with Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2023 <-inner_join(activities_data2023, #Join 2 datasets
                                   knowledge_data2023) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

##########################################
#Data Importation 2022
##########################################
knowledge_data2022 <- readxl::read_excel('Data/Knowledge 2022.xlsx',
                                         sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #Get rid of data that is missing
         !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

activities_data2022 <- readxl::read_excel('Data/Work Activities 2022.xlsx',
                                          sheet = 1) %>% 
  filter(`Element Name` == 'Working with Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2022 <-inner_join(activities_data2022, #Join 2 datasets
                                       knowledge_data2022) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

############################
#2021
############################
knowledge_data2021 <- readxl::read_excel('Data/Knowledge 2021.xlsx',
                                         sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #Get rid of data that is missing
         !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

activities_data2021 <- readxl::read_excel('Data/Work Activities 2021.xlsx',
                                          sheet = 1) %>% 
  filter(`Element Name` == 'Interacting With Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2021 <-inner_join(activities_data2021, #Join 2 datasets
                                       knowledge_data2021) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

############################
#2020
############################
knowledge_data2020 <- readxl::read_excel('Data/Knowledge 2020.xlsx',
                                         sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #Get rid of data that is missing
         !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

activities_data2020 <- readxl::read_excel('Data/Work Activities 2020.xlsx',
                                          sheet = 1) %>% 
  filter(`Element Name` == 'Interacting With Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2020 <-inner_join(activities_data2020, #Join 2 datasets
                                       knowledge_data2020) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

############################
#2019
############################
knowledge_data2019 <- readxl::read_excel('Data/Knowledge 2019.xlsx',
                                         sheet = 1) %>% 
  filter(`Element Name` == 'Computers and Electronics') %>% #look only at relevant data
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #Turn data from long to wide
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #Get rid of data that is missing
         !is.na(Level)) %>% 
  mutate(knowledge_Level = (Level)/(7)*100, #Turn data into 100 scale
         knowledge_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

activities_data2019 <- readxl::read_excel('Data/Work Activities 2019.xlsx',
                                          sheet = 1) %>% 
  filter(`Element Name` == 'Interacting With Computers') %>% #look only at relevant data 
  select(-c(`Standard Error`, #Get rid of irrelevant columns
            `Lower CI Bound`,
            `Upper CI Bound`,
            `Not Relevant`,
            `Element ID`,
            `Scale ID`,
            Date,
            N,
            `Recommend Suppress`,
            `Element Name`,
            `Domain Source`)) %>% 
  pivot_wider(names_from = 'Scale Name', #change from wide to long
              values_from = 'Data Value') %>% 
  filter(!is.na(Importance), #get rid of missing data
         !is.na(Level)) %>% 
  mutate(activity_Level = (Level)/(7)*100, #Convert to 100 scale
         activity_Importance = (Importance-1)/(5-1)*100
  ) %>% 
  select(-c(Level, Importance))

digitalization_scores2019 <-inner_join(activities_data2019, #Join 2 datasets
                                       knowledge_data2019) %>% 
  mutate(digital_score = (sqrt(knowledge_Level*knowledge_Importance) + #convert scores to digitalization score
                            sqrt(activity_Level * activity_Importance))/2) %>% 
  filter(!is.na(digital_score)) %>% 
  mutate(SOC = str_sub(`O*NET-SOC Code`, end = 7)) %>% #create SOC (which will have multiple observations)
  group_by(SOC) %>% 
  mutate(present = any(str_detect(`O*NET-SOC Code`, '.00')), #the O*NET scores ending with .00 will be the general SOC score
         Score = case_when(present == T ~ digital_score, #For observations that have .00, they'll retain their score
                           present == F ~ mean(digital_score))) %>% #For observations that do not have .00, we take the average digitalization score by group
  filter(`O*NET-SOC Code` == min(`O*NET-SOC Code`)) %>% #We only keep the observations that have .00, or that have been averaged by group
  ungroup() %>% 
  select(SOC, #keep only relevant data
         Title, 
         Score) 

digital_scores_total <- dplyr::full_join(full_join(digitalization_scores2024 %>% 
                                    rename('score2024' = 'Score' ),
                                    digitalization_scores2023 %>% 
                                      rename('score2023' = 'Score')),
                                    dplyr::full_join(digitalization_scores2022 %>% 
                                                       rename('score2022' = 'Score'),
                                  dplyr::full_join(digitalization_scores2021 %>% 
                                    rename('score2021' = 'Score'),
                                    dplyr::full_join(digitalization_scores2020%>% 
                                                       rename('score2020' = 'Score'),
                                                     digitalization_scores2019%>% 
                                                       rename('score2019' = 'Score'))))) %>% 
  #select(-Title) %>% 
  group_by(SOC) %>% 
  mutate(score2024 = as.numeric(max(score2024, na.rm=T)),
         score2023 = as.numeric(max(score2023, na.rm=T)),
         score2022 = as.numeric(max(score2022, na.rm=T)),
         score2021 = as.numeric(max(score2021, na.rm=T)),
         score2020 = as.numeric(max(score2020, na.rm=T)),
         score2019 = as.numeric(max(score2019, na.rm=T))) %>% 
  distinct()
  

 write_csv(digital_scores_total,
          'digital scores over time.csv')
 
 write_csv(digitalization_scores2023,
           'digital scores2023.csv')
 