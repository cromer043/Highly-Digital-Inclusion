##########################################
# Date: 2024/05/14
# Author: Carl Romer
# Purpose: This code replicates all the work Yang did on the data last year for old data to be repurposed for new data (Including 2023)
##########################################
#Setup
##########################################
library(tidyverse)
library(fuzzyjoin)
setwd('V:/Carl/AI/05.24 Inclusion report/0. Data')
##########################################
#Data Importation
##########################################

national_data <- read_csv('Occupation_Table_798_Occupations_in_United_States_5751.csv')

demographics_data <- read_csv('Occupation_Demographics_Table_All_Occupations_in_United_States_2229.csv')
#microdata <- read_csv('usa_00061.csv')

historical_digitalization <- read_csv('historical level of digitalization.csv')

digital_scores2019_2024 <- read_csv('V:/Carl/AI/06.24 Digital Scoring Code/digital scores over time.csv')

ipeds_data <- read_csv('STEM degrees_all US institutions_2022.csv')


digital_scores <- read_csv('V:/Carl/AI/06.24 Digital Scoring Code/digital scores2023.csv')

pew_data_internet <- read_csv('chart-export-c48d607c-fdde-4d3d-9f44-9e350c859279.csv',
                              skip = 3 )

pew_data_internet$Year = ymd(sprintf("%d-06-01",as.numeric(pew_data_internet$Year)))

pew_data_smartphone <- read_csv('chart-export-7cfdecc4-bb29-4a14-96f5-5ba4acd5c1a9.csv',
                                skip = 2)

pathways_data <- read_csv('Pathways data.csv')

demographics2022 <- tidycensus::get_acs(geography = "us",
                                        variables = c('DP05_0002PE',
                                                      "DP05_0003PE", 
                                                      "DP05_0079PE",
                                                      "DP05_0080PE",
                                                      "DP05_0081PE",
                                                      "DP05_0082PE",
                                                      "DP05_0083PE",
                                                      'DP05_0085PE',
                                                      'DP05_0003PE',
                                                      'DP05_0073PE',
                                                      'DP05_0084PE'
                                        ),
                                        key =  #INSERT KEY HERE ,
                                        year = 2022,
                                        output = "wide")

metro_demographics2022 <- tidycensus::get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                                              variables = c('DP05_0001E',
                                                            'DP05_0003E',
                                                            'DP05_0002E',
                                                            'DP05_0079E',
                                                            'DP05_0080E',
                                                            "DP05_0081E",
                                                            "DP05_0082E",
                                                            "DP05_0083E",
                                                            'DP05_0085E',
                                                            'DP05_0003E',
                                                            'DP05_0073E',
                                                            "DP05_0003PE", 
                                                            "DP05_0079PE",
                                                            "DP05_0080PE",
                                                            "DP05_0081PE",
                                                            "DP05_0082PE",
                                                            "DP05_0083PE",
                                                            'DP05_0085PE',
                                                            'DP05_0073PE'
                                              ),
                                              key =  #INSERT KEY HERE,
                                              year = 2022,
                                              output = "wide")

files <- list.files('V:/Carl/AI/05.24 Inclusion report/0. Data/Metro data/')
setwd('V:/Carl/AI/05.24 Inclusion report/0. Data/Metro data/')
helper <- lapply(files, read_csv)

for(i in seq_along(helper)){
  print(str_remove_all(str_remove_all(files[i],
                                      'Occupation_Table_All_Occupations_in_'),
                       '.csv'))
  helper[[i]][['Metro']] <- str_remove_all(str_remove_all(str_remove_all(str_remove_all(files[i],
                                                                                        'Occupation_Table_All_Occupations_in_'),
                                                                         '.csv'),
                                                          "_"),
                                           '[:digit:]')
}
metro_data <- data.frame(matrix(ncol = length(colnames(helper[[1]])),
                                nrow = 0))
colnames(metro_data) <- colnames(helper[[1]])

for(i in seq_along(helper)){
  metro_data <- rbind(metro_data,
                      helper[[i]])
}


files <- list.files('V:/Carl/AI/05.24 Inclusion report/0. Data/State data/')
setwd('V:/Carl/AI/05.24 Inclusion report/0. Data/State data/')
helper <- lapply(files, read_csv)

for(i in seq_along(helper)){
  print(str_remove_all(str_remove_all(files[i],
                                      'Occupation_Table_All_Occupations_in_'),
                       '.csv'))
  helper[[i]][['State']] <- str_remove_all(str_remove_all(str_remove_all(str_remove_all(files[i],
                                                                                        'Occupation_Table_All_Occupations_in_'),
                                                                         '.csv'),
                                                          "_"),
                                           '[:digit:]')
}
state_data <- data.frame(matrix(ncol = length(colnames(helper[[1]])),
                                nrow = 0))
colnames(state_data) <- colnames(helper[[1]])

for(i in seq_along(helper)){
  state_data <- rbind(state_data,
                      helper[[i]])
}
##########################################
#Functions
##########################################
'new_row<-'<- function(x, value){x[nrow(x) + 1,] <- value; x}

##########################################
#Data Cleaning
##########################################
digital_scores <- digital_scores %>% 
  rename('soc_code' = "SOC"
  ) %>% 
  mutate(CEM = factor(case_when(str_detect(soc_code, '11-') == T |
                                  str_detect(soc_code, '15-') == T |
                                  str_detect(soc_code, '17-') == T  ~ 1,
                                T ~ 0),levels = c(0,1),
                      labels = c("non-CEM",'CEM')
  ),
  category = case_when(`Score` > 60 ~ "High",
                       `Score` >= 33 ~ "Medium",
                       `Score` < 33 ~ "Low"
  )
  )

merged_data <- inner_join(digital_scores, national_data %>% 
                            rename('soc_code' = "SOC")) %>% 
  mutate(
    bachelors = case_when(`Typical Entry Level Education` %in% c("Bachelor's degree",
                                                                 "Doctoral or professional degree",
                                                                 "Master's degree") ~ 1,
                          T~ 0),
    industry = str_sub(soc_code, end = '3'),
    industry = factor(industry, 
                      levels = c("11-",
                                 "13-",
                                 "15-",
                                 "17-",
                                 "19-",
                                 "21-",
                                 "23-",
                                 "25-",
                                 "27-",
                                 "29-",
                                 "31-",
                                 "33-",
                                 "35-",
                                 "37-",
                                 "39-",
                                 "41-",
                                 "43-",
                                 "45-",
                                 "47-",
                                 "49-",
                                 "51-",
                                 "53-"),
                      labels = c('Management',
                                 'Business and Financial Operations',
                                 'Computer and Mathematical',
                                 'Architecture and Engineering',
                                 'Life, Physical, and Social Science',
                                 'Community and Social Service',
                                 'Legal',
                                 'Educational Instruction and Library',
                                 'Arts, Design, Entertainment, Sports, and Media',
                                 'Healthcare Practitioners and Technical',
                                 'Healthcare Support',
                                 'Protective Service',
                                 'Food Preparation and Serving Related',
                                 'Building and Grounds Cleaning and Maintenance',
                                 'Personal Care and Service',
                                 'Sales and Related',
                                 'Office and Administrative Support',
                                 'Farming, Fishing, and Forestry',
                                 'Construction and Extraction',
                                 'Installation, Maintenance, and Repair',
                                 'Production',
                                 'Transportation and Material Moving'))) %>% 
  group_by(bachelors, category, CEM) %>%
  mutate(Jobs2023_education_digitalization_CEM = sum(`2023 Jobs...4`,
                                                     na.rm = T),
         Wages2023_education_digitalization_CEM = weighted.mean(`Avg. Annual Earnings`,
                                                                w = `2023 Jobs...4`)) %>% 
  group_by(category,
           add = FALSE) %>% 
  mutate(Jobs2002_digitalization = sum(`2002 Jobs`, 
                                       na.rm=T),
         Jobs2010_digitalization = sum(`2010 Jobs...29`,
                                       na.rm=T),
         Jobs2011_digitalization = sum(`2011 Jobs`,
                                       na.rm=T),
         Jobs2012_digitalization = sum(`2012 Jobs`,
                                       na.rm=T),
         Jobs2013_digitalization = sum(`2013 Jobs`,
                                       na.rm=T),
         Jobs2014_digitalization = sum(`2014 Jobs`,
                                       na.rm=T),
         Jobs2015_digitalization = sum(`2015 Jobs`,
                                       na.rm=T),
         Jobs2016_digitalization = sum(`2016 Jobs`,
                                       na.rm=T),
         Jobs2017_digitalization = sum(`2017 Jobs`,
                                       na.rm=T),
         Jobs2018_digitalization = sum(`2018 Jobs`,
                                       na.rm=T),
         Jobs2019_digitalization = sum(`2019 Jobs`,
                                       na.rm=T),
         Jobs2020_digitalization = sum(`2020 Jobs`,
                                       na.rm=T),
         Jobs2021_digitalization = sum(`2021 Jobs`,
                                       na.rm=T),
         Jobs2022_digitalization = sum(`2022 Jobs`,
                                       na.rm=T),
         Jobs2023_digitalization = sum(`2023 Jobs...41`,
                                       na.rm=T)) %>% 
  group_by(industry,
           category, 
           add = FALSE) %>% 
  mutate(Jobs2010_industry_digitalization = sum(`2010 Jobs...29`,
                                                na.rm=T),
         Jobs2011_industry_digitalization = sum(`2011 Jobs`,
                                                na.rm=T),
         Jobs2012_industry_digitalization = sum(`2012 Jobs`,
                                                na.rm=T),
         Jobs2013_industry_digitalization = sum(`2013 Jobs`,
                                                na.rm=T),
         Jobs2014_industry_digitalization = sum(`2014 Jobs`,
                                                na.rm=T),
         Jobs2015_industry_digitalization = sum(`2015 Jobs`,
                                                na.rm=T),
         Jobs2016_industry_digitalization = sum(`2016 Jobs`,
                                                na.rm=T),
         Jobs2017_industry_digitalization = sum(`2017 Jobs`,
                                                na.rm=T),
         Jobs2018_industry_digitalization = sum(`2018 Jobs`,
                                                na.rm=T),
         Jobs2019_industry_digitalization = sum(`2019 Jobs`,
                                                na.rm=T),
         Jobs2020_industry_digitalization = sum(`2020 Jobs`,
                                                na.rm=T),
         Jobs2021_industry_digitalization = sum(`2021 Jobs`,
                                                na.rm=T),
         Jobs2022_industry_digitalization = sum(`2022 Jobs`,
                                                na.rm=T),
         Jobs2023_industry_digitalization = sum(`2023 Jobs...4`,
                                                na.rm=T),
         Wages2023_industry_digitalization = weighted.mean(`Avg. Annual Earnings`,
                                                           w = `2023 Jobs...4`),
         AIAN_industry_digitalization = sum(`Current Year American Indian or Alaska Native`,
                                            na.rm=T),
         Hispanic_industry_digitalization = sum(`Current Year Hispanic or Latino`,
                                                na.rm=T),
         Black_industry_digitalization = sum(`Current Year Black or African American`,
                                             na.rm=T),
         PI_industry_digitalization = sum(`Current Year Native Hawaiian or Other Pacific Islander`,
                                          na.rm=T),
         AA_industry_digitalization = sum(`Current Year Asian`,
                                          na.rm=T),
         White_industry_digitalization = sum(`Current Year White`,
                                             na.rm=T),
         Twoplus_industry_digitalization = sum(`Current Year Two or More Races`,
                                               na.rm=T),
         Multi_industry_digitalization = sum(`Current Year Two or More Races`,
                                             na.rm=T),
         Female_industry_digitalization = sum(`Current Year Females`,
                                              na.rm=T),
         Male_industry_digitalization = sum(`Current Year Males`,
                                            na.rm=T)
  ) %>% 
  group_by(CEM,
           category, 
           add = FALSE) %>% 
  mutate(Jobs2010_CEM_digitalization = sum(`2010 Jobs...29`,
                                           na.rm=T),
         Jobs2011_CEM_digitalization = sum(`2011 Jobs`,
                                           na.rm=T),
         Jobs2012_CEM_digitalization = sum(`2012 Jobs`,
                                           na.rm=T),
         Jobs2013_CEM_digitalization = sum(`2013 Jobs`,
                                           na.rm=T),
         Jobs2014_CEM_digitalization = sum(`2014 Jobs`,
                                           na.rm=T),
         Jobs2015_CEM_digitalization = sum(`2015 Jobs`,
                                           na.rm=T),
         Jobs2016_CEM_digitalization = sum(`2016 Jobs`,
                                           na.rm=T),
         Jobs2017_CEM_digitalization = sum(`2017 Jobs`,
                                           na.rm=T),
         Jobs2018_CEM_digitalization = sum(`2018 Jobs`,
                                           na.rm=T),
         Jobs2019_CEM_digitalization = sum(`2019 Jobs`,
                                           na.rm=T),
         Jobs2020_CEM_digitalization = sum(`2020 Jobs`,
                                           na.rm=T),
         Jobs2021_CEM_digitalization = sum(`2021 Jobs`,
                                           na.rm=T),
         Jobs2022_CEM_digitalization = sum(`2022 Jobs`,
                                           na.rm=T),
         Jobs2023_CEM_digitalization = sum(`2023 Jobs...4`,
                                           na.rm=T)
  ) %>% 
  group_by(industry, 
           add = FALSE) %>% 
  mutate(Jobs2023_industry = sum(`2023 Jobs...4`, na.rm=T) ) %>% 
  ungroup() %>% 
  mutate(underrepresented_workers_of_color_percent = (`Current Year Two or More Races` +
                                                        `Current Year Native Hawaiian or Other Pacific Islander` +
                                                        `Current Year Hispanic or Latino`+
                                                        `Current Year Black or African American`+
                                                        `Current Year American Indian or Alaska Native`) / 
           `2023 Jobs...4`,
         
         underrepresented_women_percent =  `Current Year Females` /
           `2023 Jobs...4`)


demographics_data_merged <- inner_join(
  merged_data %>% 
    select(Description,
           category,
           CEM,
           industry,
           `2010 Jobs...3`,
           `2022 Jobs`,
           `2023 Jobs...4`
    ),
  demographics_data)

metro_data_merged <- inner_join(digital_scores, metro_data %>% 
                                  rename('soc_code' = "SOC")) %>% 
  mutate(
    industry = str_sub(soc_code, end = '3'),
    industry = factor(industry, 
                      levels = c("11-",
                                 "13-",
                                 "15-",
                                 "17-",
                                 "19-",
                                 "21-",
                                 "23-",
                                 "25-",
                                 "27-",
                                 "29-",
                                 "31-",
                                 "33-",
                                 "35-",
                                 "37-",
                                 "39-",
                                 "41-",
                                 "43-",
                                 "45-",
                                 "47-",
                                 "49-",
                                 "51-",
                                 "53-"),
                      labels = c('Management',
                                 'Business and Financial Operations',
                                 'Computer and Mathematical',
                                 'Architecture and Engineering',
                                 'Life, Physical, and Social Science',
                                 'Community and Social Service',
                                 'Legal',
                                 'Educational Instruction and Library',
                                 'Arts, Design, Entertainment, Sports, and Media',
                                 'Healthcare Practitioners and Technical',
                                 'Healthcare Support',
                                 'Protective Service',
                                 'Food Preparation and Serving Related',
                                 'Building and Grounds Cleaning and Maintenance',
                                 'Personal Care and Service',
                                 'Sales and Related',
                                 'Office and Administrative Support',
                                 'Farming, Fishing, and Forestry',
                                 'Construction and Extraction',
                                 'Installation, Maintenance, and Repair',
                                 'Production',
                                 'Transportation and Material Moving'))) %>% 
  group_by(Metro,
           add = FALSE) %>% 
  mutate(jobs2023_total = sum(`2023 Jobs...4`)) %>% 
  group_by(category, Metro,
           add = FALSE) %>% 
  mutate(Jobs2002_digitalization = sum(`2002 Jobs`, 
                                       na.rm=T),
         Jobs2010_digitalization = sum(`2010 Jobs...29`,
                                       na.rm=T),
         Jobs2011_digitalization = sum(`2011 Jobs`,
                                       na.rm=T),
         Jobs2012_digitalization = sum(`2012 Jobs`,
                                       na.rm=T),
         Jobs2013_digitalization = sum(`2013 Jobs`,
                                       na.rm=T),
         Jobs2014_digitalization = sum(`2014 Jobs`,
                                       na.rm=T),
         Jobs2015_digitalization = sum(`2015 Jobs`,
                                       na.rm=T),
         Jobs2016_digitalization = sum(`2016 Jobs`,
                                       na.rm=T),
         Jobs2017_digitalization = sum(`2017 Jobs`,
                                       na.rm=T),
         Jobs2018_digitalization = sum(`2018 Jobs`,
                                       na.rm=T),
         Jobs2019_digitalization = sum(`2019 Jobs`,
                                       na.rm=T),
         Jobs2020_digitalization = sum(`2020 Jobs`,
                                       na.rm=T),
         Jobs2021_digitalization = sum(`2021 Jobs`,
                                       na.rm=T),
         Jobs2022_digitalization = sum(`2022 Jobs`,
                                       na.rm=T),
         Jobs2023_digitalization = sum(`2023 Jobs...41`,
                                       na.rm=T)) %>% 
  group_by(industry, Metro,
           category, 
           add = FALSE) %>% 
  mutate(Jobs2010_industry_digitalization = sum(`2010 Jobs...29`,
                                                na.rm=T),
         Jobs2011_industry_digitalization = sum(`2011 Jobs`,
                                                na.rm=T),
         Jobs2012_industry_digitalization = sum(`2012 Jobs`,
                                                na.rm=T),
         Jobs2013_industry_digitalization = sum(`2013 Jobs`,
                                                na.rm=T),
         Jobs2014_industry_digitalization = sum(`2014 Jobs`,
                                                na.rm=T),
         Jobs2015_industry_digitalization = sum(`2015 Jobs`,
                                                na.rm=T),
         Jobs2016_industry_digitalization = sum(`2016 Jobs`,
                                                na.rm=T),
         Jobs2017_industry_digitalization = sum(`2017 Jobs`,
                                                na.rm=T),
         Jobs2018_industry_digitalization = sum(`2018 Jobs`,
                                                na.rm=T),
         Jobs2019_industry_digitalization = sum(`2019 Jobs`,
                                                na.rm=T),
         Jobs2020_industry_digitalization = sum(`2020 Jobs`,
                                                na.rm=T),
         Jobs2021_industry_digitalization = sum(`2021 Jobs`,
                                                na.rm=T),
         Jobs2022_industry_digitalization = sum(`2022 Jobs`,
                                                na.rm=T),
         Jobs2023_industry_digitalization = sum(`2023 Jobs...4`,
                                                na.rm=T),
         Wages2023_industry_digitalization = weighted.mean(`Avg. Annual Earnings`,
                                                           w = `2023 Jobs...4`),
         AIAN_industry_digitalization = sum(`Current Year American Indian or Alaska Native`,
                                            na.rm=T),
         Hispanic_industry_digitalization = sum(`Current Year Hispanic or Latino`,
                                                na.rm=T),
         Black_industry_digitalization = sum(`Current Year Black or African American`,
                                             na.rm=T),
         PI_industry_digitalization = sum(`Current Year Native Hawaiian or Other Pacific Islander`,
                                          na.rm=T),
         AA_industry_digitalization = sum(`Current Year Asian`,
                                          na.rm=T),
         White_industry_digitalization = sum(`Current Year White`,
                                             na.rm=T),
         Twoplus_industry_digitalization = sum(`Current Year Two or More Races`,
                                               na.rm=T),
         Multi_industry_digitalization = sum(`Current Year Two or More Races`,
                                             na.rm=T),
         Female_industry_digitalization = sum(`Current Year Females`,
                                              na.rm=T),
  ) %>% 
  group_by(CEM,
           category,
           Metro, 
           add = FALSE) %>% 
  mutate(Jobs2010_CEM_digitalization = sum(`2010 Jobs...29`,
                                           na.rm=T),
         Jobs2011_CEM_digitalization = sum(`2011 Jobs`,
                                           na.rm=T),
         Jobs2012_CEM_digitalization = sum(`2012 Jobs`,
                                           na.rm=T),
         Jobs2013_CEM_digitalization = sum(`2013 Jobs`,
                                           na.rm=T),
         Jobs2014_CEM_digitalization = sum(`2014 Jobs`,
                                           na.rm=T),
         Jobs2015_CEM_digitalization = sum(`2015 Jobs`,
                                           na.rm=T),
         Jobs2016_CEM_digitalization = sum(`2016 Jobs`,
                                           na.rm=T),
         Jobs2017_CEM_digitalization = sum(`2017 Jobs`,
                                           na.rm=T),
         Jobs2018_CEM_digitalization = sum(`2018 Jobs`,
                                           na.rm=T),
         Jobs2019_CEM_digitalization = sum(`2019 Jobs`,
                                           na.rm=T),
         Jobs2020_CEM_digitalization = sum(`2020 Jobs`,
                                           na.rm=T),
         Jobs2021_CEM_digitalization = sum(`2021 Jobs`,
                                           na.rm=T),
         Jobs2022_CEM_digitalization = sum(`2022 Jobs`,
                                           na.rm=T),
         Jobs2023_CEM_digitalization = sum(`2023 Jobs...4`,
                                           na.rm=T)
  ) %>% 
  group_by(industry,
           Metro, 
           add = FALSE) %>% 
  mutate(Jobs2023_industry = sum(`2023 Jobs...4`, na.rm=T) ) %>% 
  ungroup() %>% 
  mutate(underrepresented_workers_of_color_percent = (`Current Year Two or More Races` +
                                                        `Current Year Native Hawaiian or Other Pacific Islander` +
                                                        `Current Year Hispanic or Latino`+
                                                        `Current Year Black or African American`+
                                                        `Current Year American Indian or Alaska Native`) / 
           `2023 Jobs...4`,
         
         underrepresented_women_percent =  `Current Year Females` /
           `2023 Jobs...4`)

state_data_merged <- inner_join(digital_scores, state_data %>% 
                                  rename('soc_code' = "SOC")) %>% 
  mutate(
    
    industry = str_sub(soc_code, end = '3'),
    industry = factor(industry, 
                      levels = c("11-",
                                 "13-",
                                 "15-",
                                 "17-",
                                 "19-",
                                 "21-",
                                 "23-",
                                 "25-",
                                 "27-",
                                 "29-",
                                 "31-",
                                 "33-",
                                 "35-",
                                 "37-",
                                 "39-",
                                 "41-",
                                 "43-",
                                 "45-",
                                 "47-",
                                 "49-",
                                 "51-",
                                 "53-"),
                      labels = c('Management',
                                 'Business and Financial Operations',
                                 'Computer and Mathematical',
                                 'Architecture and Engineering',
                                 'Life, Physical, and Social Science',
                                 'Community and Social Service',
                                 'Legal',
                                 'Educational Instruction and Library',
                                 'Arts, Design, Entertainment, Sports, and Media',
                                 'Healthcare Practitioners and Technical',
                                 'Healthcare Support',
                                 'Protective Service',
                                 'Food Preparation and Serving Related',
                                 'Building and Grounds Cleaning and Maintenance',
                                 'Personal Care and Service',
                                 'Sales and Related',
                                 'Office and Administrative Support',
                                 'Farming, Fishing, and Forestry',
                                 'Construction and Extraction',
                                 'Installation, Maintenance, and Repair',
                                 'Production',
                                 'Transportation and Material Moving'))) %>% 
  group_by(State) %>% 
  mutate(jobs2023_total = sum(`2023 Jobs...4`)) %>% 
  group_by(category, State,
           add = FALSE) %>% 
  mutate(Jobs2002_digitalization = sum(`2002 Jobs`, 
                                       na.rm=T),
         Jobs2010_digitalization = sum(`2010 Jobs...29`,
                                       na.rm=T),
         Jobs2011_digitalization = sum(`2011 Jobs`,
                                       na.rm=T),
         Jobs2012_digitalization = sum(`2012 Jobs`,
                                       na.rm=T),
         Jobs2013_digitalization = sum(`2013 Jobs`,
                                       na.rm=T),
         Jobs2014_digitalization = sum(`2014 Jobs`,
                                       na.rm=T),
         Jobs2015_digitalization = sum(`2015 Jobs`,
                                       na.rm=T),
         Jobs2016_digitalization = sum(`2016 Jobs`,
                                       na.rm=T),
         Jobs2017_digitalization = sum(`2017 Jobs`,
                                       na.rm=T),
         Jobs2018_digitalization = sum(`2018 Jobs`,
                                       na.rm=T),
         Jobs2019_digitalization = sum(`2019 Jobs`,
                                       na.rm=T),
         Jobs2020_digitalization = sum(`2020 Jobs`,
                                       na.rm=T),
         Jobs2021_digitalization = sum(`2021 Jobs`,
                                       na.rm=T),
         Jobs2022_digitalization = sum(`2022 Jobs`,
                                       na.rm=T),
         Jobs2023_digitalization = sum(`2023 Jobs...41`,
                                       na.rm=T)) %>% 
  group_by(industry, 
           State,
           category, 
           add = FALSE) %>% 
  mutate(Jobs2010_industry_digitalization = sum(`2010 Jobs...29`,
                                                na.rm=T),
         Jobs2011_industry_digitalization = sum(`2011 Jobs`,
                                                na.rm=T),
         Jobs2012_industry_digitalization = sum(`2012 Jobs`,
                                                na.rm=T),
         Jobs2013_industry_digitalization = sum(`2013 Jobs`,
                                                na.rm=T),
         Jobs2014_industry_digitalization = sum(`2014 Jobs`,
                                                na.rm=T),
         Jobs2015_industry_digitalization = sum(`2015 Jobs`,
                                                na.rm=T),
         Jobs2016_industry_digitalization = sum(`2016 Jobs`,
                                                na.rm=T),
         Jobs2017_industry_digitalization = sum(`2017 Jobs`,
                                                na.rm=T),
         Jobs2018_industry_digitalization = sum(`2018 Jobs`,
                                                na.rm=T),
         Jobs2019_industry_digitalization = sum(`2019 Jobs`,
                                                na.rm=T),
         Jobs2020_industry_digitalization = sum(`2020 Jobs`,
                                                na.rm=T),
         Jobs2021_industry_digitalization = sum(`2021 Jobs`,
                                                na.rm=T),
         Jobs2022_industry_digitalization = sum(`2022 Jobs`,
                                                na.rm=T),
         Jobs2023_industry_digitalization = sum(`2023 Jobs...4`,
                                                na.rm=T),
         Wages2023_industry_digitalization = weighted.mean(`Avg. Annual Earnings`,
                                                           w = `2023 Jobs...4`),
         AIAN_industry_digitalization = sum(`Current Year American Indian or Alaska Native`,
                                            na.rm=T),
         Hispanic_industry_digitalization = sum(`Current Year Hispanic or Latino`,
                                                na.rm=T),
         Black_industry_digitalization = sum(`Current Year Black or African American`,
                                             na.rm=T),
         PI_industry_digitalization = sum(`Current Year Native Hawaiian or Other Pacific Islander`,
                                          na.rm=T),
         AA_industry_digitalization = sum(`Current Year Asian`,
                                          na.rm=T),
         White_industry_digitalization = sum(`Current Year White`,
                                             na.rm=T),
         Twoplus_industry_digitalization = sum(`Current Year Two or More Races`,
                                               na.rm=T),
         Multi_industry_digitalization = sum(`Current Year Two or More Races`,
                                             na.rm=T),
         Female_industry_digitalization = sum(`Current Year Females`,
                                              na.rm=T),
  ) %>% 
  group_by(CEM,
           category,
           State, 
           add = FALSE) %>% 
  mutate(Jobs2010_CEM_digitalization = sum(`2010 Jobs...29`,
                                           na.rm=T),
         Jobs2011_CEM_digitalization = sum(`2011 Jobs`,
                                           na.rm=T),
         Jobs2012_CEM_digitalization = sum(`2012 Jobs`,
                                           na.rm=T),
         Jobs2013_CEM_digitalization = sum(`2013 Jobs`,
                                           na.rm=T),
         Jobs2014_CEM_digitalization = sum(`2014 Jobs`,
                                           na.rm=T),
         Jobs2015_CEM_digitalization = sum(`2015 Jobs`,
                                           na.rm=T),
         Jobs2016_CEM_digitalization = sum(`2016 Jobs`,
                                           na.rm=T),
         Jobs2017_CEM_digitalization = sum(`2017 Jobs`,
                                           na.rm=T),
         Jobs2018_CEM_digitalization = sum(`2018 Jobs`,
                                           na.rm=T),
         Jobs2019_CEM_digitalization = sum(`2019 Jobs`,
                                           na.rm=T),
         Jobs2020_CEM_digitalization = sum(`2020 Jobs`,
                                           na.rm=T),
         Jobs2021_CEM_digitalization = sum(`2021 Jobs`,
                                           na.rm=T),
         Jobs2022_CEM_digitalization = sum(`2022 Jobs`,
                                           na.rm=T),
         Jobs2023_CEM_digitalization = sum(`2023 Jobs...4`,
                                           na.rm=T)
  ) %>% 
  group_by(industry,
           State, 
           add = FALSE) %>% 
  mutate(Jobs2023_industry = sum(`2023 Jobs...4`, na.rm=T) ) %>% 
  ungroup() %>% 
  mutate(underrepresented_workers_of_color_percent = (`Current Year Two or More Races` +
                                                        `Current Year Native Hawaiian or Other Pacific Islander` +
                                                        `Current Year Hispanic or Latino`+
                                                        `Current Year Black or African American`+
                                                        `Current Year American Indian or Alaska Native`) / 
           `2023 Jobs...4`,
         
         underrepresented_women_percent =  `Current Year Females` /
           `2023 Jobs...4`)

demographics <- demographics2022 %>% 
  mutate(
    'Indigeneous peoples' = DP05_0081PE + DP05_0083PE,
    
  ) %>% 
  select('Male' = 'DP05_0002PE',
         "Female" = "DP05_0003PE", 
         "White" = "DP05_0079PE",
         "Black" = "DP05_0080PE",
         "Asian" = "DP05_0082PE",
         "Latino or Hispanic" = 'DP05_0073PE',
         'Some other race' = 'DP05_0084PE',
         'Indigeneous peoples',
         'Multiracial' = 'DP05_0085PE') %>% 
  pivot_longer(everything(),
               values_to = 'Share of Population',
               names_to = 'group')

metro_demographics <- metro_demographics2022 %>% 
  mutate(
    Minority = DP05_0080PE + DP05_0073PE +DP05_0083PE + DP05_0081PE,
    NAME = str_remove_all(str_remove_all(str_remove_all(str_remove_all(str_replace_all(NAME, 
                                                                                       regex("\\W+"), ""),
                                                                       'MicroArea'),
                                                        'MetroArea'),
                                         ','),
                          ' ')
  ) %>% 
  select("Female" = "DP05_0003PE", 
         Minority,
         NAME)

# microdata <- microdata %>% 
#   mutate(soc_code = case_when(OCCSOC == '1110XX' ~ '11-1011',
#                               OCCSOC == '1191XX' ~ '11-9199',
#                               OCCSOC == '37201X' ~ '37-2011',
#                               OCCSOC == '5191XX' ~ '51-9199',
#                               OCCSOC == '4520XX' ~ '45-2099',
#                               OCCSOC == '472XXX' ~ '47-2021',
#                               OCCSOC == '514XXX' ~ '51-4199', 
#                               OCCSOC == '5120XX' ~ '51-2098',
#                               OCCSOC == '2530XX' ~ '25-3099',
#                               OCCSOC == '19303X' ~ '19-3039',
#                               OCCSOC == '3930XX' ~ '39-3099',
#                               OCCSOC == '1940YY' ~ '19-4099',
#                               OCCSOC == '49904X' ~ '49-9045',
#                               OCCSOC == '5371XX' ~ '53-7199',
#                               OCCSOC == '2310XX' ~ '23-1023',
#                               OCCSOC == '4750XX' ~ '47-5099',
#                               OCCSOC == '31909X' ~ '31-9099',
#                               OCCSOC == '27102X' ~ '27-1029',
#                               OCCSOC == '434YYY' ~ '43-4199',
#                               OCCSOC == '439XXX' ~ '43-9199',
#                               OCCSOC == '1721YY' ~ '17-2199',
#                               OCCSOC == '1520XX' ~ '15-2099',
#                               OCCSOC == '17302X' ~ '17-3029',
#                               OCCSOC == '37301X' ~ '37-3019',
#                               OCCSOC == '5170XX' ~ '51-7099',
#                               OCCSOC == '29203X' ~ '29-2033',
#                               OCCSOC == '434XXX' ~ '43-4151',
#                               OCCSOC == '29205X' ~ '29-2051',
#                               OCCSOC == '5140XX' ~ '51-4061',
#                               OCCSOC == '21109X' ~ '21-1099',
#                               OCCSOC == '17301X' ~ '17-3019',
#                               OCCSOC == '1910XX' ~ '19-1099',
#                               OCCSOC == '33909X' ~ '33-9099',
#                               OCCSOC == '4990XX' ~ '49-9099',
#                               OCCSOC == '2590XX' ~ '25-9099',
#                               OCCSOC == '29112X' ~ '29-1129',
#                               OCCSOC == '4740XX' ~ '47-4098',
#                               OCCSOC == '1320XX' ~ '13-2099',
#                               OCCSOC == '31113X' ~ '31-1133',
#                               OCCSOC == '15124X' ~ '15-1243',
#                               OCCSOC == '5370XX' ~ '53-7011',
#                               OCCSOC == '5340XX' ~ '53-4099',
#                               OCCSOC == '5360XX' ~ '53-6099',
#                               OCCSOC == '19204X' ~ '19-2042',
#                               OCCSOC == '4330XX' ~ '43-3099',
#                               OCCSOC == '1940XX' ~ '19-4042',
#                               OCCSOC == '4750YY' ~ '47-5011',
#                               OCCSOC == '3940XX' ~ '39-4011',
#                               OCCSOC == '5350XX' ~ '53-5011',
#                               OCCSOC == '51609X' ~ '51-6099',
#                               OCCSOC == '3330XX' ~ '33-3031',
#                               OCCSOC == '1930XX' ~ '19-3099',
#                               OCCSOC == '1721XX' ~ '17-2171',
#                               OCCSOC == '1720XX' ~ '17-2021',
#                               T ~ paste0(str_sub(OCCSOC, start = 1, end = 2),
#                                          '-',
#                                          str_sub(OCCSOC,
#                                                  start = 3))))
# 
# check <- inner_join(microdata ,
#                    digital_scores) #%>%
#   mutate(DEGFIELD = factor(DEGFIELD, 
#                            levels = c(00,
#                                        11,
#                                        13,
#                                        14,
#                                        15,
#                                        19,
#                                        20,
#                                        21,
#                                        22,
#                                        23,
#                                        24,
#                                        25,
#                                        26,
#                                        29,
#                                        32,
#                                        33,
#                                        34,
#                                        35,
#                                        36,
#                                        37,
#                                        38,
#                                        40,
#                                        41,
#                                        48,
#                                        49,
#                                        50,
#                                        51,
#                                        52,
#                                        53,
#                                        54,
#                                        55,        
#                                        56,
#                                        57,
#                                        58,
#                                        59,
#                                        60,
#                                        61,
#                                        62,
#                                        64),
#                            labels = c(                  "N/A",
#                                  "Agriculture",
#                                  "Environment and Natural Resources",
#                                  "Architecture",
#                                  "Area, Ethnic, and Civilization Studies",
#                                  "Communications",
#                                  "Communication Technologies",
#                                  "Computer and Information Sciences",
#                                  "Cosmetology Services and Culinary Arts",
#                                  "Education Administration and Teaching",
#                                  "Engineering",
#                                  "Engineering Technologies",
#                                  "Linguistics and Foreign Languages",
#                                  "Family and Consumer Sciences",
#                                  "Law",
#                                  "English Language, Literature, and Composition",
#                                  "Liberal Arts and Humanities",
#                                  "Library Science",
#                                  "Biology and Life Sciences",
#                                  "Mathematics and Statistics",
#                                  "Military Technologies",
#                                  "Interdisciplinary and Multi-Disciplinary Studies (General)",
#                                  "Physical Fitness, Parks, Recreation, and Leisure",
#                                  "Philosophy and Religious Studies",
#                                  "Theology and Religious Vocations",
#                                  "Physical Sciences",
#                                  "Nuclear, Industrial Radiology, and Biological Technologies",
#                                  "Psychology",
#                                  "Criminal Justice and Fire Protection",
#                                  "Public Affairs, Policy, and Social Work",
#                                  "Social Sciences",
#                                  "Construction Services",
#                                  "Electrical and Mechanic Repairs and Technologies",
#                                  "Precision Production and Industrial Arts",
#                                  "Transportation Sciences and Technologies",
#                                  "Fine Arts",
#                                  "Medical and Health Sciences and Services",
#                                  "Business",
#                                  "History")),
#          CEGraduates = case_when(DEGFIELD %in% c( "Architecture",
#                                                   "Computer and Information Sciences",
#                                                   "Engineering",
#                                                   "Engineering Technologies",
#                                                   "Mathematics and Statistics",
#                                                   
#                                                   "Nuclear, Industrial Radiology, and Biological Technologies",
#                                                   "Electrical and Mechanic Repairs and Technologies") ~ 1,
#                                  TRUE ~ 0),
#   
#     SEX = factor(SEX,
#                  levels = c(1,
#                             2),
#                  labels = c('Male',
#                             'Female')),
#     RACE = factor(case_when(HISPAN %in% c(1,
#                                           2,
#                                           3,
#                                           4) ~ 10,
#                             RACE %in% c(4,5,6) ~ 4,
#                             RACE %in% c(8,9) ~8,
#                             TRUE ~ RACE
#     ),
#     levels = c(1,2,3,4,7,8,10),
#     labels = c('White',
#                'Black',
#                'American Indian or Alaska Native',
#                'Asian',
#                'Other',
#                'Multiracial',
#                'Hispanic')
#     )
#   ) %>% 
#   group_by(MET2023) %>% 
#   mutate(industry = str_sub(soc_code, end = '3'),
#          industry = factor(industry, 
#                            levels = c("11-",
#                                       "13-",
#                                       "15-",
#                                       "17-",
#                                       "19-",
#                                       "21-",
#                                       "23-",
#                                       "25-",
#                                       "27-",
#                                       "29-",
#                                       "31-",
#                                       "33-",
#                                       "35-",
#                                       "37-",
#                                       "39-",
#                                       "41-",
#                                       "43-",
#                                       "45-",
#                                       "47-",
#                                       "49-",
#                                       "51-",
#                                       "53-"),
#                            labels = c('Management',
#                                       'Business and Financial Operations',
#                                       'Computer and Mathematical',
#                                       'Architecture and Engineering',
#                                       'Life, Physical, and Social Science',
#                                       'Community and Social Service',
#                                       'Legal',
#                                       'Educational Instruction and Library',
#                                       'Arts, Design, Entertainment, Sports, and Media',
#                                       'Healthcare Practitioners and Technical',
#                                       'Healthcare Support',
#                                       'Protective Service',
#                                       'Food Preparation and Serving Related',
#                                       'Building and Grounds Cleaning and Maintenance',
#                                       'Personal Care and Service',
#                                       'Sales and Related',
#                                       'Office and Administrative Support',
#                                       'Farming, Fishing, and Forestry',
#                                       'Construction and Extraction',
#                                       'Installation, Maintenance, and Repair',
#                                       'Production',
#                                       'Transportation and Material Moving')))

pew_data_smartphone$Year = lubridate::mdy(pew_data_smartphone$Year)


pew_data_smartphone['U.S. adults'] <- NA

pew_data <- full_join(pew_data_internet,pew_data_smartphone) %>% 
  rename(Broadband = `U.S. adults`) %>% 
  pivot_longer(-Year,
               names_to = 'Group',
               values_to = 'Percentage') %>% 
  mutate(Percentage = as.numeric(str_remove(Percentage, "%")),
         Year = year(Year)) %>% 
  drop_na() %>% 
  group_by(Year, Group) %>% 
  filter(Percentage == max(Percentage))

metro_data_merged <- metro_data_merged %>% 
  mutate(Metro = case_when(Metro == "CaonCityCO" ~ 'CañonCityCO',
                           Metro == "EspaolaNM" ~ 'EspañolaNM' ,
                           T ~ Metro)) %>% 
  stringdist_inner_join(metro_demographics,
                        by = c(Metro = 'NAME' ),
                        max_dist = .5)

ipeds <- ipeds_data %>% 
  select(c(#"C2022_A.Grand total",
    "C2022_A.Male total" = "C2022_A.Grand total men",
    "C2022_A.Female total" = "C2022_A.Grand total women",
    "C2022_A.American Indian or Alaska Native total" ,        
    "C2022_A.Asian total",       
    "C2022_A.Black total" = "C2022_A.Black or African American total",                
    "C2022_A.Latino or Hispanic total" = "C2022_A.Hispanic or Latino total",                       
    "C2022_A.Native Hawaiian or Other Pacific Islander total",
    "C2022_A.White total",                                    
    'C2022_A.Multiracial total'=  "C2022_A.Two or more races total")) %>% 
  mutate_all(
    ~sum(.)
  ) %>% 
  distinct() %>% 
  mutate(`C2022_A.Indigeneous peoples total` = `C2022_A.American Indian or Alaska Native total`  +
           `C2022_A.Native Hawaiian or Other Pacific Islander total`) %>% 
  select(-c("C2022_A.Native Hawaiian or Other Pacific Islander total",
            "C2022_A.American Indian or Alaska Native total"
  )) %>% 
  pivot_longer(everything(),
               names_to = 'group',
               values_to = 'Population',
               names_prefix = 'C2022_A.',
               names_pattern = "(.*) total") %>% 
  mutate(Group = case_when(group %in% c("Male",
                                        'Female') ~ "Gender",
                           T ~ "Race")) %>% 
  group_by(Group) %>% 
  mutate(Percent = Population/sum(Population)) %>% 
  ungroup() %>% 
  select(-Group)


##########################################
#Data for paper
##########################################  
setwd('V:/Carl/AI/05.24 Inclusion report/1. Graphs')
merged_data %>% 
  group_by(category, CEM) %>% 
  summarise(sum(`2023 Jobs...4`))
##########################################
#Interactive
########################################## 
interactive_data <- metro_data_merged %>% 
  group_by(Metro) %>% 
  mutate(`Total jobs` = sum(`2023 Jobs...8`),
         `Total jobs 2010` = sum(`2010 Jobs...7`),
         `Total jobs percent change 2010-2023` = (`Total jobs` -`Total jobs 2010`)/`Total jobs 2010`,
         `Average wage, all jobs` = weighted.mean(`Avg. Annual Earnings`,
                                                  `2023 Jobs...8`)) %>% 
  filter(CEM == 'CEM',
         category == 'High') %>% 
  mutate(`CEM jobs` = sum(`2023 Jobs...8`),
         `CEM job share` = `CEM jobs`/`Total jobs`,
         `CEM jobs 2010` = sum(`2010 Jobs...7`),
         `CEM jobs percent change 2010-2023` = (`CEM jobs`-`CEM jobs 2010`)/`CEM jobs 2010`,
         `CEM male jobs` = sum(`Current Year Males`),
         `CEM female jobs` = sum(`Current Year Females`),
         `CEM white jobs` = sum(`Current Year White`),
         `CEM black jobs` = sum(`Current Year Black or African American`),
         `CEM asian jobs` = sum(`Current Year Asian`),
         `CEM hispanic jobs` = sum(`Current Year Hispanic or Latino`),
         `CEM indigenous jobs` = sum(`Current Year American Indian or Alaska Native`) + 
           sum(`Current Year Native Hawaiian or Other Pacific Islander`),
         `CEM multiracial jobs` = sum(`Current Year Two or More Races`),
         `CEM percent` = `CEM jobs`/`Total jobs`,
         `CEM average wage` = weighted.mean(`Avg. Annual Earnings`,
                                            `2023 Jobs...8`))  %>% 
  select(
    Metro,
    `Total jobs`,
    `Total jobs 2010`,
    `Total jobs percent change 2010-2023` ,
    `Average wage, all jobs`,
    `CEM jobs` ,
    `CEM job share`,
    `CEM jobs 2010` ,
    `CEM jobs percent change 2010-2023`,
    `CEM male jobs`,
    `CEM female jobs`,
    `CEM white jobs` ,
    `CEM black jobs`,
    `CEM asian jobs` ,
    `CEM hispanic jobs` ,
    `CEM indigenous jobs` ,
    `CEM multiracial jobs` ,
    `CEM percent` ,
    `CEM average wage`
  ) %>% 
  distinct() %>% 
  ungroup() %>% 
  arrange(desc(`CEM job share`)) %>% 
  stringdist_inner_join(metro_demographics2022 %>% mutate(Metro =str_replace_all(str_replace_all(str_replace_all(NAME, "[[:punct:]]", "")," ", ""), "MetroArea", "")) ,
                        max_dist = .5) %>% 
  mutate(
    'Indigenous pop' =           DP05_0081E   +      DP05_0083E,
    'Male share' = DP05_0002E / DP05_0001E,
    'Female share' = DP05_0003E / DP05_0001E,
    'White share' = DP05_0079E / DP05_0001E,
    'Black share' = DP05_0080E / DP05_0001E,
    'Indigenous share' = `Indigenous pop` / DP05_0001E,
    'Hispanic share' = DP05_0073E / DP05_0001E,
    'Asian share' = DP05_0082E / DP05_0001E,
    'Multiracial share' = DP05_0085E / DP05_0001E
    
  ) %>% 
  select(Metro = 'Metro.x',
         Population = 'DP05_0001E',
         'Male pop' ='DP05_0002E',
         'Male share',
         'Female pop' = 'DP05_0003E',
         'Female share',
         
         'White pop' =  'DP05_0079E',
         'White share',
         
         'Black pop' = 'DP05_0080E',
         'Black share',
         
         'Indigenous pop',
         'Indigenous share',
         
         'Asian pop' =  "DP05_0082E",
         'Asian share',
         
         'Multiracial pop' = 'DP05_0085E',
         'Multiracial share',
         
         'Hispanic pop' =  'DP05_0073E',
         'Hispanic share',
         
         `Total jobs`,
         `Total jobs 2010`,
         `Total jobs percent change 2010-2023` ,
         `Average wage, all jobs`,
         `CEM jobs` ,
         `CEM job share`,
         `CEM jobs 2010` ,
         `CEM jobs percent change 2010-2023`,
         `CEM male jobs`,
         `CEM female jobs`,
         `CEM white jobs` ,
         `CEM black jobs`,
         `CEM asian jobs` ,
         `CEM hispanic jobs` ,
         `CEM indigenous jobs` ,
         `CEM multiracial jobs` ,
         `CEM percent` ,
         `CEM average wage`) %>% 
  top_n(Population, 
        n = 100) %>% 
  mutate(rank = row_number())


write_csv(interactive_data,
          'interactive_data.csv')

##########################################
#Top N metros csv
##########################################
#All Jobs
top100 <- metro_data_merged %>% 
  select(Metro, 
         jobs2023_total) %>% 
  distinct()

for(i in c(100, 50, 30, 20, 10, 5)){
  print(i)
  print(top100 %>%
          slice_max(jobs2023_total,
                    n = i) %>% 
          summarise(sum(jobs2023_total)) /
          merged_data %>% 
          summarise(sum(`2023 Jobs...4`)))
}
#CEM
top100 <- metro_data_merged %>% 
  filter(CEM == 'CEM',
         category == 'High') %>%
  select(Metro, 
         jobs2023_total, 
         Jobs2023_CEM_digitalization) %>% 
  distinct()

for(i in c(100, 50, 30, 20, 10, 5)){
  print(i)
  print(top100 %>%
          slice_max(jobs2023_total,
                    n = i) %>% 
          summarise(sum(Jobs2023_CEM_digitalization)) /
          merged_data %>% 
          filter(CEM == 'CEM',
                 category == 'High') %>% 
          summarise(sum(`2023 Jobs...4`)))
}


jobs = tibble(
  `Top N` = c('All', '100', '50', '30', '20', '10', '5'
  ),
  `CEM jobs` = c(as.numeric(merged_data %>% 
                              filter(CEM == 'CEM',
                                     category == 'High') %>% 
                              summarise(sum(`2023 Jobs...4`))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 100) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 50) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 30) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 20) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 10) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization))),
                 as.numeric(top100 %>%
                              slice_max(jobs2023_total,
                                        n = 5) %>% 
                              summarise(sum(Jobs2023_CEM_digitalization)))
  ))

write_csv(jobs, 'Top N metro CEM jobs.csv')

##########################################
#Job numbers csv
##########################################


paper_data <- tibble(
  group = c(
    "National average",#1 
    "Non-HDCEM",
    "non-CEM High",
    "non-CEM Medium",
    "Non-CEM Low",
    "CEM High",
    "CEM Medium",
    "Management",
    "Computer and Mathematical",
    "Architecture and Engineering"
  ),
  jobs2023 = c(
    sum(merged_data$`2023 Jobs...4`), #national average
    as.numeric(merged_data %>%
                 filter(CEM != 'CEM'|
                          category != 'High') %>% 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'High') %>% 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Medium') %>% 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Low') %>% 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'High') %>% 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'Medium') %>%
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(industry == "Management", CEM == 'CEM',  category == 'High') %>% 
                 
                 summarise(sum(`2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(industry == 'Computer and Mathematical', CEM == 'CEM',  category == 'High') %>% 
                 summarise(sum( `2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(CEM == 'CEM',  category == 'High', industry == 'Architecture and Engineering') %>% 
                 summarise(sum( `2023 Jobs...4`)))
  ),
  jobs2020 = c(
    sum(merged_data$`2020 Jobs`), #national average
    as.numeric(merged_data %>%
                 filter(CEM != 'CEM'|
                          category != 'High') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'High') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Medium') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Low') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'High') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'Medium') %>%
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(industry == "Management", CEM == 'CEM',  category == 'High') %>% 
                 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(industry == 'Computer and Mathematical', CEM == 'CEM',  category == 'High') %>% 
                 summarise(sum(`2020 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(CEM == 'CEM',  category == 'High', industry == 'Architecture and Engineering') %>% 
                 summarise(sum(`2020 Jobs`)))
  ),
  jobs2019 = c(
    sum(merged_data$`2019 Jobs`), #national average
    as.numeric(merged_data %>%
                 filter(CEM != 'CEM'|
                          category != 'High') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'High') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Medium') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Low') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'High') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'Medium') %>%
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(industry == "Management", CEM == 'CEM',  category == 'High') %>% 
                 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(industry == 'Computer and Mathematical', CEM == 'CEM',  category == 'High') %>% 
                 summarise(sum(`2019 Jobs`))),
    as.numeric(merged_data %>% 
                 filter(CEM == 'CEM',  category == 'High', industry == 'Architecture and Engineering') %>% 
                 summarise(sum(`2019 Jobs`)))
  ),
  jobs2010 = c(
    sum(merged_data$`2010 Jobs...3`), #national average
    as.numeric(merged_data %>%
                 filter(CEM != 'CEM'|
                          category != 'High') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'High') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Medium') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Low') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'High') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'Medium') %>%
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>% 
                 filter(industry == "Management", CEM == 'CEM',  category == 'High') %>% 
                 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>% 
                 filter(industry == 'Computer and Mathematical', CEM == 'CEM',  category == 'High') %>% 
                 summarise(sum(`2010 Jobs...3`))),
    as.numeric(merged_data %>% 
                 filter(CEM == 'CEM',  category == 'High', industry == 'Architecture and Engineering') %>% 
                 summarise(sum(`2010 Jobs...3`)))
  ),
  Wages2023 = c(
    weighted.mean(merged_data$`Avg. Annual Earnings`, merged_data$`2023 Jobs...4`), #national average
    as.numeric(merged_data %>%
                 filter(CEM != 'CEM'|
                          category != 'High') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'High') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Medium') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'non-CEM',
                        category == 'Low') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'High') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>%
                 filter(CEM == 'CEM',
                        category == 'Medium') %>%
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(industry == "Management", CEM == 'CEM',  category == 'High') %>% 
                 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(industry == 'Computer and Mathematical', CEM == 'CEM',  category == 'High') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`))),
    as.numeric(merged_data %>% 
                 filter(CEM == 'CEM',  category == 'High', industry == 'Architecture and Engineering') %>% 
                 summarise(weighted.mean(`Avg. Annual Earnings`, `2023 Jobs...4`)))
  ))

paper_data <- paper_data %>% 
  mutate(`PG2020-2023` = (jobs2023-jobs2020)/jobs2020,
         `PG2019-2020` = (jobs2020-jobs2019)/jobs2019,
         `PG2019-2023` = (jobs2023-jobs2019)/jobs2019,
         `PG2010-2023` = (jobs2023-jobs2010)/jobs2010
  )

write_csv(paper_data, 'change in jobs and wages.csv')

##########################################
#Number of HD jobs 
########################################## 
hd_jobs <- merged_data %>% 
  select(category,Jobs2023_digitalization) %>% 
  distinct()
write_csv(hd_jobs,'jobs_by_digitalization.csv')
##########################################
#Number of computer user support specialist, computer network support specialist, property real estate and community association managers, electrical and electronic engineering technicians, and electrical electronics drafters
########################################## 

number_of_specific_jobs <- merged_data %>% 
  filter(Title %in% c('Computer User Support Specialists', 
                      'Computer Network Support Specialists', 
                      'Property, Real Estate, and Community Association Managers',
                      'Electrical and Electronic Engineering Technologists and Technicians', 
                      'Electrical and Electronics Drafters')) %>% 
  select(Title, `2023 Jobs...4`)
write_csv(number_of_specific_jobs,'specific jobs in text.csv')

##########################################
#Number of CEM jobs no 4 year
########################################## 
cem_jobs <- merged_data %>% 
  filter(CEM == 'CEM',
         category == 'High') %>% 
  select(`Title`, `2023 Jobs...4`, `Avg. Annual Earnings`)

cem_jobs_nodegree <- merged_data %>% 
  filter(bachelors == 0,
         CEM == 'CEM',
         category == 'High') %>% 
  select(`Title`, `2023 Jobs...4`, `Avg. Annual Earnings`)

sum(cem_jobs_nodegree$`2023 Jobs...4`)/sum(cem_jobs$`2023 Jobs...4`)
sum(cem_jobs_nodegree$`2023 Jobs...4`)

sum(cem_jobs$`2023 Jobs...4`)

##########################################
#Land grant colleges
##########################################
lgc <- read_csv('V:/Carl/AI/03.24 Inclusive Digital Graph reproduction for Rob/IPEDS data/data findings.csv')
write_csv(lgc,
          'land grant data.csv')
##########################################
#Average wage of highly digital non CEM occupations
########################################## 

scales::dollar(as.numeric(merged_data %>%
                            filter(CEM == 'non-CEM', 
                                   category == 'High') %>% 
                            summarise(weighted.mean(`Avg. Annual Earnings`,
                                                    w = `2023 Jobs...4`))))

##########################################
#Growth rate of MGMT jobs vs economy as a whole 2010-2023
########################################## 
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Management') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Management') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`)) /
  merged_data %>% 
  summarise((sum(`2023 Jobs...4`) -sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))

##########################################
#Growth rate of computer jobs vs economy as a whole 2010-2023
########################################## 
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Computer and Mathematical') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Computer and Mathematical') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`)) /
  merged_data %>% 
  summarise((sum(`2023 Jobs...4`) -sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))

##########################################
#Growth rate of architecture/engineering jobs vs economy as a whole 2010-2023
########################################## 
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Architecture and Engineering') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))
merged_data %>% filter(CEM == 'CEM',
                       category == 'High',
                       industry == 'Architecture and Engineering') %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`)) /
  merged_data %>% 
  summarise((sum(`2023 Jobs...4`) -sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))

##########################################
#Growth rate of economy as a whole 2010-2023
########################################## 
merged_data %>%
  summarise((sum(`2023 Jobs...4`)-sum(`2010 Jobs...3`))/sum(`2010 Jobs...3`))

##########################################
#Growth rate of CEM jobs vs economy as a whole 2019-2023
########################################## 
merged_data %>% filter(CEM == 'CEM'&
                         category == 'High') %>% 
  summarise((sum(`2023 Jobs...4`)-sum(`2019 Jobs`))/sum(`2019 Jobs`))
merged_data %>% filter(CEM != 'CEM' &
                         category != 'High') %>% 
  summarise((sum(`2023 Jobs...4`)-sum(`2019 Jobs`))/sum(`2019 Jobs`))

##########################################
#share of women and POC in roles 2023
########################################## 
merged_data %>% 
  filter(CEM== 'CEM',
         category == 'High') %>% 
  summarise(Female = sum(`Current Year Females`)/sum(`2023 Jobs...4`),
            
            Underrep = (sum(`Current Year American Indian or Alaska Native`) +
                          sum(`Current Year Hispanic or Latino`) +
                          sum(`Current Year Black or African American`) +
                          sum(`Current Year Native Hawaiian or Other Pacific Islander`))/sum(`2023 Jobs...4`)
  )

merged_data %>% 
  filter(industry %in% c("Healthcare Support",
                         'Community and Social Services'),
         category == 'High') %>% 
  summarise(Female = sum(`Current Year Females`)/sum(`2023 Jobs...4`),
            
            Underrep = (sum(`Current Year American Indian or Alaska Native`) +
                          sum(`Current Year Hispanic or Latino`) +
                          sum(`Current Year Black or African American`) +
                          sum(`Current Year Native Hawaiian or Other Pacific Islander`))/sum(`2023 Jobs...4`)
  )
##########################################
#Metros
########################################## 
nation_current_CEM_percent <- as.numeric(merged_data %>% #Get national average of percentage of jobs HDCEM
                                           mutate(all_jobs = sum(`2023 Jobs...4`)) %>% 
                                           filter(CEM == 'CEM',
                                                  category == 'High') %>%
                                           mutate(HDCEM = sum(`2023 Jobs...4`)) %>% 
                                           transmute(one = HDCEM/all_jobs) %>% 
                                           distinct())
metros <- as.data.frame(matrix(0, ncol = 14, nrow = 0))


colnames(metros) <- c('Metro',
                      'National CEM Percent',
                      'Metro total number of jobs',
                      'Metro CEM jobs',
                      'Metro CEM Percent',
                      'Metro NEW CEM jobs',
                      
                      'Metro Female population percent',
                      'Metro Female CEM jobs',
                      'Metro Female CEM percent',
                      'Metro NEW Female CEM jobs',
                      # 'Metro EQUITY Female CEM jobs',
                      
                      'Metro Minority population percent',
                      'Metro Minority CEM jobs',
                      'Metro Minority CEM percent',
                      'Metro NEW Minority CEM jobs'#,
                      # 'Metro EQUITY Minority CEM jobs'
)



for(i in unique(metro_data_merged$Metro)){
  metro <- metro_data_merged %>%  #metro only data
    filter(Metro==i) %>% 
    mutate()
  
  metro_total_current_jobs <- sum(metro$`2023 Jobs...8`) #Get metro total jobs 
  
  metro_current_CEM <- as.numeric(metro %>% #Get metro average of percentage of jobs HDCEM
                                    mutate(all_jobs = sum(`2023 Jobs...8`)) %>% 
                                    filter(CEM == 'CEM',
                                           category == 'High') %>%
                                    transmute(HDCEM = sum(`2023 Jobs...8`)) %>% 
                                    distinct())
  
  metro_current_CEM_percent <- metro_current_CEM/metro_total_current_jobs  #Percent of total jobs that are CEM
  
  
  metro_CEM_new_jobs <- (metro_current_CEM - metro_total_current_jobs*nation_current_CEM_percent)/(nation_current_CEM_percent-1)  #get new jobs created by bringing up CEM jobs to average
  
  
  
  metro_CEM_current_female <- as.numeric(metro %>% #Get number of women CEM jobs in metro 
                                           filter(CEM == 'CEM',
                                                  category == 'High') %>%
                                           transmute(HDCEM = sum(`Current Year Females`)) %>% 
                                           distinct())
  
  metro_CEM_current_female_percent <- metro_CEM_current_female/metro_current_CEM #get percent CEM jobs in metro held by women
  
  metro_CEM_new_female <- metro_CEM_current_female_percent * metro_CEM_new_jobs
  
  metro_population_female <- as.numeric(metro %>%  #get percent of popoulation female
                                          select(Female) %>%
                                          distinct() )/100 
  
  # metro_CEM_equity_female <- (metro_CEM_current_female - 
  #                               metro_current_CEM*
  #                               metro_population_female )/
  #     (metro_population_female-
  #        1)
  
  metro_CEM_current_POC <- as.numeric(metro %>% #get number of POC CEM jobs in metro 
                                        filter(CEM == 'CEM',
                                               category == 'High') %>%
                                        transmute(HDCEM = sum(`Current Year Black or African American`) +
                                                    sum(`Current Year Hispanic or Latino`) +
                                                    sum(`Current Year Native Hawaiian or Other Pacific Islander`) +
                                                    sum(`Current Year American Indian or Alaska Native`)) %>% 
                                        distinct())
  
  metro_CEM_current_POC_percent <- metro_CEM_current_POC/metro_current_CEM #get percent CEM jobs in memphis held by women
  metro_CEM_new_POC <- metro_CEM_current_POC_percent * metro_CEM_new_jobs
  
  metro_population_POC <- as.numeric(metro %>%
                                       select(Minority) %>%
                                       distinct() )/100
  
  # metro_CEM_equity_POC <- (metro_CEM_current_POC - 
  #                            metro_current_CEM*
  #                            metro_population_POC )/
  #     (metro_population_POC-
  #        1)
  
  vector <- c(i,
              nation_current_CEM_percent,
              metro_total_current_jobs,
              metro_current_CEM,
              metro_current_CEM_percent,
              metro_CEM_new_jobs,
              metro_population_female,
              metro_CEM_current_female,
              metro_CEM_current_female_percent,
              metro_CEM_new_female,
              #metro_CEM_equity_female,
              
              metro_population_POC,
              metro_CEM_current_POC,
              metro_CEM_current_POC_percent,
              metro_CEM_new_POC#,
              #metro_CEM_equity_POC
  )
  
  metros[nrow(metros)+1,] <- vector
}

write_csv(metros,
          'Metros.csv')

rm(i,
   nation_current_CEM_percent,
   metro_total_current_jobs,
   metro_current_CEM,
   metro_current_CEM_percent,
   metro_CEM_new_jobs,
   metro_population_female,
   metro_CEM_current_female,
   metro_CEM_current_female_percent,
   metro_CEM_new_female,
   #metro_CEM_equity_female,
   
   metro_population_POC,
   metro_CEM_current_POC,
   metro_CEM_current_POC_percent,
   metro_CEM_new_POC#,
   #metro_CEM_equity_POC
)


##########################################
#Graphs
##########################################

##########################################
#Share of all highly digital jobs contained in each occupation group
##########################################

soahdjcieog <- merged_data %>% 
  group_by(industry) %>% 
  transmute(jobs = sum(`2023 Jobs...4`)) %>% 
  distinct()

soahdjcieog_hd <- merged_data %>% 
  filter(category =='High') %>% 
  group_by(industry) %>% 
  transmute(HDjobs = sum(`2023 Jobs...4`)) %>% 
  distinct()

soahdjcieog <- left_join(soahdjcieog, soahdjcieog_hd) %>% 
  mutate(
    share = HDjobs / sum(soahdjcieog_hd$HDjobs)
  )

write_csv(soahdjcieog, 
          'Share of all highly digital jobs contained in each occupation group.csv')

##########################################
#Global penetration of broadband, cellular phones, and smartphones
##########################################
gpobcpas <- pew_data 

write_csv(gpobcpas,
          'Global penetration of broadband, cellular phones, and smartphones.csv')
gpobcpas <- gpobcpas%>% 
  ggplot()+
  geom_point(aes(x = Year,
                 y = Percentage,
                 color = Group)
  )+
  geom_line(aes(x = Year,
                y = Percentage,
                color = Group),
            size = 1.25
  )+ 
  scale_color_manual(values = c("#EF6A00",
                                "#B1B3B3",
                                "#0061A0"))+
  labs(
    x = "",
    y = "Percent of households",
    caption = "Source: Brookings analysis of data from Pew Research Center"
  )+
  theme_bw() 

ggsave(plot =  gpobcpas, 
       filename = "Global penetration of broadband, cellular phones, and smartphones.pdf", 
       width = 10, 
       height = 6, 
       device = "pdf")

ggsave(plot = gpobcpas, 
       filename = "Global penetration of broadband, cellular phones, and smartphones.jpg", 
       width = 10, 
       height = 6, 
       device = "jpg")

##########################################
#Highly digital employment growth at a glance, 2002 - 2022
##########################################
HDEGAAG <- merged_data %>% 
  select(category, 
         Jobs2023_digitalization) %>% 
  distinct() %>% 
  mutate_at(c(
    'Jobs2023_digitalization'),
    ~./sum(.)) %>% 
  select(category,
         
         'Jobs2023_digitalization'
  ) %>% 
  rename('2023' = Jobs2023_digitalization) %>% 
  left_join( .,
             historical_digitalization %>%
               rename('category'='Levels of Job Digitalization')) %>% 
  mutate_at(c('2002',
              '2010'),
            ~as.numeric(str_remove(.,
                                   '%'))/100) %>% 
  pivot_longer(-category)


write_csv(HDEGAAG,
          'Highly digital employment growth at a glance, 2002 - 2023.csv')

HDEGAAG <- HDEGAAG%>% 
  ggplot()+
  geom_bar(aes(x = factor(name,
                          levels = c('2002',
                                     '2010',
                                     '2023')),
               y = value,
               fill = factor(category,
                             levels = c('High',
                                        'Medium',
                                        'Low'))),
           stat = 'identity',
           position= 'stack')+
  geom_text(aes(x = factor(name,
                           levels = c('2002',
                                      '2010',
                                      '2023')),
                y = value - .01,
                label = scales::percent(value, accuracy = 0.1),
                fill = factor(category,
                              levels = c('High',
                                         'Medium',
                                         'Low'))),
            stat = 'identity',
            color = 'white',
            position= 'stack')+
  scale_fill_manual(
    values = c(  '#023147',
                 "#287EC9",
                 '#CAE1FA')
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  #coord_flip()+
  labs(
    title = "Highly digital employment growth at a glance, 2002 - 2023",
    x = "",
    fill = '',
    y = "",
    caption = "Note: Occupational groups that have no highly digital workers are excluded.\nSource: Brookings analysis of Lightcast data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank()
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Highly digital employment growth at a glance, 2002 - 2023.pdf",
       HDEGAAG, width = 9, height = 6)
ggsave("Highly digital employment growth at a glance, 2002 - 2023.jpg", 
       HDEGAAG, width = 9, height = 6) 
##########################################
#Breakdown of highly digital occupations, 2022
##########################################
BOHDO <- merged_data %>% 
  filter(category == "High") %>% 
  select(industry,
         Jobs2023_industry_digitalization) %>% 
  distinct() %>% 
  mutate(Jobs2023_industry_digitalization = Jobs2023_industry_digitalization/sum(Jobs2023_industry_digitalization)) %>% 
  arrange(Jobs2023_industry_digitalization, desc = TRUE)
write_csv(BOHDO,
          'Breakdown of highly digital occupations, 2023.csv')
##########################################
#Share of highly digital employment by occupational group, 2022
##########################################
SOHDEBOG <-merged_data %>% 
  select(category, 
         industry,
         CEM,
         Jobs2023_industry_digitalization) %>% 
  distinct() %>% 
  group_by(industry) %>% 
  mutate(Jobs2023_industry_digitalization = Jobs2023_industry_digitalization/sum(Jobs2023_industry_digitalization)) %>% 
  filter(category == "High") %>%
  arrange(Jobs2023_industry_digitalization) %>% 
  mutate(overall = as.numeric(merged_data %>% 
                                mutate(case = case_when(category =='High' ~ 1,
                                                        T ~ 0)) %>% 
                                summarise(weighted.mean(case, `2023 Jobs...4`))))
write_csv(SOHDEBOG,
          'Share of highly digital employment by occupational group, 2023.csv')

SOHDEBOG <- SOHDEBOG%>% 
  ggplot()+
  geom_bar(aes(x = reorder(industry,
                           Jobs2023_industry_digitalization),
               y = Jobs2023_industry_digitalization,
               fill = CEM),
           stat = 'identity',
           position= 'dodge')+
  scale_fill_manual(
    values = c(  'grey',
                 "#00649f")
  )+
  geom_hline(aes(yintercept = overall))+
  geom_text(aes(x = 8, y = overall + .25),
            label = "Overall level of highly digital workers")+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Share of highly digital employment by occupational group, 2023",
    x = "",
    y = "",
    caption = "Note: Occupational groups that have no highly digital workers are excluded.\nSource: Brookings analysis of Lightcast data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(legend.position = 'none',
        #axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.ticks.x=element_blank()
        #axis.title.y=element_blank(),
        # axis.ticks.x=element_blank(),
        #axis.text.x = element_text(angle = 70, vjust = .95,
        # hjust = .95)
  )
ggsave("Share of highly digital employment by occupational group, 2023.pdf",
       SOHDEBOG, width = 9, height = 6)
ggsave("Share of highly digital employment by occupational group, 2023.jpg", 
       SOHDEBOG, width = 9, height = 6)

##########################################
#Average wage of highly digital occupations in each occupational group
##########################################
AWOHDOIEOG <- merged_data %>%
  mutate(average_wage_all = weighted.mean(`Avg. Annual Earnings`,
                                          w = `2023 Jobs...4`)) %>% 
  filter(category == 'High') %>% 
  mutate(average_wage_hd = weighted.mean(`Avg. Annual Earnings`,
                                         w = `2023 Jobs...4`)) %>% 
  group_by(
    CEM
  ) %>% 
  mutate(average_wage_CEM = weighted.mean(`Avg. Annual Earnings`,
                                          w = `2023 Jobs...4`)) %>% 
  select(industry,
         CEM,
         average_wage_all,
         average_wage_hd,
         average_wage_CEM,
         Wages2023_industry_digitalization) %>% 
  distinct() 
write_csv(AWOHDOIEOG,
          'Average wage of highly digital occupations in each occupational group, 2022.csv')

AWOHDOIEOG<-AWOHDOIEOG%>% 
  ggplot()+
  geom_bar(aes(x = reorder(industry,
                           desc(Wages2023_industry_digitalization)),
               y = Wages2023_industry_digitalization,
               fill = CEM),
           stat = "identity", 
           position = "dodge"
  )+
  scale_fill_manual(
    values = c(  'grey',
                 "#00649f")
  )+
  geom_hline(aes(yintercept = average_wage_all),
             size = 2,
             color = "#FF9E1B")+
  geom_text(aes(x = 15, 
                y = average_wage_all + 7500),
            label = "Average wage, all jobs")+
  geom_text(aes(x = 15, 
                y = average_wage_hd + 7500),
            label = "Average wage, highly digital jobs")+
  geom_hline(aes(yintercept = average_wage_hd), 
             size = 2,
             color = "#FCD200")+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(40000,
                                60000,
                                80000,
                                100000,
                                120000,
                                140000,
                                160000),
                     limits = c(0, 160000),
                     expand = c(0,0))+
  labs(
    title = "Average wage of highly digital occupations in each occupational group, 2023",
    x = "",
    y = "Average Wage",
    caption = "Note: Occupational groups that have no highly digital workers are excluded.\nSource: Brookings analysis of BLS and Lightast data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )

ggsave("Average wage of highly digital occupations in each occupational group, 2023.pdf",
       AWOHDOIEOG, width = 9, height = 6)
ggsave("Average wage of highly digital occupations in each occupational group, 2023.jpg", 
       AWOHDOIEOG, width = 9, height = 6)

##########################################
#Average wage by digitalization score in each occupational group
##########################################
AWBDS<- merged_data %>% 
  select(Score,
         CEM,
         `Avg. Annual Earnings`,
         `2023 Jobs...4`)

write_csv(AWBDS, 
          'Average wage by digitalization score in each occupational group.csv')
AWBDS <- AWBDS %>% 
  ggplot() +
  geom_point(
    aes(x = Score,
        y = `Avg. Annual Earnings`,
        color = CEM,
        size = `2023 Jobs...4`)
  )+
  scale_color_manual(values =c('grey',
                               '#00649f'))+
  scale_x_continuous(breaks = c(0,20,40,60,80,100),
                     limits = c(0,100))+
  scale_size_continuous(range = c(.5, 10),
                        labels = scales::label_comma())+
  scale_y_continuous(trans = "log",
                     breaks = c(20000,25000,50000,
                                100000,200000,350000),
                     limits = c(20000, 375000),
                     expand = c(0,0),
                     labels = c("$0", '$25,000','$50,000',
                                '$100,000','$200,000','$350,000'))+
  labs(
    title = "Average wage by digitalization score in each occupational group, 2023",
    x = "Digitalization Score",
    y = "Average Wage",
    color = '',
    size = 'Number of jobs',
    caption = "Source: Brookings analysis of BLS and Lightast data"
  )+
  theme_classic()+
  theme(
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.ticks.y=element_blank()
  )
ggsave("Average wage by digitalization score in each occupational group, 2023.pdf",
       AWBDS, width = 9, height = 6)
ggsave("Average wage by digitalization score in each occupational group, 2023.jpg", 
       AWBDS, width = 9, height = 6)

##########################################
#Change of highly digital employment by occupation group 2010-2023
##########################################


cohdebog <- merged_data %>% 
  group_by(category) %>% 
  mutate_at(c('Jobs2011_digitalization',
              'Jobs2012_digitalization',
              'Jobs2013_digitalization',
              'Jobs2014_digitalization',
              'Jobs2015_digitalization',
              'Jobs2016_digitalization',
              'Jobs2017_digitalization',
              'Jobs2018_digitalization',
              'Jobs2019_digitalization',
              'Jobs2020_digitalization',
              'Jobs2021_digitalization',
              'Jobs2022_digitalization',
              'Jobs2023_digitalization'),
            ~(.-Jobs2010_digitalization)/Jobs2010_digitalization
  ) %>% 
  group_by(CEM, category) %>% 
  mutate_at(c('Jobs2011_CEM_digitalization',
              'Jobs2012_CEM_digitalization',
              'Jobs2013_CEM_digitalization',
              'Jobs2014_CEM_digitalization',
              'Jobs2015_CEM_digitalization',
              'Jobs2016_CEM_digitalization',
              'Jobs2017_CEM_digitalization',
              'Jobs2018_CEM_digitalization',
              'Jobs2019_CEM_digitalization',
              'Jobs2020_CEM_digitalization',
              'Jobs2021_CEM_digitalization',
              'Jobs2022_CEM_digitalization',
              'Jobs2023_CEM_digitalization'),
            ~(.-Jobs2010_CEM_digitalization)/Jobs2010_CEM_digitalization) %>% 
  group_by(industry,
           category) %>% 
  mutate_at(c('Jobs2011_industry_digitalization',
              'Jobs2012_industry_digitalization',
              'Jobs2013_industry_digitalization',
              'Jobs2014_industry_digitalization',
              'Jobs2015_industry_digitalization',
              'Jobs2016_industry_digitalization',
              'Jobs2017_industry_digitalization',
              'Jobs2018_industry_digitalization',
              'Jobs2019_industry_digitalization',
              'Jobs2020_industry_digitalization',
              'Jobs2021_industry_digitalization',
              'Jobs2022_industry_digitalization',
              'Jobs2023_industry_digitalization'),
            ~(.-Jobs2010_industry_digitalization)/Jobs2010_industry_digitalization
  ) %>% 
  mutate(
    Jobs2010_industry_digitalization = 0 ,
    Jobs2010_CEM_digitalization = 0,
    Jobs2010_digitalization = 0
  ) %>% 
  select(category,
         industry,
         CEM,
         Jobs2010_industry_digitalization,
         'Jobs2011_industry_digitalization',
         'Jobs2012_industry_digitalization',
         'Jobs2013_industry_digitalization',
         'Jobs2014_industry_digitalization',
         'Jobs2015_industry_digitalization',
         'Jobs2016_industry_digitalization',
         'Jobs2017_industry_digitalization',
         'Jobs2018_industry_digitalization',
         'Jobs2019_industry_digitalization',
         'Jobs2020_industry_digitalization',
         'Jobs2021_industry_digitalization',
         'Jobs2022_industry_digitalization',
         'Jobs2023_industry_digitalization',
         Jobs2010_CEM_digitalization,
         'Jobs2011_CEM_digitalization',
         'Jobs2012_CEM_digitalization',
         'Jobs2013_CEM_digitalization',
         'Jobs2014_CEM_digitalization',
         'Jobs2015_CEM_digitalization',
         'Jobs2016_CEM_digitalization',
         'Jobs2017_CEM_digitalization',
         'Jobs2018_CEM_digitalization',
         'Jobs2019_CEM_digitalization',
         'Jobs2020_CEM_digitalization',
         'Jobs2021_CEM_digitalization',
         'Jobs2022_CEM_digitalization',
         'Jobs2023_CEM_digitalization',
         Jobs2010_digitalization,
         'Jobs2011_digitalization',
         'Jobs2012_digitalization',
         'Jobs2013_digitalization',
         'Jobs2014_digitalization',
         'Jobs2015_digitalization',
         'Jobs2016_digitalization',
         'Jobs2017_digitalization',
         'Jobs2018_digitalization',
         'Jobs2019_digitalization',
         'Jobs2020_digitalization',
         'Jobs2021_digitalization',
         'Jobs2022_digitalization',
         'Jobs2023_digitalization',
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  pivot_longer(-c(category,
                  industry,
                  CEM),
               names_to = c('Year', '.value'),
               names_pattern = 'Jobs(\\d+)_(.*)') %>% 
  filter(category == 'High' & CEM == 'CEM'|
           category == 'High' & industry == 'Life, Physical, and Social Science'|
           category == 'Low' & industry == 'Farming, Fishing, and Forestry' |
           category == 'Medium' & industry == 'Legal'
  ) %>% 
  mutate(CEM_digitalization = case_when(category != 'High' ~ NA,
                                        T ~ CEM_digitalization),
         industry_digitalization = case_when(category != 'High' ~ NA,
                                             CEM == 'non-CEM' ~ NA, 
                                             T ~ industry_digitalization),
         digitalization = case_when(category == 'High' ~ NA,
                                    T ~ digitalization),
         industry = case_when(industry == 'Life, Physical, and Social Science'|
                                industry == 'Farming, Fishing, and Forestry' |
                                industry == 'Legal' ~ NA,
                              T ~ industry)) %>% 
  distinct() 

write_csv(cohdebog,
          'Change of highly digital employment by occupational group, 2010-2023.csv')


cohdebog <- 
  ggplot()+
  
  geom_line(aes(x = as.numeric(Year),
                y= as.numeric(CEM_digitalization),
                color = as.character(CEM)),
            linewidth = 2,
            data = droplevels(cohdebog) %>% 
              filter(!is.na(CEM_digitalization)))+
  geom_line(aes(x = as.numeric(Year),
                y= as.numeric(industry_digitalization),
                color = as.character(industry)),
            linewidth = 1,
            data = droplevels(cohdebog) %>% 
              filter(!is.na(industry_digitalization)))+
  geom_line(aes(x = as.numeric(Year),
                y= as.numeric(digitalization),
                color = as.character(category)),
            linewidth = 1,
            data = droplevels(cohdebog) %>% 
              filter(!is.na(digitalization)))+
  scale_color_manual(limits = c('CEM',
                                'non-CEM',
                                'Management',
                                'Architecture and Engineering',
                                'Computer and Mathematical',
                                'Medium',
                                'Low'),
                     values = c('#004B6E',
                                '#33660F',
                                '#E0BB00',
                                '#FF851A',
                                '#F75C57',
                                '#EC81B7',
                                '#D9BEF5'))+
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1),
                     expand = c(0,0))+
  scale_x_continuous(breaks = c(2010:2023))+
  labs(
    title = "Change of highly digital employment by occupational group, 2010-2023",
    x = "",
    color = '',
    y = "Percent growth",
    fill = '',
    caption = "Source: Brookings analysis of 2023 Lightcast data"
  )+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Change of highly digital employment by occupational group 2010-2023.pdf",
       cohdebog, width = 10, height = 6)
ggsave("Change of highly digital employment by occupational group 2010-2023.jpg", 
       cohdebog, width = 10, height = 6)
##########################################
#Change of highly digital employment by industry group, 2010-2023
##########################################

cohdebig <- merged_data %>% 
  mutate(Jobs2023_industry_digitalization_percent = (Jobs2023_industry_digitalization-Jobs2010_industry_digitalization)/Jobs2010_industry_digitalization 
  ) %>% 
  filter(category == 'High') %>% 
  select(industry,
         Jobs2023_industry_digitalization_percent,
         Jobs2023_industry_digitalization,
         Jobs2010_industry_digitalization
  ) %>% 
  distinct() %>% 
  mutate(overall_growth = (sum(national_data$`2023 Jobs...4`) - sum(national_data$`2010 Jobs...3`))/sum(national_data$`2010 Jobs...3`))

write_csv(cohdebig,
          'Change of highly digital employment by industry group, 2010-2023.csv')

cohdebig <- cohdebig %>% 
  ggplot() +
  geom_bar(aes(x = reorder(as.character(industry), Jobs2023_industry_digitalization_percent),
               y = Jobs2023_industry_digitalization_percent,
               fill = reorder(as.character(industry), Jobs2023_industry_digitalization_percent)),
           stat = 'identity',
           position = 'dodge')+
  geom_hline( aes(yintercept =overall_growth) ,
              color = 'yellow')+
  geom_text(aes(x = 1,
                y = overall_growth + .1),
            label = 'Overall job growth')+
  scale_fill_manual(
    values = c(  'grey',
                 'grey',
                 'grey',
                 'grey',
                 'grey',
                 '#00649f',
                 'grey',
                 'grey',
                 'grey',
                 'grey',
                 'grey',
                 '#00649f',
                 
                 'grey',
                 'grey',
                 'grey',
                 'grey',
                 'grey',
                 "#00649f"
    )
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(-.75,
                                -.5,
                                -.25,
                                0,
                                .25,
                                .4,
                                .75),
                     limits = c(-1, 1),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Change of highly digital employment by occupational group, 2010-2023",
    x = "",
    y = "",
    fill = '',
    caption = "Source: Brookings analysis of 2023 Lightcast data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Change of highly digital employment by industry group, 2010-2023.pdf",
       cohdebig, width = 10, height = 6)
ggsave("Change of highly digital employment by industry group, 2010-2023.jpg", 
       cohdebig, width = 10, height = 6)

##########################################
#Share of workers in highly digital occupations compared to that group's share of the population as a whole
##########################################

SOWINHOCTTGSOTPAAW <- merged_data %>%  
  select(industry,
         Jobs2023_industry_digitalization,
         Male_industry_digitalization,
         Female_industry_digitalization,
         White_industry_digitalization,
         Black_industry_digitalization,
         Hispanic_industry_digitalization,
         AA_industry_digitalization,
         PI_industry_digitalization,
         AIAN_industry_digitalization,
         Twoplus_industry_digitalization,
         CEM,
         category
  ) %>% 
  filter(CEM == 'CEM',
         category == "High") %>% 
  mutate(`Indigeneous peoples_industry_digitalization` = AIAN_industry_digitalization + PI_industry_digitalization) %>% 
  distinct() %>% 
  mutate_at(c( 'Male_industry_digitalization',
               'Female_industry_digitalization',
               'White_industry_digitalization',
               'Black_industry_digitalization',
               'Hispanic_industry_digitalization',
               'AA_industry_digitalization',
               'Twoplus_industry_digitalization',
               'Indigeneous peoples_industry_digitalization',
               'Jobs2023_industry_digitalization'),
            ~sum(.)) %>% 
  mutate_at(c( 'Male_industry_digitalization',
               'Female_industry_digitalization',
               'White_industry_digitalization',
               'Black_industry_digitalization',
               'Hispanic_industry_digitalization',
               'AA_industry_digitalization',
               'Twoplus_industry_digitalization',
               'Indigeneous peoples_industry_digitalization'),
            ~./`Jobs2023_industry_digitalization`) %>% 
  select(
    Male_industry_digitalization,
    Female_industry_digitalization,
    White_industry_digitalization,
    Black_industry_digitalization,
    'Latino or Hispanic_industry_digitalization' = Hispanic_industry_digitalization,
    'Asian_industry_digitalization' = AA_industry_digitalization,
    'Multiracial_industry_digitalization' = Twoplus_industry_digitalization,
    `Indigeneous peoples_industry_digitalization`) %>%
  distinct() %>% 
  pivot_longer(everything(),
               values_to = 'Share of highly digital CEM occupations',
               names_to = 'group') %>% 
  distinct() %>% 
  mutate(group = str_replace(group, '_industry_digitalization', ''),
         `Share of highly digital CEM occupations` = round(`Share of highly digital CEM occupations`*100, digits = 2)) %>% 
  inner_join(.,
             demographics)

new_row(SOWINHOCTTGSOTPAAW) <- c(' ',
                                 SOWINHOCTTGSOTPAAW[1,2] -SOWINHOCTTGSOTPAAW[1,2],
                                 SOWINHOCTTGSOTPAAW[1,3] -SOWINHOCTTGSOTPAAW[1,3])

SOWINHOCTTGSOTPAAW <- SOWINHOCTTGSOTPAAW %>% 
  pivot_longer(-group) %>% 
  mutate(value = value/100) %>%
  mutate(group = factor(group,levels = c(
    "Male",
    'Female',
    ' ',
    'White',
    'Black',
    "Asian",
    'Latino or Hispanic',
    'Indigeneous peoples',
    'Multiracial'))) %>% 
  mutate_all(~case_when(. == 0 ~ NA,
                        T ~ .))

write_csv(SOWINHOCTTGSOTPAAW,
          "Share of workers in highly digital CEM occupations compared to that group's share of the population as a whole 2023.csv")

SOWINHOCTTGSOTPAAW<-SOWINHOCTTGSOTPAAW%>% 
  ggplot() +
  geom_bar(aes(x = fct_rev(group),
               y = value,
               fill = fct_rev(name)),
           stat = 'identity',
           position = 'dodge')+
  geom_text(aes(x = fct_rev(group),
                y = value+.03,
                fill = fct_rev(name),
                label = scales::percent(value, accuracy = 0.1)),
            stat = 'identity',
            position = position_dodge(width = .9))+
  scale_fill_manual(
    values = c(  'grey',
                 "#00649f")
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Share of workers in highly digital CEM occupations\ncompared to that group's share of the population as a whole, 2023",
    x = "",
    y = "",
    fill = '',
    caption = 'Note: Percentages may not sum to 100 due to exclusion of Some other race.\nSource: Brookings analysis of 2023 Lightcast and 2022 ACS data'
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Share of workers in highly digital occupations compared to that group's share of the population as a whole, 2023.pdf",
       SOWINHOCTTGSOTPAAW, width = 10, height = 6)
ggsave("Share of workers in highly digital occupations compared to that group's share of the population as a whole, 2023.jpg", 
       SOWINHOCTTGSOTPAAW, width = 10, height = 6)
##########################################
#Share of highly digital work across different demographic groups, in 2010 vs. 2023
##########################################
sohdwaddg <- demographics_data_merged %>%  
  filter(CEM == "CEM",
         category == 'High') %>% 
  transmute(Total_2023 = sum(`2023 Jobs...4`),
            Total_2010 = sum(`2010 Jobs...3`),
            Male_2023 = sum(`2023 Males`) / Total_2023,
            Female_2023 = sum(`2023 Females`) / Total_2023,
            White_2023 = sum(`2023 White`)/ Total_2023,
            Black_2023 = sum(`2023 Black or African American`)/ Total_2023,
            `Latino or Hispanic_2023` = sum(`2023 Hispanic or Latino`)/ Total_2023,
            Asian_2023 = sum(`2023 Asian`)/ Total_2023,
            `Indigenous peoples_2023` = (sum(`2023 American Indian or Alaska Native`)+
                                           sum(`2023 Native Hawaiian or Other Pacific Islander`))/
              Total_2023,
            `Multiracial_2023`= sum(`2023 Two or More Races`)/ Total_2023,
            Male_2010 = sum(`2010 Males`) / Total_2010,
            Female_2010 = sum(`2010 Females`) / Total_2010,
            White_2010 = sum(`2010 White`)/ Total_2010,
            Black_2010 = sum(`2010 Black or African American`)/ Total_2010,
            `Latino or Hispanic_2010` = sum(`2010 Hispanic or Latino`)/ Total_2010,
            Asian_2010 = sum(`2010 Asian`)/ Total_2010,
            `Indigenous peoples_2010` = (sum(`2010 American Indian or Alaska Native`)+
                                           sum(`2010 Native Hawaiian or Other Pacific Islander`))/
              Total_2010,
            `Multiracial_2010`= sum(`2010 Two or More Races`)/ Total_2010) %>% 
  distinct() %>% 
  select(-c(Total_2023,Total_2010)) %>% 
  pivot_longer(everything(),
               names_to = c(".value", "Year"),
               names_pattern = "^(.*?)_(\\d+)$") %>% 
  pivot_longer(-Year) 

new_row(sohdwaddg) <- list(as.character(sohdwaddg[1,1]),
                           ' ',
                           NA)

write_csv(sohdwaddg,
          'Share of highly digital work across different demographic groups, in 2010 vs. 2023.csv')

sohdwaddg <-sohdwaddg%>%
  mutate(Year = factor(Year,
                       levels = c('2023',
                                  '2010')),
         name = factor(name,
                       levels = c("Male",
                                  'Female',
                                  ' ',
                                  "White",
                                  'Black',
                                  "Latino or Hispanic",
                                  'Asian',
                                  "Indigenous peoples",
                                  'Multiracial'
                       ))) %>% 
  ggplot(aes(x = fct_rev(name),
             fill = Year,
             label = scales::percent(value,
                                     accuracy = 0.1) )) +
  geom_bar(aes(y = value),
           stat = 'identity',
           position = position_dodge(width = .9),
           width = .75)+
  geom_text(aes(y = value + .075),
            stat = 'identity',
            position = position_dodge(width = .9))+
  scale_fill_manual(
    values = c(  
      "#00649f",
      'grey')
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Share of CEM jobs across different demographic groups, in 2010 vs. 2023",
    #subtitle = ,
    x = "",
    y = "",
    fill = '',
    caption = "Note: American Indian, Alaska Native, Native Hawaiian, and Other Pacific Islander are included\nin the Indigenous peoples group\nSource: Brookings analysis of Lightcast data.
"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Share of highly digital work across different demographic groups20102023.pdf",
       sohdwaddg, width = 10, height = 6)
ggsave("Share of highly digital work across different demographic groups20102023.jpg", 
       sohdwaddg, width = 10, height = 6)

##########################################
#Average wage of highly digital occupations in selected occupational groups and the share of underrepresented highly digital workers in highly digital occupation categories
##########################################

awohdoisogatsouhdwihdoc <- merged_data %>% 
  group_by(industry, category) %>% 
  mutate(percent_underrep = (sum(`Current Year Black or African American`) +
                               sum(`Current Year Hispanic or Latino`) +
                               sum(`Current Year Native Hawaiian or Other Pacific Islander`) +
                               sum(`Current Year American Indian or Alaska Native`))/Jobs2023_industry_digitalization,
         average_wage = weighted.mean(x = `Avg. Annual Earnings`,
                                      w = `2023 Jobs...4`)
  ) %>% 
  select(industry, 
         category,
         average_wage,
         CEM,
         percent_underrep) %>% 
  ungroup() %>% 
  filter(category == 'High') %>% 
  distinct()
write_csv(awohdoisogatsouhdwihdoc,
          'Average wage of highly digital occupations in selected occupational groups and the share of underrepresented highly digital workers in highly digital occupation categories.csv')  

awohdoisogatsouhdwihdoc <- awohdoisogatsouhdwihdoc %>%
  ggplot(aes(x =reorder(industry,
                        -average_wage))) +
  geom_bar(aes(y=average_wage, fill=CEM),
           stat="identity",
           position = 'dodge') +
  geom_line(aes(y = percent_underrep*250000),
            group = 1)+
  scale_fill_manual(values = c('grey',
                               '#00649f'))+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(0,
                                25000,
                                50000,
                                75000,
                                100000,
                                125000,
                                150000),
                     limits = c(0, 155000),
                     expand = c(0,0))+
  labs(
    title = "Average wage of highly digital occupations in selected occupational groups and\nthe share of underrepresented highly digital workers in highly digital occupation categoriess, 2023",
    x = "",
    y = "",
    color = '',
    caption = "Source: Brookings analysis of Lightcast data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )

ggsave("Average wage of highly digital occupations in selected occupational groups and the share of underrepresented highly digital workers in highly digital occupation categories, 2023.pdf",
       awohdoisogatsouhdwihdoc, width = 12, height = 6)
ggsave("Average wage of highly digital occupations in selected occupational groups and the share of underrepresented highly digital workers in highly digital occupation categories, 2023.jpg", 
       awohdoisogatsouhdwihdoc, width = 12, height = 6) 
##########################################
#Employment of highly digital occupations and the racial employment gap
##########################################
EOHDOATREG <- merged_data %>% 
  mutate(total_diversity =  (`Current Year Hispanic or Latino`+
                               `Current Year Black or African American` +
                               `Current Year American Indian or Alaska Native` +
                               `Current Year Native Hawaiian or Other Pacific Islander` +
                               `Current Year Two or More Races`)/`2023 Jobs...4`) %>% 
  filter(CEM == 'CEM',
         category == 'High')

EOHDOATREGmgmt.lm <- lm(`Avg. Annual Earnings` ~ total_diversity,                
                        weights = `2023 Jobs...4`,
                        EOHDOATREG %>% filter(industry == "Management"))
EOHDOATREGcom.lm <- lm(`Avg. Annual Earnings` ~ total_diversity,
                       weights = `2023 Jobs...4`,
                       EOHDOATREG%>% filter(industry == "Computer and Mathematical"))
EOHDOATREGeng.lm <- lm(`Avg. Annual Earnings` ~ total_diversity,
                       weights = `2023 Jobs...4`,
                       EOHDOATREG%>% filter(industry == "Architecture and Engineering"))

write_csv(EOHDOATREG %>% mutate(mgmtlmslope = coef(EOHDOATREGmgmt.lm)[["total_diversity"]],
                                comlmslope = coef(EOHDOATREGcom.lm)[["total_diversity"]],
                                englmslope = coef(EOHDOATREGeng.lm)[["total_diversity"]],
                                mgmtlmint = coef(EOHDOATREGmgmt.lm)[["(Intercept)"]],
                                comlmint = coef(EOHDOATREGcom.lm)[["(Intercept)"]],
                                englmint = coef(EOHDOATREGeng.lm)[["(Intercept)"]]),
          "Employment of highly digital occupations and the racial employment gap.csv"
)
EOHDOATREG <-EOHDOATREG %>% 
  ggplot()+
  geom_point(
    aes(x = total_diversity,
        y = `Avg. Annual Earnings`,
        size = `2023 Jobs...4`,
        color = as.character(industry))
  )+
  geom_text(aes(x = total_diversity,
                y = `Avg. Annual Earnings`+5000,
                label = `Title`),
            data = EOHDOATREG %>% 
              filter(`Title` %in% c('Architectural and Engineering Managers',
                                    'Nuclear Engineers',
                                    'Computer User Support Specialists'
              )))+
  geom_abline(slope = coef(EOHDOATREGmgmt.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATREGmgmt.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              color = '#FF9E1B')+
  geom_abline(slope = coef(EOHDOATREGcom.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATREGcom.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              
              color = '#FCD200')+
  geom_abline(slope = coef(EOHDOATREGeng.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATREGeng.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              
              color = '#00649f')+
  scale_color_manual(
    breaks = c( 'Architecture and Engineering',
                'Computer and Mathematical',
                'Management'),
    values = c("#00649f",
               "#FCD200",
               '#FF9E1B'
    )
  )+
  scale_size_continuous(range = c(2,12),
                        label = scales::comma,
                        transform = 'sqrt',
                        breaks = c(10000,
                                   100000,
                                   500000,
                                   1000000)
  )+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(50000,
                                75000,
                                100000,
                                125000,
                                150000,
                                175000),
                     limits = c(0, 185000),
                     expand = c(0,0))+
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0,
                                0.05,
                                .1,
                                .15,
                                .2,
                                .25,
                                .3,
                                .35,
                                .4),
                     limits = c(0,.41),
                     expand = c(0,0))+
  labs(
    title = "Employment of highly digital occupations and the racial employment gap, 2023",
    x = "Share of underrepresented workers of color by occupation, 2023",
    y = "",
    color = '',
    size = 'Number of Jobs',
    caption = "Source: Brookings analysis of Lightcast  data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  guides(color = guide_legend(override.aes = list(size=8)))+
  theme(
    
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Employment of highly digital occupations and the racial employment gap, 2023.pdf",
       EOHDOATREG, width = 12, height = 6)
ggsave("Employment of highly digital occupations and the racial employment gap, 2023.jpg", 
       EOHDOATREG, width = 12, height = 6)

##########################################  
#Average wage of highly digital occupations in selected occupational groups and the share of women highly digital workers in highly digital occupation categories,
##########################################

awohdoisogatsowhdwihdoc <- merged_data %>% 
  group_by(CEM, category) %>% 
  mutate(percent_women_CEM = sum(`Current Year Females`)/Jobs2023_CEM_digitalization ) %>%
  group_by(industry, category) %>% 
  mutate(percent_women = sum(`Current Year Females`)/Jobs2023_industry_digitalization,
         average_wage = weighted.mean(x = `Avg. Annual Earnings`,
                                      w = `2023 Jobs...4`)
  ) %>% 
  ungroup %>% 
  select(industry, 
         category,
         average_wage,
         CEM,
         percent_women,
         percent_women_CEM) %>% 
  ungroup() %>% 
  filter(category == 'High') %>% 
  distinct()
write_csv(awohdoisogatsowhdwihdoc,
          'Average wage of highly digital occupations in selected occupational groups and the share of women highly digital workers in highly digital occupation categories.csv')  

awohdoisogatsowhdwihdoc <- awohdoisogatsowhdwihdoc %>%
  ggplot(aes(x =reorder(industry,
                        -average_wage))) +
  geom_bar(aes(y=average_wage, fill=CEM),
           stat="identity",
           position = 'dodge') +
  geom_line(aes(y = percent_women*120000),
            group = 1)+
  scale_fill_manual(values = c('grey',
                               '#00649f'))+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(0,
                                25000,
                                50000,
                                75000,
                                100000,
                                125000,
                                150000),
                     limits = c(0, 155000),
                     expand = c(0,0))+
  labs(
    title = "Average wage of highly digital occupations in selected occupational groups and\nthe share of women highly digital workers in highly digital occupation categoriess, 2023",
    x = "",
    y = "",
    color = '',
    caption = "Source: Brookings analysis of Lightcast data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )

ggsave("Average wage of highly digital occupations in selected occupational groups and the share of women highly digital workers in highly digital occupation categories, 2023.pdf",
       awohdoisogatsowhdwihdoc, width = 12, height = 6)
ggsave("Average wage of highly digital occupations in selected occupational groups and the share of women highly digital workers in highly digital occupation categories, 2023.jpg", 
       awohdoisogatsowhdwihdoc, width = 12, height = 6) 

##########################################
#Employment of highly digital occupations and the gender employment gap
##########################################  
EOHDOATGEG <-merged_data %>% 
  mutate(total_diversity =  (`Current Year Females`)/`2023 Jobs...4`) %>% 
  filter(CEM == 'CEM',
         category == 'High')

EOHDOATGEGmgmt.lm <- lm(`Avg. Annual Earnings` ~ total_diversity,
                        weights = `2023 Jobs...4`,
                        EOHDOATGEG %>% filter(industry == "Management"))
EOHDOATGEGcom.lm <- lm(`Avg. Annual Earnings` ~ total_diversity, 
                       weights = `2023 Jobs...4`,
                       EOHDOATGEG%>% filter(industry == "Computer and Mathematical"))
EOHDOATGEGeng.lm <- lm(`Avg. Annual Earnings` ~ total_diversity, 
                       weights = `2023 Jobs...4`,
                       EOHDOATGEG%>% filter(industry == "Architecture and Engineering"))

write_csv(EOHDOATGEG %>% mutate(mgmtlmslope = coef(EOHDOATGEGmgmt.lm)[["total_diversity"]],
                                comlmslope = coef(EOHDOATGEGcom.lm)[["total_diversity"]],
                                englmslope = coef(EOHDOATGEGeng.lm)[["total_diversity"]],
                                mgmtlmint = coef(EOHDOATGEGmgmt.lm)[["(Intercept)"]],
                                comlmint = coef(EOHDOATGEGcom.lm)[["(Intercept)"]],
                                englmint = coef(EOHDOATGEGeng.lm)[["(Intercept)"]]),
          "Employment of highly digital occupations and the gender employment gap.csv"
)


EOHDOATGEG <-EOHDOATGEG %>% 
  ggplot()+
  geom_point(
    aes(x = total_diversity,
        y = `Avg. Annual Earnings`,
        size = `2023 Jobs...4`,
        color = industry)
  )+
  geom_text(aes(x = total_diversity,
                y = `Avg. Annual Earnings`+5000,
                label = `Title`),
            data = EOHDOATGEG %>% 
              filter(`Title` %in% c('Architectural and Engineering Managers',
                                    'Nuclear Engineers',
                                    'Computer User Support Specialists'
              )))+
  geom_abline(slope = coef(EOHDOATGEGmgmt.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATGEGmgmt.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              color = '#FF9E1B')+
  geom_abline(slope = coef(EOHDOATGEGcom.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATGEGcom.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              
              color = '#FCD200')+
  geom_abline(slope = coef(EOHDOATGEGeng.lm)[["total_diversity"]], 
              intercept = coef(EOHDOATGEGeng.lm)[["(Intercept)"]],
              linetype = 'dashed',
              linewidth = 1.5,
              
              color = '#00649f')+
  scale_color_manual(
    breaks = c( 'Architecture and Engineering',
                'Computer and Mathematical',
                'Management'),
    values = c("#00649f",
               "#FCD200",
               '#FF9E1B'
    )
  )+
  scale_size_continuous(range = c(2,12),
                        label = scales::comma,
                        transform = 'sqrt',
                        breaks = c(10000,
                                   100000,
                                   500000,
                                   1000000)
  )+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(50000,
                                75000,
                                100000,
                                125000,
                                150000,
                                175000),
                     limits = c(0, 185000),
                     expand = c(0,0))+
  scale_x_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7),
                     limits = c(0,.75),
                     expand = c(0,0))+
  labs(
    title = "Employment of highly digital occupations and the gender employment gap, 2023",
    x = "Share of women workers by occupation, 2023",
    y = "",
    color = '',
    size = 'Number of Jobs',
    caption = "Source: Brookings analysis of Lightcast data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  guides(color = guide_legend(override.aes = list(size=8)))+
  theme(
    
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Employment of highly digital occupations and the gender employment gap, 2023.pdf",
       EOHDOATGEG, width = 12, height = 6)
ggsave("Employment of highly digital occupations and the gender employment gap, 2023.jpg", 
       EOHDOATGEG, width = 12, height = 6)

##########################################
#Typical education for entry for highly digital occupations
########################################## 

tefefhdo <- merged_data %>% 
  group_by(bachelors) %>% 
  mutate(
    wages_bachelors = weighted.mean(`Avg. Annual Earnings`,
                                    w=`2023 Jobs...4`)) %>% 
  ungroup() %>% 
  group_by(bachelors, CEM, category) %>% 
  summarise(wages_bachelors = wages_bachelors,
            jobs = sum(`2023 Jobs...4`),
            wages = weighted.mean(`Avg. Annual Earnings`,
                                  w=`2023 Jobs...4`)) %>% 
  distinct() %>% 
  ungroup %>% 
  group_by(CEM, category) %>% 
  mutate(jobs_percent = jobs/sum(jobs)) 



write_csv(tefefhdo,
          'Typical education for entry for highly digital occupations.csv')
tefefhdo <- tefefhdo %>% 
  filter(CEM == 'CEM',
         category == 'High') %>% 
  ggplot( aes(x="", y=jobs_percent, fill=factor(bachelors, 
                                                levels = c(0,1),
                                                labels = c("Can be acessed without a bachelor's degree",
                                                           "Typically requiring a bachelor's degree or higher")))) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(
    label = paste0(scales::percent(jobs_percent),
                   "\n",
                   scales::comma(round(jobs/1000000)),
                   ' million jobs')),
    color = 'white')+
  scale_fill_manual(values = c('grey',
                               '#00649f'))+
  coord_polar("y", start=0)+
  labs(
    title = "Typical education for entry for highly digital occupations, 2023",
    x = "",
    y = "",
    color = '',
    caption = "Source: Brookings analysis of Lightcast data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_void()+
  guides(color = guide_legend(override.aes = list(size=8)))+
  theme(
    
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )

ggsave("Typical education for entry for highly digital occupations, 2023.pdf",
       tefefhdo, width = 12, height = 6)
ggsave("Typical education for entry for highly digital occupations, 2023.jpg", 
       tefefhdo, width = 12, height = 6)

##########################################
#Average wages by selected occupational groups, 2023
##########################################
AWBSOG <- merged_data %>% 
  mutate(
    `All jobs` = weighted.mean(`Avg. Annual Earnings`,
                               w = `2023 Jobs...4`)
  ) %>% 
  group_by(bachelors) %>% 
  mutate(`All jobs that do not require a bachelor's degree` = weighted.mean(`Avg. Annual Earnings`,
                                                                            w = `2023 Jobs...4`)) %>% 
  group_by(CEM, category, add = TRUE) %>% 
  mutate(`CEM jobs that do not require a bachelor's degree` = weighted.mean(`Avg. Annual Earnings`,
                                                                            w = `2023 Jobs...4`)) %>% 
  ungroup() %>% 
  filter(category == 'High',
         CEM == 'CEM',
         bachelors == 0) %>% 
  select(`All jobs`,
         `All jobs that do not require a bachelor's degree`,
         `CEM jobs that do not require a bachelor's degree`
  ) %>% 
  distinct() %>% 
  pivot_longer(everything())#

write_csv(AWBSOG,
          'Average wages by selected occupational groups.csv')
AWBSOG <-AWBSOG %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(name),
               y = value,
               fill = fct_rev(name)), 
           stat = 'identity',
           position= 'dodge')+
  geom_text(aes(x = fct_rev(name),
                y = value+5000,
                fill = fct_rev(name),
                label = scales::dollar(round((value)/100)*100)),
            color= 'black',
            stat = 'identity',
            position= position_dodge(width = .9))+
  scale_fill_manual(
    values = c("#00649f",
               "grey",
               'grey'
    )
  )+
  scale_y_continuous(labels = scales::dollar,
                     breaks = c(0,
                                10000,
                                20000,
                                30000,
                                40000,
                                50000,
                                60000,
                                70000),
                     limits = c(0, 80000),
                     expand = c(0,0))+
  labs(
    title = "Average wages by selected occupational groups, 2023",
    x = "",
    y = "",
    caption = "Note: Occupational groups that have no highly digital workers are excluded. Occupations without\na digital score are excluded from the calculation.\nSource: Brookings analysis of Lightcast data"
  )+
  theme_classic()+
  theme(
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle = 20, vjust = .95,
                               hjust = .95)
  )
ggsave("Average wages by selected occupational groups, 2023.pdf",
       AWBSOG, width = 9, height = 6)
ggsave("Average wages by selected occupational groups, 2023.jpg", 
       AWBSOG, width = 9, height = 6)
##########################################
#Share of jobs that are highly digital by state, 2023
##########################################  
sojtahdbs <- state_data_merged %>% 
  group_by(State) %>% 
  mutate(`Total jobs` = sum(`2023 Jobs...4`)) %>% 
  filter(CEM == "CEM",
         category == 'High') %>% 
  mutate(`CEM jobs` = sum(`2023 Jobs...4`),
         `CEM percent` = `CEM jobs`/`Total jobs`) %>% 
  group_by(industry, add = T) %>%
  mutate(`Industry jobs` = sum(`2023 Jobs...4`),
         `Industry percent` = `Industry jobs`/`Total jobs`) %>% 
  ungroup() %>% 
  select(State,
         industry,
         `Total jobs`,
         `CEM jobs`,
         `CEM percent`,
         `Industry jobs`,
         `Industry percent`) %>% 
  mutate(nation_average = as.numeric(merged_data %>%
                                       mutate(all_jobs = sum(`2023 Jobs...4`)) %>% 
                                       filter(CEM == 'CEM',
                                              category == 'High') %>%
                                       mutate(HDCEM = sum(`2023 Jobs...4`)) %>% 
                                       transmute(one = HDCEM/all_jobs) %>% 
                                       distinct()),
         group = factor(case_when(
           `CEM percent` <= .07 ~ 1,
           `CEM percent` <= .08 ~ 2,
           `CEM percent` <= .09~ 3,
           `CEM percent` <= .1 ~ 4,
           `CEM percent` <= .125 ~ 5,
           
           `CEM percent` > .125 ~6
         ),
         levels = c(1,2,3,4,5, 6),
         labels = c('7% and below',
                    '7%-8%',
                    '8%-9%',
                    '9%-10%',
                    '10$-12.5%',
                    '12.5% and up'))
  ) %>% 
  distinct() %>% 
  
  pivot_wider(names_from = c(industry),
              values_from = c( `Industry jobs`,
                               `Industry percent`))

library(sf)
state_map <- read_sf('V:/Carl/AI/03.24 Map table recreation/state_map.shp')

sojtahdbs <- inner_join(sojtahdbs %>% mutate(State = str_to_lower(State)),
                        check <- state_map %>% 
                          mutate(State = str_to_lower(str_remove_all(NAME, ' '))))
write_csv(sojtahdbs,
          "Share of jobs that are highly digital CEM by state.csv")
sojtahdbs <- ggplot()+
  geom_sf(color = alpha("white"), 
          aes(fill = fct_rev(group),
              geometry = geometry), 
          data = sojtahdbs )+
  scale_fill_manual(values = c("#023147",
                               '#0061A0',
                               '#287EC9',
                               '#66ACED',
                               '#8BB8E8',
                               '#CAE1FA'),
                    name = "CEM share")+
  labs(
    title = "Share of jobs that are highly digital CEM by state, 2023",
    caption = "Source: Brookings analysis of 2023 Lightcast  data"
  )+
  coord_sf(crs = st_crs(9311))+
  theme_void() 

ggsave("Share of jobs that are highly digital CEM by state, 2023.pdf",
       sojtahdbs, width = 9, height = 6)
ggsave("Share of jobs that are highly digital CEM by state, 2023.jpg", 
       sojtahdbs, width = 9, height = 6)


##########################################
#Share of metro's employment in highly digital CEM occupations in selected metro areas
##########################################  

someihdcoisma <- metro_data_merged %>% 
  group_by(Metro) %>% 
  mutate(`Total jobs` = sum(`2023 Jobs...8`)) %>% 
  filter(CEM == 'CEM',
         category == 'High') %>% 
  group_by(CEM, add = T) %>% 
  mutate(`CEM jobs` = sum(`2023 Jobs...8`),
         `CEM percent` = `CEM jobs`/`Total jobs`) %>% 
  group_by(Metro, industry, add = F) %>% 
  mutate(`Industry jobs` = sum(`2023 Jobs...8`),
         `Industry percent` = `Industry jobs`/`Total jobs`) %>% 
  ungroup() %>% 
  select(Metro,
         industry,
         `Total jobs`,
         `CEM jobs`,
         `CEM percent`,
         `Industry jobs`,
         `Industry percent`) %>% 
  distinct() %>% 
  pivot_wider(names_from = c(industry),
              values_from = c( `Industry jobs`,
                               `Industry percent`)) %>% 
  mutate(nation_average_CEM = as.numeric(merged_data %>%
                                           mutate(all_jobs = sum(`2023 Jobs...4`)) %>% 
                                           filter(CEM == 'CEM',
                                                  category == 'High') %>%
                                           mutate(HDCEM = sum(`2023 Jobs...4`)) %>% 
                                           transmute(one = HDCEM/all_jobs) %>% 
                                           distinct())) %>%
  arrange(desc(`Total jobs`)) 

library(ds4psy)
my_custom_labels = someihdcoisma %>%
  top_n(`Total jobs`, 
        n = 100) %>% 
  arrange(`CEM percent`, desc = T) %>% 
  transmute(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                              T ~ "")) %>% 
  arrange(desc(row_number()))

someihdcoisma<- someihdcoisma %>%
  top_n(`Total jobs`, 
        n = 100) %>% 
  arrange(`CEM percent`, desc = T) %>% 
  mutate(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                           T ~ ""))  

write_csv(someihdcoisma, 
          "Share of metro's employment in highly digital CEM occupations in selected metro areas.csv")
someihdcoisma<- someihdcoisma %>%
  ggplot()+
  geom_bar(aes(x = reorder(Metro, desc(`CEM percent`)),
               y = `CEM percent`), 
           stat = 'identity',
           fill = '#00649f', 
           position= 'dodge')+
  geom_hline(aes(yintercept =nation_average_CEM),
             size = 2,
             color = "#FF9E1B")+
  scale_x_discrete(labels = fct_rev(my_custom_labels$title))+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                0.05,
                                .1,
                                .15,
                                .2,
                                .25,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, .25),
                     expand = c(0,0))+
  labs(
    title = "Share of metro's employment in highly digital CEM occupations in selected metro areas, 2023",
    x = "",
    y = "",
    caption = "Occupations without a digital score are excluded from the calculation.\nSource: Brookings analysis of Lightcast data"
  )+
  theme_classic()+
  theme(
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    axis.ticks.y =element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )
ggsave("Share of metro's employment in highly digital CEM occupations in selected metro area, 2023.pdf",
       someihdcoisma, width = 9, height = 6)
ggsave("Share of metro's employment in highly digital CEM occupations in selected metro area, 2023.jpg", 
       someihdcoisma, width = 9, height = 6)
##########################################
#Female workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs cross the nation's 100 largest metro areas
##########################################  

fwauihdcemjatn100lma <- metro_data_merged %>% 
  group_by(category, CEM, Metro) %>% 
  mutate(Percent_Female_HDCEM = sum(`Current Year Females`)/Jobs2023_CEM_digitalization ) %>% 
  ungroup() %>% 
  filter(category == 'High',
         CEM == 'CEM') %>% 
  select(Metro,
         Female,
         jobs2023_total,
         Jobs2023_CEM_digitalization,
         Percent_Female_HDCEM) %>% 
  distinct() %>% 
  mutate(
    ratio = Percent_Female_HDCEM / Female * 100
  ) 


my_custom_labels = fwauihdcemjatn100lma %>%
  top_n(jobs2023_total, 
        n = 100) %>% 
  arrange(ratio, desc = T) %>% 
  transmute(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                              T ~ "")) %>% 
  arrange(desc(row_number()))

fwauihdcemjatn100lma <-fwauihdcemjatn100lma %>% 
  slice_max(jobs2023_total, n = 100) %>% 
  top_n(jobs2023_total, 
        n = 100) %>% 
  arrange(ratio, desc = T) %>% 
  mutate(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                           T ~ "")) 

write_csv(fwauihdcemjatn100lma,
          "Female workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs cross the nation's 100 largest metro areas.csv")

fwauihdcemjatn100lma <-fwauihdcemjatn100lma %>% 
  ggplot()+
  geom_bar(aes(x = reorder(Metro, desc(ratio)),
               y = ratio), 
           stat = 'identity',
           width = .75,
           fill = '#00649f', 
           position= 'dodge')+
  geom_hline(aes(yintercept =1),
             size = 2,
             color = "#FF9E1B")+
  scale_x_discrete(labels = fct_rev(my_custom_labels$title))+
  scale_y_continuous(labels = scales::comma,
                     breaks = c(0,
                                .25,
                                .5,
                                .75,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  labs(
    title = "Female workers are underrepresented in highly digital computer, engineering, and management (CEM)\njobs cross the nation's 100 largest metro areas, 2023",
    x = "",
    y = "Ratio of Female CEM workers to Female population\n(Ratio of 1 singifies proportional representation)",
    caption = "Source: Brookings analysis of Lightcast  data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  guides(color = guide_legend(override.aes = list(size=8)))+
  theme(
    
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )

ggsave("Female workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs cross the nation's 100 largest metro areas, 2023.pdf",
       fwauihdcemjatn100lma, width = 12, height = 6)
ggsave("Female workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs cross the nation's 100 largest metro areas, 2023.jpg", 
       fwauihdcemjatn100lma, width = 12, height = 6)

##########################################
#Black, Latino or Hispanic, and Indigenous workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs across the nation's 100 largest metro areas 
########################################## 
blohaiwauihdcemjatn100lma <- metro_data_merged  %>%
  group_by(category, CEM, Metro) %>% 
  mutate(Percent_Minority_HDCEM = (sum(`Current Year Black or African American`) +
                                     sum(`Current Year Hispanic or Latino`) + 
                                     sum(`Current Year American Indian or Alaska Native`) +
                                     sum(`Current Year Native Hawaiian or Other Pacific Islander`)) /
           Jobs2023_CEM_digitalization ) %>% 
  ungroup() %>% 
  filter(category == 'High',
         CEM == 'CEM') %>% 
  select(Metro,
         Minority,
         jobs2023_total,
         Jobs2023_CEM_digitalization,
         Percent_Minority_HDCEM) %>% 
  distinct() %>% 
  mutate(
    ratio = Percent_Minority_HDCEM / Minority * 100
  ) 

my_custom_labels = blohaiwauihdcemjatn100lma %>%
  top_n(jobs2023_total, 
        n = 100) %>% 
  arrange(ratio, desc = T) %>% 
  transmute(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                              T ~ "")) %>% 
  arrange(desc(row_number()))

blohaiwauihdcemjatn100lma <-blohaiwauihdcemjatn100lma %>% 
  slice_max(jobs2023_total, n = 100) %>% 
  top_n(jobs2023_total, 
        n = 100) %>% 
  arrange(ratio, desc = T) %>% 
  mutate(title = case_when(is_wholenumber(row_number()/2) ==  T ~ Metro,
                           T ~ ""))
write_csv(blohaiwauihdcemjatn100lma,
          "Black, Latino or Hispanic, and Indigenous workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs cross the nation's 100 largest metro areas.csv")

blohaiwauihdcemjatn100lma <-blohaiwauihdcemjatn100lma %>% 
  ggplot()+
  geom_bar(aes(x = reorder(Metro, desc(ratio)),
               y = ratio), 
           stat = 'identity',
           width = .75,
           fill = '#00649f', 
           position= 'dodge')+
  geom_hline(aes(yintercept =1),
             size = 2,
             color = "#FF9E1B")+
  scale_x_discrete(labels = fct_rev(my_custom_labels$title))+
  scale_y_continuous(labels = scales::comma,
                     breaks = c(0,
                                .25,
                                .5,
                                .75,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  labs(
    title = "Black, Latino or Hispanic, and Indigenous workers are underrepresented in highly digital\n computer, engineering, and management (CEM) jobs across the nation's 100 largest metro areas, 2023",
    x = "",
    y = "Ratio of Black, Latino or Hispanic, and Indigenous CEM workers to\n Black, Latino or Hispanic, and Indigenous population\n(Ratio of 1 singifies proportional representation)",
    caption = "Source: Brookings analysis of Lightcast  data"
  )+
  #guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  guides(color = guide_legend(override.aes = list(size=8)))+
  theme(
    
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    axis.text.x = element_text(angle = 70, vjust = .95,
                               hjust = .95)
  )
ggsave("Black, Latino or Hispanic, and Indigenous  workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs across the nation's 100 largest metro areas, 2023.pdf",
       blohaiwauihdcemjatn100lma, width = 12, height = 8)
ggsave("Black, Latino or Hispanic, and Indigenous  workers are underrepresented in highly digital computer, engineering, and management (CEM) jobs across the nation's 100 largest metro areas, 2023.jpg", 
       blohaiwauihdcemjatn100lma, width = 12, height = 8)
##########################################
#Women, Black, Latino or Hispanic, and Indigenous individuals are underrepresented in STEM graduates and within highly digital CEM jobs
##########################################

wblohaiiauisgawhdcemj <- demographics_data_merged %>%
  filter(CEM == "CEM",
         category == 'High') %>% 
  mutate(Jobs2023_industry_digitalization = sum(`2022 Jobs`),
         Male_industry_digitalization = sum(`2022 Males`),
         Female_industry_digitalization = sum(`2022 Females`),
         White_industry_digitalization = sum(`2022 White`),
         Black_industry_digitalization= sum(`2022 Black or African American`),
         Hispanic_industry_digitalization= sum(`2022 Hispanic or Latino`),
         AA_industry_digitalization= sum(`2022 Asian`),
         PI_industry_digitalization= sum(`2022 Native Hawaiian or Other Pacific Islander`),
         AIAN_industry_digitalization= sum(`2022 American Indian or Alaska Native`),
         Twoplus_industry_digitalization= sum(`2022 Two or More Races`)
  ) %>% 
  select(
    Jobs2023_industry_digitalization,
    Male_industry_digitalization,
    Female_industry_digitalization,
    White_industry_digitalization,
    Black_industry_digitalization,
    Hispanic_industry_digitalization,
    AA_industry_digitalization,
    PI_industry_digitalization,
    AIAN_industry_digitalization,
    Twoplus_industry_digitalization,
    CEM,
    category
  ) %>%  
  mutate(`Indigeneous peoples_industry_digitalization` = AIAN_industry_digitalization + PI_industry_digitalization) %>% 
  distinct()  %>% 
  mutate_at(c( 'Male_industry_digitalization',
               'Female_industry_digitalization',
               'White_industry_digitalization',
               'Black_industry_digitalization',
               'Hispanic_industry_digitalization',
               'AA_industry_digitalization',
               'Twoplus_industry_digitalization',
               'Indigeneous peoples_industry_digitalization'),
            ~./`Jobs2023_industry_digitalization`) %>% 
  select(
    Male_industry_digitalization,
    Female_industry_digitalization,
    White_industry_digitalization,
    Black_industry_digitalization,
    'Latino or Hispanic_industry_digitalization' = Hispanic_industry_digitalization,
    'Asian_industry_digitalization' = AA_industry_digitalization,
    'Multiracial_industry_digitalization' = Twoplus_industry_digitalization,
    `Indigeneous peoples_industry_digitalization`) %>%
  distinct() %>% 
  pivot_longer(everything(),
               values_to = 'Share of highly digital CEM occupations',
               names_to = 'group') %>% 
  distinct() %>% 
  mutate(group = str_replace(group, '_industry_digitalization', ''),
         `Share of highly digital CEM occupations` = round(`Share of highly digital CEM occupations` *
                                                             100,
                                                           digits = 2)) %>% 
  left_join(.,
            demographics) %>% 
  left_join(.,
            ipeds %>% 
              mutate(`Share of stem graduate degrees conferred` = Percent*100) %>% 
              select(-c(Population,
                        Percent)))

new_row(wblohaiiauisgawhdcemj) <- c(' ',
                                    wblohaiiauisgawhdcemj[1,2] -wblohaiiauisgawhdcemj[1,2],
                                    wblohaiiauisgawhdcemj[1,3] -wblohaiiauisgawhdcemj[1,3],
                                    wblohaiiauisgawhdcemj[1,4] -wblohaiiauisgawhdcemj[1,4])

wblohaiiauisgawhdcemj <- wblohaiiauisgawhdcemj %>% 
  pivot_longer(-group) %>% 
  mutate(value = value/100,
         group = factor(group,
                        levels = c(
                          "Male",
                          'Female',
                          ' ',
                          'White',
                          'Black',
                          "Asian",
                          'Latino or Hispanic',
                          'Indigeneous peoples',
                          'Multiracial'))) %>% 
  mutate_all(~case_when(. == 0 ~ NA,
                        T ~ .))

write_csv(wblohaiiauisgawhdcemj,
          'Women, Black, Latino or Hispanic, and Indigenous individuals are underrepresented in STEM graduates and within highly digital CEM jobse 2023.csv')

wblohaiiauisgawhdcemj <-wblohaiiauisgawhdcemj%>%
  mutate(name = factor(name,
                       levels = c('Share of Population',
                                  "Share of stem graduate degrees conferred",
                                  "Share of highly digital CEM occupations"))) %>% 
  ggplot(aes(x = fct_rev(group),
             fill = fct_rev(name),
             label = scales::percent(value,
                                     accuracy = 0.1) )) +
  geom_bar(aes(y = value),
           stat = 'identity',
           position = position_dodge(width = .9),
           width = .75)+
  geom_text(aes(y = value + .075),
            stat = 'identity',
            position = position_dodge(width = .9))+
  scale_fill_manual(
    values = c(  '#023147',
                 "#00649f",
                 '#66ACED')
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Women, Black, Latino or Hispanic, and Indigenous individuals are underrepresented\nin STEM graduates and within highly digital CEM jobs, 2022",
    subtitle = 'Share of U.S. population; Share of STEM degrees; and Share of\ncomputer, engineering and Management (CEM) occupations by race and gender, 2022',
    x = "",
    y = "",
    fill = '',
    caption = "Note: American Indian, Alaska Native, Native Hawaiian, and Other Pacific Islander are included\nin the Indigenous peoples group. Census percentages may not sum to 100% due to exclusion\nof 'some other race'. Race/ethnicity unknown and nonresident alien were excluded from\nthe calculation of IPEDS data.\nSource: Brookings analysis of 5-year 2022 ACS data, 2022 IPEDS, and\n2022 Lightcast data.
"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank()
    
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Women, Black, Latino or Hispanic, and Indigenous individuals are underrepresented in STEM graduates and within highly digital CEM jobs, 2023.pdf",
       wblohaiiauisgawhdcemj, width = 10, height = 6)
ggsave("Women, Black, Latino or Hispanic, and Indigenous individuals are underrepresented in STEM graduates and within highly digital CEM jobs, 2023.jpg", 
       wblohaiiauisgawhdcemj, width = 10, height = 6)
##########################################
#Share of graduates in computer and engineering that pursue careers in computer, engineering, and management fields by gender and race
##########################################
sogicaetpciceamfbgr <- pathways_data  %>% 
  group_by(Group) %>% 
  transmute(`Computer and math` = sum(`Computer and math`),
            Engineering = sum(Engineering)) %>% 
  distinct()
write_csv(sogicaetpciceamfbgr, 'Share of graduates in computer and engineering that pursue careers in computer, engineering, and management fields by gender and race.csv')

total_c <- as.numeric(sogicaetpciceamfbgr[1,2]/100)
total_e <- as.numeric(sogicaetpciceamfbgr[1,3]/100)

new_row(sogicaetpciceamfbgr) <- c(' ',
                                  sogicaetpciceamfbgr[1,2] -sogicaetpciceamfbgr[1,2],
                                  sogicaetpciceamfbgr[1,3] -sogicaetpciceamfbgr[1,3])

sogicaetpciceamfbgr <- sogicaetpciceamfbgr %>% 
  filter(Group != 'Total' ) %>% 
  mutate_at(c('Computer and math',
              'Engineering' ),
            ~./100) %>% 
  pivot_longer(-Group,
               names_to = 'major',
               values_to = 'percent') %>% 
  ggplot()+
  geom_bar(aes(x = fct_rev(factor(Group,
                                  levels = c('Male',
                                             'Female',
                                             ' ',
                                             'White',
                                             'Black',
                                             'Hispanic or Latino',
                                             'Asian'))),
               y = percent,
               fill = major),
           stat = 'identity',
           position= 'dodge')+
  geom_hline(yintercept = total_c,
             color = '#023147')+
  geom_hline(yintercept = total_e,
             color =  '#CAE1FA')+
  geom_text(aes(y = total_c-.1, x = 5 ),
            label = 'U.S. Avg (computer and math)',
            color = '#023147')+
  geom_text(aes(y = total_e+.1, x = 5 ),
            color ='#CAE1FA',
            label = 'U.S. Avg (engineering)')+
  
  scale_fill_manual(
    values = c(  '#023147',
                 '#CAE1FA')
  )+
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0,
                                .1,
                                .2,
                                .3,
                                .4,
                                .5,
                                .6,
                                .7,
                                .8,
                                .9,
                                1),
                     limits = c(-0.001, 1.001),
                     expand = c(0,0))+
  coord_flip()+
  labs(
    title = "Share of graduates in computer and engineering that pursue careers in computer, engineering, and management fields by gender and race",
    x = "",
    fill = '',
    y = "",
    caption = "Source: Brookings analysis of Census data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank()
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("Share of graduates in computer and engineering that pursue careers in computer, engineering, and management fields by gender and race.pdf",
       sogicaetpciceamfbgr, width = 9, height = 6)
ggsave("Share of graduates in computer and engineering that pursue careers in computer, engineering, and management fields by gender and race.jpg", 
       sogicaetpciceamfbgr, width = 9, height = 6)

##########################################
#Box plot
##########################################

historical_digitalization_plot <- inner_join(national_data,
                                             digital_scores2019_2024)

historical_digitalization_plot <- historical_digitalization_plot %>% 
  mutate(digit2024 = case_when(score2024 >= 0 ~ 1,
                               T ~ 0),
         digit2023 = case_when(score2023 >= 0 ~ 1,
                               T ~ 0),
         digit2022 = case_when(score2022 >= 0 ~ 1,
                               T ~ 0),
         digit2021 = case_when(score2021 >= 0 ~ 1,
                               T ~ 0),
         digit2020 = case_when(score2020 >= 0 ~ 1,
                               T ~ 0),
         digit2019 = case_when(score2019 >= 0 ~ 1,
                               T ~ 0),
         CEM = case_when(str_sub(SOC, end = '3') %in% c("11-",
                                                        
                                                        "15-",
                                                        "17-") ~ 'CEM',
                         T ~ 'non-CEM'),
         HD2024 = case_when(score2024 > 60 ~ 1 & CEM == 'CEM',
                            score2024 < 60 &
                              score2024 >= 0 |
                              CEM == 'non-CEM'&
                              score2024 >= 0 ~ 0,
                            score2024 < 0 ~ NA),
         HD2023 = case_when(score2023 > 60 ~ 1 & CEM == 'CEM',
                            score2023 <= 60 &
                              score2023 >= 0 |
                              CEM == 'non-CEM'&
                              score2023 >= 0 ~ 0,
                            score2023 < 0 ~ NA),
         HD2022 = case_when(score2022 > 60 ~ 1 & CEM == 'CEM',
                            score2022 <= 60 &
                              score2022 >= 0 |
                              CEM == 'non-CEM'&
                              score2022 >= 0~ 0,
                            score2022 < 0 ~ NA),
         HD2021 = case_when(score2021 > 60 ~ 1 & CEM == 'CEM',
                            score2021 <= 60 &
                              score2021 >= 0 |
                              CEM == 'non-CEM'&
                              score2021 >= 0 ~ 0,
                            score2021 < 0 ~ NA),
         HD2020 = case_when(score2020 > 60 ~ 1 & CEM == 'CEM',
                            score2020 <= 60 &
                              score2020 >= 0 |
                              CEM == 'non-CEM'&
                              score2020 >= 0 ~ 0,
                            score2020 < 0 ~ NA),
         HD2019 = case_when(score2019 > 60 ~ 1 & CEM == 'CEM',
                            score2019 <= 60 &
                              score2019 >= 0 |
                              CEM == 'non-CEM' &
                              score2019 >= 0 ~ 0,
                            score2019 < 0 ~ NA))
historical_digitalization_plot %>% 
  group_by(digit2021) %>% 
  summarise(sum(`2023 Jobs...4`))
HDP <- dplyr::tibble(
  year = as.character(c(2019:2024)),
  jobs = c(as.numeric(historical_digitalization_plot %>% 
                        filter(HD2019 == 1) %>% 
                        summarise(sum(`2023 Jobs...4`))),
           as.numeric(historical_digitalization_plot %>% 
                        filter(HD2020 == 1) %>% 
                        summarise(sum(`2023 Jobs...4`))),
           as.numeric(historical_digitalization_plot %>% 
                        filter(HD2021 == 1)%>% 
                        summarise(sum(`2023 Jobs...4`))),
           as.numeric(historical_digitalization_plot %>% 
                        filter(HD2022== 1) %>% 
                        summarise(sum(`2023 Jobs...4`))),
           as.numeric(historical_digitalization_plot %>% 
                        filter(HD2023 == 1)%>% 
                        summarise(sum(`2023 Jobs...4`))),
           as.numeric(historical_digitalization_plot %>% 
                        filter(HD2024 == 1) %>% 
                        summarise(sum(`2023 Jobs...4`))))
)
write_csv(HDP, 
          'HDCEM jobs over time.csv')
HDP <- HDP%>% 
  filter(year>=2021) %>% 
  ggplot()+
  geom_bar(aes(x = factor(year,
                          levels = c(2019:2024)),
               y = jobs,
               fill = year),
           stat = 'identity',
           position= 'stack')+
  
  scale_fill_manual(
    values = c(  
      'grey',
      'grey',
      "#287EC9",
      'grey')
  )+
  scale_y_continuous(labels = scales::comma,
                     breaks = c(0,
                                5000000,
                                7500000,
                                10000000,
                                12500000),
                     limits = c(0, 15000000),
                     expand = c(0,0))+
  labs(
    title = "Highly digital CEM jobs by digitalization score year",
    x = "",
    fill = '',
    y = "",
    caption = "Note: Occupational groups that have no highly digital CEM workers are excluded.\nSource: Brookings analysis of Lightcast and O*Net data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'none',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank()
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("HDCEM jobs over time.pdf",
       HDP, width = 9, height = 6)
ggsave("HDCEM jobs over time.jpg", 
       HDP, width = 9, height = 6) 


HDP2 <- dplyr::tibble(
  year = as.character(c(2019:2024)),
  'All other CEM jobs' = c(as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2019 == 1) %>% 
                                        summarise(sum(`2023 Jobs...4`)))- as.numeric(historical_digitalization_plot %>% 
                                                                                       filter(SOC=='11-1021') %>% 
                                                                                       summarise(sum(`2023 Jobs...4`))),
                           as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2020 == 1) %>% 
                                        summarise(sum(`2023 Jobs...4`)))- as.numeric(historical_digitalization_plot %>% 
                                                                                       filter(SOC=='11-1021') %>% 
                                                                                       summarise(sum(`2023 Jobs...4`))),
                           as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2021 == 1)%>% 
                                        summarise(sum(`2023 Jobs...4`)))- as.numeric(historical_digitalization_plot %>% 
                                                                                       filter(SOC=='11-1021') %>% 
                                                                                       summarise(sum(`2023 Jobs...4`))),
                           as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2022== 1) %>% 
                                        summarise(sum(`2023 Jobs...4`)))- as.numeric(historical_digitalization_plot %>% 
                                                                                       filter(SOC=='11-1021') %>% 
                                                                                       summarise(sum(`2023 Jobs...4`))),
                           as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2023 == 1)%>% 
                                        summarise(sum(`2023 Jobs...4`)))- as.numeric(historical_digitalization_plot %>% 
                                                                                       filter(SOC=='11-1021') %>% 
                                                                                       summarise(sum(`2023 Jobs...4`))),
                           as.numeric(historical_digitalization_plot %>% 
                                        filter(HD2024 == 1) %>% 
                                        summarise(sum(`2023 Jobs...4`)))),
  'General and Operations Managers' =  c(as.numeric(historical_digitalization_plot %>% 
                                                      filter(SOC=='11-1021') %>% 
                                                      summarise(sum(`2023 Jobs...4`))),
                                         as.numeric(historical_digitalization_plot %>% 
                                                      filter(SOC=='11-1021') %>% 
                                                      summarise(sum(`2023 Jobs...4`))),
                                         as.numeric(historical_digitalization_plot %>% 
                                                      filter(SOC=='11-1021') %>% 
                                                      summarise(sum(`2023 Jobs...4`))),
                                         as.numeric(historical_digitalization_plot %>% 
                                                      filter(SOC=='11-1021') %>% 
                                                      summarise(sum(`2023 Jobs...4`))),
                                         as.numeric(historical_digitalization_plot %>% 
                                                      filter(SOC=='11-1021') %>% 
                                                      summarise(sum(`2023 Jobs...4`))),
                                         0)
  
)
HDP2 <- HDP2 %>% 
  pivot_longer(-year, 
               names_to = 'type',
               values_to = 'jobs')

write_csv(HDP2, 
          'HDCEM jobs over time stacked.csv')

HDP2 <- HDP2 %>% 
  filter(year>=2021) %>% 
  ggplot()+
  geom_bar(aes(x = factor(year,
                          levels = c(2019:2024)),
               y = jobs,
               fill = factor(type,
                             levels =c(
                               'General and Operations Managers',
                               'All other CEM jobs'))),
           stat = 'identity',
           position= 'stack')+
  
  scale_fill_manual(
    values = c(  
      
      "#287EC9",
      'grey')
  )+
  scale_y_continuous(labels = scales::comma,
                     breaks = c(0,
                                5000000,
                                7500000,
                                10000000,
                                12500000),
                     limits = c(0, 15000000),
                     expand = c(0,0))+
  labs(
    title = "Highly digital CEM jobs by digitalization score year",
    x = "",
    fill = 'Type',
    y = "",
    caption = "Note: Occupational groups that have no highly digital CEM workers are excluded.\nSource: Brookings analysis of Lightcast and O*Net data"
  )+
  guides(fill = guide_legend(reverse = TRUE))+
  theme_classic()+
  theme(
    legend.position = 'bottom',
    #axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.x=element_blank()
    #axis.title.y=element_blank(),
    # axis.ticks.x=element_blank(),
    #axis.text.x = element_text(angle = 70, vjust = .95,
    # hjust = .95)
  )
ggsave("HDCEM jobs over time stacked.pdf",
       HDP2, width = 9, height = 6)
ggsave("HDCEM jobs over time stacked.jpg", 
       HDP2, width = 9, height = 6) 

