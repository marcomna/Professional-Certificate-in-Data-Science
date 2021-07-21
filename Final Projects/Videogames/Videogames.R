
####################REQUIRED PACKAGES########################

# if not installed, the following libraries will download

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(viridis)) install.packages("viridis", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggmosaic)) install.packages("ggmosaic", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(stargazer)) install.packages("stargazer", repos = "http://cran.us.r-project.org")

# these are the used packages throughout this report

library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(ggrepel)
library(viridis)
library(RColorBrewer)
library(lubridate)
library(ggmosaic)
library(stargazer)

######################EXPLORATORY DATA ANALYSIS##############################

# The data set can be found in the following path:

library(readr)

videogames <- read_csv("videogames.csv")

head(videogames)

# some entries have a "N/A" string, so lets replace it

videogames[videogames == "N/A"] <- NA

# there are 16 columns

names(videogames)

# and there are 16719 observations

dim(videogames)

length(unique(videogames$Name))

# shows the frecuency of games per platform

table(videogames$Platform)

# plots thegames per platform in descending order, some visual adjustments were made through the theme functions

videogames %>% 
  ggplot(aes(x = reorder(Platform, Platform, function(x) - length(x)))) +
  geom_bar(fill = "lightgoldenrod3") +
  labs(title = "Figure 2.1. Published Games by Platform, \n 1980-2017", 
       y = "Number of games published", 
       x = "Platform", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1), plot.caption = element_text(hjust= 0))

# shows the frecuency of games per genre

table(videogames$Genre)

# plots the games per genre, again, with some estetic adjusments

videogames %>% 
  ggplot(aes(x = reorder(Genre, Genre, function(x) - length(x)))) +
  geom_bar(fill = "#00BCD4") +
  labs(title = "Figure 2.2. Published Games by Genre, \n 1980-2017", 
       y = "Number of games published", 
       x = "Genre", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1), plot.caption = element_text(hjust= 0))


length(unique(videogames$Publisher))

# filters for publishers with more than 50 games; also recodes some long names that would be harden the obstructed the plot visualization

videogames %>%
  group_by(Publisher) %>% 
  filter(n() >= 50 & !is.na(Publisher)) %>%
  mutate(Publisher = recode(Publisher,
                            'Electronic Arts' = "EA",
                            'Namco Bandai Games' = "Namco",
                            'Konami Digital Entertainment' = "Konami",
                            'Sony Computer Entertainment' = "Sony",
                            'Take-Two Interactive' = "Take-Two",
                            'Warner Bros. Interactive Entertainment' = "Warner Bros.",
                            'Disney Interactive Studios' = "Disney",
                            'Eidos Interactive' = "Eidos",
                            'Midway Games' = "Midway",
                            'Microsoft Game Studios' = "Microsoft",
                            'Acclaim Entertainment' = "Acclaim",
                            'Vivendi Games' = "Vivendi",
                            'Nippon Ichi Software' = "Nippon Ichi",
                            'Zoo Digital Publishing' = "Zoo",
                            'Majesco Entertainment' = "Majesco",
                            'Rising Star Games' = "Rising Star",
                            'Bethesda Softworks' = "Bethesda",
                            'Crave Entertainment' = "Crave",
                            'Virgin Interactive' = "Virgin",
                            'Focus Home Interactive' = "Focus Home",
                            'Ignition Entertainment' = "Ignition",
                            'Marvelous Interactive' = "Marvelous",
                            'Empire Interactive' = "Empire",
                            'Kadokawa Shoten' = "Kadokawa")) %>% 
  ggplot(aes(x = reorder(Publisher, Publisher, function(x) - length(x)))) +
  geom_bar(fill = "#AD1457") +
  scale_y_continuous(breaks = c(seq(0, 1500, 250))) +
  labs(title = "Figure 2.3. Published Games by Publisher, \n 1980-2017", 
       y = "Number of games published", 
       x = "Publisher", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1), plot.caption = element_text(hjust= 0))

# plots games by platform, while the argument fill colors by genre

videogames %>% 
  ggplot(aes(x = reorder(Platform, Platform, function(x) - length(x)), fill = Genre)) +
  geom_bar(color = "black", size = 0.1) +
  labs(title = "Figure 2.4. Published Games by Platform and Genre, \n 1980-2017", 
       y = "Number of games published", 
       x = "Platform", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1), plot.caption = element_text(hjust= 0))

# plots games by publisher with more than 50 games; recodes long names and colors by genre

videogames %>%
  group_by(Publisher) %>% 
  filter(n() >= 50 & !is.na(Publisher)) %>%
  mutate(Publisher = recode(Publisher,
                            'Electronic Arts' = "EA",
                            'Namco Bandai Games' = "Namco",
                            'Konami Digital Entertainment' = "Konami",
                            'Sony Computer Entertainment' = "Sony",
                            'Take-Two Interactive' = "Take-Two",
                            'Warner Bros. Interactive Entertainment' = "Warner Bros.",
                            'Disney Interactive Studios' = "Disney",
                            'Eidos Interactive' = "Eidos",
                            'Midway Games' = "Midway",
                            'Microsoft Game Studios' = "Microsoft",
                            'Acclaim Entertainment' = "Acclaim",
                            'Vivendi Games' = "Vivendi",
                            'Nippon Ichi Software' = "Nippon Ichi",
                            'Zoo Digital Publishing' = "Zoo",
                            'Majesco Entertainment' = "Majesco",
                            'Rising Star Games' = "Rising Star",
                            'Bethesda Softworks' = "Bethesda",
                            'Crave Entertainment' = "Crave",
                            'Virgin Interactive' = "Virgin",
                            'Focus Home Interactive' = "Focus Home",
                            'Ignition Entertainment' = "Ignition",
                            'Marvelous Interactive' = "Marvelous",
                            'Empire Interactive' = "Empire",
                            'Kadokawa Shoten' = "Kadokawa")) %>% 
  ggplot(aes(x = reorder(Publisher, Publisher, function(x) - length(x)), fill = Genre)) +
  geom_bar(color = "black", size=0.1) +
  scale_y_continuous(breaks = c(seq(0, 1500, 250))) +
  labs(title = "Figure 2.5. Published Games by Publisher and Genre, \n 1980-2017", 
       y = "Number of games published", 
       x = "Publisher", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 7.5), plot.caption = element_text(hjust= 0))

# for the next graphs and analysis, the data set was modified as follows

videogames1 <- videogames %>%
  #drops NAs
  drop_na() %>%
  #drops columns that won't be used
  select(-Critic_Count, -User_Count) %>%
  #renames "Years"
  rename(Year = Year_of_Release) %>%
  # coerces variables as correct objects
  mutate(Platform = as.factor(Platform),
         Genre = as.factor(Genre),
         Publisher = as.factor(Publisher),
         Developer = as.factor(Developer),
         Rating = as.factor(Rating),
         User_Score = as.numeric(User_Score) * 10)

  #filters blank ratings and developers
videogames1 <- videogames1 %>%
  filter(Rating != "" & Developer != "")

videogames1 <- videogames1 %>% 
  # correctly renames Sales
  pivot_longer(cols = ends_with("Sales"),
               names_to = "Region", values_to = "Units_Sold")

  # renames NA region
videogames1$Region = gsub("NA_Sales", "North America", videogames1$Region)

  # consolidates Sales
videogames1$Region = gsub("_Sales", "", videogames1$Region)

# plots units sold by region

videogames1 %>% 
  filter(Region != "Global") %>% 
  ggplot(aes(reorder(Region, -Units_Sold), y = Units_Sold)) +
  geom_bar(aes(fill = Region), stat = "identity") +
  labs(x = "Global Region", 
       y = "Units Sold (in millions)", 
       title = "Figure 2.6. Units Sold by Region (in millions), 1980-2016",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  theme(plot.title = element_text(hjust = 1), plot.caption = element_text(hjust= 0))

# uses the package ggmosaic; where the two axis are Genre and Region, colors by Genre and weights by Units Sold

videogames1 %>% 
  filter(Region != "Global") %>% 
  ggplot() +
  geom_mosaic(aes(product(Genre, Region),
                  fill = Genre, weight = Units_Sold)) +
  labs(x = "Region", y = "Genre", 
       title = "Figure 2.7. Composition of Regional Sales by Genre, \n 1980-2017",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))


# plots the proportion of units sold by genre and region

videogames1 %>%
  filter(Region != "Global") %>%
  ggplot(aes(x = Genre, y = Units_Sold)) +
  geom_bar(aes(fill = Region), stat = "identity", position = "fill") +
  ylab("Proportion of units sold") +
  coord_flip() +
  labs(title = "Figure 2.8. Proportion of Units Sold by Genre and Region, \n 1980, 2017", 
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))


###################LINEAR MODELS#############################


# simple linear model

fit1 <- videogames1 %>% 
  lm(User_Score ~ Critic_Score, data =.)

# uses package stargazer to automatically compile a beautiful regression table (note the required instructions to the r code above "results = 'asis', echo = FALSE, message=FALSE, warning=FALSE")

stargazer(fit1,  
          title= "Table 3.1. Linear model, User Score vs. Critic Score",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "User Score",
          covariate.labels = "Critic Score",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)

fit2 <- videogames1 %>% 
  lm(Critic_Score ~ User_Score, data =.)

stargazer(fit2,  
          title= "Table 3.2. Linear model, Critic Score vs. User Score",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "Critic Score",
          covariate.labels = "User Score",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)

# plot user ratings against critic ratings faceting by Genre (uses the global sales)

videogames1 %>%
  filter(Region == "Global") %>% 
  ggplot(aes(x = User_Score, y = Critic_Score)) + 
  geom_point(size = 0.5) + 
  geom_smooth(aes(color = Genre), method = lm, se = FALSE) + 
  facet_wrap(~ Genre) +
  xlab("User Score") + 
  ylab("Critic Score") +
  theme_minimal() +
  labs(title = "Figure 3.1. Linear models User Scores vs. Critic Score, \n by Genre",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))

# recodes long names; filters publishers with less than 500 games and facets by Publisher

videogames1 %>%
  group_by(Publisher) %>% 
  mutate(Publisher = recode(Publisher,
                            'Electronic Arts' = "EA",
                            'Namco Bandai Games' = "Namco",
                            'Take-Two Interactive' = "Take-Two",
                            'Konami Digital Entertainment' = "Konami",
                            'Sony Computer Entertainment' = "Sony",
                            'Warner Bros. Interactive Entertainment' = "Warner Bros.",
                            'Eidos Interactive' = "Eidos",
                            'Midway Games' = "Midway",
                            'Microsoft Game Studios' = "Microsoft",
                            'Vivendi Games' = "Vivendi")) %>% 
  filter(n() >= 500 & Region == "Global" ) %>% 
  ggplot(aes(x = User_Score, y = Critic_Score)) + 
  geom_point(size = 0.5) + 
  geom_smooth(aes(color = Genre), method = lm, se = FALSE) + 
  facet_wrap(~ Publisher) +
  xlab("User Score") + 
  ylab("Critic Score") +
  theme_minimal() +
  labs(title = "Figure 3.2. Linear models User Scores vs. Critic Score, \n by Genre and Publisher",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))

# plots user score against critic scores and facets by Platform

videogames1 %>%
  filter(Region == "Global") %>% 
  ggplot(aes(x = User_Score, y = Critic_Score)) + 
  geom_point(size = 0.5) + 
  geom_smooth(aes(color = Genre), method = lm, se = FALSE) + 
  facet_wrap(~ Platform) +
  xlab("User Score") + 
  ylab("Critic Score") +
  theme_minimal() +
  labs(title = "Figure 3.3. Linear models User Scores vs. Critic Score, \n by Genre and Platform",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))

fit3 <- videogames1 %>% 
  lm(Units_Sold ~ User_Score, data =.)

stargazer(fit3,  
          title= "Table 3.3. Linear model, Units Sold vs. User Score",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "Units Sold",
          covariate.labels = "User Score",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)

fit4 <- videogames1 %>% 
  lm(User_Score ~ Units_Sold, data =.)

stargazer(fit4,  
          title= "Table 3.4. Linear model, User Score vs. Units Sold",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "User Score",
          covariate.labels = "Units Sold",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)

fit5 <- videogames1 %>% 
  lm(Critic_Score ~ Units_Sold, data =.)

stargazer(fit5,  
          title= "Table 3.5. Linear model, Critic Score vs. Units Sold",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "Critic Score",
          covariate.labels = "Units Sold",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)

fit6 <- videogames1 %>% 
  lm(Units_Sold ~ Critic_Score, data =.)

stargazer(fit6,  
          title= "Table 3.6. Linear model, Units Sold vs. Critic Score",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "Units Sold",
          covariate.labels = "Critic Score",
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)


fit7 <- videogames1 %>% 
  lm(Units_Sold ~ Critic_Score + User_Score, data =.)

stargazer(fit7,  
          title= "Table 3.7. Linear model, Units Sold vs. Critic Score + User Score",
          column.labels = c(""),
          header = FALSE,
          font.size = "footnotesize",
          dep.var.caption = "",
          dep.var.labels = "Units Sold",
          covariate.labels = c("Critic Score", "User Score"),
          notes.label = "Significance Value (P-value)",
          type="text",
          table.placement = "!htbp",
          single.row = T)


# plots both scores against units sold and facets by Genre

videogames1 %>%
  filter(Region == "Global") %>%
  pivot_longer(cols = c(User_Score, Critic_Score), names_to = "Rating_Type",
               values_to = "Score") %>%
  ggplot(aes(x = Score, y = Units_Sold)) + 
  geom_smooth(aes(color = Rating_Type), method = "lm", se = FALSE) +
  xlab("User and Critic Scores") +
  ylab("Units Sold") +
  labs(title = "Figure 3.4. Linear model, User/Critic Scores vs. Units Sold, \n by Genre",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  facet_wrap(~ Genre) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))

# plots both scores against units sold and facets by Platform

videogames1 %>%
  filter(Region == "Global") %>%
  pivot_longer(cols = c(User_Score, Critic_Score), names_to = "Rating_Type",
               values_to = "Score") %>%
  ggplot(aes(x = Score, y = Units_Sold)) + 
  geom_smooth(aes(color = Rating_Type), method = "lm", se = FALSE) +
  xlab("User and Critic Scores") +
  ylab("Units Sold") +
  labs(title = "Figure 3.5. Linear model, User/Critic Scores vs. Units Sold \n by Platform",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  facet_wrap(~ Platform) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))


# plots both scores against units sold and facets by Publisher

videogames1 %>%
  group_by(Publisher) %>%
  mutate(Publisher = recode(Publisher,
                            'Electronic Arts' = "EA",
                            'Namco Bandai Games' = "Namco",
                            'Take-Two Interactive' = "Take-Two",
                            'Konami Digital Entertainment' = "Konami",
                            'Sony Computer Entertainment' = "Sony",
                            'Warner Bros. Interactive Entertainment' = "Warner Bros.",
                            'Eidos Interactive' = "Eidos",
                            'Midway Games' = "Midway",
                            'Microsoft Game Studios' = "Microsoft",
                            'Vivendi Games' = "Vivendi")) %>% 
  filter(n() >= 500 & Region == "Global") %>%
  pivot_longer(cols = c(User_Score, Critic_Score), names_to = "Rating_Type",
               values_to = "Score") %>%
  ggplot(aes(x = Score, y = Units_Sold)) + 
  geom_smooth(aes(color = Rating_Type), method = "lm", se = FALSE) +
  xlab("User and Critic Scores") +
  ylab("Units Sold") +
  labs(title = "Figure 3.6. Linear model, User/Critic Scores vs. Units Sold \n by Publisher",
       caption = "Source: Own elaboration based on the data 'videogames' from kaggle ") +
  facet_wrap(~ Publisher) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust= 0))


