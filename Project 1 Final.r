library(tidyverse)

# Importing data sets
covid <- read_csv("https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv")
demographics <- read_csv("D:/Programs/Dropbox/Dropbox (CSU Fullerton)/CSUF Year 4/Fall 2021/CPSC 375 - Data Science and Big Data Analytics/Projects/Project 1/demographics.csv")
GDP <- read_csv("D:/Programs/Dropbox/Dropbox (CSU Fullerton)/CSUF Year 4/Fall 2021/CPSC 375 - Data Science and Big Data Analytics/Projects/Project 1/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_3011433.csv")

# Wrangling demographics data set
demographics <- 
  demographics %>%
  select(`Country Name`, `Country Code`, `Series Code`, YR2015) %>%
  pivot_wider(names_from = `Series Code`, values_from = YR2015) %>%
  mutate(SP.POP.0014.IN = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN,
         SP.POP.1564.IN = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN,
         SP.POP.65UP.IN = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN,
         SP.POP.80UP = SP.POP.80UP.FE + SP.POP.80UP.MA,
         SP.DYN.AMRT = SP.DYN.AMRT.FE + SP.DYN.AMRT.MA) %>%
  select(`Country Code`, SP.DYN.LE00.IN, SP.URB.TOTL, SP.POP.0014.IN,
         SP.POP.1564.IN, SP.POP.65UP.IN, SP.POP.80UP, SP.DYN.AMRT )

# Wrangling GDP data set
GDP <- 
  GDP %>%
  pivot_longer(5:ncol(GDP), names_to = "Year", values_to = "GDP", values_drop_na = TRUE) %>%
  arrange(`Country Code`, desc(Year)) %>%
  group_by(`Country Code`) %>%
  top_n(1, Year) %>%
  select(-`Country Name`, -`Indicator Name`, -`Indicator Code`, -Year)

# Wrangling COVID data set
covid <- 
  covid %>% filter(is.na(Province_State)) %>%
  pivot_longer(starts_with("20"), names_to = "Date", values_to = "Shots", values_drop_na = TRUE) %>%
  select(iso3, Country_Region, Population, Date, Shots) %>%
  filter(Shots > 0, !is.na(Population)) %>%
  mutate(vacRate = Shots / Population)

# Computing days since start of vaccinations
start <- 
  covid %>% 
  group_by(`Country_Region`) %>% 
  summarise(startDate = min(Date))

covid <- covid %>% 
  inner_join(start) %>%
  mutate(daysSinceStart = as.numeric(as.Date(Date) - as.Date(startDate) + 1)) %>%
  select(-Date, -startDate)

# Joining all tables
combined <- covid %>% inner_join(GDP, c(iso3 = 'Country Code'), na.rm = TRUE)
combined <- combined %>% inner_join(demographics, c(iso3 = 'Country Code'), na.rm = TRUE)

# Reordering columns to group predictor variables
combined <- combined[, c(1, 2, 5, 4, 3, 6:14)]
view(combined)

# Creating scatter plot
maxDays <- combined %>% group_by(iso3) %>% summarise(maxDays = max(daysSinceStart))
recent <- combined %>% inner_join(maxDays, c(iso3 = 'iso3', daysSinceStart = 'maxDays'))
ggplot(data = recent) + 
  geom_point(aes(x = daysSinceStart, y = vacRate)) +
  ggtitle("Most Recent Vaccination Rate Since Start of Vaccinations for Each Country") + 
  theme(plot.title = element_text(hjust = 0.5))

# Creating linear models
model1 = lm(formula = vacRate ~ GDP + daysSinceStart, data = combined)
r1 <- summary(model1)$r.squared

model2 <- lm(formula = vacRate ~ GDP + SP.DYN.LE00.IN + daysSinceStart, data = combined)
r2 <- summary(model2)$r.squared

model3 = lm(formula = vacRate ~ SP.POP.0014.IN + daysSinceStart, data = combined)
r3 <- summary(model3)$r.squared

model4 = lm(formula = vacRate ~ SP.POP.1564.IN + daysSinceStart, data = combined)
r4 <- summary(model4)$r.squared

model5 <- lm(formula = vacRate ~ SP.POP.65UP.IN + daysSinceStart, data = combined)
r5 <- summary(model5)$r.squared

combined <- combined %>% mutate(proportion65 = SP.POP.65UP.IN/Population)
model6 = lm(formula = vacRate ~ proportion65 + daysSinceStart, data = combined)
r6 <- summary(model6)$r.squared

# Creating bar plot
modelNames <- as.factor(c('GDP', 'GDP + LE', 'Pop. 0-14', 'Pop. 15-64', 'Pop. 65+', 'Prop. of Pop. 65+'))
rsqrd <- c(r1, r2, r3, r4, r5, r6)
barData <- data.frame(modelNames, rsqrd)
ggplot(data = barData) + 
  geom_bar(mapping = aes(x = modelNames, y = rsqrd), stat = 'identity', fill = 'blue') + 
  ylim(c(0, 1)) + 
  xlab("Model") + 
  ylab('R-Squared Value') + 
  ggtitle("R-Squared Value for Each Model") + 
  theme(plot.title = element_text(hjust = 0.5))