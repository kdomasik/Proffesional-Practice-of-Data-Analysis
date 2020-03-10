# load data
library(rio)
library(dplyr)
library(knitr)
library(ggplot2)
Child_Data <- import("child_mortality.xlsx", setclass = "tibble")
MyData <- Child_Data[,c(1,120:220)]
MyData <- MyData[complete.cases(MyData),]

# tidy the data

library(tidyr)

MyData <- gather(MyData, key = 'year', value = 'mortality', -country)
head(MyData)

MyData <- MyData %>% 
  mutate(country = factor(country), year = as.numeric(year)) %>%
  arrange(country)

library(knitr)
Countries <- import("Countries.xlsx", setclass = "tibble")
Countries <- filter(Countries, country %in% MyData$country)

MyData <- left_join(MyData, Countries, by = "country")
MyData <- mutate(MyData, country = factor(country), continent = factor(continent))
kable(t(as.matrix(summary(filter(MyData, year == 2005)$continent))), 
      align = c('r', 'r', 'r', 'r', 'r'))
MyData <- MyData[complete.cases(MyData),]

SubData <- filter(MyData, year %in% c(1920, 1960, 1980, 2010) & continent %in%  c("Europe", "Asia"))

ggplot(SubData, aes(x = factor(year), y = mortality, fill = factor(continent))) + 
 geom_violin(scale = "width", draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs( x = "Year", y = "Child Mortality", title = "Child mortality in Europe and Asia across last 100 years",
        subtitle = "Data source: www.gapminder.org")+
  scale_fill_brewer(palette="Dark2")

ggplot(MyData, aes(x = year, y = mortality, color=continent))+
  geom_smooth()+labs(x = "Year", y = "Yearly electircity consumption(kWh)",
                     title = "Europeans use the most electricity on average while Africans the least",
                     subtitle = "Data source: www.gapminder.org") +
  scale_fill_brewer(palette="Dark2")



MyData1 <- filter(MyData, (continent == "Europe" | continent == "Asia" |
                                  continent == "Africa" | continent == "South America" |
                                  continent == "North America") & (year %in% c(1991:2011)))
ggplot(MyData1, aes(x = year, y = mortality, color=continent))+
  geom_smooth()+labs(x = "Year", y = "Child Mortality",
                     title = "Africa remains the only continent with more  than 10% of children dying before age of 5.",
                     subtitle = "Data source: www.gapminder.org")

