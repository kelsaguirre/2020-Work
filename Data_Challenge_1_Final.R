## github link
https://github.com/kelseyaguirre/Data-Challenge-1/blob/master/Data_Challenge_1_Final.R

## loading the required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

## read in the data 
state_milk_production <- read_csv("state_milk_production.csv")
clean_cheese <- read_csv("clean_cheese.csv")
fluid_milk_sales <- read_csv("fluid_milk_sales.csv")
milk_products_facts <- read_csv("milk_products_facts.csv")
milkcow_facts <- read_csv("milkcow_facts.csv")

##my figures
##figure_1

## plot number of milk cows per year and include the change in milk production per cow
g <- ggplot(milkcow_facts, #initalizes ggplot object of price vs cow number
            aes(x = year,
                y = avg_milk_cow_number,
                color = milk_per_cow)) + 
  geom_col() + #make a bar graph
  ylab('Average Milk Cow Number') + ## change y-axis label
  xlab('Year') + ## change x-axis label
  labs(color = "Milk Produced Per Cow") ## label the legend

##adding title and caption and adjusting labels so they can be seen  
f_1 = (g + labs(title = "Average Number of Cows and Milk Per Cow Produced Each Year",
                caption = "Figure 1: Here I'm displaying average number of milk cows per year. You can see with the blue moving from dark to light that eventhough there are less milk cows now, each individual cow is producing more milk. These data support claims in the orignal articles you provided us.") +
         theme(plot.title = element_text(hjust = 0.25),
               plot.caption = element_text(hjust = 0)))
plot(f_1)
##Figure_2

## plot Northeast States change in Milk Production from 1970-2017
state_milk_production_ne <- state_milk_production %>%
  filter(region == 'Northeast') ##filter out just Northeast

ggplot(state_milk_production_ne, ##initalize ggplot object of year vs milk production
       aes(x = year,
           y = milk_produced,
           color = state)) +
  geom_line() + ## make a line plot
  ggtitle('Northeast Changes in Milk Production') + ## title the plot
  ylab('Milk Produced') + ## label the y-axis
  xlab('Year') + ## label the x-axis
  ## adding caption
  labs(caption = "Figure 2: Here I'm showing milk produced over time by individual states in the Northeast. You can see that New York (the highest blue line) and Pennsylvania are producing the most. They have also been consistently producing the more milk than other northeast states ince 1970.") +
  ## adjusting caption
  theme(plot.caption = element_text(hjust = 0))

## Figure_3

##Selecting data from clean_cheese that I want to join
mod_clean_cheese <- clean_cheese %>%
  select('Year', 'Total_American_Chese')

##Selecting data from milk_products_facts that I want to join
mod_milk_products_facts <- milk_products_facts %>%
  select('Year', 'fluid_milk')

## joining both of the modified tables I made, keeping years the same
milk_joined <- inner_join(mod_clean_cheese, mod_milk_products_facts, by = 'Year')
print(milk_joined)

## plotting my joined data
mj <- ggplot(milk_joined, ##initalize ggplot object of cheese vs milk production
             aes(x = Year))+
  ## creating two different y variables so I can show how they inversly correlate
  geom_point(aes(y=Total_American_Chese, color = 'Total_American_Chese')) +
  ##dividing my fluid milk values by 10 so to make the graph easier to look at
  ## really just want to display the negative correlation here
  geom_point(aes(y=fluid_milk / 10, color = 'fluid_milk' )) +
  ## labeling the graph
  ggtitle('Changes in Production of Milk and Cheese') +
  xlab('Year') +
  ylab('Comparing Milk and Cheese Totals') +
  ## changing the legend labels
  scale_fill_discrete(name = 'Type', labels = c('Fluid Milk/10', "Total American Cheese")) +
  ## adding caption
  labs(caption = "Figure 3: Here I'm showing how fluid milk and total american cheese are inversly correlated. I wanted to show this to support the articles that were given to us at the beginning of the assignment. Indeed, as the artiles said, when there's asurplus of fluid milk, the production of american chese goes up") +
  ## adjusting caption
  theme(plot.caption = element_text(hjust = 0))

## executing the figure
plot(mj)


## Figure_4
##flitering the data to inlude just whole milk sales
fluid_milk_sales_whole <- fluid_milk_sales %>%
  filter(milk_type == 'Whole')
h <- ggplot(fluid_milk_sales_whole, ##initalize ggplot object of year vs whole milk sales 
            aes(x = year, 
                y = pounds)) +
  ##plotting an area graph
  geom_area() +
  ## labeling the graph
  xlab('Year') +
  ylab('Pounds') +
  ggtitle('Changes in Whole Milk Sales')+
  ## adding caption
  labs(caption = "Figure 4: Here I'm displaying how whole milk sales have significantlly droppes since the mid 1970s. In addition to my other figures, this graph also supports the claims made in the news articles. What is intersting here is how it isn't as sharp of a decrese as I was expecting.") +
  ## adjusting caption
  theme(plot.caption = element_text(hjust = 0))
##executing the figure
plot(h)


## Figure_5
##filtering for just 2017 data
state_milk_production_year <- state_milk_production %>%
  filter(year == 2017)

##establishing the x and y axis

b <- ggplot(state_milk_production_year, ##initalize ggplot object of region vs milk produced
            aes(x = region,
                y = milk_produced)) +
  ##plotting boxplot
  geom_boxplot() +
  ##labeling the graph
  ggtitle('Amount of Milk Produced in Different Regions in 2017') +
  ##angeling the x-axis labels so they can be read
  theme(axis.text.x = element_text(size = 10, angle = 90)) +
  xlab('Region') +
  ylab('Amount of Milk Produced') +
  ## adding caption
  labs(caption = "Figure 5: Here I'm displaying the differences in milk production based on region. You can telll that the Lake States are producing significantly more milk than any other US regions listed here for 2017.") +
  ## adjusting caption
  theme(plot.caption = element_text(hjust = 0))

plot(b)
