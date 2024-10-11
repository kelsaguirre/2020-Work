##github link
##[text](https://github.com/kelseyaguirre/Lab_1.git)

##call libraries needed (I have some extras here)
library(dplyr)
library(mdsr)
library(ggplot2)
library(babynames)
library(Hmisc)

##select the data for the deadest names as of jan 1st 2014
deadest_names <- make_babynames_dist()

##wrangle the data to get what I need to re-create this plot
lab_1_data <- deadest_names %>%
  filter(year>= 1900) %>% ##clarify the years I want
  group_by(sex, name) %>% ##establish groups
  ##establish columns for my dataset
  summarise(total_name = sum(n), ##calculate total number of people for each name
            ttl_alive_today = sum(est_alive_today), ##calcuate sum for est_alive_prob for each name
            #calculate % of people dead today (not in % form yet)
            most_dead = ((total_name-ttl_alive_today)/total_name)) %>%
  filter(total_name>100000) %>% ##only use the most common names
  arrange(desc(most_dead)) %>% ##order the names by % dead today
  group_by(sex) %>% ## group them by sex
  top_n(10) ##select the top 10 'deadest' names

##make plot from data wrangled above
lab_1_plot <- ggplot(lab_1_data, ##pull from wrangled data
                     ##define the x and y and establsh color changes between each sex
                 aes(x = reorder(name, most_dead), y = most_dead, fill = sex))

lab_1_plot <- lab_1_plot + ## add to the previous
  ##make the bar plot
  ##make the lengths of the bars correlate with the values of the data
  geom_bar(stat = 'identity') +
  ##change the y axis, and round he most_dead calc and set it as a percent with one decimal
  ##add label with percent dead
  geom_text(aes(y = most_dead + 0.25), label = paste(round(lab_1_data$most_dead *100, 1))) +
  ##flip the bars to be on the y axis
  coord_flip()

##add and remove the labeles to be like the plot
lab_1_plot <- lab_1_plot +
  ##get rid of x and y axis labels
  xlab(NULL) +
  ylab(NULL) +
  ## set the title and subtitle
  ggtitle('Deadest Names', subtitle = 'Estimated percentage of Americans with a given name born since 
  1900 who were dead as of Jan. 1, 2014') +
  ##changing themes
  theme(plot.title = element_text(face = "bold"), ##bold title
        plot.title.position = "plot", ##link subtitle and title start location
        axis.ticks = element_blank(), ##get rid of ticks
        axis.text.x = element_blank(), ##get rid of numbers on the x axis
        legend.position = "none", ##get rid of key
        plot.background = element_rect(fill = '#DCDCDC'), ##change background color to grey
        panel.background = element_rect(fill = '#DCDCDC'), ##change backgroud color of grid to match
        panel.grid = element_line(linetype = 'blank')) ##get rid of grid behind bars

##change the colors for the bars to match the graph
lab_1_plot <- lab_1_plot +
  scale_fill_manual(values= c('#e1ad01', '#1C86EE'))

###display the plot  
lab_1_plot


##This plot shows the most common names for people estimated to be dead as of January 1st 2014. The data came from the SSA and starts with people born from 1900, and is filtered to inclued the most common 'dead' names for 10 females and 10 males. On the far right, you can see the estimated percent of people with that name who are thought to be dead as of 2014.

