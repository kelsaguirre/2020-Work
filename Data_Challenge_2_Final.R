---
title: "Data Challenge 2"
runtime: shiny
ouput: html_document
---
[github link] (https://github.com/kelseyaguirre/Data_Challenge_2)

[Shiny.io link] (https://kels.shinyapps.io/data_challenge_2/)

## Visualizing RNAseq Data from My Lab

My lab, the Piskounova Lab at Weill Cornell Medicine, studies metastasis in melanoma. Here I am telling a story by displaying and comparing data from our RNAsequencing results from two different types of melanoma, m405 and m481. These two melanomas are highly aggressive metastasizers and we are still using them in our mouse models today. RNAsequencing data shows counts for specific genes, and each of the data sets that I’m working with have over 30,000 genes displaying a count greater than zero. RNAsequencing data is known for being difficult to analyze and interpret accurately, and because I'm relatively new to coding and analysis of large datasets I'm trying my best to keep these displays as simples as possible, while also representing something useful and hopefully interesting!

Calling libraries
```{r echo=FALSE}
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(shiny)
library(reshape2)
```

Set my working directory
```{r echo=FALSE}
setwd("~/Documents/Data_Science_I/Week_4/Data_Challenge_2")
```

Manipulate my data for plot 1, organized by highest met_average counts (average for metastasis gene counts) because the metabolic changes in the metastasis vs subcutaneous tumor are what we're interested in, in my lab.
```{r echo=FALSE}
#telling which file to read
m405_all <- read.csv(file = '405_process_clean.csv') %>%
  #selecting the three columns I want here
  select('X', 'SQ_Average', 'met_Average') %>%
  #sorting by the highest counts in met_Average
   arrange(desc(met_Average)) %>%
  ##git rid of the three massive counts
  filter(SQ_Average<=6000) %>% 
  #selecting the top 250 for a managable plot
  top_n(250)
#check what I did
head(m405_all)
```

### Plot 1

Create a sliding bar to avoid over plotting
##needs work

Creating the plot:
In this plot I'm comparing the average gene counts for the Subcutaneous tumor on the x axis vs the average gene counts for the Metastasis on the y axis
```{r echo=FALSE}
##define x axis
x <- list(
  title = "Subcutaneous Average Counts per Gene"
)
#define y axis
y <- list(
  title = "Metastasis Average Counts per Gene"
)
#create the plot, call the data wrangled above
plot_ly(data = m405_all,
        #define x and y axis, and jitter to help with overlap at the bottom
        x = ~jitter(SQ_Average, factor = 10000),
        y = ~jitter(met_Average, factor = 10000),
        #make it a dot scatter plot
        type = 'scatter',
        mode = 'markers',
        #show gene name when hovering over dot
        hoverinfo = 'name',
        name = ~X,
        #to help view overlap
        opacity = 0.75,
        #take away legend
        showlegend = FALSE) %>% 
  #label x and y axis and the title
  layout(xaxis = x,
         yaxis = y,
         title = "M405 Subcutaneous vs Metastasis Gene Counts")
```
Plot 1 Summary: While plot 1 is helpful for seeing outliers, I also find it helpful in how similar the top hits for both the subcutaneous tumor genes, and the metastasis genes are. From work in the lab, I assumed that the high counts in the metastasis would differ fro the high counts in the subcutaneous tumor, but here you can see an almost linear correlation. This plot is also helpful for seeing the slight differences between the subcutaneous tumor and metastasis gene counts.


### Plot 2

Manipulating my data for the second plot:

After plotting the first plot, I noticed that top hits from my first plot aren't really helpful for the research in my lab. For figure 2 I'm going to only select genes that are directly related to cells protective response to increase in ROS (reactive oxygen species) from their environment. Cells go through a lot of bottlenecks when metastasizing, and one thing they need to be equiped to handle is the increase number of ROS in the bloodstream, since that's where they travel to metastasize in other parts of the body. Here I'm going to select for genes associated in those pathways, there are many more I can add to this at a later date, but for now I'm going to focus on metabolic genes and transcription factors we focus on for my project.

```{r echo=FALSE}
#telling which file to read
ROS <- read.csv(file = '405_process_clean.csv') %>%
#selecting the three columns I want here
  select('X', 'SQ_Average', 'met_Average') %>%
#filtering for genes that usually change when ROS increases
  filter(str_detect(X, "AKT|NAD|NMN|NAM|NRF2|BACH|GYS|GLU|KG"))
#see my list
ROS
```

Plot 2:
```{r echo=FALSE}
ROS_plot <- data.frame(type=rep(1:2, each= 50), 
                       subtype=rep(c("SQ_Average","met_Average"), each= 50),
                       value= rnorm(4000,0,1))
ROS_plot <- ggplot(ROS_plot,
                   aes(x = value,
                       fill = subtype)) +
  geom_histogram(position = "identity", bins = 40, alpha = 0.5) +
  ggtitle("M405 Subcutaneous vs Metastasis 
  ROS Genes Counts") +
  xlab("Value") +
  ylab("ROS Gene Counts")+
  theme(axis.title.x = element_text(size = 8, vjust=-1),
        axis.title.y = element_text(size = 8, vjust=-0.35),
        plot.title = element_text(size = 10, hjust=-0.15, vjust = 1),
        legend.text = element_text(size = 8))+
  scale_fill_discrete(name = "Type of Organ")
ggplotly(ROS_plot)
```

Plot 2 Summary: Plot two again is a surprise to me. From reasearch in our lab, I assumed that this plot would have larger differences when comparing genes related to the ROS response pathways. Here you can see only minor differences in the genes counts for the subcutaneous tumor averages and the metastasis averages, although the average in the metastesis is higher. Again, I'm a bit in over my head for this data analysis, but at least I'm getting practice with plotly plots!

Help: I can't seem to get the labels in the lengend to work I tried to add (labels = c("Subcutaneous Averages", "Metastasis Averages") after the name in scale fill discrete. Could you please advise for next time?

### Plot 3

Manipulating my data for the third plot.

Now I'm going to work with our M481 data! This dataset doesn't have averages, and doesn't have the zeros removed with the rowSum function like the M405 data, so it's a bit messier. I tried to perform the changes to the data so I could compare the two melanomas, but I was unsuccessful. Here I'm going to look at two individual subcutaneous tumor counts and two liver metastasis counts looking for just genes that conatin 'NAD' since our project is on NADK.

```{r echo=FALSE}
m481_all <- read.csv(file = 'M481_Organs.csv') %>% 
  select('X', 'SQ_1', 'SQ_2', 'Liver_1', 'Liver_2') %>%
  filter(str_detect(X, "NAD"))

m481_reshape <- melt(m481_all,id.vars='X', 
                     measure.vars=c('SQ_1', 'SQ_2', 'Liver_1', 'Liver_2'))
```

Plot 3:
```{r echo=FALSE}
m481_plot <- ggplot(m481_reshape) +
  geom_boxplot(aes(x=variable,
                   y=value)) +
  ggtitle("M481 Subcutaneous Tumor and Liver
     Metastasis ROS Gene Counts")+
  xlab("Sample Type") +
  ylab("ROS Gene Counts") +
  theme(axis.text.x = element_text(angle=45),
        plot.title = element_text(vjust = 1))

ggplotly(m481_plot)
```

Plot 3 Summary: My third plot probably has the greatest differences out of my three plots, this may be because my M481 data that I'm using isn't averaged for all the samples, I have each individual sample. For this plot I would compare SQ_1 (subcutaneous tumor sample 1) to Liver_1 (liver metastasis sample 1) and the same for SQ_2 and Liver_2. In my lab we often compare the Protein, DNA and RNA levels of Subcutaneous tumor samples to liver metastasis since that's often where our tumors first metastasize, so I decdied to do the same here!

Help: I tried to change the decimals for the y-axis but wasn't able to with sacle_y_continous(scales::number_format(accuracy=0.01)) command, because I got "Error: Discrete value supplied to continuous scale" please advise for next time.


### Overall Summary:
While my three plots here aren't as interactive as I initially hoped, this assignment helped me see firsthand why RNAsequencing data is so difficult to analyze. I've seen that working with raw, non-averaged data will produce greater differences and I now know that I will need more time to accurately compare the m405 and the m481 sequencing data. Hopefully I can come back to this data later in the semester when I am better with interactive apps!