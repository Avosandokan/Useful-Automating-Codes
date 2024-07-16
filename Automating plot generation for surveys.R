#Author: Isabella Mele

# This is a loop to generate any type of plot from survey responses. In this case it works
# with categorical responses (like: Agree/Disagree/somewhat agree, etc: or after asking to rate
# categories (ie. 1 = a lot, 2 = sometimes, 3 = often, etc). But Really it can be applied to any large dataset.

# You Ideally want to have data cleaned and ready with you columns being the questions, 
# and the rows being the responses (whether in number or character they can be categorical or integer)

# The loop saves the column title, and then processes the graphical plotting for each column that you select.
# it will save all of them into a pdf with one format.
# HERE WE MAKE A STACKED BAR CHART

# load library, if other are needed, R usually warns you
library(purrr)
library(tidyverse)

pdf(file = paste0("name_your_file.pdf"))
# creates a pdf file with the given name in your pre-established working directory
# remember to set a directory at the beginning of your script

loop.vector <- names(your_data)[1:10] 
# selected the column  containg the questions that you want to plot (Here set from 1 to 10 by default)
for (i in loop.vector) {
  #for every element in the list of columns you have selecetd above
 # X <- your_data[,i] You dont need this bit probably
  # 
  your_data_loop <- your_data %>% drop_na(all_of(i))
  # strip all the NA values in your data set and save the clean list as a separate object
  your_plot =ggplot(your_data_loop ,aes(x = .data[[i]], 
                                   #Use .data[[i]] within the aes function to dynamically refer to each column.
                                   # You need to use the .data pronoun from the rlang package to properly refer to each column dynamically within the loop.
                                   fill = your_group_variable, na.rm = TRUE)) + 
    # substitute the group you want to represent in the plot (ex. Country group, or stakeholder type) as this is a stacked barchart
    # from here below you are free to set your own plot as you prefer
    geom_bar(color = "black") + # here we are making a stacked bar chart
    scale_y_continuous(breaks = seq(0, 50, by = 1)) + #set the y axis to present each response with an interval of 1 (ie: 1,2,3,4..)
    scale_fill_brewer(palette = "Greens") + # change color palette as needed
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) + # wrap x axis label if you have long text, otherwise omit
    labs(x=paste("",i), # here we are copying each level of the responses to the x axis (ie: Agree/Disagree? etc)
         y = "Number of respondents") + # set y axis title as needed
    ggtitle(str_wrap(i)) + # if you have long questions, this will wrap the title so that you see the full question on top of the plot
    theme(plot.title = element_text(size = 10, vjust = 1), # additional margin setting for the title and plot, to adjust as needed
          panel.background = element_blank(), axis.line = element_line(colour = "black")) #add black contour and white background
  print(your_plot) #the print function is needed in order to transfer the plots into the pdf 
}
dev.off() 
# dev.off()  is a wrapper for the pdf function. A temporary file is created by pdf, acting as normal graphical device. After plotting, when dev.off is called, the file is closed
