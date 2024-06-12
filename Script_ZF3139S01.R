# Survey analysis and plotting for ZF3139_EU_AQUACULTURE_FWC SO1
# Stepwise visualisation, cleaning and plotting by IM

# LIBRARIES ####
library(tidyverse)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpol)
library(RColorBrewer)
library(ggpubr)
library(waffle)
library(tm)
library(readxl)
library(ggpol)
library(gtable)
library(grid)
library(ggh4x)
library(readr)
library(readxl)
library(plyr)
library(purrr)
library(doBy)
library(dplyr)
library(ggplot2)
library(rlang)
library(scales)
library(ggstats)
library(ggplot2)
library(ggrepel)
# Load survey in raw format
library(readxl)
d_raw <- read_excel("Survey Results Final 7th July.xlsx")


###########################################################
# DATA CLEANING AND PREP####

colnames(d_raw) <- gsub("Country in which you work/country of focus of the case study: ", "QCountry", colnames(d_raw))
# colnames(d_raw) <- gsub("Under what category of stakeholder would you classify yourself? ", "Category", colnames(d_raw))
d_raw$Category <- d_raw$`Under what category of stakeholder would you classify yourself? `
# previous line not working so this last one did instead


# fornat the countries
levels(as.factor(d_raw$QCountry))
# [1] "belgium"                 "Czech Rep."              "Czech Republic"          "czechia"                
# [5] "Danmark"                 "Denmark"                 "Ell;ada"                 "Estonia"                
# [9] "Eλλαδα"                  "greece"                  "Greece"                  "GREECE"                 
# [13] "GREECE, EPIRUS, PREVEZA" "htrh"                    "Hungary"                 "Ireland"                
# [17] "Latvia"                  "m"                       "Magyar"                  "magyarország"           
# [21] "Magyarország"            "Mexico"                  "N Ireland"               "Poland"                 
# [25] "POLAND"                  "Polska"                  "POLSKA"                  "Sverige"                
# [29] "Sweden"                  "The Czech Republic"      "ελλαδα"                  "Ελλαδα"                 
# [33] "ΕΛΛΑΔΑ"                  "Ελλάδα"                  "ΕΛΛΑΣ"

#mutate name of the column to make it easier to process
d_raw$Country  <- NA


# Tidying up and mutating all the country names so that they are homogenised and saving them into a new column to avoid overlap
d_raw$Country <- ifelse(
  d_raw$QCountry %in% c("N Ireland", "Ireland"), "Ireland",
  ifelse(d_raw$QCountry %in% c("POLAND", "Polska", "POLSKA", "Poland"), "Poland",
         ifelse(d_raw$QCountry %in% c("m", "Magyar", "magyarország", "Magyarország", "Hungary"), "Hungary",
                ifelse(d_raw$QCountry %in% c("ελλαδα","Ελλαδα", "ΕΛΛΑΔΑ","Ελλάδα","ΕΛΛΑΣ",
                                             "Ell;ada", "Eλλαδα", "greece","Greece","GREECE","GREECE, EPIRUS, PREVEZA"), "Greece",
                       "Baltic" ))))
# The function ifelse does not handle multiple conditions with vectors in the way you've written. Instead, you should use logical conditions properly 
# combined with | (OR) operators. Thus you need to use %in% and not ==

# check, correctly processed
levels(as.factor(d_raw$Country))
count(d_raw$Country)
# x freq
# 1  Baltic  109
# 2  Greece   75
# 3 Hungary   38
# 4 Ireland   27
# 5  Poland   20

### Doing the same for stakeholder category
levels(as.factor(d_raw$Category))

# [1] "Farmer, Processor, Producer Organisation Representative, Industry Body Representative"                   
# [2] "Government Authority, Competent Authority, Regulatory Agency Representative, State Agency Representative"
# [3] "Non Governmental Organisation (NGO) Representative, Advocacy Group"                                      
# [4] "Other"                                                                                                   
# [5] "Scientist, Scientific Researcher (Academic, Private Sector or Government)" 
count(as.factor(d_raw$Category))
#x
# 1  , freq
# 1   51Farmer
# 2   60Government
# 3    9NGO
# 4   25Other
# 5   37Scientist
# 6   87<NA>

d_raw$`Stakeholder Category` <- ifelse(
  d_raw$Category == "Farmer, Processor, Producer Organisation Representative, Industry Body Representative", "Farmers",
  ifelse( d_raw$Category == "Government Authority, Competent Authority, Regulatory Agency Representative, State Agency Representative", "Government Authority",
          ifelse(d_raw$Category == "Non Governmental Organisation (NGO) Representative, Advocacy Group" , "NGOs",
                 ifelse(d_raw$Category == "Scientist, Scientific Researcher (Academic, Private Sector or Government)", "Scientist",
                        "Other"))))
count(as.factor(d_raw$`Stakeholder Category`)) # ok 
#    x freq
# 1              Farmers   51
# 2 Government Authority   60
# 3                 NGOs    9
# 4                Other   25
# 5            Scientist   37
# 6                 <NA>   87


# split data into two groups based on aquaculture type
d_raw$`Which type of aquaculture would you most associate with? ` <- as.factor(d_raw$`Which type of aquaculture would you most associate with? `)
levels(d_raw$`Which type of aquaculture would you most associate with? `)

# Split the dataframe
split <- split(d_raw, d_raw$`Which type of aquaculture would you most associate with? `)
d_fin <- split[[1]]
d_bivalve <- split[[2]]

#Now we begin cleaning the data. Firstly, lets remove any columns with no data in them, since they are useless to us
d_fin <- d_fin %>%
  select_if(function(x){!all(is.na(x))})

d_bivalve <- d_bivalve %>%
  select_if(function(x){!all(is.na(x))})

# Now because most of the questions are actually not relevant to our plots, for easy visualisation we export them and visually analyse into excel
# we will later import them again with an easier format to delete all the un-necessary columns (personal preference)
# write.csv(d_bivalve, "bivalve.csv")
# write.csv(d_fin, "fin.csv")

# Maybe we can do this in R and it saves me tons of time
d_fin <- d_fin %>% select(-contains(c("Are you aware", "if you answered", "[Comment]", "If you selected Other above, please describe this", "please elaborate")))
d_bivalve <- d_bivalve %>% select(-contains(c("Are you aware", "if you answered", "[Comment]", "If you selected Other above, please describe this", "please elaborate")))

# there are a bunch of annoying columns at the end of the files, lets delete them because we want to apply a loop to make plots
d_bivalve <- d_bivalve[,-c(64:74)]
d_fin <- d_fin[, -c(72:81)]
d_fin <- d_fin[, -c(1:13)]
d_bivalve <- d_bivalve[, -c(1:13)]

# turn everything into a factor?
d_bivalve[sapply(d_bivalve, is.character)] <- lapply(d_bivalve[sapply(d_bivalve, is.character)],as.factor)
d_bivalve[sapply(d_bivalve, is.numeric)] <- lapply(d_bivalve[sapply(d_bivalve, is.numeric)], as.factor)
str(d_bivalve)

d_fin[sapply(d_fin, is.character)] <- lapply(d_fin[sapply(d_fin, is.character)],as.factor)
d_fin[sapply(d_fin, is.numeric)] <- lapply(d_fin[sapply(d_fin, is.numeric)], as.factor)
str(d_fin)





##################################################################################
## QUESTIONS FILTERING
####################################################################
# Not all the questions have to be processed so now we need to filter based on the questions we want to plot

#open excel file
library(readxl)
xl_full <- read_excel("Color formatted IM.xlsx")

# do the same cleaning as above for later merging
colnames(xl_full) <- gsub("Country in which you work/country of focus of the case study: ", "QCountry", colnames(xl_full))
xl_full$Category <- xl_full$`Under what category of stakeholder would you classify yourself? `

# easy out the countries and type of SH
xl_full$Country  <- NA
xl_full$Country <- ifelse(
  xl_full$QCountry %in% c("N Ireland", "Ireland"), "Ireland",
  ifelse(xl_full$QCountry %in% c("POLAND", "Polska", "POLSKA", "Poland"), "Poland",
         ifelse(xl_full$QCountry %in% c("m", "Magyar", "magyarország", "Magyarország", "Hungary"), "Hungary",
                ifelse(xl_full$QCountry %in% c("ελλαδα","Ελλαδα", "ΕΛΛΑΔΑ","Ελλάδα","ΕΛΛΑΣ",
                                             "Ell;ada", "Eλλαδα", "greece","Greece","GREECE","GREECE, EPIRUS, PREVEZA"), "Greece",
                       "Baltic" ))))

# check, correctly processed
levels(as.factor(xl_full$Country))

# repeat
xl_full$`Stakeholder Category` <- ifelse(
  xl_full$Category == "Farmer, Processor, Producer Organisation Representative, Industry Body Representative", "Farmers",
  ifelse( xl_full$Category == "Government Authority, Competent Authority, Regulatory Agency Representative, State Agency Representative", "Government Authority",
          ifelse(xl_full$Category == "Non Governmental Organisation (NGO) Representative, Advocacy Group" , "NGOs",
                 ifelse(xl_full$Category == "Scientist, Scientific Researcher (Academic, Private Sector or Government", "Scientist",
                        "Other"))))
levels(as.factor(xl_full$`Stakeholder Category`)) # ok correct



# Intersect and merge only the same column for both AQua types (ie. keep the questions that we need)
fin_col_ex <- intersect(colnames(d_fin), colnames(xl_full))
biv_col_ex <- intersect(colnames(d_bivalve), colnames(xl_full))

d_fin_clean <- d_fin[,fin_col_ex]
d_bivalve_clean <- d_bivalve[,biv_col_ex]




#########################################################

#PLOTTING ############################
# Let's make a prelimanary plot to see if this is what we need and the type of format that we want to achieve

d_fin %>% drop_na(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `) %>%
# drop all the nas that will produce an unwated bar in the chart
  ggplot(aes(x = `Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `,
                  fill = `Stakeholder Category`, na.rm = TRUE)) + 
 # select the variables and category for each bar
   geom_bar(color = "black") + 
# choose bar chart with black outline
    scale_y_continuous(breaks = seq(0, 60, by = 10),
                       sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin),
                                                    labels = percent,
                                                    name = "Percentage over tot. respondents",  
                                                    breaks = seq(0, 2, by = 0.05))) +
# set a scale for the right brakes on the y axis
    scale_fill_brewer(palette = "Blues") +
  # choose an easy and beautiful colour palette
  scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
# set the visual scale on x axis to avoid overlapping long text
  ggtitle(str_wrap("Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity)")) +
 # wrap overlapping long text of the question on top as a title
   theme(plot.title = element_text(size = 10, vjust = 1),#panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),# adjust the color background to white
        axis.title.x = element_blank()) # get rid of the annoying text at the bottom of the axis




## Now we have the plot that we need. but we need to apply it to our two sets d_fin_clean and d_bivalve_clean
# LOOPING


#npt sure if this is needed
pdf(file = paste0("ZF_31390S01_plots_Fin.pdf"))

loop.vector <- names(d_fin_clean)[2:23] # selected the columns names we need
for (i in loop.vector) {
  #for every element in the column names
  X <- d_fin_clean[,i] 
  d_fin_na_loop <- d_fin_clean %>% drop_na(all_of(i))
  p_try =ggplot(d_fin_na_loop ,aes(x = .data[[i]], #Use .data[[i]] within the aes function to dynamically refer to each column.
                                  # You need to use the .data pronoun from the rlang package to properly refer to each column dynamically within the loop.
                                  fill = `Stakeholder Category`, na.rm = TRUE)) + 
    geom_bar(color = "black") + 
    scale_y_continuous(breaks = seq(0, 50, by = 3)) +
   scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
    labs(x=paste("",i), y = "Number of respondents") +
    ggtitle(str_wrap(i)) +
    theme(plot.title = element_text(size = 10, vjust = 1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  print(p_try)
}
dev.off()




### LETS DO THE SAME FOR BIVALVE
pdf(file = paste0("ZF_31390S01_plots_Bivalve.pdf"))
loop.vector <- names(d_bivalve_clean)[2:21] # selected the columns names we need
for (w in loop.vector) {
  #for every element in the column names
  X <- d_bivalve_clean[,w] 
  d_biv_na_loop <- d_bivalve_clean %>% drop_na(all_of(w))
  p_biv =ggplot(d_biv_na_loop ,aes(x = .data[[w]], #Use .data[[i]] within the aes function to dynamically refer to each column.
                                   # You need to use the .data pronoun from the rlang package to properly refer to each column dynamically within the loop.
                                   fill = `Stakeholder Category`, na.rm = TRUE)) + 
    geom_bar(color = "black") + 
    scale_y_continuous(breaks = seq(0, 50, by = 1)) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
    labs(x=paste("",w), y = "Number of respondents") +
    ggtitle(str_wrap(w)) +
    theme(plot.title = element_text(size = 10, vjust = 1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  print(p_biv)
}
dev.off()





###############################################################################
# SPARE PLOTS
###############################################################################

# because we dont like spaces in column variables lets simplify the text


# WORKS
ggplot(d_fin) +
  aes(x = Country, fill = `Stakeholder Category`, by = 1) +
  geom_bar() +
  geom_text( stat = "prop",
    position = position_stack(.5), size =  3)

# GREAT THIS WORKS!
d_fin %>% drop_na(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `) %>%
  ggplot(aes(x = `Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `,
             fill = `Stakeholder Category`, na.rm = TRUE)) + 
  geom_bar(color = "black") + 
  geom_text(stat = "prop", position = position_stack(0.6), 
            size = 3.5, color = "white", fontface = "bold")+
#  scale_y_continuous(breaks = seq(0, 50, by = 10)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
  ggtitle(str_wrap("Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity)")) +
  theme(plot.title = element_text(size = 10, vjust = 1),#panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),# adjust the color background to white
        axis.title.x = element_blank()) # get rid of the annoying text at the bottom of the axis


#now we need to make the y axis a total percentage of replies 
# exactly what we need!
label_try <- d_fin %>%  drop_na(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `) %>%
group_by(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `)   %>%
   dplyr::summarise(n = n())
label_try$`Stakeholder Category` <- NA
 


 d_fin %>% drop_na(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `) %>%
  ggplot(aes(x = `Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `, 
      fill = `Stakeholder Category`,
      y = after_stat(prop))) +
  geom_bar(stat = "prop", color = "black") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 2, by = 0.05),
                     sec.axis = sec_axis(~., name="Number of Respondents")) +
  scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
  ggtitle(str_wrap("Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity)")) +
  theme(plot.title = element_text(size = 10, vjust = 1),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())  +
  labs(y = "Percentage over total respondents") +
   geom_text(data = label_try, aes(fill = `Stakeholder Category`, x = `Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `,
              y = -0.05, label = n), vjust = .5) 
   
 
 
 
 
 
 
#################################################################################
# LOOPING
###################################################################################
 
 
 # THE FINAL SOLUTION HAS BEEN A PLOT WITH DOUBLE AXIS TO SIMPLIFY VISUALISATION. 
 # This will need to happen for ALL FIN AND ALL BIVALVE .... then... for each case study. A total of 8 reports

 # FIN FINSH
  pdf(file = paste0("ZF_31390S01_yy_Fin.pdf"))
 
 loop.vector <- names(d_fin_clean)[2:23] # selected the columns names we need
 for (i in loop.vector) {
   d_fin_na_loop <- d_fin_clean %>% drop_na(all_of(i))
   p_yy_fin =ggplot(d_fin_na_loop ,aes(x = .data[[i]], #Use .data[[i]] within the aes function to dynamically refer to each column.
                                    # You need to use the .data pronoun from the rlang package to properly refer to each column dynamically within the loop.
                                    fill = `Stakeholder Category`, na.rm = TRUE)) + 
     geom_bar(color = "black") + 
     scale_y_continuous(breaks = seq(0,60, by = 3),
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin_na_loop),
                                                     labels = percent,
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +
     scale_fill_brewer(palette = "Blues") +
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
     labs(x=paste("",i), y = "Number of respondents") +
     ggtitle(str_wrap(i)) +
     theme(plot.title = element_text(size = 10, vjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"),
           axis.title.x = element_blank())
   print(p_yy_fin)
 }
 dev.off()
 
 
 
 # Bivalve
 pdf(file = paste0("ZF_31390S01_yy_Bivalve.pdf"))
 loop.vector <- names(d_bivalve_clean)[2:21] # selected the columns names we need
 for (w in loop.vector) {
   d_biv_na_loop <- d_bivalve_clean %>% drop_na(all_of(w))
   p_yy_biv = ggplot(d_biv_na_loop ,aes(x = .data[[w]], fill = `Stakeholder Category`, na.rm = TRUE)) + 
     geom_bar(color = "black") + 
     scale_y_continuous(breaks = seq(0, 50, by = 1),
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_biv_na_loop),
                                                     labels = percent,
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +
     scale_fill_brewer(palette = "Blues") +
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
     labs(x=paste("",w), y = "Number of respondents") +
     ggtitle(str_wrap(w)) +
     theme(plot.title = element_text(size = 10, vjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"),
           axis.title.x = element_blank())
   print(p_yy_biv)
 }
 dev.off()
 
 
 
###########################################################################
 # BY COUNTRY
 ##########################################################################
 
# FILTER by country it actually works
 d_fin %>% dplyr::filter(Country == "Greece") %>% drop_na(`Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `) %>%
   ggplot(aes(x = `Select to what extent you agree or disagree with the following statement:      Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity). `,
              fill = `Stakeholder Category`, na.rm = TRUE)) + 
  geom_bar(color = "black") + 
   scale_y_continuous(breaks = seq(0, 60, by = 1),
                      sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin),
                                                   labels = percent,
                                                   name = "Percentage over tot. respondents",  
                                                   breaks = seq(0, 1, by = 0.01))) +
    scale_fill_brewer(palette = "Blues") +
   scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
   ggtitle(str_wrap("Fish farming in ponds and wetlands (including lagoons, reservoirs and estuaries) contributes to maintaining or enhancing biodiversity (including through impacts on native species and/or creating habitat diversity)")) +
   theme(plot.title = element_text(size = 10, vjust = 1),#panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black"),# adjust the color background to white
         axis.title.x = element_blank()) # get rid of the annoying text at the bottom of the axis
 
 
 
 #### BY CASE STUDY
 # BIVALVES
 # Ireland
 # Greece
 # Baltic Sea (Denmark, Estonia, Latvia, Sweden, other)
 # 

 # BALTIC
 pdf(file = paste0("ZF_31390S01_Baltic_Bivalve.pdf"))
 loop.vector <- names(d_bivalve_clean)[2:21] # selected the columns names we need
 for (w in loop.vector) {
   d_biv_b <- d_bivalve_clean %>% filter(Country == "Baltic") %>% drop_na(all_of(w))                  
   if (nrow(d_biv_b) == 0) {            
     # Check if there are no rows left after removing NAs
     next  # Skip the loop iteration if there are no rows left
   }
   
   p_yy = ggplot(data = d_biv_b, aes(x = .data[[w]], fill = `Stakeholder Category`)) +  
     geom_bar(color = "black") +  
     scale_y_continuous(breaks = seq(0, 50, by = 1),  
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_biv_b), 
                                                     labels = scales::percent, 
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +  
     scale_fill_brewer(palette = "Blues") +  
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +  
     labs(x = paste("", w), y = "Number of respondents") +  
     ggtitle(str_wrap(w)) +  
     theme(plot.title = element_text(size = 10, vjust = 1),  
           panel.background = element_blank(),  
           axis.line = element_line(colour = "black"),  
           axis.title.x = element_blank())  
   
   print(p_yy)  # Print the plot to the PDF
 }
 
 dev.off() 
 
 
 #greece
 pdf(file = paste0("ZF_31390S01_Greece_Bivalve.pdf"))
 loop.vector <- names(d_bivalve_clean)[2:21] # selected the columns names we need
 for (w in loop.vector) {
   d_biv_g <- d_bivalve_clean %>% filter(Country == "Greece") %>% drop_na(all_of(w))
   p_yy_biv = ggplot(d_biv_g ,aes(x = .data[[w]], fill = `Stakeholder Category`, na.rm = TRUE)) + 
     geom_bar(color = "black") + 
     scale_y_continuous(breaks = seq(0, 50, by = 1),
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_biv_g),
                                                     labels = percent,
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +
     scale_fill_brewer(palette = "Blues") +
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
     labs(x=paste("",w), y = "Number of respondents") +
     ggtitle(str_wrap(w)) +
     theme(plot.title = element_text(size = 10, vjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"),
           axis.title.x = element_blank())
   print(p_yy_biv)
 }
 dev.off()
 
 
 #IRELAND now works with NA remove
 pdf(file = paste0("ZF_31390S01_IE_Bivalve.pdf"))
 loop.vector <- names(d_bivalve_clean)[2:21] # selected the columns names we need
 for (w in loop.vector) {
   d_biv_ie <- d_bivalve_clean %>% filter(Country == "Ireland") %>% drop_na(all_of(w))
   p_yy_biv = ggplot(d_biv_ie ,aes(x = .data[[w]], fill = `Stakeholder Category`, na.rm = TRUE)) + 
     geom_bar(color = "black", ) + 
     scale_y_continuous(breaks = seq(0, 50, by = 1),
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_biv_ie),
                                                     labels = percent,
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +
     scale_fill_brewer(palette = "Blues") +
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
     labs(x=paste("",w), y = "Number of respondents") +
     ggtitle(str_wrap(w)) +
     theme(plot.title = element_text(size = 10, vjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"),
           axis.title.x = element_blank())
   print(p_yy_biv)
 }
 dev.off()
 
 
 
 # FINFISH
 # Poland
 # Hungary
 # Greece
 
 # poland - issues with NA
 pdf(file = paste0("ZF_31390S01_PL_FIN.pdf"))
 loop.vector <- names(d_fin_clean)[2:23] # selected the columns names we need
 for (w in loop.vector) {
   d_fin_pl <- d_fin_clean %>% filter(Country == "Poland") %>% drop_na(all_of(w))                  
   # Remove rows with NA values in the current column
   
   if (nrow(d_fin_pl) == 0) {            
     # Check if there are no rows left after removing NAs
     next  # Skip the loop iteration if there are no rows left
   }
   
   p_yy = ggplot(data = d_fin_pl, aes(x = .data[[w]], fill = `Stakeholder Category`)) +  
     geom_bar(color = "black") +  
     scale_y_continuous(breaks = seq(0, 50, by = 1),  
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin_pl), 
                                                     labels = scales::percent, 
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +  
     scale_fill_brewer(palette = "Blues") +  
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +  
     labs(x = paste("", w), y = "Number of respondents") +  
     ggtitle(str_wrap(w)) +  
     theme(plot.title = element_text(size = 10, vjust = 1),  
           panel.background = element_blank(),  
           axis.line = element_line(colour = "black"),  
           axis.title.x = element_blank())  
   
   print(p_yy)  # Print the plot to the PDF
 }
 
 dev.off() 
 
 
 #hungary 
 
 pdf(file = paste0("ZF_31390S01_HU_FIN.pdf"))
 loop.vector <- names(d_fin_clean)[2:23] # selected the columns names we need
 for (w in loop.vector) {
   d_fin_hu <- d_fin_clean %>% filter(Country == "Hungary") %>% drop_na(all_of(w))
   p_yy = ggplot(d_fin_hu ,aes(x = .data[[w]], fill = `Stakeholder Category`, na.rm = TRUE)) + 
     geom_bar(color = "black") + 
     scale_y_continuous(breaks = seq(0, 50, by = 1),
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin_hu),
                                                     labels = percent,
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +
     scale_fill_brewer(palette = "Blues") +
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
     labs(x=paste("",w), y = "Number of respondents") +
     ggtitle(str_wrap(w)) +
     theme(plot.title = element_text(size = 10, vjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"),
           axis.title.x = element_blank())
   print(p_yy)
 }
 dev.off()
 
 
 #Found the solution to avoid x=0 and still getting rid of NAs 
 
 #GREECE
 pdf(file = paste0("ZF_31390S01_GR_FIN.pdf"))  # Open a PDF device to save the plots
 
 loop.vector <- names(d_fin_clean)[2:23]
 for (w in loop.vector) {
   d_fin_gr <- d_fin_clean %>% 
     filter(Country == "Greece") %>%    
     # Filter the data to keep only rows where Country is "Greece"
     drop_na(all_of(w))                  
   # Remove rows with NA values in the current column
   
   if (nrow(d_fin_gr) == 0) {            
     # Check if there are no rows left after removing NAs
     next  # Skip the loop iteration if there are no rows left
   }
   
   p_yy = ggplot(data = d_fin_gr, aes(x = .data[[w]], fill = `Stakeholder Category`)) +  
     geom_bar(color = "black") +  
     scale_y_continuous(breaks = seq(0, 50, by = 1),  
                        sec.axis = ggplot2::sec_axis(trans = ~./nrow(d_fin_gr), 
                                                     labels = scales::percent, 
                                                     name = "Percentage over tot. respondents",  
                                                     breaks = seq(0, 2, by = 0.05))) +  
     scale_fill_brewer(palette = "Blues") +  
     scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +  
     labs(x = paste("", w), y = "Number of respondents") +  
     ggtitle(str_wrap(w)) +  
     theme(plot.title = element_text(size = 10, vjust = 1),  
           panel.background = element_blank(),  
           axis.line = element_line(colour = "black"),  
           axis.title.x = element_blank())  
   
   print(p_yy)  # Print the plot to the PDF
 }
 
 dev.off()  # Close the PDF device
 
 
####################################################################################
 List <- colnames(dftotal)
 List <- List[-1]
 loop.vector.list <- as.list(loop.vector)
for (j in d_fin_na_loop[loop.vector]) 
 { df <- data.frame(group_by(d_fin_na_loop[loop.vector[j]], 
                             dplyr::summarise(n = n())))
   assign(paste("df", loop.vector.list[j]), df)}
 

pdf(file = paste0("Try.pdf"))
loop.vector <- names(d_fin_clean)[2:23] # selected the columns names we need
for (i in loop.vector) {
  #for every element in the column names
  #X <- d_fin_clean[,i] 
  d_fin_na_loop <- d_fin_clean %>% drop_na(all_of(i))
  p_try =ggplot(d_fin_na_loop ,aes(x = .data[[i]], #Use .data[[i]] within the aes function to dynamically refer to each column.
                                   # You need to use the .data pronoun from the rlang package to properly refer to each column dynamically within the loop.
                                   fill = `Stakeholder Category`, na.rm = TRUE)) + 
    geom_bar(color = "black") + 
    scale_y_continuous(breaks = seq(0, 50, by = 3)) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(labels = ~ str_wrap(as.character(.x), 10)) +
    labs(x=paste("",i), y = "Number of respondents") +
    ggtitle(str_wrap(i)) +
    theme(plot.title = element_text(size = 10, vjust = 1), 
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"),
          axis.title.x = element_blank())
  print(p_try)
}
dev.off()

