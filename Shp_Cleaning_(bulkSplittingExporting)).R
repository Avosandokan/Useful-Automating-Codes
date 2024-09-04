############ LIBRARIES ###########

library(terra)
library(sf)
library(tidyverse)
library(sp)
library(readr)
library(readxl)
library(plyr)
library(raster)
library(rasterVis)
library(lattice)
# library(rgdal)
######################################################

linestrings <- read_sf("annotation_LineString.shp")
POlygon <- read_sf("annotation_Polygon.shp")

linestrings$fname <- as.factor(linestrings$fname) 

# remove space and turn into a factor
linestrings$title <- gsub('\\s+', '',  linestrings$title)
linestrings$title <- gsub("[[:punct:]]", '',  linestrings$title)

  linestrings$title <- as.factor(linestrings$title) 
levels(linestrings$fname)
levels(linestrings$title)
#"BD_IM_v4" "NW_IM_v4" "SP_IM_v4"

#same for the polygons
POlygon$fname <- as.factor(POlygon$fname)
POlygon$title <- gsub("[[:punct:]]", '',  POlygon$title)
POlygon$title <- gsub('\\s+', '',  POlygon$title)
POlygon$title <- as.factor(POlygon$title)

#divide into type of scenario
nw_lines <- linestrings %>% subset(linestrings$fname == "NW_IM_v4")
bd_lines <- linestrings %>% subset(linestrings$fname == "BD_IM_v4")
sp_lines <- linestrings %>% subset(linestrings$fname == "SP_IM_v4")

nw_polygons <- POlygon %>% subset(POlygon$fname == "NW_IM_v4")
bd_polygons <- POlygon %>% subset(POlygon$fname == "BD_IM_v4")
sp_polygons <- POlygon %>% subset(POlygon$fname == "SP_IM_v4")



# 
# 
# create_new_df <- function (dataframe, vars) {
#   # Creates a new data frame in the global environment based on names of variables in 'vars'
#   split(dataframe, as.list(dataframe[, vars]), drop = TRUE) %>%
#     lapply(function (subset_dataframe) {
#       new_object_name <- paste(as.character(subset_dataframe[1, vars]),
#                                # The double arrowed '<<-' creates a new object in the global environment
#                                command <- paste0(new_object_name, collapse="_"), "<<-subset_dataframe")
#       eval(parse(text=command))
#     }) %>%
#     invisible()
# }
# 
# levels(nw_lines$title)
# variables_nw_lines <- c("BA_Acquacoltura costiera","BA_Acquacoltura costiera BD","BA_difesa costiera", "BA_turismo N@W","BA_turismo N@W BD","CL_BA_Offshore Wind Area BD","CL_BA_Offshore Wind Area SP","CL_BA_Offshore Wind Area_N@W")
# create_new_df(nw_lines, variables_nw_lines)



############### NW ##########################################
# there were some issues with the levels assigned, so let's do this now
nw_lines$title <- as.character(nw_lines$title)
nw_lines$title <- as.factor(nw_lines$title)
levels(nw_lines$title)
unique(nw_lines$title)

# lets create a loop that splits dataframes by level of annotation
for (i in unique(nw_lines$title)) { 
  # for every level in the title column
  command <- paste0(i, "<-subset(nw_lines, title=='", i, "')")
  # create a new dataframe 
  eval(parse(text=command))
  #that has the bname equivalent to the string in the title
  # parse() returns an expression and eval() evaluates the expression
}

# lets rename not to cretae confusion by overwriting
library(gdata)
#"BA_Acquacolturacostiera"   "BA_difesacostiera"         "BA_turismoNW"              "CL_BA_OffshoreWindArea_NW"
mv(from = "BAAcquacolturacostiera", to= "NW_BA_Acquacolturacostiera")
mv(from = "BAdifesacostiera", to= "NW_BA_difesacostiera")
mv(from = "BAturismoNW", to= "NW_BAturismo")
mv(from = "CLBAOffshoreWindAreaNW", to= "NW_CLBAOffshoreWindAreaNW")



# same for polygons
nw_polygons$title <- as.factor(nw_polygons$title)
levels(nw_polygons$title)
# "AMBMarzamemiPortopalodiCp"                    "AMBRelittietal"                               "BAAMPPantelleria"                            
# [4] "BABanchiGrahamTerribile"                      "BAdifesacostierasabbie"                       "BAEstensioneAMPIsolePelagie"                 
# [7] "BAnuoveFRA"                                   "BARidezVelocita"                              "BARINW"                                      
# [10] "BAtraspmarit"                                 "BAturismoNW"                                  "CLBAOffshoreWindAreaNW"                      
# [13] "MBEnvironmentalprotectionandnaturalresources"s"

for (i in unique(nw_polygons$title)) { 
  # for every level in the title column
  command <- paste0(i, "<-subset(nw_polygons, title=='", i, "')")
  # create a new dataframe 
  eval(parse(text=command))
  #that has the name equivalent to the string in the title
}


# rename for tidiness
mv(from = "AMBMarzamemiPortopalodiCp", to= "NW_AMBMarzamemiPortopalodiCp")
mv(from =  "AMBRelittietal", to=  "NW_AMBRelittietal")
mv(from =  "BAAMPPantelleria", to=  "NW_BAAMPPantelleria")
mv(from ="BAnuoveFRA", to = "NW_BAnuoveFRA")
mv(from = "BABanchiGrahamTerribile", to = "NW_BABanchiGrahamTerribile"    )
mv(from = "BAdifesacostierasabbie", to = "NW_BAdifesacostierasabbie")
mv(from=    "BAEstensioneAMPIsolePelagie" , to =    "NW_BAEstensioneAMPIsolePelagie" )
mv(from =  "BARidezVelocita", to =  "NW_BARidezVelocita")
mv(from =      "BARINW", to =      "NW_ BARINW")
mv(from ="BAtraspmarit", to = "NW_BAtraspmarit")
mv(from = "BAturismoNW", to = "NW_BAturismoNW")
mv(from = "CLBAOffshoreWindAreaNW", to ="NW_CLBAOffshoreWindAreaNW")
mv(from = "MBEnvironmentalprotectionandnaturalresources", to= "NW_MBEnvironmentalprotectionandnaturalresources")


############ SP ############################################################

# lines
levels(sp_lines$title)
sp_lines$title <- as.character(sp_lines$title)
sp_lines$title <- as.factor(sp_lines$title)
levels(sp_lines$title)
# "CLBAOffshoreWindAreaSP" and "BAdifesacostiera" 

SP_CLBAOffshoreWindAreaSP <- sp_lines%>% subset(sp_lines$title == "CLBAOffshoreWindAreaSP")


# polygons
sp_polygons$title <- as.character(sp_polygons$title)
sp_polygons$title <- as.factor(sp_polygons$title)
levels(sp_polygons$title)
# ° "BAdifesacostierasabbie" "BAOG"                   "BARINW"                 "BAtraspmaritSPBD"       "CLBAOffshoreWindAreaSP"
SP_OG <- sp_polygons %>% subset(title == "BAOG")
SP_RI <- sp_polygons %>% subset(title == "BARINW")
SP_OWF <- sp_polygons %>% subset(title == "CLBAOffshoreWindAreaSP")

######### BD #######################################################
bd_lines$title <- as.character(bd_lines$title)
bd_lines$title <- as.factor(bd_lines$title)
levels(bd_lines$title)
# "BAAcquacolturacostieraBD"      "CLBAOffshoreWindAreaBD"  

BD_aquaculture <- bd_lines %>% subset(title == "BAAcquacolturacostieraBD")
BD_OWF<- bd_lines %>% subset(title == "CLBAOffshoreWindAreaBD")



# polygons
bd_polygons$title <- as.character(bd_polygons$title)
bd_polygons$title <- as.factor(bd_polygons$title)
levels(bd_polygons$title)

for (i in unique(bd_polygons$title)) { 
  # for every level in the title column
  command <- paste0(i, "<-subset(bd_polygons, title=='", i, "')")
  # create a new dataframe 
  eval(parse(text=command))
  #that has the name equivalent to the string in the title
}




# bulkk export all the datasets into shapefiles
obj <- ls() 

for (i in 1:length(obj)) {
  
  if (all(class(get(obj[i])) == c("sf", "tbl_df","tbl","data.frame"))) {
    
    fname <- paste0(obj[i], ".shp")
    
    sf::st_write(get(obj[i]), fname)
  }
}

############################################################
Let's break down the code step by step:

1. for (i in unique(bd_polygons$title)) {
Purpose: This initiates a for loop that will iterate over each unique value in the title column of the bd_polygons data frame.
Explanation:
unique(bd_polygons$title) returns a vector containing all the unique values in the title column.
for (i in ...) sets up a loop where i will take on each of these unique values one at a time.
2. command <- paste0(i, "<-subset(bd_polygons, title=='", i, "')")
Purpose: Constructs a string that represents an R command.
Explanation:
paste0() is used to concatenate strings without any spaces.
i is the current unique value of the title column in this iteration.
The resulting string is a command that looks like this: "value <- subset(bd_polygons, title=='value')" where value is replaced by the current i.
Example: If i is "Park", the command string would be "Park <- subset(bd_polygons, title=='Park')".
3. eval(parse(text=command))
Purpose: Executes the command created in the previous step as if it were an R script.
Explanation:
parse(text=command) converts the string stored in command into an R expression.
eval() then evaluates (executes) this expression as if you typed it directly in the R console.
Result: This creates a new data frame with the name equivalent to the current value of i and stores the subset of bd_polygons where the title column matches i.
Example: For i = "Park", the line "Park <- subset(bd_polygons, title=='Park')" is executed, creating a new data frame named Park containing only rows where the title is "Park".
4. }
Purpose: Ends the for loop.
Explanation: This bracket marks the end of the code block that is repeated for each unique value in bd_polygons$title.
Summary of What the Code Does:
The code loops over each unique value in the title column of the bd_polygons data frame.
For each unique value, it constructs a command to create a new data frame containing only the rows from bd_polygons where the title matches that value.
It then evaluates this command, effectively creating a series of new data frames, each named after a unique value in the title column and containing only the corresponding subset of data.
This is a way to dynamically create multiple data frames from a single data frame based on the values in one of its columns.
