library(dplyr)

#----------------------------------
# I set the working directory.
#----------------------------------

setwd("/home/benedek/Documents/societe/")

#-------------------------
# Datasets are obtained. 
#-------------------------

train <- read.csv("./raw_dataset/train.csv", sep = ";", stringsAsFactors = FALSE)
test <- read.csv("./raw_dataset/test.csv", sep = ";", stringsAsFactors = FALSE)

#---------------------------------
# The year linerization is loaded.
#----------------------------------

mapping <- read.csv("./raw_dataset/mapped_year_linearized.csv", sep = ",", stringsAsFactors = FALSE)

#------------------------------------------------------
# The mapping is joined to the test and training sets.
#------------------------------------------------------

train <- left_join(train, mapping)
test <- left_join(test, mapping)

#-----------------------------------------------------------------------------------------
# Creating a row binded table with subselected columns and ordering it by proper columns.
#-----------------------------------------------------------------------------------------

out <- rbind(train, test)
out <- out[,c("ID", "month", "year", "country", paste0("X",1:12,"_diffSumClosing.stocks.kmt."))]
out <- out[order(out$country, out$year, out$month, decreasing = TRUE),]

#---------------------------------------------
# I will iterate through each unique country.
#---------------------------------------------

countries <- unique(out$country)

#-------------------------------------------------------------------
# For each country we need to generate a mapping of the time index.
# I subselect the table and join the actual year index.
# The country specific tables are concatenated and dumped to disk.
#-------------------------------------------------------------------

for (country in countries){
  
    country_table <- out[out$country == country,]
  
    one <- country_table[country_table$month == 1, ]
    two <- country_table[country_table$month == 12, ]
  
    one <- one[,c(3, 5)]
    colnames(one) <- c("year_1", "joiner")
  
    two <- two[,c(3, 6)]
    colnames(two) <- c("year_2", "joiner")
  
    others <- left_join(two, one)

    starting_point <- setdiff(others$year_2,others$year_1)
  
    while (length(starting_point) <nrow(others)+1){
        starting_point <- c(starting_point, others[others$year_2 == last(starting_point),3])
    }
  
    extreme_table <- data.frame(starting_point, c(1:14))
    colnames(extreme_table) <- c("year","new_year")
    country_table <- left_join(country_table, extreme_table)
    
    #---------------------------------------------------------
    # For the first country I generate the new_country_table.
    # Later I generate the subtabels to it.
    #---------------------------------------------------------
    
    if (country == 76){
       new_country_table <- country_table 
    }
    else{
      new_country_table <- rbind(new_country_table, country_table)
   }
}

#--------------------------------------------
# A proper time index is generated.
# Only the ID, year and time index is needed.
# The results is dumped to disk.
#--------------------------------------------

new_country_table$time <- (14 - new_country_table$new_year)*12 +new_country_table$month

new_country_table <- new_country_table[,c("ID","new_year","time")]

write.csv(new_country_table, "./raw_dataset/time_indices.csv", row.names = FALSE)
