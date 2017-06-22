library(dplyr)

#------------------------------
# I set the working directory.
#------------------------------

setwd("/home/benedek/Documents/societe/")

#---------------------------------------------------------------
# The test and training set is used for the mapping generation.
#---------------------------------------------------------------

train <- read.csv("./raw_dataset/train.csv", sep = ";",stringsAsFactors = FALSE)
test <- read.csv("./raw_dataset/test.csv", sep = ";",stringsAsFactors = FALSE)

#--------------------------------
# We integrate the two datasets.
#--------------------------------

integrated <- rbind(train, test)

#----------------------------------------
# We need the unique countries.
#----------------------------------------

countries <- unique(integrated$country)

#----------------------------------------
# The months in this specific order.
#----------------------------------------

months <- c(6, 7, 1, 2, 3, 4, 5, 8, 9, 10, 11, 12)

#-------------------------------------------------------------
# The month that is going to be the pair in the other series.
#-------------------------------------------------------------

pairs <- 13 - months

#----------------------------------------
# I create the mapping for each country.
#----------------------------------------

for (country in countries){
  
    #-------------------------------------
    # I need a country specific subtable.
    #-------------------------------------
  
    sub_table <- integrated[integrated$country == country, ]
    
    #----------------------------------------------------------------------------------------------------------
    # For each month I create a subsetted table.
    # For the middle month I join these tables in order the linearize the time series.
    # This works because of the unique year, month and country IDs and because of the granularity of the data. 
    #----------------------------------------------------------------------------------------------------------
    
    
    for (i in 1:12){
      
        real_sub_table <- sub_table[sub_table$month == months[i],]
        
        if (months[i] == 6){
          
            country_specific_table <- real_sub_table[,c("ID","month", "country", paste0("X",pairs[i],"_diffSumExports.kmt."))]
            colnames(country_specific_table) <- c("ID_6","month_6","country_6", "joiner")
          
        }
        else{
          
            new_specific_table <- real_sub_table[,c("ID","month", "country", paste0("X",pairs[i],"_diffSumExports.kmt."))]
            colnames(new_specific_table) <- c(paste0("ID_",months[i]),paste0("month_",months[i]),paste0("country_",months[i]), "joiner")
            
            country_specific_table <- left_join(country_specific_table, new_specific_table)
            
        }
    }
    
    #----------------------------------------------------------------------
    # The ID columns now have the month specific data point IDs every year. 
    # The rows represemt years.
    # The country specific mapping table is in the final mapping dataframe.
    #----------------------------------------------------------------------
    
    selecting_from_the_country_table <- country_specific_table[, paste0("ID_", c(1:12))]
    
    new_years <- c(1:14)
    new_months <- c(1:12)
    
    final_mapping <- data.frame(matrix(0, 168, 4))
    
    colnames(final_mapping)
    
    index <- 0
    
    colnames(final_mapping) <- c("year", "month", "ID", "country")
    
    for (new_year in new_years){
        for (new_month in new_months){
          
            index <- index + 1
            
            final_mapping$year[index] <- new_year
            final_mapping$month[index] <- new_month  
            final_mapping$ID[index] <- selecting_from_the_country_table[new_year, new_month]
            final_mapping$country <- country
            
        }
    }
    
    #-------------------------------------------------------------------
    # The final mapping tables are concatenated together row after row.
    # The first table is for country 52.
    #-------------------------------------------------------------------
    
    if (country == 52){
        out_mapping <- final_mapping
    }
    else{
        out_mapping <- rbind(out_mapping, final_mapping)
    }
}

#------------------------------
# Dumping the mapping on disk.
#------------------------------

write.csv(out_mapping, "./raw_dataset/mapped_year_linearized.csv",  row.names = FALSE)
