library(dplyr)

#------------------------------------
# I set the working directory here.
#------------------------------------

setwd("/home/benedek/Documents/societe/")

#------------------------------------------
#------------------------------------------
# FUNCTION for hot-one encoding.
#------------------------------------------
#------------------------------------------

dummygen <- function(new_table, original_table, dummified_column, column_values, new_name){ 
  
  #-----------------------------------------------------------------
  # INPUT 1. -- The new cleaned table -- I will attach the dummies.
  # INPUT 2. -- The original table that is being cleaned.
  # INPUT 3. -- The column that has the strings.
  # INPUT 4. -- The unique values in the column encoded.
  # INPUT 5. -- The new name of the columns.
  # OUTPUT -- The new table with the dummy variables.
  #-----------------------------------------------------------------
  
  i <- 0
  
  for (val in column_values){
    i <- i + 1
    new_variable <- data.frame(matrix(0, nrow(new_table), 1))
    new_variable[original_table[,dummified_column] == val, 1] <- 1
    colnames(new_variable) <- paste0(new_name, i)
    new_table <- cbind(new_table,new_variable)
  }
  return(new_table)
}

#--------------------------------------------
#--------------------------------------------
# FUNCTION for aggregate dataset generation. 
#--------------------------------------------
#--------------------------------------------

aggregate_joiner <- function(input_table, time_aggregator, rowbinded_table, naming_convention){
  
  #-----------------------------------------------------------------
  # INPUT 1. -- The training or test set.
  # INPUT 2. -- A list of country-time level grouping keys.
  # INPUT 3. -- The rwo binded test and training data.
  # INPUT 4. -- Names in the new table.
  # OUTPUT -- Aggregates added to the input table.
  #-----------------------------------------------------------------
  
  aggregates <- list()

  #----------------------------------------------------
  # The aggregates that I consider are as follows:
  # Mean, min, max, standard deviation and median.
  #----------------------------------------------------
  
  functions <- c("mean", "min", "max", "sd", "median")
  statistics <- c("means", "mins", "maxs", "sds", "medians")
  
  for(i in 1:5){
  
      aggregates[[statistics[i]]] <- aggregate(rowbinded_table, by = time_aggregator, eval(parse(text = functions[i])))
  
  }
  
  naming <- c("country", "year")
  
  indexing_1 <- c(3:122)
  
  indexing_2 <- c(1:120)  
 
  for (stat_value in statistics){
     colnames(aggregates[[stat_value]])[1:2] <- naming
     colnames(aggregates[[stat_value]])[indexing_1] <- paste0("aggregated_", stat_value, "_", naming_convention, "_", indexing_2)
     input_table <- left_join(input_table, aggregates[[stat_value]])
  }
  
  return(input_table)
}

#--------------------------------------------
#--------------------------------------------
# FUNCTION for dataset cleaning.
#--------------------------------------------
#--------------------------------------------

data_clean <- function(input_table, input_table_2, target_aggregates, time_indices){
  
    #--------------------------------------------------------------------------------
    # INPUT 1. -- Training or test dataset.
    # INPUT 2. -- The other dataset.
    # INPUT 3. -- The aggregates of the target variable.
    # INPUT 4. -- The linearized time mapping obtained with 2_time_series_pairing.R.
    # OUTPUT -- The cleaned input table.
    #--------------------------------------------------------------------------------
    
    #-----------------------------------------------------------
    # Creating a subset of the dataset with non index columns.
    #-----------------------------------------------------------

    non_index_columns <- c(4:123)
  
    new_table <- input_table[, non_index_columns]
  
    #-----------------------------
    # Concatenating the datasets.
    #-----------------------------
    
    rowbinded_table <- rbind(input_table, input_table_2)

    #--------------------------------------------------------------------
    # Creating aggregation keys based on country-year and country-month.
    #-------------------------------------------------------------------- 
        
    time_aggregator_1 <- list(rowbinded_table$country, rowbinded_table$year)
    time_aggregator_2 <- list(rowbinded_table$country, rowbinded_table$month)
    
    #-----------------------------------------------------
    # Keeping the non-index columns of the joined dataset.
    #-----------------------------------------------------
    
    rowbinded_table <- rowbinded_table[, non_index_columns]
    
    #--------------------------------------------
    # Creating the respective aggregate tables.
    #--------------------------------------------
    
    input_table <- aggregate_joiner(input_table, time_aggregator_1, rowbinded_table, "1")
    
    input_table <- aggregate_joiner(input_table, time_aggregator_2, rowbinded_table, "2")
    
    #-------------------------------------------
    # Concatenating columwise the aggregates.
    #-------------------------------------------
    
    new_table <- cbind(new_table, input_table[125:ncol(input_table)])
  
    #-------------------------------
    # Adding month binary features.
    # Adding year binary features.
    #-------------------------------
    
    new_table <- dummygen(new_table, input_table, "month", c(1:12), "month_")
  
    new_table <- dummygen(new_table, input_table, "country", c(1:76), "country_")

    #------------------------------
    # Adding the target aggregates.
    #------------------------------
      
    mapped_table <- left_join(input_table, target_aggregates)       
    new_table <- cbind(new_table, mapped_table[,c("aggregated_target")])

    #-------------------------------------
    # Adding the linearized year indices.
    #-------------------------------------       
      
    new_table <- new_table[, 1:ncol(new_table)-1]
    new_table <- new_table[, colnames(new_table) != "year"]

    mapped_2_table <- left_join(input_table, time_indices)

    new_table$new_year <- mapped_2_table$new_year
    new_table$time <- mapped_2_table$time
     
    return(new_table)
}

########################################
##------------------------------------##
##------------------------------------##
##------------------------------------##
## Actual data cleaning happens here. ##
##------------------------------------##
##------------------------------------##
##------------------------------------##
########################################

#--------------------------------------
# Reading the training and test data.
#--------------------------------------


train <- read.csv("./raw_dataset/train.csv", sep = ";", stringsAsFactors = FALSE)
test <- read.csv("./raw_dataset/test.csv", sep = ";", stringsAsFactors = FALSE)

#------------------------------
# Reading the target variable.
#------------------------------

target <- read.csv("./raw_dataset/target.csv", sep = ";", stringsAsFactors = FALSE)

#-------------------------------
# Reading the linearized year.
#-------------------------------

mapping <- read.csv("./raw_dataset/mapped_year_linearized.csv", sep = ",", stringsAsFactors = FALSE)

#----------------------
# Joining the mapping.
#----------------------

train <- left_join(train, mapping)

test <- left_join(test, mapping)

#-----------------------------
# Reading the time indices.
#-----------------------------

time_indices <- read.csv("./raw_dataset/time_indices.csv", sep = ",", stringsAsFactors = FALSE)

#--------------------------
# Creating the aggregates.
#--------------------------

target_aggregates <- aggregate(target$Target, by = list(train$country, train$year), mean)

#----------------------------------
# Renaming the aggregate columns.
#----------------------------------

colnames(target_aggregates) <- c("country", "year", "aggregated_target")

#-------------------------------------------------------
# Creating the datasets for training with the function.
#-------------------------------------------------------

new_train <- data_clean(train, test, target_aggregates, time_indices)
new_test <- data_clean(test, train, target_aggregates, time_indices)

#-----------------
# Dumping to disk.
#-----------------

write.csv(new_train, "./clean_dataset/train.csv", row.names = FALSE)
write.csv(new_test, "./clean_dataset/test.csv", row.names = FALSE)
