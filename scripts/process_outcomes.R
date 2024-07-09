library('glue')


ProcessOutcomes <- function(data_table = NULL) {
    
    if (file.exists("data_table.rds")) {
        
        data_table <- readRDS("data_table.rds")
        
    } else {
        
        saveRDS(data_table, file = "data_table.rds")
    }
    
    data_table$ID <- NULL
    
    # make sure the order of columns are correct
    
    invalid_count <- 0
    
    for (i in 1:nrow(data_table)) {
        
        one_row <- data_table[i, ]
        

        if (TRUE) {
            
            if (!IsValidOutcomes(one_row)) {
                
                invalid_count <- invalid_count + 1
                
                #print(glue('row {i} is not valid'))
                #print(one_row)
            }
            
        } else {
            
            result_one_row <- ProcessOneRow(one_row)
            
            if (!result_one_row$IsValidRow) {
                
                invalid_count <- invalid_count + 1
                print(glue('row {i} is not valid'))
                print(one_row)
                                
            } else {
                
                print(result_one_row$Survival)
                print(result_one_row$Time)
            }
        }

    }
    
    print(glue('invalid row count = {invalid_count}'))
}


IsValidOutcomes <- function(one_row) {
    
    all_values <- as.vector(as.matrix(one_row))
    
    neat_values <- na.omit(all_values)
    
    for (col in 1:length(neat_values)) {
        
        if (col == 1) {
            
            shrinked_values <- neat_values[col]
            
        } else {
            
            if (neat_values[col] == neat_values[col - 1]) {
                
                next
            }
            
            shrinked_values <- c(shrinked_values, neat_values[col])
        }
    }
    
    if (identical(shrinked_values, 0) ||
        identical(shrinked_values, 1)
        ) {
        
        #print(one_row)
        return(TRUE)
    }
        
    if (identical(shrinked_values, c(1, 0))) {
        
        #print(one_row)
        return(TRUE)
    }
        
    if (identical(shrinked_values, c(0, 1))) {
        
        #print(one_row)
        return(TRUE)
    }

    if ((!is.na(all_values[7]) && all_values[7] == 1) &&
        (!is.na(all_values[6]) && all_values[6] == 1)) {
            
        #print(one_row)
        return(TRUE) # ???
    }   
    
    if ((!is.na(all_values[7]) && all_values[7] == 0) &&
        (!is.na(all_values[6]) && all_values[6] == 0)) {
            
        #print(one_row)
        return(TRUE)
    } 
    
    # here is wrong if three are all NA
    if ((is.na(all_values[7]) || all_values[7] == 1) &&
        (is.na(all_values[6]) || all_values[6] == 1) &&
        (is.na(all_values[5]) || all_values[5] == 1)) {
        
        #print(one_row)
        return(TRUE)
    }   
    
    # here is wrong if three are all NA
    if ((is.na(all_values[7]) || all_values[7] == 0) &&
        (is.na(all_values[6]) || all_values[6] == 0) &&
        (is.na(all_values[5]) || all_values[5] == 0)) {
        
        #print(one_row)
        return(TRUE)
    }
    
    
    if (identical(shrinked_values, c(0, 1, 0))) {
        
        #print(one_row)
        return(TRUE)
    }
    
    # ??? debug error
    if (identical(shrinked_values, c(1, 0, 1))) {
        
        #print(one_row)
        return(TRUE)
    }

    #print(shrinked_values)
    
    print(one_row)
    return(FALSE)
}

#ProcessOutcomes()

