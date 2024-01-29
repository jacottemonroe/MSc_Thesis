## MSc Thesis
## Jacotte Monroe 
## Preprocessing Elephant data 

## function to create sequences for specific period (week or month) in dataframe 
## function removes all missing data to have fixed weeks of data of same length

groupElephantByPeriod <- function(elephant_data, number_of_weeks, constant_step_size = F){
  
  # set label 
  if (number_of_weeks == 1){label <- 'week'}
  else if (number_of_weeks == 4){label <- 'month'}
  else {label <- 'period'}
  
  # create column with period values (number since Jan 1st, 1970) 
  elephant_data[label] <- as.integer(as.numeric(elephant_data$date_time)/(604800*number_of_weeks)) # 604800 seconds in a week
  
  # two approaches for defining paths 
  if(constant_step_size == T){
    # Choose to keep a constant step size per weekly path. 
    # All weeks that have missing data or tailing weeks (= not same step size) are omitted. 
    
    # select only rows with missing data (to create a sort of mask)
    missing_data_rows <- elephant_data[is.na(elephant_data$location.long),]
    
    # get period values that have missing data
    missing_data_mask <- as.vector(unique(missing_data_rows[[label]]))
    
    # take all rows of the elephant dataset that are not in the missing data list
    elephant_grouped <- elephant_data[(!elephant_data[,label] %in% missing_data_mask),]
    
    # get values of starting and ending periods since don't have same number of fixes 
    tails <- c(elephant_data[1,label],elephant_data[nrow(elephant_grouped), label])
    
    # remove tailing periods 
    elephant_grouped <- elephant_grouped[(!elephant_grouped[,label] %in% tails),]
    
    rownames(elephant_grouped) <- 1:nrow(elephant_grouped)
    
    return(elephant_grouped)
    
  }else{
    # Choose to allow weeks with different step sizes (because in theory should not affect model). 
    # Missing data marks split into different elephant paths. 
    # Elephant weeks can have different number of paths and different step sizes. 
    
    # create column that assigns 0 if !is.na() and 1 if is.na()
    elephant_data['NA_count'] <- 0
    
    elephant_data$NA_count[is.na(elephant_data$location.long)] <- 1
    
    # create column accumulates count of previous column for each week 
    # source: https://stackoverflow.com/questions/21818427/how-to-add-a-cumulative-column-to-an-r-dataframe-using-dplyr
    # need rlang package for turning string into column name 
    # source: https://stackoverflow.com/questions/56944486/dplyr-pipeline-in-a-function
    # source: https://adv-r.hadley.nz/quasiquotation.html#unquoting-many-arguments
    period <- syms(label)
    elephant_data <- elephant_data %>% group_by(!!!period) %>% mutate(csum = cumsum(NA_count)) 
    
    # remove rows with missing data 
    elephant_data <- elephant_data[!is.na(elephant_data$location.long),]
    
    # update path count -> final column that assigns ordered number for each unique value for each week 
    # source: https://stackoverflow.com/questions/68584595/assign-unique-numbers-to-unique-values-within-group-dplyr
    elephant_data <- elephant_data %>% group_by(!!!period) %>% mutate(path = match(csum, unique(csum)))
    
    # remove intermediate columns 
    elephant_data <- subset(elephant_data, select = -c(NA_count, csum))
    
    # select paths with stand-alone fixes (single fix in path)
    # source: https://stackoverflow.com/questions/27829558/count-occurrences-in-unique-group-combination
    e_count <- count(elephant_data, week, path) %>% ungroup()
    
    e_solo_steps <- e_count[e_count$n == 1, 1:2]
    
    #number_e_solo_steps <- nrow(e_solo_steps)
    #print(number_e_solo_steps)
    #print(nrow(elephant_data) - number_e_solo_steps)
    
    if(nrow(e_solo_steps) != 0){
      # remove stand-alone fixes/paths (need at least two consecutive fixes to build a full step)
      for(i in 1:nrow(e_solo_steps)){
        elephant_data <- elephant_data[-(elephant_data$week == e_solo_steps$week[i] 
                                         & elephant_data$path == e_solo_steps$path[i]),]
      } 
    }
    
    return(elephant_data) 
  }
}
