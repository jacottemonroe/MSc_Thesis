## MSc Thesis
## Elephant Preprocessing
## Jacotte Monroe 
## 29/01/24

## Function script 

## Function to create sequences for specific period (week or month) in dataframe. 
## The series of elephant fixes is split into weeks. Each sequence of fixes per week is now a path. 
## If a path has remaining missing data. The path is split into two paths at the point of missing data and the missing fixes are removed.
## Paths that have a single fix are removed because insufficient data to derive movement later.
## Input: elephant fix dataframe and specified period to split data into (default 1 week).
## Ouput: elephant fix dataframe grouped into weeks and paths.



groupElephantByPeriod <- function(elephant_data, number_of_weeks){
  
  # set label 
  if (number_of_weeks == 1){label <- 'week'}
  else if (number_of_weeks == 4){label <- 'month'}
  else {label <- 'period'}
  
  # create column with period values (number since Jan 1st, 1970) 
  elephant_data[label] <- as.integer(as.numeric(elephant_data$date_time)/(604800*number_of_weeks)) # 604800 seconds in a week
  
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
  
  # remove paths that have single fix (need at least two consecutive fixes to build a full step)
  if(nrow(e_solo_steps) != 0){
    for(i in 1:nrow(e_solo_steps)){
      elephant_data <- elephant_data[-(elephant_data$week == e_solo_steps$week[i] 
                                       & elephant_data$path == e_solo_steps$path[i]),]
    } 
  }
  
  return(elephant_data) 
}
