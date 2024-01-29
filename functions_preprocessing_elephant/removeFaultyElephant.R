## MSc Thesis 
## Jacotte Monroe 
## Data Exploration 

## function to remove any faulty elephants by ID

# takes elephant_data and ID number to remove
# returns new dataframe without faulty elephant
# source: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/

removeElephantByID <- function(elephant_dataset, faulty_elephant_ID){
  e_df <- elephant_dataset[!(elephant_dataset$individual.local.identifier %in% faulty_elephant_ID),]
  return(e_df)
}