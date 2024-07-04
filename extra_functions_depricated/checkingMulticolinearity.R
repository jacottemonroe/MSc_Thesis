## MSc Thesis 
## Jacotte Monroe 

## Check multicolinearity (correlation between all covariates)
## Input: dataset with all covariates (and response variable that should then exclude)
## Output: plot of correlation matrix 

# packages 
if(!('ggcorrplot') %in% installed.packages()){install.packages('ggcorrplot')} # to check multicolinearity (correlation plot)
library(ggcorrplot)

checkMulticolinearity <- function(dataset, correlation_threshold = NULL){
  ## check for correlation between covariates
  # source: https://www.datacamp.com/tutorial/multiple-linear-regression-r-tutorial
  
  # create dataframe of covariate values (without response variable) --> here all covariates start with 'B' (could change this later)
  covariates <- data.frame(dataset['B.'])
  
  # compute correlation (round 2 decimal places)
  correlation_matrix <- round(cor(covariates), 2)
  
  # Plot correlation matrix
  # source: https://stackoverflow.com/questions/41269593/customize-ggcorrplot
  corr_matrix <- ggcorrplot(correlation_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 2)+
    theme(axis.text.x=element_text(size=5),
          axis.text.y=element_text(size=5))
  
  print(corr_matrix)
  
  # if there is a set correlation threshold, return list of covariates above threshold
  if(!is.null(correlation_threshold)){
    
    # check if there are any highly correlated covariates
    hc_cov <- any(correlation_matrix >= correlation_threshold & correlation_matrix < 1)
    
    # get all column and row names of elements with high correlation
    # source: https://stackoverflow.com/questions/61588499/extracting-col-row-names-from-a-matrix-based-on-value-condition-in-r
    hc_rows <- rownames(correlation_matrix)[which(correlation_matrix >= correlation_threshold 
                                                  & correlation_matrix < 1, arr.ind = TRUE)[, 1]]
    hc_cols <- colnames(correlation_matrix)[which(correlation_matrix >= correlation_threshold 
                                                  & correlation_matrix < 1, arr.ind = TRUE)[, 2]]
    
    # make a table with col.row combinations that have high correlation 
    hc_combinations <- data.frame(col = hc_cols, row = hc_rows) 
    
    # remove duplicate combinations --> sort elements by row so all smaller elements 
    # in left col > apply to all rows (combinations will have same order) > turn into 
    # dataframe > transpose it because wrong way > keep only unique combinations 
    # source: https://stackoverflow.com/questions/28574006/unique-rows-considering-two-columns-in-r-without-order
    hc_combinations <- unique(t(data.frame(apply(hc_combinations, 1, sort))))
    
    # order element in freq
    # source: https://bookdown.org/ndphillips/YaRrr/order-sorting-data.html
    hc_cov_freq <- as.data.frame(table(hc_combinations))
    hc_cov_freq <- hc_cov_freq[order(hc_cov_freq$Freq, decreasing = T), ]
    
    return(list(hc_occurrence_check = hc_cov, correlation_matrix = correlation_matrix, 
                high_corr_cov_combinations = hc_combinations, high_corr_cov_freq = hc_cov_freq))
  }else{return(correlation_matrix)}
}