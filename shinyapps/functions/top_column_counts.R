#the purpose of this function is to determine the counts of each value within a column and return the top 5(default option) counts along with an "other" (i.e.not top 5) count

#switch na_rule to NA to exclude NA
df_column_counts <- function(df,top=5, other = TRUE, factors = FALSE, na_exclude = NULL){
  table_a <- apply(df,2,function(x) table(x,exclude=na_exclude))
  if(other==FALSE)
    top_x <- lapply(table_a, function(x) x[c(order(-x))][1:min(length(x),top)])
  if(other)
    top_x <- lapply(table_a, function(x) c(x[c(order(-x))][1:min(length(x),top)], "other" = sum(x[c(order(-x))][-c(1:min(length(x),top))])))
  top_df <- as_tibble(data.frame(col_name = rep(names(top_x),sapply(top_x,length)), 
                                 label_name = str_trim(unlist(sapply(top_x, names))), 
                                 count = unlist(top_x), stringsAsFactors = factors))
  return(top_df[!top_df$count==0,])
}