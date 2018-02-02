#Read the name file 

#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 


split_name <- function(filename)
{
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
  
  return(split_name)
}


#join_files works merging  files pdf into only one.
#-Argument: name_method  name of method of clustering
#         : num_cluter       number of cluster

#the workspace must be where are the others folders

join_files <- function(name_method, num_cluter)
{
  
  
  
  
  
}