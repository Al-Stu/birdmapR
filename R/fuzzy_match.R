#-------------------------------------------------------------------------#
#------------------------------ FUZZY MATCH ------------------------------#
#-------------------------------------------------------------------------#

# FUZZYMATCHPAIRS
# this function finds the best matching pair for each value in vector x in vector y
# it outputs a two-column dataframe

# author: alice stuart | date modified: 2020-0x-xx
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0


### FUNCTION -------------------------------------------------------------
fuzzymatchpairs <- function(x,y){
  # create a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(x,y, partial = TRUE, ignore.case = TRUE)

  # find pairs with minimum distance
  min.name<-apply(dist.name, 1, min)
  match.x.y<-NULL

  for(i in 1:nrow(dist.name)){
    y.i<-match(min.name[i],dist.name[i,])
    x.i<-i
    match.x.y<-rbind(data.frame(y.i=references[y.i,]$paper_id,x.i=x.i,yname=references[y.i,]$title, xname=file_id[x.i,]$title, adist=min.name[i]),match.x.y)
  }

  # use these pairs to assign correct file IDs
  result <- vector(mode = 'character', length  = length(match.x.y$y.i))
  for(j in 1:length(match.x.y$y.i)){
    result[match.x.y$x.i[j]] <- match.x.y$y.i[j]
  }
  return(result)
}

# FUZZYMATCHPOSITION
fuzzymatchposition <- function(x,y){
  # create a matrix with the Standard Levenshtein distance between the name fields of both sources
  dist.name<-adist(x,y, partial = FALSE, ignore.case = TRUE)

  # find pairs with minimum distance
  min.name<-apply(dist.name, 1, min)

  result <- position(min.name,dist.name)

  return(result)
}
