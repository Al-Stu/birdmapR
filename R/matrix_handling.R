#-------------------------------------------------------------------------#
#---------------------------- MATRIX HANDLING ----------------------------#
#-------------------------------------------------------------------------#

# MATRIXROWTOTALS
# this function finds the total for each row in a matrix, option to specify IDs that match matrix names 
# it outputs a numeric vector with length nrow

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

matrixrowtotals <- function(matrix, IDs){
  result <- vector(mode = 'numeric', length = nrow(matrix))
  if(is.null(IDs)|is.null(rownames(matrix))){
    for(i in 1:length(result)){
      result[i] <- sum(matrix[i,])
    }
  }else{
    for(i in 1:length(result)){
      matrix_row <- position(x = IDs[i], y = rownames(matrix))
      result[i] <- sum(matrix[matrix_row,])
    }
  }
    return(result)
}


# MATRIXTODATAFRAME
# this function converts a matrix to a dataframe specifying each x-y pair
# it outputs dataframe with columns x, y and uses

# author: alice stuart | date modified: 2020-03-02
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

matrixtodataframe <- function(matrix, x_title, y_title){
  result <- data.frame(y=character(),
                       x=character(),
                       uses=integer(),
                       stringsAsFactors=FALSE)
  for(i in 1:ncol(matrix)){
    y <- rep_len(y_title[i],nrow(matrix))
    x <- vector(mode = 'character', length = nrow(matrix))
    uses <- vector(mode = 'integer', length = nrow(matrix))
    for(j in 1:length(matrix[,i])){
      x[j] <- x_title[j]
      uses[j] <- matrix[j,i]
    }
    temp_data <- cbind(y,x,uses)
    result <- rbind(result,temp_data)
  }
  result <- result[-position(x = 0, y = result$uses),]
  return(result)
}

# WEIGHTEDUSE
# this function weights the uses per search term for each paper-search term combination output from searchinpaper() by the total number of time searchterms were used in that paper
# it outputs a matrix containing the weighted use of each searchterm in each paper

# author: alice stuart | date modified: 2020-02-28
# compiled in R version 3.6.2 (2019-12-12) Dark and Stormy Night running x86_64-apple-darwin15.6.0

weighteduse <- function(matrix){
  result <- matrix(nrow = nrow(matrix), ncol = ncol(matrix))
  for (i in 1:ncol(matrix)) {
    if(all(matrix[,i] == 0)){
      result[,i] <- matrix[,i]
    }else{
      result[,i] <- matrix[,i]/sum(matrix[,i])
    }
  }  
  rownames(result) <- rownames(matrix)
  colnames(result) <- colnames(matrix)
  return(result)
}