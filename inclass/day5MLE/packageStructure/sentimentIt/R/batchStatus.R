#' Check the status of batch(es)
#' 
#' @details 
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
#' have run the readText() function to upload your data objects to the SentimentIt server. You should have also
#' run the createBatches() function and saved the batch ID numbers in your R workspace. You should have also
#' run the makeComps() function to save a portion of your total comparisons under each batch. You should have
#' also run the createTasks() function to send batches of paired data objects to Mechanical Turk for comparison.
#' 
#' Once you have submitted batches of comparisons to Mechanical Turk, you can check their status to see how many 
#' comparisons in the batch have been completed. This can be done using the `checkBatches()` function. To check 
#' the status of a batch, all you need is the batch ID number.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#' 
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param batch_id Vector of batch IDs to check
#'
#' @return output Dataframe with the variables:
#' \itemize{
#'   \item id
#'   \item total_count
#'   \item submitted count
#'   \item completed_count
#'   \item expired_count
#' }
#' @author David Carlson
#' @examples
#' 
#' \dontrun{ 
#' batchStatus(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids[1])
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname batchStatus
#' @export
batchStatus <- function(email, password, batch_id){
  auth_token <- sentimentIt::authenticate(email, password)
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be a vector of numerics")
  }
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  output<-data.frame()
  for(i in batch_id){
    myurl<- GET(paste0('http://sentimentit.com/api/batches/',i,'/status.json?email=',email,'&auth_token=', auth_token))
    if(rawToChar(as.raw(myurl$content))!='  '){
      data <- fromJSON(rawToChar(as.raw(myurl$content)))
      status <- data.frame(data$id, data$comparisons)
      output <- rbind(output, status)
    }else{
      print(paste('Check batch',i,'-- nonexistent.'))
    }
  }
  if(dim(output)[1]==0){
    print('Check batch number(s). Batch(es) non-existent.')
    output <- data.frame(matrix(0, nrow=1,ncol=5)) 
  }
  colnames(output) <- c("id", "total_count", "submitted_count", "completed_count", "expired_count" )
  return(output)
}

