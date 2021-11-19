#' Create new batches
#' 
#' @details 
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
#' have run the readText() function to upload your data objects to the SentimentIt server.
#' 
#' The createBatches() function creates unique ID numbers for the batches you will use to submit comparisons
#' to Mechanical Turk. You must specify how many batches you would like to release your comparisons in. The
#' function will return a vector of the unique batch IDs you created. This vector should be saved in your
#' R workspace.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#' 
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param task_setting_id ID of Task setting to use
#' @param num_batches Number of separate batches to create. Default is 1.
#'
#' @return batch_ids Vector of batch IDs created
#' @author David Carlson
#' @examples
#' \dontrun{
#' batch_ids <- createBatches(email = 'researcher@school.edu', password = 'uniquePassword', task_setting_id = 8, num_batches = 3)
#' }
#' @rdname createBatches
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
createBatches <- function(email, password, task_setting_id, num_batches=1){
  auth_token <- sentimentIt::authenticate(email, password)
  if(!is.numeric(task_setting_id)){
    stop("task_setting_id needs to be a numeric")
  }
  batch_ids <- vector()
  for(i in 1:num_batches){
    args <- paste("hit_setting_id=", task_setting_id, '&email=', email, '&auth_token=', auth_token, sep="")
    myget <- POST(paste('https://www.sentimentit.com/api/batches.json?',
                       args, sep=''))
    batch_ids[i] <- fromJSON(rawToChar(as.raw(myget$content)))$id
  }
  return(batch_ids)
}

