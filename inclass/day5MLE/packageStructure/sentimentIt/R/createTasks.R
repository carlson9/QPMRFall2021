#' Create new tasks for comparisons
#' 
#' @details 
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
#' have run the readText() function to upload your data objects to the SentimentIt server. You should have also
#' run the createBatches() function and saved the batch ID numbers in your R workspace. You should have also
#' run the makeComps() function to save a portion of your total comparisons under each batch.
#' 
#' To send batches to Mechanical Turk, use the `createTasks()` function. As you can see in the below example, 
#' you can determine which batches you would like to send to Mechanical Turk by only specifying those specific 
#' batch IDs. You do not need to send them all at once. 
#' 
#' Note that when you run this function, workers will automatically be paid for the work they do in that batch, 
#' regardless of the quality of their work. In the SentimentIt system, any worker who completes a task is 
#' automatically paid every 24 hours.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#' 
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param comp_ids Vector of comparison ids for which to create tasks, if comparisons were not created under a batch. Default is NULL. Leave as NULL if batch_id is provided.
#' @param task_setting_id Task setting to use for the created tasks, if comparisons were not created under a batch. Default is NULL. Leave as NULL if batch_id is provided.
#' @param batch_id Batch ID to be used if comparisons created under a batch. Default is NULL.
#'
#' @return out ID for batch of comparisons
#' @author David Carlson
#' @examples
#' 
#' \dontrun{
#' createTasks(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids[1])
#' }
#' @rdname createTasks
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
createTasks <- function(email, password, comp_ids=NULL, task_setting_id=NULL, batch_id=NULL){
  auth_token <- sentimentIt::authenticate(email, password)
  if(is.null(comp_ids)){
    args <- paste('batch_id=', batch_id, '&email=', email, "&auth_token=", auth_token, sep='')
  }else{
    args <- paste('hit_setting=', task_setting_id, '&ids=', paste(comp_ids,collapse=','), '&email=', email, '&auth_token=', auth_token, sep='')
  }
  myget <- GET(paste('https://www.sentimentit.com/api/comparisons/create_hits.json?',
                     args, sep=''))
  mytry <- try(out<-fromJSON(rawToChar(as.raw(myget$content))))
  if(class(mytry) == "try-error"){
    out <- 'error'
  }
  return(out)
}

