#' Read in data provided by the workers
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
#' Once you have sent the batches to Mechanical Turk using `createTasks()` and allowed some time for comparisons 
#' to be completed, you can retrieve data on the batches you sent. To retrieve this data, use the `readInData()` 
#' function. 
#' 
#' You must specify a batch ID to the function, and the function will return a data frame containing the batch ID, 
#' comparison ID, document ID, result of the comparison, ID of the task, ID of the worker who completed the task, 
#' and the time when the task was completed.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#'
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt
#' @param batch_id A vector of batch numbers to download data from.
#'
#' @return output a dataframe with the data from the specified batches with columns
#' \describe{
#'   \item{batch_id}{the batch number}
#'   \item{comparison_id}{id number of the comparison being made}
#'   \item{document_id}{id number of the document}
#'   \item{result}{result of document comparison}
#'   \item{task_id}{id of the task}
#'   \item{worker_id}{id of the worker who did the comparison}
#'   \item{completed_at}{time comaparison was completed}
#' }
#' @author Jacob M. Montgomery
#' @examples
#' 
#' \dontrun{
#' output <- readInData(email = 'researcher@school.edu', password = 'uniquePassword', batch_id = batch_ids[1])
#' }
#' @rdname readInData
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
readInData <- function(email, password, batch_id) {
  if (!is.vector(batch_id) | !is.numeric(batch_id)) {
    stop("batch_id needs to be numeric")
  }
  auth_token <- sentimentIt::authenticate(email, password)
  # Put the batch_id in numerical order and remove duplicates
  batch_id <- unique(batch_id)
  batch_id <- batch_id[sort.list(batch_id)]
  output<-data.frame()
  for(i in batch_id){
    myurl<- GET(paste0('http://www.sentimentit.com/api/batches/',i,'/download.json?email=', email, '&auth_token=', auth_token))
    myurl <- rawToChar(as.raw(myurl$content))
    myurl <- strsplit(myurl,'\"')[[1]][4]
    Sys.sleep(60)
    x <- getURL(myurl,async=FALSE)
    # attempt to connect to server until data is downloaded
    try_count <- 0
    while(grepl('Access Denied', x) & try_count < 15){
      Sys.sleep(20)
      x <- getURL(myurl,async=FALSE)
      try_count = try_count + 1
    }
    if(grepl('Access Denied', x) & try_count == 15){
      message <- paste("Failed to download batch", i,
                       "try again later and check that the batch number exists.")
      warning(message)
    }else{
      hold <- as.data.frame(read.csv(text = x), stringsAsFactors = FALSE)
      for(k in 1:ncol(hold)){
        if(is.factor(hold[,k])) hold[,k] <- as.character(hold[,k])
      }
      output <- rbind(output,hold)
    }
  }
  return(output)
}

