#' Create comparisons and post to SentimentIt
#' 
#' @details
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
#' have run the readText() function to upload your data objects to the SentimentIt server. You should have also
#' run the createBatches() function and saved the batch ID numbers in your R workspace.
#' 
#' To generate random pairings of your data objects, run the `makeComps()` function, which will generate random 
#' pairings of your data objects and distribute them evenly among the number of batches you specify. Before 
#' running this function, you should have your unique batch ID numbers stored in your R workspace and you should 
#' have decided what quality you want to compare between the paired objects, which should be specified in the 
#' `question` input.
#' 
#' Once the comparisons have been generated, they will be stored in the SentimentIt server and sent to Mechanical 
#' Turk when you direct them to. The function should return a vector of the batch IDs you provided. If not, the 
#' function did not correctly set up your batches. 
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#'
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt registration
#' @param ids Numerical vector of IDs for the documents
#' @param number_per Number of comparisons desired
#' @param batch_id Batch IDs to be used for the tasks
#' @param question A character of the question the worker will see once the worker selects the task
#' @param per_batch Number of comparisons per batch desired. Defaulted to 1000.
#' @param pairwise_path File path to store matrix of document IDs used for comparisons. Default is pairwise.Rdata
#' @param ids_as_comps an indicator as to whether or not the IDs provided refer to comparison IDs rather than document IDs. Default is FALSE.
#'
#' @return out A table with the text and correspondings ID's that have been sent.
#'
#' @author David Carlson
#' @examples 
#' 
#' \dontrun{
#' docInfo <- read.table(email, password, "ReviewsWithIds",header=TRUE)
#' makeComps(email = 'researcher@school.edu', password = 'uniquePassword', ids = docInfo[,'ids'], number_per = 10, batch_id = batch_ids, question = 'Below is text taken from two movie reviews. Please choose the text that you think comes from the most positive review', pairwise_path = 'Comparisons/first10.Rdata')
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname makeComps
#' @export
makeComps <- function(email, password, ids, number_per, batch_id, question, per_batch=1000, 
                         pairwise_path='pairwise.Rdata', ids_as_comps=FALSE){
  if(!ids_as_comps){
    pairwise <- createPairwise(ids, number_per, pairwise_path)
  }else{
    pairwise <- ids
  }
  num_comps <- dim(pairwise)[1]
  if(length(batch_id)*per_batch<num_comps){
    print('Not enough batch_id supplied')
    return(NULL)
  }
  out <- vector()
  auth_token <- sentimentIt::authenticate(email, password)
  if(per_batch > num_comps){
    args <- list(email=email, auth_token=auth_token, question=question, ids=pairwise, batch_id=batch_id[1])
    args <- toJSON(args, auto_unbox=TRUE)
    mypost <- POST('https://www.sentimentit.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
    out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
  }else{
    for(i in 1:(num_comps%/%per_batch)){
      args <- list(email=email, auth_token=auth_token, question=question, ids=pairwise[((i-1)*per_batch+1):(i*per_batch),], batch_id=batch_id[i])
      args <- toJSON(args, auto_unbox=TRUE)
      mypost <- POST('https://www.sentimentit.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
      out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
    }
    if(num_comps%%per_batch!=0){
      args <- list(email=email, auth_token=auth_token, question=question, ids=pairwise[((i)*per_batch+1):(dim(pairwise)[1]),], batch_id=batch_id[i+1])
      args <- toJSON(args, auto_unbox=TRUE)
      mypost <- POST('https://www.sentimentit.com/api/comparisons/create.json',
                   body = args, content_type_json(),
                   encode='json')
      out <- c(out, unlist(fromJSON(rawToChar(as.raw(mypost$content)))))
    }
  }
  return(out)
}

