#' Revoke a certification for workers
#' 
#' @details
#'
#' When running an analysis, certifications can be used to control which workers are allowed to access your 
#' tasks. These certifications allow you to ensure that all workers have passed a training module, and no 
#' workers that you have determined delivery poor quality work are allowed to continue responding to your 
#' task. Additional certification documentation can be found here: 
#' https://www.sentimentit.com/documentation/certifications.markdown.
#' 
#' The revokeCert() function can be used to revoke a certification from workers. By revoking the certification 
#' of workers, you can prevent these workers from completing your Mechanical Turk tasks that require these 
#' certifications.
#' 
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return The number of workers revoked.  
#'
#' @author David Carlson
#' @examples
#' 
#' \dontrun{ 
#' revokeCert(email = 'researcher@school.edu', password = 'uniquePassword', cert = 'snippets', workers = ban_workers)
#' }
#'
#' @rdname revokeCert
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{signout}}
#' @export
revokeCert <- function(email, password, cert, workers){
 auth_token <- sentimentIt::authenticate(email, password)
  out <- vector()
  
  if(length(workers)==1){
    args <- list(email = email, auth_token = auth_token, certification = cert, workers = list(workers))
  }else{
    args <- list(email = email, auth_token = auth_token, certification = cert, workers = workers)
  }
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/certifications/revoke.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, rawToChar(as.raw(mypost$content)))
  return(out)
  }
