#' Grants certifications to workers
#'
#' @details 
#' 
#' When running an analysis, certifications can be used to control which workers are allowed to access your 
#' tasks. These certifications allow you to ensure that all workers have passed a training module, and no 
#' workers that you have determined delivery poor quality work are allowed to continue responding to your 
#' task. Additional certification documentation can be found here: 
#' https://www.sentimentit.com/documentation/certifications.markdown.
#' 
#' The createCert() function can be used to assign a certification to workers. If you attempt to assign a 
#' non-exist certification to a worker, this function will create the certification before assigning it. 
#' You can require workers to possess a particular certification to work on tasks you send to Mechanical Turk.
#' 
#' 
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param cert The name of the certification given to the workers.
#' @param workers The workers you want to grant certification.
#'
#' @return Character indicating number of workers certified.  
#'
#' @author David Carlson
#'
#' @examples
#' 
#' \dontrun{ 
#' createCert(email = 'researcher@school.edu', password = 'uniquePassword', cert= 'bannedmovie_reviews', workers = ban_workers)
#' }
#' @rdname createCert
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export
createCert <- function(email, password, cert, workers){
  auth_token <- sentimentIt::authenticate(email, password)
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  out <- vector()
  if(length(workers)==1){
      args <- list(email = email, auth_token = auth_token, certification = cert, workers = list(workers))
  }else{
      args <- list(email = email, auth_token = auth_token, certification = cert, workers = workers)
  }
  args <- toJSON(args, auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/certifications/create.json',
                 body = args, content_type_json(),
                 encode='json')
  out <- c(out, rawToChar(as.raw(mypost$content)))
  return(out)
}

