#' Checks if a worker has a certification
#' 
#' @details 
#' 
#' When running an analysis, certifications can be used to control which workers are allowed to access your 
#' tasks. These certifications allow you to ensure that all workers have passed a training module, and no 
#' workers that you have determined delivery poor quality work are allowed to continue responding to your 
#' task. Additional certification documentation can be found here: 
#' https://www.sentimentit.com/documentation/certifications.markdown.
#' 
#' To check if a worker has a particular certification, use the checkCert() function. To use this function, 
#' you must know the name of the certification you would like to check for as well as the Mechanical Turk ID 
#' for the worker of interest.
#'
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt 
#' @param cert The name of the certification given to the workers.
#' @param worker The MTurk worker ID you want to check for certification.
#'
#' @return TRUE or FALSE indicating if the worker has the certification.  
#'
#' @author David Carlson
#'
#' @examples
#' 
#' \dontrun{ 
#' x <- "ab1"
#' y <- c("a204")
#' check <- checkCert(email, password, x, y)
#' }
#'
#' @rdname checkCert
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @export

checkCert <- function(email, password, cert, worker){
  auth_token <- sentimentIt::authenticate(email, password)
  if(!is.character(cert) | nchar(cert)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(worker) | nchar(worker)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  mypost <- GET(paste0("https://www.sentimentit.com/api/certifications/", as.character(cert),
                       "/turk_workers/", as.character(worker), ".json?email=", email, "&auth_token=", auth_token))
  if(is.null(fromJSON(rawToChar(as.raw(mypost$content)))$allowed)){
    return(FALSE)
  }
  return(fromJSON(rawToChar(as.raw(mypost$content)))$allowed)
}

