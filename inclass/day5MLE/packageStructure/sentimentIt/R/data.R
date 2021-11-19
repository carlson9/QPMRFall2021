#' Movie reviews example data set
#'
#' An example dataset of Rotten Tomato movie reviews with the number of stars given by the user. This is the first application in the associated paper introducing SentimentIt.
#'
#' @format A data frame with 500 rows and 2 columns:
#' \describe{
#'   \item{Stars}{rating on scale from 1-5 by Mechanical Turk worker}
#'   \item{Review}{The movie review looked over by worker}
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
#' @source \url{https://www.rottentomatoes.com}
"reviews"

#' Movie reviews example comparison output from server
#'
#' An example output of comparisons created from the SentimentIt system from the movie reviews data set
#'
#' @format A data frame with 19854 rows and 7 columns:
#' \describe{
#'   \item{batch\_id}{The batch ID number from the SentimentIt server}
#'   \item{comparison\_id}{The comparison ID number from the SentimentIt server for the unique comparison}
#'   \item{document\_id}{The SentimentIt ID number for the document in the comparison}
#'   \item{result}{This will be a 1 if this document was chosen in the comparison, and 0 otherwise}
#'   \item{hit\_id}{This is the ID used by Amazon identifying the HIT, which should not be needed by the researcher}
#'   \item{worker_id}{This is the ID of the MTurk worker who completed the HIT. THis is used in the package to identify and assess workers}
#'   \item{completed\_at}{This is the time marker for the time the HIT was competed on MTurk. This can be used to assess the speed MTurkers are completing the HITs}
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
"movieReviewOutput"

#' Human rights example comparison output from server
#'
#' An example output of comparisons created from the SentimentIt system from the human rights data set
#'
#' @format A data frame with 32092 rows and 7 columns:
#' \describe{
#'   \item{batch\_id}{The batch ID number from the SentimentIt server}
#'   \item{comparison\_id}{The comparison ID number from the SentimentIt server for the unique comparison}
#'   \item{document\_id}{The SentimentIt ID number for the document in the comparison}
#'   \item{result}{This will be a 1 if this document was chosen in the comparison, and 0 otherwise}
#'   \item{hit\_id}{This is the ID used by Amazon identifying the HIT, which should not be needed by the researcher}
#'   \item{worker_id}{This is the ID of the MTurk worker who completed the HIT. THis is used in the package to identify and assess workers}
#'   \item{completed\_at}{This is the time marker for the time the HIT was competed on MTurk. This can be used to assess the speed MTurkers are completing the HITs}
#' }
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
"humanRightsOutput"

#' Human rights text data with country IDs and document IDs
#'
#' The data set including country torture text, created once the documents were uploaded to the server. Used in fitStanHier() example.
#'
#' @format A data frame with 1652 rows and 4 columns:
#' \describe{
#'   \item{V1}{The country iso3 code}
#'   \item{V2}{The country name}
#'   \item{V3}{The paragraphs of the human rights reports}
#'   \item{ids}{The document IDs created by the server}
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{revokeCert}} \code{\link{signout}}
"humanRightsDocs"

