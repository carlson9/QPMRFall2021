#' Find/create documents and retrieve IDs
#' 
#' @details 
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise.
#' 
#' The readText() function imports the data to be used in comparisons and assigns unique ID 
#' numbers to each object in the data. The function call takes the data as an input, imports it into the 
#' SentimentIt repository, and assigns unique ID values to each object in the data. The function will then 
#' export the data, complete with ID numbers, to a specified file path. If any objects already exist in the 
#' repository, the ID number for the existing object will be used instead of importing a duplicate. 
#' 
#' Your data should contain the set of objects you want to perform pairwise comparisons on. It may contain 
#' other data as well, but you will need to specify which column contains the data to be used in the pairwise 
#' comparisons. The best practice for running this function is to first import your data into your R workspace 
#' before running the function.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#'
#'
#' @param email The researcher's email associated with the SentimentIt account.
#' @param password The researcher's password associated with the SentimentIt account.
#' @param read_documents_from The file path the data will be drawn form, or a vector of text.
#' @param write_documents_to The file path to write the original data, merged with the document IDs. Default is NULL and the results will not be saved, but only returned. For best functionality specify a csv.
#' @param what Argument passed to scan() function. Default is character. Only needed when index is NULL.
#' @param sep Argument passed to read.table() function. Default is line break. Only needed when index is not NULL.
#' @param quiet Argument passed to scan(). Default is TRUE. Only needed when index is NULL.
#' @param index The index number of the table to extract the text from, or the name of the column. Default is NULL, indicating the text was not sent in a table.
#' @param which_source Depricated
#' @param ... Additional arguments passed to either scan() or read.table() depending on type of data used
#'
#' @examples
#'
#' \dontrun{
#' data(reviews)
#'
#' docInfo <- readText(email = 'researcher@school.edu', password = 'uniquePassword', read_documents_from = reviews, write_documents_to = "ReviewsWithIds.csv", index = 'Review')
#' }
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @author David Carlson
#' @rdname readText
#' @export
readText <- function(email, password, read_documents_from, write_documents_to=NULL, what='character', sep='\n', quiet=TRUE, index=NULL, which_source='apiR', ...){
  if(!is.null(index)){
    if(!is.character(read_documents_from)){
      hold.table = read_documents_from
    }else{
      hold.table <- read.table(file=read_documents_from, sep=sep, ...)
    }
    textToSend <- hold.table[,index]
  }else textToSend <- scan(file=read_documents_from, what=what, sep=sep, quiet=quiet, ...)
  auth_token <- sentimentIt::authenticate(email, password)
  args <- mapply(function(x,y) list(text=x, source=y), textToSend, which_source, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  args <- toJSON(list("email"=email, "auth_token"=auth_token, "documents"=args), auto_unbox=TRUE)
  mypost <- POST('https://www.sentimentit.com/api/documents/find_or_create.json',
                 body = args, content_type_json(),
                 encode='json')
  ids <- unlist(fromJSON(rawToChar(as.raw(mypost$content))))
  textToSend <- gsub('\t',' ', textToSend)
  if(is.null(index)){
    if(!is.null(write_documents_to)) write.table(cbind(textToSend, ids), write_documents_to, sep='\t', row.names=FALSE)
    return(as.data.frame(cbind(textToSend, ids), stringsAsFactors=FALSE))
  }else{
    hold.table[,index] <- textToSend
    if(!is.null(write_documents_to)) write.table(cbind(hold.table, ids), write_documents_to, sep='\t', row.names=FALSE)
    return(as.data.frame(cbind(hold.table, ids), stringsAsFactors=FALSE))
  }
}
