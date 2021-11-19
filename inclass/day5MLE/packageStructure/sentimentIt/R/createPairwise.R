#' Creates a matrix of random pairs for comparison
#' 
#' @details The createPairwise() function randomly selects pairs of ID numbers for objects to be compared. 
#' This function will return a matrix with two columns, each containing the 
#'
#' This function randomly samples from all possible pairings of IDs. In other words, no comparison will be duplicated.
#'
#' @param ids The id numbers of the texts you want to use.
#' @param number_per How many documents per batch to be compared.
#' @param pairwise_path Path to save the created pairwise matrix. Default is pairwise.Rdata
#'
#' @return A matrix of random pairwise comparisons without repitition.
#'
#' @author David Carlson
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname createPairwise
#' @export
createPairwise <- function(ids, number_per, pairwise_path='pairwise.Rdata'){
  if(!is.numeric(number_per)){
    stop("number_per should be numeric")
  }

  documents <- unique(as.numeric(ids))
  # This code sets up the random pairwise comparisons
  pairwise<-cbind(rep(documents, (number_per+1)%/%2), matrix(replicate((number_per+1)%/%2, sample(documents)), ncol=1))
  duplicates<-pairwise[which(pairwise[,1]==pairwise[,2]),]

  # some ugly code to keep documents from being compared with themselves
  if(!is.null(nrow(duplicates))){
    while(nrow(duplicates)>1){
      duplicates[,1]<-duplicates[sample(1:nrow(duplicates)),1]
      pairwise[which(pairwise[,1]==pairwise[,2]),]<-duplicates
      duplicates<-matrix(pairwise[which(pairwise[,1]==pairwise[,2]),], ncol=2)
    }
    if(nrow(duplicates)==1){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }else{
    if(is.vector(duplicates)){
      oneIndex<-which(pairwise[,1]==pairwise[,2])
      oneValue<-pairwise[oneIndex,2]
      twoIndex<-sample(c(1:nrow(pairwise))[pairwise[,2]!=unique(c(duplicates))], 1)
      twoValue<-pairwise[twoIndex,2]
      pairwise[oneIndex,2]<-twoValue
      pairwise[twoIndex,2]<-oneValue
    }
  }
  save(pairwise, file=pairwise_path)
  return(pairwise)
}

