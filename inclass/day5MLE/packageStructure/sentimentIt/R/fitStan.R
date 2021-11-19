#' Fit a Stan model with results from SentimentIt
#' 
#' @details 
#' 
#' Before using this function, you must have created an Amazon Web Service and Mechanical Turk account. You
#' also must have created an account at SentimentIt.com. This function will not work otherwise. You also should
#' have run the readText() function to upload your data objects to the SentimentIt server. You should have also
#' run the createBatches() function and saved the batch ID numbers in your R workspace. You should have also
#' run the makeComps() function to save a portion of your total comparisons under each batch. You should have
#' also run the createTasks() function to send batches of paired data objects to Mechanical Turk for comparison.
#' You should have also run the readInData() function because this will give you the necessary data for the Stan
#' model.
#' 
#' Once you have sent batches of comparisons to Mechanical Turk and retrieved the results, you can estimate the 
#' reliability of the workers completing each task using a Stan model. For more information on Stan models, refer 
#' to page 7 of the following paper: https://www.sentimentit.com/SentimentIt.pdf (Carlson + Montgomery). 
#' 
#' You can fit either a non-hierarchical or a hierarchical Stan model. To estimate a non-hierarchical Stan model, 
#' use the `fitStan()` function. To estimate a hierarchical Stan model, use the `fitStanHier()` function.
#'
#' Fit a random utility model using Hamiltonian MCMC in Stan with data retrieved from the SentimentIt platform.
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#' 
#'
#' @param email The researcher's email used for SentimentIt registration. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param password The researcher's password used for SentimentIt. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param data A data set or a vector of batch numbers.
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Defalt is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return fit A list containing the following elements:
#' \itemize{
#' \item fit The Stan fit object for the model
#' \item alphaPosts A matrix with the full posteriors of the document estimates merged with the document IDs from the SentimentIt server
#' }
#'
#' @author David Carlson
#'
#' @examples
#'
#' \dontrun{
#' data(movieReviewOutput)
#' fit <- fitStan(data = movieReviewOutput) #can alternatively be batch IDs
#' fit <- fitStan(data = output)
#' }
#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname fitStan
#' @export
fitStan <- function(email=NULL, password=NULL, data, chains=3, iter=2500, seed=1234, n.cores=3){
  requireNamespace('rstan') #bug in rstan - needs explicit call
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = n.cores)
  requireNamespace('Rcpp')

  if(is.vector(data)){
    data <- readInData(email, password, data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension mismatches")
  }

  y <- data1$result[seq(1, dim(data1)[1], by=2)]
  z <- y
  z[z==0] <- -1
  data1$document_id_old <- data1$document_id
  data1$document_id <- as.numeric(as.factor(data1$document_id))
  g <- data1$document_id[seq(1, dim(data1)[1], by=2)]
  h <- data1$document_id[seq(1, dim(data1)[1], by=2) + 1]
  j <- as.numeric(as.factor(data1$worker_id[seq(1, dim(data1)[1], by=2)]))
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  model_code <- '
data {
int N; // number of comparisons
int M; // number of documents
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second itein comparison
int j[N]; // id map for workers
}
parameters {
real a[M];
real<lower=0> b[P];
real<lower=0> sigma;  
}
model {
sigma~normal(0,3);
for(p in 1:P){
b[p] ~ normal(0,sigma);
}
for(m in 1:M){
a[m] ~ normal(0,1);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'

  fit <- rstan::stan(model_code = model_code, data=c("y", "g", "h", "N", "M", "P", "j"),
              chains=chains, iter=iter, seed=seed)
  
  rhats = rstan::summary(fit)$summary[,'Rhat']
  if(any(rhats>1.1)) warning('The largest Rhat is ', max(rhats), ', consider increasing the number of iterations.')
  
  
  alphas = rstan::summary(fit)$summary[grep('a\\[',rownames(rstan::summary(fit)$summary)),]
  ids = unique(data1$document_id_old[order(data1$document_id_old)])
  alphaPosts = cbind(ids, alphas)
  
  return(list('fit'=fit, 'alphaPosts' = alphaPosts))
}
