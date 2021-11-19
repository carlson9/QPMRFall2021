#' Fit a Stan hierarchical model with results from SentimentIt
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
#' to page 7 of the following paper: \url{https://www.sentimentit.com/SentimentIt.pdf} (Carlson + Montgomery). 
#' 
#' You can fit either a non-hierarchical or a hierarchical Stan model. To estimate a non-hierarchical Stan model, 
#' use the `fitStan()` function. To estimate a hierarchical Stan model, use the `fitStanHier()` function.
#'
#' Fit a multilevel random utility model using Hamiltonian MCMC in Stan with data retrieved from the SentimentIt platform.
#' 
#' The function will remove repeated documents from the hierarchical data, for example paragraphs that are
#' identical. To deal with repeated documents in order to include them in the hierarchical modeling, see
#' the replication materials for the Human Rights application found at:
#' \url{https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/0ZRGEE}
#' 
#' Reference Paper: Carlson, David and Jacob M. Montgomery. Forthcoming. “A Pairwise Comparison Framework for 
#' Fast, Flexible, and Reliable Human Coding of Political Texts.” American Political Science Review.
#' 
#'
#' @param email The researcher's email used for SentimentIt registration. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param password The researcher's password used for SentimentIt. Default is NULL and only needs to be provided if batch numbers are used instead of data.
#' @param data A data set or a vector of batch numbers.
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy, with column ids filled with SentimentIt document IDs (created from \code{readText()}).
#' @param hierarchy_var A name of the variable in \code{hierarchy_data} that is used as a hierarchy.
#' @param chains The number of chains. (Default is 3)
#' @param iter The number of iteration. (Default is 2500)
#' @param seed Set seed. (Default is 1234)
#' @param n.cores Number of cores to be used in stan fit. (Default is 3)
#'
#' @return fit_hier A list containing the following elements:
#' \itemize{
#' \item fit The Stan fit object for the model
#' \item alphaPosts A matrix with the full posteriors of the lower-level estimates merged with the document IDs from the SentimentIt server
#' \item tPosts A matrix with the full posteriors of the higher-level estimates merged with the hierarchical identifier
#' }
#'
#' @author David Carlson
#' 
#' @examples
#'
#' \dontrun{
#' data(humanRightsOutput)
#' data(humanRightsDocs)
#' fit <- fitStanHier(data = humanRightsOutput, #can alternatively be batch IDs
#'          hierarchy_data = humanRightsDocs,
#'          hierarchy_var = 1, # can alternatively be a column name
#'           iter=8500) # this example requires more iterations
#' }
#'

#'
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname fitStanHier
#' @export
fitStanHier <- function(email=NULL, password=NULL, data, hierarchy_data, hierarchy_var,
                        chains=3, iter=2500, seed=1234, n.cores=3){
  requireNamespace('rstan') #bug in rstan - needs explicit call
  rstan::rstan_options(auto_write = TRUE)
  options(mc.cores = n.cores)
  requireNamespace('Rcpp')

  if(is.vector(data)){
    data <- readInData(email, password, data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension is incorrect")
  }
  
  hierarchy_data = hierarchy_data[!duplicated(hierarchy_data$ids),]

  y <- data1$result[seq(1, dim(data1)[1], by=2)]
  data1$document_id_old <- data1$document_id
  data1$document_id <- as.numeric(as.factor(data1$document_id))
  g <- data1$document_id[seq(1, dim(data1)[1], by=2)]
  h <- data1$document_id[seq(1, dim(data1)[1], by=2) + 1]
  j <- as.numeric(as.factor(data1$worker_id[seq(1, dim(data1)[1], by=2)]))
  k <- as.numeric(as.factor(as.character(hierarchy_data[,hierarchy_var])))
  M <- length(unique(c(g, h)))
  N <- length(y)
  P <- length(unique(j))
  D <- length(unique(k))
  g <- as.numeric(as.factor(g))
  h <- as.numeric(as.factor(h))
  k <- as.numeric(as.factor(k))
  model_code <- '
data {
int N; // number of comparisons
int M; // number of paragraphs
int D; // number of documents (countries)
int P; //Number of coders
int y[N]; // outcome
int g[N];    // id  map first item in comparison
int h[N];    // id map of second item in comparison
int j[N]; // id map for workers
int k[M]; // id map for documents (countries) relating to documents
}
parameters {
real a[M]; // paragraphs
real t[D]; // documents (countries)
real<lower=0> b[P];
real<lower=0> sigmac[D];
}
model {
for(p in 1:P){
b[p] ~ normal(0,1);
}
for(d in 1:D){
t[d] ~ normal(0,1);
sigmac[d] ~ normal(0,.5);
}
for(m in 1:M){
a[m] ~ normal(t[k[m]],sigmac[k[m]]);
}
for(n in 1:N) {
y[n] ~ bernoulli(inv_logit(b[j[n]]*(a[g[n]]-a[h[n]])));
}
}'

  fit_hier <- rstan::stan(model_code = model_code,
              data=c("y", "g", "h", "N", "M", "P", "j", "D", "k"),
              chains=chains, iter=iter, seed=seed, control=list(max_treedepth=50))
  
  rhats = rstan::summary(fit_hier)$summary[,'Rhat']
  if(any(rhats>1.1)) warning('The largest Rhat is ', max(rhats), ', consider increasing the number of iterations.')
  
  
  alphas = rstan::summary(fit_hier)$summary[grep('a\\[',rownames(rstan::summary(fit_hier)$summary)),]
  ids = unique(data1$document_id_old[order(data1$document_id_old)])
  alphaPosts = cbind(ids, alphas)
  
  ts = rstan::summary(fit_hier)$summary[grep('t\\[',rownames(rstan::summary(fit_hier)$summary)),]
  hier_ids[unique(k)] = as.character(unique(hierarchy_data[,hierarchy_var]))
  tPosts = data.frame(hier_ids, ts, stringsAsFactors=FALSE)
  
  
  return(list('fit' = fit_hier, 'alphaPosts' = alphaPosts, 'tPosts' = tPosts))
}

