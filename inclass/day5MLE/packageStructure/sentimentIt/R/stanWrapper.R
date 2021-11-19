#' @export
.stanWrapper <- function(email=NULL, password=NULL, data, hierarchy_data=NULL, hierarchy_var=NULL,
                        return_fit=FALSE, cut_point=1, cut_proportion=0.9,
                        n.questions=50, plot_hist=FALSE, hist_path=NULL,
                        chains=3, iter=2500, seed=1234, n.cores=3){

  if(is.vector(data)){
    data <- readInData(email, password, data)
  }

  data1 <- data

  if(dim(data1)[2] != 7){
    stop("data dimension mismatches")
  }

  # fit fit_stan or fit_stan_hier
  if(!is.null(hierarchy_data) & !is.null(hierarchy_var)){
    fit <- fitStanHier(data=data1, hierarchy_data=hierarchy_data,
                       hierarchy_var=hierarchy_var,
                       chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }
  else{
    fit <- fitStan(data=data1, chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }

  workerCheck <- checkWorkers(stan_fit=fit, data=data1, cut_point=cut_point,
                           cut_proportion=cut_proportion, n.questions=n.questions,
                           plot_hist=plot_hist, hist_path=hist_path)


  
  if(!is.null(hierarchy_data) & !is.null(hierarchy_var)){
    if(!return_fit){
      return(list(outlying_workers=workerCheck$ban_workers, worker_posteriors = workerCheck$worker_posteriors, stan_fit=NULL, alphaPosts=NULL, tposts=NULL))
    }else{
      return(list(outlying_workers=workerCheck$ban_workers, worker_posteriors = workerCheck$worker_posteriors, stan_fit=fit$fit, alphaPosts=fit$alphaPosts, tposts=fit$tPosts))
    }
  }else{
    if(!return_fit){
      return(list(outlying_workers=workerCheck$ban_workers, worker_posteriors = NULL, stan_fit=NULL, alphaPosts=NULL, tposts=NULL))
    }else{
      return(list(outlying_workers=workerCheck$ban_workers, worker_posteriors = workerCheck$worker_posteriors, stan_fit=fit$fit, alphaPosts=fit$alphaPosts, tposts=NULL))
    }
  }
}
