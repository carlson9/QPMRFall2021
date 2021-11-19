#' @export
.createTasksTimed <- function(email, password, batches, time_per, mintime,
                            maxtime, certone, certtwo, check_workers_at=NULL,
                            hierarchy_data=NULL, hierarchy_var=NULL,
                       cut_point=1, cut_proportion=0.9,
                            n.questions=50, plot_hist=FALSE, hist_path=NULL,
                            chains=3, iter=2500, seed=1234, n.cores=3){
  out <- vector()
  q <- 1
  for(i in batches){
    .checkTime(mintime, maxtime)
    x <- createTasks(email, password, batch_id=i)
    out <- c(out, x)
    Sys.sleep(time_per*3600)
    if(i %in% batches[check_workers_at]) {
        hist_pathi <- hist_path[q]
       .givetakeCert(email, password, certone, certtwo, .stanWrapper(email, password, data=batches[1:length(out)],
                                                  hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                                                  return_fit=FALSE, cut_point=cut_point, cut_proportion=cut_proportion,
                                                  n.questions=n.questions, plot_hist=plot_hist, hist_path=hist_pathi,
                                                  chains=chains, iter=iter, seed=seed, n.cores=n.cores)$outlying_workers)
        q <- ifelse(length(hist_path)>q, q+1, q)
    }
  }
  return(out)
}

