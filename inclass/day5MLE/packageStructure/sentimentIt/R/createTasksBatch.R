#' @export
.createTasksBatch <- function(email, password, batches, certone, certtwo, mintime, maxtime,
rate, threshold, check_workers_at,
cut_point, cut_proportion,
n.questions, plot_hist, hist_path,
                   hierarchy_data, hierarchy_var,  
                   
                   chains, iter, seed, n.cores, wait_to_repost){
  out <- vector()
  q <- 1
  for(i in batches){
    .checkTime(mintime, maxtime)
    x <- createTasks(email, password, batch_id=i)
    out[i] <- x
    done <- FALSE
    current <- Sys.time()
      while(!done){
        Sys.sleep(rate*3600)
        status <- batchStatus(email, password, i)
        done <- (((status$submitted_count - status$completed_count) <= threshold) | (as.difftime(Sys.time() - current, units='mins') > (wait_to_repost*60)))
      }
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

