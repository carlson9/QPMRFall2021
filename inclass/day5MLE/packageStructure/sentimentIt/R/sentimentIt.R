#' sentimentIt
#'
#' @description THIS FUNCTION IS IN THE BETA STAGE AND IS STILL A WORK IN PROGRESS.
#' 
#' \code{sentimentIt()} is a wrapper function of all base functionality. 
#'
#' @param email The researcher's email used for SentimentIt registration
#' @param password The researcher's password used for SentimentIt
#' @param read_documents_from File path of the text to be analyzed
#' @param write_documents_to Where to write the text with document IDs appended. Defaulted to NULL. For best functionality specify a csv.
#' @param what Argument passed to scan() function when reading in text data. Default is character. Only needed when index is NULL.
#' @param sep Argument passed to read.table() function when reading in data from a data frame. Default is line break. Only needed when index is not NULL.
#' @param quiet Argument passed to scan() function when reading in text data. Default is TRUE. Only needed when index is NULL.
#' @param index The index number of the table to extract the text from, or the name of the column. Default is NULL, indicating the text was not sent in a table.
#' @param which_source Source used within SentimentIt server assoicated with document uploads. Only used for later reference. Default is apiR.
#' @param number_per How many documents per batch to be compared. Default is 20.
#' @param task_setting_id ID of task setting to use for comparisons created on SentimentIt platform.
#' @param question The question the worker will be asked when comparing documents.
#' @param per_batch Number of comparisons per batch desired. Defaulted to 1000.
#' @param pairwise_path Path to save the created pairwise matrix of document IDs used for comparison. Default is pairwise.Rdata
#' @param certone The certification needed to complete tasks. Default is NULL. If a certification is needed in the SentimentIt platform, this is only needed if the researcher wishes to check and ban workers.
#' @param certtwo The certification granted if the worker is banned. Default is NULL
#' @param timed Logical indicating whether or not the batches should be sent to MTurk based on time rather than batch completion status. Default is TRUE, the recommended setting. Expired tasks will be reposted regardless.
#' @param time_per Time in hours to wait to post new batches. Default is 1.
#' @param mintime This is the earliest time in the day, in 24 hour time, to post batches. Default is 8. This is based on system time and therefore timezones ought be considered.
#' @param maxtime This is the latest time in the day, in 24 hour time, to post batches. Default is 22
#' @param rate Time, in hours, to wait to check if a batch is (near) completed. Only needed if not posting based on time. Default is 1/3.
#' @param threshold When posting batches based on completion status, this is the number of comparisons remaining at which point the batch can be considered (near) complete. Default is 5. This threshold is ignored after 4 hours, and the next batch is posted.
#' @param check_workers_at This is a vector of how often to check workers, e.g. \code{c(1,3)} would be after the first and third batch. Default is NULL and workers are not checked.
#' @param cut_point A cutoff point to classify posterior worker estimates. The proportion of posterior draws below \code{cut_point} is used to determine outliers. (Default is 1)
#' @param cut_proportion A cutoff proportion of posterior draws of worker estimates below \code{cut_point}. If the proportion of posterior coefficients below \code{cut_points} is higher than \code{cut_proportion}, a worker will be considered an outlier if answering more than the number of questions in \code{n.questions}. (Default is 0.9)
#' @param n.questions The number of questions an outlying worker has to answer in order to be banned. (Default is 50).
#' @param plot_hist If TRUE, plot the histogram of workers with a rug plot. Default is FALSE
#' @param hist_path Save the histogram to path and file name specified. Default is NULL and no plot is saved. If mutliple checks are performed the file will be overwritten unless a vector of paths is provided equal to the number of checks performed. If fewer are provided, the last one will be overwritten until completion.
#' @param rest_time The amount of time in seconds to wait to post a batch after the comparisons are created. This ensures the comparisons are ready before sending the request. Default is 60.
#' @param hierarchy_data A file that contains the variable that is used as a hierarchy group. Default is NULL and is only needed if heirarchical analysis is desired.
#' @param hierarchy_var A column name or number of the variable in \code{hierarchy_data} that is used as a hierarchy. Default is NULL and is only needed if heirarchical analysis is desired.
#' @param return_stan Return the Stan fit object from the final data analysis if TRUE as part of the return. Default is FALSE
#' @param stan_file The path and name of the file to save the Stan fitted model. Default is NULL and the fit is not saved.
#' @param return_data A logical indicating if the data from the tasks should be returned. Default is TRUE
#' @param data_file The directory and name of the file to save the HIT data. Default is NULL and the data is not saved
#' @param chains The number of chains to use for all Stan runs. Default is 3
#' @param iter The number of iterations for all Stan runs. Default is 2500
#' @param seed The seed to use for Stan runs. Default is 1234
#' @param n.cores Number of cores to be used in Stan runs. Default is 3. Note that, because this wrapper will generally run for extended periods of time, the researcher may want to leave some cores free for other processes
#' @param wait_to_repost Amount of time in hours to wait to repost expired tasks after all batches have been posted. Default is 2 hours. This this should be at least as long as the duration of the tasks set in the GUI, as only those tasks that have not been completed by this time are considered expired. This is also the time that the system will wait after reposting to download the complete data if any are expired.
#' @param ... Additional arguments passed to either scan() or read.table() depending on type of data used
#'
#' @return sentimentItOut A list with the requested information returned. The data with document IDs, merged with document estimates and hierarchical estimates if applicable, will always be returned, regardless if saved elsewhere.
#'
#' @examples
#'
#' \dontrun{
#' data(reviews)
#' movies <- sentimentIt(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    read_documents_from = reviews,
#'    write_documents_to = 'ReviewsWithIds.csv',
#'    index = 'Review', task_setting_id = 8,
#'    number_per = 10,
#'    question = 'Below is text taken from
#'        two movie reviews. Please
#'        choose the text that you
#'        think comes from the most
#'        positive review',
#'    pairwise_path = 'Comparisons.Rdata',
#'    certone = 'snippets', certtwo = 'bannedmovie_reviews',
#'    timed = FALSE, check_workers_at = c(1,2),
#'    rest_time = 60, rate = 1/3, threshold = 5,
#'    return_stan = TRUE, return_data = TRUE)
#' }
#' @seealso \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}} \code{\link{signout}}
#' @rdname sentimentIt
#' @export
sentimentIt <- function(email, password, read_documents_from,
    write_documents_to = NULL,
    what = 'character', sep = '\n', quiet=TRUE, index=NULL, which_source='apiR',
    number_per=20, task_setting_id, question, per_batch=1000, pairwise_path='pairwise.Rdata',
    certone=NULL, certtwo=NULL, timed = TRUE, time_per = 1,
    mintime=8, maxtime=22, 
    rate=1/3, threshold=5, check_workers_at=NULL,
    cut_point=1, cut_proportion=0.9, n.questions=50, plot_hist=FALSE, hist_path=NULL,
    rest_time=60, hierarchy_data=NULL, hierarchy_var=NULL,
    return_stan=FALSE, stan_file=NULL,
    return_data=TRUE, data_file=NULL,
    chains=3, iter=2500, seed=1234, n.cores=3, 
    wait_to_repost=2, ...){

  textDoc <- readText(email, password, read_documents_from, write_documents_to, what, sep, quiet, index, which_source, ...)
  
  # num batches is created from length of ids * number of comparisons / number per batch
  num_batches <- ceiling(length(unique(textDoc$ids)) * number_per / per_batch / 2)
  batches <- createBatches(email, password, task_setting_id, num_batches)

  # creates comparisons attached to the created batches.
  makeComps(email, password, ids=unique(textDoc$ids), number_per, batch_id=batches, question, per_batch, 
                         pairwise_path)
  Sys.sleep(rest_time)

  # Create Tasks for each of the created batches
  if(timed){
   .createTasksTimed(email, password, batches=batches, time_per=time_per, mintime=mintime, maxtime=maxtime,
                    check_workers_at=check_workers_at, certone=certone, certtwo=certtwo,
                    hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var, cut_point=cut_point, cut_proportion=cut_proportion,
                    n.questions=n.questions, plot_hist=plot_hist, hist_path=hist_path,
                    chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  }else{

    .createTasksBatch(email, password, batches=batches, certone=certone, certtwo=certtwo, mintime=mintime, maxtime=maxtime,
rate=rate, threshold=threshold, check_workers_at=check_workers_at,
cut_point=cut_point, cut_proportion=cut_proportion,
n.questions=n.questions, plot_hist=plot_hist, hist_path=hist_path,
                   hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,  
                   chains=chains, iter=iter, seed=seed, n.cores=n.cores, wait_to_repost=wait_to_repost)
  }
  Sys.sleep(wait_to_repost*3600)
  torepost <- NULL  
  for(i in batches){
    status <- batchStatus(email, password, i)
    if(status$submitted_count - status$completed_count > 0){
      torepost <- c(torepost, i)
    }
  }
  if(!is.null(torepost)){
    repostExpired(email, password, torepost)
    Sys.sleep(wait_to_repost*3600)
  }
  
  sentimentItOut <- list()
  
  stanfit <- .stanWrapper(email, password, data=batches,
                          hierarchy_data=hierarchy_data, hierarchy_var=hierarchy_var,
                          return_fit=TRUE, cut_point=cut_point, cut_proportion=cut_proportion,
                          n.questions=n.questions, plot_hist=plot_hist, hist_path=hist_path[length(hist_path)],
                          chains=chains, iter=iter, seed=seed, n.cores=n.cores)
  if(!is.null(stan_file)) save(stanfit, file=stan_file)
  if(return_stan) sentimentItOut$stanfit <- stanfit
  if(return_data | !is.null(data_file)){
    data <- readInData(email, password, batches)
    if(return_data) sentimentItOut$data <- data
    if(!is.null(data_file)) save(data, file=data_file)
  }
  alphas = stanfit$alphaPosts[,c('ids','mean')]
  textDoc = merge(textDoc, alphas, by='ids')
  if(!is.null(hierarchy_data) & !is.null(hierarchy_var)){
    hierPosts = stanfit$tPosts
    for(id in hierPosts[,'hier_ids']){
      textDoc[textDoc[,hierarchy_var] == id,'hier_est'] = hierPosts[hierPosts[,'hier_ids'] == id, 'mean']
    }
  }
  sentimentItOut$textDoc = textDoc
  signout(email, password)
  return(sentimentItOut)
}
