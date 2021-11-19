#' sentimentIt
#'
#' The sentimentIt package interacts with the SentimentIt API to analyze text and capture latent traits of interest.
#' @name sentimentIt
#' @docType package
#' @author  David Carlson \email{carlson.david@@wustl.edu} and Jacob M. Montgomery \email{jacob.montgomery@@wustl.edu}
#' @details
#' The SentimentIt system is designed to estimate a researcher-specified latent trait of interest for text corpora. The system is introduced in Montgomery and Carlson ''A pairwise comparison framework for fast, flexible, and reliable human coding of political texts.'' The system crowd-sources pairwise comparisons of the documents, currently through Amazon Mechanical Turk. This is a cognitively simple task and much easier for workers to achieve reliably than thermometer-type coding or similar approaches. The workers are simply asked which of the two documents is further along some dimension of interest, e.g. positivity. Once a sufficient number of comparisons are done per document (when randomly selecting comparisons we find 20 is a reasonable number), the traits are estimated using a random utility model via Hamiltonian MCMC in Stan. This process generates both point estimates and full posterior distributions. This is a reliable and efficient way to estimate the underlying sentiment in text, a task often too difficult for machines and too time-consuming, costly, and unreliable for expert coders.
#'
#' This package is designed to allow nearly full implementation of the system. Some start-up is necessary outside of the R environment. To begin using SentimentIt, navigate to \url{https://www.sentimentit.com/}. Here you will find detailed information on setting up a SentimentIt account, setting up an MTurk account, linking the two, setting up certifications and training for workers, and creating HIT settings. Once these steps have been completed, all that is needed is the HIT Setting ID number you wish to use for your task. You can then proceed to use the package. The documentation also explains the purpose of these settings and guidance for best practices.
#'
#' First, the base-level functionality of the package is explained to give an intuition behind the process. We then show how the entire process can be done using the wrapper function \code{\link{sentimentIt}}. All argument names are the same in the wrapper function as they are in base-level functions. Data on Rotten Tomato movie reviews comes with the package (\code{\link{reviews}}) and we will use this data to demonstrate how to uncover the positivity of a movie review which can then be benchmarked to the stars the reviewer provided.
#'
#' We begin by reading in the data using the \code{\link{readText}} function. If we want to use the movie review data in the package, we could run the following code:
#'
#' \code{data(\link{reviews})}
#'
#' \code{docInfo <- \link{readText}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    read_documents_from = reviews,
#'    write_documents_to = "ReviewsWithIds.csv",
#'    index = 'Review')}
#'
#' This would load the \code{\link{reviews}} data, read the text from the column specified by \code{index} (which can alternatively be a numeric for the column), put the text on the SentimentIt server, which will return unique ID numbers for the documents, and the function \code{\link{readText}} will both return a dataframe with the inputted data with the IDs appended, and write this data frame to "ReviewsWithIds.csv". View the function documentation for all possible arguments, which, again, have the same names as the higher level wrapper function.
#'
#' Now that the data is on the server and we have the document IDs, we can create the batches of pairwise comparisons. We start with 10 comparisons per document, leading to 2,500 comparisons. We wish to send them out in batches of 1,000, so we need three batches. We run the following function:
#'
#' \code{batch_ids <- \link{createBatches}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    task_setting_id = 8,
#'    num_batches = 3)}
#'
#' This assigns \code{batch_ids} to the batch identification numbers returned from the server. The first argument following authentication, \code{task_setting_id} refers to the HIT setting we wish to use. These can be set on our server using a simple form to indicate what certification (if any) is required, how long workers have to complete the task once they have started, how much money they should be paid, and how long HITs should be posted for completion before expiration. In this case, the HIT setting requires a training certification, pays the workers $0.04 per HIT, allows the worker to take up to an hour answering the question, and leaves the HIT active on MTurk for 2 hours. This also dictates what the workers will see before selecting the HIT, which should include the URL to the certification if a certification is used.  If the workers follow the link and successfully complete the training, their worker ID will be automatically added to the list of approved workers and complete the associated HITs. If they fail the certification, they are banned from retaking the training or answering HITs associated with this setting.
#'
#' Now that the batch settings are specified, we can create 10 random pairwise comparisons. We first need to retrieve the document IDs created earlier, written to the file specified, then create the comparisons using these IDs. The following code will set up the comparisons:
#'
#' \code{\link{makeComps}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    ids = docInfo[,'ids'],
#'    number_per = 10,
#'    batch_id = batch_ids,
#'    question = 'Below is text taken from two movie reviews.
#'            Please choose the text that you think comes
#'            from the most positive review',
#'    pairwise_path = 'Comparisons/first10.Rdata')}
#'
#' The first argument, \code{ids}, indicates the numerical IDs for the documents. The argument \code{number_per} is the number of comparisons desired (in this case 10). The \code{batch_id} argument indicates the batch IDs to be used for the HITs. The \code{question} argument specifies the question the worker will see once the worker selects the HIT. There is an argument \code{per_batch} indicating the number of comparisons per batch desired, defaulted to 1,000. If the number of comparisons is not a multiple of this number, the final batch will have fewer comparisons. We have 2,500 comparisons, so the final batch only has 500 comparisons. The number of comparisons per batch could have been automatically determined, but forcing this number to be provided ensures no mistakes are made, such as providing too few batches. The \code{pairwise_path} argument is used to save the comparisons that were created to a specified path and file name where the comparisons should be stored. The function returns the batch IDs as returned by the server (which we already have stored). This serves as an assurance that the function has been correctly called. Full argument documentation is available under \code{\link{makeComps}}, and all arguments have the same name as the wrapper.
#'
#' Now that the comparisons are set up and on the server, we can post them as HITs to AMT. If we wish to send our first batch, we run:
#'
#' \code{\link{createTasks}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    batch_id = batch_ids[1])}
#'
#' The argument \code{batch_id} is the identification number for the batch that we want to send, which we retrieved from the call to \code{\link{createBatches}}. Full documentation for the arguments is available under \code{\link{createTasks}}.
#'
#' At this point, we want to occasionally check the status of a batch. That is, how many of the comparisons have been completed. To do this we run the function:
#'
#' \code{\link{batchStatus}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    batch_id = batch_ids[1])}
#'
#' The only argument to this function, other than authentication, is \code{batch_id}, the ID of the batch you wish to check. This could be a vector of batch IDs. This returns a dataframe with the batch ID and the number of comparisons total, submitted, completed, and expired.
#'
#' Once the batch is completed (or near completed), we can check the workers to find any that are deviant. First, we need to read in the data from the server. We accomplish this by running:
#'
#' \code{output <- \link{readInData}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    batch_id = batch_ids[1])}
#'
#' The argument to this function is \code{batch_id}, which could be a scalar or a vector of batch ID numbers. The returned output is a data frame with the following columns: batch_id, comparison_id, document_id, result, hit_id, worker_id, and completed_at.  The data is organized by document-comparison, and the result is an indicator if the document was chosen over the other document in the given comparison. (There are, therefore, two rows for every comparison conveniently grouped by comparison so every odd row is immediately followed by an entry for the other document in the same comparison.) The worker_id is the AMT identification number of the worker, important for keeping track of the performance of workers to determine deviant (or highly reliable) workers. Finally, completed_at is a time stamp for the HIT completion time. This can be used to determine how quickly workers are completing tasks. If a worker is finishing HITs in a very fast amount of time this may suggest the worker is simply clicking through as fast as possible. In our experience only a very small proportion of workers do this, and it is quite obvious if workers are simply providing insincere evaluations.
#'
#' We now need to fit the Stan model to estimate worker reliability. For the non-hierarchical model, which we used in this example, we run:
#'
#' \code{fit <- \link{fitStan}(data = output)}
#'
#' The data can alternatively be a (vector of) batch number(s) rather than actual data, allowing the researcher to skip the earlier step. See the documentation here or under \code{\link{fitStan}} for a full list of arguments. The latter arguments are all used in the call to Stan through \code{\link{rstan}}.
#'
#' There is a related function that fits the hierarchical Stan model, \code{\link{fitStanHier}}. The arguments are the same as \code{\link{fitStan}}, but with two additional arguments, \code{hierarchy_data} and \code{hierarchy_var}. In order to fit the hierarchical model, the data used to set up the documents and comparisons needs to be provided in order to map the document IDs to their respective higher-level grouping. The higher-level group could be long documents while the lower level is paragraphs within the documents.
#'
#' Once the model is fitted, we can check for outlier workers. To do so, we run:
#'
#' \code{ban_workers <- \link{checkWorkers}(stan_fit = fit, data = output)}
#'
#' The first argument is the \code{rstan} object obtained from the previous step, and the data is the output from the batch(es). The function has optional arguments with defaults. Read the documentation here or under \code{\link{checkWorkers}}.
#'
#' We want to revoke the certification for these workers and add them to the list of banned workers for this task. We keep track of banned workers by granting them a different certification that indicates their certifications have been revoked. This is also how we keep track of workers that fail the qualification. First, to revoke the certification, we run:
#'
#' \code{\link{revokeCert}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    cert = 'snippets', workers = ban_workers)}
#'
#' The \code{cert} argument is the name of the certification as used on the server, which in this case is titled snippets for movie reviews. The next argument is a vector of worker IDs obtained from the previous step. We now add them to banned list:
#'
#' \code{\link{createCert}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    cert= 'bannedmovie_reviews', workers = ban_workers)}
#'
#' This will grant the workers the certification bannedmovie_reviews which indicates they can no longer participate in HITs requiring the snippets qualification.
#'
#' We can now proceed with posting the other batches, checking workers whenever we choose. Once all the batches are completed, we find it common that a few HITs remain incomplete with a few HITs per batch overlooked. We can then run:
#'
#' \code{\link{repostExpired}(email = 'researcher@school.edu',
#'    password = 'uniquePassword',
#'    batch_id = batch_ids)}
#'
#' This will repost all of the expired HITs from the vector of batch IDs. Finally, we can run \code{\link{readInData}} and \code{\link{fitStan}} to retrieve final estimates of all the data.
#'
#' Finally, after all the data has been retrieved, run \code{\link{signout}(email, password)} to sign out of the session. The function \code{\link{authenticate}} signs the researcher in, but this function is called internally and there should be no reason the researcher would need this functionality, unless using the API directly. Note that if the qualification survey remains active, the researcher will be signed back in if the survey is taken. This should not cause issues but taking a survey offline will prevent this from happening.
#'
#' The functionality discussed to this point is the lowest level of functionality, where the researcher has the most control. All of this functionality can be automated through the wrapper function, allowing new batches to be posted either as a function of time or as batches are (nearly) completed. The example provided in the Examples section would perform the following:
#' \itemize{
#' \item All documents in the \code{\link{reviews}} dataframe will be read in and passed to our servers.
#' \item HIT settings for the task will be assigned (question wording, compensation, duration, etc.) and the snippet certification will be required.
#' \item Ten random pairwise comparisons per document will be created.
#' \item All unique identifiers for documents will be stored at ReviewsWithIds.csv
#' \item Comparisons will be posted to AMT in batches of 1,000.
#' \item New batches will be posted once the current batch is completed up until the \code{threshold} of five incomplete comparisons. Completion status will be checked every \code{rate=1/3} of an hour.
#' \item A Stan model with three chains and 2,500 iterations will be fit when the workers are checked and when the final data is analyzed.
#' \item Workers will be evaluated after the first 1,000 and second 1,000 comparisons are complete, and workers with \code{0.9} of the posterior draws falling below the default cut point of \code{b_k=1}, will be banned from completing future tasks.
#' \item After posting comparisons, the function will wait \code{rest_time = 60} seconds before posting HITs to Mechanical Turk to ensure all of the comparisons are ready to be posted.
#' \item All incomplete tasks will be re-posted.
#' \item After all tasks are completed, the data and Stan estimates of all model parameters will be returned and the researcher will be signed out.
#' }
#'
#' There are several advantages to this higher-level approach.  First and foremost, this functionality makes the process of interacting with online workers simple and efficient from the perspective of a researcher. Once the qualifications and HIT settings have been created, a single command can supervise the collection of worker evaluations and the creation of a meaningful measure, even if this process requires several days. However, a further advantage of this approach is that it makes the process of turning text into data highly replicable.  Researchers wishing to evaluate the reliability of any measure can simply re-run the task using the original call to create a replication of the original measure. Thus, the SentimentIt platform has the potential to bring about a higher degree of transparency to the task of turning natural language into meaningful data.
#'
#' Some important things to note, however:
#' \itemize{
#' \item The wrapper function will take over your R session, so you'll need to start a new session to continue using R or run this from a terminal. If running a Linux/Unix environment the software \code{screen} or \code{tmux} can allow running commands from the terminal and will continue running even if the terminal window closes.
#' \item It is recommended that all saving options are utilized. If a power outage or system crash interrupts the process having the information saved will greatly ease starting it back up.
#' \item Because Stan will intermittently run on the specified number of cores, your machine may lose computing power throughout the process. Consider this when specifying the number of cores.
#' }
NULL
