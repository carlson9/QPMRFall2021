#' Signs the researcher out of the session
#'
#' This function will revoke the authorization token and sign the researcher out of the session. However, if the certification surveys remain active, if someone takes the survey the researcher will be signed back in.
#'
#' @param email The email used by the researcher to register with SentimentIt.
#' @param password The password associated with the account.
#'
#' @return success TRUE if sign out succeeded. Function will be called recursively until success.
#'
#' @author David Carlson
#'
#' @rdname signout
#' @seealso \code{\link{sentimentIt}} \code{\link{authenticate}} \code{\link{batchStatus}} \code{\link{checkCert}} \code{\link{checkWorkers}} \code{\link{createBatches}} \code{\link{createCert}} \code{\link{createPairwise}} \code{\link{createTasks}} \code{\link{fitStan}} \code{\link{fitStanHier}} \code{\link{makeComps}} \code{\link{readInData}} \code{\link{readText}} \code{\link{repostExpired}} \code{\link{reviews}} \code{\link{revokeCert}}
#' @export
signout <- function(email, password){
  auth_token <- sentimentIt::authenticate(email, password)
  args <- list(email = email, auth_token = auth_token)
  args <- toJSON(args, auto_unbox=TRUE)
  mydelete <- DELETE(paste0('https://www.sentimentit.com/api/sessions/sign_out.json?email=', email, '&auth_token=',
                          auth_token), content_type_json(),
                   encode='json')
  out <- fromJSON(rawToChar(as.raw(mydelete$content)))
  if(!out$success){
    email <- readline('Invalid email or password. Re-enter email: ')
    password <- readline('Re-enter password: ')
    return(signout(email, password))
  }
  return(out$success)
}

