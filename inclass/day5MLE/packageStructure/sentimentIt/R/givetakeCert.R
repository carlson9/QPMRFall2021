#' @export
.givetakeCert <- function(email, password, certone, certtwo, workers){
  if(!is.character(certone) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(!is.character(certtwo) | nchar(certtwo)<1){
    stop("You must input a non-blank certification and one made of characters.")
  }
  if(certone == certtwo){
    stop("The certifications you are giving and taking away cannot be made the same.")
  }
  if(!is.character(workers) | nchar(workers)<1){
    stop("You must input a non-blank worker id and one made of characters.")
  }
  createCert(email, password, certtwo, workers)
  revokeCert(email, password, certone, workers)
}

