#' Whenever the 'string' encounters @ symbol it will be replaced with 'replacement'; 'replacement' can be a vector
subst <- function(string, replacement, collapse=', '){len <- length(replacement); lis <- list()
      for(i in 1:len){lis[[i]] <- gsub('@',replacement[i], string)}
      return(paste(unlist(lis),collapse=collapse)) }

#' abbreviate the paste command to p
p <- function(..., sep=' ', collapse=NULL){paste(..., sep = sep, collapse = NULL)} 

#' evaluate a string
ev <- function(...){eval(parse(text=paste(c(...), collapse='')))}

#' set negative entries to zero
neg.is.zero <- function(x){ifelse(x < 0, 0, x)}

#' set NULL entries to zero
null.is.zero <- function(x){ifelse(is.na(x), 0, x)}
