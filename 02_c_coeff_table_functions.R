#######I am going to override rethinking's internal coeftab function and assosicated things

# #override xcoef and make it use specified parameters
xcoef2 <- function( model , func=mean ) {
  int_params <- c("a" , "as" )
  yy <- as.vector(sort(model@stanfit@model_pars)) #list of model pars
  yy2 <- yy[which(str_detect(yy , "b_"))] #creates object that only contains charachters w/ b_ (i.e. slopes)
  yy3 <- c(int_params, yy2)
  result <- summary( model@stanfit , pars=yy3)$summary[,1]
  result
}




#override xse2 and make it use specified parameters
xse2 <- function( model ) {
    int_params <- c("a" , "as" )
    yy <- as.vector(sort(model@stanfit@model_pars)) #list of model pars
    yy2 <- yy[which(str_detect(yy , "b_"))] #creates object that only contains charachters w/ b_ (i.e. slopes)
    yy3 <- c(int_params, yy2 )
    result <- summary( model@stanfit , pars=yy3)$summary[,3]
    result
}


coeftab <- function (..., se = FALSE, se.inside = FALSE, nobs = TRUE, digits = 2, 
                     width = 7, rotate = FALSE) 
{
  if (se.inside == TRUE) 
    se <- TRUE
  L <- list(...)
  if (is.list(L[[1]]) && length(L) == 1) 
    L <- L[[1]]
  mnames <- match.call()
  mnames <- as.character(mnames)[2:(length(L) + 1)]
  param.names <- {
  }
  for (i in 1:length(L)) {
    c.names <- names(xcoef2(L[[i]]))
    param.names <- unique(c(param.names, c.names))
  }
  if (se == TRUE && se.inside == FALSE) {
    for (i in 1:length(L)) {
      kse.names <- paste(names(xcoef2(L[[i]])), ".se", sep = "")
      param.names <- unique(c(param.names, kse.names))
    }
  }
  nk <- length(param.names)
  d <- matrix(NA, ncol = nk)
  d <- data.frame(d)
  colnames(d) <- c(param.names)
  dse <- d
  for (i in 1:length(L)) {
    klist <- xcoef2(L[[i]])
    selist <- xse2(L[[i]])
    for (j in 1:length(klist)) {
      d[i, ][names(klist[j])] <- as.numeric(round(klist[j], 
                                                  digits))
      dse[i, ][names(klist[j])] <- as.numeric(selist[j])
    }
  }
  if (se == TRUE) {
    for (i in 1:length(L)) {
      kse <- xse2(L[[i]])
      names(kse) <- names(xcoef2(L[[i]]))
      for (j in 1:length(kse)) {
        if (se.inside == FALSE) 
          d[i, ][paste(names(kse)[j], ".se", sep = "")] <- as.numeric(round(kse[j], 
                                                                            digits))
        else d[i, ][names(kse)[j]] <- paste(formatC((d[i, 
        ][names(kse)[j]]), digits = digits), " (", 
        formatC(as.real(kse[j]), digits = digits), 
        ")", sep = "")
      }
    }
  }
  rownames(d) <- mnames
  if (se.inside == TRUE && se == TRUE) {
    comment(d) <- "Values in parentheses are quadratic estimate standard errors."
    colnames(d) <- paste(colnames(d), "(se)")
    for (i in 1:nrow(d)) {
      for (j in 1:ncol(d)) {
        d[i, j] <- ifelse(is.na(d[i, j]), "", d[i, j])
      }
    }
  }
  if (nobs == TRUE) {
    nobs <- 0
  }
  else {
    nobs <- 0
  }
  if (rotate == FALSE) {
    d <- t(d)
    dse <- t(dse)
  }
  new("coeftab", coefs = as.matrix(d), se = as.matrix(dse), 
      nobs = nobs, digits = digits, width = width)
}
