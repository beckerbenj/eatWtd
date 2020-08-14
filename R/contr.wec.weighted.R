#' Calculates contrasts for a weighted factor variable based on weighted effect coding
#'
#' Function works equivalent to \code{\link[wec]{contr.wec}} from the \code{wec} package, but allows
#' for weighted contrasts.
#'
#'
#'@param x Grouping variable of class factor
#'@param omitted Label of the factor label that should be taken as the omitted category
#'@param weights Numeric vector of non-negative weights
#'from \code{x} as well as from \code{weights} prior to variance estimation.
#'
#'@return Returns a contrast matrix based on weighted effect coding.
#'
#'@examples
#'### exemplary data according to wec paper
#'dat <- data.frame ( group = as.factor(c(rep(1,3), rep(2,2))), wgt = c(2/3, 4/3, 2, 3/8, 5/8))
#'### default contrasts
#'contrasts(dat[,"group"])
#'### weighted effect coding for weighted data
#'contr.wec.weighted(x= dat[,"group"], omitted=1,weights=dat[,"wgt"])
#'### equal to weighted effect coding: wec::contr.wec(x= dat[,"group"], omitted=1)
#'contr.wec.weighted(x= dat[,"group"], omitted=1,weights=rep(1, nrow(dat)))
#'
#'@export
contr.wec.weighted <- function (x, omitted, weights) {
  if (!identical(class(x), "factor")) {stop("Variable 'x' must be of class 'factor'.")}
  rawfreq <- table(x)
  if ( length( which(rawfreq == 0))>0) {warning("Drop ",length( which(rawfreq == 0))," empty level(s): '",paste(names(rawfreq)[which(rawfreq ==0)], collapse="', '"))}
  frequencies <- wtdTable(x, weights = weights)
  n.cat <- length(frequencies)
  omit  <- which(levels(droplevels(x)) == omitted)
  if ( length(omit)==0){
    stop("Level declared to be omitted ('",omitted,"') does not exist in 'x' or has zero observations.")
  }
  new.contrasts <- stats::contr.treatment(n.cat, base = omit)
  new.contrasts[omit, ] <- -1 * frequencies[-omit]/frequencies[omit]
  colnames(new.contrasts) <- names(frequencies[-omit])
  return(new.contrasts)}
