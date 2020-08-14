#' Weighted heterogeneous correlation matrix.
#'
#' Computes a weighted heterogeneous correlation matrix, consisting of Pearson
#'product-moment correlations between numeric variables, polyserial correlations between
#'numeric and ordinal variables, and polychoric correlations between ordinal variables.
#'
#' Variables in the data.frame should be accordingly classified as numeric or factor variables.
#'Function resembles the \code{hetcor} function from the \code{polycor} package, but allows
#'for incorporating weights. For this purpose, the function makes use of the \code{weightedCorr}
#'function from the \code{wCorr} package.
#'
#'@param dataFrame a data.frame containing all variables
#'@param vars character or numeric vector indicating the variables for which a correlation table should
#'be computed. If \code{NULL}, all variables in the data.frame will be used.
#'@param weights Numeric vector of non-negative weights. If \code{NULL}, equally weighted cases are
#'assumed, i.e. all weights are defaulted to 1.
#'@param out Specifies the output format. \code{"wide"} gives a classical correlation matrix,
#'\code{"long"} gives a long format table which includes the type of correlation.
#'
#'@return a correlation table or a list
#'
#'@examples
#'data(mtcars)
#'# create arbitrary weights
#'mtcars[,"weight"] <- abs(rnorm(nrow(mtcars), 10,5))
#'# choose variables
#'vars <- c("mpg", "cyl", "hp")
#'# inappropriate classes: variables which are inherently ordinal, have the 'wrong'
#'# class 'numeric'. (We use only the first imputation of the data set.)
#'sapply(mtcars[,vars], class)
#'mtcars[,"cyl"] <- as.factor(mtcars[,"cyl"])
#'wtdHetcor(mtcars, vars = vars, out = "long")
#'wtdHetcor(mtcars, vars = vars, weights = "weight", out = "long")
#'
#'@export
### wenn vars gleich NULL; werden alle Variablen genommen
wtdHetcor <- function ( dataFrame, vars=NULL, weights=NULL, out = c("wide", "long", "both")  ) {
        out    <- match.arg(arg = out, choices = c("wide", "long", "both"))
        if(!"data.frame" %in% class(dataFrame)) {stop("Object 'dataFrame' must be of class 'data.frame'.")}
        allVars<- list(vars = vars, weights = weights)
        if(is.null(vars)) {vars <- colnames(dataFrame)}
        allNam <- lapply(allVars, FUN=function(ii) {eatTools::existsBackgroundVariables(dat = dataFrame, variable=ii)})
        dataFrame <- eatTools::facToChar(dataFrame, from = "integer", to = "numeric")
        classes<- sort(unique(sapply(dataFrame[,allNam[["vars"]]], class)))
        if ( !all(classes %in% c("factor", "numeric")) ) {stop("All variables must be of class 'factor' or 'numeric'")}
        wb     <- ctype(dataFrame=dataFrame, vars = allNam[["vars"]])           ### workbook
        wb     <- do.call("rbind", plyr::alply(wb, .margins = 1, .fun = function ( z ) {
                  xvar<- dataFrame[,z[[2]]]
                  yvar<- dataFrame[,z[[1]]]
                  wvar<- dataFrame[,allNam[["weights"]]]
                  if (!is.null(allVars[["weights"]])) {                         ### z = 'zeile'
                      w <- ", weights = wvar"
                  }  else  {
                      w <- ""
                  }
                  na1 <- which(is.na(yvar))
                  na2 <- which(is.na(xvar))
                  if(length(na1)>0) {warning("Found ",length(na1)," missing values in variable '",z[[1]],"'")}
                  if(length(na2)>0) {warning("Found ",length(na2)," missing values in variable '",z[[2]],"'")}
                  if(length(na1)>0 | length(na2)>0) {
                     weg <- unique(c(na1, na2))
                     xvar<- xvar[-weg]
                     yvar<- yvar[-weg]
                     wvar<- wvar[-weg]
                  }
                  str1<- paste0("out <- wCorr::weightedCorr(y=yvar, x=xvar, method = \"",z[[5]],"\"",w,")")
                  eval(parse(text=str1))
                  z[,"cor"] <- out
                  return(z)}))
        wide   <- reshape2::dcast(wb, Var1~Var2, value.var = "cor")
        if ( out == "wide") {return(wide)}
        if ( out == "long") {return(wb)}
        if ( out == "both") {return(list ( long=wb, wide=wide))}}


### ctype = correlation type; Hilfsfunktion fuer 'wtdHetcor'
ctype <- function ( dataFrame, vars ) {
        komb   <- expand.grid(vars, vars, stringsAsFactors=FALSE)
        komb   <- komb[which(komb[,1] != komb[,2]),]
        komb[,"sort"] <- apply(komb, MARGIN = 1, FUN = function (zeile){paste(sort(zeile), collapse="_") })
        komb   <- komb[!duplicated(komb[,"sort"]),]
        komb   <- data.frame ( komb, class1 = NA, class2 = NA, stringsAsFactors = FALSE)
        for ( i in 1:nrow(komb)) { komb[i,"class1"] <- class(dataFrame[, komb[i,"Var1"]]); komb[i,"class2"] <- class(dataFrame[, komb[i,"Var2"]])}
        komb2  <- data.frame ( do.call("rbind", plyr::alply(komb, .margins = 1, .fun = function ( zeile) {
                  zle <- unlist(zeile)                                          ### wenn es eine Faktorvariable gibt, muss sie immer zuerst kommen
                  srt <- sort(zle[(length(zle)-1):length(zle)], index.return=TRUE)[["ix"]]
                  zle <- zle[c(srt, srt+3)]
                  if ( all(zle[3:4] == c("factor", "numeric"))) { zle <- c(zle, "Polyserial")}
                  if ( all(zle[3:4] == c("factor", "factor"))) { zle <- c(zle, "Polychoric")}
                  if ( all(zle[3:4] == c("numeric", "numeric"))) { zle <- c(zle, "Pearson")}
                  return(zle)}) ), stringsAsFactors=FALSE)
        colnames(komb2)[5] <- "method"
        return(komb2)}

