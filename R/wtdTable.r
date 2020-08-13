#' Weighted frequency tables.
#'
#' Compute a weighted frequency table.
#'
#' This function provides similar functionality as the \code{\link[Hmisc]{wtd.table}} function from \code{Hmisc}.
#'
#'@param x a character or category or factor vector
#'@param weights a numeric vector of non-negative weights
#'@param na.rm set to \code{FALSE} to suppress checking for NAs. If \code{TRUE}, NAs are removed
#'from \code{x} as well as from \code{weights} prior to variance estimation.
#'
#'@return a frequency table
#'
#'@examples
#'x <- c(50, 1, 25)
#'w <- c(1, 4, 1)
#'
#'# compute weighted variance
#'wtdTable(x, w)
#'
#'@export
wtdTable <- function ( x, weights, na.rm = FALSE) {
            frm   <- data.frame ( variable=x, wgt=weights, stringsAsFactors=FALSE)
            if ( na.rm == TRUE ) {frm <- stats::na.omit(frm)}
            Table <- as.data.frame(data.table::setDT(frm)[, .(n = sum(wgt)), variable])
            Table <- eatTools::facToChar(Table)
            tab   <- Table[sort(Table[,"variable"],index.return=TRUE)$ix, "n"]
            names(tab) <- sort(Table[,"variable"])
            return(tab)}

#'@import data.table
## quiets concerns of R CMD check regarding NSE by data.table
utils::globalVariables(c(".", "wgt", "variable"))


