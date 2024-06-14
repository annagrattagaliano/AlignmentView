#' Parse a CIGAR string
#' This function takes as input the CIGAR string and finds the 
#' alphabetic and numeric parts and seperates them
#' 
#' @param cigar is the CIGAR string of the read
#' @return a list with operations and their corresponding 
#' lengths
#' @export parse_cigar
#' 
#' @examples
#' cigar <- "51M1D24M"
#' parse_cigar(cigar)
#' 
#' @author Anna Grattagliano\cr Politecnico di Milano\cr Maintainer:
#' Anna Grattagliano\cr E-Mail: <anna.grattagliano@mail.polimi.it>
#' 
#' @seealso \code{\link{show_alignment}}


# Function to parse CIGAR string
parse_cigar <- function(cigar) {
    matches <- gregexpr("\\d+|[A-Z]+", cigar)
    parts <- regmatches(cigar, matches)[[1]]
    lengths <- as.numeric(parts[seq(1, length(parts), 2)])
    ops <- parts[seq(2, length(parts), 2)]

    return(list(ops = ops, lengths = lengths))
}
