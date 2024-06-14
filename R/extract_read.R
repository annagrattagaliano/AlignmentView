#' extract a specific read ID’s Information from a BAM/SAM file #' 
#' This function loads a BAM/SAM file and a specific read ID and 
#' will returnthe necessary information 
#' @param file the path to the BAM/SAM input file
#' @param read_id is the identifier of the read 
#' @return the read information regarding the specifico read ID
#' @export extract_read
#' @import Rsamtools
#' @import data.table
#' @examples
#' library(Rsamtools)
#' library(data.table)
#' file <- system.file("extdata", "ERR188273_chrX.bam", 
#' package = "AlignmentView")
#' read_id <- "ERR188273.11944385"
#' extract_read(file,read_id)
#'
#' @author Anna Grattagliano\cr Politecnico di Milano\cr Maintainer:
#' Anna Grattagliano\cr E-Mail: <anna.grattagliano@mail.polimi.it>
#' 
#' @seealso \code{\link{show_alignment}}

# Function to extract read information
extract_read <- function(file, read_id) {
    param <- ScanBamParam(what = c("qname", "rname", "strand", 
					"pos", "cigar", "seq"))
    bam <- scanBam(file, param = param)[[1]]
    bam_dt <- data.table(
        qname = unlist(bam$qname),
        rname = unlist(bam$rname),
        strand = unlist(bam$strand),
        pos = unlist(bam$pos),
        cigar = vapply(bam$cigar, as.character, FUN.VALUE = character(1)),
        seq = vapply(bam$seq, as.character, FUN.VALUE = character(1))
    )
  
    # Find all matches for the specific read ID
    read_info <- bam_dt[qname == read_id]
  
    if (nrow(read_info) == 0) {
        stop("The read ID you are looking for was not found in BAM/SAM file.")
    }
  
    return(read_info)
}
