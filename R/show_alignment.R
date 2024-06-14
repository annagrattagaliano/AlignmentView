#' Show alignment of a read to the reference genome
#' 
#' This function takes a file containing read information, a read ID, 
#' and a reference genome,and prints the alignment of the specified 
#' read to the reference genome.
#' 
#' @param file A character string representing the path to the file 
#' containing read information.
#' @param read_id A character string representing the ID of the read 
#' to be aligned.
#' @param reference_genome A `BSgenome` object representing 
#' the reference genome.
#' 
#' @return Prints the alignment of the read to the reference genome.
#' @import Biostrings
#' @import GenomicRanges
#' @export show_alignment
#' @examples
#' library(BSgenome.Hsapiens.UCSC.hg38)
#' file <- system.file("extdata", "ERR188273_chrX.bam", 
#' package = "AlignmentView")
#' read_id <- "ERR188273.11944385"
#' genome <- BSgenome.Hsapiens.UCSC.hg38
#' show_alignment(file, read_id, genome)
#' 
#' @seealso \code{\link{parse_cigar}}, \code{\link{align_read}}, 
#' \code{\link{colorize_sequence}}
#' 
# Function to show alignment of a read to the reference genome
show_alignment <- function(file, read_id, reference_genome) {
    read_info <- extract_read(file, read_id)
  
    for (i in seq_len(nrow(read_info))) {
        cigar <- read_info$cigar[i]
        read_seq <- read_info$seq[i]
        ref_name <- as.character(read_info$rname[i])
    
        if (is.na(ref_name) || ref_name == "") {
            cat("Reference name is missing.")
            next
    }
    
        ref_length <- seqlengths(reference_genome)[ref_name]
        if (length(ref_length) != 1 || is.na(ref_length)) {
            cat("Reference sequence length not found.")
            next
    }
    
        ref_start <- as.integer(read_info$pos[i])
        if (is.na(ref_start)) {
            cat("Invalid start position. Read ID:", read_id, "Index:", i, "\n")
            next
    }
    
        cigar_parsed <- parse_cigar(cigar)
        read_length <- sum(cigar_parsed$lengths
[cigar_parsed$ops %in% c("M", "D", "=", "X", "N")])
        ref_end <- ref_start + read_length - 1
        if (ref_start < 1 || ref_end > ref_length) {
            cat("Start or end position is out of bounds 
of the reference sequence.")
            next
    }
    
        reference_seq <- as.character(getSeq(reference_genome, ref_name, 
start = ref_start, end = ref_end))
        alignment <- align_read(reference_seq, read_seq, cigar)
        cat(read_id, "\n")
        cat("CIGAR:", cigar, "\n")
        cat("Reference:", colorize_sequence(alignment$ref_aligned), "\n")
        cat("Read:     ", colorize_sequence(alignment$read_aligned), "\n\n")
    }
}
