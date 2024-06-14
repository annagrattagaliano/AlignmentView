#' Align a read to a reference sequence based on the CIGAR string
#'
#' This function takes as input a reference sequence, a read sequence, and a  
#' CIGAR string, and returns the aligned reference and read sequences. 
#' 
#' @param reference_seq A character string representing the reference sequence. 
#' @param read_seq A character string representing the read sequence. 
#' @param cigar A character string representing the CIGAR string of the read. 
#' 
#' @return A list with two elements, the reference sequence and the 
#' read sequence.
#' 
#' @examples 
#' reference_seq <- "AGCTTAGCTAGCTACCTATATCTTGGTCTTG"
#' read_seq <- "AGCTTAGCTAGCTAC-TATCTTGGCCTTG"
#' cigar <- "14M1D11M1X6M"
#' align_read(reference_seq, read_seq, cigar) 
#' 
#' @export align_read
#' @author Anna Grattagliano\cr Politecnico di Milano\cr Maintainer:
#' Anna Grattagliano\cr E-Mail: <anna.grattagliano@mail.polimi.it>
#' 
#' @seealso \code{\link{show_alignment}}

# Function to align read to reference sequence based on the CIGAR string
align_read <- function(reference_seq, read_seq, cigar) {
    cigar_parsed <- parse_cigar(cigar)
    ops <- cigar_parsed$ops
    lengths <- cigar_parsed$lengths
    ref_aligned <- character(sum(lengths))
    read_aligned <- character(sum(lengths))
    ref_idx <- 1
    read_idx <- 1
    align_idx <- 1
    for (j in seq_along(ops)) {
        op <- ops[j]
        len <- lengths[j]
        #match
        if (op == "M" || op == "=" || op == "X") {
            ref_aligned[align_idx:(align_idx + len - 1)] <- strsplit(substr
(reference_seq, ref_idx, ref_idx + len - 1), "")[[1]]
            read_aligned[align_idx:(align_idx + len - 1)] <- strsplit(substr
(read_seq, read_idx, read_idx + len - 1), "")[[1]]
            ref_idx <- ref_idx + len
            read_idx <- read_idx + len
            #insertion wrt reference
        } else if (op == "I") {
            ref_aligned[align_idx:(align_idx + len - 1)] <- "-"
            read_aligned[align_idx:(align_idx + len - 1)] <- strsplit(substr
(read_seq, read_idx, read_idx + len - 1), "")[[1]]
            read_idx <- read_idx + len
            #deletion wrt reference
        } else if (op == "D") {
            ref_aligned[align_idx:(align_idx + len - 1)] <- strsplit(substr
(reference_seq, ref_idx, ref_idx + len - 1), "")[[1]]
            read_aligned[align_idx:(align_idx + len - 1)] <- "-"
            ref_idx <- ref_idx + len
            #split or spliced alignment
        } else if (op == "N") {
            ref_aligned[align_idx:(align_idx + len - 1)] <- strsplit(substr
(reference_seq, ref_idx, ref_idx + len - 1), "")[[1]]
            read_aligned[align_idx:(align_idx + len - 1)] <- "-"
            ref_idx <- ref_idx + len
            #soft-clipping
        } else if (op == "S") {
            read_idx <- read_idx + len
            #hard-clipping
        } else if (op == "H") {
            next
            #padding
        } else if (op == "P") {
            # Padding: skip the bases in both read and reference
            ref_aligned[align_idx:(align_idx + len - 1)] <- "*"
            read_aligned[align_idx:(align_idx + len - 1)] <- "*"
        }
        align_idx <- align_idx + len
    }
    ref_aligned <- paste(ref_aligned, collapse = "")
    read_aligned <- paste(read_aligned, collapse = "")

    return(list(ref_aligned = ref_aligned, read_aligned = read_aligned))
}

