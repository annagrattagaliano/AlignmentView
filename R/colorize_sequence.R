#' Colorize a DNA sequence
#' 
#' This function takes a DNA sequence and returns a colorized version 
#' of the sequence where each base (A, C, G, T) is colored differently.
#' 
#' @param sequence A character string representing the DNA sequence.
#' 
#' @return A character string with HTML/CSS color codes applied to each base.
#' @examples
#' sequence <- 'AGCTTAGCTAGCTACCTATATCTTGGTCTTG'
#' colorize_sequence(sequence)
#' 
#' @export colorize_sequence
#' @import crayon

# Function to colorize sequences
colorize_sequence <- function(sequence) {
    colored_sequence <- vapply(strsplit(sequence, "")[[1]], function(base) {
        if (base == "A") green(base)
        else if (base == "C") blue(base)
        else if (base == "G") yellow(base)
        else if (base == "T") red(base)
        else base # For gaps or other characters
    }, character(1))
    return(paste(colored_sequence, collapse = ""))
}
