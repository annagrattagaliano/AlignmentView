% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorize_sequence.R
\name{colorize_sequence}
\alias{colorize_sequence}
\title{Colorize a DNA sequence}
\usage{
colorize_sequence(sequence)
}
\arguments{
\item{sequence}{A character string representing the DNA sequence.}
}
\value{
A character string with HTML/CSS color codes applied to each base.
}
\description{
This function takes a DNA sequence and returns a colorized version
of the sequence where each base (A, C, G, T) is colored differently.
}
\examples{
sequence <- 'AGCTTAGCTAGCTACCTATATCTTGGTCTTG'
colorize_sequence(sequence)

}
