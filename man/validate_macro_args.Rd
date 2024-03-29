% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_macro.r
\name{validate_macro_args}
\alias{validate_macro_args}
\title{checks to make sure that only defined arguments are used for splicing in a macro}
\usage{
validate_macro_args(blocks, args, .left = "{|", .right = "|}")
}
\arguments{
\item{blocks}{a macro file parsed with `get_blocks`}

\item{args}{output from `get_macro_args()`}

\item{.left}{left bracket for argument tags}

\item{.right}{right bracket for argument tags}
}
\value{
`invisible(blocks)`, if valid; otherwise throws an error`
}
\description{
checks to make sure that only defined arguments are used for splicing in a macro
}
