% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BamFileReference.R
\docType{class}
\name{BamFileReference}
\alias{BamFileReference}
\alias{BamFileReference-class}
\alias{saveObject,BamFileReference-method}
\alias{readBamFileReference}
\alias{BamWrapper}
\alias{BamWrapper-class}
\alias{stageObject,BamWrapper-method}
\alias{loadBamWrapper}
\alias{BamIndexWrapper}
\alias{BamIndexWrapper-class}
\alias{stageObject,BamIndexWrapper-method}
\alias{loadBamIndexWrapper}
\title{Reference to a BAM file}
\usage{
BamFileReference(path, index = NULL)
}
\arguments{
\item{path}{String containing the path to a BAM file.}

\item{index}{String specifying the path to a BAI or CSI index file, or \code{NULL} if no index is available.}
}
\value{
A BamFileReference instance that can be used in \code{\link{saveObject}}.
}
\description{
Reference to a BAM file, for saving and reading in the \pkg{alabaster} framework.
}
\examples{
# Using a BAM file from Rsamtools.
fl <- system.file("extdata", "ex1.bam", package="Rsamtools", mustWork=TRUE)

# Creating a BamFileReference.
wrapped <- BamFileReference(fl)
wrapped

# Fetching the path information:
path(wrapped)
wrapped$index

# Saving to disk:
dir <- tempfile()
saveObject(wrapped, dir)
list.files(dir, recursive=TRUE)

# Reading it back again:
readObject(dir)

}
\author{
Aaron Lun
}
