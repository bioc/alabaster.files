% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BcfFileReference.R
\docType{class}
\name{BcfFileReference}
\alias{BcfFileReference}
\alias{BcfFileReference-class}
\alias{saveObject,BcfFileReference-method}
\alias{readBcfFileReference}
\alias{VcfWrapper}
\alias{VcfWrapper-class}
\alias{stageObject,VcfWrapper-method}
\alias{loadVcfWrapper}
\title{Reference to a BCF file}
\usage{
BcfFileReference(path, index = NULL)
}
\arguments{
\item{path}{String containing the path to a BCF file.}

\item{index}{String specifying the path to an index file in tabix or CSI format, or \code{NULL} if no index is available.}
}
\value{
A BcfFileReference instance that can be used in \code{\link{saveObject}}.
}
\description{
Reference to a BCF file, for saving and loading in the \pkg{alabaster} framework.
}
\examples{
# Using Rsamtools's example file.
fl <- system.file("extdata", "ex1.bcf.gz", package="Rsamtools")

# Creating a BcfFileReference.
wrapped <- BcfFileReference(fl)
wrapped

# Fetching the path information:
path(wrapped)
wrapped$index

# Staging the BcfFileReference.
dir <- tempfile()
saveObject(wrapped, dir)
list.files(dir, recursive=TRUE)

# Loading it back again:
readObject(dir)

}
\author{
Aaron Lun
}
