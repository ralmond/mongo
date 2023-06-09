% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.json.R
\docType{methods}
\name{m_id}
\alias{m_id}
\alias{m_id<-}
\alias{m_id,MongoRec-method}
\alias{m_id<-,MongoRec-method}
\title{Accessor for the Mongo id element of a record.}
\usage{
m_id(x)

m_id(x) <- value

\S4method{m_id}{MongoRec}(x)

\S4method{m_id}{MongoRec}(x) <- value
}
\arguments{
\item{x}{An object of type \link{MongoRec}.}

\item{value}{(character) the new ID value, use \code{NA_character_} for missing.}
}
\description{
Objects of class \code{\linkS4class{MongoRec}} have a \verb{_id} slot
which stores the database ID.  This function accesses it.
}
\details{
The \verb{_id} slot should be a character object with the name
\dQuote{oid}.  The methods enforce this.  If the object does not
have a Mongo ID (i.e., it was never stored in a database), then the
value of \verb{_id} should be \code{NA_character_}.
}
\section{Functions}{
\itemize{
\item \code{m_id(x) <- value}: Setter for Mongo ID

}}
\examples{
mr <- MongoRec()
m_id(mr) # NA
m_id(mr) <- "012345"
m_id(mr)

}
