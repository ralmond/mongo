% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MongoDB.R
\name{showDatabases}
\alias{showDatabases}
\alias{showDatabases,MongoDB-method}
\alias{showDatabases,NULL-method}
\title{Lists Databases}
\usage{
showDatabases(
  db = NULL,
  uri = "mongodb://localhost",
  options = mongolite::ssl_options()
)

\S4method{showDatabases}{MongoDB}(
  db = NULL,
  uri = "mongodb://localhost",
  options = mongolite::ssl_options()
)

\S4method{showDatabases}{NULL}(
  db = NULL,
  uri = "mongodb://localhost",
  options = mongolite::ssl_options()
)
}
\arguments{
\item{db}{(or MongoDB NULL) -- Database reference. (Note:
\code{mongolite::mongo} cannot be used.)}

\item{uri}{character -- URI for database connections.}

\item{options}{list -- SSL options for an SSL connection}
}
\value{
a data frame containing the names and sizes of the databases
}
\description{
Lists Databases
}
\details{
This function lists the names of the databases which are accessible
for the current user.

This function needs to make a new connection to the \code{admin}
database.  If a MongoDB object is supplied, then the \code{uri} and
\code{options} are taken from it.  If the \code{db} argument is NULL, a new
connection is made.

Note that there is currently no documented way of retrieving the
\code{url} and \code{ssl_options} from a \code{mongolite::mongo} object.
}
\examples{
irisdb <- MongoDB("iris",noMongo=!interactive())
showDatabases(irisdb)
}
\seealso{
\verb{\link[mongolite]\{mongo\}}, \verb{\link{mdbRun}}
}
