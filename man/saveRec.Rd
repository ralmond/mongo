% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jqmongo.R
\name{saveRec}
\alias{saveRec}
\title{Saves a MongoRec object to a Mongo database}
\usage{
saveRec(col, rec, serialize = TRUE)
}
\arguments{
\item{col}{(or MongoDB mongo NULL) A mongo collection reference.  If \code{NULL} record will not be saved.}

\item{rec}{The message (object) to be saved.}

\item{serialize}{A logical flag. If true,
\code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
field (and other objects which might contain complex R code.}
}
\value{
Returns the message argument, which may be modified by setting the
\code{"_id"} field if this is the first time saving the object.
}
\description{
This function saves an S4 object as a record in a Mongo database.  It uses
\code{\link{as.json}} to covert the object to a JSON string.
}
\examples{

\dontrun{
load_Events() # Uses the sample Event class.
m1 <- new("Event",uid="Fred",mess="Task Done",
                timestamp=Sys.time(),
                data=list("Selection"="B"))
m2 <- new("Event",uid="Fred",mess="New Obs",timestamp=Sys.time(),
                data=list("isCorrect"=TRUE,"Selection"="B"))
m3 <- new("Event",uid="Fred","New Stats",
                details=list("score"=1,"theta"=0.12345,"noitems"=1))

testcol <- MongoDB("Messages",noMongo=!interactive())
## Save them back to capture the ID.
m1 <- saveRec(testcol,m1)
m2 <- saveRec(testcol,m2)
m3 <- saveRec(testcol,m3)
}

}
\seealso{
\code{\link{as.json}}, \code{\linkS4class{MongoRec}},
\code{\link{buildObject}}, \code{\link{getOneRec}},
\code{\link{MongoDB}}
}
\author{
Russell Almond
}
\keyword{database}
