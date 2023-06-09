% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jqmongo.R
\name{buildJQuery}
\alias{buildJQuery}
\title{Transforms a query into JQuery JSON.}
\usage{
buildJQuery(..., rawfields = character())
}
\arguments{
\item{\dots}{This should be a named list of arguments.  The values should be
the desired query value, or a more complex expression (see details).}

\item{rawfields}{These arguments are passed as character vectors directly
into the query document without processing.}
}
\value{
The function \code{buildJQuery} returns a unicode string which
contains the JSON query document.
}
\description{
This function takes a query which is expressed in the argument list and
transforms it into a JSON query document which can be used with the Mongo
Database.  The function \code{buildJQterm} is a helper function which builds
up a single term of the query.
}
\details{
A typical query to a Mongo database collection is done with a JSON object
which has a number of bits that look like
\dQuote{\emph{field}:\emph{value}}, where \emph{field} names a field in the
document, and \emph{value} is a value to be matched.  A record matches the
query if all of the fields specified in the query match the corresponding
fields in the record.

Note that \emph{value} could be a special expression which gives specifies a
more complex expression allowing for ranges of values.  In particular, the
Mongo query language supports the following operators: \code{"$eq", "$ne",
"$gt", "$lt", "$gte", "$lte"}.  These can be specified using a value of the
form \code{c(<op>=<value>)}, where \emph{op} is one of the mongo operators,
without the leading \sQuote{$}.  Multiple op--value pairs can be specified;
for example, \code{count=c(gt=3,lt=6)}.  If no op is specified, then
\code{"$eq"} is assumed.  Additionally, the \code{"$oid"} operator can be
used to specify that a value should be treated as a Mongo record identifier.

The \code{"$in"} and \code{"$nin"} are also ops, but the corrsponding value
is a vector.  They test if the record is in or not in the specified value.
If the value is vector valued, and no operator is specified it defaults to
\code{"$in"}.

The function \code{buildJQuery} processes each of its arguments, adding them
onto the query document.  The \code{rawfields} argument adds the fields onto
the document without further processing.  It is useful for control arugments
like \code{"$limit"} and \code{"$sort"}.
}
\examples{

buildJQuery(app="default",uid="Phred")
buildJQuery("_id"=c(oid="123456789"))
buildJQuery(name="George",count=c(gt=3,lt=5))
buildJQuery(name="George",count=c(gt=3,lt=5),
            rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))

## Queries on IDs need special handling
buildJQuery("_id"=c(oid="123456789abcdef"))

}
\references{
The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
}
\seealso{
\code{\link{as.json}}, \code{\link{mdbFind}},
\code{\link{getOneRec}}, \code{\link{getManyRecs}}
\code{\link[mongolite]{mongo}}
}
\author{
Russell Almond
}
\keyword{database}
\keyword{interface}
