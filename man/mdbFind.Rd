% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MongoDB.R
\name{mdbFind}
\alias{mdbFind}
\alias{mdbFind,MongoDB-method}
\alias{mdbFind,mongo-method}
\title{Finds records which match the query and returns as data frame}
\usage{
mdbFind(
  db,
  query = "{}",
  fields = "{\\"_id\\":0}",
  sort = "{}",
  skip = 0,
  limit = 0,
  handler = NULL,
  pagesize = 1000
)

\S4method{mdbFind}{MongoDB}(
  db,
  query = "{}",
  fields = "{\\"_id\\":0}",
  sort = "{}",
  skip = 0,
  limit = 0,
  handler = NULL,
  pagesize = 1000
)

\S4method{mdbFind}{mongo}(
  db,
  query = "{}",
  fields = "{\\"_id\\":0}",
  sort = "{}",
  skip = 0,
  limit = 0,
  handler = NULL,
  pagesize = 1000
)
}
\arguments{
\item{db}{(or MongoDB mongo) -- the database collection.}

\item{query}{character -- A query string in \code{JSON} format.  See
\verb{\link{buildJQuery}}.}

\item{fields}{character -- A JSON expression describing which
fields to extract from the selected records.  The default returns
all fields except for the internal Mongo id field (see \verb{\link{m_id}}.}

\item{sort}{character -- A JSON field indicating how the record
should be sorted.}

\item{skip}{integer -- The number of records to skip.}

\item{limit}{integer -- The maximum number of records to return.}

\item{handler}{(or NULL function) -- Undocumented.}

\item{pagesize}{integer -- Used for buffering}
}
\value{
data.frame giving query results
}
\description{
Finds records which match the query and returns as data frame
}
\details{
The \code{mdbFind} function takes a collection of records and turns it
into a \code{data.frame} with the columns representing the frame.  To
process the raw JSON stream, try \verb{\link{mdbIterate}}

The \code{query}, \code{fields} and \code{sort} are all JSON expressions.  Note
that field names shuld be quoted inside these query strings (the
quotes are optional in the Mongo shell, but not here).  I recommend
using single quotes for the outer expression and double quotes
inside the JSON string.
\subsection{Query}{

The mongo query is a rather rich language.  The simplest version
restricts a field to a specific value.  For example
\code{'{"Species":"virginica"}'} would select only virginica irises.
There are a number of different operators which can used to specify
the query, for examples \code{'{"Species":{$ne:"virginica"}}'} and
\code{'{"Species":{$in:["setosa","versicolor"]}}'} both select the other
iris types.

The mongo operators are "$eq" -- equals, "$gt" -- greater than,
"$gte" -- greater than or equals, "$lt" -- less than, "$lte" --
less than or equals, "$ne" -- not equal,"$nin" -- not in (argument
is a list ('[]'),"$in" -- in (argument is a list) and "$regex" -- a
regular expression.

The function \verb{\link{buildJQuery}} uses a more R-like syntax and
converts them to JSON.  This makes it easier to build a query
inside of R.
}

\subsection{fields}{

The fields JSON expression should be a collection of fields with a
\code{true} or \code{false} (or 0 or 1).  Note that the "_id" field is
automatically included unless explicitly excluded.  For example:
\verb{\{"Petal.Length":1, "Petal.Width":1, "Species":1, "_id":0\}} will
select the petal length and width field and species field.  See
the topic \code{Projection} in the Mongo manual for more details.
}

\subsection{sort}{

This is a short object which gives the name of the field to sort on
and the direction (1 for ascending, -1 for descending).  If more
than one sort key is given, the first one is given the highest
priority.  The sort keys should be included in the field.  For example
\code{{"Petal.Length":1}} sort in ascending order according to petal
length.  See the \code{sort} function in the Mongo reference manual.
}
}
\examples{
irisdb <- MongoDB("iris",noMongo=!interactive())
mdbDrop(irisdb)
mdbInsert(irisdb,iris)
mdbFind(irisdb,buildJQuery(Species="setosa"),
        fields = '{"Petal_Width":1, "Petal_Length":1}',
        sort = '{"Petal_Width":-1}', limit=10)
}
\seealso{
\verb{\link[mongolite]\{mongo\}}, \verb{\link{buildJQuery}}
\verb{\link{getOneRec}}, \verb{\link{getManyRecs}}, \verb{\link{mdbIterate}}
\url{https://www.mongodb.com/docs/manual/reference/operator/query/}
\url{https://www.mongodb.com/docs/manual/reference/command/find/}
\url{https://www.mongodb.com/docs/manual/reference/method/db.collection.find/#std-label-find-projection}
\url{https://www.mongodb.com/docs/manual/reference/method/cursor.sort/#mongodb-method-cursor.sort}
}
