\docType{package}
\name{mongo-package}
\alias{mongo-package}
\alias{mongo}
\title{\packageTitle{mongo}}
\description{
\packageDescription{mongo}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{mongo}
\packageIndices{mongo}

This package provides extensions to the `mongolite` and `jsonlite` package, specifically for saving and restoring
S4 objects as JSON documents in a mongo database.
}
\section{MongoDB class and methods}{

  Using a \code{\link[mongolite]{mongo}} object in an R6 reference class
  presents a number of problems.  In particular, there is potential race
  condition where the prototype object is built during load time, and
  the will try to make a connection to the database before the
  appropriate code is loaded.  The \code{\linkS4class{MongoDB}} fixes
  this problem.  It is a wrapper for the \code{mongo} object which is
  created when first used (by calling the \code{$db()} method).

  The "mongo" package offers a number of generic functions which wrap
  the corresponding method of the \code{\link[mongolite]{mongo}} object:
  \code{\link{mdbAggregate}},
  \code{\link{mdbCount}},
  \code{\link{mdbDisconnect}},
  \code{\link{mdbDistinct}},
  \code{\link{mdbDrop}},
  \code{\link{mdbExport}},
  \code{\link{mdbFind}},
  \code{\link{mdbImport}},
  \code{\link{mdbIndex}},
  \code{\link{mdbInfo}},
  \code{\link{mdbInsert}},
  \code{\link{mdbIterate}},
  \code{\link{mdbMapreduce}},
  \code{\link{mdbRemove}},
  \code{\link{mdbRename}},
  \code{\link{mdbReplace}},
  \code{\link{mdbRun}}, and
  \code{\link{mdbUpdate}}. It also adds
  \code{\link{showCollections}} and
  \code{\link{showDatabases}} which operate like the corresponding Mongo
  shell commands.  All of these functions are S4 generics with methods
  for both the \code{mongo::MongoDB} and \code{mongolite::mongo} objects.

}
\section{Syntactic Sugar for Mongo Queries}{

  The function \code{\link{buildJQuery}} is a helper function which
  creates JSON documents from R lists.  For example,
  \code{buildJQuery(name="Fred",timestamp=list(gte=Sys.time()))}
  evaluates to
  \code{{ "name":"Fred", "timestamp":{ "$gte":[{"$date":1684370555315}] } } }
  The function \code{\link{buildJQterm}} is a helper, and
  \code{\link{makeDBuri}} is used for setting the `uri` (or `url`) field
  for the \code{\linkS4class{MongoDB}} class.

}
\section{JSON representation for S4 object}{

  Saving an S4 object as a JSON document is a complex process.  One
  approach is to use the functions
  \code{jsonlite::\link[jsonlite]{serializeJSON}} and
  \code{jsonlite::\link[jsonlite]{unserializeJSON}} methods.  These will
  faithfully reproduce the object, but using the serialized object
  outside of R (in particular, in a Mongo collection or with the command
  line utility \code{jq}) is very difficult.

  A second approach is to first turn the object into a list using
  \code{\link[base]{attributes}(obj)}, and then using
  \code{jsonlite::\link[jsonlite]{toJSON}} and
  \code{jsonlite::\link[jsonlite]{fromJSON}} to do the conversion.  One
  big problem with this approach is that these functions always turn R
  object into JSON arrays, even if the value is a scalar.  Also, the
  types of the fields may not be the same after saving and restoring the
  object.

  To fix the scalar/vector problem, the function
  \code{\link[jsonlite]{unbox}} function marks a value as a scalar.  The
  function \code{\link{unboxer}} is improved version which descends
  recursively through a complex list structure.  The function
  \code{\link{ununboxer}} undoes the marking (mainly needed for
  testing).  The functions \code{\link{unparseData}} and
  \code{\link{parseSimpleData}} provide some support for more complex
  structures.

  The \code{\link{as.json}} function attempts to implement the second
  approach in a way that is transparent to the end user, but requires
  some effort on the part of the package designer.  In particular, most
  S4 classes will require a custom \code{\link{as.jlist}} method which
  does appropriate transformations on the fields of the object (now
  elements of the list).  The vignette "JSON for S4 objects" provides an
  example.

  The function \code{\link{parse.json}} goes in the opposite direction.
  The JSON is converted into a list and then passed along to the builder
  function helper \code{\link{buildObject}}.  The builder calls
  \code{\link{parse.jlist}} to reverse the \code{as.jlist} processing.
  The default builder then passes the list to the \code{new} function to
  create the new object.  This approach should work well for most S4
  objects, but S3 objects may need a custom constructor.  In this case,
  the \code{buildObject} default builder needs to be replaced with
  custom code.

  The vignette "JSON for S4 Objects" provides an extended example.  The
  example \code{Event} and its associated \code{as.jlist} and
  \code{parse.jlist} methods are found in the file
  \code{system.file("examples","Event.R",package="mongo")}.  This file
  can be loaded (used in some of the documentation examples) using the
  function \code{\link{load_example}} function.

}
\section{Saving S4 objects in a database}{

  Once the \code{as.json} and \code{parse.json} methods are built,
  saving the S4 object in a Mongo collection is straightforward;
  restoring the objects is also straightforward but requires the use of
  \code{\link{mdbIterate}} instead of \code{\link{mdbFind}} (the latter
  returns a data frame not a list).

  The functions \code{\link{saveRec}}, \code{\link{getOneRec}}, and
  \code{\link{getManyRecs}} combine the calls to provide a
  straightforward mechanism for saving and restorting S4 objects from a
  database.  S3 object may require a custom builder function (instead of
  \code{\link{buildObject}}), which can be passed as an optional
  argument to \code{getOneRec} or \code{getManyRecs}.

  Mongo databases use the special field "_id" to provide a unique
  identifier for the collection.  The class
  \code{\linkS4class{MongoRec}} is a simple class which provides that
  field (and can be used in the \code{contains} argument in the
  \code{setClass} method).  The generic function \code{\link{m_id}} gets
  or sets the "_id" field.  For objects that have not yet been
  added to the database, the value \code{NA_character_} should be used.

  The \code{saveRec} method modified its behavior based on the value of
  the value of \code{m_id(obj)}.  If this is missing, \code{saveRec}
  adds a new document to the collection, if it is present, then
  \code{saveRec} replaces the item in the collectin.

}
\section{A mock MongoDB class}{

  In designing a test suite for a class, it is often useful to 'mock'
  functions which rely on external resources with ones which have more
  predictable result.  In particular, the \code{\link{fake_mongo}} is a
  drop in replacement for the \code{\link{MongoDB}} function which
  returns the results of various queries from a queue instead of a database.

  The \code{\link{iterator}} class is a simple queue implementation. It
  is an R6 class with a collection of elements and a pointer to the next
  item in the collection.  The \code{$hasNext()} method is a logical
  method that checks for more elements and \code{$nextElement()} which
  returns the next element (and updates the pointer).  The
  \code{$reset()} method moves the pointer back to the beginning with
  and optional argument which replaces the element collection.  It also
  has \code{$one()} and \code{$batch(n)} methods so it can mock the
  output of \code{\link{mdbIterate}}.

  The \code{\link{fake_mongo}} class has queues corresponding to the
  functions which return messages other than status messages.
  \tabular{rr}{
    \strong{Method} \tab \strong{Queue Name} \cr
    \code{\link{mdbAggregate}} \tab "aggregate" \cr
    \code{\link{mdbCount}} \tab "count" \cr
    \code{\link{mdbDistinct}} \tab "distinct" \cr
    \code{\link{mdbFind}} \tab "find" \cr
    \code{\link{mdbIterate}} \tab "iterate" \cr
    \code{\link{mdbMapreduce}} \tab "mapreduce" \cr
    \code{\link{mdbRun}} \tab "run" \cr
    \code{\link{showCollections}} \tab "collections" \cr
    \code{\link{showDatabases}} \tab "databases" \cr
  }

  The \code{$que(which)} method returns the \code{\link{iterator}}
  implementing the queue.  The \code{$resetQue(which)} method resets a
  queue (with an optional arugment allowing to set a new collection of
  elements) and \code{$resetAll()} resets all queues.
}
\seealso{
\code{vignette("json-aaquickstart",package="jsonlite")}
\code{\link[mongolite]{mongolite}},
\code{\link[Proc4:Proc4-package]{Proc4}}
(\url{https://ralmond.r-universe.dev/ralmond/Proc4})
}
\references{
Mongolite User Manual:
\url{https://jeroen.github.io/mongolite/}

Mongo DB command reference (make sure you look at the version
corresponding to the mongo database used by your system).
\url{https://www.mongodb.com/docs/manual/reference/command/}

}

\author{
\packageAuthor{mongo}

Maintainer:
\packageMaintainer{mongo}
}
\examples{
  \dontrun{
    vingette("JSON for S4 Objects")
  }
}
\keyword{database}
\keyword{package}
