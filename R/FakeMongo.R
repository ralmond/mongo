
#' An object which iterates over a collection
#'
#' @field elements list -- The object to return
#' @field position integer -- a pointer to the last returned object
#'
#' # Methods
#'
#' * `$hasNext()` -- logical; returns `TRUE` if collection has been exhausted.
#' * `$nextElement(warn=TRUE)` -- returns the next element of the collection.  If there are no unused elements left,
#'  the method issues a warning (if `warn` is `TRUE`) and returns `NULL`
#' * `$one()` -- A synonym for `$nextElement(warn=FALSE)`.  For compatibility with the internal iterator class
#'  returned by `\link{mdbIterate}.
#' * `$batch(count)` -- Returns the next `count` elements in the collection (updating the internal pointer by
#'   `count`.
#' * `$reset(newElements=list())` -- Resets the pointer back to the beginning of the element collection.
#' If the `newElements` argument is not missing, it also replaces the elements collection with the value of
#' `newElements`.
#'
#' @seealso `\linkS4class{fake_mongo}`, [mdbIterate()]
#'
#' @note
#'
#' This is a utility class that serves two purposes.
#' (1) It implements a result queue for the `linkS4class{fake_mongo}` class.
#' (2) it mimics the iterator returned by the `mdbIterate()`
#' generic function, and so can be used in the result queue for the `mdbIterate-fake_mongo` method.
#'
#' Unlike the internal iterator class from the `mongolite`, this one has a `$hasNext()` method which is
#' part of the general iterator recipe.  The `$one()` and `$batch()` methods should be compatible with the
#' internal `mongolite` iterator, can so it can be used as drop in replacement.
#'
#' @return An object of class iterator.
#' @exportClass iterator
#' @export iterator
#' @aliases iterator
#' @examples
#' iter <- iterator(as.list(1:5))
#' while (iter$hasNext())
#'   print(iter$nextElement())
#'
#'
setRefClass("iterator",fields=c(elements="list",position="integer"),
              methods=list(
                  initialize=function(elements=list(),...) {
                    callSuper(elements=elements,position=0L,...)
                  },
                  hasNext = function() {
                    position < length(elements)
                  },
                  nextElement = function(warn=TRUE) {
                    position <<- position +1L
                    if (position > length(elements)) {
                      if (warn) {
                        warning("No elements left in interator")
                      }
                      return(NULL)
                    }
                    return(elements[[position]])
                  },
                  one = function() {
                    nextElement(FALSE)
                  },
                  reset = function(newElements=list()) {
                    if (!missing(newElements)) {
                      elements <<- newElements
                    }
                    position <<- 0L
                    .self ## Needs to return self so we can use with lapply
                  },
                  batch = function(count) {
                    count <- as.integer(count)
                    max <- position+count
                    if (max > length(elements)) {
                      warning(sprintf("%d elements requested, but only %d left.",
                                      count,length(elements)-position))
                      max <- length(elements)
                    }
                    if (length(elements) <= position) return(NULL)
                    res <- elements[(position+1):max]
                    position <<- position + count
                    return(res)
                  }))


iterator <- function(elements=list()) {
  new("iterator",elements=elements)
}


#' A simulated `MongoDB` object for testing
#'
#' This class simulates the behavior of a mongo collection providing a set of scripted responses to queries.  In particular,
#' [mdbAggregate()], [mdbCount()], [mdbDistinct()], [mdbFind()], [mdbIterate()], [mdbMapreduce()],
#' [mdbRun()], [showCollections()] and [showDatabases()] methods are overridden to return prespecified
#' results in order.  Usually, no connection is made to an actual database, so this can be used to run tests
#' in environments where it is unknown whether or not an appropriate mongo database is available.
#'
#' @name fake_mongo-class
#' @aliases fake_mongo
#' @field queues a named list of `\linkS4class{iterator}` objects which provide the simulated responses.
#'
#' # Methods
#' * `$que(which)` -- Returns the internal iterator associated with the operation `which`.
#' * `$resetQue(which,newElements)` -- Calls `$reset()` method on `$que(which)`.
#' * `$resetAll()` -- resets all queues
#'
#' @details
#'
#' Internally the `fake_mongo` class has a list of iterators named "aggregate", "count", "distinct", "find",
#' "iterate", "mapreduce", "run", "databases", and "collections".  The corresponding methods will return the next entry
#' in the iterator (if it exists) or else will call the parent method to get the default return value (varies with generic
#' function).  Usually, no connection to a mongo database is made.
#'
#'  The Queue names are given in the following table.
#'   \tabular{rr}{
#'    \strong{Method} \tab \strong{Queue Name} \cr
#'    \code{\link{mdbAggregate}} \tab "aggregate" \cr
#'    \code{\link{mdbCount}} \tab "count" \cr
#'    \code{\link{mdbDistinct}} \tab "distinct" \cr
#'    \code{\link{mdbFind}} \tab "find" \cr
#'    \code{\link{mdbIterate}} \tab "iterate" \cr
#'    \code{\link{mdbMapreduce}} \tab "mapreduce" \cr
#'    \code{\link{mdbRun}} \tab "run" \cr
#'    \code{\link{showCollections}} \tab "collections" \cr
#'    \code{\link{showDatabases}} \tab "databases" \cr
#'  }
#'
#' These names are used as `which` arguments to the `$que(which)` and `$resetQueue(which)`
#' methods as well as for initializing the queue using the `fake_mongo` constructor.
#'
#' @return An object of class `fake_mongo`
#' @exportClass fake_mongo
#' @export fake_mongo
#'
#' @examples
#' showClass("fake_mongo")
setRefClass("fake_mongo",
              fields=c(queues="list"),
              contains = "MongoDB",
              methods=list(
                  initialize= function(collection="test",
                                       db="test", url="mongodb://localhost",
                                       verbose=FALSE,noMongo=TRUE,
                                       options=mongolite::ssl_options(),
                                       aggregate.q=list(),
                                       count.q=list(),
                                       distinct.q=list(),
                                       find.q=list(),
                                       iterate.q=list(),
                                       mapreduce.q=list(),
                                       run.q=list(),
                                       databases.q=list(),
                                       collections.q=list(),
                                       ...) {
                    qqq <- list(
                        aggregate=iterator(aggregate.q),
                        count=iterator(count.q),
                        distinct=iterator(distinct.q),
                        find=iterator(find.q),
                        iterate=iterator(iterate.q),
                        mapreduce=iterator(mapreduce.q),
                        run=iterator(run.q),
                        databases=iterator(databases.q),
                        collections=iterator(collections.q)
                    )
                    callSuper(colname=collection,dbname=db,uri=url,
                              noMongo=noMongo, queues=qqq,
                              verbose=verbose, options=options, ...)
                  },
                  que = function(which) {
                    queues[[which]]
                  },
                  resetQue = function(which, newElements=NULL) {
                    if (missing(newElements)) {
                      queues[[which]]$reset()
                    } else {
                      queues[[which]]$reset(newElements)
                    }
                    if (which=="iterate") {
                      ## Reset queues of iterators.
                      lapply(queues$iterate$elements,function(qq) qq$reset())
                    }
                  },
                  resetAll = function() {
                    lapply(names(queues),function (qname) resetQue(qname))
                  }))

#' @describeIn fake_mongo-class Constructor
#'
#' @param collection character -- name of the referenced collection
#' @param db character -- name of the referenced database
#' @param url character -- URI for accessing the database.
#' @param verbose logical -- passed to `\link[mongolite]{mongo}`
#' @param options ANY -- SSL options passed to `mongo` call.
#' @param aggregate list -- simulated responses from [mdbAggregate()] queries.
#' @param count list -- simulated responses from [mdbCount()] queries.
#' @param distinct list -- simulated responses from [mdbDistinct()] queries.
#' @param find list -- simulated responses from [mdbFind()] queries.
#' @param iterate list -- simulated responses from [mdbIterate()] queries.
#' @param mapreduce list -- simulated responses from [mdbMapreduce()] queries.
#' @param run list -- simulated responses from [mdbRun()] queries.
#' @param databases list -- simulated responses from [showDatabases()] queries.
#' @param collections list -- simulated responses from [showCollections()] queries.
#' @param pipeline,handler,pagesize,query,key,fields,sort,skip,limit,map,reduce,command,simplify,uri,dbname --
#'   arguments to the generic functions which are ignored in the `fake_mongo` methods.
#' @return An object of type `\linkS4class{fake_mongo}
#' @export fake_mongo
#'
fake_mongo <-
function (collection = "test", db = "test", url = "mongodb://localhost",
          verbose = FALSE, options = mongolite::ssl_options(),
          aggregate=list(),
          count=list(),
          distinct=list(),
          find=list(),
          iterate=list(),
          mapreduce=list(),
          run=list(),
          databases=list(),
          collections=list()) {
  new ("fake_mongo",collection=collection, db=db, url=url,
       verbose=verbose, options=options,
       aggregate.q=aggregate, count.q=count, distinct.q=distinct,
       find.q=find, iterate.q=iterate, mapreduce.q=mapreduce,
       run.q=run, databases.q=databases, collections.q=collections)
}

#' @rdname fake_mongo-class
setMethod("mdbAggregate","fake_mongo",
          function (db, pipeline='{}',  options='{"allowDiskUse":true}', handler=NULL,
                    pagesize = 1000, iterate=FALSE) {
  qqq <- db$que("aggregate")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbCount", "fake_mongo",
          function (db, query = '{}') {
  qqq <- db$que("count")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbDistinct","fake_mongo",
  function (db, key, query = '{}') {
    qqq <- db$que("distinct")
    if (qqq$hasNext())
      return(qqq$nextElement())
    else
      callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbFind","fake_mongo",
  function (db, query = '{}', fields = '{"_id":0}',
            sort = '{}', skip = 0,
            limit=0, handler  = NULL,
            pagesize = 1000) {
    qqq <- db$que("find")
    if (qqq$hasNext())
      return(qqq$nextElement())
    else
      callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbIterate", "fake_mongo",
          function (db, query = '{}', fields = '{"_id":0}',
                    sort = '{}', skip = 0, limit = 0) {
  qqq <- db$que("iterate")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbMapreduce","fake_mongo",
          function (db, map, reduce, query = '{}',
                    sort='{}', limit = 0) {
  qqq <- db$que("mapreduce")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})


#' @rdname fake_mongo-class
setMethod("mdbRun","fake_mongo",
  function (db, command = '{"ping":1}', simplify = TRUE) {
  qqq <- db$que("run")
  if (qqq$hasNext())
     return(qqq$nextElement())
  else
    callNextMethod()
})


#' @rdname fake_mongo-class
setMethod("showDatabases","fake_mongo",
          function(db=NULL, uri="mongodb://localhost",
                   options=mongolite::ssl_options()) {
  qqq <- db$que("databases")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})


#' @rdname fake_mongo-class
setMethod("showCollections","fake_mongo",
          function(db=NULL, dbname="test", uri="mongodb://localhost",
                   options=mongolite::ssl_options()) {
  qqq <- db$que("collections")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})
