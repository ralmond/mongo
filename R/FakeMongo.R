#' An object which iterates over a collection
#'
#' An `iterator` loops through a collection using the `$hasNext()` and `$nextElement()` methods.
#' This class also supports the `$one()` and `$batch()` methods to mimic the iterator returned
#' by the `\link[mongolite]{mongo}()$iterate()` method.
#'
#' # Class-based Methods
#'
#' * `$initialize(elements=list(),...)` Constructor
#' * `$hasNext()` -- Logical value.  Checks whether there are unseen
#' elements in the collection.  Position is not advanced.
#' * '$nextElement(warn=TRUE)` Returns the next item in the collection
#' and advances the position.  If no items remain, then
#' `NULL` is returned and a warning is issued if `warn` is `TRUE`.
#' * `$one()` Returns the next item in the collection.  Designed
#' to mimic the return from the `\link[mongolite]{mongo}$iterate()`
#' function. The next object, or `NULL` (without a warning) if the
#' collection is empty.
#' * `$batch(count)`  Fetechs `count` elements as a list, advancing
#' the position by the argument.  Issues a warning if there
#' are not `count` elements left in the collection.
#' Advances the position by `count`.
#' * `$reset()` Resets the position back to the beginning.  If an
#' argument is supplied, it also replaces the `elements`.
#'
#'
#' @note
#'
#' This is a utility class that serves two purposes.
#' (1) It implements a result queue for the `linkS4class{fake_mongo}` class.
#' (2) it mimics the iterator returned by the `mdbIterate()`
#' generic function, and so can be used in the result queue for the
#' `mdbIterate-fake_mongo` method.
#'
#' Unlike the internal iterator class from the `mongolite`, this one
#' has a `$hasNext()` method which is  part of the general iterator
#' recipe.  The `$one()` and `$batch()` methods should be compatible
#' with the internal `mongolite` iterator, can so it can be used as
#' drop in replacement.
#'
#' @return An object of class iterator.
#' @exportClass iterator
#' @seealso [mongo::mdbIterate()], `\linkS4class{fake_mongo}`
#' @examples
#' iter <- iterator(as.list(1:5))
#' while (iter$hasNext())
#'   print(iter$nextElement())
#'
#'
setRefClass("iterator",
            fields=c(
                #' @field elements list -- The objects to return
                elements="list",
                #' @field position integer -- a pointer to the last
                #' returned object
                position="integer"),
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

#'@rdname iterator-class
#'@export iterator
#'@param elements A list of elements for the iterator to return.
#'@returns The newly created iterator.
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
#' @param collection character -- name of the referenced collection
#' @param db character -- name of the referenced database
#' @param url character -- URI for accessing the database.
#' @param verbose logical -- passed to `\link[mongolite]{mongo}`
#' @param options ANY -- SSL options passed to `mongo` call.
#' @param noMongo logical -- If true (default), no attempt is made to connect to the Mongo database.
#' @param logging logical -- If true (default), then calls to the database will be logged.
#' @param aggregate list -- simulated responses from [mdbAggregate()] queries.
#' @param count list -- simulated responses from [mdbCount()] queries.
#' @param distinct list -- simulated responses from [mdbDistinct()] queries.
#' @param find list -- simulated responses from [mdbFind()] queries.
#' @param iterate list -- simulated responses from [mdbIterate()] queries.
#' @param mapreduce list -- simulated responses from [mdbMapreduce()] queries.
#' @param run list -- simulated responses from [mdbRun()] queries.
#' @param databases list -- simulated responses from [showDatabases()] queries.
#' @param collections list -- simulated responses from [showCollections()] queries.
#' @param pipeline,handler,pagesize,query,key,fields,sort,skip,limit,map,reduce,command,simplify,uri,dbname,con,bson,add,remove,data,stop_on_error,just_one,mdb,name,update,upsert,filters,multiple,... --
#'   arguments to the generic functions which are ignored in the `fake_mongo` methods.
#' @name fake_mongo-class
#' @aliases fake_mongo
#'
#' @section Class-Based Methods:
#'
#' * `$initialize(...)` -- See `fake_mongo` function for arguments.
#' * `$que(which)` -- Returns an individual response queue as an
#' `\linkS4class{iterator}`  The `which` argument should be one of the
#' names in the table in the Details session.
#' * `$resetQueue(which, newElements=NULL)` -- Calls the `$reset()` method on
#' `\linkS4class{iterator}` associated with operation `which`.  If
#' `newElements` is supplied, the elements or the iterator are replaced.
#' Note that if `which="iterate"`, then the queue is an iterator which
#' returns iterators.  The `$reset()` method is called on all of the
#' elements of the queue as well.
#' * `$resetAll()` -- Resets all Queues.
#' * `$logging(newState)` Checks or sets the logging state.  If the
#' argument is supplied, this sets the state.
#' * `logCall(call)` -- Logs a database CRUD operation.
#' The `call` argument is a named list.  The first element, named `op`
#' is the database operation (the name of the call minus the `mdb` prefix).
#' The remaining arguments are the values of the arguments in the CRUD call.
#' * getLog(newestFirst=TRUE) -- Fetches the entire log.  Log is
#' stored with the newest call first (reverse chronological order), so this
#' is the default order.
#' * `$lastLog()` -- Returns the most recently added element in the
#' log.
#' * `$resetLog()` -- Clears the call log.
#'
#'
#' @section Methods:
#'
#' This class overrides all of the normal \link{CRUD} (`mdbXXX`)
#' methods.
#'
#' For all methods, the internal `$logCall()` method is called giving
#' the details of the call.
#'
#' For the methods which correspond to a queue, the next element in
#' the corresponding queue will be returned.
#'
#' This allows faking the database connection to test functions which
#' interact with the mongo database.
#'
#' @details
#'
#' Internally the `fake_mongo` class has a list of iterators named "aggregate", "count", "distinct", "find",
#' "iterate", "mapreduce", "run", "databases", and "collections".  The corresponding methods will return the next entry
#' in the iterator (if it exists) or else will call the parent method to get the default return value (varies with generic
#' function).  Usually, no connection to a mongo database is made.
#'
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
#' If logging is turned on (either by setting `logging=TRUE` in the constructor, or by calling
#' `$logging(TRUE)`, then each `mdbXXX` method will log the call to the `log` collection.
#' The `$getLog()` and `$lastLog()` methods access the queue, and `$resetQueue()` resets it.
#'
#' @return An object of class `fake_mongo`
#' @exportClass fake_mongo
#' @export fake_mongo
#'
#' @examples
#' showClass("fake_mongo")
setRefClass("fake_mongo",
            fields=c(
                #' @field queues a named list of
                #' `\linkS4class{iterator}` objects which provide the
                #'simulated responses.
                queues="list",
                #' @field log a list of database calls made
                log="list",
                #' @field logp logical If `TRUE` then method class
                #' will be logged.
                logp="logical"),
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
                                       logging=TRUE,
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
                              verbose=verbose, options=options,
                              log=list(),logp=logging,...)
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
                  },
                  logging = function(newState) {
                    if (!missing(newState)) {
                      logp <<- isTRUE(newState)
                    }
                    logp
                  },
                  logCall = function(call) {
                    if (logp)
                      log <<- c(list(call),log)
                  },
                  getLog = function(newestFirst=TRUE) {
                    if (newestFirst) {
                      log
                    } else {
                      rev(log)
                    }
                  },
                  lastLog = function() {
                    if (length(log) > 0L) log[[1]]
                    else NULL
                  },
                  resetLog = function() {
                    log <<- list()
                  }
                  ))

#' @describeIn fake_mongo-class Constructor
#'
#' @return An object of type `\linkS4class{fake_mongo}
#' @export fake_mongo
#'
fake_mongo <-
function (collection = "test", db = "test", url = "mongodb://localhost",
          verbose = FALSE, options = mongolite::ssl_options(),
          noMongo=TRUE, logging=TRUE,
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
       verbose=verbose, options=options, noMongo=noMongo, logging=logging,
       aggregate.q=aggregate, count.q=count, distinct.q=distinct,
       find.q=find, iterate.q=iterate, mapreduce.q=mapreduce,
       run.q=run, databases.q=databases, collections.q=collections)
}

#' @rdname fake_mongo-class
setMethod("mdbAvailable","fake_mongo",function(db) TRUE)


#' @rdname fake_mongo-class
setMethod("mdbAggregate","fake_mongo",
          function (db, pipeline='{}',  options='{"allowDiskUse":true}', handler=NULL,
                    pagesize = 1000, iterate=FALSE) {
  db$logCall(list(op="aggregate", pipeline=pipeline, options=options, handler=handler,
              pagesize=pagesize, iterate=iterate))
  qqq <- db$que("aggregate")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbCount", "fake_mongo",
          function (db, query = '{}') {
  db$logCall(list(op="count",query=query))
  qqq <- db$que("count")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbDisconnect","fake_mongo",
          function(db) {
            db$logCall(list(op="disconnect"))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbDistinct","fake_mongo",
  function (db, key, query = '{}') {
    db$logCall(list(op="distinct",key=key,query=query))
    qqq <- db$que("distinct")
    if (qqq$hasNext())
      return(qqq$nextElement())
    else
      callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbDrop","fake_mongo",
          function(db) {
            db$logCall(list(op="drop"))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbExport","fake_mongo",
          function (db, con=stdout(), bson=FALSE,
                    query = '{}', fields = '{}',
                    sort = '{"_id":1}') {
            db$logCall(list(op="export",con=con,bson=bson,
                            query=query,fields=fields,sort=sort))
            callNextMethod()
          })
#' @rdname fake_mongo-class
setMethod("mdbImport","fake_mongo",
          function (db, con=stdout(), bson=FALSE) {
            db$logCall(list(op="import",con=con,bson=bson))
            callNextMethod()
          })


#' @rdname fake_mongo-class
setMethod("mdbFind","fake_mongo",
  function (db, query = '{}', fields = '{"_id":0}',
            sort = '{}', skip = 0,
            limit=0, handler  = NULL,
            pagesize = 1000) {
    db$logCall(list(op="find",query=query, fields=fields, sort=sort,
                    skip=skip, limit=limit, handler=handler, pagesize=pagesize))
    qqq <- db$que("find")
    if (qqq$hasNext())
      return(qqq$nextElement())
    else
      callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbIndex","fake_mongo",
          function (db, add=NULL, remove=NULL) {
            db$logCall(list(op="index",add=add,remove=remove))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbInfo","fake_mongo",
          function (db) {
            db$logCall(list(op="info"))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbInsert","fake_mongo",
          function (db,data, pagesize=100, stop_on_error=TRUE,...){
            db$logCall(list(op="insert", data=data, pagesize=pagesize,
                            stop_on_error=stop_on_error, rest = list(...)))
            callNextMethod()
          })



#' @rdname fake_mongo-class
setMethod("mdbIterate", "fake_mongo",
          function (db, query = '{}', fields = '{"_id":0}',
                    sort = '{}', skip = 0, limit = 0) {
  db$logCall(list(op="iterate",query=query,fields=fields,sort=sort,
                  skip=skip,limit=limit))
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
  db$logCall(list(op="mapreduce",map=map,reduce=reduce, query=query, sort=sort, limit=limit))
  qqq <- db$que("mapreduce")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})


#' @rdname fake_mongo-class
setMethod("mdbRemove","fake_mongo",
          function (db, query='{}', just_one=FALSE) {
            db$logCall(list(op="remove", query=query, just_one=just_one))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbRename","fake_mongo",
          function (mdb, name, db=NULL) {
  mdb$logCall(list(op="rename",name=name,db=db))
  callNextMethod()
})

#' @rdname fake_mongo-class
setMethod("mdbReplace","fake_mongo",
          function (db, query, update='{}', upsert=FALSE) {
            db$logCall(list(op=ifelse(upsert,"upsert","replace"), query=query, update=update))
            callNextMethod()
          })
#' @rdname fake_mongo-class
setMethod("mdbUpsert","fake_mongo",
          function (db, query, update='{}', upsert=TRUE) {
            db$logCall(list(op=ifelse(upsert,"upsert","replace"), query=query, update=update))
            callNextMethod()
          })

#' @rdname fake_mongo-class
setMethod("mdbRun","fake_mongo",
  function (db, command = '{"ping":1}', simplify = TRUE) {
    db$logCall(list(op="run",command=command,simplify=simplify))
    qqq <- db$que("run")
    if (qqq$hasNext())
       return(qqq$nextElement())
    else
      callNextMethod()
})


#' @rdname fake_mongo-class
setMethod("mdbUpdate","fake_mongo",
          function (db, query, update='{"$set":{}}', filters=NULL,
                    upsert=FALSE, multiple=FALSE) {
            db$logCall(list(op="update", query=query, update=update,
                            filters=filters, upsert=upsert,multiple=multiple))
            callNextMethod()
          })
#' @rdname fake_mongo-class
setMethod("showDatabases","fake_mongo",
          function(db=NULL, uri="mongodb://localhost",
                   options=mongolite::ssl_options()) {
  db$logCall(list(op="showDatabases",uri=uri))
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
  db$logCall(list(op="showCollections",dbname=dbname,uri=uri))
  qqq <- db$que("collections")
  if (qqq$hasNext())
    return(qqq$nextElement())
  else
    callNextMethod()
})
