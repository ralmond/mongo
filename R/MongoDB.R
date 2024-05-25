#' @import methods

setOldClass("mongo")
## Missing method
setMethod("toString","mongo",function(x,...) {
  paste0("<Mongo collection: '",
         mongolite:::mongo_collection_name(parent.env(x)$col),
         "'>")
})

## TODO would it be better to redo this using R6Class package?

MongoDB.class <- function() {
  ## Dummy function so that S4 objects appear in RStudio content page.
}
#' MongoDB -- Reference class wrapping a connection to a Mongo database collection.
#'
#' @name MongoDB-class
#' @aliases MongoDB
#'
#' @details
#' Including a \link[mongolite]{mongo} object in an Reference class presents a potential race condition.
#' The prototype class is built at package load time, however, calling the `\link[mongolite]{mongo}` may not work
#' at this time.  The `MongoDB` class works around this by capturing the arguments to the `mongo` call, and then
#' creating the actual database connection when the database is first accessed.  The database should always be accessed
#' through the `$db()` method which builds the database if needed.
#'
#' @param collection character -- name of collection
#' @param db character -- name of database
#' @param url character -- URI for mongo connection (see [makeDBuri()])
#' @param verbose logical -- Should operate in verbose mode.
#' @param noMongo logical -- If true, then no connection to Mongo database will be made,
#' and CRUD operations will become no-ops.
#' @param options -- SSL options for connections, see [mongolite::ssl_options()].
#' @section Class-based Methods:
#'
#' * `$initialize(collection, db, url, verbose, options, ...)` -- Constructor.
#' * `$db()` --   Returns the the actual database connection
#' (`\link[mongolite]{mongo}` object), or `NULL` if `uri==""` or
#' `noMongo==TRUE`.  If the actual call to `\link[mongolite]{mongo}
#' has not been made, this method will create the connection;
#' otherwise, the cached connection is returned.
#' * `$available()` --  Returns false if no database is present (i.e.,
#' `noMongo` is `TRUE`. Used to suppress actual mongo calls when
#' database is not available.
#' * `$resetDB()` -- Resets the `mongoObj` field to force a
#' reconnection to Mongo the next time `$db()` is called.  This is
#' probably useful to call when restoring an R session.
#' * `$toString()` -- Returns a string represenation of an object.
#' @section Methods:
#'
#' The S4 generic functions correspond to the normal \link{CRUD}
#' (Create, Read, Update and Delete) methods.  Particularly:
#' `\link{mdbAggregate}`, `\link{mdbCount}`, `\link{mdbDisconnect}`, `\link{mdbDrop}`,
#' `\link{mdbExport}`, `\link{mdbFind}`, `\link{mdbImport}`, `\link{mdbIndex}`,
#' `\link{mdbInsert}`, `\link{mdbIterate}`, `\link{mdbMapreduce}`,
#' `\link{mdbRemove}`, `\link{mdbRename}`, `\link{mdbReplace}`,
#' `\link{mdbRun}`, `\link{mdbUpdate}`, `\link{showCollections}` and
#' `\link{showDatabases}`.
#'
#' @note
#' Many of the examples use `MongoDB(...,noMongo=!interactive())`.
#' This means the dummy mechanism will be used during package checking
#' (where Mongo may or may not be available in the development
#' environment), but running the examples from the help files will
#' make the connections (and will generate an error if Mongo is not
#' installed).
#'
#' @exportClass MongoDB
#' @export MongoDB
#' @importFrom mongolite mongo
#' @seealso [\link[mongolite]{mongo}]
#' More extensive documentation on most of the `mdbXXX` functions can be found at the Mongo API documentation web site.
#' \url{https://www.mongodb.com/docs/manual/reference/command/}
#'
#' @examples
#' mdp <- MongoDB("test","test","mongodb://localhost")
#' \dontrun{
#' # This will generate an error if mongo doesn't exist.
#' mdbCount(mdp,'{}')
#' }
#' nullmdp <- MongoDB(noMongo=TRUE)
#' mdbCount(nullmdp)
#' # This will return `NA`.
setRefClass("MongoDB",
            field=c(
                #' @field mongoObj ANY -- This is the actual
                #' `\link[mongolite]{mongo}` object or `NULL` if it has
                #' not been initialized yet.
                mongoObj="ANY",
                #' @field uri character -- URI for the mongo connection.
                uri="character",
                #' @field dbname character -- The name of the mongo database.
                dbname="character",
                #' @field colname character -- The name of the mongo collection
                colname="character",
                #' @field noMongo logical -- If `TRUE`, then the
                #various methods will turn into NO-OPS.
                #' This allows a class which contains a reference to a
                #' Mongo database to ignore the database calls when
                #' there is no database to connect to.
                noMongo="logical",
                #' @field verbose logical -- This field is passed on
                #' to the `\link[mongolite]{mongo}` call.
                verbose="logical",
                #' @field options ANY -- This field is passed on to
                #' the  `\link[mongolite]{mongo}` call.  It is used to
                #' store additional SSL connection information, see
                #' `\link[mongolite]{ssl_options}`.
                options = "ANY"),
              methods=list(
                  initialize= function(collection="test",
                                       db="test", url="mongodb://localhost",
                                       verbose=FALSE,noMongo=FALSE,
                                       options=mongolite::ssl_options(),
                                       ...) {
                    callSuper(mongoObj=NULL,
                              colname=collection,dbname=db,uri=url,
                              noMongo=noMongo,
                              verbose=verbose, options=options, ...)
                    },
                  db = function () {
                    if (noMongo) return(NULL)
                    if (is.null(mongoObj) && nchar(uri) >0L) {
                      mongoObj <<- mongolite::mongo(colname,dbname,uri)
                    }
                    mongoObj
                  },
                  available = function() {
                    !noMongo
                  },
                  resetDB = function () {
                    mongoObj <<- NULL
                  },
                  toString = function() {
                    sprintf("<MongoDB: %s.%s@%s>",
                            dbname,colname,
                            ifelse(noMongo,"/dev/null",uri))
                  }))
#' @rdname MongoDB-class
#' @returns An object of class `MongoDB`
MongoDB <-
  function (collection="test",
                     db="test", url="mongodb://localhost",
                     verbose=FALSE,noMongo=FALSE,
                     options=mongolite::ssl_options()) {
  ## If url is empty string, then suppress.
  if (missing(noMongo) && !missing(url) && (nchar(url) == 0L || length(url)==0L)) noMongo = TRUE
  new("MongoDB", collection=collection, db=db, url=url, verbose=verbose,
      noMongo=noMongo, options=options)
}

setOldClass("mongo")

#' Class which supports the mdbCRUD methods.
#' @name JSONDB-class
#' @aliases mdbCRUD CRUD
#'
#' @details
#' The CRUD (Create, Read, Update and Delete) are the basic set of
#' operators for manipulating a database.  The `mongo` package defines
#' a number of operators (mostly named `mdbXXX`) which calls the
#' corresponding CRUD operations.  The `JSONDB` class is intended for
#' any class that supports these operations.
#'
#' @exportClass JSONDB
#' @details
#' The following operations are supported:
#' * [mdbAvailable()] -- Returns logical value.  If false, CRUD operations
#' will basically be no-ops.
#' * [mdbAggregate()] -- Runs an aggregation pipeline
#' * [mdbCount()] -- Counts records matching query
#' * [mdbDisconnect()] -- Drops connection to database (will be
#' reconnected on next operation).
#' * [mdbDistinct()] -- Lists unique values of a field.
#' * [mdbDrop()] -- Drops the collection from the database.  This is a
#' fast way to clear a database.
#' * [mdbImport()],[mdbExport()] -- Imports/Exports documents
#' into/from a collection from a file (or connection).
#' * [mdbFind()] -- Finds documents matching query and returns result
#' as a `data.frame`.
#' * [mdbIndex()] -- Adds or removes an index from a collection.
#' * [mdbInfo()] -- Returns info about a collection.
#' * [mdbInsert()] -- Adds one or more documents into a collections.
#' Works with both `data.frame` (one document per row) and JSON
#' character vectors (one document per element).
#' * [mdbIterate()]/[mdbFindL()] -- Finds documents matching query and returns
#' these as an iterator/list.
#' * [mdbMapreduce()] -- Executes a mapreduce operation using
#' javascript map and reduce oerations.
#' * [mdbRemove()] -- Removes matching documents from a collection.
#' * [mdbRename()] -- Renames a collection.
#' * [mdbReplace()]/[mdbUpsert()] -- Replaces a document in a
#' collection.
#' * [mdbRun()] -- Runs a Mongo command.
#' * [mdbUpdate()] -- Modifies records in a database.
#' * [showDatabases()] -- Lists available databases.
#' * [showCollections()] -- Lists collections in a database.
setClassUnion("JSONDB",c("MongoDB","mongo"))

setMethod("toString","MongoDB", function(x, ...) {
  x$toString()
})
setMethod("show","MongoDB",function(object) {
  cat(toString(object),"\n")
})

mdb.available <- function() {}
#' Is the collection available for writing.
#'
#' Returns `FALSE` if the connection to the database is not available, so the CRUD operations (\link{mdbCRUD}).
#' will not be executed.
#'
#' @param db (or MongoDB mongo) -- Reference to collection
#' @return logical value.  If false, there is no active connection and CRUD operations will be no-ops.
#' @export mdbAvailable
setGeneric("mdbAvailable",function(db) standardGeneric("mdbAvailable"))
#' @rdname mdbAvailable
#' @exportMethod mdbAvailable
setMethod("mdbAvailable","MongoDB",function(db) db$available())
#' @rdname mdbAvailable
#' @note
#' When using the \code{mongolite::\link[mongolite]{mongo}} collection reference
#' operations are not skipped.
setMethod("mdbAvailable","mongo",function(db) TRUE)


mdb.aggregate <-
  function(){
}
#' Execute Aggregration Pipeline
#'
#' @param db MongoDB or mongo -- The database collection handle.
#' @param pipeline character -- a json object describing the pipeline.
#' @param options character -- a json object giving options to the pipeline.  (This is missing from
#' the `mongolite` documentation, so see Mongo documentation).
#' @param handler -- undocumented.
#' @param pagesize integer -- Size of pages
#' @param iterate logical -- If `TRUE` return an iterator (see `\link{mdbIterate}`), if false a `data.frame`.
#'
#' @details
#' Execute a pipeline using the Mongo aggregation framework.  Set `iterate = TRUE` to return an iterator
#' instead of a data frame.
#'
#' @returns Data frame or iterator with query results.
#' @seealso [\link[mongolite]{mongo}]
#' \url{https://www.mongodb.com/docs/manual/reference/command/aggregate/}
#' @export mdbAggregate
#' @exportMethod mdbAggregate
#'
#' @examples
# # adapted from mongolite user guide
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' stats <- mdbAggregate(irisdb,
#'   paste('[{"$group":{"_id":"$Species", "count": {"$sum":1},',
#'                     '"average_Petal_Length": {"$avg":"$Petal_Length"}',
#'                     '}}]'),
#'   options = '{"allowDiskUse":true}'
#' )
#' if (!is.null(stats)) {
#'   names(stats) <- c("Species", "Count", "Petal Length")
#' }
#' print(stats)
setGeneric("mdbAggregate", function (db, pipeline='{}',
                                     options='{"allowDiskUse":true}',
                                     handler=NULL,
                                     pagesize = 1000, iterate=FALSE) {
  standardGeneric("mdbAggregate")
})
#' @rdname mdbAggregate
setMethod("mdbAggregate","MongoDB",
          function (db, pipeline='{}', options='{"allowDiskUse":true}', handler=NULL,
                    pagesize = 1000, iterate=FALSE) {
            if (!db$available()) return (NULL)
            db$db()$aggregate(pipeline,options, handler,pagesize,iterate)
})
#' @rdname mdbAggregate
setMethod("mdbAggregate","mongo",
          function (db, pipeline='{}', options='{"allowDiskUse":true}', handler=NULL,
                    pagesize = 1000, iterate=FALSE) {
            db$aggregate(pipeline,options, handler,pagesize,iterate)
})

mdb.count <-
  function(){
}
#' Counts the number of records matching Query.
#'
#' @param db MongoDB or mongo -- Reference to the collection
#' @param query character -- JSON expression giving the query. See
#' `\link{buildJQuery}`.
#' @details
#' The `query` argument is a partial match for the records (in JSON format) which is
#' essentially a partial match for the object.
#'
#' @return integer The number of records found (or `NA` if `noMongo = TRUE`)
#' @seealso `\link[mongolite]{mongo}`, `\link{buildJQuery}`
#' \url{https://www.mongodb.com/docs/manual/reference/command/count/}
#' @export mdbCount
#' @exportMethod mdbCount
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbCount(irisdb)
#' mdbCount(irisdb,'{"Species":"setosa"}')
#' mdbCount(irisdb,buildJQuery(Sepal.Width=c(lt=3),Petal.Width=c(gt=.3,lt=1.8)))
setGeneric("mdbCount", function (db, query = '{}') standardGeneric("mdbCount"))
#' @rdname mdbCount
setMethod("mdbCount", "MongoDB",
          function (db, query = '{}') {
            if (!db$available()) return(NA_integer_)
            db$db()$count(query)
})
#' @rdname mdbCount
setMethod("mdbCount", "mongo",
          function (db, query = '{}') {
            db$count(query)
})

mdb.disconnect <-
  function(){
}
#' Disconnects connection to database.
#'
#' @param db MongoDB or mongo -- The database connection to drop.
#' @param gc logical -- Should the garbage collection be run.
#'
#' @return status message
#' @seealso [\link[mongolite]{mongo}]
#' @export mdbDisconnect
#' @exportMethod mdbDisconnect
#'
#' @details
#' While this closes the connection, the `MongoDB` object retains the
#' information needed to re-open it.  It will be reopened on the next
#' call.
#'
#' @examples
#' ## Setting noMongo=TRUE, so we don't actually run this.
#' testDB <- MongoDB("test", noMongo=!interactive())
#' mdbDisconnect(testDB)
setGeneric("mdbDisconnect", function (db, gc = TRUE)
  standardGeneric("mdbDisconnect"))
#' @rdname mdbDisconnect
setMethod("mdbDisconnect", "MongoDB",
          function (db, gc = TRUE) {
            if (!db$available()) return (NULL)
            db$db()$disconnect(gc)
            db$resetDB()
})
#' @rdname mdbDisconnect
setMethod("mdbDisconnect", "mongo",
          function (db, gc = TRUE) {
            db$disconnect(gc)
})

mdb.distinct <-
  function(){
}
#' Find the distinct values of a particular field
#'
#' @param db (or MongoDB mongo) -- Reference to database collection
#' @param key character -- field to extract
#' @param query character -- JSON expression indicating subcollection.
#' See `\link{buildJQuery}`.
#'
#' @return list of values
#' @seealso [\link[mongolite]{mongo}]
#' \url{https://www.mongodb.com/docs/manual/reference/command/distinct/}
#' @export mdbDistinct
#' @exportMethod mdbDistinct
#'
#' @details
#' Finds the unique values of the field specified by `key`.  If
#' `query` is supplied, then search is restricted to records
#' satisfying query.
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbDistinct(irisdb,"Species")
setGeneric("mdbDistinct", function (db, key, query = '{}')
  standardGeneric("mdbDistinct"))
#' @rdname mdbDistinct
setMethod("mdbDistinct","MongoDB",
  function (db, key, query = '{}') {
    if (!db$available()) return(NA)
    db$db()$distinct(key,query)
})
#' @rdname mdbDistinct
setMethod("mdbDistinct","mongo",
  function (db, key, query = '{}') {
    db$distinct(key,query)
})

mdb.drop <-
  function(){
}
#' Drops the database collection
#'
#' @param db (or MongoDB mongo) -- Reference to collection to drop
#'
#' @return miniprint object giving status
#' @seealso [\link[mongolite]{mongo}]
#' \url{https://www.mongodb.com/docs/manual/reference/command/drop/}
#' @export mdbDrop
#' @exportMethod mdbDrop
#'
#' @details
#' Dropping the collection and then inserting values is an easy way to
#' reset the collection contents, so is a common idiom.
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
setGeneric("mdbDrop", function (db)
  standardGeneric("mdbDrop"))
#' @rdname mdbDrop
setMethod("mdbDrop","MongoDB",
  function (db) {
    if (!db$available()) return (NULL)
    db$db()$drop()
})
#' @rdname mdbDrop
setMethod("mdbDrop","mongo",
  function (db) {
    db$drop()
})

mdb.import <-
  function(){
}
#' Exports/imports data from external JSON or BSON file
#' @aliases mdbImport
#'
#' @param db (or MongoDB mongo) -- Database collection of focus
#' @param con connetion -- a file or other connection for import/export
#' @param bson logical -- If `TRUE` use BSON (binary JSON) if `FALSE`
#' then `JSON`
#' @param query character -- JSON expression providing a query
#' selecting records to export.  See `\link{buildJQuery}`.
#' @param fields character -- JSON expression selecting fields of the
#' objects to be exported.  See `\link{mdbFind}` for details.
#' @param sort character -- JSON expression indicating field and
#' direction for sorting exported records. See `\link{mdbFind}` for details.
#'
#' @details
#' The `export` function dumps a collection to a file or other
#' connection.  This can be in either plain text (utf8) `JSON` format,
#' or a binary `BSON` format (this is specific to Mongo).  The
#' `import` function reverses this process.
#'
#' On export, the `query`, `fields` and `sort` fields can be used to
#' control what is exported.
#'
#' @return miniprint object giving the status
#' @seealso [\link[mongolite]{mongo}]
#' \url{https://www.mongodb.com/docs/manual/reference/command/import/}
#' \url{https://www.mongodb.com/docs/manual/reference/command/export/}
#' @export mdbExport
#' @export mdbImport
#' @exportMethod mdbExport
#' @exportMethod mdbImport
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbCount(irisdb)
#' outfile <- tempfile(fileext="json")
#' mdbExport(irisdb,file(outfile),sort='{"Petal_Length":-1}')
#' mdbDrop(irisdb)
#' mdbCount(irisdb)
#' mdbImport(irisdb,file(outfile))
#' mdbCount(irisdb)
setGeneric("mdbExport", function (db, con=stdout(), bson=FALSE,
                                  query = '{}', fields = '{}',
                                  sort = '{"_id":1}')
  standardGeneric("mdbExport"))
#' @rdname mdbExport
setMethod("mdbExport","MongoDB",
          function (db, con=stdout(), bson=FALSE,
                                  query = '{}', fields = '{}',
                                  sort = '{"_id":1}') {
            if (!db$available()) return(NULL)
            db$db()$export(con,bson,query,fields,sort)
})
#' @rdname mdbExport
setMethod("mdbExport","mongo",
          function (db, con=stdout(), bson=FALSE,
                                  query = '{}', fields = '{}',
                                  sort = '{"_id":1}') {
            db$export(con,bson,query,fields,sort)
})
#' @rdname mdbExport
setGeneric("mdbImport", function (db, con, bson=FALSE)
  standardGeneric("mdbImport"))
#' @rdname mdbExport
setMethod("mdbImport","MongoDB",
  function (db, con, bson=FALSE) {
    if (!db$available()) return(NULL)
    db$db()$import(con,bson)
})
#' @rdname mdbExport
setMethod("mdbImport","mongo",
  function (db, con, bson=FALSE) {
    db$import(con,bson)
})

mdb.find <-
  function(){
}
#' Finds records which match the query and returns as data frame
#'
#' @param db (or MongoDB mongo) -- the database collection.
#' @param query character -- A query string in `JSON` format.  See
#' `\link{buildJQuery}`.
#' @param fields character -- A JSON expression describing which
#' fields to extract from the selected records.  The default returns
#' all fields except for the internal Mongo id field (see `\link{m_id}`.
#' @param sort character -- A JSON field indicating how the record
#' should be sorted.
#' @param skip integer -- The number of records to skip.
#' @param limit integer -- The maximum number of records to return.
#' @param handler (or NULL function) -- Undocumented.
#' @param pagesize integer -- Used for buffering
#'
#' @details
#' The `mdbFind` function takes a collection of records and turns it
#' into a `data.frame` with the columns representing the frame.  To
#' process the raw JSON stream, try `\link{mdbIterate}`
#'
#' @details
#'
#' The `query`, `fields` and `sort` are all JSON expressions.  Note
#' that field names shuld be quoted inside these query strings (the
#' quotes are optional in the Mongo shell, but not here).  I recommend
#' using single quotes for the outer expression and double quotes
#' inside the JSON string.
#'
#' ### Query
#'
#' The mongo query is a rather rich language.  The simplest version
#' restricts a field to a specific value.  For example
#' `'{"Species":"virginica"}'` would select only virginica irises.
#' There are a number of different operators which can used to specify
#' the query, for examples `'{"Species":{$ne:"virginica"}}'` and
#' `'{"Species":{$in:["setosa","versicolor"]}}'` both select the other
#' iris types.
#'
#' The mongo operators are "$eq" -- equals, "$gt" -- greater than,
#' "$gte" -- greater than or equals, "$lt" -- less than, "$lte" --
#' less than or equals, "$ne" -- not equal,"$nin" -- not in (argument
#' is a list ('[]'),"$in" -- in (argument is a list) and "$regex" -- a
#' regular expression.
#'
#' The function `\link{buildJQuery}` uses a more R-like syntax and
#' converts them to JSON.  This makes it easier to build a query
#' inside of R.
#'
#' ### fields
#'
#' The fields JSON expression should be a collection of fields with a
#' `true` or `false` (or 0 or 1).  Note that the "_id" field is
#' automatically included unless explicitly excluded.  For example:
#' `{"Petal.Length":1, "Petal.Width":1, "Species":1, "_id":0}` will
#' select the petal length and width field and species field.  See
#' the topic `Projection` in the Mongo manual for more details.
#'
#' ### sort
#'
#' This is a short object which gives the name of the field to sort on
#' and the direction (1 for ascending, -1 for descending).  If more
#' than one sort key is given, the first one is given the highest
#' priority.  The sort keys should be included in the field.  For example
#' `{"Petal.Length":1}` sort in ascending order according to petal
#' length.  See the `sort` function in the Mongo reference manual.
#'
#' @return data.frame giving query results
#' @seealso `\link[mongolite]{mongo}`, `\link{buildJQuery}`
#' `\link{getOneRec}`, `\link{getManyRecs}`, `\link{mdbIterate}`
#' \url{https://www.mongodb.com/docs/manual/reference/operator/query/}
#' \url{https://www.mongodb.com/docs/manual/reference/command/find/}
#' \url{https://www.mongodb.com/docs/manual/reference/method/db.collection.find/#std-label-find-projection}
#' \url{https://www.mongodb.com/docs/manual/reference/method/cursor.sort/#mongodb-method-cursor.sort}
#' @export mdbFind
#' @exportMethod mdbFind
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbFind(irisdb,buildJQuery(Species="setosa"),
#'         fields = '{"Petal_Width":1, "Petal_Length":1}',
#'         sort = '{"Petal_Width":-1}', limit=10)
setGeneric("mdbFind", function (db, query = '{}', fields = '{"_id":0}',
                                sort = '{}', skip = 0,
                                limit=0, handler = NULL,
                                pagesize = 1000)
   standardGeneric("mdbFind"))
#' @rdname mdbFind
setMethod("mdbFind","MongoDB",
  function (db, query = '{}', fields = '{"_id":0}',
            sort = '{}', skip = 0,
            limit=0, handler  = NULL,
            pagesize = 1000) {
    if (!db$available()) return(data.frame())
    db$db()$find(query,fields,sort,skip,limit,handler,pagesize)
})
#' @rdname mdbFind
setMethod("mdbFind","mongo",
  function (db, query = '{}', fields = '{"_id":0}',
            sort = '{}', skip = 0,
            limit=0, handler  = NULL,
            pagesize = 1000) {
    db$find(query,fields,sort,skip,limit,handler,pagesize)
})


mdb.index <-
  function(){
}
#' Build/remove an index for the collection.
#'
#' @param db (or MongoDB mongo) -- Collection in question.
#' @param add character -- JSON object describing fields to index.
#' @param remove character -- Name of indexes to remove.
#'
#' @details
#' If `add` is specified, then a new index is added.  If `remove` then
#' the index is removed.  If neither is specified, then a data frame
#' giving the existing indexes is returned.
#'
#' @details
#'
#' The syntax of the `add` field is similar to the `sort` argument of
#' `\link{mdbFind}.  The `remove` function uses the `name` from the
#' returned data.frame.
#'
#' If sorted queries are going to be frequent, then building indexes
#' will improve performance.
#'
#' @return data frame desribing indexes.
#' @seealso [\link[mongolite]{mongo}]
#' \url{https://www.mongodb.com/docs/manual/reference/method/db.collection.createIndex/}
#' @export mdbIndex
#' @exportMethod mdbIndex
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbIndex(irisdb,add=buildJQuery("Petal_Length"=1))
#' mdbIndex(irisdb,add='{"Petal_Length":1,"Petal_Width":-1}')
#' indexes <- mdbIndex(irisdb)
#' print(indexes)
#' mdbIndex(irisdb,remove="Petal_Length_1")
setGeneric("mdbIndex", function (db, add=NULL, remove=NULL)
  standardGeneric("mdbIndex"))
#' @rdname mdbIndex
setMethod("mdbIndex","MongoDB",
  function (db, add=NULL, remove=NULL) {
    if (!db$available()) return(data.frame())
    db$db()$index(add,remove)
})
#' @rdname mdbIndex
setMethod("mdbIndex","mongo",
  function (db, add=NULL, remove=NULL) {
    db$index(add,remove)
})

mdb.info <-
  function(){
}
#' Get Information about the collection
#'
#' @param db (or MongoDB mongo) The collection of interest.
#'
#' @return Object of class `miniprint` giving information about the collection.
#' @seealso [\link[mongolite]{mongo}]
#' @export mdbInfo
#' @exportMethod mdbInfo
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbInfo(irisdb)
setGeneric("mdbInfo", function (db)
  standardGeneric("mdbInfo"))
#' @rdname mdbInfo
setMethod("mdbInfo","MongoDB",
  function (db) {
    if (!db$available()) return(NULL)
    db$db()$info()
})
#' @rdname mdbInfo
setMethod("mdbInfo","mongo",
  function (db) {
    db$info()
})


mdb.insert <-
  function(){
}
#' Insert a new record into a collection
#'
#' Inserts one or more records.  If the `data` argument is a `data.frame`, then
#' each row becomes a new record.
#'
#' @param db (or MongoDB mongo) -- Collection into which new recrods
#' will be inserted
#' @param data (or data.frame named list character) -- New data to be inserted.
#' @param pagesize integer -- size of data stores
#' @param stop_on_error logical
#' @param ... -- extra data
#'
#'
#' @details
#'
#' ### Data frames
#'
#' Data frames are converted into mongo documents and then inserted.
#' Each row is a document, and the fields in the document correspond
#' to properties.  This is perhaps the easiest way to use this
#' function.  `mdbInsert` save a data frame in a mongo collection and
#' `\link{mdbFind}` retrieves it.
#'
#' ### Character
#'
#' An alternative is to express the document to be stored as a JSON
#' string.  If the input is a character vector with each element being
#' a complete JSON document, these will be added to the collection.
#' The function `\link[jsonlite]{serializeJSON}` in the jsonlite
#' package converts an R object to JSON in a way that will reproduce
#' the object but is not particularly easy to find or index in the
#' database.  The function `\link{jsonlite}{toJSON}` produces a more
#' readable version, but still has issues (in partuclar, it does not
#' distinguish between scalar and vector fields).  The function
#' `\link{as.json}` provides a mechanism for encoding S4 objects as
#' JSON expressions.
#'
#' The function `\link{saveRec}` provides a more object-oriented
#' interface for saving a single S4 object.
#'
#' ### List
#'
#' The source code for `\link[mongolite]{mongo}()$insert()` provides a
#' mechanism for using lists, but does not describe what the lists
#' elements should be.
#'
#' @return Object of class `miniprint` giving status information.
#' @seealso `\link[mongolite]{mongo}`, `\link[jsonlite]{toJSON}`,
#' `\link[jsonlite]{serializeJSON}`, `\link{as.json}`, `\link{saveRec}`
#' \url{https://www.mongodb.com/docs/manual/reference/method/db.collection.insert/}
#' @export mdbInsert
#' @exportMethod mdbInsert
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' testdb <- MongoDB("test",noMongo=!interactive())
#' mdbDrop(testdb)
#' mdbInsert(testdb,'{"Student":"Fred", "Scores":[83, 87, 91, 79],
#' "Grade":"B"}')
#'
setGeneric("mdbInsert", function (db, data, pagesize= 1000,
                                  stop_on_error = TRUE, ...)
  standardGeneric("mdbInsert"))
#' @rdname mdbInsert
setMethod("mdbInsert","MongoDB", function (db, data, pagesize= 1000,
                                           stop_on_error = TRUE, ...) {
  if (!db$available()) return(NULL)
  db$db()$insert(data, pagesize, stop_on_error, ...)
})
#' @rdname mdbInsert
setMethod("mdbInsert","mongo", function (db, data, pagesize= 1000,
                                           stop_on_error = TRUE, ...) {
  db$insert(data, pagesize, stop_on_error, ...)
})

mdb.iterate <-
  function(){
}
#' Returns documents as lists (jlists) from the database.
#'
#' @param db (or MongoDB mongo) -- the database collection.
#' @param query character -- A query string in `JSON` format.  See
#' `\link{buildJQuery}` and `\link{mdbFind}.`
#' @param fields character -- A JSON expression describing which
#' fields to extract from the selected records.  The default returns
#' all fields except for the internal Mongo id field (see `\link{m_id}`.
#' @param sort character -- A JSON field indicating how the record
#' should be sorted.
#' @param skip integer -- The number of records to skip.
#' @param limit integer -- The maximum number of records to return.
#'
#' @details
#' Unlike the `\link{mdbFind}` operation, which converts to the query
#' output to a data frame, `mdbIterate` produces an iterator, which
#' will cycle through the query results, which are returned as `\link{jlist}`
#' objects.
#'
#' @section Iterators:
#'
#' The iterator object returned from this function has two methods:
#' * `$one()`  -- Returns the next object, or `NULL` if there is none.
#' * `$batch(n)` -- Returns the next `n` objects.
#'
#' @return An iterator with the values.
#' @seealso [\link[mongolite]{mongo}]
#' @export mdbIterate
#' @exportMethod mdbIterate
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' iter <- mdbIterate(irisdb,limit=10)
#' if (!is.null(iter)) {
#' iter$one()
#' iter$batch(3)
#' ## Note extra parens.
#' while (!is.null((item <- iter$one()))) {
#'   print(sprintf("A %s iris with petal length %f",
#'                 item$Species,item$Petal_Length))
#' }
#'}
setGeneric("mdbIterate", function (db, query = '{}', fields = '{"_id":0}',
                                   sort = '{}', skip = 0, limit = 0)
  standardGeneric("mdbIterate"))
#' @rdname mdbIterate
setMethod("mdbIterate", "MongoDB",
          function (db, query = '{}', fields = '{"_id":0}',
                    sort = '{}', skip = 0, limit = 0) {
            if (!db$available()) return(NULL)
            db$db()$iterate(query,fields, sort, skip, limit)
})
#' @rdname mdbIterate
setMethod("mdbIterate", "mongo",
          function (db, query = '{}', fields = '{"_id":0}',
                    sort = '{}', skip = 0, limit = 0) {
            db$iterate(query,fields, sort, skip, limit)
})

#' @rdname mdbIterate
#' @export mdbFindL
setGeneric("mdbFindL", function (db, query = '{}', fields = '{"_id":0}',
                                   sort = '{}', skip = 0, limit = 0)
  standardGeneric("mdbFindL"))
#' @rdname mdbIterate
#' @exportMethod mdbFindL
setMethod("mdbFindL", "JSONDB",
  function (db, query = '{}', fields = '{"_id":0}',
            sort = '{}', skip = 0, limit = 0) {
    n <- limit
    if (n== 0L || !is.finite(n)) {
      n <-mdbCount(db,query)
      if (is.na(n)) return (NULL)
    }
    iter <- mdbIterate(db,query,fields,sort,skip,limit)
    if (is.null(iter)) return (NULL)
    iter$batch(n)
})




mdb.mapreduce <-
  function(){
}
#' Applies a summary operation to a collection
#'
#' Runs a map-reduce operation in the database side.  The `map` and
#' `reduce` functions are expressed as javascript methods.
#'
#' @param db (or MongoDB mong) -- The collection to operate on.
#' @param map character -- A javascript function to apply to each document.
#' @param reduce character -- A javascript function to summarize the result
#' @param query character -- A JSON query to slect part of the
#' collection.  See `\link{buildJQuery}` and `\link{mdbFind}`.
#' @param sort charcter -- JSON object giving sorting order for result set (see `\link{mdbFind}`).
#' @param limit integer -- maximum number of records to process
#'
#'
#' @details
#'
#' The Mongo database manual suggests that aggregation piplelines have
#' better performance than map-reduce.  Starting in Mongo 5.0
#' map-reduce is depricated.
#'
#' @return data frame with results
#' @seealso `\link[mongolite]{mongo}` `\link{mdbAggregate}`
#' \url{https://www.mongodb.com/docs/manual/core/map-reduce/}
#'
#' @export mdbMapreduce
#' @exportMethod mdbMapreduce
#'
#'
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' histdata <- mdbMapreduce(irisdb,
#'   map= "function (){emit(Math.floor(this.Petal_Length*5)/5, 1)}",
#'   reduce="function (id,counts){return Array.sum(counts)}"
#' )
#' if (any(!is.na(histdata))) {
#' names(histdata) <- c("Petal.length","count")
#' }
#' head(histdata)
#' histdata1 <- mdbAggregate(irisdb,
#'  '[{"$set": {
#'     "ptlround": {
#'       "$divide":[
#'            {"$floor": {
#'             "$multiply": ["$Petal_Length", 5]
#'             }}, 5]}}},
#'    {"$group": {
#'      "_id": "$ptlround",
#'      "count": {"$sum":1}
#'    }}
#' ]'
#' )
#' if (!is.null(histdata1)) {
#' names(histdata1) <- c("Petal.length","count")
#' }
#' head(histdata1)
setGeneric("mdbMapreduce", function (db, map, reduce, query = '{}',
                                           sort = '{}', limit = 0)
   standardGeneric("mdbMapreduce"))
#' @rdname mdbMapreduce
setMethod("mdbMapreduce","MongoDB",
          function (db, map, reduce, query = '{}',
                    sort='{}', limit = 0) {
            if (!db$available()) return(NA)
            db$db()$mapreduce(map,reduce,query,sort,limit)
})
#' @rdname mdbMapreduce
setMethod("mdbMapreduce","mongo",
          function (db, map, reduce, query = '{}',
                    sort='{}', limit = 0) {
            db$mapreduce(map,reduce,query,sort,limit)
})


mdb.remove <-
  function(){
}
#' Remove selected objects from collection
#'
#' Query selects a subset of the collection to remove.  Note for
#' removing everything, `\link{mdbDrop}` is faster.
#'
#' @param db (or MongoDB mongo) -- Collection affected.
#' @param query character -- Mongo query expressed as JSON object.
#' See `\link{buildJQuery}` and `\link{mdbFind}`.
#' @param just_one logical -- If true, only the first matching record
#' is removed.
#'
#'
#' @return miniprint Information about the results.
#' @seealso `\link[mongolite]{mongo}`, `\link{mdbFind}`,
#' `\link{mdbDrop}`
#' \url{https://www.mongodb.com/docs/manual/reference/command/delete/}
#' @export mdbRemove
#' @exportMethod mdbRemove
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbDrop(irisdb)
#' mdbInsert(irisdb,iris)
#' mdbCount(irisdb)
#' mdbRemove(irisdb,'{"Species":"setosa"}')
#' mdbCount(irisdb)
setGeneric("mdbRemove", function (db, query = '{}', just_one = FALSE)
  standardGeneric("mdbRemove"))
#' @rdname mdbRemove
setMethod("mdbRemove","MongoDB",
  function (db, query = '{}', just_one = FALSE) {
    if (!db$available()) return(NULL)
    db$db()$remove(query,just_one)
})
#' @rdname mdbRemove
setMethod("mdbRemove","mongo",
  function (db, query = '{}', just_one = FALSE) {
    db$remove(query,just_one)
})

mdb.rename <-
  function(){
}
#' Renames collection or moves it to a new database
#'
#' Using the `name` argument simply renames the collection.  Using the
#' `db` argument copies the collection to a new database.
#'
#' @param mdb (or MongoDB mongo) -- Reference to collection to move
#' @param name character -- new name for collection
#' @param db character -- new database for collection.
#'
#'
#' @return miniprint Status Message
#' @seealso `\link[mongolite]{mongo}`
#' \url{https://www.mongodb.com/docs/manual/reference/command/renameCollection/}
#' @export mdbRename
#' @exportMethod mdbRename
#'
#' @examples
#' mdbDrop(MongoDB("FisherIrises",noMongo=!interactive()))
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' showCollections(irisdb)
#' mdbRename(irisdb,"FisherIrises")
#' showCollections(irisdb)
setGeneric("mdbRename", function (mdb, name, db=NULL)
  standardGeneric("mdbRename"))
#' @rdname mdbRename
setMethod("mdbRename","MongoDB",
  function (mdb, name, db=NULL) {
    if (!mdb$available()) return(NULL)
    mdb$db()$rename(name,db)
})
#' @rdname mdbRename
setMethod("mdbRename","mongo",
  function (mdb, name, db=NULL) {
    mdb$rename(name,db)
})


mdb.replace <-
  function(){
}
#' Replace a document with a new document
#' @aliases mdbUpsert
#'
#' @param db (or MongoDB mongo) Reference to the collection.
#' @param query character Query as JSON document, see `\link{mdbFind}`
#' and `\link{buildJQuery}`.
#' @param update character Replacement document in JSON format.
#' @param upsert logical If `TRUE` document will be inserted if the
#' query does not return a result.
#'
#'
#' @return miniprint with results.
#' @seealso `\link[mongolite]{mongo}`, `\link{mdbFind}`, `\link{mdbUpdate}`
#' \url{https://www.mongodb.com/docs/manual/reference/method/db.collection.replaceOne/}
#' @export mdbReplace
#'
#' @details
#' In this method, the entire selected document (including the `_id`
#' field is replaced.  In the `\link{update}` method, the existing
#' record is modified.
#'
#' The `query` argument should return 0 or 1 arguments.  If it
#' returns 0 and `upsert` is `TRUE`, then the document is inserted.
#' The function `mbdUpsert(...)` is an alias for `mdbReplace(...,
#' upsert=TRUE)`.
#'
#' @examples
#' testdb <- MongoDB(noMongo=!interactive())
#' mdbDrop(testdb)
#' mdbInsert(testdb,'{"name":"Fred", "gender":"M"}')
#' mdbFind(testdb,fields='{}')
#' mdbReplace(testdb,'{"name":"Fred"}', '{"name":"Phred",
#' "gender":"F"}')
#' mdbFind(testdb,fields='{}')
setGeneric("mdbReplace", function (db, query, update='{}', upsert=FALSE)
  standardGeneric("mdbReplace"))
#' @rdname mdbReplace
#' @exportMethod mdbReplace
setMethod("mdbReplace","MongoDB",
  function (db, query, update='{}',upsert=FALSE) {
    if (!db$available()) return(NULL)
    db$db()$replace(query,update,upsert)
})
#' @rdname mdbReplace
setMethod("mdbReplace","mongo",
  function (db, query, update='{}', upsert=FALSE) {
    db$replace(query,update,upsert)
})
#' @rdname mdbReplace
#' @export mdbUpsert
setGeneric("mdbUpsert", function (db, query, update='{}', upsert=TRUE)
  standardGeneric("mdbUpsert"))
#' @rdname mdbReplace
#' @exportMethod mdbUpsert
setMethod("mdbUpsert","JSONDB",
  function (db, query, update='{}', upsert = TRUE) {
  mdbReplace(db, query, update, upsert)
  })


mdb.run <-
  function(){
}
#' Runs a Mongo command on the collection
#'
#' @param db (or MongoDB mongo) Reference to the collection.
#' @param command character JSON document providing the command.
#' @param simplify logical If true, the output structure is simplified.
#'
#' @return list containing returned value.
#' @seealso `\link[mongolite]{mongo}`
#' \url{https://www.mongodb.com/docs/manual/reference/command/}
#' @export mdbRun
#' @exportMethod mdbRun
#'
#' @details
#' A command is a JSON document.  See the Mongo reference manual for
#' the supported commands (these will vary quite a lot by the version
#' of the database).  Note that some commands only run against the
#' "admin" database.
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' mdbRun(irisdb,'{"collStats":"iris"}')
setGeneric("mdbRun", function (db, command = '{"ping":1}', simplify = TRUE)
  standardGeneric("mdbRun"))
#' @rdname mdbRun
setMethod("mdbRun","MongoDB",
  function (db, command = '{"ping":1}', simplify = TRUE) {
    if (!db$available()) return(NA)
    db$db()$run(command,simplify)
})
#' @rdname mdbRun
setMethod("mdbRun","mongo",
  function (db, command = '{"ping":1}', simplify = TRUE) {
    db$run(command,simplify)
})


mdb.update <-
  function(){
}
#' Modify document(s) in a collection
#'
#' The `query` field identifies a number of documents.  The `update`
#' is a set of instructions for changing the documents.  The
#' `\link{mdbReplace}` function replaces the document instead of
#' modifying it.
#'
#' @param db (or MongoDB mongo) -- The collection to modify.
#' @param query character -- A JSON document identifying the document(s)
#' to modify.
#' @param update character -- A JSON document identifying the changes
#' to make to a document.
#' @param filters character -- A JSON document which controls which documents in the collection
#' gets updated.  (See the "Mongolite User Manual").
#' @param upsert logical -- If true and the query returns no results,
#' then insert the document instead.
#' @param multiple logical -- If true, all documents matching `query`
#' are affected; if false, only the first one.
#'
#'
#' @details
#'
#' ### Queries
#'
#' The rules here match those for `\link{mdbFind}`.
#'
#' ### Update Documents
#'
#' This is a special document which describes how to modify the
#' document.  There are a number of commands described in the Mongo
#' documentation, of which the two most useful are `$set` and
#' `$unset`.
#'
#' The `$set` command takes as argument a JSON object giving field
#' value pairs.  If the field exists it value will be changed, if it
#' doesn't exist a new field will be created.  For example `'{"$set":
#' {"processed":false}}'` will set the `processed` field to false,
#' additing it if necessary.
#'
#' The `$unset` command removes a field from a document.  For example,
#' `'{"$unset":{"processed":0}}'` removes the process field.  The
#' value after the field name is ignored.
#'
#' For multiple complex changes, consider using
#' `\link{mdbAggregate} for more complex changes.
#'
#' @return A list with the returned object.
#' @seealso `\link[mongolite]{mongo}`, `\link{mdbReplace}`
#' `\link{mdbFind}`, `\link{mdbAggregate}`
#' @export mdbUpdate
#' @exportMethod mdbUpdate
#'
#' @examples
#'  mdb <- MongoDB("testthis","test","mongodb://localhost",noMongo=!interactive())
#'  mdbDrop(mdb)
#'  mdbInsert(mdb,c('{"name":"Fred", "gender":"M"}',
#'                  '{"name":"George", "gender":"M"}'))
#' mdbFind(mdb)
#' mdbUpdate(mdb,'{"name":"Fred"}', '{"$set":{"gender":"F"}}')
#' mdbFind(mdb)
setGeneric("mdbUpdate", function (db, query, update='{"$set":{}}',
                                  filters=NULL,
                                  upsert = FALSE, multiple = FALSE)
  standardGeneric("mdbUpdate"))
#' @rdname mdbUpdate
setMethod("mdbUpdate", "MongoDB",
          function (db, query, update='{"$set":{}}',
                    filters=NULL, upsert = FALSE,
                    multiple = FALSE) {
            if (!db$available()) return(NULL)
            db$db()$update(query,update,filters,upsert,multiple)
})
#' @rdname mdbUpdate
setMethod("mdbUpdate", "mongo",
          function (db, query, update='{"$set":{}}',
                    filters=NULL, upsert = FALSE,
                    multiple = FALSE) {
            db$update(query,update,filters,upsert,multiple)
})


show.databases <-
  function(){
}
#' Lists Databases
#'
#' @param db (or MongoDB NULL) -- Database reference. (Note:
#' `mongolite::mongo` cannot be used.)
#' @param uri character -- URI for database connections.
#' @param options list -- SSL options for an SSL connection
#'
#' @details
#' This function lists the names of the databases which are accessible
#' for the current user.
#'
#' @details
#'
#' This function needs to make a new connection to the `admin`
#' database.  If a MongoDB object is supplied, then the `uri` and
#' `options` are taken from it.  If the `db` argument is NULL, a new
#' connection is made.
#'
#' Note that there is currently no documented way of retrieving the
#' `url` and `ssl_options` from a `mongolite::mongo` object.
#'
#' @return a data frame containing the names and sizes of the databases
#' @seealso `\link[mongolite]{mongo}`, `\link{mdbRun}`
#' @export showDatabases
#' @exportMethod showDatabases
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' showDatabases(irisdb)
setGeneric("showDatabases",
           function(db=NULL, uri="mongodb://localhost",
                    options=mongolite::ssl_options())
  standardGeneric("showDatabases"))
#' @rdname showDatabases
setMethod("showDatabases","MongoDB",
          function(db=NULL, uri="mongodb://localhost",
                   options=mongolite::ssl_options()) {
  ## Need a new connection to the admin database.
            if (!db$available()) return (data.frame())
            if (missing(uri)) uri <- db$uri
            if (missing(options)) options <- db$options
            adb <- mongo(db="admin",url=uri,options=options)
            adb$run('{"listDatabases":1}')$databases
})
#' @rdname showDatabases
setMethod("showDatabases","NULL",
          function(db=NULL, uri="mongodb://localhost",
                   options=mongolite::ssl_options()) {
            ## Need a new connection to the admin database.
            adb <- mongo(db="admin",url=uri, options=options)
            adb$run('{"listDatabases":1}')$databases
})

show.collections <-
  function(){
}#' Shows collections in the current database.
#'
#' @param db (or MongoDB mongo NULL) -- Connection to target database.
#' If `NULL` a new connection is made using the specified information.
#' @param dbname character -- name for new collection
#' @param uri character -- URI for database connections.
#' @param options list -- SSL options for an SSL connection
#'
#' @details
#' Shows all of the collections which are in the referenced database.
#' If the `db` argument is a `MongoDB` or `mongolite::mongo` object,
#' the current database is used.  If `db` is `NULL`, then a new
#' connection is created with the information.
#'
#' @return character vector of database names
#' @seealso [\link[mongolite]{mongo}]
#' @export showCollections
#' @exportMethod showCollections
#'
#'
#' @examples
#' irisdb <- MongoDB("iris",noMongo=!interactive())
#' showCollections(irisdb)
setGeneric("showCollections",
           function(db=NULL, dbname="test", uri="mongodb://localhost",
                    options=mongolite::ssl_options())
             standardGeneric("showCollections"))
#' @rdname showCollections
setMethod("showCollections","mongo",
          function(db=NULL, dbname="test", uri="mongodb://localhost",
                   options=mongolite::ssl_options())
          {
            res <- db$run('{"listCollections":1}')$cursor$firstBatch
            data.frame(name=res$name,type=res$type,readOnly=res$info$readOnly)
})
#' @rdname showCollections
setMethod("showCollections","MongoDB",
          function(db=NULL, dbname="test", uri="mongodb://localhost",
                   options=mongolite::ssl_options())
          {
            if (!db$available()) return (data.frame())
            col <- db$db()
            if (!missing(dbname) && dbname != db$dbname) {
              if (missing(uri)) uri <- db$uri
              if (missing(options)) options <- db$options
              col <-
                mongolite::mongo(db=dbname,url=uri,ssl_options=options)
            }
            showCollections(col)
})
#' @rdname showCollections
setMethod("showCollections","NULL",
          function(db=NULL, dbname="test", uri="mongodb://localhost",
                   options=mongolite::ssl_options())
            showCollections(
                mongolite::mongo(db=dbname,url=uri,ssl_options=options)))












