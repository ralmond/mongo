
#' Load example Event class
#'
#' This function loads the example "Event" class; needed for examples.
#'
#' @return invisible details about package
#' @export load_example
#'
#' @examples
#' load_example()
#' fred1  ## sample data item.
#' ## The source file
#' system.file("examples","Event.R",package="mongo")
load_example <- function() {
  source(system.file("examples","Event.R",package="mongo"))
}


mongoQueries <- c("eq","gt","gte","lt","lte","ne","nin","in","","oid")


#' Build a single query function.
#'
#' @param name character name of the referenced field
#' @param value vector named collection of possible values.
#'
#' @return character scalar giving JSON expression
#' @export buildJQterm
#'
#' @details
#'
#' This is mostly an internal function, but may be of some use.
#'
#' @examples
#' buildJQterm("uid","Fred")
#' buildJQterm("uid",c("Phred","Fred"))
#' buildJQterm("time",Sys.time())
#' buildJQterm("num",1:4)
#' buildJQterm("num",c(gt=7))
#' buildJQterm("num",c(lt=7))
#' buildJQterm("num",c(gte=7))
#' buildJQterm("num",c(lte=7))
#' buildJQterm("num",c(ne=7))
#' buildJQterm("num",c(eq=7))
#' buildJQterm("num",c(gt=2,lt=7))
#' buildJQterm("count",c(nin=1,2:4))
#' buildJQterm("count",c("in"=1,2:4))
#' buildJQterm("count",c(ne=1,ne=5))
buildJQterm <- function (name,value) {
  if (length(value)==0L)
    stop("Query term ",name,"has no value.")
  compOps <- names(value)
  names(value) <- NULL
  if (is.null(compOps)) {
    if (length(value) == 1L) {
      ## Singleton query.
      vstring <- jsonlite::toJSON(unboxer(value),POSIXt="mongo")
    } else {
      ## Unmarked $in query
      vstring <- paste('{"$in":',jsonlite::toJSON(unlist(value),
                                        POSIXt="mongo"),'}',sep="")
    }
  } else {
    compOps <- sub('^\\$','',compOps)   #Strip leading $
    if(!all(compOps %in% mongoQueries)) {
      stop("Unspported operator ",compOps[!(compOps %in% mongoQueries)],
           " in query for field ",name)
    }
    if(compOps[1]=="nin" || compOps[1]=="in" || compOps[1]=="") {
      ## Special Handling for (n)in query)
      op <- ifelse(compOps[1]=="","in",compOps[1])
      vstring <- paste('{"$',op,'":',jsonlite::toJSON(unlist(value),
                                            POSIXt="mongo"),'}',sep="")
    } else {
      ## iterate over values.
      vstring <- sapply(1:length(compOps),
                        function (i)
                          paste('"$',compOps[i],'":',
                                jsonlite::toJSON(unboxer(value[i]),POSIXt="mongo"),
                                sep=""))
      vstring <- paste('{',paste(vstring,collapse=", "),'}')
    }
  }
  paste('"',name,'":',vstring,sep="")
}



#' Transforms a query into JQuery JSON.
#'
#'
#' This function takes a query which is expressed in the argument list and
#' transforms it into a JSON query document which can be used with the Mongo
#' Database.  The function \code{buildJQterm} is a helper function which builds
#' up a single term of the query.
#'
#'
#' A typical query to a Mongo database collection is done with a JSON object
#' which has a number of bits that look like
#' \dQuote{\emph{field}:\emph{value}}, where \emph{field} names a field in the
#' document, and \emph{value} is a value to be matched.  A record matches the
#' query if all of the fields specified in the query match the corresponding
#' fields in the record.
#'
#' Note that \emph{value} could be a special expression which gives specifies a
#' more complex expression allowing for ranges of values.  In particular, the
#' Mongo query language supports the following operators: \code{"$eq", "$ne",
#' "$gt", "$lt", "$gte", "$lte"}.  These can be specified using a value of the
#' form \code{c(<op>=<value>)}, where \emph{op} is one of the mongo operators,
#' without the leading \sQuote{$}.  Multiple op--value pairs can be specified;
#' for example, \code{count=c(gt=3,lt=6)}.  If no op is specified, then
#' \code{"$eq"} is assumed.  Additionally, the \code{"$oid"} operator can be
#' used to specify that a value should be treated as a Mongo record identifier.
#'
#' The \code{"$in"} and \code{"$nin"} are also ops, but the corrsponding value
#' is a vector.  They test if the record is in or not in the specified value.
#' If the value is vector valued, and no operator is specified it defaults to
#' \code{"$in"}.
#'
#' The function \code{buildJQuery} processes each of its arguments, adding them
#' onto the query document.  The \code{rawfields} argument adds the fields onto
#' the document without further processing.  It is useful for control arugments
#' like \code{"$limit"} and \code{"$sort"}.
#'
#' @aliases buildJQuery
#' @param \dots This should be a named list of arguments.  The values should be
#' the desired query value, or a more complex expression (see details).
#' @param rawfields These arguments are passed as character vectors directly
#' into the query document without processing.
#' @return The function \code{buildJQuery} returns a unicode string which
#' contains the JSON query document.
#' @author Russell Almond
#' @seealso \code{\link{as.json}}, \code{\link{mdbFind}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB 4.0 Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#'
#' buildJQuery(app="default",uid="Phred")
#' buildJQuery("_id"=c(oid="123456789"))
#' buildJQuery(name="George",count=c(gt=3,lt=5))
#' buildJQuery(name="George",count=c(gt=3,lt=5),
#'             rawfields=c('"$limit":1','"$sort":{timestamp:-1}'))
#'
#' ## Queries on IDs need special handling
#' buildJQuery("_id"=c(oid="123456789abcdef"))
#'
#' @export buildJQuery
buildJQuery <- function (...,rawfields=character()) {
  terms <- list(...)
  fields <- names(terms)
  jstrings <- sapply(fields,function(f) buildJQterm(f,terms[[f]]))
  jstrings <- c(jstrings,rawfields)
  query <- paste('{',paste(jstrings,collapse=", "),'}')
  query
}


#' Fetches Messages from a Mongo databas
#'
#'
#' This function fetches \code{\linkS4class{MongoRec}} objects from a
#' \code{\link[mongolite]{mongo}} database.  The message parser is passed as an
#' argument, allowing it to fetch other kinds of objects than P4Messages.  The
#' function \code{getManyRecs} retrieves all matching objects and the function
#' \code{getOneRec} retrieves the first matching object.
#'
#'
#' This function assumes that a number of objects (usually, but not necessarily
#' subclasses of \code{\link{MongoRec}} objects) have been stored in a Mongo
#' database.  The \code{col} argument is the \code{\link{MongoDB}}
#' object in which they are stored.  These functions retrieve the selected
#' objects.
#'
#' The first argument should be a string containing a JSON query document.
#' Normally, thes are constructed through a call to \code{\link{buildJQuery}}.
#'
#' The query is used to create an iterator over JSON documents stored in the
#' database.  At each round, the iterator extracts the JSON document as a
#' (nested) list structure.  This is passed to the \code{builder} function to
#' build an object of the specified type.  See the \code{\link{buildObject}}
#' function for an example builder.
#'
#' The sorting argument controls the way the returned list of objects is
#' sorted. This should be a numeric vector with names giving the field for
#' sorting.  The default values \code{c("timestamp"=1)} and
#' \code{c("timestamp"=-1)} sort the records in ascending and descending order
#' respectively.  In particular, the default value for \code{getOneRec} means
#' that the most recent value will be returned.  The defaults assume that
#' \dQuote{timestamp} is a field of the stored object.  To suppress sorting of
#' outputs, use \code{NULL} as the argument to \code{sort}.
#'
#' @aliases getOneRec getManyRecs
#' @export getOneRec
#' @export getManyRecs
#' @param col (or MongoDB mongo) A reference to a Mongo collection.
#' @param jquery A string providing a Mongo JQuery to select the appropriate
#' records.  See \code{\link{buildJQuery}}.
#' @param builder A function which will take the list of fields returned from
#' the database and build an appropriate R object.  See
#' \code{\link{buildObject}}.
#' @param sort A named numeric vector giving sorting instructions.  The names
#' should correspond to fields of the objects, and the values should be positive
#' or negative one for increasing or decreasing order. Use the value
#' \code{NULL} to leave the results unsorted.
#' @param skip integer  This many records should be skipped before returning records
#' @param limit A numeric scalar giving the maximum number of objects to
#' retrieve.  If `Inf`, then all objects matching the query will be retrieved.
#' @return
#'
#' The function \code{getOneRec} returns an object whose type is determined by
#' the output of the \code{builder} function.  The default `\link{buildObject}` method uses
#' the `class` field of the record is used to select the object type.  (It assumes a `\link{parse.jlist}`
#' method is available for that object type.)
#'
#' The function \code{getManyRecs} returns a list of object whose type is
#' determined by the output of the \code{builder} function.
#' @author Russell Almond
#' @seealso \code{\link{saveRec}}, \code{\link{buildObject}},
#' \code{\link{getOneRec}}, \code{\link{getManyRecs}}
#' \code{\link[mongolite]{mongo}}
#' @references The MongoDB Manual: \url{https://docs.mongodb.com/manual/}
#' @keywords interface database
#' @examples
#'
#'\dontrun{
#' ## Requires Mongo test database to be set up.
#' load_Events()
#'
#' m1 <- new("Event", uid="James Goodfellow",mess="Task Done",processed=FALSE,
#'            timestamp=Sys.time(),
#'            data=list("Selection"="B"))
#' m2 <- new("Event", uid="James Goodfellow", mess="New Obs", processed=FALSE,
#'            timestamp=Sys.time(),
#'            data=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- new("Event", uid="Fred",mess="New Stats",
#'            timestamp=Sys.time(),
#'            data=list("score"=1,"theta"=0.12345,"noitems"=1))
#'
#' EventDB <- MongoDB(Event,noMongo=!interactive())
#'
#' Assign these back to themselves to capture the mongo ID
#' m1 <- saveRec(EventDB,m1)
#' m2 <- saveRec(EventDB,m2)
#' m3 <- saveRec(EventDB,m3)
#'
#' m1@data$time <- list(tim=25.4,units="secs")
#' m1 <- saveRec(EventDB,m1)
#'
#' ## Note use of oid keyword to fetch object by Mongo ID.
#' m1a <- getOneRec(EventDB,buildJQuery("_id"=c(oid=m1@"_id")))
#' m123 <- getManyRecs(EventDB,buildJQuery(uid="Fred"))
#' m23 <- getManyRecs(EventDB,buildJQuery(uid="Fred",sender=c("EI","EA")))
#' m321 <- getManyRecs(EventDB,buildJQuery(uid="Fred",timestamp=c(lte=Sys.time())),
#'             sort=c(timestamp=-1))
#' getManyRecs(EventDB,buildJQuery(uid="Fred",
#'                           timestamp=c(gte=Sys.time()-as.difftime(1,units="hours"))))
#'
#'
#' }
getOneRec <- function(col,jquery='{}',builder=buildObject,
                      sort=buildJQuery(timestamp=-1)) {
  it <- mdbIterate(col,jquery,'{}',sort=sort,limit=1)
  rec <- it$one()
  if (is.null(rec)) return(rec)
  do.call(builder,list(rec))
}

#' @rdname getOneRec
getManyRecs <- function(col,jquery,builder=buildObject,
                        sort=buildJQuery(timestamp=-1),
                        skip=0, limit = 0) {
  n <- mdbCount(col,jquery)
  if (limit>0) n <- min(max(n-skip,0),limit)
  result <- vector("list",n-skip)
  it <- mdbIterate(col,jquery,'{}',sort=sort,skip=skip,limit=n)
  nn <- 1
  while (!is.null(rec <- it$one())) {
    result[[nn]] <- do.call(builder,list(rec))
    nn <- nn +1
  }
  result
}




#' Saves a MongoRec object to a Mongo database
#'
#'
#' This function saves an S4 object as a record in a Mongo database.  It uses
#' \code{\link{as.json}} to covert the object to a JSON string.
#'
#' @param col (or MongoDB mongo NULL) A mongo collection reference.  If `NULL` record will not be saved.
#' @param rec The message (object) to be saved.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @return
#'
#' Returns the message argument, which may be modified by setting the
#' \code{"_id"} field if this is the first time saving the object.
#' @author Russell Almond
#' @seealso
#'
#' \code{\link{as.json}}, \code{\linkS4class{MongoRec}},
#' \code{\link{buildObject}}, \code{\link{getOneRec}},
#' \code{\link{MongoDB}}
#' @keywords database
#' @examples
#'
#' \dontrun{
#' load_Events() # Uses the sample Event class.
#' m1 <- new("Event",uid="Fred",mess="Task Done",
#'                 timestamp=Sys.time(),
#'                 data=list("Selection"="B"))
#' m2 <- new("Event",uid="Fred",mess="New Obs",timestamp=Sys.time(),
#'                 data=list("isCorrect"=TRUE,"Selection"="B"))
#' m3 <- new("Event",uid="Fred","New Stats",
#'                 details=list("score"=1,"theta"=0.12345,"noitems"=1))
#'
#' testcol <- MongoDB("Messages",noMongo=!interactive())
#' ## Save them back to capture the ID.
#' m1 <- saveRec(testcol,m1)
#' m2 <- saveRec(testcol,m2)
#' m3 <- saveRec(testcol,m3)
#' }
#'
#' @export saveRec
saveRec <- function (col, rec, serialize=TRUE) {
  if (!is.null(col)) {
    jso <- as.json(rec,serialize)
    if (is.na(m_id(rec))) {
      ## Insert
      mdbInsert(col,jso)
      it <- mdbIterate(col,jso,'{"_id":true}',limit=1)
      m_id(rec) <- it$one()$"_id"
    } else {
      if (mdbCount(col,paste('{"_id":{"$oid":"',m_id(rec),'"}}',sep=""))) {
        ## Replace
        mdbUpdate(col,paste('{"_id":{"$oid":"',m_id(rec),'"}}',sep=""),
                   paste('{"$set":',jso,'}',sep=""))
      } else {
        ## ID is out of date, insert and get new ID.
        mdbInsert(col,jso)
        it <- mdbIterate(col,jso,'{"_id":true}',limit=1)
        m_id(rec) <- it$one()$"_id"
      }
    }
  } else {
    ## flog.trace("DB is null, not saving rec.")
  }
  rec
}


###
## This is a construction I find myself using in a lot of places to
## build up the "mongodb://" URI for the database.



#' Creates the URI needed to connect to a mongo database.
#'
#'
#' This function formats the universal record indicator (URI) for connecting to
#' a Mongo database.  It is mostly a utility function for formatting the
#' string.
#'
#'
#' @param username The name of the database user (login credential), or an
#' empty string if no username is required.
#' @param password The name of the database password (login credential), or an
#' empty string if no password is required.
#' @param host The name or IP address of the system hosting the database.
#' @param port The port to be used for connections.  Note that the port for a
#' default configuration of mongo is 27018.  This can be left blank to use the
#' default port.
#' @param protocol A character scalar giving the protocol to use when
#' connecting, e.g., \dQuote{mongodb}.
#' @return
#'
#' A character string giving the database URI which can be passed to the
#' \code{\link[mongolite]{mongo}} function to create a database collection
#' handle.
#'
#' Note that the password is stored in clear text, so appropriate care should
#' be taken with the result of this function.
#' @author Russell Almond
#' @seealso
#'
#' \code{\link{MongoDB}}, \code{\link[mongolite]{mongo}}
#'
#' This is an input argument to a number of other classes which use mongo
#' connections.
#' @keywords interface database
#' @examples
#'
#' makeDBuri()
#' makeDBuri(user="admin",password="secret")
#' makeDBuri(user="admin")
#' makeDBuri(host="example.com",port=12345)
#'
#' @export makeDBuri
makeDBuri <- function(username="",password="", host="localhost",
                      port="",protocol="mongodb") {
  ## Setup DB URI
  security <- ""
  if (nchar(username) > 0L) {
    if (!is.null(password) && nchar(password) > 0L)
      security <- paste(username,password,sep=":")
    else
      security <- username
  }
  if (nchar(port) > 0L)
    host <- paste(host,port,sep=":")
  else
    host <- host
  if (nchar(security) > 0L)
    host <- paste(security,host,sep="@")
  paste(paste(protocol,":/",sep=""),host,sep="/")
}
