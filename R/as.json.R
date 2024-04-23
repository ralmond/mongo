## as.json.R  These functions extend the capabilities of
## jsonlite to handle S4 objects.  See Vingette "JSON for S4 Objects"

## @importFrom jsonlite toJSON fromJSON unbox
## @importFrom jsonlite serializeJSON unserializeJSON
NULL


#' Marks scalar objects to be preserved when converting to JSON
#'
#'
#' The function \code{\link[jsonlite]{toJSON}} coverts vectors (which all R
#' objects are) to vectors in the JSON code.  The function
#' \code{jsonlite::\link[jsonlite]{unbox}} protects the object from this
#' behavior, which makes the fields eaiser to search and protects against loss
#' of name attributes.  The function \code{unboxer} extents \code{unbox} to
#' recursively unbox lists (which preserves names).  The function
#' \code{ununbox} removes the unboxing flag and is mainly used for testing
#' parser code.
#'
#'
#' The \code{jsonlite::\link[jsonlite]{unbox}} function does not necessarily
#' preserve the name attributes of elements of the list.  In other words the
#' sequence \code{\link{as.jlist}} -> \code{\link[jsonlite]{toJSON}} ->
#' \code{\link[jsonlite]{fromJSON}} -> \code{\link{buildObject}} might not be
#' the identity.
#'
#' The solution is to recursively apply \code{\link[jsonlite]{unbox}} to the
#' elements of the list.  The function \code{unboxer} can be thought of as a
#' recursive version of \code{unbox} which handles the entire tree struction.
#' If \code{x} is not a list, then \code{unboxer} and \code{unbox} are
#' equivalent.
#'
#' The typical use of this function is defining methods for the
#' \code{\link{as.jlist}} function.  This gives the implementer fine control of
#' which attributes of a class should be scalars and vectors.
#'
#' The function \code{ununboxer} clears the unboxing flag.  Its main purpose is
#' to be able to test various parsers.
#'
#' @aliases unboxer ununboxer
#' @param x Object to be boxed/unboxed.
#' @return The function \code{unboxer} returns the object with the added class
#' \code{scalar}, which is the \code{jsonlite} marker for a scalar.
#'
#' The function \code{ununboxer} returns the object without the \code{scalar}
#' class marker.
#' @note
#'
#' There is a bug in the way that \code{\link[base]{POSIXt}} classes are
#' handled, \code{unboxer} fixes that problem.
#' @section Warning: Dependence on jsonlite implementation:
#'
#' These functions currently rely on some internal mechanisms of the jsonline
#' pacakge.  In particular, \code{ununbox} relies on the
#' \dQuote{scalar} class mechanism.
#' @author Russell Almond
#' @seealso \code{\link[jsonlite]{unbox}}, \code{\link[jsonlite]{toJSON}},
#' \code{\link{as.jlist}}, \code{\link{buildObject}}
#' @keywords interface
#' @examples
#' \dontrun{
#' load_examples() ## Example uses event class.
#'
#' ## as.jlist method shows typical use of unboxer.
#' getMethod("as.jlist",c("Event","list"))
#'
#' ## Use ununboxer to test as.jlist/buildObject pair.
#' m4 <- Event("Phred","New Stats",
#'                data=list("agents"=c("ramp","ramp","lever")))
#' m4jl <- as.jlist(m4,attributes(m4))
#' m4a <- buildObject(ununboxer(m4jl))
#' testthat::expect_equal(m4a,m4)
#' }
#'
#'
#'
#' @export unboxer
#' @export ununboxer
unboxer <- function (x) {
  if (is(x,"list")) {
    lapply(x,unboxer) #Saves name data.
  } else {
    if (length(x) == 1L) {
      jsonlite::unbox(x)
    } else {
      x
    }
  }
}

## Need this for testing.
#' @describeIn unboxer  Undoes the effect of unboxer (in particular,
#' removes the scalar mark).
ununboxer <- function (x) {
  if (is(x,"scalar")) {
    class(x) <- setdiff(class(x),"scalar")
  } else {
    if (is.list(x) && !is(x,"POSIXlt")) {
      x <- lapply(x, function(s) {
        sapply(s,ununboxer)
      })
    }
  }
  x
}


#' List representation of a document.
#'
#' A `namedList` which corresponds to a Mongo document; this is not an
#' official type, but rather a particular use of use of the primitive
#' `namedList` type.  The field names are given by the names and the
#' values are the list values.  If any of the elements is a list, then
#' it is a sub-document.  The `jsonlite` package provides a `toJSON`
#' and `fromJSON` method for converting between jlists and JSON
#' character objects.
#'
#' @name jlist
#' @details
#'
#' Note that R makes no distinction between scalars and vectors of
#' length 1; however, JSON does.  For example, `'{"scalar":0,
#' "vector":[0]}'`.  The `jsonlite` package provides a tool
#' `\link[jsonlite]{unbox}` which marks the element as a scalar.  The
#' function `\link{unboxer}` will do this recursively over a jlist
#' object.
#'
#' The distinction between vectors and scalars is unimportant if the
#' goal is simply to save and restore the object, but if the goal is
#' to build an index over the field (`\link{mdbIndex}`), then scalars
#' are easier to work with than vectors.  To covert an S4 object to a
#' class, the solution is to write a method for the `\link{as.jlist}`
#' method which makes appropriate transformations of the fields (and a
#' `\link{parse.jlist}` method to reverse the process).
#'
#'
#' @seealso `\link{as.jlist}`, `\link{buildObject}`, `\link[jsonlite]{toJSON}`,
#' `\link[jsonlite]{fromJSON}`,  `\link{mdbIterate}`,
#' `\link[jsonlite]{unbox}`, `\link{unboxer}`
list()


MongoRec.class <- function() {}
#' Class "MongoRec".
#' @name MongoRec-class
#' @description
#' This is a lightweight class meant to be extended.
#' It contains a
#' single field for a Mongo identifier, which can be accessed using
#' the `m_id()` method.  It is meant to store something that is a
#' record in a Mongo collection, where `_id` is the Mongo identifier.
#'
#' @docType class
#' @section Objects from the Class:
#'
#' Objects can be created by calls to the `MongoRec()` function.
#' @slot _id (character) The Mongo ID, `NA_character_` if not saved.
#' @author Russell G. Almond
#'
#' @returns
#' The constructor `MongoRec` returns an object of class `MongoRec`.
#'
#' The `m_id` method returns a character scalar (with name `oid`) which contains the mongo identifer.
#'
#' @seealso
#' [as.json()]
#' [buildObject()], [saveRec()], [getOneRec()]
#' @keywords classes
#' @examples
#'
#' showClass("MongoRec")
#'
#' @exportClass MongoRec
setClass("MongoRec",
         slots=c("_id"="character"
                 ))

m.id <- function() {}
#' Accessor for the Mongo id element of a record.
#'
#' Objects of class \code{\linkS4class{MongoRec}} have a `_id` slot
#' which stores the database ID.  This function accesses it.
#'
#' The `_id` slot should be a character object with the name
#' \dQuote{oid}.  The methods enforce this.  If the object does not
#' have a Mongo ID (i.e., it was never stored in a database), then the
#' value of `_id` should be `NA_character_`.
#'
#' @param x An object of type [MongoRec].
#'
#' @export m_id
#' @exportMethod m_id
#' @docType methods
#' @rdname m_id
#'
#' @examples
#' mr <- MongoRec()
#' m_id(mr) # NA
#' m_id(mr) <- "012345"
#' m_id(mr)
#'
setGeneric("m_id",function(x) standardGeneric("m_id"))

#' @describeIn m_id Setter for Mongo ID
#' @param value (character) the new ID value, use `NA_character_` for missing.
#' @export "m_id<-"
#' @exportMethod "m_id<-"
#'
setGeneric("m_id<-",function(x, value) standardGeneric("m_id<-"))

#' @rdname m_id
#' @aliases m_id,MongoRec-method
setMethod("m_id","MongoRec", function(x) x@"_id")

#' @rdname m_id
#' @aliases m_id<-,MongoRec-method
setMethod("m_id<-","MongoRec", function(x,value) {
  names(value) <- "oid"
  x@"_id" <- value
  x})

#' @describeIn MongoRec-class Constructor for `MongoRec`
#' @aliases MongoRec
#' @export MongoRec
#' @param m_id character Mongo identifier.  Use `NA_character_` for unsaved objects.
#' @param ... Other arguments (pass through for initialization method).
MongoRec <- function(...,m_id=NA_character_) {
  new("MongoRec", "_id"=c(oid=m_id))
}

as_json <- function() {}
#' Converts S4 objects to JSON representation.
#'
#'
#' These methods extend the \code{\link[jsonlite]{toJSON}} function providing
#' an extensible protocol for serializing S4 objects.  The function
#' \code{as.json} turns the object into a string containing a JSON document by
#' first calling \code{as.jlist} to convert the object into a list and then
#' calling \code{toJSON} to do the work.
#'
#'
#' The existing \code{\link[jsonlite]{toJSON}} does not support S4 objects, and
#' the \code{\link[jsonlite]{serializeJSON}} provides too much detail; so while
#' it is good for saving and restoring R objects, it is not good for sharing
#' data between programs.  The function \code{as.json} and \code{as.jlist} are
#' S4 generics, so they can be easily extended to other classes.
#'
#' The default method for \code{as.json} is essentially \code{toJSON(
#' as.jlist(x, attributes(x)))}.  The function \code{attributes(x)} turns the
#' fields of the object into a list, and then the appropriate method for
#' \code{as.jlist} further processes those objects.  For example, it can set
#' the \code{"_id"} field used by the Mongo DB as a unique identifier (or other
#' derived fields) to \code{NULL}.
#'
#' Another important step is to call \code{unboxer} on fields which should not
#' be stored as vectors.  The function \code{toJSON} by default wraps all R
#' objects in \sQuote{[]} (after all, they are all vectors), but that is
#' probably not useful if the field is to be used as an index.  Wrapping the
#' field in \code{unboxer()}, i.e., using \code{ml$field <- unboxer(ml$field)},
#' suppresses the brackets.  The function \code{unboxer()} in this package is
#' an extension of the \code{jsonlite::\link[jsonlite]{unbox}} function, which
#' does not properly unbox POSIXt objects.
#'
#' Finally, for a field that can contain arbitrary R objects, the function
#' \code{\link{unparseData}} coverts the data into a JSON string which will
#' completely recover the data.  The \code{serialize} argument is passed to
#' this function.  If true, then \code{\link[jsonlite]{serializeJSON}} is used
#' which produces safe, but not particularly human editable JSON.  If false, a
#' simpler method is employed which produes more human readable code.  This
#' with should work for simpler data types, but does not support objects, and
#' may fail with complex lists.
#'
#' @aliases as.json as.json,ANY-method as.jlist as.jlist,ANY,list-method
#' @param x An (S4) object to be serialized.
#' @param obj The object being serialized
#' @param ml A list of fields of the object; usually \code{attributes(obj)}.
#' @param serialize A logical flag. If true,
#' \code{\link[jsonlite]{serializeJSON}} is used to protect the \code{data}
#' field (and other objects which might contain complex R code.
#' @param serialize logical -- Preserve all R information at the expense of legibility. Passed to [jsonlite::toJSON()]
#' @param dataframe ("rows", "columns", "values") -- Order for data frames. Passed to [jsonlite::toJSON()]
#' @param matrix ("rowmajor" "columnmajor") -- Order for matrix elements.  Passed to [jsonlite::toJSON()]
#' @param Date ("ISO8601" "epoch") -- Passed to [jsonlite::toJSON()]
#' @param POSIXt ("string" "ISO8601" "epoch" "mongo") -- Date/time format. Passed to [jsonlite::toJSON()]
#' @param complex ("string" "list") -- Representation for complex numbers.  Passed to [jsonlite::toJSON()]
#' @param factor ("string" "list") -- Treatment of factor variables.  Passed to [jsonlite::toJSON()]
#' @param raw ("base64" "hex" "mongo" "int" "js") -- Treatment of raw data.  Passed to [jsonlite::toJSON()]
#' @param null ("list" "null") -- Treatment of null fields. Passed to [jsonlite::toJSON()]
#' @param na ("null" "string") -- Representation for NA's.  Passed to [jsonlite::toJSON()]
#' @return
#'
#' The function \code{as.json} returns a unicode string with a serialized
#' version of the object.
#'
#' The function \code{as.jlist} returns a list of the fields of the object
#' which need to be serialized (usually through a call to
#' \code{\link[jsonlite]{toJSON}}.
#' @author Russell Almond
#' @seealso In this package: \code{\link{buildObject}}, \code{\link{saveRec}},
#' \code{\link{parseData}}, \code{\link{parseSimpleData}}
#'
#' In the jsonlite package: \code{\link[jsonlite]{toJSON}},
#' \code{\link[jsonlite]{serializeJSON}},
#' \code{jsonlite::\link[jsonlite]{unbox}}
#' @keywords IO interfaces
#' @examples
#' \dontrun{
#' vingette("JSON for S4 Objects")
#' }
#'
#' @export as.json
#' @export as.jlist
#' @exportMethod as.json
#' @exportMethod as.jlist
setGeneric("as.json",function(x,serialize=TRUE,
                              dataframe = c("rows", "columns", "values"),
                              matrix = c("rowmajor","columnmajor"),
                              Date = c("ISO8601", "epoch"),
                              POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                              factor = c("string", "list"),
                              complex = c("string", "list"),
                              raw = c("base64", "hex", "mongo", "int", "js"),
                              null = c("list", "null"),
                              na = c("null", "string"))
  standardGeneric("as.json"))

#' @rdname as.json
#' @aliases as.jlist
setGeneric("as.jlist",function(obj,ml,serialize=TRUE)
  standardGeneric("as.jlist"))

#' @rdname as.json
#' @aliases as.json,ANY-method
setMethod("as.json","ANY",
          function(x,serialize=TRUE,
                   dataframe = c("rows", "columns", "values"),
                   matrix = c("rowmajor","columnmajor"),
                   Date = c("ISO8601", "epoch"),
                   POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                   factor = c("string", "list"),
                   complex = c("string", "list"),
                   raw = c("base64", "hex", "mongo", "int", "js"),
                   null = c("list", "null"),
                   na = c("null", "string")) {
    if (isS4(x)) {
      jlist <- as.jlist(x,attributes(x),serialize)
    } else if (is.object(x) && is.list(x)) {
      jlist <- unboxer(c(x,class=class(x)))
    } else {
      jlist <- unboxer(x)
    }
    jsonlite::toJSON(jlist, dataframe[1], matrix[1], Date[1], POSIXt[1],
           factor[1], complex[1],
           raw[1], null[1], na[1])
})

#' @describeIn as.json The `as.json` for  `\linkS4class{MongoRec}` objects defaults to
#' using "mongo" format for the `POSIXt` and `raw` options.
#' @aliases as.json,MongoRec-method
setMethod("as.json","MongoRec",
          function(x,serialize=TRUE,
                   dataframe = c("rows", "columns", "values"),
                   matrix = c("rowmajor","columnmajor"),
                   Date = c("ISO8601", "epoch"),
                   POSIXt = c("string", "ISO8601", "epoch", "mongo"),
                   factor = c("string", "list"),
                   complex = c("string", "list"),
                   raw = c("base64", "hex", "mongo", "int", "js"),
                   null = c("list", "null"),
                   na = c("null", "string")) {
            ## Different defaults for Mongo
            if (missing(POSIXt)) POSIXt <- "mongo"
            if (missing(raw)) raw <- "mongo"
            jlist <- as.jlist(x,attributes(x),serialize)
            jsonlite::toJSON(jlist, dataframe[1], matrix[1],
                   Date[1], POSIXt[1], factor[1], complex[1],
                   raw[1], null[1], na[1])
          })

#' Adds/removes  package information to class descriptions
#'
#' If the class has a "package" attribute, then changes the descriptor to a form
#' "package::classname", e.g., the \code{\linkS4class{MongoRec}} class, which lives in the
#' "mongo" package becomes `mongo::MongoRec`.  The function `decodeClass()` reverses this.
#'
#' @param class character Class identifiers.  For `codeClass()` these might have the "package"
#' attribute set.  For `decodeClass()` the package attribute is represented by the prefix "package::".
#' @returns  The function `codeClass()` returns a character vector with "package" attributes changed to
#' "package::" prefixes.  The function `decodeClass()` returns a character vector with "package::"
#' prefixes removed and "package" attributes set.
#'
#' @note
#'
#' The `codeClass()` function applies [unboxer()] to mark single class names as singletons.
#' The `decodeClass()` function applies [ununboxer()] to remove the mark if needed.  Also,
#' if `class` is a list (happens if it was not quoted with [unboxer()] when saved to JSON),
#' `decodeClass()` will try to coerce it into a character vector.
#' @export codeClass
#' @examples
#' codeClass(class(MongoRec()))
#' codeClass(class(matrix(1:4,2,2)))
#' decodeClass(codeClass(class(MongoRec())))
#' decodeClass(codeClass(class(matrix(1:4,2,2))))
#'
codeClass <- function(class) {
  pack <- attr(class,"package")
  if (!is.null(pack) && pack != ".GlobalEnv") {
      class <- paste(pack,class,sep="::")
  }
  unboxer(class)
}
#' @rdname codeClass
#' @export decodeClass
decodeClass <- function (class) {
  class <- as.character(ununboxer(class))
  if (length(class)==1L && grepl("::",class)) {
    cc <- strsplit(class,"::")[[1]]
    class <- cc[2]
    attr(class,"package") <- cc[1]
  }
  class
}

#' @describeIn as.json
#' This is the default method, it simply returns
#' the list of slots `ml`.  This also does not contain a call to
#' `callNextMethod`, so it will serve as the termination point for an
#' inheritance chain.
setMethod("as.jlist",c("ANY","list"), function(obj,ml,serialize=TRUE) {
  ml$class <- codeClass(class(obj))
  ml
})

#' @describeIn as.json  This method actually removes the Mongo id
#' (`_id`) as generally, that is not pass as part of an update query.
setMethod("as.jlist",c("MongoRec","list"), function(obj,ml,serialize=TRUE) {
  ml$"_id" <- NULL
  callNextMethod(obj,ml,serialize)
})

parse_jlist <- function() {}
#' @describeIn parse.json This is the inner function for processing
#' the slots prior to object creation.  Generally, this is the method
#' that needs to be specialized.  See the `vignette("JSON for S4
#' Objects")`.
setGeneric("parse.jlist",function(class,rec)
  standardGeneric("parse.jlist"))


#' @describeIn parse.json Base case for callNextMethod; just returns
#' the slot list.
setMethod("parse.jlist",c("ANY","list"),
          function(class,rec) {
            rec
            })

isDefaultMethod <- function(meth) {
  meth@target["class"] == "ANY"
}

#' @describeIn parse.json This method takes the jlist, cleans it with an appropriate
#' `parse.jlist` method and then tries to generate an object based on the class.
buildObject <- function (rec, class=decodeClass(rec$class)) {
  if (is.list(class) && length(class)==1L)
    ## toJSON has wrapped the class name, fix.
    class <- as.character(class[[1]])
  if (length(class)==1L && grepl("::",class)) {
    cc <- strsplit(class,"::")[[1]]
    class <- cc[2]
    attr(class,"package") <- cc[1]
  }
  if (length(class)==1L) {
    jlp <- selectMethod("parse.jlist",c(class,"list"))
  } else {
    ## S3 class with multiple possible targets.
    jlp <- selectMethod("parse.jlist",c("ANY","list"))
    for (cc in class) { #S3 class names are a list, and break S4 generic dispatch
      jlp <- selectMethod("parse.jlist",c(cc,"list"))
      if (!isDefaultMethod(jlp)) break
    }
  }

  if (!is.null(jlp)) {
    rec <- do.call(jlp,list(class,rec))
  } else {
    rec <- parseSimpleData(rec)
  }
  rec$class <- NULL # Make sure it is not marked as an extra argument.
  if (length(class)==0L) {
      result <- rec
  } else {
    if (length(class) >1L || !isClass(class)) {
      ## S3 class
      result <- rec
      class(result) <- class
    } else {
      ## S4 class
      result <- do.call("new",c(list(class),rec))
    }
  }
  result
}

#' Construct an S4 object from a list of its slot values.
#'
#' The `parse.json` function uses the
#' \code{\link[jsonlite]{fromJSON}} function to turn the JSON into a list, which
#' is processed using the function \code{parse.jlist}
#' to massage the elements, and then passes it to the \code{new} function
#' to create a new object of type \code{class}.
#'
#' The \code{parse.jlist} function is a helper function designed to do any massaging
#' necessary to unencode the slot values before the object is produced.  The function
#' \code{\link{ununboxer}} undoes the effect of \code{unboxer}, and the
#' function \code{\link{unparseData}} undoes the effect of \code{parseData}.
#'
#' @param encoded -- a character scalar giving the raw JSON object.
#' @param rec -- A list which is the output of \code{\link[jsonlite]{fromJSON}}
#' @param builder -- A function which will construct an object from a list of fields values.
#' @param class  -- A character string defining the class of the output object.
#'     If the list has an element named `class`, that will be used.
#'
#' @return An S4 object of type `class`
#' @export parse.json
#' @export buildObject
#' @export parse.jlist
#' @exportMethod parse.jlist
#'
#' @examples
#' \dontrun{
#' vignette("JSON for S4 Objects")
#' }
parse.json <- function(encoded, builder=buildObject) {
  jlist <- jsonlite::fromJSON(encoded,FALSE)
  do.call(builder,list(jlist))
}


#' @describeIn parse.json Makes sure the `_id` field corresponds to
#' conventions, and inserts `NA` if it is missing.
setMethod("parse.jlist",c("MongoRec","list"), function(class, rec) {
  if (is.null(rec$"_id"))
    id <- NA_character_
  else
    id <- as.character(ununboxer(rec$"_id"))
  if (is.null(names(id)))  names(id) <- "oid"
  rec$"_id" <- id
  callNextMethod(class, rec)
})

#' parseSimpleData
#'
#' Simple parser works with data which is mostly primitive R types (`numeric`, `integer`, `logical`,
#' `character`).
#'
#' @param messData list output from `\link[jsonlite]{fromJSON}`
#'
#' @return list, simplified.
#' @export parseSimpleData
#'
#' @details
#'
#' The `\link[jsonlite]{fromJSON}` method does not distinguish between arrays type character,
#' logical, integer or numeric.  This function finds lists of a single atomic type and
#' replaces them with the corresponding `vector`.
#'
#'
#' @examples
#' parseSimpleData(list(chars=list("a","b","c"),nums=list(2.3,3.4,4.5),
#'   ints=list(1,2,3), logic=list(TRUE,FALSE)))
parseSimpleData <- function (messData) {
  ##Need to convert back from list to numeric/character
  if (length(messData) == 0L) return(list())
  for (i in 1:length(messData)) {
    datum <- messData[[i]]
    if (all(sapply(datum,is.character)) && all(sapply(datum,length)==1L)) {
      datum <- as.character(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.logical)) && all(sapply(datum,length)==1L)) {
      datum <- as.logical(datum)
      names(datum) <- names(messData[[i]])
    }
    if (all(sapply(datum,is.numeric)) && all(sapply(datum,length)==1L)) {
      if (all(sapply(datum, function(d) ceiling(d)==floor(d)))) {
        datum <- as.integer(datum)
      } else {
        datum <- as.double(datum)
      }
      names(datum) <- names(messData[[i]])
    }
    ## May need an extra step here to decode data which
    ## are not one of the primitive vector types.
    messData[[i]] <- datum
  }
  messData
}

#' Prepare R data for storage or restore R data from jlist
#'#'
#' The `parseData` function is a helper function for [parse.jlist()] methods, and `unparseData`
#' for [as.jlist()], which represents complex objects as JSON.
#'
#' @aliases unparseData
#' @param messData (or character jlist)
#' @param data ANY the data to be saved.
#' @param serialize logical if Tru
#'
#' @return
#'
#' `parseData` returns the parsed object.  `unparseData` returns a jlist or character scalar
#' which can be saved.
#'
#' @details
#'
#' There are three strategies for saving/restoring an R object a JSON.
#'
#' * Use the `\link[jsonlite]{serializeJSON}` and `\link[jsonlite]{unserializeJSON}` method.
#' This will faithfully reproduce the object, but it will be difficult to manipulate the object
#' outside of R.
#' * For an S4 object write a [as.jlist()] and [parse.jlist()] method.
#' * For a S3 object or just a list of arbitrary objects, write out the object using
#' `\link[jsonlite]{toJSON}` and fix up the types of the components when the object is read back in.
#'
#' When `unparseData(...,serialize=TRUE)` is called, then `parseData` and `unparseData`
#' take the first approache.
#'
#' Otherwise, it takes the third approach.  In particular, `\link[jsonlite]{fromJSON}` turns
#' a vector which contains all elements of the same type
#' (currently only "logical", "integer", "numeric" and "character")
#' it turns the list into a vector of the corresponding mode.
#'
#'
#'
#'
#' @export parseData
#' @export unparseData
#'
#' @examples
#'
#' dat <- list(chars=letters[1:3], nums=c(-3.3, 4.7),
#'   ints=1L:3L, logic=c(TRUE,FALSE))
#' j1 <- jsonlite::toJSON(unparseData(dat))
#' j2 <- unparseData(dat,serialize=TRUE)
#' jsonlite::fromJSON(j1)
#' parseData(jsonlite::fromJSON(j1))
#' parseData(jsonlite::fromJSON(j2))
#'
parseData <- function (messData) {
  if (is.character(messData)) {
    jsonlite::unserializeJSON(messData)
  } else {
    parseSimpleData(messData)
  }
}

#' @rdname parseData
unparseData <- function (data,serialize=TRUE) {
  if (serialize)
    jsonlite::unbox(jsonlite::serializeJSON(data))
  else
    unboxer(data)
}


