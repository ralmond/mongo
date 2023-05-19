setClass("Event",
         slots=list(uid="character",
                    mess="character",
                    timestamp="POSIXt",
                    processed="logical",
                    data="list"
         ),
         contains="MongoRec")

Event <- function(uid,mess,data=list(),timestamp=Sys.time(),processed=FALSE,
                  m_id=c("oid"=NA_character_)) {
  new("Event",uid=uid,mess=mess,timestamp=timestamp,processed=FALSE,"_id"=m_id)
}

anEvent <- new("Event",uid="Tester",mess="Typed",
               timestamp=as.POSIXct("2022-05-30 16:38:58 EDT"),
               processed=FALSE,"_id"=c("oid"=NA_character_),
               data=list(input="Hello, World!"))
setMethod("as.jlist",c("Event","list"),
          function(obj, ml, serialize=TRUE) {
            ml$uid <- unboxer(ml$uid)
            ml$mess <- unboxer(ml$mess)
            ml$timestamp <- unboxer(ml$timestamp)
            ml$processed <- unboxer(ml$processed)
            ml$data <- unparseData(ml$data,
                                   serialize)
            callNextMethod(obj, ml, serialize)
          })

setMethod("parse.jlist",c("Event","list"),
          function(class, rec) {
            rec$uid <- as.character(ununboxer(rec$uid))
            rec$mess <- as.character(ununboxer(rec$mess))
            rec$timestamp <- as.POSIXct(ununboxer(rec$timestamp))
            rec$processed <- as.logical(ununboxer(rec$processed))
            rec$data <- parseData(rec$data)
            callNextMethod(class, rec)
          })

all.equal.Event <- function (target, current,
                             ...,
                             checkTimestamp=FALSE,check_ids=TRUE) {
  if (!is(current,"Event"))
    return(paste("Target is 'Event' and current is '",
                 class(current),"'."))
  msg <- character()
  if (check_ids)
    if ((is.na(target@"_id") && !is.na(current@"_id")) ||
        (!is.na(target@"_id") &&
         !isTRUE(all.equal(target@"_id", current@"_id"))))
      msg <- c(msg,"Database IDs do not match.")
  if (!isTRUE(all.equal(target@uid,current@uid)))
    msg <- c(msg,"User IDs do not match.")
  if (!isTRUE(all.equal(target@mess,current@mess)))
    msg <- c(msg,"Messages do not match.")
  if (!isTRUE(all.equal(target@processed,current@processed)))
    msg <- c(msg, "Processed flags do not match.")
  ## Check Data
  namet <- names(target@data)
  namec <- names(current@data)
  if (length(target@data) != length(current@data) ||
      !setequal(namet,namec)) {
    msg <- c(msg,"Names or number of data differ.")
    if (length(setdiff(namet,namec)) > 0L)
      msg <- c(msg,paste("Data in target but not in current:",
                         setdiff(namet,namec)))
    if (length(setdiff(namec,namet)) > 0L)
      msg <- c(msg,paste("Data in current but not in target:",
                         setdiff(namec,namet)))
  }
  msgd <- all.equal(target@data,current@data,...)
  if (!isTRUE(msgd)) msg <- c(msg,msgd)
  ## Timestamp
  if (checkTimestamp) {
    if (abs(target@timestamp-current@timestamp) >
        as.difftime(.1,units="secs"))
      msg <- c(msg,"Timestamps differ by more than .1 secs")
  }

  ## Return true if message list is empty.
  if (length(msg)==0L) TRUE
  else msg
}

fred1 <- new("Event",uid="Fred",mess="Typed",
               timestamp=as.POSIXct("2022-05-30 16:38:58 EDT"),
               processed=TRUE,"_id"=c("oid"=NA_character_),
               data=list(input="Hello, World!"))
fred2 <- new("Event",uid="Fred",mess="Typed",
               timestamp=as.POSIXct("2022-05-30 16:40:58 EDT"),
               processed=FALSE,"_id"=c("oid"=NA_character_),
               data=list(input="Hello, Weird!"))
fred3 <- new("Event",uid="Fred",mess="Deleted",
             timestamp=as.POSIXct("2022-05-30 16:42:58 EDT"),
             processed=TRUE,"_id"=c("oid"=NA_character_),
             data=list(input="Hello, World!"))
fred4 <- new("Event",uid="Fred",mess="Submitted",
             timestamp=as.POSIXct("2022-05-30 16:43:58 EDT"),
             processed=FALSE,"_id"=c("oid"=NA_character_),
             data=list(status="Draft"))
phred1 <- new("Event",uid="Phred",mess="Typed",
             timestamp=as.POSIXct("2022-05-30 15:38:58 EDT"),
             processed=FALSE,"_id"=c("oid"=NA_character_),
             data=list(input="Hello, World!"))


sampleEvents <- list(fred1,fred2,fred3,fred4,phred1)



