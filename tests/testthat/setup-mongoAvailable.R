## Set a flag if mongo is not available (so we can run the skip test just once)
if (identical(Sys.getenv("NOT_CRAN"),"true")) {
  con <-  try (mongolite::mongo())
  if (is(con, "try-error"))
    MongoAvailable <- FALSE
  else
    MongoAvailable <- TRUE
} else {
  ## On CRAN, skip
  warning("Mongo Tests skipped on CRAN")
  MongoAvailable <- FALSE
}

EventDB <- MongoDB("Events",noMongo=!MongoAvailable)

rebuildTestEvents <- function() {
mdbDrop(EventDB)
fred1 <<- saveRec(EventDB,fred1)
fred2 <<- saveRec(EventDB,fred2)
fred3 <<- saveRec(EventDB,fred3)
fred4 <<- saveRec(EventDB,fred4)
phred1 <<- saveRec(EventDB,phred1)
mdbIndex(EventDB,buildJQuery(processed=1,timestamp=1))
}
rebuildTestEvents()





