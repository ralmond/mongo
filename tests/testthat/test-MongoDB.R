
expect_in_table <- function (object, table) {
  act <- quasi_label(rlang::enquo(object),arg="object")
  act$nfields <- length(act$val)
  if (act$nfields!=ncol(table)) {
    message <- sprintf("%s has %d fields, but table has %d fields",
                       act$arg, act$nfields, ncol(table))
    testthat::fail(message)
  }
  nrec <- sort(names(object))
  ntab <- sort(names(table))
  mtab <- sapply(1:length(nrec), function (c) {
    act$val[[nrec[c]]] == table[,ntab[c]]
  })
  matches <- apply(mtab,1,all)
  if (!any(matches)) {
    testthat::fail("Object not found in table.")
  }
  testthat::succeed()
  invisible(act$val)
}

test_that("expect_in_table test",{
  expect_success(expect_in_table(iris[1,],iris[1:20,]))
  expect_failure(expect_in_table(iris[101,],iris[1:10,]),
               "Object not found in table")
})


expect_contains <- function(object, element) {
  act <- quasi_label(rlang::enquo(object),arg="object")
  expect(element %in% act$val,
         sprintf("%s (%s) does not contain %s",
                 act$arg, paste(act$val, collapse=", "), element))
  invisible(act$val)
}

expect_not_contains <- function(object, element) {
  act <- quasi_label(rlang::enquo(object),arg="object")
  expect(!(element %in% act$val),
         sprintf("%s (%s) contain %s, but should not",
                 act$arg, paste(act$val, collapse=", "), element))
  invisible(act$val)
}

test_that("expect_contains test", {
  expect_success(expect_contains(letters[1:5],"c"))
  expect_failure(expect_contains(letters[1:5],"z"))
  expect_failure(expect_not_contains(letters[1:5],"c"))
  expect_success(expect_not_contains(letters[1:5],"z"))
})


test_that("Create DB", {
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  expect_s4_class(mdb,"MongoDB")
})


test_that("MongoDB db",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  expect_null(mdb$mongoObj)
  mcol <- mdb$db()
  expect_s3_class(mcol,"mongo")
  expect_s3_class(mdb$mongoObj,"mongo")
})

test_that("MongoDB avaliable",{
  skip_if_not(MongoAvailable)
  mdb_null <- MongoDB("foo","bar","")
  expect_false(mdb_null$available())
  mdb_valid <- MongoDB("foo","bar","mongodb://localhost")
  expect_true(mdb_valid$available())
})

test_that("MongoDB drop",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbInsert(mdb,iris)
  dblist <- showCollections(mdb)$name
  expect_contains(dblist,"testthis")
  mdbDrop(mdb)
  dblist <- showCollections(mdb)$name
  expect_not_contains(dblist,"testthis")

})


test_that("MongoDB insert",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  testthat::expect_equal(mdbCount(mdb),nrow(iris))
})

test_that("MongoDB count",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  testthat::expect_equal(mdbCount(mdb),nrow(iris))
  testthat::expect_equal(
                mdbCount(mdb,'{"Species":"setosa"}'),
                sum(iris$Species=="setosa"))

})



test_that("MongoDB replace",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,c('{"name":"Fred", "gender":"M"}',
                  '{"name":"George", "gender":"M"}'))
  expect_equal(mdbCount(mdb,'{"name":"Fred"}'),1)
  expect_equal(mdbCount(mdb,'{"name":"Phred"}'),0)
  expect_equal(mdbCount(mdb,'{"gender":"M"}'),2)
  expect_equal(mdbCount(mdb,'{"gender":"F"}'),0)
  mdbReplace(mdb,'{"name":"Fred"}', '{"name":"Phred", "gender":"F"}')
  expect_equal(mdbCount(mdb,'{"name":"Fred"}'),0)
  expect_equal(mdbCount(mdb,'{"name":"Phred"}'),1)
  expect_equal(mdbCount(mdb,'{"gender":"M"}'),1)
  expect_equal(mdbCount(mdb,'{"gender":"F"}'),1)
  })

test_that("MongoDB find",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  results <- mdbFind(mdb,'{"Species":"setosa"}',
         fields = '{"Petal_Width":true, "Petal_Length":true, "_id":0}',
         sort = '{"Petal_Width":-1}', limit=10)
  expect_equal(nrow(results),10)
  expect_setequal(names(results),c("Petal_Width","Petal_Length"))
  expect_equal(order(results$Petal_Width,decreasing=TRUE),1:10)
  })

test_that("MongoDB index",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  mdbIndex(mdb,add='{"Petal_Length":1}')
  ind1 <- mdbIndex(mdb,add='{"Petal_Width":-1}')
  expect_equal(nrow(ind1$key),3)
  expect_setequal(names(ind1$key),c("_id","Petal_Length","Petal_Width"))
  expect_equal(sum(ind1$key$"_id",na.rm=TRUE),1)
  expect_equal(sum(ind1$key$"Petal_Length",na.rm=TRUE),1)
  expect_equal(sum(ind1$key$"Petal_Width",na.rm=TRUE),-1)
  ind2 <- mdbIndex(mdb,remove="Petal_Length_1")
  expect_equal(nrow(ind2$key),2)
  expect_setequal(names(ind2$key),c("_id","Petal_Width"))
  expect_equal(sum(ind2$key$"_id",na.rm=TRUE),1)
  expect_equal(sum(ind2$key$"Petal_Width",na.rm=TRUE),-1)
  })

test_that("MongoDB mapreduce",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  histdata <-
    mdbMapreduce(mdb,
                 map= "function (){emit(Math.floor(this.Petal_Length*5)/5, 1)}",
                 reduce="function (id,counts){return Array.sum(counts)}"
                 )
  names(histdata) <- c("Petal.length","count")
  expect_equal(sum(histdata$count),nrow(iris))
  expect_equal(min(histdata$Petal.length),min(iris$Petal.Length),tolerance=.11)
  expect_equal(max(histdata$Petal.length),max(iris$Petal.Length),tolerance=.11)
  })

test_that("MongoDB update",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,c('{"name":"Fred", "gender":"M"}',
                  '{"name":"George", "gender":"M"}'))
  expect_equal(mdbCount(mdb,'{"name":"Fred"}'),1)
  expect_equal(mdbCount(mdb,'{"name":"George"}'),1)
  expect_equal(mdbCount(mdb,'{"gender":"M"}'),2)
  expect_equal(mdbCount(mdb,'{"gender":"F"}'),0)
  mdbUpdate(mdb,'{"name":"Fred"}', '{"$set":{"gender":"F"}}')
  expect_equal(mdbCount(mdb,'{"name":"Fred"}'),1)
  expect_equal(mdbCount(mdb,'{"name":"George"}'),1)
  expect_equal(mdbCount(mdb,'{"gender":"M"}'),1)
  expect_equal(mdbCount(mdb,'{"gender":"F"}'),1)
  })


test_that("MongoDB aggregate",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  stats <- mdbAggregate(mdb,
   '[{"$group":{"_id":"$Species", "count": {"$sum":1},
                 "average_Petal_Length":{"$avg":"$Petal_Length"}}}]',
   options = '{"allowDiskUse":true}'
   )
  species <- stats$"_id"
  expect_setequal(species,as.character(unique(iris$Species)))

  countm <- stats$count
  names(countm) <- species
  countr <- sapply(species, function(sp) sum(iris$Species==sp))
  expect_mapequal(countm, countr)

  aplr <- sapply(species, function(sp) mean(iris[iris$Species==sp,"Petal.Length"]))
  expect_equal(stats$average_Petal_Length,c(aplr,use.names=FALSE))
  })

test_that("MongoDB rename",{
  skip_if_not(MongoAvailable)
  fdb <- MongoDB("foo","test","mongodb://localhost")
  withr::defer(mdbDrop(fdb))
  ffdb <- MongoDB("foofoo","test","mongodb://localhost")
  withr::defer(mdbDrop(ffdb))
  mdbInsert(fdb,'{"foo":"bar"}')
  mdbDrop(ffdb)
  col1 <- showCollections(fdb)
  expect_contains(col1$name,"foo")
  expect_not_contains(col1$name,"foofoo")
  mdbRename(fdb,"foofoo")
  col2 <- showCollections(fdb)
  expect_contains(col2$name,"foofoo")
  expect_not_contains(col2$name,"foo")
  })

test_that("MongoDB distinct",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  spec <- mdbDistinct(mdb,"Species")
  expect_setequal(spec, levels(iris$Species))
  })

test_that("MongoDB info",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  expect_equal(mdbInfo(mdb)$stats$count,nrow(iris))
  })

test_that("MongoDB run",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  cs <- mdbRun(mdb,'{"collStats":"testthis"}')
  expect_equal(cs$ns, "test.testthis")
  expect_equal(cs$count, nrow(iris))
  })


test_that("MongoDB iterate",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  iter <- mdbIterate(mdb,limit=10)
  i1 <- iter$one()
  expect_in_table(i1,iris)
  i3 <- iter$batch(3)
  expect_equal(length(i3),3)
  expect_in_table(i3[[1]],iris)
  expect_in_table(i3[[2]],iris)
  expect_in_table(i3[[3]],iris)
  n <- 4
  while (!is.null((item = iter$one()))) {
    n <- n +1
    expect_in_table(item,iris)
  }
  expect_equal(n,10)
})

test_that("MongoDB remove",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  mdbDrop(mdb)
  mdbInsert(mdb,iris)
  expect_equal(mdbCount(mdb),nrow(iris))
  mdbRemove(mdb,'{"Species":"setosa"}')
  expect_equal(mdbCount(mdb),sum(iris$Species != "setosa"))
  })

test_that("MongoDB disconnect",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthis","test","mongodb://localhost")
  col <- mdb$db()  ## Need to hang onto mongo record to test for disconnection.
  mdbDisconnect(mdb)
  expect_null(mdb$mongoObj)
  expect_message(col$info(),
                 "Connection lost. Trying to reconnect with mongo...")
})

test_that("MongoDB showDatabases",{
  skip_if_not(MongoAvailable)
  mdb <- MongoDB("testthat","test","mongodb://localhost")
  expect_contains(showDatabases(mdb)$name,"test")
  expect_contains(showDatabases(mdb)$name,"admin")
  })

test_that("MongoDB showCollections",{
  skip_if_not(MongoAvailable)
  mdt <- MongoDB("testthis","test","mongodb://localhost")
  dbname <- basename(tempfile("col"))
  mdb <- MongoDB(dbname,"test","mongodb://localhost")
  withr::defer(mdbDrop(mdb))
  mdbInsert(mdb, '{"foo":"bar"}')
  expect_contains(showCollections(mdt)$name,dbname)
  mdbDrop(mdb)
  expect_not_contains(showCollections(mdt)$name,dbname)
})




