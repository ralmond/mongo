
## Start by checking the helper event class.

testthat::test_that("all.equal.Event works", {
  anEvent1 <- anEvent
  anEvent2 <- anEvent
  anEvent2@mess <- "Shouted"
  testthat::expect_true(isTRUE(all.equal(anEvent,anEvent1)))
  testthat::expect_false(isTRUE(all.equal(anEvent,anEvent2)))
})


testthat::test_that("expect_eq works", {
  anEvent1 <- anEvent
  anEvent2 <- anEvent
  anEvent2@mess <- "Shouted"
  testthat::expect_success(expect_eq(anEvent,anEvent1))
  testthat::expect_failure(expect_eq(anEvent,anEvent2))
})

testthat::test_that("Round trip test for as.json and buildObject.", {
  jEvent <- as.json(anEvent)
  pEvent <- buildObject(jsonlite::fromJSON(jEvent),"Event")
  expect_eq(pEvent,anEvent)
})

testthat::test_that("m_id", {
  mr <- MongoRec()
  testthat::expect_true(is.na(m_id(mr)))
  m_id(mr) <- "012345"
  testthat::expect_equal(m_id(mr),c(oid="012345"))
})

testthat::test_that("(de)codeClass",{
  expect_equal(codeClass(class(MongoRec())),unboxer("mongo::MongoRec"))
  expect_equal(codeClass(class(matrix(1:4,2,2))),c("matrix","array"))
  expect_equal(decodeClass("mongo::MongoRec"),class(MongoRec()))
  expect_equal(decodeClass(c("matrix","array")),class(matrix(1:4,2,2)))
})

testthat::test_that("as.jlist_ANY", {
  inlist <- list(name="test",data=1:3,time=Sys.time())
  testthat::expect_equal(as.jlist(TRUE,inlist),c(inlist,class=list(unboxer("logical"))))
})


testthat::test_that("as.jlist_MongoRec", {
  mr <- MongoRec()
  testthat::expect_null(as.jlist(mr,attributes(mr))$`_id`)
  m_id(mr) <- "01234"
  testthat::expect_null(as.jlist(mr,attributes(mr))$`_id`)
})


testthat::test_that("as.jlist_Event", {
  jl <- as.jlist(anEvent,attributes(anEvent))
  ## These should be all unboxed, i.e., of class "scalar".
  expect_s3_class(jl$uid,"scalar")
  expect_s3_class(jl$mess,"scalar")
  expect_s3_class(jl$timestamp,"scalar")
  expect_s3_class(jl$processed, "scalar")
  expect_s3_class(jl$data,"scalar")
  expect_type(jl$data,"character")
  expect_equal(c(jl$class),"Event")
})

setClass("anObject",list(field="numeric"))
anObject <- function (val) {
  new("anObject",field=val)
}

testthat::test_that("as.json-ANY", {
  expect_equal(as.character(as.json(anObject(3))),'{"field":[3],"class":"mongo::anObject"}')
  expect_equal(as.character(as.json(1:3)),'[1,2,3]')
})

testthat::test_that("as.json_MongoRec", {
  mr <- MongoRec()
  expect_equal(as.character(as.json(mr)),'{"class":"mongo::MongoRec"}')
  m_id(mr) <- "123"
  expect_equal(as.character(as.json(mr)),'{"class":"mongo::MongoRec"}')
})


testthat::test_that("buildObject S4 Class", {
  obj <- anObject(3)
  jobj <- as.json(anObject(3))
  expect_equal(parse.json(jobj),obj)
})

testthat::test_that("buildObject with parse.jlist", {
  restoredEvent <- buildObject(as.jlist(anEvent,attributes(anEvent)))
  expect_eq(restoredEvent,anEvent)
})

testthat::test_that("buildObject S3 class", {
  rec <- list(student="Fred",assessment="pretest",score=17L)
  class(rec) <- c("Score")
  restored <- parse.json(as.json(rec))
  expect_equal(restored,rec)
  })

testthat::test_that("buildObject primitive object", {
 obj <- list(num=1:3,char="foo")
 restored <- parse.json(as.json(obj))
 expect_equal(obj,restored)
})


testthat::test_that("parse.jlist_MongoRec", {
  mr<- MongoRec(m_id="123")
  mrjl <- as.jlist(mr,attributes(mr))
  nmr <- parse.jlist(mr,mrjl)
  expect_equal(nmr$`_id`,c(oid=NA_character_))
  nmr1 <- parse.jlist(mr,list("_id"="123"))
  expect_equal(nmr1$`_id`,c(oid="123"))
})


testthat::test_that("unboxer", {
  expect_s3_class(unboxer(1),"scalar")
  expect_equal(ununboxer(unboxer(1)),1)
  expect_false(is(unboxer(1:3),"scalar"))
  expect_equal(ununboxer(unboxer(1:3)),1:3)
  today <- Sys.Date()
  expect_s3_class(unboxer(as.POSIXct(today)),"scalar")
  expect_s3_class(unboxer(as.POSIXlt(today)),"scalar")
  expect_equal(ununboxer(unboxer(as.POSIXct(today))),as.POSIXct(today))
  expect_equal(ununboxer(unboxer(as.POSIXlt(today))),as.POSIXlt(today))
  dat <- list(one=1,three=1:3)
  undat <- unboxer(dat)
  expect_s3_class(undat$one,"scalar")
  expect_false(is(undat$three,"scalar"))
  expect_equal(ununboxer(undat),dat)
  now <- as.POSIXlt(Sys.time())
  expect_equal(ununboxer(now),now)

  ldat <- list(a=1,b="foo")
  expect_equal(ununboxer(unboxer(ldat)),ldat)

})


testthat::test_that("parseSimpleData", {
   out <- parseSimpleData(list(chars=list(a="a",b="b",c="c"),nums=list(2.3,3.4,4.5),
     ints=list(1,2,3), logic=list(TRUE,FALSE)))
   expect_type(out$char,"character")
   expect_type(out$nums,"double")
   expect_type(out$ints,"integer")
   expect_type(out$logic,"logical")
   expect_equal(names(out$char),letters[1:3])
})


testthat::test_that("parseData", {
  jEvent <- as.json(anEvent)
  pEvent <- buildObject(jsonlite::fromJSON(jEvent))
  expect_eq(pEvent,anEvent)
})

testthat::test_that("parsePOSIX", {
  dt <- Sys.time()
  expect_equal(parsePOSIX(unboxer(dt)),dt,ignore_attr=c("tzone","waldo_opts"))

  dtjm <- jsonlite::toJSON(unboxer(dt),POSIXt="mongo")
  dtlm <- jsonlite::fromJSON(dtjm,FALSE)
  expect_equal(parsePOSIX(dtlm),dt,ignore_attr=c("tzone","waldo_opts"))

  dtjz <- jsonlite::toJSON(unboxer(dt))
  dtlz <- jsonlite::fromJSON(dtjm,FALSE)
  expect_equal(parsePOSIX(dtlz),dt,ignore_attr=c("tzone","waldo_opts"))

})

