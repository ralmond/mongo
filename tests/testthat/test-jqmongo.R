
testthat::test_that("buildJQterm", {
  expect_equal(buildJQterm("uid","Fred"),'"uid":"Fred"')
  expect_equal(buildJQterm("uid",c("Phred","Fred")),'"uid":{"$in":["Phred","Fred"]}')
  ## Need explicit time zone as test may run on computer in different ime zone.
  time1 <- as.POSIXct("2018-08-16 19:12:19 EDT", tz="GMT")
  t1 <- 1534446739L  # internal number time 1000
  expect_equal(buildJQterm("time",time1),
               sprintf('"time":{"$date":%d000}',t1))
  time1l <- as.POSIXlt("2018-08-16 19:12:19 EDT", tz="GMT")
  expect_equal(buildJQterm("time",time1l),
               sprintf('"time":{"$date":%d000}',t1))
  time2 <- as.POSIXct("2018-08-16 19:13:19 EDT", tz="GMT")
  t2 <- 1534446799L
  expect_equal(buildJQterm("time",c(time1,time2)),
               sprintf('"time":{"$in":[{"$date":%d000},{"$date":%d000}]}',t1,t2))
  expect_equal(buildJQterm("time",c(gt=time1)),
               sprintf('"time":{ "$gt":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(lt=time1)),
               sprintf('"time":{ "$lt":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(gte=time1)),
               sprintf('"time":{ "$gte":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(lte=time1)),
               sprintf('"time":{ "$lte":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(ne=time1)),
               sprintf('"time":{ "$ne":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(eq=time1)),
               sprintf('"time":{ "$eq":{"$date":%d000} }',t1))
  expect_equal(buildJQterm("time",c(gt=time1,lt=time2)),
               sprintf('"time":{ "$gt":{"$date":%d000}, "$lt":{"$date":%d000} }', t1, t2))
  expect_equal(buildJQterm("count",c(nin=1,2:4)),
             '"count":{"$nin":[1,2,3,4]}')
  expect_equal(buildJQterm("count",c("in"=1,2:4)),
             '"count":{"$in":[1,2,3,4]}')
  expect_equal(buildJQterm("count",c(ne=1,ne=5)),
             '"count":{ "$ne":1, "$ne":5 }')
})


testthat::test_that("buildJQuery", {
  expect_equal(buildJQuery(app="default",uid="Phred"),
             '{ "app":"default", "uid":"Phred" }')
  expect_equal(buildJQuery("_id"=c(oid="123456789")),
             '{ "_id":{ "$oid":"123456789" } }')
  expect_equal(buildJQuery(name="George",count=c(gt=3,lt=5)),
             '{ "name":"George", "count":{ "$gt":3, "$lt":5 } }')
  expect_equal(buildJQuery(name="George",count=c(gt=3,lt=5),
                         rawfields=c('"$limit":1','"$sort":{timestamp:-1}')),
             '{ "name":"George", "count":{ "$gt":3, "$lt":5 }, "$limit":1, "$sort":{timestamp:-1} }')
  expect_equal(buildJQuery("_id"=c(oid="123456789abcdef")),
             '{ "_id":{ "$oid":"123456789abcdef" } }')
})

testthat::test_that("getOneRec", {
  skip_if_not(MongoAvailable)
  rebuildTestEvents()
  rec <- getOneRec(EventDB,buildJQuery(name="Fred",processed=FALSE))
  expect_null(rec)
  rec <- getOneRec(EventDB,buildJQuery(uid="Fred",processed=FALSE))
  expect_eq(rec,fred4,check_ids=FALSE)
  recr <- getOneRec(EventDB,buildJQuery(uid="Fred",processed=FALSE),
                   sort=buildJQuery(imestamp=1))
  expect_eq(recr,fred2,check_ids=FALSE)
})


testthat::test_that("getManyRecs", {
  skip_if_not(MongoAvailable)
  rebuildTestEvents()
  recs <- getManyRecs(EventDB,buildJQuery(uid="Fred",processed=FALSE),
                      sort=buildJQuery(timestamp=1))
  expect_length(recs,2)
  expect_eq(recs[[1]],fred2,check_ids=FALSE)
  expect_eq(recs[[2]],fred4,check_ids=FALSE)
  recsr <- getManyRecs(EventDB,buildJQuery(uid="Fred",processed=FALSE),
                      sort=buildJQuery(timestamp=-1))
  expect_length(recsr,2)
  expect_eq(recsr[[2]],fred2,check_ids=FALSE)
  expect_eq(recsr[[1]],fred4,check_ids=FALSE)
})


testthat::test_that("saveRec new Rec", {
  skip_if_not(MongoAvailable)
  rebuildTestEvents()
  jim <- Event(uid="James Goodfellow",mess="Hello",data=list(xxx=1:3))
  saveRec(EventDB,jim)
  jim1 <- getOneRec(EventDB,buildJQuery(uid="James Goodfellow"))
  expect_eq(jim1,jim,check_ids=FALSE)
})

testthat::test_that("saveRec replace", {
  skip_if_not(MongoAvailable)
  rebuildTestEvents()
  jim <- Event(uid="James Goodfellow",mess="Hello",data=list(xxx=1:3))
  jim <- saveRec(EventDB,jim)
  expect_false(is.na(m_id(jim)))
  jim@data$yyy <- 3:5
  saveRec(EventDB,jim)
  expect_equal(mdbCount(EventDB,buildJQuery(uid="James Goodfellow")),1)
  jim1 <- getOneRec(EventDB,buildJQuery(uid="James Goodfellow"))
  expect_eq(jim1,jim,check_ids=TRUE,check_timestamps=TRUE)
})

testthat::test_that("makeDBuri", {
  expect_equal(makeDBuri(),"mongodb://localhost")
  expect_equal(makeDBuri(user="admin",password="secret"),
              "mongodb://admin:secret@localhost")
  expect_equal(makeDBuri(user="admin"),
                       "mongodb://admin@localhost")
  expect_equal(makeDBuri(host="example.com",port=12345),
             "mongodb://example.com:12345")
})

