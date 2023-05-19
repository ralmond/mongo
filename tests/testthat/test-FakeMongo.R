testthat::test_that("iterator",{
  itt <- iterator(as.list(1:3))
  expect_true(itt$hasNext())
  expect_equal(itt$nextElement(),1L)
  expect_true(itt$hasNext())
  expect_equal(itt$nextElement(),2L)
  expect_true(itt$hasNext())
  expect_equal(itt$nextElement(),3L)
  expect_false(itt$hasNext())
  expect_warning(nextEl <- itt$nextElement())
  expect_null(nextEl)
  expect_no_warning(itt$nextElement(FALSE))
})

testthat::test_that("iterator one and batch",{
  itt <- iterator(as.list(1:6))
  expect_equal(itt$one(),1L)
  expect_equal(itt$batch(3),as.list(2:4))
  expect_warning(result <- itt$batch(3))
  expect_equal(result,as.list(5:6))
  expect_no_warning(result <- itt$one())
  expect_null(result)
  expect_warning(result <- itt$batch(3))
  expect_null(result)
})

testthat::test_that("iterator reset",{
  itt <- iterator(as.list(1:2))
  expect_equal(itt$nextElement(),1L)
  expect_equal(itt$nextElement(),2L)
  expect_false(itt$hasNext())
  itt$reset()
  expect_true(itt$hasNext())
  expect_equal(itt$nextElement(),1L)
  itt$reset(as.list(5:6))
  expect_true(itt$hasNext())
  expect_equal(itt$nextElement(),5L)
})

agg1 <- data.frame("_id"=c("virginica","setosa","versicolor"),count=c(50,50,50))
agg2 <- data.frame("_id"=c("virginica"),count=c(50))
itt1.1 <- as.jlist(fred1,attributes(fred1))
itt1.2 <- as.jlist(fred2,attributes(fred2))
itt2.1 <- as.jlist(phred1,attributes(phred1))
dbs <- jsonlite::unserializeJSON('{"type":"list","attributes":{"names":{"type":"character","attributes":{},"value":["name","sizeOnDisk","empty"]},"class":{"type":"character","attributes":{},"value":["data.frame"]},"row.names":{"type":"integer","attributes":{},"value":[1,2,3,4]}},"value":[{"type":"character","attributes":{},"value":["admin","config","local","test"]},{"type":"double","attributes":{},"value":[40960,110592,73728,430080]},{"type":"logical","attributes":{},"value":[false,false,false,false]}]}')
cols1 <- data.frame(name=c("testthis","Events","test","iris"),type="collection",readOnly=FALSE)
cols2 <- data.frame(name=c("testthis","foofoo","Events","test","bar","iris"),type="collection",readOnly=FALSE)


make_fake <- function() {
  fake_mongo(aggregate=list(agg1, agg2),
             count=list(10L,150L),
             distinct=list(c("red","blue","green"),character()),
             find=list(iris,iris[101:150,]),
             iterate=list(iterator(list(itt1.1,itt1.2)), iterator(list(itt2.1))),
             mapreduce=list(agg1,agg2),
             run=list(list(ok=1),list(ns="test.iris",size=58650,count=450)),
             databases=list(dbs),
             collections=list(cols1,cols2))
}

testthat::test_that("fake_mongo",{
  fm <- make_fake()
  expect_no_condition(mdbDisconnect(fm))
  expect_no_condition(mdbDrop(fm))
  expect_no_condition(mdbExport(fm,temfile()))
  expect_no_condition(mdbImport(fm,tempfile()))
  expect_no_condition(mdbIndex(fm,add=buildJQuery(processed=1)))
  expect_no_condition(mdbInsert(fm, iris))
  expect_no_condition(mdbRemove(fm,buildJQuery(uid="foo")))
  expect_no_condition(mdbRename(fm,"newname"))
  expect_no_condition(mdbReplace(fm,buildJQuery(uid="Phred"),as.json(fred1)))
})

testthat::test_that("fake_mongo que",{
  fm <- make_fake()
  queues <- c("aggregate", "count", "distinct", "find", "iterate", "mapreduce", "run",
  "collections")
  for (qq in queues) {
    qqq <- fm$que(qq)
    expect(is(qqq,"iterator") && length(qqq$elements)==2L,
           sprintf("Bad value for %s iterator.", qq))
  }
  qqq <- fm$que("databases")
  expect(is(qqq,"iterator") & length(qqq$elements)==1L,
         "Bad value of databases iterator.")
})

testthat::test_that("fake_mongo reset",{
  fm <- make_fake()
  expect_equal(mdbCount(fm),10L)
  expect_equal(mdbCount(fm,'{"name"="James Goodfellow"}'),150L)
    expect_true(is.na(mdbCount(fm)))
  fm$resetQue("count")
  expect_equal(mdbCount(fm),10L)
  fm$resetQue("count",list(20L,25L))
  expect_equal(mdbCount(fm),20L)
  fm$resetAll()
  expect_equal(mdbCount(fm),20L)
})

testthat::test_that("fake_mongo reset iterators.",{
  fm <- make_fake()
  it1 <- mdbIterate(fm)
  expect_equal(it1$position,0L)
  expect_equal(it1$one(),as.jlist(fred1,attributes(fred1)))
  expect_equal(it1$position,1L)
  fm$resetQue("iterate")
  it2 <- mdbIterate(fm)
  expect_equal(it2$position,0L)
  expect_equal(it2$one(),as.jlist(fred1,attributes(fred1)))
  expect_equal(it2$position,1L)
  fm$resetAll()
  it3 <- mdbIterate(fm)
  expect_equal(it3$position,0L)
  expect_equal(it3$one(),as.jlist(fred1,attributes(fred1)))
  expect_equal(it3$position,1L)
})


testthat::test_that("fake_mongo  mdbAggregate",{
  fm <- make_fake()
  expect_equal(mdbAggregate(fm),agg1)
  expect_equal(mdbAggregate(fm,paste('[{"$group":{"_id":"$Species", "count": {"$sum":1},',
                            '"average_Petal_Length": {"$avg":"$Petal_Length"}',
                            '}}]')),agg2)
  expect_null(mdbAggregate(fm))
  fm$resetQue("aggregate")
  expect_equal(mdbAggregate(fm),agg1)

})

testthat::test_that("fake_mongo  mdbCount",{
  fm <- make_fake()
  expect_equal(mdbCount(fm),10L)
  expect_equal(mdbCount(fm,'{"name"="James Goodfellow"}'),150L)
  expect_true(is.na(mdbCount(fm)))
  fm$resetQue("count")
  expect_equal(mdbCount(fm),10L)
})

testthat::test_that("fake_mongo  mdbDistinct",{
  fm <- make_fake()
  expect_equal(mdbDistinct(fm),c("red","blue","green"))
  expect_equal(mdbDistinct(fm,'{"name"="James Goodfellow"}'),character())
  expect_true(is.na(mdbDistinct(fm)))
  fm$resetQue("distinct")
  expect_equal(mdbDistinct(fm),c("red","blue","green"))
})

testthat::test_that("fake_mongo  mdbFind",{
  fm <- make_fake()
  expect_equal(mdbFind(fm),iris)
  expect_equal(mdbFind(fm,'{"Species"="virginica"}'),iris[101:150,])
  expect_equal(mdbFind(fm),data.frame())
  fm$resetQue("find")
  expect_equal(mdbFind(fm),iris)
})
testthat::test_that("fake_mongo  mdbIterate",{
  fm <- make_fake()
  itta <- mdbIterate(fm)
  expect_true(itta$one()$processed)
  expect_false(itta$one()$processed)
  expect_null(itta$one())
  ittb <- mdbIterate(fm,'{"uid"="Phred"}')
  expect_equal(ittb$one()$uid,unboxer("Phred"))
  expect_null(mdbIterate(fm))
  fm$resetQue("iterate")
  ittc <- mdbIterate(fm)
  expect_equal(ittc$one()$uid,unboxer("Fred"))
})

testthat::test_that("fake_mongo  mdbMapreduce",{
  fm <- make_fake()
  expect_equal(mdbMapreduce(fm),agg1)
  expect_equal(mdbMapreduce(fm,
                            map= "function (){emit(Math.floor(this.Petal_Length*5)/5, 1)}",
                            reduce="function (id,counts){return Array.sum(counts)}"),
              agg2)
  expect_true(is.na(mdbMapreduce(fm)))
  fm$resetQue("mapreduce")
  expect_equal(mdbMapreduce(fm),agg1)
})

testthat::test_that("fake_mongo  mdbRun",{
  fm <- make_fake()
  expect_equal(mdbRun(fm),list(ok=1))
  expect_equal(mdbRun(fm,'{"name"="James Goodfellow"}'),list(ns="test.iris",size=58650,count=450))
  expect_true(is.na(mdbRun(fm)))
  fm$resetQue("run")
  expect_equal(mdbRun(fm),list(ok=1))
})
testthat::test_that("fake_mongo  showCollections",{
  fm <- make_fake()
  expect_equal(showCollections(fm),cols1)
  expect_equal(showCollections(fm,dbname="Records"),cols2)
  expect_equal(showCollections(fm),data.frame())
  fm$resetQue("collections")
  expect_equal(showCollections(fm),cols1)

})
testthat::test_that("fake_mongo  showDatabases",{
  fm <- make_fake()
  expect_equal(showDatabases(fm),dbs)
  expect_equal(showDatabases(fm),data.frame())
  fm$resetQue("databases")
  expect_equal(showDatabases(fm),dbs)
})




