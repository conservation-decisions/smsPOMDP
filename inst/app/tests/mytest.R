app <- ShinyDriver$new("../", shinyOptions = list(test.mode = TRUE))
app$snapshotInit("mytest")

app$snapshot()
