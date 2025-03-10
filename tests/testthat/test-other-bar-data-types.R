context("canvasXpress Charts - Bar")


test_that("bar as data list items", {
    tryCatch({
        x <- read.table("https://www.canvasxpress.org/data/r/cX-generic-smp.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        y <- read.table("https://www.canvasxpress.org/data/r/cX-generic-dat.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        z <- read.table("https://www.canvasxpress.org/data/r/cX-generic-var.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    },
    error = function(e) {
        skip('Unable to read data files')
    })

    skip_if_not(exists('x') && exists('y') && exists('z'))

    expect_error(canvasXpress(data = list(), smpAnnot = x, varAnnot = z, graphType = "Bar"),
                 regexp = "data specified as a list must contain at least one item")
    expect_error(canvasXpress(data = list(y, z, x), smpAnnot = x, varAnnot = z, graphType = "Bar"),
                 regexp = "data specified as a list of multiple items must have named elements")
    expect_error(canvasXpress(data = list(a = y, b = x), smpAnnot = x, varAnnot = z, graphType = "Bar"),
                 regexp = "data specified as a list of multiple items must contain a <y> element")
    expect_error(canvasXpress(data = list(y = 1), smpAnnot = x, varAnnot = z, graphType = "Bar"),
                 regexp = "data list elements <y> are not data.frame or matrix elements")

    expect_silent(canvasXpress(data = list(y = y), smpAnnot = x, varAnnot = z, graphType = "Bar"))
    expect_silent(canvasXpress(data = list(y), smpAnnot = x, varAnnot = z, graphType = "Bar"))
    expect_silent(canvasXpress(data = list(y = y, a = x), smpAnnot = x, varAnnot = z, graphType = "Bar"))
})
