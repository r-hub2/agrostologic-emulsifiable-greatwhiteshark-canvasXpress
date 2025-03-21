context("canvasXpress Charts - Layout")


test_that("segregation layout change - Samples", {
    tryCatch({
        y <- read.table("https://www.canvasxpress.org/data/r/cX-generic-dat.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        x <- read.table("https://www.canvasxpress.org/data/r/cX-generic-smp.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        z <- read.table("https://www.canvasxpress.org/data/r/cX-generic-var.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    },
    error = function(e) {
        skip('Unable to read data files')
    })

    result <- canvasXpress(data               = y,
                           smpAnnot           = x,
                           varAnnot           = z,
                           graphType          = "Bar",
                           title              = "Layout 1X3",
                           segregateSamplesBy = list("Factor1"),
                           layoutTopology     = "1X3")

    check_ui_test(result)
})

test_that("segregation layout change - Variables", {
    tryCatch({
        y <- read.table("https://www.canvasxpress.org/data/r/cX-generic-dat.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        x <- read.table("https://www.canvasxpress.org/data/r/cX-generic-smp.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        z <- read.table("https://www.canvasxpress.org/data/r/cX-generic-var.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    },
    error = function(e) {
        skip('Unable to read data files')
    })

    result <- canvasXpress(data                 = y,
                           smpAnnot             = x,
                           varAnnot             = z,
                           graphType            = "Bar",
                           legendPosition       = "top",
                           legendColumns        = 4,
                           title                = "Layout 4X1",
                           segregateVariablesBy = list("Annt1"),
                           layoutTopology       = "4X1")

    check_ui_test(result)
})

test_that("segregation updated layout change", {
    tryCatch({
        y <- read.table("https://www.canvasxpress.org/data/r/cX-generic-dat.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        x <- read.table("https://www.canvasxpress.org/data/r/cX-generic-smp.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
        z <- read.table("https://www.canvasxpress.org/data/r/cX-generic-var.txt", header = TRUE, sep = "\t", quote = "", row.names = 1, fill = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    },
    error = function(e) {
        skip('Unable to read data files')
    })

    result <- canvasXpress(data               = y,
                           smpAnnot           = x,
                           varAnnot           = z,
                           graphType          = "Bar",
                           title              = "Change Segregation Data property to 'Variable' and check that layout is updated",
                           segregateSamplesBy = list("Factor1"),
                           layoutTopology     = "1X3")

    check_ui_test(result)
})
