test_that("spelling errors", {
  skip_on_appveyor() ## no suitable spellchecker installed
  skip_on_travis()   ## no suitable spellchecker installed
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("hunspell")

  pkg <- "NetLogoR"
  pkgDir <- system.file(package = pkg)
  wordsFile <- file.path(".aspell", paste0(pkg, ".rds"))

  if (interactive()) {
    ## add custom words to package dictionary
    words <- c("NetLogoR", "SpaDES")
    saveRDS(words, wordsFile)
  }

  ## check vignettes
  wrdsRmd <- aspell_package_vignettes(pkgDir)
  expect_equal(nrow(wrdsRmd), 0)

  ## check help (Rd) files
  wrdsRd <- aspell_package_Rd_files(pkgDir, drop = c("\\author", "\\references"))
  expect_equal(nrow(wrdsRd), 0)

  ## check code files (messages, warnings, etc.)
  wrdsC <- aspell_package_C_files(pkgDir)
  expect_equal(nrow(wrdsC), 0)
})
