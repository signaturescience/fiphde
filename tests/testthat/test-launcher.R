load(system.file("testdata/testdata.rd", package="fiphde", mustWork=TRUE))

test_that("FIPHDE launcher app submission dir file convention works", {

  ## create a tempdir for testing
  broken_launcher_dir <- file.path(tempdir(), "test-launcher")
  dir.create(broken_launcher_dir)

  ## copy one of the files from extdata to the testing dir
  ## deliberately name *without* .candidate.csv suffix
  file.copy(from = list.files(system.file("extdata", "submission-example", "SigSci-CREG", package = "fiphde"), recursive = TRUE, full.names = TRUE)[1],
            to = paste0(broken_launcher_dir, "/submission.csv"))

  ## expect error because there are no .candidate.csv files
  ## NOTE: using prepped_hosp from testdata/testdata.rd
  expect_error({
    fiphde_launcher(.data = prepped_hosp,
                  submission_dir = broken_launcher_dir,
                  host = "0.0.0.0",
                  launch.browser = TRUE,
                  port = 80)
  })

  ## clean up
  unlink(broken_launcher_dir)

})
