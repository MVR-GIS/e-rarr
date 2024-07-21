test_that("project all risks report", {
  projID = "St. Augustine Back Bay CSRM" 
  p2ID   = ""
  p2sub  = ""
  report <- file.path(system.file("app", "rmd", package = "erarr"), 
                      "ProjectAllRiskReport.Rmd")
  expected_report = paste0(tempfile(pattern = "test_all_risks_"), ".html")
  if (file.exists(expected_report)) file.remove(expected_report)
  rmarkdown::render(input = report,
                    output_file = expected_report,
                    params = list(projID = projID, 
                                  p2ID   = p2ID,
                                  p2sub  = p2sub),
                    envir = new.env(),
                    intermediates_dir = tempdir())
  #shell(output_file)
  expect_true(file.exists(expected_report))
})

test_that("project all risks report", {
  projID = "" 
  p2ID   = 505374
  p2sub  = ""
  report <- file.path(system.file("app", "rmd", package = "erarr"), 
                      "ProjectAllRiskReport.Rmd")
  expected_report = paste0(tempfile(pattern = "test_all_risks_"), ".html")
  if (file.exists(expected_report)) file.remove(expected_report)
  rmarkdown::render(input = report,
                    output_file = expected_report,
                    params = list(projID = projID, 
                                  p2ID   = p2ID,
                                  p2sub  = p2sub),
                    envir = new.env(),
                    intermediates_dir = tempdir())
  #shell(output_file)
  expect_true(file.exists(expected_report))
})
