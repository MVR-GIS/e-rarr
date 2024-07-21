test_that("project risk item report", {
  riskID = "ECV-9 Changes in Engineering Modeling Scope"
  projID = "St. Augustine Back Bay CSRM" 
  p2ID   = ""
  report <- file.path(system.file("app", "rmd", package = "erarr"), 
                      "RiskItemReport.Rmd")
  expected_report = paste0(tempfile(pattern = "test_risk_item_"), ".html")
  if (file.exists(expected_report)) file.remove(expected_report)
  rmarkdown::render(input = report,
                    output_file = expected_report,
                    params = list(riskID = riskID,
                                  projID = projID, 
                                  p2ID   = p2ID),
                    envir = new.env(),
                    intermediates_dir = tempdir())
  #shell(output_file)
  expect_true(file.exists(expected_report))
})
