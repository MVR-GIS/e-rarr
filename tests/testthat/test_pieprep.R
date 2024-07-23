# Get risks
erisk_item <- read_csv(system.file("app", "data", "erisk_item.csv",
                                   package = "erarr"), 
                       show_col_types = FALSE)
risk_item_db <- data.frame(erisk_item)
# Get projects
erisk_project <- read_csv(system.file("app", "data", "erisk_project.csv",
                                      package = "erarr"),
                          show_col_types = FALSE)
risk_project_db <- data.frame(erisk_project)
# Join risk and projects
erisk_ItemProj <- left_join(risk_item_db, risk_project_db, 
                            by = "PROJECT_ID")

riskdf <- erisk_ItemProj |>
  select("P2_NUMBER.x", "RISK_IDENTIFIER", "PROJECT_NAME.x", "RISK_NAME",
         "RISKCATEGORY", "DISCIPLINE", "USACE_ORGANIZATION.x", 
         "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")


test_that("returns a data frame", {
  rankcol <- "COST_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_true(is.data.frame(pieprep_df))
})

test_that("check for the correct number of Cost records", {
  rankcol <- "COST_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_equal(length(pieprep_df$COST_RANK_DESC), 4)
})

test_that("check for named rank col field", {
  rankcol <- "COST_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_true(rankcol %in% colnames(pieprep_df))
})

test_that("check for count field", {
  rankcol <- "COST_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_true("count" %in% colnames(pieprep_df))
})

test_that("check for color field", {
  rankcol <- "COST_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_true("color" %in% colnames(pieprep_df))
})

test_that("check for the correct number of Schedule records", {
  rankcol <- "SCHEDULE_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_equal(length(pieprep_df$SCHEDULE_RANK_DESC), 4)
})

test_that("check for the correct number of records", {
  rankcol <- "PERFORMANCE_RANK_DESC"
  pieprep_df <- pieprep(riskdf = riskdf, rankcol)
  expect_equal(length(pieprep_df$PERFORMANCE_RANK_DESC), 4)
})
