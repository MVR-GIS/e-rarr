# Get risks
erisk_item <- read_csv(system.file("app", "data", "RISKLIST_FULL.csv",
                                   package = "erarr"), 
                       show_col_types = FALSE)
risk_item_db <- data.frame(erisk_item)
# Get projects
erisk_project <- read_csv(system.file("app", "data", "PROJECTLIST_FULL.csv",
                                      package = "erarr"),
                          show_col_types = FALSE)
risk_project_db <- data.frame(erisk_project)
# Join risk and projects
erisk_ItemProj <- left_join(risk_item_db, risk_project_db, 
                            by = "PROJECT_ID")
# Select only needed fields
riskdf <- erisk_ItemProj |>
  select("P2_NUMBER.x", "RISK_IDENTIFIER", "PROJECT_NAME.x", "RISK_NAME",
         "RISKCATEGORY", "DISCIPLINE", "USACE_ORGANIZATION.x", 
         "COST_RANK_DESC", "SCHEDULE_RANK_DESC", "PERFORMANCE_RANK_DESC")
# Calculate pie stats
cost_pie_df <- pieprep(riskdf = riskdf, "COST_RANK_DESC")
schedule_pie_df <- pieprep(riskdf = riskdf, "SCHEDULE_RANK_DESC")
performance_pie_df <- pieprep(riskdf = riskdf, "PERFORMANCE_RANK_DESC")


# Temp workaround print function for htmlwidgets not displaying
print.htmlwidget <- function(widget){
  temp_file <- paste(tempfile('widget'), 'html', sep = '.')
  htmlwidgets::saveWidget(widget, temp_file, selfcontained = FALSE)
  shell(sprintf("start chrome -app=file://%s", temp_file))
}

pie_plots <- pie_plots(cost_pie_df, schedule_pie_df, performance_pie_df)
#print(pie_plots)

test_that("returns a plotly object", {
  expect_true("plotly" %in% class(pie_plots))
})
