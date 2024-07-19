#' @title Pie Plots
#'
#' @description Creates a figure of three pie charts for the specified 
#' data frames of Cost, Schedule, and Performance risks according to risk 
#' category ("High", "Medium", "Low", "Opportunity"). 
#'
#' @param cost_pie     data frame; A data frame of filtered cost risks 
#' @param schedule_pie data frame; 
#' @param perform_pie  data frame;
#' 
#' @return A plotly plot containing 3 pie charts of project risk.
#'
#' @importFrom rlang .data
#' @importFrom plotly plot_ly add_pie layout
#' @export
#'
pie_plots<- function(cost_pie, schedule_pie, perform_pie){
  fig <- plotly::plot_ly(textfont = list(color = '#FFFFFF')) |>
    plotly::add_pie(
      data = cost_pie,
      values =  ~ .data$count,
      labels = ~ .data$COST_RANK_DESC,
<<<<<<< HEAD
      title=list(text='<b> Cost <br> </b>', font = list(size = 15)),
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
      sort=FALSE,
      textfont = list(color = '#FFFFFF', size=15),
=======
      title = list(text = '<b> Cost <br> </b>', font = list(size = 15)),
      name = " ",
      textinfo = 'value',
      sort = FALSE,
      textfont = list(color = '#FFFFFF', size = 15),
>>>>>>> 093369693203a1fd0b141aa8c8c587fd08eddefd
      domain = list(row = 0, column = 0),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5))
    ) |>
    plotly::add_pie(
      data = schedule_pie,
      values = ~ .data$count,
      labels = ~ .data$SCHEDULE_RANK_DESC,
      textfont = list(color = '#FFFFFF',size=15),
      sort = FALSE,
<<<<<<< HEAD
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
      title =list(text='<b> Schedule <br> </b>', font = list(size = 15)),
=======
      name = " ",
      textinfo = 'value',
      title = list(text = '<b> Schedule <br> </b>', font = list(size = 15)),
>>>>>>> 093369693203a1fd0b141aa8c8c587fd08eddefd
      domain = list(row = 0, column = 1),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5))
    ) |>
    plotly::add_pie(
      data = perform_pie,
      values = ~ .data$count,
      labels = ~ .data$PERFORMANCE_RANK_DESC,
<<<<<<< HEAD
      title = list(text='<b> Performance <br> </b>', font = list(size = 15)),
      name= " ",
      textinfo='value',
      hoverinfo = 'label+percent',
=======
      title = list(text = '<b> Performance <br> </b>', font = list(size = 15)),
      name = " ",
      textinfo = 'value',
>>>>>>> 093369693203a1fd0b141aa8c8c587fd08eddefd
      textfont = list(color = '#FFFFFF', size=15),
      sort = FALSE,
      domain = list(row = 0, column = 2),
      marker = list(
        colors = ~ color,
        line = list(color = '#FFFFFF', width = 1.5))
    )

  pies <- fig |>
    plotly::layout(title = "", showlegend = T, 
                   margin = list(l = 20, r = 20, b = 20, t = 20, pad = 0),
                   grid = list(rows = 1, columns = 3), 
                   legend = list(orientation = 'h', xanchor = "center", 
                                 x = 0.5, traceorder = "normal", 
                                 font = list(size = 15))
    )

  return(pies)
}
