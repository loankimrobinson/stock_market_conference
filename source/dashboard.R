

output$trending_table <- renderReactable({
  req(trend_tickes())
  dt <- trend_tickes()
  dt$`Last Price` <- as.numeric(dt$`Last Price`)
  dt <- dt %>% arrange(desc(`Last Price`))
  tb <- reactable(dt,
                  searchable = FALSE,
                  fullWidth = TRUE,
                  defaultPageSize = 10,
                  defaultColDef = colDef(
                    headerStyle = list(background = '#FFFFFF', color = '#012c70', align= "left")
                  ),
                  theme = reactableTheme(
                    borderColor = "#dfe2e5",
                    stripedColor = "#f6f8fa",
                    highlightColor = "#f0f5f9",
                    cellPadding = "7px 10px 7px 30px", #top, right, bottom, left
                    style = list(
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Symbol = colDef(
                      style = list(color = "#012c70",fontSize = 15)
                    ),
                    Name = colDef(
                      minWidth = 250,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13)
                    ),
                    `Last Price` = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                     bar_height = 8,
                                     text_position =  'outside-base',
                                     number_fmt = scales::comma_format(accuracy = 0.1),
                                     fill_color_ref = "color",
                                     box_shadow = TRUE,
                                     round_edges = TRUE, 
                                     background = "transparent")
                    ),
                    Change = colDef(
                      style = function(value,index) {
                        if(value <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    ),
                    `% Change` = colDef(
                      style = function(value,index) {
                        if(dt$Change[index] <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )
})



output$currences_table <- renderReactable({
  req(currences())
  dt <- currences()
  dt$`Last Price` <- as.numeric(dt$`Last Price`)
  dt <- dt %>% arrange(desc(`Last Price`))
  tb <- reactable(dt,
                  searchable = FALSE,
                  fullWidth = TRUE,
                  defaultPageSize = 10,
                  defaultColDef = colDef(
                    headerStyle = list(background = '#FFFFFF', color = '#012c70', align= "left")
                  ),
                  theme = reactableTheme(
                    borderColor = "#dfe2e5",
                    stripedColor = "#f6f8fa",
                    highlightColor = "#f0f5f9",
                    cellPadding = "7px 10px 7px 30px", #top, right, bottom, left
                    style = list(
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Symbol = colDef(
                      minWidth = 150,
                      style = list(color = "#012c70",fontSize = 15)
                    ),
                    Name = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13)
                    ),
                    `Last Price` = colDef(
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                       bar_height = 8,
                                       text_position =  'outside-base',
                                       number_fmt = scales::comma_format(accuracy = 0.1),
                                       fill_color_ref = "color",
                                       box_shadow = TRUE,
                                       round_edges = TRUE, 
                                       background = "transparent")
                    ),
                    Change = colDef(
                      style = function(value,index) {
                        if(value <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    ),
                    `% Change` = colDef(
                      style = function(value,index) {
                        if(dt$Change[index] <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )
  values$currences_table <- tb
})

output$dl_currences_table_ui <- renderUI({
  dlUI("dl_currences_table")
})


observeEvent(values$currences_table,{
  callModule(dlServer, "dl_currences_table", file=reactive(values$currences_table) ,data = NULL, zip = FALSE)
})

#====================================================


output$sector_table <- renderReactable({
  req(sector())
  dt <- sector()
  dt$Weight <- as.numeric(gsub("%","",dt$Weight))
  dt$Return<- as.numeric(gsub("%","",dt$Return))
  dt$Sector[-1] <- paste0("&nbsp;&nbsp;&nbsp;&nbsp;",dt$Sector[-1])
  tb <- reactable(dt,
                  searchable = FALSE,
                  fullWidth = TRUE,
                 
                  defaultPageSize = nrow(dt),
                  defaultColDef = colDef(
                    headerStyle = list(background = '#FFFFFF', color = '#012c70', align= "left")
                  ),
                  theme = reactableTheme(
                    borderColor = "#dfe2e5",
                    stripedColor = "#f6f8fa",
                    highlightColor = "#f0f5f9",
                    cellPadding = "7px 10px 7px 30px", #top, right, bottom, left
                    style = list(
                      fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                    ),
                    searchInputStyle = list(width = "100%")),
                  columns = list(
                    color = colDef(show = FALSE),
                    Sector= colDef(
                      minWidth = 250,
                      style = list(color = "#012c70",fontSize = 15),
                      html = TRUE
                    ),
                    Weight = colDef(
                      "Market Weight",
                      minWidth = 150,
                      style = list(color = "gray", fontStyle= "italic", fontSize = 13),
                      cell = data_bars(dt,
                                       bar_height = 8,
                                       text_position =  'outside-base',
                                       number_fmt = scales::comma_format(accuracy = 0.1,suffix = '%'),
                                       fill_color_ref = "color",
                                       box_shadow = TRUE,
                                       round_edges = TRUE, 
                                       background = "transparent")
                    ),
                    Return = colDef(
                      "YTD Return",
                      minWidth = 150,
                      style = function(value,index) {
                        if(value <0 ) {
                          color = "#c81c43"
                        }else{
                          color = "#012c70"
                        }
                        list(color = color, fontWeight = "bold")
                      }
                    )
                  )
  )

})

#=============================================

output$sector_plot <- renderPlotly({
  dt <- sector()
  dt <- dt[-1,]
  dt$Weight <- as.numeric(gsub("%","",dt$Weight))
  dt$parent <- ""
  dt$label <- paste0(dt$Sector, "<br> Market Weight: ",
                     dt$Weight, "%", "<br>YTD Return: ", dt$Return)
  colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(14)
  colr <- rev(colors[1:length(unique(dt$Sector))])
  red <- dt[as.numeric(gsub("%","",dt$Return)) <0, ]
  
  colors = setNames(colr, unique(dt$Sector))
  colors[names(colors) %in% red$Sector] <- "#c81c43"

  p <- plotly::plot_ly(type = "treemap",
                  data = dt,
                  labels = ~label,
                  parents = ~parent,
                  ids = ~Sector,
                  values = ~Weight,
                  hoverinfo = "text",
                  hovertemplate = "<b>%{label}</b><br><extra></extra>",
                  textposition = "inside",
                  insidetextanchor = "middle",
                  marker = list(colors = colors),
                  textfont = list(size = 15)) %>%
   plotly::layout(
      uniformtext = list(minsize = 10),
      showlegend = TRUE,
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)',
      margin = list(l = 0, r = 0, b = 0, t = 0)
    ) %>% plotly::config(displayModeBar = FALSE)
  
  values$sector_plot <- p
})

output$dl_sector_plot_ui <- renderUI({
  dlUI("dl_sector_plot")
})

observeEvent(values$sector_plot,{
  callModule(dlServer, "dl_sector_plot", file=reactive(values$sector_plot) ,data = NULL, zip = FALSE)
})

#========================================



