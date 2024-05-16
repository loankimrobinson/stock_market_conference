dashboardHeader(
  tags$li(class = "dropdown",
          tags$style(".main-header {max-height: 38px}"),
          tags$style(".main-header .logo {height: 38px}")),
  title = div(span(img(src = "stock_header.png", height = 40, width = "20%"), 
                   "Stock Market Forecasting"),align = "left", width = "100%", 
              style = "padding-right:0px;"),
  titleWidth = 310)