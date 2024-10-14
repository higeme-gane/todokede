library(conflicted)
library(shiny)
library(tidyverse)

# 実行日の取得
execution_date <- Sys.Date()
execution_date <- year(execution_date) * 10000 +
  month(execution_date) * 100 +
  day(execution_date)

# データの読み込み
file_name <- list.files(path = getwd(),
                        pattern = "^df_select.*\\.rds$",
                        full.names = FALSE)
file_name <- sort(file_name, decreasing = TRUE)
file_name <- file_name[1]
df_select <- readRDS(file_name)

# Shinyアプリケーションの定義
ui <- fluidPage(
  titlePanel("データ検索とダウンロード"),
  sidebarLayout(
    sidebarPanel(
      textInput("search", "検索キーワード", value = ""),
      actionButton("search_btn", "検索"),
      br(),
      selectInput("choice", "選択", choices = NULL),
      br(),
      downloadButton("download", "ダウンロード")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # 検索ボタンが押された時の処理
  observeEvent(input$search_btn, {
    vec_word <- input$search
    df_distinct <- distinct(.data = df_select, name, .keep_all = FALSE)
    df_distinct_filter <- dplyr::filter(.data = df_distinct, str_detect(name, vec_word))
    updateSelectInput(session, "choice", "選択", choices = df_distinct_filter$name)
  })

  output$table <- renderTable({
    req(input$choice)
    vec_choice <- input$choice
    df_filter <- dplyr::filter(.data = df_select, name == vec_choice) |> 
                 mutate(start_date = format(start_date, "%Y-%m-%d"))
  }, rownames = FALSE, colnames = TRUE)
  
  # ダウンロードボタンが押された時の処理
  output$download <- downloadHandler(
    filename = function() {
      str_c(input$choice, execution_date, ".csv")
    },
    content = function(file) {
      vec_choice <- input$choice
      df_filter <- dplyr::filter(.data = df_select, name == vec_choice)
      write_excel_csv(df_filter, file)
    }
  )
}

# アプリケーションの実行
shinyApp(ui, server)