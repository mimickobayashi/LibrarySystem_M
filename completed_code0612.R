library(shiny)
library(DT)

# UI部分の定義
ui <- fluidPage(
  titlePanel("心理統計ゼミ図書管理システム"),
  navbarPage(
    "",
    
    tabPanel(
      "図書一覧",
      sidebarLayout(
        sidebarPanel(
          selectInput("sortInput", "Sort by",
                      choices = c("Date Added (Oldest First)", "Title (A-Z)"),
                      selected = "Date Added (Oldest First")
        ),
        mainPanel(
          DT::dataTableOutput("bookTable")
        )
      )
    ),
    tabPanel(
      "本の追加と削除",
      sidebarLayout(
        sidebarPanel(
          textInput("titleInput", "Title"),
          textInput("authorsInput", "Authors"),
          numericInput("ISBN", "ISBN Number", value = NULL),
          actionButton("addButton", "Add Book"),
          textInput("deleteInput", "Enter Title, Author, or ISBN to Delete"),
          actionButton("deleteButton", "Delete Book")
        ),
        mainPanel(
          DT::dataTableOutput("bookListTable")
        )
      )
    ),
    tabPanel(
      "編集",
      DT::dataTableOutput("editTable")
    )
  )
)

# サーバー部分の定義
server <- function(input, output, session) {
  # 初期のデータフレーム
  initialBook <- data.frame(
    title = character(),
    authors = character(),
    ISBN = character(),
    dateAdded = character(),
    stringsAsFactors = FALSE
  )
  
  # データフレームの作成
  book <- reactiveVal(initialBook)
  
  # 書籍の追加
  observeEvent(input$addButton, {
    newBook <- data.frame(
      title = input$titleInput,
      authors = input$authorsInput,
      ISBN = ifelse(is.null(input$ISBN), "", input$ISBN),
      dateAdded = format(Sys.Date(), "%Y-%m-%d"),
      stringsAsFactors = FALSE
    )
    bookData <- rbind(book(), newBook)
    book(bookData)
  })
  
  # 書籍の削除
  observeEvent(input$deleteButton, {
    req(input$deleteButton, input$deleteInput)
    showModal(modalDialog(
      title = "Confirm Deletion",
      "Are you sure you want to delete the book?",
      footer = tagList(
        actionButton("confirmDelete", "Yes", class = "btn-primary"),
        modalButton("No")
      )
    ))
  })
  
  # 削除の確定（Yesボタン）
  observeEvent(input$confirmDelete, {
    removeModal()
    bookData <- book()
    bookData <- bookData[!(bookData$title %in% input$deleteInput |
                             bookData$ISBN %in% input$deleteInput), ]
    book(bookData)
  })
  
  # 書籍の編集
  observeEvent(input$editTable_cell_edit, {
    req(input$editTable_cell_edit)
    info <- input$editTable_cell_edit
    row <- info$row
    col <- info$col
    value <- info$value
    
    bookData <- book()
    bookData[row, col] <- value
    book(bookData)
  })
  
  # 書籍の検索
  output$searchResult <- renderPrint({
    req(input$searchButton, input$searchInput)
    searchResult <- book()[grepl(input$searchInput, book()$title, ignore.case = TRUE) |
                             book()$ISBN == input$searchInput, ]
    if (nrow(searchResult) > 0) {
      searchResult
    } else {
      "No matching books found."
    }
  })
  
  # データテーブルの出力（タイトル順または追加時期順にソート）
  output$bookTable <- DT::renderDataTable({
    bookData <- book()
    if (input$sortInput == "Date Added (Oldest First)") {
      bookData <- bookData[order(as.Date(bookData$dateAdded)), ]
    } else if (input$sortInput == "Title (A-Z)") {
      bookData <- bookData[order(bookData$title), ]
    }
    DT::datatable(
      bookData,
      options = list(
        rownames = FALSE
      )
    )
  })
  
  # データテーブルの出力（本の一覧）
  output$bookListTable <- DT::renderDataTable({
    book()
  })
  
  # データテーブルの出力（編集用）
  output$editTable <- DT::renderDataTable({
    bookData <- book()
    DT::datatable(
      bookData,
      selection = "single",
      editable = list(
        target = "cell",
        disable = list(
          columns = c(1, 3, 4)
        )
      ),
      options = list(
        rownames = FALSE,
        dom = "t",
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).on('click', 'tr', function() {",
          "Shiny.setInputValue('editRow', table.row(this).index());",
          "});",
          "}")
      )
    )
  })
  
  # 変更の確定
  observeEvent(input$confirmButton, {
    showModal(modalDialog(
      title = "Confirm Changes",
      "Are you sure you want to confirm the changes?",
      footer = tagList(
        actionButton("confirmYes", "Yes", class = "btn-primary"),
        modalButton("No")
      )
    ))
  })
  
  # 変更の確定（Yesボタン）
  observeEvent(input$confirmYes, {
    removeModal()
  })
  
  # テーブル行のクリックイベント
  observeEvent(input$editRow, {
    if (!is.null(input$editRow)) {
      showModal(modalDialog(
        title = "Edit Book",
        textInput("editTitle", "Title", value = book()$title[input$editRow]),
        textInput("editAuthors", "Authors", value = book()$authors[input$editRow]),
        numericInput("editISBN", "ISBN Number", value = book()$ISBN[input$editRow]),
        footer = tagList(
          actionButton("confirmButton", "Confirm", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
    }
  })
}

# Shinyアプリの起動
shinyApp(ui = ui, server = server)
