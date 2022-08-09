source("atlas_functions.R", encoding = "UTF-8")
library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(shinyjs)
library(shinycssloaders)
library(DT)
library(fuzzyjoin)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Search parameteres UI
search_parameters <- tabsetPanel(
  id = "search_method_panel",
  type = "hidden",
  tabPanel("normal"),
  tabPanel("fuzzy",
           sliderInput(inputId = "fuzzy_word_num", value = 0,
                       min = 0, max = 1, step = 1,
                       label = "Max number of not matched word"),
           sliderInput(inputId = "fuzzy_char_num", value = 0,
                       min = 0, max = 1, step = 1,
                       label = "Max number of not matched characther in a word")
  ),
  tabPanel("synonyms")
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "atlas"),
    dashboardSidebar(
        sidebarMenu(shinyFeedback::useShinyFeedback(),
                    useShinyjs(),
                    menuItem("Atlas search engine", 
                             tabName = "main_tab", startExpanded = TRUE
                    ),
                    sidebarSearchForm(textId = "searchtext", buttonId = "searchButton",
                                      label = "Search..."),
                    radioButtons("search_method", "Search method",
                                 c("Normal" = "normal",
                                    "Fuzzy" = "fuzzy",
                                    "Synonyms" = "synonyms")),
                    search_parameters,
                    textInput("corpus_folder", label = "Document folder path" ),
                    actionButton("create_index_dialog", "Create index", 
                                 icon = icon("bezier-curve"),
                                 class = "btn action-button",
                                 style = "color: white;
                                        background-color: #0080FF",
                                 width = "200px"),
                    actionButton("update_index_dialog", "Update index",
                                 icon = icon("sync-alt"),
                                 class = "btn action-button",
                                 style = "color: white;
                                        background-color: #FFB400",
                                 width = "200px"),
                    actionButton("clean_index_dialog", "Clean index",
                                 icon = icon("trash-alt"),
                                 class = "btn action-button",
                                 style = "color: white;
                                        background-color: #A50000",
                                 width = "200px")
                    )
        
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
            .main-header .logo {
            font-family: "Georgia", Times, "Times New Roman", serif;
            font-weight: bold;
            font-size: 36px;
            }'))),
        tags$style(type='text/css', '#text_show {white-space: pre-wrap;}'),
        tabItems(
            tabItem(tabName = "main_tab",
                    box(title = "Dashboard",
                        width = 6,
                        status = "info",
                        htmlOutput("user_session"),
                        h4("Index metrics:"),
                        withSpinner(htmlOutput("index_metrics")),
                        br(), br()
                    ),
                    box(title = "Results",
                        width = 12,
                        status = "success",
                        htmlOutput("query"),
                        htmlOutput("results_number"), br(),
                        htmlOutput("message_result_box"), br(),
                        withSpinner(DT::dataTableOutput("results_table"))
                    ),
                    box(title = "Dashboard",
                        width = 12,
                        status = "info",
                        #withSpinner(htmlOutput("message_console_box")),
                        htmlOutput("message_console_box"), br(),
                        withSpinner(verbatimTextOutput("text_show"))
                    )
            )
        )
    )
)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
server <- function(input, output, session) {

    # FUNCTIONS ---------------------------------------------------------------
    # Buttons function
      custom_button <- function(FUN, len, id, ...) {
        inputs <- character(len)
          for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, i), ...))
          }
          inputs
        }
    # Open files function
      open_file <- function(dir_path, file_path){
        project_path <- getwd()
        on.exit(setwd(project_path))
        
        setwd(dir_path)
        browse(file_path)
      }
    # Read file functions
      cst_read_csv <- function(file_path){
        #if(index) showNotification("Loading index..", duration = 3)
        tryCatch(read_csv(file_path),
                 error = function(err){
                   index_found(FALSE)
                 })
      }
  
    #--------------------------------------------------------------------------
    # CONTROL
    index_found <- reactiveVal(TRUE)
    message_console_box <- reactiveVal("")
    message_result_box <- reactiveVal("")
    text_show <- reactiveVal("")
    #--------------------------------------------------------------------------
    # LOADING DATA
    word_doc_index <- reactiveFileReader (1000,session,"index/word_doc_index.csv", cst_read_csv)
    doc_register <- reactiveFileReader (1000,NULL,"index/doc_register.csv", cst_read_csv)
    word_register <- reactiveFileReader (1000,NULL,"index/word_register.csv", cst_read_csv)
    index_config <- reactiveFileReader (1000,NULL,"index/index_config.csv", cst_read_csv)
    stopwords <- tidytext::stop_words$word
    #--------------------------------------------------------------------------
    index_loader <- reactive(nrow(word_doc_index()))
    output$index_loader <- renderText({paste("<b>Index weigth:</b> ", index_loader())})
    # -------------------------------------------------------------------------
    #DYNAMIC UI
    observeEvent(input$search_method, {
      updateTabsetPanel(session, "search_method_panel", selected = input$search_method)
    })
    observeEvent(input$searchtext,{
      updateSliderInput(session, "fuzzy_word_num", max = query_length() - 1)
      updateSliderInput(session, "fuzzy_char_num", max = query_min_word_length() - 1)
    })
    #--------------------------------------------------------------------------
    # ELABORATE SEARCH RESULTS
    valid_query <- reactive({
      valid <- !str_detect(input$searchtext, "[^a-zA-Z@èéòàì ]")
      #feedbackWarning("searchtext", !valid, "The query is not valid")
      if(valid){
        message_console_box("")
        return(TRUE)
      }else{
        message_console_box("The query is not valid. Plese use only characters")
        return(FALSE)
      }
    })
    query <- reactive({
      req(valid_query() == TRUE)
      query <- data.frame(user_query = input$searchtext) %>%
        unnest_tokens(user_word, user_query, token = "words") %>% 
        mutate(user_word = lemmatize_words(user_word)) %>% 
        filter(!(user_word %in% stopwords))

    })
    query_length <- reactive(nrow(query()))
    query_min_word_length <- reactive(min(nchar(query()[,1])))
   
    
    #Results table
    results <- eventReactive (input$searchButton, {
      query <- query()
      doc_register <- doc_register()
      word_register <- word_register()
      word_doc_index <- word_doc_index()
      results <- switch(input$search_method,
             normal = {
               query_word <- query$user_word
               results <- word_doc_index %>%
                 filter(word %in% query_word) %>%
                 left_join(word_register) %>%
                 left_join(select(doc_register, doc_id, doc_number.word)) %>%
                 mutate(doc_word_prob = doc_word_number/doc_number.word,
                        doc_word_weight = doc_word_prob/corpus_word_prob) %>%
                 group_by(doc_id) %>%
                 summarise(doc_found.word_number = n(),
                           doc_word = paste(word, collapse = ", "),
                           doc_weight = sum(doc_word_weight)) %>%
                 ungroup() %>%
                 filter(doc_found.word_number == query_length()) %>% 
                 left_join(doc_register, by = "doc_id") %>%
                 arrange(-doc_weight) %>%
                 top_n(100) %>%
                 mutate(size = toupper(utils:::format.object_size(size, "auto")),
                        last_access_date = as.character(access_time),
                        last_modification_date = as.character(modification_time),
                        creation_date = as.character(creation_time))
             },
             fuzzy = {
               query_word <- query$user_word
               results <- query %>%
                 rename(word = user_word) %>%
                 stringdist_left_join(word_register, by ="word", method = "dl",
                                      max_dist = input$fuzzy_char_num,
                                      distance_col = "query_distance") %>%
                 rename(user_word = word.x,
                        word = word.y) %>%
                 left_join(word_doc_index) %>%
                 left_join(select(doc_register, doc_id, doc_number.word)) %>%
                 mutate(word_proximity=  1/(query_distance+1),
                        doc_word_prob = doc_word_number/doc_number.word,
                        doc_word_weight = (doc_word_prob/corpus_word_prob)*word_proximity) %>%
                 group_by(doc_id) %>%
                 summarise(doc_found.word_number = n(),
                           doc_word = paste(word, collapse = ", "),
                           doc_weight = sum(doc_word_weight)) %>%
                 ungroup() %>%
                 left_join(doc_register, by = "doc_id") %>%
                 arrange(-doc_weight) %>%
                 top_n(100) %>%
                 filter(doc_found.word_number >= query_length() - input$fuzzy_word_num) %>%
                 mutate(size = toupper(utils:::format.object_size(size, "auto")),
                        last_access_date = as.character(access_time),
                        last_modification_date = as.character(modification_time),
                        creation_date = as.character(creation_time))

             },
             synonyms = {
               query_word <- query %>%
                 rowwise() %>%
                 mutate(words = paste(user_word,
                                      paste(qdap::synonyms(user_word, multiwords = FALSE, return.list = FALSE),
                                            collapse = " "))) %>%
                 unnest_tokens(word, words, token = "words") %>%
                 group_by(user_word) %>%
                 mutate(synonyms_number = n()-1,
                        word_proximity = ifelse(user_word == word, 1, 1/synonyms_number)) %>%
                 select(word, word_proximity)
               
               results <- query_word %>%
                 left_join(word_register) %>%
                 left_join(word_doc_index) %>%
                 left_join(select(doc_register, doc_id, doc_number.word)) %>%
                 mutate(doc_word_prob = doc_word_number/doc_number.word,
                        doc_word_weight = (doc_word_prob/corpus_word_prob)* word_proximity) %>%
                 group_by(doc_id) %>%
                 summarise(doc_found.word_number = n(),
                           doc_word = paste(word, collapse = ", "),
                           doc_weight = sum(doc_word_weight)) %>%
                 ungroup() %>%
                 left_join(doc_register, by = "doc_id") %>%
                 arrange(-doc_weight) %>%
                 top_n(100) %>%
                 rowwise() %>%
                 mutate(size = toupper(utils:::format.object_size(size, "auto")),
                        last_access_date = as.character(access_time),
                        last_modification_date = as.character(modification_time),
                        creation_date = as.character(creation_time))

             })
    })
    results_length <- reactive({
      results_length <- tryCatch(nrow(results()),
                 error = function(x) 0)
    })
    #--------------------------------------------------------------------------
    # GENERATE INTERACTIVE RESULTS TABLE
    # Table logic
    results_interactive <- reactive({
        req(results_length() > 0)
        results <- results()
        results_length <- results_length()
        data <- data.frame(
            results,
            Show = custom_button(FUN = actionButton,
                                 len = results_length,
                                 id = 'show_button_',
                                 label = "Show",
                                 icon = icon("bolt"),
                                 # style = "color: white;
                                 #        background-color: #FF8C00",
                                 onclick = 'Shiny.onInputChange(\"show_button\",  this.id + Math.random())'),
            Open = custom_button(FUN = actionButton,
                                 len = results_length,
                                 id = 'open_button_',
                                 label = "Open",
                                 icon = icon("book-open"),
                                 class = "btn action-button",
                                 style = "color: white;
                                        background-color: #0080FF",
                                 onclick = 'Shiny.onInputChange(\"open_button\",  this.id + Math.random())' ),
            Folder = custom_button(FUN = actionButton,
                                 len = results_length,
                                 id = 'button_',
                                 label = "Open in folder",
                                 icon = icon("folder"),
                                 class = "btn action-button",
                                 style = "color: white;
                                        background-color: #707070",
                                 onclick = 'Shiny.onInputChange(\"open_folder_button\",  this.id + Math.random())' ),
            stringsAsFactors = FALSE,
            row.names = 1:results_length)
        
    })
    observeEvent(input$show_button, {
        tryCatch({
          element <- as.numeric(str_extract(input$show_button, "(?<=_)[0-9]+(?=0)"))
          path <- results_interactive()$doc_path[element]
          name <- results_interactive()$doc_name[element]
          words <- results_interactive()$doc_word[element]
          if(!file.exists(path)){
            stop("File do not exist, please update your index.")
          }
          text <- cst_read_document(path)
          text <- str_sub(text, 1, 10000)
          text <- paste0(name, "\n\n", text)
          message_console_box(paste("<b>Showing:</b> ", name, "<br>",
                                    "<b>Founded words:</b>", words))
          text_show(text)
          },
          error = function(err){
            showNotification("Something went wrong", type = "warning", duration = NULL)
            message_console_box(paste("Error code:", err))
            text_show("")
          })
    })
    observeEvent(input$open_button, {
        tryCatch({
          element <- as.numeric(str_extract(input$open_button, "(?<=_)[0-9]+(?=0)"))
          path <- results_interactive()$doc_path[element]
          words <- results_interactive()$doc_word[element]
          if(!file.exists(path)){
            stop("File do not exist, please update your index.")
          }
          dir_path <- dirname(path)
          file_name <- basename(path)
          open_file(dir_path, file_name)
          message_console_box(paste("<b>Opening:</b> ", file_name,  "<br>",
                                    "<b>Founded words:</b>", words))
        },
        error = function(err){
          showNotification("Something went wrong", type = "warning", duration = NULL)
          message_console_box(paste("Error code:", err))  
        })
    })
    observeEvent(input$open_folder_button, {
      tryCatch({
        element <- as.numeric(str_extract(input$open_folder_button, "(?<=_)[0-9]+(?=0)"))
        dir_path <- dirname(results_interactive()$doc_path[element])
        words <- results_interactive()$doc_word[element]
        if(!dir.exists(dir_path)){
          stop("Directory do not exist, please update your index.")
        }
        browse(dir_path)
        message_console_box(paste("<b>Opening folder:</b> ", dir_path, "<br>",
                                  "<b>Founded words:</b>", words))
      },
      error = function(err){
        showNotification("Something went wrong", type = "warning", duration = NULL)
        message_console_box(paste("Error code:", err))  
      })

    })
    
    #--------------------------------------------------------------------------
    # OUTPUTS
    # User metrics
    output$user_session <- renderText(paste0("<b>Session: </b>", getwd()))
    # Index metrics
    output$index_metrics <- renderText({
      ifelse(index_found(),
             paste0("<b>Documents folder: </b>", index_config()[3,2],"<br />",
                    "<b>Index creation date: </b>", index_config()[1,2], "<br />",
                    "<b>Number of documents found: </b>", index_config()[4,2], "<br />",
                    "<b>Document folder size: </b>", index_config()[6,2], "<br />",
                    "<b>Index data points:</b> ", index_loader()
                    ),
             "Index files non found, please create the index<br />")
    })
    # Query feedback
    output$query <- renderText({
      ifelse(valid_query(),
             paste0("<b>Processed query:</b> ",paste(query()[,1], collapse = ", ")),
             "<b>Processed query:</b> The query is not valid. Plese use only characters"
      )
      })
    output$results_number <- renderText(paste("<b>Number of results:</b>",results_length() ))
    #Table results
    output$results_table <- DT::renderDataTable({
        if (index_found()) {
          results_interactive() %>%
            rename("Name" = doc_name,
                   "Similarity" = doc_weight,
                   "Size" = size,
                   "Last access date" = last_access_date,
                   "Last modification date" = last_modification_date,
                   "Creation date" = creation_date) %>% 
            mutate(Similarity = round(Similarity,2)) %>% 
            select(Name, Similarity, Size, 'Last access date',
                   'Last modification date', 'Creation date',
                   Show, Open, Folder) 
          }
    },
        server = FALSE,
        escape = FALSE,
        selection = 'none',
        options = list( pageLength = 5)
    )
    
    #Show message results
    output$message_result_box <- renderText({
      text <- message_result_box()
    })
    #Show message console
    output$message_console_box <- renderText({
      text <- message_console_box()
    })
    #show text document
    output$text_show<- renderText({
      text <- text_show()
    })
    
    #--------------------------------------------------------------------------
    # INDEX MANAGEMENT 
    #Dialog option
    observeEvent(input$create_index_dialog, {
      showModal(create_index_dialog)
    })
    observeEvent(input$update_index_dialog, {
      showModal(update_index_dialog)
    })
    observeEvent(input$clean_index_dialog, {
      showModal(clean_index_dialog)
    })
    
    create_index_dialog <- modalDialog(
      "Please close all open documents. Create index can require a long time.",
      title = "Index creation",
      footer = tagList(
        actionButton("create_index", "Create index"),
        actionButton("stop_dialog", "Not now", class = "btn btn-danger")
      )
    )
    update_index_dialog <- modalDialog(
      "Please, close all open documents. Update index can require a long time.",
      title = "Index update",
      footer = tagList(
        actionButton("update_index", "Update index"),
        actionButton("stop_dialog", "Not now", class = "btn btn-danger")
      )
    )
    clean_index_dialog <- modalDialog(
      "Are you sure to clean the index? This will delete all index files",
      title = "Index clean",
      footer = tagList(
        actionButton("clean_index", "Clean index"),
        actionButton("stop_dialog", "Not now", class = "btn btn-danger")
      )
    )
    observeEvent(input$stop_dialog, 
                 removeModal()
    )
    #--------------------------------------------------------------------------
    # Index creation
    valid_path <- reactive({
      exist <- dir.exists(input$corpus_folder)
      feedbackWarning("corpus_folder", !exist, "Incorrect document folder path.")
      if(exist){
        return(TRUE)
      }else{
        message_console_box("Incorrect document folder path.")
        return(FALSE)
      }
    })
    # Creation
    observeEvent(input$create_index, {
      removeModal()
      if(valid_path()){
        tryCatch({
          # Initialization
          message_console_box("")
          message_result_box("")
          text_show("")
          
          showNotification("Searching documents", duration = NULL)
          unlink("index", recursive = TRUE, force = FALSE)
          if(dir.exists("index")) stop()
          index_found(FALSE)
          cst_initialize_create(input$corpus_folder)

          #Creating doc register
          cst_create_doc_register()
          
          #Creating index
          progress_bar <- Progress$new(max = corpus_number.doc)
          progress_bar$set(message = paste0("Found: ",  corpus_number.doc, " documents"))
          cst_read_corpus(progress_bar)
          
          showNotification("Indexing documents", duration = NULL)
          cst_index_corpus()
          
          #Create configurations
          cst_create_word_register()
          cst_update_doc_register()
          cst_create_config()
          
          showNotification("Index created",type = "message", duration = NULL)
          index_found(TRUE)
        },
        error = function(err){
          showNotification("Something went wrong", type = "warning", duration = NULL)
          message_console_box(paste("Error code:", err))  
        })
        

      }
    })
    # Update
    observeEvent(input$update_index,{
      removeModal()
      if(index_found()){
        tryCatch({
          message_console_box("")
          message_result_box("")
          text_show("")
          
          corpus_folder <- index_config()[[3,2]]
          if(!dir.exists(corpus_folder)) stop("Document folder no longer exists")
          
          cst_initialiaze_update()
          
          #Creating doc register
          showNotification("Searching documents", duration = NULL)
          cst_create_doc_register()
          cst_prepare_update_word_doc_index()
          
          #Updating index
          progress_bar <- Progress$new(max = corpus_number.doc.pending)
          progress_bar$set(message = paste0("Found: ",  corpus_number.doc.pending, " new  documents"))
          cst_read_corpus(progress_bar)
          
          showNotification("Indexing documents", duration = NULL)
          cst_index_corpus()

          #Create configurations
          cst_conclude_update_word_doc_index()
          cst_create_word_register()
          cst_update_doc_register()
          cst_create_config()
          
          showNotification("Index updated",type = "message", duration = NULL)
          
        },
        error = function(err){
          showNotification("Something went wrong", type = "warning", duration = NULL)
          message_console_box(paste("Error code:", err))  
        })
      }else{
        message_console_box("Index not found, please create an index first.")
      }
    })
    # Clean index
    observeEvent(input$clean_index, {
      removeModal()
        tryCatch({
          showNotification("Star cleaning index", duration = NULL)
          unlink("index", recursive = TRUE, force = FALSE)
          index_found(FALSE)
          message_console_box("")
          message_result_box("")
          text_show("")
          if(dir.exists("index")) stop()
          showNotification("Index removed", type = "error", duration = NULL)
          },
          error = function(err){
            showNotification("Something went wrong", type = "warning", duration = NULL)
            message_console_box(paste("Error code:", err))  
          })
    })
}

shinyApp(ui, server)


