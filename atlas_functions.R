library(tidyverse)
library(textreadr)
library(tidytext)
library(tools)
library(textstem)
# ---------------------------------------------------------------------------------------------
#Tidyread
cst_read_document <- function(path){
  textreadr::read_document(path) %>%
    paste(collapse = "\n")
}

# Initalization - create 
cst_initialize_create <- function(corpus_folder){
  if (!file.exists("index")){
    dir.create("index")
  }
  corpus_folder <<- corpus_folder
  corpus_number.doc <<- 0
  corpus_number.word <<- 0
  
  doc_register <<- data.frame(doc_id = as.integer(),
                             doc_name = as.character(),
                             extension = as.character(),
                             doc_path = as.character(),
                             size = as.double(),
                             modification_time = as.character(),
                             creation_time = as.character(),
                             access_time = as.character(),
                             doc_number.word = as.integer())
  word_doc_index <<- data.frame(doc_id = as.integer(),
                               word = as.character(),
                               doc_word_number = as.integer())
  word_register <<- data.frame(word = as.character(), 
                              corpus_word_prob = as.character())
}

# Create doc_register
cst_create_doc_register <- function() {
  doc_register <<- data.frame(doc_path = list.files(corpus_folder, 
                                                 full.names = TRUE,
                                                 recursive = TRUE)) %>% 
    filter(!str_detect(doc_path, "atlas/index/")) %>%
    mutate(extension = file_ext(doc_path)) %>% 
    filter(extension %in% c("pdf", "docx", "doc", "rtf",
                            "txt", "pptx")) %>% 
    mutate(doc_id = as.integer(rownames(.))) %>%
    cbind(., file.info(.$doc_path)) %>% 
    mutate(doc_name = base_name(doc_path),
           doc_number.word = 0) %>% 
    filter(!str_detect(doc_name, "^~$")) %>% #remove tmp files
    mutate(modification_time = as.character(mtime),
           creation_time = as.character(ctime),
           access_time = as.character(atime)) %>% 
    select(doc_id,
           doc_name,
           extension,
           doc_path,
           size,
           modification_time,
           creation_time,
           access_time,
           doc_number.word)
  
  corpus_number.doc <<- nrow(doc_register)
  doc_tobe_read <<- doc_register %>% 
    select(doc_id, doc_path)
  corpus_number.doc.pending <<- nrow(doc_tobe_read)
}

# Read corpus engine
cst_read_corpus <- function(progress_bar = NULL){
  word_doc_index <<- doc_tobe_read %>%
    select(doc_id, doc_path) %>%
    mutate(text = "")
  if(corpus_number.doc.pending != 0){
    for (i in 1:corpus_number.doc.pending){
      word_doc_index$text[i] <<- cst_read_document(word_doc_index$doc_path[i])
      if(!is.null(progress_bar)) progress_bar$inc(1)
    }
  }
  remove(doc_tobe_read, inherits = TRUE)
}

#Index corpus engine
cst_index_corpus <- function(){
  stopwords <- tidytext::stop_words$word
  
  word_doc_index <<- word_doc_index %>%
    mutate(text = paste(base_name(doc_path), text)) %>% 
    select(-doc_path,) %>%
    mutate(text = str_replace_all(text, "[^a-zA-Z@èéòàì]", " ")) %>% 
    unnest_tokens(word, text) %>%
    filter(!(word %in% stopwords)) %>%
    mutate(word = str_trim(word),
           word = lemmatize_words(word)) %>% 
    group_by(doc_id, word) %>% 
    summarise(doc_word_number = n()) %>% 
    select(doc_id, word, doc_word_number) %>%
    arrange(doc_id, word) %>% 
    ungroup()
}

# Create word_register
cst_create_word_register <- function(){
  corpus_number.word <<- sum(word_doc_index$doc_word_number)
  if (nrow(word_doc_index != 0)){
    word_register <<- word_doc_index %>% 
      group_by(word) %>% 
      summarise(corpus_word_number = sum(doc_word_number)) %>% 
      mutate(corpus_word_prob = corpus_word_number/corpus_number.word) %>% 
      select(word, corpus_word_prob)
  }  
}

# Insert number of words in doc_register
cst_update_doc_register <- function(){
  if (nrow(word_doc_index != 0)){
    doc_number.word <- word_doc_index %>% 
      group_by(doc_id) %>% 
      summarise(doc_number.word = sum(doc_word_number, na.rm = TRUE))
    doc_register <<- doc_register %>%
      select(-doc_number.word) %>% 
      left_join(doc_number.word, by = "doc_id") %>% 
      mutate(doc_number.word = ifelse(is.na(doc_number.word), 0, doc_number.word))
  }  
}

# Create config
cst_create_config <- function(){
  # General
  index_creation.date <- Sys.time()
  project_folder <- getwd()
  
  # Corpus size
  corpus_doc.size <- if(nrow(doc_register != 0)){
    sum(doc_register$size, na.rm = TRUE) %>% 
      utils:::format.object_size("auto") %>% 
      toupper()
  }else{
    0
  }
  config <- data.frame(element = c("index_creation.date",
                                   "project_folder",
                                   
                                   "corpus_folder",
                                   "corpus_number.doc",
                                   "corpus_number.word",

                                   "corpus_doc.size"),
                       value = c(as.character(index_creation.date),
                                 project_folder,
                                 
                                 corpus_folder,
                                 corpus_number.doc,
                                 corpus_number.word,
                                 
                                 corpus_doc.size
                       )) 
  write_csv(config, "index/index_config.csv")
  write_csv(word_register, "index/word_register.csv")
  write_csv(doc_register, "index/doc_register.csv")
  write_csv(word_doc_index, "index/word_doc_index.csv")
  
  remove(doc_register,
         word_register,
         word_doc_index,
         corpus_folder,
         corpus_number.doc,
         corpus_number.word, inherits = TRUE
  )
}

# Initalization - update 
cst_initialiaze_update <- function(){
  doc_register_old <<- read_csv("index/doc_register.csv", col_types = cols(
                          doc_id = col_integer(),
                          doc_name = col_character(),
                          extension = col_character(),
                          doc_path = col_character(),
                          size = col_double(),
                          modification_time = col_character(),
                          creation_time = col_character(),
                          access_time = col_character(),
                          doc_number.word = col_double())
                                ) %>% 
    select(doc_id, doc_path, modification_time) %>% 
    rename(doc_id_old = doc_id) 
  word_doc_index_old <<- read_csv("index/word_doc_index.csv", col_types = cols(
                                  doc_id = col_double(),
                                  word = col_character(),
                                  doc_word_number = col_double()))
  word_register <<- data.frame(word = as.character(), 
                               corpus_word_prob = as.character())
  
  config <- read_csv("index/index_config.csv", col_types = cols(
                                  element = col_character(),
                                  value = col_character()
  ))
  corpus_folder <<- as.character(config[3,2])
  corpus_number.doc <<- 0
  corpus_number.word <<- 0
  
  
}

# Define to-be updated doc and refresh IDs
cst_prepare_update_word_doc_index <- function(){
  
  doc_register_new <<- doc_register %>% 
    select(doc_id, doc_path, modification_time) %>% 
    rename(doc_id_new = doc_id)
  
  doc_register_all <<- full_join(doc_register_old, doc_register_new) %>%
    select(doc_id_old, doc_id_new, doc_path, modification_time)
  
  
  # Remove deleted doc from index
  word_doc_index_old <<- doc_register_all %>%
    filter(is.na(doc_id_new)) %>% 
    select(doc_id_old) %>%
    rename(doc_id = doc_id_old) %>% 
    anti_join(word_doc_index_old, .)
  
  # Modify doc_id
  word_doc_index_old <<- doc_register_all%>%
    filter(!is.na(doc_id_old), !is.na(doc_id_new)) %>% 
    select(doc_id_old, doc_id_new) %>% 
    rename(doc_id = doc_id_old) %>%
    left_join(word_doc_index_old, .) %>% 
    select(doc_id_new, word, doc_word_number) %>%
    rename(doc_id = doc_id_new)
  
  # Create to be read id's list
  doc_tobe_read <<- doc_register_all %>% 
    filter(is.na(doc_id_old)) %>% 
    select(doc_id_new, doc_path) %>% 
    rename(doc_id = doc_id_new)
  corpus_number.doc.pending <<- nrow(doc_tobe_read)
}

# Update index
cst_conclude_update_word_doc_index <- function(){

  #Merge
  word_doc_index <<- rbind(word_doc_index_old, word_doc_index) %>% 
    arrange(doc_id, word)
  
  
  # Remove
  remove(word_doc_index_old,
         doc_register_old,
         doc_register_new,
         doc_register_all,
         corpus_number.doc.pending, inherits = TRUE)
}

# ---------------------------------------------------------------------------------------------



