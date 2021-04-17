# data libraries (extraction, cleaning, plotting)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(plotly)
library(hunspell)
library(textclean)
library(syuzhet)
# word cloud libraries
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(extrafont) #font_import()

### IMPORTING DATA #############################################################
original_data <- read.csv("data/original_data.csv", stringsAsFactors = FALSE)
clean_data <- original_data

clean_data <- read.csv("clean_data.csv")

### CLEANING DATA ##############################################################

# changing column names
colnames(clean_data) <- c('time', 'name', 'email', 'object', 'motivation', 'photo_link', 'photo_source', 'ailments', 'consent')

# FUNCTION: filter for valid emails
filter_valid <- function(data, pattern) {
  data <- data %>% 
    dplyr::filter(str_detect(email, '@'))
}

clean_data <- filter_valid(clean_data, '@')

# FUNCTION: make column lowercase
make_col_lower <- function(data, col_names) {
  for (col in col_names) {
    data[[col]] <- tolower(data[[col]])
  }
  return(data)
}

#clean_data <- make_col_lower(clean_data, c("motivation", "ailments", "object"))

# FUNCTION: select relevant question columns
select_questions <- function(data, col_names) {
  data <- subset(data, select=col_names)
  return(data)
}

clean_data <- select_questions(clean_data, c("motivation", "ailments", "object"))

# FUNCTION: check for misspellings
find_typos <- function(data, data_name) {
  file_name <- paste0(data_name, ".csv")
  write.csv(data, file = file_name, row.names=FALSE)
  clean_text <- read_lines(file_name, skip_empty_rows = TRUE)
  bad_words <- hunspell(clean_text, format = "latex")
  bad_words <- sort(unique(unlist(bad_words)))
}

typos <- find_typos(clean_data, "clean_data")
print(typos)

# FUNCTION: replace misspellings in data
replace_typo <- function(data, typo_list, index, replacement, col_names) {
  for (col in col_names) {
    data[[col]] <- gsub(typo_list[index], replacement, data[[col]])
    print(data[[col]])
  }
  return(data)
}

# FUNCTION: fix typos by user input
fix_typos <- function(data, typo_list, col_names) {
  for (i in 1:length(typo_list)) {
    word = typo_list[i]
    print(word)
    print(i)
    replacement <- readline(prompt = "Enter new word to fix typo, 's' to skip, or 'q' to quit: ")
    if (replacement != 's' & replacement != 'q') {
      data <- replace_typo(data, typo_list, i, replacement, col_names) 
    }
    if (replacement == 'q') {
      break
    }
    else {
      #data <- replace_typo(data, typo_list, i, word, col_names)
      next
    }
  }
  return(data)
}

clean_data <- fix_typos(clean_data, typos, c("motivation", "ailments", "object"))

# FUNCTION: fix typo by index 
replace_index_typo <- function(data, typo_list, col_names) {
  print(typo_list)
  index <- as.numeric(readline("Enter index: "))
  new_word <- readline("Enter replacement word: ")
  data <- replace_typo(data, typo_list, index, new_word, col_names)
}

clean_data <- replace_index_typo(clean_data, typos, c("motivation", "ailments", "object"))

### RUNNING FUNCTIONS ##########################################################
# CLEAN DATA
clean_data <- original_data
colnames(clean_data) <- c('time', 'name', 'email', 'object', 'motivation', 'photo_link', 'photo_source', 'ailments', 'consent')
clean_data <- filter_valid(clean_data, '@')
clean_data <- select_questions(clean_data, c("motivation", "ailments", "object"))
typos_list <- find_typos(clean_data, "clean_data")
print(typos_list)

clean_data <- fix_typos(clean_data, typos_list, c("motivation", "ailments", "object"))
clean_data <- replace_index_typo(clean_data, typos_list, c("motivation", "ailments", "object"))

write.csv(clean_data, file = "clean_data.csv", row.names = FALSE)
# CHECKPOINT: for weird symbols 
#clean_text <- read_lines("clean_data.csv", skip_empty_rows = TRUE)
#check_text(clean_text, checks=c("misspelled", "non_ascii", "non_character"))

# TEST DATA
test_data <- original_data
colnames(test_data) <- c('time', 'name', 'email', 'object', 'motivation', 'photo_link', 'photo_source', 'ailments', 'consent')
test_data <- filter_valid(test_data, '@')
test_data <- select_questions(test_data, c("motivation", "ailments", "object"))
typo_test <- find_typos(test_data, "test_data")
print(typo_test)

test_data <- fix_typos(test_data, typo_test, c("motivation", "ailments", "object"))
test_data <- replace_index_typo(test_data, typo_test, c("motivation", "ailments", "object"))

write.csv(test_data, "test_data.csv", row.names=FALSE)
# CHECKPOINT: for weird symbols 
#test_text <- read_lines("test_data.csv", skip_empty_rows = TRUE)
#check_text(test_text, checks=c("misspelled", "non_ascii", "non_character"))

#test_data$object <- gsub("Géjou", "Gejou", test_data$object)
#print(test_data$object)

#test_cloud <- create_word_cloud2("ailments", test_data)

### CREATING FROM DATA ########################################################

# FUNCTION: searching for specific terms
cell_selection <- function(search, col, data) {
  search = paste(search, "|/UixgmJ", sep= "")
  data <- data %>%
    filter(str_detect(!!as.name(col), search)) %>% 
    select(!!as.name(col))
  return(data)
}

# FUNCTION: counting frequency of category in data
count_frequency <- function(category) {
  category %>% 
    nrow()
}

# FUNCTION: creating a word cloud (method 1) 
create_word_cloud1 <- function(col, data) {
  select_data <- data %>% 
    select(col)
  file_name <- paste0(col, "_data.csv")
  write.csv(select_data, file=file_name, row.names=FALSE)
  text <- readLines(file_name)
  source('http://www.sthda.com/upload/rquery_wordcloud.r')
  res<-rquery.wordcloud(text,
                        lang = "english",
                        min.freq = 1,
                        max.words = 200)
}

# FUNCTION: keep dashes in between words
removeMostPunctuation <- function (x, preserve_intra_word_dashes = FALSE) 
  {
    rmpunct <- function(x) {
      x <- gsub("'", "\002", x)
      x <- gsub("[[:punct:]]+", "", x)
      gsub("\002", "'", x, fixed = TRUE)
    }
    if (preserve_intra_word_dashes) { 
      x <- gsub("(\\w)-(\\w)", "\\1\001\\2", x)
      x <- rmpunct(x)
      gsub("\001", "-", x, fixed = TRUE)
    } else {
      rmpunct(x)
    }
  }

# FUNCTION: creating a word cloud (method 2)
create_word_cloud2 <- function(col, data) {
  # selecting text
  select_data <- data %>% 
    select(col)
  file_name <- paste0(col, "_data.csv")
  write.csv(select_data, file=file_name, row.names=FALSE)
  text <- readLines(file_name)
  # cleaning text, creating word freq table
  docs <- Corpus(VectorSource(text))
  docs <- tm_map(docs, content_transformer(removeMostPunctuation), preserve_intra_word_dashes = TRUE)
  docs <- docs %>% 
    tm_map(removeNumbers) %>% 
    tm_map(stripWhitespace)
  #docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix), decreasing=TRUE)
  df <- data.frame(word = names(words), freq=words)
  # making word cloud
  png(paste0(col, "_wordcloud.png"), width=12,height=8, units='in', res=300)
  wordcloud(words = df$word,
            freq = df$freq,
            min.freq=1,
            max.words=200,
            random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(8, "Dark2"),
            family="Century",
            font=1,
            scale=c(8, 0.5))
  dev.off()
}

# FUNCTION: creating a frequency table
create_freq_table <- function(num, cloud) {
  tdm <- cloud$tdm
  freqTable <- cloud$freqTable
  head(freqTable, num)
}

# FUNCTION: creating a bar chart of word frequencies
create_freq_chart <- function(num, cloud, question, color) {
  tdm <- cloud$tdm
  freqTable <- cloud$freqTable
  head(freqTable, num)
  
  barplot(freqTable[1:num,]$freq, las = 2, 
          names.arg = freqTable[1:num,]$word,
          col = color, main = question,
          ylab = "word frequencies")
  
}

### QUESTION: What do you think motivated them to hunt for ancient objects? ####

money <- cell_selection("money|monetary|greed|lucrative|profit", "motivation", clean_data)
collect <- cell_selection("collect|collections|museum|preserve|art", "motivation", clean_data)
fame <- cell_selection("fame|recognition|name", "motivation", clean_data)

count_money <- count_frequency(money)
count_collect <- count_frequency(collect)
count_fame <- count_frequency(fame)

# creating new data frame
motivation <- c('money', 'collecting', 'fame')
count <- c(count_money, count_collect, count_fame)
plot_df <- data.frame(motivation, count)

# creating a bar graph
ggplot(plot_df, aes(x=motivation, y=count, fill=motivation)) +
  geom_col() +
  labs(title = "What motivated the hunt for antiquities?")

fig <- plot_ly(
  x = motivation,
  y = count,
  name = "What motivated the hunt for antiquities?",
  type = "bar",
  marker = list(color = c("#d4e6a3", "#30f0e9", "#203662"))) %>%
  layout(title="What motivated the hunt for antiquities?",
         margin = list(t = 70),
         font = list(family = "Georgia"),
         xaxis = list(title = "motivation"),
         yaxis = list(title = "count"))
fig

# word cloud, freq table, freq bar chart
motivation_cloud2 <- create_word_cloud2("motivation", clean_data)
motivation_cloud1 <- create_word_cloud1("motivation", clean_data)
motivation_table <- create_freq_table(10, motivation_cloud1)
motivation_chart <- create_freq_chart(10,
                                      motivation_cloud1,
                                      "What do you think motivated them to hunt for ancient objects?",
                                      "maroon")

### QUESTION: What objects did Joseph sell, and to who? ########################

# word cloud, freq table, freq bar chart
clean_data$object <- gsub("IbrahimGéjou", "Ibrahim Gejou", clean_data$object)
print(clean_data$object)
object_cloud2 <- create_word_cloud2("object", clean_data)
object_cloud1 <- create_word_cloud1("object", clean_data) 
object_table <- create_freq_table(10, object_cloud1)
object_chart <- create_freq_chart(10,
                                  object_cloud1,
                                  "What objects did Joseph sell, and to who?",
                                  "navyblue")

### QUESTION: What ailments did Joseph experience, and what remedies...? #######

#word cloud, freq table, freq bar chart
ailment_cloud2 <- create_word_cloud2("ailments", clean_data)
ailment_cloud1 <- create_word_cloud1("ailments", clean_data)
ailment_table <- create_freq_table(10, ailment_cloud1)
ailment_chart <- create_freq_chart(10,
                                   ailment_cloud1,
                                   "What ailments did Joseph experience, and what remedies did he try to deal with these?",
                                   "pink")

