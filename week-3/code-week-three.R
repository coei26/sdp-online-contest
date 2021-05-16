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
original_data <- read.csv("week_three_data.csv", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")
clean_data <- original_data

clean_data <- read.csv("clean_data.csv")

### CLEANING DATA ##############################################################

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

# FUNCTION: select relevant question columns
select_questions <- function(data, col_names) {
  data <- subset(data, select=col_names)
  return(data)
}


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
    data[[col]] <- gsub(paste0('\\<', typo_list[index], '\\>'), replacement, data[[col]])
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


# FUNCTION: fix typo by index 
replace_index_typo <- function(data, typo_list, col_names) {
  print(typo_list)
  index <- as.numeric(readline("Enter index: "))
  new_word <- readline("Enter replacement word: ")
  data <- replace_typo(data, typo_list, index, new_word, col_names)
}

### RUNNING FUNCTIONS ##########################################################
# CLEAN DATA
clean_data <- original_data
clean_data <- select_questions(clean_data, c("artefacts", "excavate"))
typos_list <- find_typos(clean_data, "clean_data")
print(typos_list)

clean_data <- fix_typos(clean_data, typos_list, c("artefacts", "excavate"))
clean_data <- replace_index_typo(clean_data, typos_list, c("artefacts", "excavate"))

write.csv(clean_data, file = "clean_data.csv", row.names = FALSE)
clean_data <- read.csv("clean_data.csv")
# CHECKPOINT: for weird symbols 
#clean_text <- read_lines("clean_data.csv", skip_empty_rows = TRUE)
#check_text(clean_text, checks=c("misspelled", "non_ascii", "non_character"))

### CREATING FROM DATA ########################################################

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
  res <- rquery.wordcloud(text,
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
  png(paste0(col, "_wordcloud.png"), width=25,height=12, units='in', res=300)
  wordcloud(words = df$word,
            freq = df$freq,
            min.freq=1,
            max.words=200,
            random.order=FALSE,
            rot.per=0.35,
            colors=brewer.pal(6, "Dark2"),
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

### QUESTION: What do you think motivated antiquity dealers like Géjou to ###### 
### trade ancient artefacts?
clean_data$artefacts <- gsub(paste0('\\<', "Artefacts", '\\>'), "", clean_data$artefacts)
artefacts_cloud2 <- create_word_cloud2("artefacts", clean_data)
artefacts_cloud1 <- create_word_cloud1("artefacts", clean_data)
artefacts_table <- create_freq_table(10, artefacts_cloud1)
artefacts_chart <- create_freq_chart(10,
                                     artefacts_cloud1,
                                     "What motivated antiquity dealers like Géjou to trade ancient artefacts?",
                                     "#A2352F")

### QUESTION: Which foreign nations have come to Iraq to excavate? #############

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

germany <- cell_selection("Germany", "excavate", clean_data)
count_germany <- count_frequency(germany)

us <- cell_selection("United States", "excavate", clean_data)
count_us <- count_frequency(us)

spain <- cell_selection("Spain", "excavate", clean_data)
count_spain <- count_frequency(spain)

turkey <- cell_selection("Turkey", "excavate", clean_data)
count_turkey <- count_frequency(turkey)

italy <- cell_selection("Italy", "excavate", clean_data)
count_italy <- count_frequency(italy)

# creating new data frame
excavating_countries <- c('Germany', 'United States', 'Turkey', 'Italy', 'Spain')
count <- c(count_germany, count_us, count_turkey, count_italy, count_spain)
plot_df <- data.frame(excavating_countries, count)


# creating a bar graph
ggplot(plot_df, aes(reorder(excavating_countries, -count), count)) +
  geom_col(aes(fill = excavating_countries)) +
  labs(title = "Which foreign nations came to Iraq to excavate?",
       x = "foreign nations",
       y = "count",
       fill = "nations") +
  theme(plot.title = element_text(hjust=0.5, size=17),
        axis.title.x = element_text(size=16),
        axis.text.x = element_text(size=14, angle=18))
