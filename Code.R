# Function to install a package if not already installed
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Load required packages and install if necessary
packages <- c("dplyr", "tidyr", "stringr", "tidytext", "ggplot2", "reshape2", "wordcloud2", "textdata", "wordcloud")

invisible(lapply(packages, install_if_missing))


# List and read TSV files, add source column, combine, and remove missing rows/observations

# you need to replace path where to where you have stored the 23 .tsv files
path <- "F:/Github Projects/exploring-text-data/data files" 

data <- list.files(path = path, pattern = "\\.tsv$", full.names = TRUE) %>%
  lapply(function(file) read.delim(file) %>% mutate(source_file = basename(file))) %>%
  bind_rows() %>%
  rename(text = Text,
         theme = Theme,
         source = source_file) %>%
  mutate(across(everything(), ~na_if(str_trim(.), ""))) %>%
  drop_na()

glimpse(data)

data %>% count(source) # number of source files (should be 23)
data %>% count(theme) # number of categories assigned to text  


# Clean data
data2 <- data %>%
  mutate(text = str_replace_all(text, "[?âè.ã'’'½_&$,%'-']", " "), # Remove special characters
         text = str_replace_all(text, "([0-9])([a-z])", "\\1 \\2"),  # Add space between numbers and letters
         text = str_replace_all(text, "([a-z])([0-9])", "\\1 \\2"),  # Add space between letters and numbers
         text = str_squish(text))                                   # Strip extra white space

glimpse(data2)


# Tidy data - Unnest tokens
tidy_data <- data2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%                 # remove stop words          
  mutate(characters = nchar(word)) %>%      # number of alphabets in a word
  filter(characters > 1)                # filtering for words with > 1 alphabets


glimpse(tidy_data)

# Top 15 words
top15_tidy <- tidy_data %>%
  count(word, theme, source) %>%
  filter(!str_detect(word, "^[0-9]*$")) %>%           # Exclude words that are just numbers
  count(word, theme) %>%
  group_by(theme) %>%
  slice_max(n, n = 15, with_ties = FALSE) %>%         # Select exactly top 10 words per theme
  ungroup() %>%
  arrange(theme, desc(n), word)                       # Arrange by theme, frequency, and word

glimpse(top15_tidy)

# Visualize top 15 words for each category
top15_tidy_plot <- top15_tidy %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col () +
  labs(x = NULL, y = "n") +
  facet_wrap(~theme, ncol = 2, scales = "free") +
  coord_flip()
top15_tidy_plot


# Wordcloud
theme_data <- tidy_data %>%
  filter(theme == "Natural") %>%              # You can replace "Natural" with any other category, e.g., "Human"
  filter(!str_detect(word, "^[0-9]*$")) %>%   # Remove words that are just numbers
  count(word) %>%
  arrange(desc(n))

wordcloud(words = theme_data$word, freq = theme_data$n, 
          scale = c(2, 0.5), random.order = FALSE, max.words = 200,
          colors = brewer.pal(8, "Dark2"))

# Positive and Negative Tone Word Cloud
tidy_data %>%
  filter(theme == "Natural") %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 200, title.size = 3, scale = c(3, 1.3))


