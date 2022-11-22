#most of the libraries needed
library(ggplot2) #visualizations
library(tidytext) #text mining
library(wordcloud2) #creative visualizations
library(stringr) #string manipulation

user_data <- read.csv("poppinuser.csv")

user_data$bio <- str_replace_all(user_data$bio, "([\n])", "") # Remove new line characters from bios
user_data$bio <- str_replace_all(user_data$bio, "’", "'") # Adjust apostrophe formatting
user_data$bio <- tolower(user_data$bio) # Convert to lowercase


bio_words <- user_data %>% unnest_tokens(output = word, input = bio)  # Unnests bio entries by word
bio_words <- bio_words %>% anti_join(stop_words) # Removes stopwords (a, the, i, etc.)

bio_wordcounts <- bio_words %>% count(word, sort = TRUE) # Gets word count 

for (i in seq_along(bio_wordcounts$word)) { # Clean naughty words
  if (bio_wordcounts$word[i] == "fuck") bio_wordcounts$word[i] <- "f*ck" 
  else if (bio_wordcounts$word[i] == "shit") bio_wordcounts$word[i] <- "sh*t" 
  else if (bio_wordcounts$word[i] == "bitches") bio_wordcounts$word[i] <- "b*tches"
  else if (bio_wordcounts$word[i] == "nigga") bio_wordcounts$word[i] <- "n*gga" 
}

bio_wordcounts <- bio_wordcounts[bio_wordcounts$n > 1, ] # Removes words with fewer than 3 occurances across user bios

poppin_colors <- c(rep(c("#D705F2", "#00ADFF", "#a628c9", "#7F76FE", "#3C498F", "#0d63ea", "#810062"), 4), rep("#ffffff", length(bio_wordcounts$word))) # Color vector with Poppin color scheme

wordcloud2(data = bio_wordcounts, fontFamily = "Poppins", size = 1, color = poppin_colors, backgroundColor = "black") # Create wordcloud

bio_wordcounts %>% # Create bar graph
  filter(n > 10) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Frequent Words In User Bios \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", color="darkblue", size = 12),
        axis.title.y = element_text(face="bold", color="darkblue", size = 12))


