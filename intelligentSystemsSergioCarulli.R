library(utf8)
library(qdap)
library(tidytext)
library(quanteda)
library(VIM)
library(tm)
library(wordcloud2)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(scales)
library(circlize)
library(radarchart)
library(e1071)
library(gmodels)

#Load dataset
steam <- read.csv("./steamReducido.csv", header = FALSE)

#Import lexicons
#Originally I did the program in kaggle and this was 
#the solution to load afinn without using the terminal
afinn <- read_csv("./lexicons/Afinn.csv",
                  col_types = cols(word = col_character(), value = col_double()))
bing <- read_csv("./lexicons/Bing.csv",
                 col_types = cols(word = col_character(), sentiment = col_character()))
nrc <- read_csv("./lexicons/NRC.csv",
                col_types = cols(word = col_character(), sentiment = col_character()))

#Set Colors
options(repr.plot.width=15, repr.plot.height=7)

# Custom Color Palette
my_colors <- c("#05A4C0", "#85CEDA", "#D2A7D8", "#A67BC5", "#BB1C8B", "#8D266E")
show_col(my_colors, labels = F, borders = NA)


# Custom Theme Variable
my_theme <- theme(plot.background = element_rect(fill = "grey98", color = "grey20"),
                  panel.background = element_rect(fill = "grey98"),
                  panel.grid.major = element_line(colour = "grey87"),
                  text = element_text(color = "grey20"),
                  plot.title = element_text(size = 22),
                  plot.subtitle = element_text(size = 17),
                  axis.title = element_text(size = 15),
                  axis.text = element_text(size = 15),
                  legend.box.background = element_rect(color = "grey20", fill = "grey98", size = 0.1),
                  legend.box.margin = margin(t = 3, r = 3, b = 3, l = 3),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 15),
                  strip.text = element_text(size=17))

#Exploratory Analysis
#Seeing missing values
aggr(steam)
#Remove missing values
steam <- na.omit(steam)
#Seeing number of observations and type of the variables
str(steam)
#Seeing number of comments per game
dataFrameAux <- as.data.frame(steam)
aux <- data.frame(table(steam$V1))
aux[order(aux$Freq,decreasing = TRUE),]
#Seeing number of positive reviews
aux <- data.frame(table(steam$V3))
aux[order(aux$Freq,decreasing = TRUE),]
#Seeing number of relevant reviews
aux <- data.frame(table(steam$V4))
aux[order(aux$Freq,decreasing = TRUE),]
#Check if the text is in utf8
linesQ <- steam$V2
print(linesQ[!utf8_valid(linesQ)])
#Character(0) is that all lines are made of correct UTF-8 characters
#Check character normalization (NFC)
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ) 
#0 means text is in NFC

#Cleaning data
# Set the text to lowercase
steam$V2 <- tolower(steam$V2)
# Remove mentions, emojis, numbers, punctuations, etc.
steam$V2 <- gsub("\\d+\\w*\\d*", "", steam$V2)
steam$V2 <- gsub("#\\w+", "", steam$V2)
steam$V2 <- gsub("[^\x01-\x7F]", "", steam$V2)
steam$V2 <- gsub("[[:punct:]]", " ", steam$V2)
steam$V2 <- gsub("\t|\n", "", steam$V2)
steam$V2 <- gsub('\\p{So}|\\p{Cn}', '', steam$V2, perl = TRUE)
# Remove spaces and newlines
steam$V2 <- gsub("\n", " ", steam$V2)
steam$V2 <- gsub("^\\s+", "", steam$V2)
steam$V2 <- gsub("\\s+$", "", steam$V2)
steam$V2 <- gsub("[ |\t]+", " ", steam$V2)

#Sentiment Analysis

#Making WordCloud
#Make corpus
my_corpus <- VCorpus(VectorSource(steam$V2))
# removes stopwords
stopwords_remove <- c(stopwords("en"), c("thats","weve","hes","theres","ive","im",
                                         "will","can","cant","dont","youve","us",
                                         "youre","youll","theyre","whats","didnt"))
my_corpus <- tm_map(my_corpus, removeWords, stopwords_remove)
my_corpus <- TermDocumentMatrix(my_corpus)
my_corpus <- removeSparseTerms(my_corpus, 0.999)
m <- as.matrix(my_corpus)
word_freqs <- sort(rowSums(m), decreasing = T)
# change to dataframe
df <- data.frame(word=names(word_freqs), freq=word_freqs)
#Make the wordcloud
wordcloud2(df, size=1.6, minSize = 0.9, 
           color='random-light', backgroundColor="black", 
           fontFamily="HersheySymbol")

#Making positive-negative wordcloud
# Breaks the game comment into words on each row
# in order to append the "sentiment" of the comment
break_steam <- steam %>% 
  mutate(text = as.character(steam$V2)) %>% 
  unnest_tokens(word, text)

options(repr.plot.width=15, repr.plot.height=15)

break_steam %>% 
  inner_join(bing, by="word") %>%
  count(word, sentiment, sort=T) %>% 
  acast(word ~ sentiment, value.var = "n", fill=0) %>% 
  
  # wordcloud
  comparison.cloud(colors=my_colors[c(5, 1)], max.words = 400, title.size = 2,
                   scale = c(3,.5))

#Primary emotions analysis
options(repr.plot.width=15, repr.plot.height=9)

# The plot:
break_steam %>% 
  inner_join(nrc, "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(sentiment, sort=T) %>% 
  
  ggplot(aes(x=reorder(sentiment, n), y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5, fill="white") +
  labs(x="Sentiment", y="Frequency", title="What is the overall mood in game comments?") +
  scale_fill_gradient(low = my_colors[3], high = my_colors[1], guide="none") +
  coord_flip() + 
  my_theme + theme(axis.text.x = element_blank())

#Emotion Split by words
options(repr.plot.width=15, repr.plot.height=9)

break_steam %>% 
  inner_join(nrc, "word") %>% 
  count(sentiment, word, sort=T) %>%
  group_by(sentiment) %>% 
  arrange(desc(n)) %>% 
  slice(1:7) %>% 
  
  # Plot:
  ggplot(aes(x=reorder(word, n), y=n)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, ncol = 5) +
  coord_flip() +
  my_theme + theme(axis.text.x = element_blank()) +
  labs(x="Word", y="Frequency", title="Sentiment split by most frequent words") +
  scale_fill_manual(values = c(my_colors, "#BE82AF", "#9D4387", "#DEC0D7",
                               "#40BDC8", "#80D3DB", "#BFE9ED"))

#Sentiment Distribution
options(repr.plot.width=15, repr.plot.height=9)

break_steam %>% 
  # Count how many word per value
  inner_join(afinn, "word") %>% 
  group_by(value) %>% 
  count(value, sort=T)  %>% 
  
  # Plot
  ggplot(aes(x=value, y=n)) +
  geom_bar(stat="identity", show.legend = F, width = 0.5, fill = my_colors[1]) +
  geom_label(aes(label=format(n, big.mark = ",")), size=5) +
  scale_x_continuous(breaks=seq(-5, 5, 1)) +
  labs(x="Score", y="Frequency", title="Word count distribution over intensity of sentiment: Neg - Pos") +
  my_theme + theme(axis.text.y = element_blank())

#Chord Diagram
# Filter only main 3 games with most comments
games <- break_steam %>%
  filter(V1 %in% c(10, 10090,1002))

# Create totals dataframe for the 3 games
total_bing <- games %>% 
  inner_join(bing, by="word") %>%
  count(V1) %>% 
  group_by(V1) %>% 
  summarise(total_tweets = sum(n), .groups = "drop_last")

options(repr.plot.width=15, repr.plot.height=9)

to_plot <- games %>% 
  # get 'bing' and filter the data
  inner_join(bing, by="word") %>%
  
  # sum number of words per sentiment and game
  count(sentiment, V1) %>% 
  group_by(V1, sentiment) %>% 
  summarise(sentiment_sum = sum(n), .groups = "drop_last") %>% 
  inner_join(total_bing, by="V1") %>% 
  mutate(sentiment_perc = sentiment_sum/total_tweets) %>% 
  select(V1, sentiment, sentiment_perc)

#Making Chord Diagram  
circos.clear()
circos.par(gap.after = c(rep(2, length(unique(to_plot[[1]])) - 1), 15,
                         rep(2, length(unique(to_plot[[2]])) - 1), 15), gap.degree=2)

myColors = c("10" = my_colors[3], "10090" = my_colors[4], 
             "positive" = "#D7DBDD", "negative" = "#D7DBDD")

chordDiagram(to_plot, grid.col = myColors, transparency = 0.2, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.03, 0.06))

title("Relationship between Sentiment and Games")

#Radar Chart
# Filter only main 3 games with most tweets
games <- break_steam %>%
  filter(V1 %in% c(10,10090,1002))
# Table with games, sentiment and word count
char_sentiment <- games %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  group_by(V1, sentiment) %>% 
  count(V1, sentiment) %>% 
  select(V1, sentiment, char_sentiment_count=n)

# Total Count of sentiments per game
total_count <- games %>% 
  inner_join(nrc, "word") %>% 
  filter(!sentiment %in% c("positive", "negative")) %>% 
  count(V1) %>% 
  select(V1, total=n)
# Radar Chart:
plt <- char_sentiment %>% 
  inner_join(total_count, by="V1") %>% 
  mutate(percent = char_sentiment_count / total * 100 ) %>% 
  select(-char_sentiment_count, -total) %>% 
  spread(V1, percent)  %>% 
  chartJSRadar(showToolTipLabel = T, main="Games comments and Emotion", maxScale=25, responsive=T,
               addDots = T, 
               colMatrix = grDevices::col2rgb(my_colors[c(3, 5, 4, 2)]),
               lineAlpha = 0.7, polyAlpha = 0.2)

plt

#Words with the biggest contribution in sentiment
options(repr.plot.width=15, repr.plot.height=9)

break_steam %>% 
  # by word and value count number of occurences
  inner_join(afinn, "word") %>% 
  count(word, value, sort=T) %>% 
  mutate(contribution = n * value,
         sentiment = ifelse(contribution<=0, "Negative", "Positive")) %>% #another variable
  arrange(desc(abs(contribution))) %>% 
  head(20)  %>% 
  
  # plot
  ggplot(aes(x=reorder(word, contribution), y=contribution, fill=sentiment)) +
  geom_col(aes(fill=sentiment), show.legend = F) +
  labs(x="Word", y="Contribution", title="Words with biggest contributions in positive/negative sentiments") +
  coord_flip() +
  scale_fill_manual(values=my_colors[c(3, 2)]) + 
  my_theme

#Classification models
my_corpus <- Corpus(VectorSource(steam$V2))
clean.corpus <- tm_map(my_corpus, tolower)
clean.corpus <- tm_map(clean.corpus, removeNumbers)
clean.corpus <- tm_map(clean.corpus, removeWords, stopwords())
clean.corpus <- tm_map(clean.corpus, removePunctuation)
clean.corpus <- tm_map(clean.corpus, stripWhitespace)
clean.corpus.dtm <- DocumentTermMatrix(clean.corpus)  
n <- nrow(steam)
raw.text.train <- steam[1:round(.8 * n),]
raw.text.test  <- steam[(round(.8 * n)+1):n,]

nn <- length(clean.corpus)
clean.corpus.train <- clean.corpus[1:round(.8 * nn)]
clean.corpus.test  <- clean.corpus[(round(.8 * nn)+1):nn]

nnn <- nrow(clean.corpus.dtm)
clean.corpus.dtm.train <- clean.corpus.dtm[1:round(.8 * nnn),]
clean.corpus.dtm.test  <- clean.corpus.dtm[(round(.8 * nnn)+1):nnn,]
#Eliminate any words that appear in less than three reviews or less than about 0.1 percent of records in the training data.
freq.terms <- findFreqTerms(clean.corpus.dtm.train, 3)
clean.corpus.dtm.freq.train <- DocumentTermMatrix(clean.corpus.train, list(dictionary = freq.terms))
clean.corpus.dtm.freq.test  <- DocumentTermMatrix(clean.corpus.test, list(dictionary = freq.terms))

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

clean.corpus.dtm.freq.train <- apply(clean.corpus.dtm.freq.train, MARGIN = 2, convert_counts)
clean.corpus.dtm.freq.test  <- apply(clean.corpus.dtm.freq.test, MARGIN = 2, convert_counts)

#Clasificate the comments as positive or negative

# Constructing model and making prediction
text.classifer <- naiveBayes(clean.corpus.dtm.freq.train, raw.text.train$V3)
text.pred <- predict(text.classifer, clean.corpus.dtm.freq.test)
CrossTable(text.pred, raw.text.test$V3,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#Classificate the comments as relevants or irrelevants

# Constructing model and making prediction
text.classifer2 <- naiveBayes(clean.corpus.dtm.freq.train, raw.text.train$V4)
text.pred2 <- predict(text.classifer2, clean.corpus.dtm.freq.test)

CrossTable(text.pred2, raw.text.test$V4,
           prop.chisq = FALSE, 
           prop.t = FALSE,
           dnn = c('predicted', 'actual'))
