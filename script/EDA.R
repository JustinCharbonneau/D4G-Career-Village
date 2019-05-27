setwd("~/Desktop/Kaggle/D4G_CareerVillage")

# Project structure:

# --D4G_CareerVillage
# ----Data
# ----Script

# Load packages

library(tidyverse)
library(lubridate)

students <- read_csv('Data/students.csv')
questions <- read_csv('Data/questions.csv')

questions <- questions %>%
  mutate(date_start = as.numeric(as.Date(as.POSIXlt(questions_date_added))))

# Create a table for the minimum occurence
min_table <- questions %>%
  group_by(questions_author_id) %>%
  summarise(min = min(date_start))

questions <- merge(questions, min_table, by = "questions_author_id")

# Recombine both datasets to include the min for each
questions <- questions %>%
  mutate("elapsed_days" = (date_start - min))

# Total questions per day
dummy <- questions %>%
  mutate(questions_author_id = as.factor(questions_author_id)) %>%
  group_by(questions_author_id,date_start) %>%
  summarise(total_questions = n())

new_dummy <- distinct(merge(dummy,temp_questions, by=c("questions_author_id","date_start"))[,c('questions_author_id','elapsed_days','total_questions')])

# Looking at the top 5 
dummy_1 <- new_dummy %>%
  filter(total_questions < 6) %>%
  group_by(total_questions) %>%
  summarize(freq = n()) %>%
  mutate(total_questions = as.factor(total_questions))

ggplot(data = dummy_1, aes(x=total_questions,  y=freq, fill = total_questions)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), vjust=-0.3, size=3.5) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 13), 
        title = element_text(face = "bold", size = 16),
        legend.position = "none") +
  labs(title="Total Number of Questions by User (under 5)", x = "Number of questions", y = "Count of users") +
  scale_fill_manual(values=c("#133d82","#133d82","grey","grey","grey"), aesthetics = "fill")+
  geom_curve(x = 3, xend = 2, y = 7000, yend = 6200, size = .3, 
             arrow = arrow(length = unit(.03, "npc")),
             curvature = 0.1) +
  annotate("text", x = 4.1, y = 7000, label = "Most students only ask 1-2 questions", 
           color = "#5b5b5b")

# Annotation style inspired by Michael Johnson (https://www.kaggle.com/mistermichael/careervillage-exploration)

dummy_2 <- new_dummy %>%
  filter(total_questions > 5) %>%
  group_by(total_questions) %>%
  summarize(freq = n()) %>%
  mutate(total_questions = as.factor(total_questions))

# Continuation of the analysis for people who asked over 5 questions

ggplot(data = dummy_2, aes(x=total_questions,  y=freq, fill = total_questions)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=freq), vjust=-0.3, size=3.5) +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 13), 
        title = element_text(face = "bold", size = 16),
        legend.position = "none") +
  labs(title="Total Number of Questions by User (over 5)", x = "Number of questions", y = "Count of users") +
  scale_fill_manual(values=c("grey","grey","grey","grey","grey","grey","grey","grey","grey","grey",
                             "grey","grey","grey","#133d82","#133d82"), aesthetics = "fill") +
  geom_curve(x = 12, xend = 14.2, y = 15, yend = 5, size = .3, 
             arrow = arrow(length = unit(.03, "npc")),
             curvature = -0.1) +
  annotate("text", x = 12, y = 17, label = "Two students asked over 30 questions!", 
           color = "#5b5b5b")

library(DT)

dummy %>% head() %>% 
  datatable()


filtered_dummy <- new_dummy %>%
  filter(elapsed_days > 30)

# Let's look at the first 50. Wow! user '05444 ... ' had many many questions very late. I wonder what was his initial quesiton. 
ggplot(data=filtered_dummy[0:50,], aes(x = elapsed_days, y = questions_author_id,size = total_questions))+
  geom_point(colour="Turquoise4")


library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

# What are the users following? (Interest)

tags <- read_csv('Data/tags.csv')
tag_users <- read_csv('Data/tag_users.csv')

tag_users <- tag_users %>%
  mutate(tags_tag_id = tag_users_tag_id)

# Is there a correlation for the frequency of users asking many
# questions and the subjects? Some subjects may be harder to answer.

patterns <- c("#|-|!|@")

tag_users_full <- inner_join(tags,tag_users, by ='tags_tag_id') %>%
  mutate(tags_tag_name = str_replace_all(tags_tag_name, patterns,""))
  
# Create a corpus and plot the wordcloud
tag_users_full.Corpus<-Corpus(VectorSource(tag_users_full$tags_tag_name))
wordcloud(words = tag_users_full.Corpus,max.words = 100,min.freq =500,random.color = TRUE,random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# Get a frequency table to see clearly
users_hashtags_frequency <- as.data.frame(table(tag_users_full$tags_tag_name))

'''
College is the most common hashtag followed by students. This makes sense and isnt suprising.
'''



# What are the questions

tag_questions <- read_csv('Data/tag_questions.csv')

tag_questions <- tag_questions %>%
  mutate(tags_tag_id = tag_questions_tag_id)

# Is there a correlation for the frequency of users asking many
# questions and the subjects? Some subjects may be harder to answer.

patterns <- c("#|-|!|@")

tag_questions_full <- inner_join(tags,tag_questions, by ='tags_tag_id') %>%
  mutate(tags_tag_name = str_replace_all(tags_tag_name, patterns,""))

# Create a corpus and plot the wordcloud
tag_questions_full.Corpus<-Corpus(VectorSource(tag_questions_full$tags_tag_name))
wordcloud(words = tag_questions_full.Corpus,max.words = 100,min.freq =500,random.color = TRUE,random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# Get a frequency table to see clearly
questions_hashtags_frequency <- as.data.frame(table(tag_questions_full$tags_tag_name))

'''
College is the most common question hashtag. This is good, that means the students are following the right hashtags,
but not quite. The question hashtag "career" was used more than 500 times but no one follows the hashtag "career". 

In a latter time, maybe looking if there are "repeatable" questions that occur frequently, and if users had a followed 
the appropriate hashtag, their questions would of been answered.

Furthermore, Carreer advice makes sense, as most of the word cloud is formed of professions.

In the users questions they are following, they seem to be interested in following different types of hashtags. more like 
instagram hashtags.

This means that now I wonder if people who follow the hashtags are the ones who ask the questions. Is there a correlation in the users?

'''



# There are some with hashtags and others without hashtags. some cleaning is needed.

temp_cnt <- temp %>%
  group_by(tags_tag_name) %>%
  summarise(total_tag_name = n())

patterns <- c("#|-|!")

my_string <- c("#lol","lol2!","lol4")
second_string <- c("test21","-te4","lol#lol")
my_df <- merge(my_string,second_string)
colnames(my_df) <- c('first','second')



my_df <- my_df %>%
  mutate(first = str_replace_all(first, patterns,""))


# Create a corpus
jobTitles <- Corpus(VectorSource(cleanFFData$CurrentJobTitleFreeForm))

# Convert to plain text document
jobTitles <- tm_map(jobTitles, PlainTextDocument)

# Remove numbers and punctuation, just in case
jobTitles <- tm_map(jobTitles, removeNumbers)
jobTitles <- tm_map(jobTitles, removePunctuation)

# Make all jobTitles lowercase
jobTitles <- tm_map(jobTitles, content_transformer(tolower))

# Remove non job title words
jobTitles <- tm_map(jobTitles, removeWords, c("and"))

# Generate the wordcloud
wordcloud(jobTitles, 
          scale = c(5,0.2), 
          max.words = 150, 
          random.order = FALSE, 
          rot.per = 0.35, 
          use.r.layout = TRUE, 
          colors = brewer.pal(6, "Blues")[c(4,5,6,7,8,9)])



m <- matrix(c(9, 9, 9, 9, 9, 2, 9, 9, 1, 9, 9, 9, 9, 1, 1, 9), 4,
            dimnames = list(c(1, 2, 3, "U"), c("A", "B", "C", "D")))
m

library(lpSolve)
fm <- lp.assign(m)

fm$solution

fm$solution


# Creating a dummy example


get_fq_tag_questions <- as.data.frame(table(tag_questions$tag_questions_question_id))

The assignment problem





