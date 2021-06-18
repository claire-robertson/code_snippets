## Validation Stim Selection
## C. Robertson S2021

#### LOAD LIBRARIES AND DATA ####

require(tidyverse)
require(DataCombine)
require(gutenbergr)
require(tidytext)

## Let's pretend we want to create a qualtrics survey examining reading comprehension. We want to see how reliably individuals can extract meaning from 5-word chunks from a common children's book. Here's one way to make a validation survey that tests 20 of these 5-word chunks. 

## read in Data
alice <- gutenberg_download(11) %>% 
  mutate(book = "Alice's Adventures in Wonderland") %>% 
  slice(-(1:29))

#Separate into chunks
alice_ngram <- alice %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 5) 

#Clean data
alice_ngram <- alice_ngram %>% 
  filter(!is.na(ngram)) %>% 
  select(ngram) %>% 
  mutate(id = row_number(), .before = "ngram")

#Set seed and randomly select N number of tests of data. 
set.seed(05212021)
tests <- sample(1:length(unique(alice_ngram$id)), 20) 

#Pull tests of data
survey_sample <- alice_ngram %>% 
  filter(id %in% tests)

#pull only the headline. 
survey_sample<- survey_sample %>% 
  select(text = ngram)

## Labels for the loop
header_label <- "[[Question:DB]]"
header <- "Please read the following piece of text: <br><br>"

#Question 1
label1 <- '[[Question:MC:SingleAnswer:Horizontal]]'
question1 <- "How much semantic meaning does the above text contain?"
choices1 <- data.frame("text" = c("[[AdvancedChoices]]", "[[Choice]]", "1<br>", "No Meaning", "[[Choice]]", "2", "[[Choice]]", "3", "[[Choice]]", "4<br>", "Some Meaning", "[[Choice]]", "5", "[[Choice]]", "6", "[[Choice]]", "7<br>", "Much Meaning" ))

#Question 2
label2 <- '[[Question:MC:SingleAnswer:Horizontal]]'
question2 <- "How negative or positive do you think this text is?"
choices2 <- data.frame("text" = c("[[AdvancedChoices]]", "[[Choice]]", "-3<br>", "Very Negative", "[[Choice]]", "-2", "[[Choice]]", "-1", "[[Choice]]", "0<br>", "Neutral", "[[Choice]]", "1", "[[Choice]]", "2", "[[Choice]]", "3<br>", "Very Positive" ))

#Page break
pagebreak <- "[[PageBreak]]"

#Initialize df
survey_df <- data.frame(text = character())

#Loop for creating the choice questions. 
for(i in 1:nrow(survey_sample)) {
  loop_text <- survey_sample[i,]

  loop_text$text <- paste0("<b>", loop_text$text)
  loop_text$text <- paste0(loop_text$text, "<b>")  
  
  #Header text
  loop_text <- InsertRow(loop_text, header_label, RowNum = 1)
  loop_text <- InsertRow(loop_text, header, RowNum = 2)
  
  #Question 1
  loop_text <- rbind(loop_text, label1)
  loop_text <- rbind(loop_text, question1)
  loop_text <- rbind(loop_text, choices1)
 
  #Question 2
  loop_text <- rbind(loop_text, label2)
  loop_text <- rbind(loop_text, question2)
  loop_text <- rbind(loop_text, choices2)
  
  #Page Break
  loop_text <- rbind(loop_text, pagebreak)

  #add to dataframe
  survey_df <- rbind(survey_df, loop_text)
}

#Add descriptors 
format <- "[[AdvancedFormat]]"
block <- "[[Block:MC Block]]"
survey_df <- InsertRow(survey_df, format, RowNum = 1)
survey_df <- InsertRow(survey_df, block, RowNum = 2)

# Write samples csv
write.table(survey_df, "sample_survey.txt", sep = ".", row.names = F, quote = F)

