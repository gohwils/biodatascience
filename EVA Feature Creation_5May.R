#' Sentiment Analysis - Feature Extractions - 20/21- April 2020
#### Info Dump ####
#' This part is an info dump for me to figure out what I need to do
#' Features created by Wilson
#' 1. Average Unfiltered Sentiment Score (No Zero-Filtered)
#' 2. Average Filtered Sentiment Score (Zero-Filtered)
#' 3. Largest "Happy" island (The spread of "happy". One crest to another crest) 
#' 4. Largest "Sad" island (The spread of "sad". One Trough to another trough)
#' 5. Normalized flip frequency (If sentence 1 is happy & sentence 2 is sad -> n = 1) -> n/(No. of sentences)
#' 6. Variance (Variance of filtered score)
#' 7. Variance (Variance of unfiltered score)
#' 8. Hill(Crest) spacing (Top 2 Positive island & measure the sentences between them divided by total sentences)
#' 9. Trough spacing (Top 2 negative island & measure the sentences between them divided by total sentences)
#' 10. Highest "Happy" island Sentiment
#' 11. Highest "Sad" island Sentiment
#' 12. Simple moving average (moving window) -> Try 3 first 
#' 13. Variance of the Highest "happy" island -> Calculate the sentiment score of the highest happy island sentiment points
#' 14. Variance of the largest sadest island  -> Calculate the sentiment score of the highest sad island sentiment points
#' 15. Ratio of number of positive peaks to number of negative peaks
#' 16. Ratio of total sentiment average of all positive peaks :total sentiment average of all negative average negative peaks (Assuming is number not the sentiment value itself)

#' Libraries

library(readtext)
library(dplyr)
library(sentimentr)
library(tidyverse)

#' Reading in Files 
#' Commented 18/19. Just going to work on 17/18 first 

files <- list.files(path = getwd(), pattern = "*.txt", full.names = T)
text.data_17 <- readtext(files)
text.data_18 <- readtext(files)

cleantext <- function(x, punct = FALSE, number = FALSE, lower = FALSE){
  x <- 
    str_replace_all(x, '[:cntrl:]', ' ') %>%
    str_replace_all('\\\\\\s+', '') %>%
    str_replace_all('\\\\', '') %>%
    str_replace_all('\\u2028', '') %>%
    str_replace_all('\\s+', ' ')
  
  if (isTRUE(punct))
    x <- str_replace_all(x, '[:punct:]', '')
  
  if (isTRUE(number))
    x <- str_replace_all(x, '[:digit:]', '')
  
  if (isTRUE(lower))
    x <- str_to_lower(x, locale = 'en')
  
  return(x)
}


text.data_17[['text']] <-
  text.data_17[['text']] %>%
  sapply(iconv, 'UTF-8', 'ASCII', sub = '') %>%
  cleantext(lower = FALSE) 

text.data_18[['text']] <-
  text.data_18[['text']] %>%
  sapply(iconv,'UTF-8','ASCII', sub = '') %>%
  cleantext(lower =  FALSE)

# attribute_creator <- function(text_data){ 
#   emptylist <- list()
#   for (i in c(1:length(text_data))){
#     saveme <- sentiment_attributes(text_data[i]) #sentiment_attributes is a function in sentimentr
#     emptylist[[i]] <- saveme
#   }
#   return(emptylist)
# }
# 
# attributes_17 <- attribute_creator(text.data_17$text)

sentences_17 <- get_sentences(text.data_17)
sentiment_17 <- sentiment(sentences_17)
sentences_18 <- get_sentences(text.data_18)
sentiment_18 <- sentiment(sentences_18)


#Creation of Filtered Sentiment List ####
sentiment_creator <- function(creation){
  sentiment_zeroless <- creation[which(creation$sentiment != 0)]
  createList <- list() #Empty list to save df
  for (i in c(1:max(creation$element_id))) {
    df <- data.frame(sentiment_zeroless[which(sentiment_zeroless$element_id == i)])
    createList[[i]] <- df
  }
  return(createList)
}

x <- sentiment_creator(sentiment_17)
x18 <- sentiment_creator(sentiment_18)

#Creation of Unfiltered Sentiment List ####
sentiment_creator_w0 <- function(creation){
  #sentiment_zeroless <- creation[which(creation$sentiment != 0)]
  createList <- list() #Empty list to save df
  for (i in c(1:max(creation$element_id))) {
    df <- data.frame(creation[which(creation$element_id == i)])
    createList[[i]] <- df
  }
  return(createList)
}

y <- sentiment_creator_w0(sentiment_17)
y18 <- sentiment_creator_w0(sentiment_18)

#' Creating individual sections but can be done in no particular order
# 1/2. Average Filtered/Unfiltered Sentiment Score (DONE) ####
#' Code returns a vector of average sentiments

Average_scores <- function(sentiment_data){
  avg_sentiment <- c()
  for (i in c(1:length(sentiment_data))){
    avg_sentiment <- append(avg_sentiment,mean(sentiment_data[[i]]$sentiment))
  }
  return(avg_sentiment)
}




# 3. Largest Happy Island (DONE) ####
#' Code returns a vector


Happy_island <- function(happy){
  store = c()
  for (i in c(1:length(happy))){
    #storange variables
    maxLength = 0
    currentLength = 0
    sentences_journal <- length(happy[[i]]$sentence_id) #To find sentences per journal
    happy_quantile <- quantile(happy[[i]]$sentiment[which(happy[[i]]$sentiment > 0)]) #finding quantiles
    Happy_75_quantile <- happy_quantile[4] #4th index is the 75th quantile which is top 25%
    for (k in c(1:sentences_journal)){ 
      if (is.na(Happy_75_quantile) == TRUE){  #Case 1 accounts for any 
        maxLength = 0
      } else if (happy[[i]]$sentiment[k] >= Happy_75_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1 
        maxLength = currentLength
      }
      else if (happy[[i]]$sentiment[k] >= Happy_75_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength > maxLength){
          maxLength = currentLength 
        }
        currentLength = 0
      }
    } 
    store <- append(store,maxLength)
  }
  return(store)
}

# 4. Largest Sad Island (DONE?) ####
#' Done

Sad_island <- function(sad){
  store = c()
  for (i in c(1:length(sad))){
    #storange variables
    maxLength = 0
    currentLength = 0
    sentences_journal <- length(sad[[i]]$sentence_id) #To find sentences per journal
    sad_quantile <- quantile(sad[[i]]$sentiment[which(sad[[i]]$sentiment < 0)]) #finding quantiles
    sad_25_quantile <- sad_quantile[2] 
    for (k in c(1:sentences_journal)){
      if (is.na(sad_25_quantile) == TRUE){
        maxLength = 0
      }
      else if (sad[[i]]$sentiment[k] <= sad_25_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1
        maxLength = currentLength
      }
      else if (sad[[i]]$sentiment[k] <= sad_25_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength > maxLength){
          maxLength = currentLength 
        }
        currentLength = 0
      }
    } 
    store <- append(store,maxLength)
  }
  return(store)
}
# 5. Normalized Flip Freuqnecy (DONE) ####
#My function ignores neutral sentences 
Flip_frequency <- function(sentiment_data){
  flip_store <- c()
  for (i in c(1:length(sentiment_data))){
    counter = 0 
    for (k in c(2:length(sentiment_data[[i]]$sentiment))){
      if ((sentiment_data[[i]]$sentiment[k-1] > 0) && (sentiment_data[[i]]$sentiment[k] < 0)){
        counter = counter + 1
      } else if ((sentiment_data[[i]]$sentiment[k-1] < 0) && (sentiment_data[[i]]$sentiment[k] > 0)){
        counter = counter + 1
      }
    }
    flip_store <- append(flip_store, counter)
  }
  return(flip_store)
} #This function totally ignores neutral sentences
library(testthat) 
# require(skip)
Flip_frequency_2 <- function(sentiment_data){
  flip_store <- c()
  sentences_store <- c()
  flip_freq <- c()
  for (i in c(1:length(sentiment_data))){
    counter = 0 
    sentences_store <- append(sentences_store,length(sentiment_data[[i]]$sentence_id))
    for (k in c(2:length(sentiment_data[[i]]$sentiment))){
      if(length(sentiment_data[[i]]$sentence_id) <= 1){
        counter = 0 
      }
      else if ((sentiment_data[[i]]$sentiment[k-1] > 0) && (sentiment_data[[i]]$sentiment[k] < 0)){
        counter = counter + 1
      } else if ((sentiment_data[[i]]$sentiment[k-1] < 0) && (sentiment_data[[i]]$sentiment[k] > 0)){
        counter = counter + 1
      } else if (sentiment_data[[i]]$sentiment[k-1] == 0){
        test_this <- tryCatch({
          if(sentiment_data[[i]]$sentiment[k-2] < 0 && sentiment_data[[i]]$sentiment[[k]] > 0 || sentiment_data[[i]]$sentiment[k-2] > 0 && sentiment_data[[i]]$sentiment[[k]] < 0){
            counter = counter + 1
          }}, error = function(skip_it){
            skip
          })
      }
    }
    flip_store <- append(flip_store, counter)
  }
  flip_freq <- flip_store/sentences_store
  df <- data.frame(flip_store,flip_freq)
  return(df)
} 

#This function tries to see if the sentence before the neutral sentence is >/< than 0 and see if it flips with the next sentence
#I use this most of the time 



# filtered_flip_frequency == filtered_flip_frequency_2
#' If you have no neutral sentences/filtered out, you can use the above code to check if the 2 fns are the same

# 6/7. Variance of Filtered/Unfiltered Sentiment (DONE) ####
#' Future note: Can nest with (1)

Variance_calculator <- function(sentiment_data){
  variance_store <- c()
  for (i in c(1:length(sentiment_data))){
    variance_store <- append(variance_store,var(sentiment_data[[i]]$sentiment))
  }
  variance_store[is.na(variance_store)] <- 0
  return(variance_store)
}



# 8. Hill/Crest Spacing (WIP because values are weird) ####

Hill_Spacing <- function(happy, na.value = 0){
  store = c()
  sentence_vector = c()
  journal_sentences = c()
  for (i in c(1:length(happy))){
    #storange variables
    maxLength = 0
    currentLength = 0
    store_vec  = c()
    sentence_vec = c()
    sentences_journal <- length(happy[[i]]$sentence_id) #To find sentences per journal
    journal_sentences <- append(journal_sentences, sentences_journal)
    happy_quantile <- quantile(happy[[i]]$sentiment[which(happy[[i]]$sentiment > 0)]) #finding quantiles
    happy_75_quantile <- happy_quantile[4] #4th index is the 75th quantile which is top 25%
    for (k in c(1:sentences_journal)){
      if (is.na(happy_75_quantile)== TRUE){
        maxLength = 0
        sentence_number = 0
        sentence_vec <- append(sentence_vec, sentence_number)
        store_vec <- append(store_vec, maxLength)
      }
      else if (happy[[i]]$sentiment[k] >= happy_75_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1
        maxLength = currentLength
        sentence_number = k
        sentence_vec <- append(sentence_vec, sentence_number)
        store_vec <- append(store_vec,maxLength)
      }
      else if (happy[[i]]$sentiment[k] >= happy_75_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength >= 1){
          maxLength = currentLength
          sentence_number = k-1 
          sentence_vec <- append(sentence_vec, sentence_number)
          store_vec <- append(store_vec,maxLength)
        }
        currentLength = 0
      }
    } 
    df <- data.frame(sentence_vec,store_vec)
    store[[i]] <- df #order in descending  
    #Comment second block of code and uncomment return(store) to see the df of second block
  }
  #Start of the second part of this function (Goes through multiple if/else statements)
  for (j in c(1:length(store))){ 
    store[[j]] <- store[[j]][order(-store[[j]]$store_vec),]
    if (length(store[[j]]$store_vec) <= 1){
      sentence_vector <- append(sentence_vector, na.value)
    }else if (store[[j]]$store_vec[1]> store[[j]]$store_vec[2] && length(store[[j]]$store_vec) == 2){
      sentence_value = abs(store[[j]]$sentence_vec[2] - store[[j]]$sentence_vec[1])
      sentence_vector<- append(sentence_vector, sentence_value)
    }else if (store[[j]]$store_vec[1] > store[[j]]$store_vec[2] && store[[j]]$store_vec[2] == store[[j]]$store_vec[3]){
      second_highest_value <- store[[j]]$store_vec[2]
      second_highest_sentences <- store[[j]]$sentence_vec[which(store[[j]]$store_vec ==second_highest_value)]
      second_highest_sentences <- abs(second_highest_sentences - store[[j]]$sentence_vec[1])
      sentence_vector <- append(sentence_vector,min(second_highest_sentences))
    } else if (store[[j]]$store_vec[1] == store[[j]]$store_vec[2]){
      sentence_toggle <- c() 
      for (a in c(1:length(store[[j]]$store_vec))){
        if (store[[j]]$store_vec[a] == max(store[[j]]$store_vec)){
          sentence_diff_same = abs(store[[j]]$sentence_vec[a] - store[[j]]$sentence_vec[a+1])
          sentence_toggle <- append(sentence_toggle, sentence_diff_same)
        } 
      } 
      sentence_vector <- append(sentence_vector, min(sentence_toggle,na.rm = T))
    } else if (store[[j]]$store_vec[1] > store[[j]]$store_vec[2]) {
      sentence_diff = abs(store[[j]]$sentence_vec[1] - store[[j]]$sentence_vec[2])
      sentence_vector <- append(sentence_vector, sentence_diff)
    }
  }
  new_df <- data.frame(journal_sentences, sentence_vector)
  new_df <- new_df %>% 
    mutate(Spacing = sentence_vector/journal_sentences)
  new_df <- new_df[,2:3]
  colnames(new_df) <- c("H_Min_Sent_Split","Hill_Spacing")
  return(new_df)
  #return(store)
}

#' Total_Sent(Journal_sentences), is the total number of sentences in that text sample
#' Min_Sent_Split(sentence_vector), is the MINIMUM number of sentences between two of the top 2 happy island
#' Hill Spacing, is the (MINIMUM number of sentences between top 2 happy island)/(Total_Sentences)
 


# 9. Trough Spacing (WIP because Values are weird) ####
Trough_Spacing <- function(sad, na.value = 0) {
  store = c()
  sentence_vector = c()
  journal_sentences = c()
  for (i in c(1:length(sad))) {
    #storange variables
    maxLength = 0
    currentLength = 0
    store_vec  = c()
    sentence_vec = c()
    sentences_journal <- length(sad[[i]]$sentence_id) #To find sentences per journal
    journal_sentences <- append(journal_sentences, sentences_journal)
    sad_quantile <- quantile(sad[[i]]$sentiment[which(sad[[i]]$sentiment < 0)]) #finding quantiles
    sad_25_quantile <- sad_quantile[2] #2nd index is the 25th% 
    for (k in c(1:sentences_journal)){
      if(is.na(sad_25_quantile) ==  TRUE){
        maxLength = 0
        sentence_number = 0
        sentence_vec <- append(sentence_vec, sentence_number)
        store_vec <- append(store_vec, maxLength)
      }
      else if (sad[[i]]$sentiment[k] <= sad_25_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1
        maxLength = currentLength
        sentence_number = k
        sentence_vec <- append(sentence_vec, sentence_number)
        store_vec <- append(store_vec,maxLength)
      } else if (sad[[i]]$sentiment[k] <= sad_25_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength >= 1) {
          maxLength = currentLength
          sentence_number = k-1 
          sentence_vec <- append(sentence_vec, sentence_number)
          store_vec <- append(store_vec,maxLength)
        }
        currentLength = 0
      }
    } 
    df <- data.frame(sentence_vec,store_vec)
    store[[i]] <- df #order in descending 
  } 
  for (j in c(1:length(store))){ 
    store[[j]] <- store[[j]][order(-store[[j]]$store_vec),]
    if (length(store[[j]]$store_vec) <= 1){
      sentence_vector <- append(sentence_vector, na.value) 
    }else if (store[[j]]$store_vec[1]> store[[j]]$store_vec[2] && length(store[[j]]$store_vec) == 2){
      sentence_value = abs(store[[j]]$sentence_vec[2] - store[[j]]$sentence_vec[1])
      sentence_vector<- append(sentence_vector, sentence_value)
    }else if (store[[j]]$store_vec[1] > store[[j]]$store_vec[2] && store[[j]]$store_vec[2] == store[[j]]$store_vec[3]){
      second_highest_value <- store[[j]]$store_vec[2]
      second_highest_sentences <- store[[j]]$sentence_vec[which(store[[j]]$store_vec ==second_highest_value)]
      second_highest_sentences <- abs(second_highest_sentences - store[[j]]$sentence_vec[1])
      sentence_vector <- append(sentence_vector,min(second_highest_sentences))
    } else if (store[[j]]$store_vec[1] == store[[j]]$store_vec[2]){
      sentence_toggle <- c() 
      for (a in c(1:length(store[[j]]$store_vec))){
        if (store[[j]]$store_vec[a] == max(store[[j]]$store_vec)){
          sentence_diff_same = abs(store[[j]]$sentence_vec[a] - store[[j]]$sentence_vec[a+1])
          sentence_toggle <- append(sentence_toggle, sentence_diff_same)
        } 
      } 
      sentence_vector <- append(sentence_vector, min(sentence_toggle,na.rm = T))
    } else if (store[[j]]$store_vec[1] > store[[j]]$store_vec[2]) {
      sentence_diff = abs(store[[j]]$sentence_vec[1] - store[[j]]$sentence_vec[2])
      sentence_vector <- append(sentence_vector, sentence_diff)
    }
  }
  new_df <- data.frame(journal_sentences, sentence_vector)
  new_df <- new_df %>% 
    mutate(Spacing = sentence_vector/journal_sentences)
  new_df <- new_df[,2:3]
  colnames(new_df)<- c("T_Min_Sent_Split","Trough_Spacing")
  return(new_df)
}





# 10./11/13 Highest Happy/Sad Island (Value)+ Var (DONE)
# 13. Variance of largest happy island & variance of largest sad island (DONE) - Inside 10/11 ####
# I used x from (1)
#' Future note: Can nest with (1)


Happy_tree_island <- function(happy){
  store = c()
  max_store = c()
  max_sentiment = c()
  sentiment_var = c()
  for (i in c(1:length(happy))){
    #storange variables
    maxLength = 0
    currentLength = 0
    maxID = 0
    sentences_journal <- length(happy[[i]]$sentence_id) #To find sentences per journal
    happy_quantile <- quantile(happy[[i]]$sentiment[which(happy[[i]]$sentiment > 0)]) #finding quantiles
    Happy_75_quantile <- happy_quantile[4] #4th index is the 75th quantile which is top 25%
    for (k in c(1:sentences_journal)){
      if(is.na(Happy_75_quantile) == TRUE){
        maxLength = 0
        maxID = 0
      } else if (happy[[i]]$sentiment[k] >= Happy_75_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1
        maxLength = currentLength
        maxID = k
      }
      else if (happy[[i]]$sentiment[k] >= Happy_75_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength > maxLength){
          maxLength = currentLength 
          maxID = k 
        }
        currentLength = 0
      }
    } 
    max_store <- append(max_store,maxID)
    store <- append(store,maxLength)
  }
  df <- data.frame(max_store, store)
  for (j in c(1:length(df$max_store))){
    island_sentiment <- c()
    max_sentiment_store = 0
    island_sentiment <- happy[[j]]$sentiment[(df$max_store[j]-df$store[j]):(df$max_store[j]-1)]
    max_sentiment_store <- max(island_sentiment)
    max_sentiment <- append(max_sentiment, max_sentiment_store)
    sentiment_var <- append(sentiment_var, var(island_sentiment))
  }
  df <- cbind(df, max_sentiment)
  df <- cbind(df, sentiment_var)
  df <- df[,3:4]
  colnames(df) <- c("MaxSentiment","Var")
  df[is.na(df)] = 0
  df[df == -Inf] <- 0
  df[df == Inf] <- 0
  return(df)
}

e <- Happy_tree_island(x)
d <- Happy_tree_island(x)
a <- Sad_tree_island(x)

# 11. Highest Sad Island (value)
Sad_tree_island <- function(sad){
  store = c()
  max_store = c()
  min_sentiment = c()
  sent_var = c()
  for (i in c(1:length(sad))){
    #storange variables
    maxLength = 0
    currentLength = 0
    maxID = 0
    sentences_journal <- length(sad[[i]]$sentence_id) #To find sentences per journal
    sad_quantile <- quantile(sad[[i]]$sentiment[which(sad[[i]]$sentiment < 0)]) #finding quantiles
    sad_25_quantile <- sad_quantile[2]  
    for (k in c(1:sentences_journal)){
      if (is.na(sad_25_quantile) ==  TRUE) {
        maxLength = 0
        maxID = 0
      } else if (sad[[i]]$sentiment[k] <= sad_25_quantile && k == sentences_journal && currentLength >= maxLength){
        currentLength = currentLength + 1
        maxLength = currentLength
        maxID = k
      }
      else if (sad[[i]]$sentiment[k] < sad_25_quantile) {
        currentLength = currentLength + 1
      } else {
        if (currentLength > maxLength){
          maxLength = currentLength 
          maxID = k 
        }
        currentLength = 0
      }
    } 
    max_store <- append(max_store,maxID)
    store <- append(store,maxLength)
  }
  df <- data.frame(max_store, store)
  for (j in c(1:length(df$max_store))){
    island_sentiment <- c()
    min_sentiment_store = 0 
    island_sentiment <- sad[[j]]$sentiment[(df$max_store[j]-df$store[j]):(df$max_store[j]-1)]
    min_sentiment_store <- min(island_sentiment)
    min_sentiment <- append(min_sentiment, min_sentiment_store)
    sent_var <- append(sent_var, var(island_sentiment))
  }
  df <- cbind(df, min_sentiment)
  df <- cbind(df, sent_var)
  df <- df[,3:4]
  colnames(df) <- c("MinSentiment","Var")
  df[is.na(df)] = 0
  df[df == -Inf] <- 0
  df[df == Inf] <- 0
  return(df)
}




# 14. R^2 of ECG to smoothing function (EMPTY) ####

# install.packages("Metrics")
library(Metrics)
RMSE_counter <- function(sentiment_data,smoothed_data){
  RMSE_Journal <- c()
  for (i in c(1:length(1:length(sentiment_data)))){
    actual <- sentiment_data[[i]]$sentiment
    predicted <- smoothed_data[[i]]$sentiment
    RMSE_Journal <- append(RMSE_Journal,rmse(actual,predicted))
  }
  return(RMSE_Journal)
}

#WARNING#
#This can only run if you run the bottom (smoothing avg) first 

# 15. Ratio of positive peaks to negative peaks(DONE) ####
#' Need a more proper definition of peaks? Negative peaks means in negative space? What about a drop in value but still in positive space
#' Code currently does not take into account if 1st or last sentence is a peak
#' Returns a dataframe

Peaks_ratio <- function(sentiment_data){
  positive_peaks <- c()
  negative_peaks <- c()
  peak_ratio <- c()
  for (i in c(1:length(sentiment_data))){
    positive_peaks_counter = 0 
    negative_peaks_counter = 0
    for (k in c(1:length(sentiment_data[[i]]$sentiment))){ 
      if (sentiment_data[[i]]$sentiment[k] > 0){
        tryCatch(if (sentiment_data[[i]]$sentiment[k] > sentiment_data[[i]]$sentiment[k-1] && sentiment_data[[i]]$sentiment[k]> sentiment_data[[i]]$sentiment[k+1]){
          positive_peaks_counter = positive_peaks_counter + 1
        }, error = function(skip_this){
          skip
          })
      } else if (sentiment_data[[i]]$sentiment[k]<0) {
        tryCatch(if (sentiment_data[[i]]$sentiment[k] < sentiment_data[[i]]$sentiment[k-1] && sentiment_data[[i]]$sentiment[k]< sentiment_data[[i]]$sentiment[k+1]){
          negative_peaks_counter = negative_peaks_counter + 1
        }, error = function(skip_this){
          skip
          })
      }
    }
    positive_peaks <- append(positive_peaks,positive_peaks_counter)
    negative_peaks <- append(negative_peaks,negative_peaks_counter)
    peak_ratio <- append(peak_ratio, (positive_peaks_counter - negative_peaks_counter))
  }
  df <- data.frame(positive_peaks,negative_peaks,peak_ratio)
  df[df == Inf] <- 0  
  df$peak_ratio[is.nan(df$peak_ratio)] <- 0 
  df[df == -Inf] <- 0 
  return(df)
}


Peaks_ratio(x)


# 16. Ratio of average positive peaks to average negative peaks (DONE) ####
#' I abs the negative values for negative peaks

Avg_Peaks_ratio <- function(sentiment_data){
  avg_peak_ratio <- c()
  sum_positive_peak <- c()
  sum_negative_peak <- c()
  for (i in c(1:length(sentiment_data))){
    positive_peaks <- c()
    negative_peaks <- c()
    ratio = 0
    for (k in c(1:length(sentiment_data[[i]]$sentiment)))
      if (sentiment_data[[i]]$sentiment[k] > 0){
        tryCatch(if (sentiment_data[[i]]$sentiment[k] > sentiment_data[[i]]$sentiment[k-1] && sentiment_data[[i]]$sentiment[k]> sentiment_data[[i]]$sentiment[k+1]){
          positive_peaks <- append(positive_peaks,sentiment_data[[i]]$sentiment[k])
        }, error = function(skip_this){
          skip
        })
      } else if (sentiment_data[[i]]$sentiment[k]<0) {
        tryCatch(if (sentiment_data[[i]]$sentiment[k] < sentiment_data[[i]]$sentiment[k-1] && sentiment_data[[i]]$sentiment[k]< sentiment_data[[i]]$sentiment[k+1]){
          negative_peaks <- append(negative_peaks,sentiment_data[[i]]$sentiment[k])
        }, error = function(skip_this){
          skip
        })
      }
    sum_positive_peak <- append(sum_positive_peak, sum(positive_peaks))
    sum_negative_peak <- append(sum_negative_peak, abs(sum(negative_peaks)))
    ratio = sum(positive_peaks) - abs(sum(negative_peaks))
    avg_peak_ratio <- append(avg_peak_ratio, ratio)
  }
  df <- data.frame(sum_positive_peak,sum_negative_peak, avg_peak_ratio)
  df[df == Inf] <- 0  
  df$avg_peak_ratio[is.nan(df$avg_peak_ratio)] <- 0 
  df[df == -Inf] <- 0 
  return(df)
}




# Transfer every single one of the variables into 1 dataframe  ####

avg_filtered_sentiment <- Average_scores(x18)
avg_unfiltered_sentiment <- Average_scores(y18)
filtered_happy_island <- Happy_island(x)
unfiltered_happy_island <- Happy_island(y18)
filtered_sad_island <- Sad_island(x18)
unfiltered_sad_island <- Sad_island(y18)
unfiltered_flip_frequency <- Flip_frequency_2(y18)
filtered_flip_frequency <- Flip_frequency_2(x18)
variance_filtered_sentiment <- Variance_calculator(x18)
variance_unfiltered_sentiment <- Variance_calculator(y18)
filtered_hill_spacing <- Hill_Spacing(x18, na.value = 0)
unfiltered_hill_spacing <- Hill_Spacing(y18, na.value = 0)
filtered_trough_spacing <- Trough_Spacing(x18, na.value = 0)
unfiltered_trough_spacing <- Trough_Spacing(y18, na.value = 0)
filtered_max_happy_island <- Happy_tree_island(x18)
unfiltered_max_happy_island <- Happy_tree_island(y18)
filtered_min_sad_island <- Sad_tree_island(x18)
unfiltered_min_sad_island <- Sad_tree_island(y18)
RMSE_Filtered <- RMSE_counter(x18,smoothing_sentiment_filtered) #rmb to run the one below if you using new dataset
RMSE_Unfiltered <- RMSE_counter(y18, smoothing_sentiment_unfiltered)
unfiltered_peaks_ratio <- Peaks_ratio(y18)
filtered_peaks_ratio <- Peaks_ratio(x18)
filtered_avg_peak_ratio <- Avg_Peaks_ratio(x18) 
unfiltered_avg_peak_ratio <- Avg_Peaks_ratio(y18)



All_tgt_now_2 <- data.frame(avg_filtered_sentiment, avg_unfiltered_sentiment,filtered_happy_island,unfiltered_happy_island,
                          filtered_sad_island, unfiltered_sad_island,filtered_flip_frequency,unfiltered_flip_frequency,
                          variance_filtered_sentiment,variance_unfiltered_sentiment,filtered_hill_spacing,unfiltered_hill_spacing,
                          filtered_trough_spacing,unfiltered_trough_spacing, filtered_max_happy_island, unfiltered_max_happy_island,
                          filtered_min_sad_island, unfiltered_min_sad_island,RMSE_Filtered,RMSE_Unfiltered,filtered_peaks_ratio,unfiltered_peaks_ratio,
                          filtered_avg_peak_ratio,unfiltered_avg_peak_ratio)

manynames <- c("FS_AVG","UFS_AVG","FS_HI","UFS_HI","FS_SI","UFS_SI","FS_NFF","FS_FF","UFS_NFF","UFS_FF",
               "FS_VAR","UFS_VAR","FS_HMSS","FS_HS","UFS_HMSS","UFS_HS",
               "FS_TMSS","FS_TS","UFS_TMSS","UFS_TS",
               "FS_MAXHI","FS_HVAR","UFS_MAXHI","UFS_HVAR",
               "FS_MINSI","FS_SVAR","UFS_MINSI","UFS_SVAR",
               "FS_RMSE","UFS_RMSE",
               "FS_PP","FS_NP","FS_PR","UFS_PP","UFS_NP","UFS_PR",
               "FS_SPP","FS_SNP","FS_APR","UFS_SPP","UFS_SNP","UFS_APR")


# LEGENDS (PLEASE READ THIS SECTION)####
#' #####################################
#' LHS_RHS ---- (LHS) Labels          ##
#' FS - Filtered Sentiment            ##
#' UFS - Unfiltered Sentiment         ##
#' #####################################
#' LHS_RHS ---- (RHS) Labels          ##
#' AVG - Average sentiment            ##
#' HI - Length of Happy Island        ##
#' SI - Length of Sad Island          ##
#' FF - Flip Frequency Value          ##
#' NFF - Number of Flips              ##
#' VAR - Variance of Sentiment Scores ##
#' HMSS - Hill Min Sentence Split     ##
#' HS - Hill Spacing                  ##
#' TMSS - Trough Min Sentence Split   ##
#' TS - Trough Spacing                ##
#' MAXHI - Maximum Sentiment of HI    ##
#' HVAR - Variance of Happy Island    ##
#' MINSI - Minimum Sentiment of SI    ##
#' SVAR - Variance of Sad Island      ##
#' RMSE - Root Mean Square Error      ##
#' PP - Positive Peaks                ##
#' NP - Negative Peaks                ##
#' PR - Peaks Ratio (PP:NP)           ##
#' SPP - Sum of Sentiment of PP       ##
#' SNP - Sum of Sentiment of NP       ##
#' APR - Average Peak Ratio (SPP/SNP) ##
#' #####################################

colnames(All_tgt_now_2) <- manynames

write.csv(All_tgt_now_2, "Feature_Creation_18_19.csv", quote = F, row.names = F)

#----------------SECTION BREAK-------------------
#' This section is to redo the top parts but done after smoothing 
# 12. Simple Moving Average (Smoothing Fn) DONE ####

library(TTR)


moving_avg <- function(sentiment_data, window = 3, remove.na = FALSE){ #I default this function to have a window of 3
  backup_sentiment <- sentiment_data
  for (i in c(1:length(sentiment_data))){
    if (length(sentiment_data[[i]]$sentence_id) <= window) {
      skip
    }
    else if (remove.na == TRUE){ #remove.na changes the "windows" from NA to take the same value as the initial
      sentiment_data[[i]]$sentiment <- SMA(sentiment_data[[i]]$sentiment, n = window)
      for (k in c(1:(window-1))){
        sentiment_data[[i]]$sentiment[k] <- backup_sentiment[[i]]$sentiment[k]
      }
    }
  }
  return(sentiment_data)
}

smoothing_sentiment_unfiltered <- moving_avg(y, window = 3 , remove.na = T)
smoothing_sentiment_filtered <- moving_avg(x, window = 3, remove.na = T)

moving_avg(kagglefiltered)


#' New Variables used in this part

smoothing_sentiment_unfiltered #Smoothing Fn of y
smoothing_sentiment_filtered #Smoothing Fn of x

# 2.1. Average Unfiltered Score/Filtered Score ####
avg_smoothed_sentiment_unfiltered <- Average_scores(smoothing_sentiment_unfiltered)
avg_smoothed_sentiment_filtered <- Average_scores(smoothing_sentiment_filtered)

# 2.2. Largest Happy/sad Island ####

smoothed_largest_happy_island <- Happy_island(smoothing_sentiment_filtered)
smoothed_largest_sad_island <- Sad_island(smoothing_sentiment_filtered)
smoothed_largest_happy_island_unfiltered <- Happy_island(smoothing_sentiment_unfiltered)
smoothed_largest_sad_island_unfiltered <- Sad_island(smoothing_sentiment_unfiltered)

# 2.3. Normalized Flip Frequency ####

filtered_smoothed_flip_frequency_2 <- Flip_frequency_2(smoothing_sentiment_filtered) 
#' Some reasoning here: Flip frequency 2 was made for unfiltered version. Because sentence 1 can be + and 
#' sentence 2 is neutral 
#' But sentence 3 is positive again. (So it flips but it shouldn't?)
#' So Flip frequency 2 will check to make sure sentence 1 is +, 2 is 0 and 3 is - and it will flip
#' My estimate is that after smoothing, flip 1 and flip 2 will be more or less the same unless there is a 
#' long period of neutral sentences

unfiltered_smoothed_flip_frequency_2 <- Flip_frequency_2(smoothing_sentiment_unfiltered)

# 2.4. Variance of Filtered/Unfiltered Sentiment ####

filtered_smoothed_var <- Variance_calculator(smoothing_sentiment_filtered)
unfiltered_smoothed_var <- Variance_calculator(smoothing_sentiment_unfiltered)

# 2.5. Hill/Crest/Trough Spacing ####

smoothing_hill_spacing_filtered <- Hill_Spacing(smoothing_sentiment_filtered)
smoothing_hill_spacing_unfiltered <- Hill_Spacing(smoothing_sentiment_unfiltered)
ssf_trough_spacing <- Trough_Spacing(smoothing_sentiment_filtered, na.value = 0)
ssu_trough_spacing <- Trough_Spacing(smoothing_sentiment_unfiltered,na.value = 0)

# 2.6. Largest Happy/Sad Island(Value) + Var ####

ssf_happy_island <- Happy_tree_island(smoothing_sentiment_filtered)
ssu_happy_island <- Happy_tree_island(smoothing_sentiment_unfiltered)
ssf_sad_island <- Sad_tree_island(smoothing_sentiment_filtered)
ssu_sad_island <- Sad_tree_island(smoothing_sentiment_unfiltered)
# 2.7. Ratio of Positive/Negative Peaks ####

ssf_peak_ratio <- Peaks_ratio(smoothing_sentiment_filtered)
ssu_peak_ratio <- Peaks_ratio(smoothing_sentiment_unfiltered)

# 2.8. Ratio of Average Positive/Average Negative Peaks ####

ssf_avg_peak <- Avg_Peaks_ratio(smoothing_sentiment_filtered)
ssu_avg_peak <- Avg_Peaks_ratio(smoothing_sentiment_unfiltered)

# 2.9 RMSE same as ontop
#RMSE_Filtered <- RMSE_counter(x,smoothing_sentiment_filtered)
#RMSE_Unfiltered <- RMSE_counter(y, smoothing_sentiment_unfiltered)

smoothing_all_tgt_now <- data.frame(avg_smoothed_sentiment_filtered,avg_smoothed_sentiment_unfiltered,
                                    smoothed_largest_happy_island,smoothed_largest_sad_island,
                                    smoothed_largest_happy_island_unfiltered,smoothed_largest_sad_island_unfiltered,
                                    filtered_smoothed_flip_frequency_2,unfiltered_smoothed_flip_frequency_2,
                                    filtered_smoothed_var,unfiltered_smoothed_var,
                                    smoothing_hill_spacing_filtered, smoothing_hill_spacing_unfiltered,
                                    ssf_trough_spacing,ssu_trough_spacing,
                                    ssf_happy_island,ssu_happy_island,ssf_sad_island,ssu_sad_island,
                                    RMSE_Filtered,RMSE_Unfiltered,
                                    ssf_peak_ratio,ssu_peak_ratio,ssf_avg_peak,ssu_avg_peak)

smoothed_labels <- c("SFS_AVG","SUFS_AVG",
                     "SFS_HI","SFS_SI",
                     "SUFS_HI","SUFS_SI",
                     "SFS_NFF","SFS_FF","SUFS_NFF","SUFS_FF",
                     "SFS_VAR","SUFS_VAR",
                     "SFS_HMSS","SFS_HS","SUFS_HMSS","SUFS_HS",
                     "SFS_TMSS","SFS_TS","SUFS_TMSS","SFS_TS",
                     "SFS_MAXHI","SFS_HVAR","SUFS_MAXHI","SUFS_HVAR","SFS_MINSI","SFS_SVAR","SUFS_MINSI","SUFS_SVAR",
                     "SFS_RMSE","SUFS_RMSE",
                     "SFS_PP","SFS_NP","SFS_PR","SUFS_PP","SUFS_NP","SUFS_PR","SFS_SPP","SFS_SNP","SFS_APR","SUFS_SPP","SUFS_SNP","SUFS_APR")

colnames(smoothing_all_tgt_now) <- smoothed_labels

# LEGENDS (PLEASE READ THIS SECTION)######
#' #######################################
#' LHS_RHS ---- (LHS) Labels            ##
#' SFS - Smoothed Filtered Sentiment    ##
#' SUFS - Smoothed Unfiltered Sentiment ##
#' #######################################
#' LHS_RHS ---- (RHS) Labels            ##
#' AVG - Average sentiment              ##
#' HI - Length of Happy Island          ##
#' SI - Length of Sad Island            ##
#' FF - Flip Frequency Value            ##
#' NFF - Number of Flips                ##
#' VAR - Variance of Sentiment Scores   ##
#' HMSS - Hill Min Sentence Split       ##
#' HS - Hill Spacing                    ##
#' TMSS - Trough Min Sentence Split     ##
#' TM - Trough Spacing                  ##
#' MAXHI - Maximum Sentiment of HI      ##
#' HVAR - Variance of Happy Island      ##
#' MINSI - Minimum Sentiment of SI      ##
#' SVAR - Variance of Sad Island        ##
#' RMSE - Root Mean Square Error        ##
#' PP - Positive Peaks                  ##
#' NP - Negative Peaks                  ##
#' PR - Peaks Ratio (PP:NP)             ##
#' SPP - Sum of Sentiment of PP         ##
#' SNP - Sum of Sentiment of NP         ##
#' APR - Average Peak Ratio (SPP/SNP)   ##
#' #######################################
write.csv(smoothing_all_tgt_now,"smoothed_feature_creation_18_19.csv", row.names = F,quote =F)


# MAIN FUNCTIION ####

bring_it_on <- function(data, n = 3, pp = 0){
  x18 <- sentiment_creator(data)
  y18 <- sentiment_creator_w0(data)
  avg_filtered_sentiment <- Average_scores(x18)
  avg_unfiltered_sentiment <- Average_scores(y18)
  filtered_happy_island <- Happy_island(x18)
  unfiltered_happy_island <- Happy_island(y18)
  filtered_sad_island <- Sad_island(x18)
  unfiltered_sad_island <- Sad_island(y18)
  unfiltered_flip_frequency <- Flip_frequency_2(y18)
  filtered_flip_frequency <- Flip_frequency_2(x18)
  variance_filtered_sentiment <- Variance_calculator(x18)
  variance_unfiltered_sentiment <- Variance_calculator(y18)
  filtered_hill_spacing <- Hill_Spacing(x18, na.value = pp)
  unfiltered_hill_spacing <- Hill_Spacing(y18, na.value = pp)
  filtered_trough_spacing <- Trough_Spacing(x18, na.value = pp)
  unfiltered_trough_spacing <- Trough_Spacing(y18, na.value = pp)
  filtered_max_happy_island <- Happy_tree_island(x18)
  unfiltered_max_happy_island <- Happy_tree_island(y18)
  filtered_min_sad_island <- Sad_tree_island(x18)
  unfiltered_min_sad_island <- Sad_tree_island(y18)
  smoothing_sentiment_unfiltered <- moving_avg(y18, window = n , remove.na = T)
  smoothing_sentiment_filtered <- moving_avg(x18, window = n, remove.na = T)
  RMSE_Filtered <- RMSE_counter(x18,smoothing_sentiment_filtered)
  RMSE_Unfiltered <- RMSE_counter(y18, smoothing_sentiment_unfiltered)
  unfiltered_peaks_ratio <- Peaks_ratio(y18)
  filtered_peaks_ratio <- Peaks_ratio(x18)
  filtered_avg_peak_ratio <- Avg_Peaks_ratio(x18) 
  unfiltered_avg_peak_ratio <- Avg_Peaks_ratio(y18)
  
  
  #Joining all the features into a df
  All_tgt_now_2 <- data.frame(avg_filtered_sentiment, avg_unfiltered_sentiment,filtered_happy_island,unfiltered_happy_island,
                              filtered_sad_island, unfiltered_sad_island,filtered_flip_frequency,unfiltered_flip_frequency,
                              variance_filtered_sentiment,variance_unfiltered_sentiment,filtered_hill_spacing,unfiltered_hill_spacing,
                              filtered_trough_spacing,unfiltered_trough_spacing, filtered_max_happy_island, unfiltered_max_happy_island,
                              filtered_min_sad_island, unfiltered_min_sad_island,RMSE_Filtered,RMSE_Unfiltered,
                              filtered_peaks_ratio,unfiltered_peaks_ratio,
                              filtered_avg_peak_ratio,unfiltered_avg_peak_ratio)
  #Renaming all the features
  manynames <- c("FS_AVG","UFS_AVG","FS_HI","UFS_HI","FS_SI","UFS_SI","FS_NFF","FS_FF","UFS_NFF","UFS_FF",
                 "FS_VAR","UFS_VAR","FS_HMSS","FS_HS","UFS_HMSS","UFS_HS",
                 "FS_TMSS","FS_TS","UFS_TMSS","UFS_TS",
                 "FS_MAXHI","FS_HVAR","UFS_MAXHI","UFS_HVAR",
                 "FS_MINSI","FS_SVAR","UFS_MINSI","UFS_SVAR",
                 "FS_RMSE","UFS_RMSE",
                 "FS_PP","FS_NP","FS_PR","UFS_PP","UFS_NP","UFS_PR",
                 "FS_SPP","FS_SNP","FS_APR","UFS_SPP","UFS_SNP","UFS_APR")
  
  colnames(All_tgt_now_2) <- manynames
  return(All_tgt_now_2)
}
smoothing_bring_it_on <- function(x,y){ 
  smoothing_sentiment_unfiltered <- moving_avg(y, window = 3 , remove.na = T)
  smoothing_sentiment_filtered <- moving_avg(x, window = 3, remove.na = T)
  avg_smoothed_sentiment_unfiltered <- Average_scores(smoothing_sentiment_unfiltered)
  avg_smoothed_sentiment_filtered <- Average_scores(smoothing_sentiment_filtered)
  smoothed_largest_happy_island <- Happy_island(smoothing_sentiment_filtered)
  smoothed_largest_sad_island <- Sad_island(smoothing_sentiment_filtered)
  smoothed_largest_happy_island_unfiltered <- Happy_island(smoothing_sentiment_unfiltered)
  smoothed_largest_sad_island_unfiltered <- Sad_island(smoothing_sentiment_unfiltered)
  filtered_smoothed_flip_frequency_2 <- Flip_frequency_2(smoothing_sentiment_filtered) 
  unfiltered_smoothed_flip_frequency_2 <- Flip_frequency_2(smoothing_sentiment_unfiltered)
  filtered_smoothed_var <- Variance_calculator(smoothing_sentiment_filtered)
  unfiltered_smoothed_var <- Variance_calculator(smoothing_sentiment_unfiltered)
  smoothing_hill_spacing_filtered <- Hill_Spacing(smoothing_sentiment_filtered)
  smoothing_hill_spacing_unfiltered <- Hill_Spacing(smoothing_sentiment_unfiltered)
  ssf_trough_spacing <- Trough_Spacing(smoothing_sentiment_filtered, na.value = 0)
  ssu_trough_spacing <- Trough_Spacing(smoothing_sentiment_unfiltered,na.value = 0)
  ssf_happy_island <- Happy_tree_island(smoothing_sentiment_filtered)
  ssu_happy_island <- Happy_tree_island(smoothing_sentiment_unfiltered)
  ssf_sad_island <- Sad_tree_island(smoothing_sentiment_filtered)
  ssu_sad_island <- Sad_tree_island(smoothing_sentiment_unfiltered)
  ssf_peak_ratio <- Peaks_ratio(smoothing_sentiment_filtered)
  ssu_peak_ratio <- Peaks_ratio(smoothing_sentiment_unfiltered)
  ssf_avg_peak <- Avg_Peaks_ratio(smoothing_sentiment_filtered)
  ssu_avg_peak <- Avg_Peaks_ratio(smoothing_sentiment_unfiltered)
  RMSE_Filtered <- RMSE_counter(x,smoothing_sentiment_filtered)
  RMSE_Unfiltered <- RMSE_counter(y, smoothing_sentiment_unfiltered)
  
  smoothing_all_tgt_now <- data.frame(avg_smoothed_sentiment_filtered,avg_smoothed_sentiment_unfiltered,
                                      smoothed_largest_happy_island,smoothed_largest_sad_island,
                                      smoothed_largest_happy_island_unfiltered,smoothed_largest_sad_island_unfiltered,
                                      filtered_smoothed_flip_frequency_2,unfiltered_smoothed_flip_frequency_2,
                                      filtered_smoothed_var,unfiltered_smoothed_var,
                                      smoothing_hill_spacing_filtered, smoothing_hill_spacing_unfiltered,
                                      ssf_trough_spacing,ssu_trough_spacing,
                                      ssf_happy_island,ssu_happy_island,ssf_sad_island,ssu_sad_island,
                                      RMSE_Filtered,RMSE_Unfiltered,
                                      ssf_peak_ratio,ssu_peak_ratio,ssf_avg_peak,ssu_avg_peak)
  
  smoothed_labels <- c("SFS_AVG","SUFS_AVG",
                       "SFS_HI","SFS_SI",
                       "SUFS_HI","SUFS_SI",
                       "SFS_NFF","SFS_FF","SUFS_NFF","SUFS_FF",
                       "SFS_VAR","SUFS_VAR",
                       "SFS_HMSS","SFS_HS","SUFS_HMSS","SUFS_HS",
                       "SFS_TMSS","SFS_TS","SUFS_TMSS","SFS_TS",
                       "SFS_MAXHI","SFS_HVAR","SUFS_MAXHI","SUFS_HVAR","SFS_MINSI","SFS_SVAR","SUFS_MINSI","SUFS_SVAR",
                       "SFS_RMSE","SUFS_RMSE",
                       "SFS_PP","SFS_NP","SFS_PR","SUFS_PP","SUFS_NP","SUFS_PR","SFS_SPP","SFS_SNP","SFS_APR","SUFS_SPP","SUFS_SNP","SUFS_APR")
  
  colnames(smoothing_all_tgt_now) <- smoothed_labels
  
  return(smoothing_all_tgt_now)
}
#----------------------------------------------------------------------------------------Section Break ####
#'
#Corelation Plots####

library(corrplot)

# Correlation Plot for Filtered/Unfiltered No Split

results <- cor(All_tgt_now_2)
pdf("correlationplot.pdf")
corrplot(results, type = "upper",order = "hclust",tl.col = "black",tl.srt = 45,tl.cex = 0.45)
dev.off()

# Correlation Plot for Filtered/Unfiltered WITH split

give_me_names <- colnames(All_tgt_now_2)
give_me_names <- sort(give_me_names)
reordered_df <- All_tgt_now_2[,give_me_names]
filtered_data <- reordered_df[,c(1:21)]
unfiltered_data <- reordered_df[,c(22:42)]
results4 <- cor(filtered_data)
results5 <- cor(unfiltered_data)

pdf("filtered_corplot.pdf")
corrplot(results4, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
dev.off()

pdf("unfiltered_corplot.pdf")
corrplot(results5, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
dev.off()



# Correlation Plot for Smoothed No Split
results2 <- cor(smoothing_all_tgt_now)
pdf("correlationplot2.pdf")
corrplot(results2,type = "upper", order ="hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.45)
dev.off()

# Correlation Plot for Smoothed w Split

hand_me_names <- colnames(smoothing_all_tgt_now)
hand_me_names <- sort(hand_me_names)
smoothing_sorted <- smoothing_all_tgt_now[,hand_me_names]
filtered_smooth <- smoothing_sorted[,c(1:21)]
unfiltered_smooth <- smoothing_sorted[,c(22:42)]
results6 <- cor(filtered_smooth)
results7 <- cor(unfiltered_smooth)

pdf("FSmooth_corplot.pdf")
corrplot(results6, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
dev.off()

pdf("UFSmooth_corplot.pdf")
corrplot(results7, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.55)
dev.off()

# Correlation Plot for Both Tgt (Doing this to make sure that the features I think should be perfectly correlated are)
complete_df <- cbind(All_tgt_now,smoothing_all_tgt_now)
results3 <- cor(complete_df)
pdf("CompleteCorplot.pdf")
corrplot(results3, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.35)
dev.off()

# Multiple Regression Model
grades <- readxl::read_excel("Master_List.xls")
grades_only <- grades[1:21,5]


colnames(All_tgt_now)
post_only <- seq(1,42,2)
post_only_n <- All_tgt_now[post_only,]
post_only_n <- cbind(post_only_n,grades_only)


model <- lm(formula = Grades ~ FS_AVG + FS_HI + FS_SI + FS_FF + FS_VAR + FS_HS +FS_TS +
              FS_VAR + FS_MAXHI + FS_HVAR +FS_MINSI +FS_SVAR + FS_RMSE +FS_PR +FS_PP +FS_NP +FS_APR,
            data = post_only_n)
summary(model)
pdf("models.pdf")
plot(model)
dev.off()

sink("model.txt")
print(summary(model))
sink()





