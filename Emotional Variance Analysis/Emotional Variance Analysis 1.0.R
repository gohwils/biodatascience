#####################
# importing materials
#####################

# import libraries
library(readtext)
library(dplyr)
library(sentimentr)
library(tidyverse)
library(TTR)
library(testthat)
library(Metrics)
library(rstudioapi)

# import transcripts
setwd(dirname(getActiveDocumentContext()$path))
curr_dir <- getwd()
transcript_folder <- "/Transcripts"
transcript_path <- paste0(curr_dir, transcript_folder)

text_files <- list.files(path=transcript_path, pattern=".txt", full.names = T)

# store transcripts in data frame
transcript_df <- readtext(text_files)
transcript_id <- basename(text_files)

######################
# pre-processing texts
######################
# remove symbols from transcription key
transcript_df <- transcript_df %>%
  mutate(text=gsub("Transcriber.*?2025", "", text)) %>% # transcriber information
  mutate(text=gsub("I:.*?P:", "", text)) %>% # interviewer speech
  mutate(text=gsub("P:|I:", "", text)) %>% # speaker label
  mutate(text=gsub("\\b\\w+-\\b", "", text)) %>% # word cut off
  mutate(text=gsub("\\*+", "", text)) %>% # redacted information
  mutate(text=gsub("<.*?>", "", text)) %>% # deviated pronunciations
  mutate(text=gsub("X+", "", text)) %>% # uncertain syllable
  mutate(text=gsub("<X>|</X>", "", text)) %>% # uncertain words
  mutate(text=gsub("\\[.*?\\]", "", text)) %>% # transcriber's comments
  mutate(text=gsub("-", "", text)) %>% # stress
  mutate(text=gsub("<S>", "", text)) %>% # utterance
  mutate(text=gsub("<Z>", ".", text)) %>% # silence
  mutate(text=gsub("=", "", text)) %>% # lengthening
  mutate(text=gsub("\\(H(x)?\\)", "", text)) %>% # vocal noise
  mutate(text=gsub("@", "", text)) %>% # laughter
  mutate(text=gsub("(\\.{2,4})(?=\\s(?!I)[A-Z])", ".", text, perl=T)) %>% # pauses before uppercase
  mutate(text=gsub("\\.{2,4}", "", text, perl=T)) %>% # pauses in mid sentence
  mutate(text=gsub("[↑↓]", "", text)) %>% # pauses in mid sentence
  mutate(text=gsub("\\{.*?\\}", "", text)) %>% # chinese pin yin
  mutate(text=str_squish(text)) # remove white spaces


# split each transcript into sentences
sentence_df <- get_sentences(transcript_df)

# calculate sentiment for each sentence
sentiment_df <- sentiment(sentence_df)

# remove sentences with NA word count
sentiment_df <- na.omit(sentiment_df)

# reset sentence_id
sentiment_df <- sentiment_df %>%
  group_by(element_id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup()

#################################################
# function to keep sentences with |sentiment| > 0
# returns a list of filtered data frames, each corresponding to 1 transcript
#################################################
get_filtered_sentiment <- function(sentiment_df){

  filtered_sentiment_df <- sentiment_df[which(sentiment_df$sentiment != 0), ]
  filtered_list <- list() # initialize filtered list

  for (i in 1:max(sentiment_df$element_id)){

    df <- data.frame(filtered_sentiment_df[which(filtered_sentiment_df$element_id == i), ])
    filtered_list[[i]] <- df

  }

  return(filtered_list)
}

################################
# function to keep all sentences
# returns a list of unfiltered data frames, each corresponding to 1 transcript
################################
get_unfiltered_sentiment <- function(sentiment_df){

  unfiltered_list <- list() # initialize unfiltered list

  for (i in 1:max(sentiment_df$element_id)){

    df <- data.frame(sentiment_df[which(sentiment_df$element_id == i), ])
    unfiltered_list[[i]] <- df

  }

  return(unfiltered_list)
}

####################
###### EVA 1.0 #####
####################

########################################
# EVA Feature 1: Average Sentiment (AVG)
# 1.1 AVG (filtered)
# 1.2 AVG (unfiltered)
# Description: mean sentiment score of all sentences in a text
########################################
get_eva_avg <- function(sentiment_df_list){

  eva_avg <- c() # initialize output vector

  for (i in 1:length(sentiment_df_list)){
    eva_avg <- append(eva_avg, mean(sentiment_df_list[[i]]$sentiment))
  }

  eva_avg[is.na(eva_avg)] <- 0 # AVG values of transcripts with 0 sentences will be NA

  return(eva_avg)
}

##########################################
# EVA Feature 2: Longest Happy Island (HI)
# 2.1 HI (filtered)
# 2.2 HI (unfiltered)
# Description: largest number of adjacent sentences with sentiment scores within
# top 25% of all positive sentiment scores
##########################################
get_eva_hi <- function(sentiment_df_list){

  eva_hi <- c() # initialize output vector

  for (i in 1:length(sentiment_df_list)){

    maxLength = 0 # length of longest island
    currentLength = 0 # length of current island
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # get 4th quantile of positive sentiments
    positive_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment > 0)])
    q4 <- positive_quantile[4]

    # check if there are positive sentiments (4th quantile is NOT null)
    if (!is.na(q4)){

      for (k in 1:n_sentences){

        # Case: current sentence is in 4th quantile
        if (sentiment_df_list[[i]]$sentiment[k] >= q4){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){
            maxLength = currentLength
          }

        # Case: current sentence is not in 4th quantile
        } else {

          # Sub-case: current island is the longest
          if (currentLength > maxLength){
            maxLength = currentLength
          }

          currentLength = 0
        }
      }
    }

    eva_hi <- append(eva_hi, maxLength) # update output vector
  }

  return(eva_hi)
}

########################################
# EVA Feature 3: Longest Sad Island (SI)
# 3.1 SI (filtered)
# 3.2 SI (unfiltered)
# Description: largest number of adjacent sentences with sentiment scores within
# bottom 25% of all positive sentiment scores
########################################
get_eva_si <- function(sentiment_df_list){

  eva_si <- c() # initialize output vector

  for (i in 1:length(sentiment_df_list)){

    maxLength = 0 # length of longest island
    currentLength = 0 # length of current island
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # get 1st quantile of negative sentiments
    negative_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment < 0)])
    q1 <- negative_quantile[2]

    # check if there are negative sentiments (1st quantile is NOT null)
    if (!is.na(q1)){

      for (k in 1:n_sentences){

        # Case: current sentence is in 1st quantile
        if (sentiment_df_list[[i]]$sentiment[k] <= q1){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){
            maxLength = currentLength
          }

          # Case: current sentence is not in 1st quantile
        } else {

          # Sub-case: current island is the longest
          if (currentLength > maxLength){
            maxLength = currentLength
          }

          currentLength = 0
        }
      }
    }

    eva_si <- append(eva_si, maxLength) # update output vector
  }

  return(eva_si)
}

###############################################
# EVA Feature 4: Normalized Flip Frequency (FF)
# 4.1 FF (filtered)
# 4.2 FF (unfiltered)
# FF Description: frequency of polarity change between adjacent sentences,
# normalized by sentence count
###############################################
######################################
# EVA Feature 5: Number of Flips (NFF)
# 5.1 NFF (filtered)
# 5.2 NFF (unfiltered)
# NFF Description: frequency of polarity change between adjacent sentences
######################################
get_eva_ff <- function(sentiment_df_list){

  eva_nff <- c() # initialize output vector NFF
  eva_ff <- c() # initialize output vector FF
  n_sentences_store <- c() # store number of sentences for each transcript

  for (i in 1:length(sentiment_df_list)){

    n_flips = 0 # initialize flip counter
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences
    n_sentences_store <- append(n_sentences_store, n_sentences) # update sentence store

    # check if there are at least 2 sentences
    if (n_sentences >= 2){

      for (k in 2:length(sentiment_df_list[[i]]$sentiment)){

        # Case: polarity flip from positive to negative
        if ((sentiment_df_list[[i]]$sentiment[k-1] > 0) &&
            (sentiment_df_list[[i]]$sentiment[k] < 0)){

          n_flips = n_flips + 1

        # Case: polarity flip from negative to positive
        } else if ((sentiment_df_list[[i]]$sentiment[k-1] < 0) &&
                   (sentiment_df_list[[i]]$sentiment[k] > 0)){

          n_flips = n_flips + 1

        }
      }
    }

    eva_nff <- append(eva_nff, n_flips) # update output vector NFF
  }

  eva_ff <- eva_nff / n_sentences_store # normalize output vector NFF
  df <- data.frame(eva_ff, eva_nff) # store output vectors NFF and FF in data frame

  colnames(df) <- c("FF","NFF")
  df$FF[is.na(df$FF)] <- 0 # FF values of transcripts with 0 sentences will be NA

  return(df)
}

###################################################
# EVA Feature 6: Variance of Sentiment Scores (VAR)
# 6.1 VAR (filtered)
# 6.2 VAR (unfiltered)
# Description: variance of sentiment of all sentences in a text
###################################################
get_eva_var <- function(sentiment_df_list){

  eva_var <- c() # initialize output vector

  for (i in 1:length(sentiment_df_list)){
    eva_var <- append(eva_var, var(sentiment_df_list[[i]]$sentiment))
  }

  eva_var[is.na(eva_var)] <- 0 # VAR values of transcripts with 0 or 1 sentences will be NA

  return(eva_var)
}

##################################
# EVA Feature 7: Hill Spacing (HS)
# 7.1 HS (filtered)
# 7.2 HS (unfiltered)
# Description: number of sentences between the 2 longest happy islands, normalized
# by sentence count
##################################
###################################################
# EVA Feature 9: Hill Minimum Sentence Split (HMSS)
# 9.1 HMSS (filtered)
# 9.2 HMSS (unfiltered)
# Description: number of sentences between the 2 longest happy islands
###################################################
get_eva_hs <- function(sentiment_df_list){

  eva_hmss = c() # initialize output vector HMSS
  n_sentences_store = c() # store number of sentences for each transcript
  island_list = c() # list of island info of each transcript

  for (i in 1:length(sentiment_df_list)){

    maxLength = 0 # length of longest island
    currentLength = 0 # length of current island
    length_islands = c() # length of each island
    sentence_index_islands = c() # sentence index of each island (NOT sentenceID)

    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences
    n_sentences_store <- append(n_sentences_store, n_sentences) # update sentence store

    # get 4th quantile of positive sentiments
    positive_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment > 0)])
    q4 <- positive_quantile[4]

    # Case: there are no positive sentiments (4th quantile is null)
    if (is.na(q4)){

      length_islands <- append(length_islands, 0)
      sentence_index_islands <- append(sentence_index_islands, 0)

    # Case: there are positive sentiments (4th quantile is NOT null)
    } else {

      for (k in 1:n_sentences){

        # Case: current sentence is in 4th quantile
        if (sentiment_df_list[[i]]$sentiment[k] >= q4){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){

            maxLength = currentLength
            length_islands <- append(length_islands, maxLength)
            sentence_index_islands <- append(sentence_index_islands, k)

          }

        # Case: current sentence is not in 4th quantile
        } else {

          # Sub-case: an island has just ended
          if (currentLength >= 1){

            maxLength = currentLength
            length_islands <- append(length_islands, maxLength)
            sentence_index_islands <- append(sentence_index_islands, k-1)

          }

          currentLength = 0
        }
      }
    }

    island_list[[i]] <- data.frame(sentence_index_islands, length_islands) # update island info list
  }

  for (j in 1:length(island_list)){

    # sort islands by descending lengths
    island_list[[j]] <- island_list[[j]][order(-island_list[[j]]$length_islands),]

    # Case: less than 2 islands
    if (length(island_list[[j]]$length_islands) < 2){

      eva_hmss <- append(eva_hmss, 0)

    # Case: exactly 2 islands
    } else if (length(island_list[[j]]$length_islands) == 2){

      # Sub-case: island 1 is longer than island 2
      if (island_list[[j]]$length_islands[1] > island_list[[j]]$length_islands[2]){

        # subtract 1 to calculate number of sentences instead of gaps
        # (eg islands that are 2 sentences apart have only 1 sentence in between them)
        sentence_split = abs(island_list[[j]]$sentence_index_islands[1] - island_list[[j]]$sentence_index_islands[2]) - 1
        eva_hmss <- append(eva_hmss, sentence_split)

      # Sub-case: more than 1 longest island
      } else {
        eva_hmss <- append(eva_hmss, 0)
      }

    # Case: more than 2 islands
    } else {

      # Sub-case: island 1 is longer than island 2
      if (island_list[[j]]$length_islands[1] > island_list[[j]]$length_islands[2]){

        # Sub-sub-case: more than 1 second longest island
        if (island_list[[j]]$length_islands[2] == island_list[[j]]$length_islands[3]){

          length_island_2 <- island_list[[j]]$length_islands[2]
          sentence_index_island_2 <- island_list[[j]]$sentence_index_islands[which(island_list[[j]]$length_islands == length_island_2)]

          # subtract 1 to calculate number of sentences instead of gaps
          # (eg islands that are 2 sentences apart have only 1 sentence in between them)
          sentence_split <- abs(island_list[[j]]$sentence_index_islands[1] - sentence_index_island_2) - 1
          eva_hmss <- append(eva_hmss, min(sentence_split))

        # Sub-sub-case: exactly 1 second longest island
        } else {

          # subtract 1 to calculate number of sentences instead of gaps
          # (eg islands that are 2 sentences apart have only 1 sentence in between them)
          sentence_split = abs(island_list[[j]]$sentence_index_islands[1] - island_list[[j]]$sentence_index_islands[2]) - 1
          eva_hmss <- append(eva_hmss, sentence_split)

        }

      # Sub-case: more than 1 longest island
      } else {

        temp_split_list <- c() # list of sentence splits between longest islands
        longest_island = max(island_list[[j]]$length_islands) # length of longest island

        for (a in 1:(length(island_list[[j]]$length_islands) - 1)){

          # Sub-sub-case: current and next islands are longest islands
          if (island_list[[j]]$length_islands[a] == longest_island &&
              island_list[[j]]$length_islands[a+1] == longest_island){

            # subtract 1 to calculate number of sentences instead of gaps
            # (eg islands that are 2 sentences apart have only 1 sentence in between them)
            sentence_split = abs(island_list[[j]]$sentence_index_islands[a] - island_list[[j]]$sentence_index_islands[a+1]) - 1
            temp_split_list <- append(temp_split_list, sentence_split)

          }
        }

        eva_hmss <- append(eva_hmss, min(temp_split_list))
      }
    }
  }

  eva_hs <- eva_hmss / n_sentences_store # normalize output vector HMSS
  df <- data.frame(eva_hs, eva_hmss) # store output vectors HMSS and HS in data frame

  colnames(df) <- c("HS", "HMSS")
  df$HS[is.na(df$HS)] <- 0 # HS values of transcripts with 0 sentences will be NA

  return(df)
}

####################################
# EVA Feature 8: Trough Spacing (TS)
# 8.1 TS (filtered)
# 8.2 TS (unfiltered)
# Description: Number of sentences between top 2 longest sad islands, normalized
# by sentence count
####################################
######################################################
# EVA Feature 10: Trough Minimum Sentence Split (TMSS)
# 9.1 TMSS (filtered)
# 9.2 TMSS (unfiltered)
# Description: number of sentences between the 2 longest sad islands
######################################################
get_eva_ts <- function(sentiment_df_list) {

  eva_tmss = c() # initialize output vector TMSS
  n_sentences_store = c() # store number of sentences for each transcript
  island_list = c() # list of island info of each transcript

  for (i in 1:length(sentiment_df_list)){

    maxLength = 0 # length of longest island
    currentLength = 0 # length of current island
    length_islands = c() # length of each island
    sentence_index_islands = c() # sentence index of each island (NOT sentenceID)

    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences
    n_sentences_store <- append(n_sentences_store, n_sentences) # update sentence store

    # get 1st quantile of negative sentiments
    negative_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment < 0)])
    q1 <- negative_quantile[2]

    # Case: there are no negative sentiments (1st quantile is null)
    if (is.na(q1)){

      length_islands <- append(length_islands, 0)
      sentence_index_islands <- append(sentence_index_islands, 0)

    # Case: there are negative sentiments (1st quantile is NOT null)
    } else {

      for (k in 1:n_sentences){

        # Case: current sentence is in 1st quantile
        if (sentiment_df_list[[i]]$sentiment[k] <= q1){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){

            maxLength = currentLength
            length_islands <- append(length_islands, maxLength)
            sentence_index_islands <- append(sentence_index_islands, k)

          }

        # Case: current sentence is not in 1st quantile
        } else {

          # Sub-case: an island has just ended
          if (currentLength >= 1){

            maxLength = currentLength
            length_islands <- append(length_islands, maxLength)
            sentence_index_islands <- append(sentence_index_islands, k-1)

          }

          currentLength = 0
        }
      }
    }

    island_list[[i]] <- data.frame(sentence_index_islands, length_islands) # update island info list
  }

  for (j in 1:length(island_list)){

    # sort islands by descending lengths
    island_list[[j]] <- island_list[[j]][order(-island_list[[j]]$length_islands),]

    # Case: less than 2 islands
    if (length(island_list[[j]]$length_islands) < 2){

      eva_tmss <- append(eva_tmss, 0)

    # Case: exactly 2 islands
    } else if (length(island_list[[j]]$length_islands) == 2){

      # Sub-case: island 1 is longer than island 2
      if (island_list[[j]]$length_islands[1] > island_list[[j]]$length_islands[2]){

        # subtract 1 to calculate number of sentences instead of gaps
        # (eg islands that are 2 sentences apart have only 1 sentence in between them)
        sentence_split = abs(island_list[[j]]$sentence_index_islands[1] - island_list[[j]]$sentence_index_islands[2]) - 1
        eva_tmss <- append(eva_tmss, sentence_split)

      # Sub-case: more than 1 longest island
      } else {
        eva_tmss <- append(eva_tmss, 0)
      }

    # Case: more than 2 islands
    } else {

      # Sub-case: island 1 is longer than island 2
      if (island_list[[j]]$length_islands[1] > island_list[[j]]$length_islands[2]){

        # Sub-sub-case: more than 1 second longest island
        if (island_list[[j]]$length_islands[2] == island_list[[j]]$length_islands[3]){

          length_island_2 <- island_list[[j]]$length_islands[2]
          sentence_index_island_2 <- island_list[[j]]$sentence_index_islands[which(island_list[[j]]$length_islands == length_island_2)]

          # subtract 1 to calculate number of sentences instead of gaps
          # (eg islands that are 2 sentences apart have only 1 sentence in between them)
          sentence_split <- abs(island_list[[j]]$sentence_index_islands[1] - sentence_index_island_2) - 1
          eva_tmss <- append(eva_tmss, min(sentence_split))

        # Sub-sub-case: exactly 1 second longest island
        } else {

          # subtract 1 to calculate number of sentences instead of gaps
          # (eg islands that are 2 sentences apart have only 1 sentence in between them)
          sentence_split = abs(island_list[[j]]$sentence_index_islands[1] - island_list[[j]]$sentence_index_islands[2]) - 1
          eva_tmss <- append(eva_tmss, sentence_split)

        }

      # Sub-case: more than 1 longest island
      } else {

        temp_split_list <- c() # list of sentence splits between longest islands
        longest_island = max(island_list[[j]]$length_islands) # length of longest island

        for (a in 1:(length(island_list[[j]]$length_islands) - 1)){

          # Sub-sub-case: current and next islands are longest islands
          if (island_list[[j]]$length_islands[a] == longest_island &&
              island_list[[j]]$length_islands[a+1] == longest_island){

            # subtract 1 to calculate number of sentences instead of gaps
            # (eg islands that are 2 sentences apart have only 1 sentence in between them)
            sentence_split = abs(island_list[[j]]$sentence_index_islands[a] - island_list[[j]]$sentence_index_islands[a+1]) - 1
            temp_split_list <- append(temp_split_list, sentence_split)

          }
        }

        eva_tmss <- append(eva_tmss, min(temp_split_list))
      }
    }
  }

  eva_ts <- eva_tmss / n_sentences_store # normalize output vector TMSS
  df <- data.frame(eva_ts, eva_tmss) # store output vectors TMSS and TS in data frame

  colnames(df) <- c("TS", "TMSS")
  df$TS[is.na(df$TS)] <- 0 # TS values of transcripts with 0 sentences will be NA

  return(df)
}

#################################################
# EVA Feature 11: Maximum Sentiment of HI (MAXHI)
# 11.1 MAXHI (filtered)
# 11.2 MAXHI (unfiltered)
# Description: highest sentiment in the HI
#################################################
#######################################
# EVA Feature 13: Variance of HI (HVAR)
# 13.1 HVAR (filtered)
# 13.2 HVAR (unfiltered)
# Description: variance of sentiment in the HI
#######################################
get_eva_maxhi <- function(sentiment_df_list){

  maxLength_store = c() # store length of longest island of each transcript
  maxID_store = c() # store sentence index of longest island of each transcript
  eva_maxhi = c() # initialize output vector MAXHI
  eva_hvar = c() # initialize output vector HVAR

  for (i in 1:length(sentiment_df_list)){

    currentLength = 0 # length of current island
    maxLength = 0 # length of longest island
    maxID = 0 # sentence index of longest island (NOT sentenceID)
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # get 4th quantile of positive sentiments
    positive_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment > 0)])
    q4 <- positive_quantile[4]

    # check if there are positive sentiments (4th quantile is NOT null)
    if (!is.na(q4)){

      for (k in 1:n_sentences){

        # Case: current sentence is in 4th quantile
        if (sentiment_df_list[[i]]$sentiment[k] >= q4){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){

            maxLength = currentLength
            maxID = k # current sentence is the last of the longest island

          }

        # Case: current sentence is not in 4th quantile
        } else {

          # Sub-case: current island is the longest
          if (currentLength > maxLength){

            maxLength = currentLength
            maxID = k - 1 # sentence BEFORE current sentence is the last of the longest island

          }

          currentLength = 0
        }
      }
    }

    maxLength_store <- append(maxLength_store, maxLength)
    maxID_store <- append(maxID_store, maxID)
  }

  df <- data.frame(maxLength_store, maxID_store) # store longest island info of each transcript in data frame

  for (j in 1:length(df$maxID_store)){

    island_sentiment <- c() # initialize sentiment vector of longest island

    # Case: there is no longest island
    if (df$maxLength_store[j] == 0){

      eva_maxhi <- append(eva_maxhi, 0) # update output vector MAXHI
      eva_hvar <- append(eva_hvar, 0) # update output vector HVAR

    # Case: there is a longest island
    } else {

      # index of first sentence = index of last sentence - length of island + 1
      # index of last sentence = maxID
      island_sentiment <- sentiment_df_list[[j]]$sentiment[(df$maxID_store[j] - df$maxLength_store[j] + 1):(df$maxID_store[j])]

      eva_maxhi <- append(eva_maxhi, max(island_sentiment)) # update output vector MAXHI
      eva_hvar <- append(eva_hvar, var(island_sentiment)) # update output vector HVAR

    }
  }

  df <- cbind(df, eva_maxhi)
  df <- cbind(df, eva_hvar)
  df <- df[, 3:4]

  colnames(df) <- c("MAXHI", "HVAR")
  df$HVAR[is.na(df$HVAR)] <- 0 # HVAR values of transcripts with 1-sentence HIs will be NA

  return(df)
}

#################################################
# EVA Feature 12: Minimum Sentiment of SI (MINSI)
# 11.1 MINSI (filtered)
# 11.2 MINSI (unfiltered)
# Description: lowest sentiment in the SI
#################################################
#######################################
# EVA Feature 14: Variance of SI (SVAR)
# 13.1 SVAR (filtered)
# 13.2 SVAR (unfiltered)
# Description: variance of sentiment in the SI
#######################################
get_eva_minsi <- function(sentiment_df_list){

  maxLength_store = c() # store length of longest island of each transcript
  maxID_store = c() # store sentence index of longest island of each transcript
  eva_minsi = c() # initialize output vector MINSI
  eva_svar = c() # initialize output vector SVAR

  for (i in c(1:length(sentiment_df_list))){

    currentLength = 0 # length of current island
    maxLength = 0 # length of longest island
    maxID = 0 # sentence index of longest island (NOT sentenceID)
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # get 1st quantile of negative sentiments
    negative_quantile <- quantile(sentiment_df_list[[i]]$sentiment[which(sentiment_df_list[[i]]$sentiment < 0)])
    q1 <- negative_quantile[2]

    # check if there are positive sentiments (1st quantile is NOT null)
    if (!is.na(q1)){

      for (k in c(1:n_sentences)){

        # Case: current sentence is in 1st quantile
        if (sentiment_df_list[[i]]$sentiment[k] <= q1){

          currentLength = currentLength + 1

          # Sub-case: current sentence is the last sentence AND current island is the longest
          if (k == n_sentences && currentLength > maxLength){

            maxLength = currentLength
            maxID = k # current sentence is the last of the longest island

          }

        # Case: current sentence is not in 1st quantile
        } else {

          # Sub-case: current island is the longest
          if (currentLength > maxLength){

            maxLength = currentLength
            maxID = k - 1 # sentence BEFORE current sentence is the last of the longest island

          }

          currentLength = 0
        }
      }
    }

    maxLength_store <- append(maxLength_store, maxLength)
    maxID_store <- append(maxID_store, maxID)
  }

  df <- data.frame(maxLength_store, maxID_store) # store longest island info of each transcript in data frame

  for (j in c(1:length(df$maxID_store))){

    island_sentiment <- c() # initialize sentiment vector of longest island

    # Case: there is no longest island
    if (df$maxLength_store[j] == 0){

      eva_minsi <- append(eva_minsi, 0) # update output vector MINSI
      eva_svar <- append(eva_svar, 0) # update output vector SVAR

    } else {

      # index of first sentence = index of last sentence - length of island + 1
      # index of last sentence = maxID
      island_sentiment <- sentiment_df_list[[j]]$sentiment[(df$maxID_store[j] - df$maxLength_store[j] + 1):(df$maxID_store[j])]

      eva_minsi <- append(eva_minsi, min(island_sentiment)) # update output vector MINSI
      eva_svar <- append(eva_svar, var(island_sentiment)) # update output vector SVAR

    }
  }

  df <- cbind(df, eva_minsi)
  df <- cbind(df, eva_svar)
  df <- df[, 3:4]

  colnames(df) <- c("MINSI", "SVAR")
  df$SVAR[is.na(df$SVAR)] <- 0 # SVAR values of transcripts with 1-sentence SIs will be NA

  return(df)
}

##################################################################
# EVA Feature 15: Moving Average and Root Mean Square Error (RMSE)
# 15.1 RMSE (filtered)
# 15.2 RMSE (unfiltered)
# Description: RMSE between original sentiments and smoothed sentiments
##################################################################
# smoothing function with default window size of 3
get_smoothed_data <- function(sentiment_df_list, window=3){

  original_sentiment <- sentiment_df_list # copy of original sentiment

  for (i in 1:length(sentiment_df_list)){

    # Case: transcript length is smaller than window size
    if (length(sentiment_df_list[[i]]$sentence_id) < window) {
      skip

    # Case: transcript length is at least window size
    } else {

      sentiment_df_list[[i]]$sentiment <- SMA(sentiment_df_list[[i]]$sentiment, n=window)

      # for sentences whose indices are smaller than window size, set sentiments to original values
      for (k in 1:(window - 1)){
        sentiment_df_list[[i]]$sentiment[k] <- original_sentiment[[i]]$sentiment[k]
      }
    }
  }

  return(sentiment_df_list)
}

get_eva_rmse <- function(sentiment_df_list){

  smoothed_sentiment <- get_smoothed_data(sentiment_df_list) # call smoothing function
  eva_rmse <- c() # initialize output vector

  for (i in 1:length(sentiment_df_list)){

    original <- sentiment_df_list[[i]]$sentiment
    smoothed <- smoothed_sentiment[[i]]$sentiment
    eva_rmse <- append(eva_rmse, rmse(original, smoothed)) # update output vector

  }

  eva_rmse[is.na(eva_rmse)] <- 0 # RMSE values of transcripts with 0 sentences will be NA

  return(eva_rmse)
}

#####################################
# EVA Feature 16: Positive Peaks (PP)
# 16.1 PP (filtered)
# 16.2 PP (unfiltered)
# Description: number of sentences with sentiments higher than those of adjacent sentences
#####################################
#####################################
# EVA Feature 17: Negative Peaks (NP)
# 17.1 NP (filtered)
# 17.2 NP (unfiltered)
# Description: number of sentences with sentiments lower than those of adjacent sentences
#####################################
##################################
# EVA Feature 18: Peaks Ratio (PR)
# 18.1 PR (filtered)
# 18.2 PR (unfiltered)
# Description: difference between PP and NP (PP - NP)
##################################
get_eva_pr <- function(sentiment_df_list){

  eva_pp <- c() # initialize output vector PP
  eva_np <- c() # initialize output vector NP
  eva_pr <- c() # initialize output vector PR

  for (i in 1:length(sentiment_df_list)){

    pp_count = 0 # number of PP in current transcript
    np_count = 0 # number of NP in current transcript
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # check if there are at least 3 sentences (required to define a peak)
    if (n_sentences >= 3){

      # start index: 2nd sentence
      # end index: 2nd last sentence
      for (k in 2:(length(sentiment_df_list[[i]]$sentiment) - 1)){

        # Case: current sentiment is positive
        if (sentiment_df_list[[i]]$sentiment[k] > 0){

          # Sub-case: adjacent sentiments are lower
          if (sentiment_df_list[[i]]$sentiment[k] > sentiment_df_list[[i]]$sentiment[k-1] &&
              sentiment_df_list[[i]]$sentiment[k] > sentiment_df_list[[i]]$sentiment[k+1]){

            pp_count = pp_count + 1

          }

        # Case: current sentiment is negative
        } else if (sentiment_df_list[[i]]$sentiment[k] < 0){

          # Sub-case: adjacent sentiments are higher
          if (sentiment_df_list[[i]]$sentiment[k] < sentiment_df_list[[i]]$sentiment[k-1] &&
              sentiment_df_list[[i]]$sentiment[k] < sentiment_df_list[[i]]$sentiment[k+1]){

            np_count = np_count + 1

          }
        }
      }
    }

    eva_pp <- append(eva_pp, pp_count) # update output vector PP
    eva_np <- append(eva_np, np_count) # update output vector NP
    eva_pr <- append(eva_pr, (pp_count - np_count)) # update output vector PR
  }

  df <- data.frame(eva_pp, eva_np, eva_pr) # store output vectors PP, NP, and PR in data frame
  colnames(df) <- c("PP", "NP", "PR")

  return(df)
}

########################################
# EVA Feature 19: Sum of PP values (SPP)
# 16.1 SPP (filtered)
# 16.2 SPP (unfiltered)
# Description: sentiment sum of PPs
########################################
########################################
# EVA Feature 20: Sum of NP values (NPP)
# 20.1 NPP (filtered)
# 20.2 NPP (unfiltered)
# Description: absolute sentiment sum of NPs
########################################
##########################################
# EVA Feature 21: Average Peak Ratio (APR)
# 21.1 APR (filtered)
# 21.2 APR (unfiltered)
# Description: difference between SPP and NPP (SPP - NPP)
##########################################
get_eva_apr <- function(sentiment_df_list){

  eva_spp <- c() # initialize output vector SPP
  eva_npp <- c() # initialize output vector NPP
  eva_apr <- c() # initialize output vector APR

  for (i in 1:length(sentiment_df_list)){

    pp_sentiment <- c() # sentiments of PPs in current transcript
    np_sentiment <- c() # sentiments of NPs in current transcript
    apr_sentiment = 0 # APR of current transcript
    n_sentences <- length(sentiment_df_list[[i]]$sentence_id) # get number of sentences

    # check if there are at least 3 sentences (required to define a peak)
    if (n_sentences >= 3){

      # start index: 2nd sentence
      # end index: 2nd last sentence
      for (k in 2:(length(sentiment_df_list[[i]]$sentiment) - 1)){

        # Case: current sentiment is positive
        if (sentiment_df_list[[i]]$sentiment[k] > 0){

          # Sub-case: adjacent sentiments are lower
          if (sentiment_df_list[[i]]$sentiment[k] > sentiment_df_list[[i]]$sentiment[k-1] &&
              sentiment_df_list[[i]]$sentiment[k] > sentiment_df_list[[i]]$sentiment[k+1]){

            pp_sentiment <- append(pp_sentiment, sentiment_df_list[[i]]$sentiment[k])
          }

        # Case: current sentiment is negative
        } else if (sentiment_df_list[[i]]$sentiment[k] < 0) {

          # Sub-case: adjacent sentiments are higher
          if (sentiment_df_list[[i]]$sentiment[k] < sentiment_df_list[[i]]$sentiment[k-1] &&
              sentiment_df_list[[i]]$sentiment[k] < sentiment_df_list[[i]]$sentiment[k+1]){

            np_sentiment <- append(np_sentiment, sentiment_df_list[[i]]$sentiment[k])
          }
        }
      }
    }

    eva_spp <- append(eva_spp, sum(pp_sentiment)) # update output vector SPP
    eva_npp <- append(eva_npp, abs(sum(np_sentiment))) # update output vector NPP

    apr_sentiment = sum(pp_sentiment) - abs(sum(np_sentiment))
    eva_apr <- append(eva_apr, apr_sentiment) # update output vector APR
  }

  df <- data.frame(eva_spp, eva_npp, eva_apr) # store output vectors SPP, NPP, and APR in data frame

  colnames(df) <- c("SPP", "NPP", "APR")
  return(df)
}

###############################################################################
# wrapper function to generate EVA features from processed sentiment data frame
###############################################################################
eva_v1_wrapper <- function(sentiment_df){

  # get lists of filtered and unfiltered data frames
  fs_df_list <- get_filtered_sentiment(sentiment_df)
  ufs_df_list <- get_unfiltered_sentiment(sentiment_df)

  ###########################
  # calling EVA 1.0 functions
  ###########################
  # Feature 1: AVG
  eva_fs_avg <- get_eva_avg(fs_df_list)
  eva_ufs_avg <- get_eva_avg(ufs_df_list)

  # Feature 2: HI
  eva_fs_hi <- get_eva_hi(fs_df_list)
  eva_ufs_hi <- get_eva_hi(ufs_df_list)

  # Feature 3: SI
  eva_fs_si <- get_eva_si(fs_df_list)
  eva_ufs_si <- get_eva_si(ufs_df_list)

  # Features 4 and 5: FF and NFF
  eva_fs_ff <- get_eva_ff(fs_df_list)
  eva_ufs_ff <- get_eva_ff(ufs_df_list)

  # Feature 6: VAR
  eva_fs_var <- get_eva_var(fs_df_list)
  eva_ufs_var <- get_eva_var(ufs_df_list)

  # Features 7 and 9: HS and HMSS
  eva_fs_hs <- get_eva_hs(fs_df_list)
  eva_ufs_hs <- get_eva_hs(ufs_df_list)

  # Features 8 and 10: TS and TMSS
  eva_fs_ts <- get_eva_ts(fs_df_list)
  eva_ufs_ts <- get_eva_ts(ufs_df_list)

  # Features 11 and 13: MAXHI and HVAR
  eva_fs_maxhi <- get_eva_maxhi(fs_df_list)
  eva_ufs_maxhi <- get_eva_maxhi(ufs_df_list)

  # Features 12 and 14: MINSI and SVAR
  eva_fs_minsi <- get_eva_minsi(fs_df_list)
  eva_ufs_minsi <- get_eva_minsi(ufs_df_list)

  # Feature 15: RMSE
  eva_fs_rmse <- get_eva_rmse(fs_df_list)
  eva_ufs_rmse <- get_eva_rmse(ufs_df_list)

  # Features 16, 17, and 18: PP, NP, and PR
  eva_fs_pr <- get_eva_pr(fs_df_list)
  eva_ufs_pr <- get_eva_pr(ufs_df_list)

  # Features 19, 20, and 21: SPP, NPP, APR
  eva_fs_apr <- get_eva_apr(fs_df_list)
  eva_ufs_apr <- get_eva_apr(ufs_df_list)

  # store EVA 1.0 features in data frame
  eva_v1_df <- data.frame(transcript_id,
                          eva_fs_avg, eva_ufs_avg,
                          eva_fs_hi, eva_ufs_hi,
                          eva_fs_si, eva_ufs_si,
                          eva_fs_ff, eva_ufs_ff,
                          eva_fs_var, eva_ufs_var,
                          eva_fs_hs, eva_ufs_hs,
                          eva_fs_ts, eva_ufs_ts,
                          eva_fs_maxhi, eva_ufs_maxhi,
                          eva_fs_minsi, eva_ufs_minsi,
                          eva_fs_rmse, eva_ufs_rmse,
                          eva_fs_pr, eva_ufs_pr,
                          eva_fs_apr,eva_ufs_apr)

  # naming data frame columns
  eva_v1_cols <- c("sn",
                   "FS_AVG", "UFS_AVG",
                   "FS_HI", "UFS_HI",
                   "FS_SI", "UFS_SI",
                   "FS_FF", "FS_NFF", "UFS_FF","UFS_NFF",
                   "FS_VAR", "UFS_VAR",
                   "FS_HS", "FS_HMSS", "UFS_HS", "UFS_HMSS",
                   "FS_TS", "FS_TMSS", "UFS_TS", "UFS_TMSS",
                   "FS_MAXHI", "FS_HVAR", "UFS_MAXHI", "UFS_HVAR",
                   "FS_MINSI", "FS_SVAR", "UFS_MINSI", "UFS_SVAR",
                   "FS_RMSE", "UFS_RMSE",
                   "FS_PP", "FS_NP", "FS_PR", "UFS_PP", "UFS_NP", "UFS_PR",
                   "FS_SPP", "FS_SNP", "FS_APR", "UFS_SPP", "UFS_SNP", "UFS_APR")

  # assigning names to data frame columns
  colnames(eva_v1_df) <- eva_v1_cols

  # save EVA features in a CSV file
  write.csv(eva_v1_df, "eva_v1.csv", quote = F, row.names = F)

  return(eva_v1_df)
}

# call wrapper function
eva_v1_wrapper(sentiment_df)

