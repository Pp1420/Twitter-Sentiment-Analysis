## Shiny Server component for dashboard

library(DT)

function(input, output, session)
{
  
  # Raw Data table Output
  output$dataT <- DT::renderDT({
    Tweets_subset <- head(Tweets, nrow(Tweets))
    datatable(Tweets_subset, options = list(pageLength = 20))
  })
  
  

  # Processed Data table Output
  output$Processed_dataT <- DT::renderDT({
    # Subset the first 100 rows of the data
    after_subset <- head(after_with_rownum, nrow(after_with_rownum))
    # Render the data table
    datatable(after_subset, options = list(pageLength = 20), rownames = FALSE)
  })
  

  # Text Element Stats - Show cleaning process
  # Text Stats chart output
  output$Text_stats_chart <- renderPlot({
    ggplot(stats_long, aes(x = category, y = count, fill = dataset)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of Text Elements in 'Raw Data' and 'Processed Data' Datasets",
           x = "Text Element",
           y = "Count",
           fill = "") +
      theme_light() +
      scale_x_discrete(labels = c("Lowercase", "Uppercase", "Numbers")) +
      scale_fill_manual(labels = c("Raw", "Processed"), 
                        values=c("#A9A9A9", "#FFA07A")) +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5), # modify plot title position
            plot.title.position = "plot") # remove plot title offset
  })
  
  
  #Words in Lexicons
  
  output$wordslexicon <- renderPlot({
    ggplot(tib_rows, aes(x = tibble_name, y = num_rows_tib, fill = tibble_name)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = num_rows_tib), vjust = -0.5) +
      ylab("Words Stored") +
      xlab("Lexicons") +
      ggtitle("Comparison of Words Stored in Lexicons") +
      scale_fill_manual(name = "Lexicons", values = c(Bing = "#009E73", NRC = "#D55E00", Syuzhet = "#0072B2")) +
      theme(plot.title = element_text(size = 18, face = "bold"))
  })
  
  
  
  
  
  
  output$text_summary <- renderText({
    s <- summary(after_with_rownum$Tweet)
    paste("Number of Tweets: ", s[1])
  })
  
  
  # ------------------------------------------
  
  # Dashboard
  
  # ------------------------------------------
  
  num_rows <- reactive({
    nrow(brkSyuTweet$second_column)
  })
  
  pos_perc <- reactive({
    num_pos_tweets <- table(brkSyuTweetTest$Sentiment)[["Positive"]]
    num_SyuTweet <- nrow(brkSyuTweetTest)
    perc_pos_tweets <- round((num_pos_tweets / num_SyuTweet) * 100, 2)
    return(perc_pos_tweets)
  })
  
  output$pos_perc_box <- renderValueBox({
    valueBox(
      paste0(pos_perc(), "%"),
      "Positive Tweets",
      icon = icon("thumbs-up"),
      color = "orange"
    )
  })
  
  accuracy_box <- reactive({
    return(round(accuracyhigh, 2))
  })
  
  output$accuracybox <- renderValueBox({
    valueBox(
      paste0(accuracy_box(), "%"),
      "Highest Accuracy",
      icon = icon("line-chart"),
      color = "green"
    )
  })
  
  
  # User Input
  
  # Define function to analyze sentiment of user input
  analyze_sentiment <- function(user_input) {
    
    # Split user input into words
    user_words <- strsplit(tolower(user_input), "\\W")[[1]]
    
    # Remove empty strings from user words
    user_words <- user_words[user_words != ""]
    
    # Match user words with words in syuzhet_tib
    matched_words <- intersect(syuzhet_tib$word, user_words)
    
    # If no matches found, return message
    if (length(matched_words) == 0) {
      return("No sentiment found for the input text.")
    }
    
    # Get sentiments for matched words
    matched_sentiments <- syuzhet_tib %>% filter(word %in% matched_words) %>% select(sentiment)
    
    # Count number of positive and negative sentiments
  #  pos_count <- length(grep("positive", matched_sentiments$sentiment, ignore.case = TRUE))
   # neg_count <- length(grep("negative", matched_sentiments$sentiment, ignore.case = TRUE))
   
     # Count number of positive and negative words
    pos_words <- matched_words[matched_sentiments$sentiment == "positive"]
    neg_words <- matched_words[matched_sentiments$sentiment == "negative"]
    pos_count <- length(pos_words)
    neg_count <- length(neg_words)
    
    # Calculate ratio of positive to negative sentiments
    if (neg_count == 0 & pos_count == 0) {
      ratio <- 0
    } else if (neg_count == 0) {
      ratio <- Inf
    } else {
      ratio <- pos_count/neg_count
    }
    
    
    # Classify sentence as positive, negative or neutral based on ratio
    if (ratio > 1.1) {
      sentiment <- "Positive"
    } else if (ratio < 0.9) {
      sentiment <- "Negative"
    } else {
      sentiment <- "Neutral"
    }
    
    # Concatenate positive and negative words into strings
    pos_words_str <- paste(pos_words, collapse = ", ")
    neg_words_str <- paste(neg_words, collapse = ", ")
    
    # Create output string
    output_str <-    paste0("Overall Sentiment : ", sentiment, "\n")
    if (pos_count > 0) {
      output_str <- paste0(output_str, "Positive Words    : ", pos_words_str, "\n")
    }
    if (neg_count > 0) {
      output_str <- paste0(output_str, "Negative Words    : ", neg_words_str, "\n")
    }
   # output_str <- paste0(output_str, "Ratio of Positive to Negative Words: ", round(ratio, 2))
    
    return(output_str)
    
    # Return sentiment and counts
   # return(paste("Overall Sentiment:", sentiment, "\nPositive Count:", pos_count, " ||  Negative Count:", neg_count))
  }
  
  
  # Define reactive values for user input and sentiment output
  user_input <- reactiveValues(text = "")
  sentiment_output <- reactiveValues(text = "")
  show_message <- reactiveValues(message = FALSE)
  
  # Update user input value when input changes
  observeEvent(input$userinput, {
    user_input$text <- input$userinput
    show_message$message <- FALSE
  })
  
  # Update sentiment output value when analyze button is clicked
  observeEvent(input$analyze_btn, {
    sentiment_output$text <- analyze_sentiment(user_input$text)
    show_message$message <- TRUE
  })
  
  # Clear user input and sentiment output values when refresh button is clicked
  observeEvent(input$refresh_btn, {
    updateTextInput(session, "userinput", value = "")
    sentiment_output$text <- ""
    show_message$message <- FALSE
  })
  
  # Show message when sentiment analysis is completed
  observeEvent(show_message$message, {
    if (show_message$message) {
      showNotification("Sentiment Analysis Completed.")
    }
  })
  
  # Render sentiment output text
  output$sentiment_output <- renderText({
    sentiments <- sentiment_output$text
    if (is.character(sentiments)) {
      return(sentiments)
    } else {
      return(paste("Sentiment:", paste(sentiments, collapse = ", ")))
    }
  })
  
  
  
  #Top 20
  output$plot20 <- renderUI({
    if (input$plot_choice == "Bing") {
      plotOutput("Bing")
    } else if (input$plot_choice == "NRC") {
      plotOutput("NRC")
    } else if (input$plot_choice == "Syuzhet") {
      plotOutput("Syuzhet")
    }
  })
  
  output$Bing <- renderPlot({
    # code to create plot 1
    bingTop20words %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = NULL , x = NULL) +
      coord_flip()
    
  })
  
  
  output$NRC <- renderPlot({
    #Top 20 Words Using NRC Lexicon
    # code to create plot 2
    ggplot(emotion_sums, aes(x = reorder(s, -count), y = count)) +
      geom_bar(stat = 'identity', fill = "#0072B2", color = "black", width = 0.5) +
      labs(x = "Sentiment Category", y = "Count") + # add axis labels
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5)) # move title to the middle
  })
  
  output$Syuzhet <- renderPlot({
    # code to create plot 3
    # Create the negative words plot
    
    # Define a color scale
    my_colors <- c( "#74C476", "#41AB5D", "#238B45", "#006D2C")
    
    
    # Modify the negative words plot
    p1 <- ggplot(neg_syuzhet, aes(word, n, fill = impact)) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradientn(colors = my_colors, na.value = "grey") + # use custom color scale
      labs(y = "Counts"  , x = NULL) +
      coord_flip() +
      ggtitle("Negative Words")
    
    # Modify the positive words plot
    p2 <- ggplot(pos_syuzhet, aes(word, n, fill = impact)) +
      geom_col(show.legend = FALSE) +
      scale_fill_gradientn(colors = my_colors, na.value = "grey") + # use custom color scale
      labs(y = "Counts" , x = NULL) +
      coord_flip() +
      ggtitle("Positive Words")
    
    # Combine the two plots using grid.arrange
    grid.arrange(p1, p2, nrow = 1)
    
  })
  
  
  # Define density plot
  output$density_plot <- renderPlot({
    brkSyuTweetTest %>% 
      ggplot(aes(x = syuzhet, fill = Sentiment)) +
      geom_density(alpha = 0.7, color = 'black') +
      ggtitle("Density Plot - Polarity Distribution of Tweets with Syuzhet Lexicon") +
      labs(x = "Syuzhet Score", y = "Density") +
      xlim(-10, 10)
  })
  
  #Wordcloud
  
  # Make the wordcloud drawing predictable during a session
  set.seed(122)
  wordcloud_rep <- repeatable(wordcloud)
  
  
  output$plot <- renderPlot({
    wordcloud(words = names(word_freqs), 
              freq = word_freqs,
              max.words = input$max_words,
              random.order = FALSE,
              min.freq = 5,
              colors = brewer.pal(7, 'Set1'), # change color palette here
              scale = c(7, 0.4),
              rot.per = 0.9)
  })
  
  
  
  
  output$plotsyus <- renderPlot({
    ggplot(brkSyuTweet, aes(TweetNum, syuzhet, fill = syuzhet > 0)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c("red", "blue")) +
      labs(x = "Tweet Number", y = "Score",
           title = "Syuzhet Sentiment Scores for Twitter Tweets") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
            plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      ylim(-5, 5)
  })
  
  
  
  
  
  output$model_acc_plot <- renderPlot({
    ggplot(data = accuracy_df, aes(x = model_names, y = accuracies*100)) +
      geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
      geom_text(aes(label = paste0(round(accuracies * 100, 1), "%")), vjust = -0.5, size = 5) +
      labs(x = "", y = "Accuracy (%)", title = "\n\nComparison of Model Accuracies") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
            axis.text = element_text(size = 12), 
            axis.title = element_text(size = 14, face = "bold")) +
      coord_cartesian(ylim = c(0, 100))  # set y-axis limits to 0-100
  }, height = 600)
  
  
  
  
  
  output$NaivePrediction <- renderPrint({
    confusionMatrix(SyuCM)
    })
  
  output$NaiveHeatmap <- renderPlot({
    ggplot(SyuCM_long, aes(x = variable, y = True_Sentiment, fill = value)) +
      geom_tile() +
      geom_text(aes(label = value), color = "black", size = 6) + # increase font size
      theme(text = element_text(size=16)) + # set theme font size
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Confusion Matrix Heatmap",
           x = "Predicted Sentiment",
           y = "Actual Sentiment")
  }, height=600)
  
  
  output$SVMPrediction <- renderPrint({
    confusionMatrix(SyuPred1, SyuTest1$Sentiment, positive = "positive")
  })
  
  
  output$SVMHeatmap <- renderPlot({
    ggplot(conf_df, aes(x = Predicted, y = Actual, fill = Count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Count), color = "black", size = 6) +
      theme(text = element_text(size=16)) +
      scale_fill_gradient(low = "white", high = "red") +
      labs(title = "Confusion Matrix Heatmap",
           x = "Predicted Sentiment",
           y = "Actual Sentiment")
  }, height=600)
  
  
  
  output$XGPrediction <- renderPrint(width = 30,{
    confusionMatrix(XGpred_Valid, as.factor(XGvalidData$Sentiment))
  })
  
  output$XGHeatmap <- renderPlot({
    ggplot(conf_dfXG_melt, aes(x = Predicted, y = Actual, fill = Count)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Count), color = "black", size = 6) +
      scale_fill_gradient(low = "white", high = "green") +
      ggtitle("Confusion Matrix Heatmap") +
      xlab("Predicted Sentiment") +
      ylab("Actual Sentiment") +
      theme(text = element_text(size = 16))
  }, height = 600)
  
  
  
  #Report
  
  
  
  
  output$report = downloadHandler(
    filename<- function(){
      paste("Summary",Sys.Date(),switch(
        input$format, PDF = '.pdf', Word = '.docx'
      ),sep = "")
    },
    
    
    content = function(file) {
      if (input$format=="PDF"){
        #### Progressing indicator
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('report.Rmd', pdf_document())
                       file.rename(out, file)
                       
                       
                     })
        ### below is the end of pdf content
      }else{
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.01)
                       }
                       
                       ## End of progression
                       src <- normalizePath('report.Rmd')
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'report.Rmd', overwrite = TRUE)
                       
                       library(rmarkdown)
                       out <- render('report.Rmd', word_document())
                       file.rename(out, file)
                     })
      }
      
    })
  
}
  
  
  


  




  


  
  
  
  
  
  
  


    
    




  
