
# *x.1* defines the first vocabulary comparison for news waves, which is usually an unmodified comparison that also includes rare features. Results from compare_vocabulary with a raw dfm.
# *x.2* defines the second vocabulary comparison for news waves, which is usually a more restricted comparison that ignores the least frequent features and focuses more on core vocabulary.  Results from compare_vocabulary with a restricted (rarest features removed) dfm.
# *x.3* defines the third vocabulary comparison for news waves, which is usually the most restricted comparison that has deleted all less frequent features and focuses exclusively on core vocabulary.  Results from compare_vocabulary with a strongly restricted (rare features removed) dfm.
# *event_id* is the number of the news wave that is to be analyzed

  firstwords <- function(x,nwords){
    y <- paste(c(x[1:nwords],"........ \n ... "),collapse=" ")
  }

  validate_nw <- function(x.1,x.2,x.3,event_id){
    x1 <- x.1[[event_id]]
    x2 <- x.2[[event_id]]
    x3 <- x.3[[event_id]]
    head.t <- tokens(x1$texts$headline,remove_punct=TRUE)
    head.r <- tokens_remove(x=head.t,sw)
    head.u <- as.character(head.r)

    text_excerpts <- tms_tk_u[docvars(tms_tk_u)$id%in%x1$texts$id]
    vector_text_excerpts <- unlist(lapply(lapply(text_excerpts, firstwords, 100),paste, collapse=" "))

    vali <- list(
      "From: ... Until ..."= paste(as.Date(x1$startdate),as.Date(x1$enddate)),
      "Spike keywords"=paste0(x1$wordlist),
      "Spike keywords (words occur in at least 100 documents)"=paste0(x2$wordlist),
      "Spike keywords (words occur in at least 1000 documents)"=paste0(x3$wordlist),
      "Frequent words in headlines"=paste0(names(table(head.u))[order(table(head.u),decreasing=TRUE)][1:20]),
      "Topic signifiers"=paste0(x1$topic),
      "Document IDs"=paste0(x1$texts$id),
      "Headlines"=paste0(x1$texts$headline[1:20]),
      "Text Excerpts"= (paste(str_wrap(vector_text_excerpts), "\n\n\n")))
    return(vali)
  }
