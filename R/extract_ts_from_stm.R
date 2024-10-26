
extract_ts_from_stm <- function(x, date, id){
  doc_probs <- data.frame(x$theta)
  topics <- names(doc_probs)
  doc_probs$id <- id
  doc_probs$date <- date
  by_day <- list()
  ts_c <- list()
  for (i in 1:length(topics))
    {
    doc_probs |>
      group_by(date) |>
        summarize(salience=sum(get(topics[i]), na.rm=TRUE)) -> by_day[[i]]
    ts_c[[i]] <- data.frame(day=seq(1,max(date, na.rm=TRUE),1), salience=NA)
    ts_c[[i]][,"salience"] <- by_day[[i]][match(ts_c[[i]]$day, by_day[[i]]$date),"salience"]
    ts_c[[i]][,"salience0"] <- ifelse(is.na(ts_c[[i]][,"salience"]), 0, ts_c[[i]][,"salience"])
    print(i)
    flush.console()
    }
  return(ts_c)
}