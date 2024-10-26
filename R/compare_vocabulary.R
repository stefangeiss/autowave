  # *data* is the document-feature matrix against that includes all the texts from the pre, post, and target periods
  # *pre* defines the number of days before the start of the target period for which topic-related articles should be considered.
  # *post* defines the number of days after the end of the target period for which topic-related articles should be considered. 
  # *start* defines the start date for the start of the target period (measured in days since start of the time series)
  # *end* defines the end date for the target period (measured in days since start of the time series)
  # *nover* defines the number of words that should be included that are more prevalent than in pre/post periods, e.g., 5 would mean that the top 5 over-represented words are provided.
  # *nunder* defines the number of words that should be included that are less prevalent than in pre/post periods, e.g., 5 would mean that the top 5 under-represented words are provided.
  # *topic* defines which topic should be compared between the pre, target, and post periods. Only articles will be considered that have at least 0.5% topic probability for that topic.
  # *area* defines the topic area of the news wave. Not important for the function, it is just carried over to the output. 


compare_vocabulary <- function(data=data,pre=pre,post=post,start=start,end=end,nover=nover,nunder=nunder,topic=topic,area=area,origin.date)
    {
    startdate <- as.POSIXct(start*60*60*24,origin=origin.date) # 
    enddate   <- as.POSIXct(end*60*60*24,origin=origin.date)
    topicX <- to.lab[to.lab$topic250==topic,"topic"]
    # pre.bag.dtm 	<- dfm_subset(data,days>(start-pre) & days<start) # without topic requirement
    # target.bag.dtm 	<- dfm_subset(data,days>(start) & days<end)   # without topic requirement
    # post.bag.dtm 	<- dfm_subset(data,days>(end) & days<(end+post))  # without topic requirement
    pre.bag.dtm 	<- dfm_subset(data,days>(start-pre) & days<start & topics_doc[,topicX]>0.005)
    target.bag.dtm 	<- dfm_subset(data,days>(start) & days<end  & topics_doc[,topicX]>0.005)
    post.bag.dtm 	<- dfm_subset(data,days>(end) & days<(end+post) & topics_doc[,topicX]>0.005)
    full.bag 	<- colSums(pre.bag.dtm)+colSums(target.bag.dtm)+colSums(post.bag.dtm)
    pre.bag 	<- ifelse(colSums(pre.bag.dtm)[full.bag>0]>0,colSums(pre.bag.dtm)[full.bag>0],1)
    target.bag 	<- ifelse(colSums(target.bag.dtm)[full.bag>0]>0,colSums(target.bag.dtm)[full.bag>0],1)
    post.bag 	<- ifelse(colSums(post.bag.dtm)[full.bag>0]>0,colSums(post.bag.dtm)[full.bag>0],1)
    pre.pr 		<- pre.bag/sum(pre.bag,na.rm=TRUE)
    target.pr 	<- target.bag/sum(target.bag,na.rm=TRUE)
    post.pr 	<- post.bag/sum(post.bag,na.rm=TRUE)

    ### Pre-to-target
    pre.prfactor 	<- (target.pr/pre.pr)
    pre.prraw   	<- (target.pr-pre.pr)
    pre.fraw   		<- (target.bag-pre.bag)
    pre.voc.change <- data.frame(feature=names(pre.prfactor),pre.bag,target.bag,pre.pr,target.pr,probability.factor=pre.prfactor,probability.jump=pre.prraw,frequency.jump=pre.fraw,frequency.jump.relative=pre.fraw/target.bag)
    pre.top.voc.change <- subset(pre.voc.change,target.bag>10&target.pr>0.0001 & (probability.factor + probability.jump*5000>2))
    pre.top20.rchange <- pre.top.voc.change[order(pre.top.voc.change$probability.factor*1000*pre.top.voc.change$target.pr,decreasing=TRUE),][1:nover,]
    pre.top20.fchange <- pre.top.voc.change[order(pre.top.voc.change$frequency.jump*1000*pre.top.voc.change$target.pr,decreasing=TRUE),][1:nover,]

    ### Post-to-target
    post.prfactor 	<- (target.pr/post.pr)
    post.prraw   	<- (target.pr-post.pr)
    post.fraw   		<- (target.bag-post.bag)
    post.voc.change <- data.frame(feature=names(post.prfactor),post.bag,target.bag,post.pr,target.pr,
                probability.factor=post.prfactor,probability.jump=post.prraw,
                frequency.jump=post.fraw,frequency.jump.relative=post.fraw/target.bag)
    post.top.voc.change <- subset(post.voc.change,target.bag>10&target.pr>0.0001 & (probability.factor>2 & probability.jump>0.0001))
    post.top20.rchange <- post.top.voc.change[order(post.top.voc.change$probability.factor,decreasing=TRUE),][1:nover,]
    post.top20.fchange <- post.top.voc.change[order(post.top.voc.change$frequency.jump,decreasing=TRUE),][1:nover,]

    wordlist <- paste(	unique(c(as.character(pre.top20.rchange$feature),
              as.character(post.top20.rchange$feature),
              as.character(pre.top20.fchange$feature),
              as.character(post.top20.fchange$feature))))

    topic.textlist <- data.frame(id=topics_doc[topics_doc$id%in%target.bag.dtm@docvars$id,"id"],prob=topics_doc[topics_doc$id%in%target.bag.dtm@docvars$id,to.lab[which(to.lab$topic250==topic),"topic"]],headline=topics_doc[topics_doc$id%in%target.bag.dtm@docvars$id,"headline"],date=topics_doc[topics_doc$id%in%target.bag.dtm@docvars$id,"Time"])
    topic.textlist <- topic.textlist[order(topic.textlist$prob,decreasing=TRUE),]
    topic.textlist <- if(dim(topic.textlist)[1]>40){topic.textlist[1:40,]} else {topic.textlist}
    # order(topic.textlist$prob)

  return(list(startdate=startdate,enddate=enddate,
        topic=topic,area=area,
        wordlist=wordlist,
        texts=topic.textlist[order(topic.textlist$prob,decreasing=TRUE),],
        top20.relative.change.pre.to.target=pre.top20.rchange,
        top20.relative.change.post.to.target=post.top20.rchange,
        top20.absolute.change.pre.to.target=pre.top20.fchange,
        top20.absolute.change.post.to.target=post.top20.fchange))
  }