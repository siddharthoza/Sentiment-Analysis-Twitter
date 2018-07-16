library(twitteR)
getTwitterOAuth('sidoza0305', 'Keepguessing@1')

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )
?cred

RAFA.list <- searchTwitter('#USopen2017 #RafaelNadal', n=2000, since = '2017-09-06', until = '2017-09-07')
RAFA.df = twListToDF(RAFA.list)

DEL.list <- searchTwitter('#USopen2017 #DelPotro', n=2000, since = '2017-09-06', until = '2017-09-07')
DEL.df = twListToDF(DEL.list)

KEV.list <- searchTwitter('#USopen2017 #KevinAnderson', n=2000, since = '2017-09-06', until = '2017-09-07')
KEV.df = twListToDF(KEV.list)

Pablo.list <- searchTwitter('#USopen2017 @pablocarreno91', n=2000, since = '2017-09-06', until = '2017-09-07')
Pablo.df = twListToDF(Pablo.list)


#the function for extracting and analyzing tweets
RAFA.df <- RAFA.df[, order(names(RAFA.df))]
RAFA.df$created <- strftime(RAFA.df$created, '%Y-%m-%d')
if (file.exists(paste('#USopen2017 #RafaelNadal', 'RAFA_stack.csv'))==FALSE)
  write.csv(df, file=paste('#USopen2017 #RafaelNadal', 'RAFA_stack.csv'), row.names=F)

DEL.df <- DEL.df[, order(names(DEL.df))]
DEL.df$created <- strftime(DEL.df$created, '%Y-%m-%d')
if (file.exists(paste('#USopen2017 #DelPotro', 'DEL_stack.csv'))==FALSE)
  write.csv(df, file=paste('#USopen2017 #DelPotro', 'DEL_stack.csv'), row.names=F)

KEV.df <- KEV.df[, order(names(KEV.df))]
KEV.df$created <- strftime(KEV.df$created, '%Y-%m-%d')
if (file.exists(paste('#USopen2017 #KevinAnderson', 'KEV_stack.csv'))==FALSE)
  write.csv(df, file=paste('#USopen2017 #KevinAnderson', 'KEV_stack.csv'), row.names=F)

Pablo.df <- Pablo.df[, order(names(Pablo.df))]
Pablo.df$created <- strftime(Pablo.df$created, '%Y-%m-%d')
if (file.exists(paste('#USopen2017 #pablocarreno91', 'Pablo_stack.csv'))==FALSE)
  write.csv(df, file=paste('#USopen2017 #pablocarreno91', 'Pablo_stack.csv'), row.names=F)

?gsub

#merge the last extraction with storage file and remove duplicates
stack <- read.csv(file=paste('#USopen #RafaelNadal', 'RAFA_stack.csv'))
stack <- rbind(stack, RAFA.df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste('#USopen #RafaelNadal', 'RAFA_stack.csv'), row.names=F)

#tweets evaluation function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

#folder with positive dictionary
pos <- scan('F:/courses/BA with R/Class 1/material/positive-words.txt', what='character', comment.char=';') 
neg <- scan('F:/courses/BA with R/Class 1/material/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words = c(pos, 'upgrade', 'awsum')
neg.words = c(neg, 'wtf', 'wait','waiting', 'epicfail', 'mechanical',"suspension","no")

Nadal.scores = score.sentiment(RAFA.df$text,pos.words,neg.words, .progress='text')
Nadal.scores

DEL.scores = score.sentiment(DEL.df$text,pos.words,neg.words, .progress='text')
DEL.scores

KEV.scores = score.sentiment(KEV.df$text,pos.words,neg.words, .progress='text')
KEV.scores

Pablo.scores = score.sentiment(Pablo.df$text,pos.words,neg.words, .progress='text')
Pablo.scores

Nadal.scores$Player = 'RAFA'
DEL.scores$Player = 'DEL Potro'
Pablo.scores$Player = 'Pablo'
KEV.scores$Player = 'KEV'


head(all.scores)
all.scores = rbind(Nadal.scores, DEL.scores, KEV.scores,Pablo.scores)
all.scores
?rbind
length(all.scores$score)
length(all.scores$Player)
score

table(all.scores$score,all.scores$Player)
table
ggplot(data=all.scores) + 
  geom_bar(mapping=aes(x=score, fill=Player)) + facet_grid(Player~.) +
  theme_bw() + scale_fill_brewer()

  # ggplot works on data.frames, always geom_bar(mapping=aes(x=score, fill=Team), binwidth=1) + facet_grid(Team~.) + # make a separate plot for each hashtag theme_bw() + scale_fill_brewer() # plain display, nicer colors

install.packages("twitteR")
install.packages("wordcloud")
install.packages("tm")

library("twitteR")
library("wordcloud")
library("tm")
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
