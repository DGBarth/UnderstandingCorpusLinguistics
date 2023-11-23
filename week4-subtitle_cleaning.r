
#week 4 - subtitle data

library(data.table)
library(srt) #let's use a special package
library(corpus) #for our stopwords
library(ggplot2)
library(wordcloud2)
library(splitstackshape)


#we will load 3 subtitle files, join them, label variables and then clean them
#we will then tokenize our texts and create w-2, w-1, w+1, w+2 contexts
#we will also do a few basic calculations
#we will then create some basic graphs

#today we will work with the subtitles from masterpieces of cinema: frozen 2, jumanji 2 & no country for old men
froz<-as.data.table(read_srt(file.choose(), collapse = " ")) #this is multiple functions inside each other to save lines of code
froz
jum<-as.data.table(read_srt(file.choose(), collapse = " "),encoding="UTF-8")
jum
old<-as.data.table(read_srt(file.choose(), collapse = " "))
old

#before we join, let's add some metadata to each file. if we use consistent column names, we can join these easily
froz$film<-"frozen2"
froz$year<-2019
froz$genre<-"childrens"
froz$length<-103
froz$lang<-"English"
froz$subtitler<-"Pizlo, bobset & supertooth"

jum$film<-"jumanji2"
jum$year<-2019
jum$genre<-"action"
jum$length<-123
jum$lang<-"English"
jum$subtitler<-"explosiveskull"

old$film<-"no.country.for.old.men"
old$year<-2007
old$genre<-"crime"
old$length<-122
old$lang<-"English"
old$subtitler<-"unknown"

films<-rbind(froz,jum,old)

films

#remove lines with <i>Re-Sync...</i> from frozen
#remove strings <i>...</i>, but keep contents from frozen & jumanji
#remove â™ª (music symbol), but label these columns as singing
#remove lines with [...] from jumanji
#remove "subtitles by *" from jumanji
#remove NIGEL: and similar from jumanji

#grep is for searching. it returns a string containing what you are looking for
films[grep("<i>",subtitle)]
films[grep("<i>Re",subtitle)]

films<-films[!grep("<i>Re",subtitle)] #this will delete lines containing <i>Re (! means don't include). We replace old 'films' with updated 'films'

films[grep("<i>",subtitle)]

#here we have to make some decisions. what do we keep and why?

#ifelse statement contains a test, then what to do if true, then what to do if false
#grepl is grep + logical so it returns a logical value of true/false rather than returning the result of the grep
films$discourse<-ifelse(grepl("â™ª",films$subtitle),"singing","talking")
films[films$discourse=="singing",]

films$subtitle<-gsub("â™ª", "", films$subtitle) #substitute "â™ª" for nothing is a way to delete it
films$subtitle<-gsub("<.?i>", "", films$subtitle) #this is a regular expression that finds <i> or </i> because .? means optionally any character

films[grep("<i>",subtitle)]
films[films$n==89]

films[grep("\\[",subtitle)] #if I want to find [ symbol, I need to escape it. meaning I need to tell R that I'm really looking for the character, not what [ means in a regex
films<-films[!grep("\\[",subtitle)]
films[films$film=="jumanji2"]
nrow(jum)

#let's get rid of the first row of jumanji and the "subtitles by" lines
films<-films[!grep("CBR",subtitle)]   #remember we are including ! to mean not
films<-films[!grep("Subtitles",subtitle)]

films[films$film=="jumanji2"]

films[grep("[A-Z]+:",subtitle)]
films$subtitle<-gsub(("[A-Z]+:"),"",films$subtitle) #substitute the whole name string by putting () around it with ""

films[films$film=="jumanji2"]

#That's all the cleaning we will do for these subtitles
#now let's change our data shape by putting one word on each row. we will keep the phrase column for context

films$phrase<-films$subtitle

#library(splitstackshape)

films2<-cSplit(films,"subtitle", "\\W", direction = "long", fixed=F)
films2

head(films2,20) #are we happy with the time s split? What about goblin s? This is a question you can decide on. You can substitute 's, 'm, 're for _s, etc before splitting to keep words as time_s, goblin_s, etc.

#there are some blank lines, let's drop those
films2<-films2[!films2$subtitle=="",]
films2
films2$subtitle<-tolower(films2$subtitle)

#now that we have nice tabular data, let's think about some each calculations we can do and include as columns in our data

#how many words per film?
films2[,film.words := .N, by="film"]
#how many words per phrase?
films2[,phrase.words := .N, by=list(phrase,film)]
films2[,phrase.words := .N, by=list(n,film)]

#now we can create columns with preceding and following words. but we don't want to join up the words for different films. I have a function for this


lagleadR02<-function(DT,V1,lab,NUM,GRP1=NULL,GRP2=NULL){				#specify arguments: data.table, column to concordance, how many words bf/after, by group variables, null means they don't need to be specified, up to 2 by group vars possible with this function
  require("data.table")									#need the data.table package for this to work
  pcols = as.character(seq( from = NUM, to = 1, by = -1 ))			#get a sequence of numbers, high to low
  panscols = paste(lab, pcols, sep="_pre")						#create variable names for preceding words
  fcols = as.character(seq( from = 1, to = NUM, by = 1 ))			#get a sequence of numbers, low to high
  fanscols = paste(lab, fcols, sep="_post")						#create variable names for following words
  if( (is.null(GRP1)) & (is.null(GRP2)) ){ 						#if both by group vars are null (unspecified/missing)
    DT[, (panscols) := shift(get(V1), NUM:1,type= "lag")]			#preceding words, data.table syntax: paste names of new columns, shift, get() needed because it's RHS/char 
    DT[, (fanscols) := shift(get(V1), 1:NUM,type="lead")]			#following words
  } else {		
    if( is.null(GRP2) ) {											#if just one by group variable
      DT[, (panscols) := shift(get(V1), NUM:1,type= "lag"),by=list(get(GRP1))]	#adding by argument, get() needed because it's potentially RHS/char	
      DT[, (fanscols) := shift(get(V1), 1:NUM,type="lead"),by=list(get(GRP1))]
    } else {													#if both group variables used
      DT[, (panscols) := shift(get(V1), NUM:1,type= "lag"),by=list(get(GRP1),get(GRP2))]
      DT[, (fanscols) := shift(get(V1), 1:NUM,type="lead"),by=list(get(GRP1),get(GRP2))] 
    }
  }
  return(DT)
}

#use the function
 lagleadR02(films2,"subtitle","w",2,"film")

films2
colnames(films2)
setcolorder(films2,neworder=c(1:3,5:12,15:16,4,17:18,13:14))
films2

#we can do some more calculations
films2[,phrase.len:=end-start]
films2[,phrase.rate:=phrase.len/phrase.words]
films2[,film.rate:=length*60/film.words]
films2[,word.len:=nchar(subtitle)]
films2[,word.len.avg := mean(word.len), by ="film"]
films2[, word_count_film := .N, by = list(subtitle,film)] #.N counts the instances of things
films2[, word_count_corpus := .N, by = "subtitle"] 


films2
write.csv(films2,file.choose()) #save your dataset

#think about what other kinds of information you could easily extract from your dataset

#now we have some data and some variables. Let's make some visualisations using the code from week 3
#please compare the commands from week 3 and 4 to see what aspects changed and what was held constant. that will help you to know what YOU need to change


#reduce tokens to types by getting the unique tokens
film.dtmA<-unique(films2[,c("subtitle","word_count_corpus")])
#I want to keep more columns for second dtm, so I put more in the c()
films.3.dtmA<-unique(films2[,c("subtitle","word_count_film","word_count_corpus","film")])

#getting rid of lists in stopwords list + extras that I noticed working with the files
film.dtm<-film.dtmA[! film.dtmA$subtitle %in% c(stopwords_en,"s","re","m","don","t","ll","d","ve"),]  
films.3.dtm<-films.3.dtmA[! films.3.dtmA$subtitle %in% c(stopwords_en,"s","re","m","don","t","ll","d","ve"),] 

film.dtm
films.3.dtm

setkey(film.dtm,word_count_corpus)
setkey(films.3.dtm,word_count_film)

film.dtm
films.3.dtm

#basic frequency graph
ggplot(subset(film.dtm, word_count_corpus > 40) ,aes(reorder(subtitle, -word_count_corpus), word_count_corpus))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + ggtitle("Subtitle word frequency (over 40 tokens, stopwords removed)")+
  xlab("Word")+
  ylab("Frequency")

ggplot(subset(films.3.dtm, word_count_film > 40) ,aes(reorder(subtitle, -word_count_film), word_count_film))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + ggtitle("Subtitle word frequency by film (over 40 tokens per film, stopwords removed)")+
  xlab("Word")+
  ylab("Frequency")+
  facet_grid("film")

#let's avoid the problem of eliminating interesting words just because they do not appear often enough in each corpus
#we combine info from word_count_corpus & word_count_film in this ggplot
ggplot(subset(films.3.dtm, word_count_corpus > 60) ,aes(reorder(subtitle, -word_count_corpus), word_count_film))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=.5))+ 
  ggtitle("Subtitle word frequency by film (over 60 tokens in corpus, stopwords removed)")+
  xlab("Word")+
  ylab("Frequency")+
  facet_grid("film")


#word cloud
wordcloud2(data=film.dtm[film.dtm$word_count_corpus >10,], size = 1, color="random-dark", shape="square", hoverFunction=NULL)
wordcloud2(data=films.3.dtm[films.3.dtm$film=="frozen2",], size = 1, color="random-dark", shape="square", hoverFunction=NULL)

wordcloud2(data=films.3.dtm[films.3.dtm$film=="jumanji2",], size = 1, color="random-dark", shape="diamond", hoverFunction=NULL)
wordcloud2(data=films.3.dtm[films.3.dtm$film=="no.country.for.old.men" & films.3.dtm$word_count_corpus > 10,], size = 1, color="random-light", shape="triangle", hoverFunction=NULL)


#zipfian log-log plot
#lets use film.dtmA which does not have stopwords excluded
film.dtm2 <- film.dtmA[order(-word_count_corpus)] 
film.dtm2[, rank:= .I, by="word_count_corpus"] #we are adding a column for the frequency rank
film.dtm2

ggplot(film.dtm2 ,aes(rank,word_count_corpus))+
  geom_point(position=position_jitter(width=.05,height=.05))+ 
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Film Corpus log-log plot")+
  xlab("Rank (on log scale)")+
  ylab("Frequency (on log scale)")

ggplot(film.dtm2 ,aes(rank,word_count_corpus))+
  geom_point()+ #without jitter
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Film Corpus log-log plot")+
  xlab("Rank (on log scale)")+
  ylab("Frequency (on log scale)")

#let's try plotting word length by characters by film. What would this really represent?
#for this we go back to the original films2
films2

ggplot(films2 ,aes(film,word.len))+
  geom_point(position=position_jitter(width=.05,height=.05))+
  ggtitle("Average Vocabulary Score by Film")+
  xlab("Film")+
  ylab("Word Length")+
  theme_linedraw()

ggplot(films2 ,aes(film,word.len))+
  geom_point()+
  ggtitle("Average Vocabulary Score by Film")+
  xlab("Film")+
  ylab("Word Length")+
  facet_grid("year")+
  theme_minimal()

ggplot(unique(films2[,c("film","word.len.avg")]) ,aes(film,word.len.avg,fill=film))+
  geom_bar(stat="identity")+
    ggtitle("Average Vocabulary Score by Film")+
  xlab("Film")+
  ylab("Word Length Average")

ggplot(unique(films2[,c("genre","film.rate")]) ,aes(reorder(genre,-film.rate),film.rate))+
  geom_bar(stat="identity")+
  ggtitle("Average Speech Rate by Film Genre")+
  xlab("Genre")+
  ylab("Film Speech Rate")+
  theme(plot.background = element_rect(fill = "lightblue3"),panel.background = element_rect(fill = "pink"))

