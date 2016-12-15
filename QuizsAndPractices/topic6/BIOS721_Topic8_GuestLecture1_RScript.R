#####################################################
#                                                   #
# Text Mining & Visualizations                      #
# Allen Ross                                        #
# 12/1/16                                           #
#                                                   #
#####################################################

# Install required packages if not done so already
install.packages(c("tm","dplyr","wordcloud","ggplot2","reshape2","gridExtra","plotly"))

source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

# Libraries
library(tm)            # Text Mining
library(Rgraphviz)     # Association plot
library(wordcloud)     # Word cloud
library(ggplot2)       # GGplots
library(reshape2)      # Data manipulation
library(gridExtra)     # Multiple plots
library(plotly)        # Plotly plots

#####################################################
#                                                   #
# Example 1 (PDF Books)                             #
#                                                   #
#####################################################

docs <- Corpus(DirSource("C:/Classes/Duke/Other/Texts"))   
docs <- Corpus(DirSource("D:/GitHub/biostats721/QuizsAndPractices/topic6"))
#summary(docs)   
#inspect(docs)

#as.character(docs[[1]])[1:50]

# Preprocessing
docs<-tm_map(docs, content_transformer(tolower))
head(stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs<-tm_map(docs, stemDocument)
docs<-tm_map(docs, removePunctuation)
docs<-tm_map(docs, removeNumbers)
docs<-tm_map(docs, stripWhitespace)
docs<-tm_map(docs, removeWords, c("chapter","one","two","three"))


# Create document term matrix and transpose
doc.mat<-DocumentTermMatrix(docs)
inspect(doc.mat[,5000:5003])

term.mat<-TermDocumentMatrix(docs)
inspect(term.mat[5000:5003,])

# Explore DTM
freq<-sort(colSums(as.matrix(doc.mat)),decreasing=T)
length(freq) # Number of words in DTM

head(table(freq)) # See how many low count words occur
tail(table(freq)) # See how many high count words occur

# Remove sparse terms
doc.mat.s<-removeSparseTerms(doc.mat,sparse=0.25)
freq.s<-sort(colSums(as.matrix(doc.mat.s)),decreasing=T)
head(freq.s)
head(table(freq.s))
tail(table(freq.s))

# Look for associations
findFreqTerms(doc.mat.s,lowfreq=1000)
findAssocs(doc.mat.s,"heaven",corlimit=.999)
findAssocs(doc.mat,"heaven",corlimit=0.999)

# Some plots
plot(doc.mat.s,terms=findFreqTerms(doc.mat.s,lowfreq=1750),corThreshold=0.5,
     attrs=list(node=list(shape="rectangle",fontsize=150)))

set.seed(123)
wordcloud(names(freq), freq, min.freq=1000, 
          scale=c(2,.1), colors=brewer.pal(5, "Accent"))

set.seed(456)
wordcloud(names(freq.s), freq.s, min.freq=1000, 
          scale=c(2,.1), colors=brewer.pal(5, "Spectral"))


# Plot with ggplot2
top.terms<-findFreqTerms(doc.mat.s,lowfreq=2000)
word.freq<-as.data.frame(t(as.matrix(doc.mat.s)))
word.freq$term<-row.names(word.freq)
word.freq.t<-melt(word.freq,by="term",variable.name='text',value.name='count')

ggplot(word.freq.t[word.freq.t$term%in%top.terms,],aes(x=term,y=count))+geom_bar(stat='identity')

ggplot(word.freq.t[word.freq.t$term%in%top.terms,],aes(x=term,y=count,fill=text))+geom_bar(stat='identity')

ggplot(word.freq.t[word.freq.t$term%in%top.terms,],aes(x=term,y=count,fill=text))+geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=45,hjust=1))

ggplot(word.freq.t[word.freq.t$term%in%top.terms,],aes(x=term,y=count,fill=text))+geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x="Word",y="Count",title="Top Words in Corpus")

ggplot(word.freq.t[word.freq.t$term%in%top.terms,],aes(x=term,y=count,fill=text))+geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x="Word",y="Count",title="Top Words in Corpus")+
  scale_fill_manual(values=c("yellow","blue","green","black"),name="Book\nTitle",breaks=c("dracula.txt","journeytothewest.txt","ramayana.txt",
        "tokillamockingbird.txt"),labels=c("Dracula","Journey to the West","Ramayana","To Kill a Mockingbird"))

word.freq.scale<-as.data.frame(t(apply(word.freq[,1:4],1,scale)))
names(word.freq.scale)<-names(word.freq)[1:4]
word.freq.scale$term<-row.names(word.freq.scale)
word.freq.scale.t<-melt(word.freq.scale,by=term,variable.name='text',value.name='scaled')

# Top n words function
freq.s.df<-as.data.frame(freq.s)
freq.s.df$term<-rownames(freq.s.df)

top.n<-function(n) return(freq.s.df[1:n,'term'])

ggplot(word.freq.scale.t[word.freq.scale.t$term%in%top.n(25),],aes(term,text))+geom_tile(aes(fill=scaled),colour="white")+
  scale_fill_gradient(low="white",high="red")+theme(axis.text.x=element_text(angle=45,hjust=1))

# Try 2
word.freq.scale2<-as.data.frame(apply(word.freq[,1:4],2,scale))
word.freq.scale2$term<-row.names(word.freq)
word.freq.scale2.t<-melt(word.freq.scale2,by=term,variable.name='text',value.name='scaled')

ggplot(word.freq.scale2.t[word.freq.scale2.t$term%in%top.n(25),],aes(term,text))+geom_tile(aes(fill=scaled),colour="white")+
  scale_fill_gradient(low="white",high="royalblue")+theme(axis.text.x=element_text(angle=45,hjust=1))

# Throw both heatmaps together
a<-ggplot(word.freq.scale.t[word.freq.scale.t$term%in%top.n(25),],aes(term,text))+geom_tile(aes(fill=scaled),colour="white")+
    scale_fill_gradient(low="white",high="red")+theme(axis.text.x=element_text(angle=45,hjust=1))

b<-ggplot(word.freq.scale2.t[word.freq.scale2.t$term%in%top.n(25),],aes(term,text))+geom_tile(aes(fill=scaled),colour="white")+
  scale_fill_gradient(low="white",high="royalblue")+theme(axis.text.x=element_text(angle=45,hjust=1))

grid.arrange(a,b)

# Plotly example with ~2400 words
sc.mat<-matrix(word.freq.scale2.t$scaled,nrow=4,ncol=length(freq.s))
plot_ly(x=word.freq.scale2.t$term,y=word.freq.scale2.t$text,
        z=sc.mat,type='heatmap',colorscale='Greys',showscale=F) %>% 
        layout(xaxis=list(title='Term',showticklabels=F,ticks=""), 
               yaxis=list(title='Text',showticklabels=F,ticks=""))

#####################################################
#                                                   #
# Example 2 (Twitter Feed)                          #
#                                                   #
#####################################################


# Read twitter data in
tf <- read.csv("demonetization-tweets.csv")

# Subset tweets to exclude twitter handle
tf$text[1]
gsub("^[^:]+:", "", tf$text[1])
tf$text<-gsub("^[^:]+:", "", tf$text)

# Read into corpus
tf.doc<-Corpus(VectorSource(tf$text))
as.character(tf.doc[[1]])

tf.doc<-tm_map(tf.doc, content_transformer(tolower))
tf.doc<-tm_map(tf.doc, removeWords, stopwords("english"))
tf.doc<-tm_map(tf.doc, stemDocument)
tf.doc<-tm_map(tf.doc, removePunctuation)
tf.doc<-tm_map(tf.doc, removeNumbers)
tf.doc<-tm_map(tf.doc, stripWhitespace)
as.character(tf.doc[[1]])

tf.dtm<-DocumentTermMatrix(tf.doc)
dim(tf.dtm)
tf.dtm.s<-removeSparseTerms(tf.dtm,sparse=0.90)
inspect(tf.dtm.s[1:10,])
# Cannot use sparse dtm

tf.freq<-sort(colSums(as.matrix(tf.dtm)),decreasing=T)
head(tf.freq)

findFreqTerms(tf.dtm,lowfreq=500)

plot(tf.dtm,terms=findFreqTerms(tf.dtm,lowfreq=500),corThreshold=0.5,
     attrs=list(node=list(shape="rectangle",fontsize=50)))
set.seed(123)
wordcloud(names(tf.freq), tf.freq, min.freq=300, 
          scale=c(2,1), colors=brewer.pal(5, "PiYG"))

# Sentiment analysis
install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer)

pos <- sum(sapply(tf.doc, tm_term_score, terms_in_General_Inquirer_categories("Positiv")))
neg <- sum(sapply(tf.doc, tm_term_score, terms_in_General_Inquirer_categories("Negativ")))
tf.tdm<-TermDocumentMatrix(tf.doc)
pos.score <- tm_term_score(tf.tdm, 
                           terms_in_General_Inquirer_categories("Positiv")) 

neg.score <- tm_term_score(tf.tdm, 
                           terms_in_General_Inquirer_categories("Negativ")) 

total.df <- data.frame(id = c(1:nrow(tf.dtm)),positive = pos.score, negative = neg.score)
total.df <- transform(total.df, net = positive - negative)
head(total.df)
summary(total.df)

coll.df<-data.frame(total=colSums(total.df[,-1]),type=names(total.df)[-1])

ggplot(coll.df,aes(x=type,y=total))+geom_bar(stat='identity')

ggplot(coll.df,aes(x=type,y=total,fill=type))+geom_bar(stat='identity')+
  scale_x_discrete(limits=c('net', 'negative', 'positive'))+
  scale_fill_discrete(breaks=c("positive","negative","net"))+
  theme(legend.title=element_blank())+
  labs(x="Polarity",y="Total Count",title="Twitter Word Count Polarity")+
  coord_flip()

# Net significantly different from zero?
wilcox.test(total.df$net)

# Lets try to weight these more
fav.weight<-data.frame(id=c(1:nrow(tf.dtm)),favweight=total.df$net*tf$favoriteCount)
# How many zero favorites?
nrow(fav.weight[fav.weight$favweight==0,]) # 7836 not favorited we should delete them
fav.weight.nozero<-fav.weight[fav.weight$favweight!=0,] # Leaves 164
ggplot(fav.weight.nozero,aes(favweight))+geom_density()

retweet.weight<-data.frame(id=c(1:nrow(tf.dtm)),retweetweight=total.df$net*tf$retweetCount)
nrow(retweet.weight[retweet.weight$retweetweight==0,]) # 4341 not retweeted
retweet.weight.nozero<-retweet.weight[retweet.weight$retweetweight!=0,] # Leaves 3659
ggplot(retweet.weight.nozero,aes(retweetweight))+geom_density()

wilcox.test(fav.weight$favweight)
wilcox.test(retweet.weight$retweetweight)

# Throw all together in one 3-D plot
# Include color for PM Modi
pm.count.words<-c('pm','narendramodi','modi','narendra')
pm.count<-as.matrix(DocumentTermMatrix(tf.doc,list(dictionary=pm.count.words)))
pm.count.ind<-ifelse(rowSums(pm.count)>0,'PM Mentioned','PM Not Mentioned')

td.tf.plot<-data.frame(favoriteCount=tf$favoriteCount,retweetCount=tf$retweetCount,
                       net=total.df$net,modi=as.factor(pm.count.ind))
plot_ly(td.tf.plot,x=~favoriteCount,y=~retweetCount,z=~net,color=~modi,colors=c('red','blue')) %>%
  add_markers() %>%
  layout(scene=list(xaxis=list(title='Favorited Count'),
                    yaxis=list(title='Retweet Count'),
                    zaxis=list(title='Net Polarity')))

