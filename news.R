library(alphavantager)
library(xts)
library(extremevalues)
library(h2o)
library(lmtest)
library(dplyr)
library(RSentiment)
library(tm)

#API alphavantager
av_api_key("YBKRXTMZ1LYJPGCW")
usdrub_intraday <-
  av_get(
    symbol = "USDRUB",
    av_fun = "TIME_SERIES_INTRADAY",
    interval = "60min",
    outputsize = "full"
  )
plot(usdrub_intraday$close, type = "l")

#read price
data_usdrub <- read.csv(
  file = "c:/QRG/R/reuters/usdrub.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)

#convert to xts
data_usdrub <- read.csv(
  file = "c:/QRG/R/reuters/usdrub.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)

#data cleaning
x <- na.omit(x)
data_usdrub$USDRUB_TOM <- ifelse(data_usdrub$USDRUB_TOM > 0,
                                 data_usdrub$USDRUB_TOM,
                                 NA)
x <- na.omit(data_usdrub)
x.Date <- as.Date(x$Date)
x <- zoo(x$USDRUB_TOM, x.Date)
usdrub <- as.xts(x)
plot(usdrub)

#subsets
usdrub1 <- usdrub["2008-01-01/2009-03-01"]
usdrub2 <- usdrub["2009-03-01/2012-06-30"]
usdrub3 <- usdrub["2012-07-01/2014-10-31"]
usdrub4 <- usdrub["2014-11-01/2016-01-31"]
usdrub5 <- usdrub["2016-02-01/2018-03-25"]

#descriptive statistics
summary(usdrub1)
summary(usdrub2)
summary(usdrub3)
summary(usdrub4)
summary(usdrub5)

#autocorrelation function
acf(na.omit(diff(usdrub1)))
acf(na.omit(diff(usdrub2)))
acf(na.omit(diff(usdrub3)))
acf(na.omit(diff(log(usdrub4))))
acf(na.omit(diff(usdrub5)))

#outliers
y1 <- as.numeric(na.omit(diff(usdrub1)))
y2 <- as.numeric(na.omit(diff(usdrub2)))
y3 <- as.numeric(na.omit(diff(usdrub3)))
y4 <- as.numeric(na.omit(diff(log(usdrub4))))
y5 <- as.numeric(na.omit(diff(usdrub5)))
K <- getOutliers(y1, method = "I", distribution = "normal")
L <- getOutliers(y1, method = "II", distribution = "normal")
par(mfrow = c(1, 2))
outlierPlot(y1, K, mode = "qq")
outlierPlot(y1, L, mode = "residual")
result_negitive <-
  na.omit(data.frame(ifelse(y1 < K$yMin, y1, NA),
                     time(usdrub1[2:length(usdrub1)])))
result_positive <-
  na.omit(data.frame(ifelse(y1 > K$yMax, y1, NA),
                     time(usdrub1[2:length(usdrub1)])))

#clustering lag/return
h2o.init()
ret1 <- as.numeric(na.omit(diff(usdrub1)))
ret1_lag5 <- as.numeric(na.omit(diff(lag(usdrub1, 5))))
ret1_lag30 <- as.numeric(na.omit(diff(lag(usdrub1, 30))))
ret2 <- as.numeric(na.omit(diff(usdrub2)))
ret3 <- as.numeric(na.omit(diff(usdrub3)))
ret4 <- as.numeric(na.omit(diff(log(usdrub4))))
ret5 <- as.numeric(na.omit(diff(usdrub5)))
data <- data.frame(ret1[31:length(ret1)],
                   ret1_lag5[26:length(ret1_lag5)], ret1_lag30)
colnames(data) <- c("lag0", "lag5", "lag30")
model.hex <- as.h2o(data)
cluster <- h2o.kmeans(
  training_frame = model.hex,
  k = 2,
  x = c("lag0", "lag5", "lag30"),
  nfolds = 3,
  seed = 2
)
cluster@model$centers

#auto-encoders
model.dl <- h2o.deeplearning(
  x = 1:3,
  training_frame = model.hex,
  autoencoder = TRUE,
  hidden = c(20, 10, 20),
  epochs = 100,
  max_w2 = 10,
  l1 = 1e-5,
  activation = "RectifierWithDropout"
)
model.anon.per.feature <- h2o.anomaly(model.dl, model.hex,
                                      per_feature = TRUE)
model.anon.per.feature

#regression lm, glm
#read feautures
data <- read.csv(
  file = "c:/QRG/R/regression/regr_dataset.csv",
  header = TRUE,
  sep = ";",
  dec = ","
)
regr <- na.omit(data)
regr <-
  data.frame(
    regr$DATE,
    regr$CCUSMA02RUM618N,
    regr$CCRETT01RUM661N,
    regr$CPALTT01RUM657N,
    regr$CPALTT01RUM659N,
    regr$CPGDFD01RUM661N,
    regr$CPGDLF01RUM661N,
    regr$INTDSRRUM193N,
    regr$IR3TIB01RUM156N,
    regr$IRLTCT01RUM156N,
    regr$IRLTLT01RUM156N,
    regr$IRSTCB01RUM156N,
    regr$LMUNRLTTRUM647N,
    regr$LMUNRLTTRUM647S,
    regr$LMUNRRTTRUM156N,
    regr$LMUNRRTTRUM156S,
    regr$LRUN74TTRUM156N,
    regr$LRUNTTTTRUM156N,
    regr$MABMM201RUM189N,
    regr$MABMM201RUM189S,
    regr$MABMM301RUM189N,
    regr$MABMM301RUM189S,
    regr$MABMM301RUM657S,
    regr$MAM3A4RUM189N,
    regr$MANMM101RUM189N,
    regr$MANMM101RUM189S,
    regr$MANMM101RUM657S,
    regr$MANMM102RUM189N,
    regr$MANMM102RUM189S,
    regr$MYAGM2RUM189N,
    regr$NBRUBIS,
    regr$PIEATI02RUM661N,
    regr$RBRUBIS,
    regr$RUSCPIALLMINMEI,
    regr$RUSEPUINDXM,
    regr$RUSPPDMMINMEI,
    regr$RUSPROINDMISMEI,
    regr$SPASTT01RUM657N,
    regr$SPASTT01RUM661N,
    regr$XTEITT01RUM156N,
    regr$XTEITT01RUM156S,
    regr$XTEXVA01RUM657S,
    regr$XTEXVA01RUM659S,
    regr$XTEXVA01RUM664S,
    regr$XTEXVA01RUM667N,
    regr$XTEXVA01RUM667S
  )
#correlation matrix
regr_cor <- regr[2:length(regr)]
cor(regr_cor)
x1.Date <- as.Date(regr$regr.DATE)
regr <- xts(regr[2:length(regr)], x1.Date)

#read price
price <- regr$regr.CCUSMA02RUM618N

#fit regression
x1 <- regr[, 2]
x2 <- regr[, 3]
x3 <- regr[, 4]
y <- price
model.lm <- lm(y ~ x1 + x2 + x3)
summary(model.lm)
dwtest(model.lm)

#GBM-regression
#data for train,validation,test
data <- data.frame(ret1[31:length(ret1)],
                   ret1_lag5[26:length(ret1_lag5)], ret1_lag30)
colnames(data) <- c("lag0", "lag5", "lag30")
model.hex <- as.h2o(data)
model2.hex <- as.h2o(regr_cor)
r <- h2o.runif(model.hex)
r2 <- h2o.runif(model2.hex)
data_train.hex <- model.hex[r < 0.6, ]
data_train2.hex <- model2.hex[r2 < 0.6, ]
data_valid.hex <- model.hex[(r >= 0.6) & (r < 0.9), ]
data_valid2.hex <- model2.hex[(r2 >= 0.6) & (r2 < 0.9), ]
data_test.hex <- model.hex[r >= 0.9, ]
data_test2.hex <- model2.hex[r2 >= 0.9, ]

#GBM
model.gbm <- h2o.gbm(
  y = 1,
  x = 2:3,
  training_frame = data_train.hex,
  validation_frame = data_valid.hex,
  ntrees = 100,
  max_depth = 4,
  learn_rate = 0.1,
  seed = 1234
)
model2.gbm <- h2o.gbm(
  y = 1,
  x = 2:3,
  training_frame = data_train2.hex,
  validation_frame = data_valid2.hex,
  ntrees = 100,
  max_depth = 4,
  learn_rate = 0.1,
  seed = 1234
)

#test and prediction
perf_gbm <- h2o.performance(model.gbm, data_test.hex)
perf2_gbm <- h2o.performance(model2.gbm, data_test2.hex)
pred_gbm <- h2o.predict(model.gbm, data_test.hex)
pred2_gbm <- h2o.predict(model2.gbm, data_test2.hex)

#FNN-regression
model.dl <- h2o.deeplearning(
  x = 2:3,
  y = 1,
  training_frame = data_train.hex,
  validation_frame = data_valid.hex,
  hidden = c(20, 10, 20),
  epochs = 100,
  max_w2 = 10,
  l1 = 1e-5,
  activation = "RectifierWithDropout"
)

perf_dl <- h2o.performance(model.dl, data_test.hex)
pred_dl <- h2o.predict(model.dl, data_test.hex)

#NLP
#1.Dictionary
#2.LSTM

#price usrub
x <- na.omit(x)
data_usdrub$USDRUB_TOM <- ifelse(data_usdrub$USDRUB_TOM > 0,
                                 data_usdrub$USDRUB_TOM,
                                 NA)
x <- na.omit(data_usdrub)
x.Date <- as.Date(x$Date)
x <- zoo(x$USDRUB_TOM, x.Date)
usdrub <- as.xts(x)
plot(usdrub)

news_sep_17 <- read.csv(
  file = "c:/QRG/R/reuters/news_sep_2017.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_sep_17$Date, "%d.%m.%Y")
news_sep_17_xts <- xts(news_sep_17[2:5], x1.Date)

news_oct_17 <- read.csv(
  file = "c:/QRG/R/reuters/news_oct_2017.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_oct_17$Date, "%d.%m.%Y")
news_oct_17_xts <- xts(news_oct_17[2:5], x1.Date)

news_nov_17 <- read.csv(
  file = "c:/QRG/R/reuters/news_nov_2017.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_nov_17$Date, "%d.%m.%Y")
news_nov_17_xts <- xts(news_nov_17[2:5], x1.Date)

news_dec_17 <- read.csv(
  file = "c:/QRG/R/reuters/news_dec_2017.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_dec_17$Date, "%d.%m.%Y")
news_dec_17_xts <- xts(news_dec_17[2:5], x1.Date)

news_jan_18 <- read.csv(
  file = "c:/QRG/R/reuters/news_jan_2018.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_jan_18$Date, "%d.%m.%Y")
news_jan_18_xts <- xts(news_jan_18[2:5], x1.Date)

news_feb_18 <- read.csv(
  file = "c:/QRG/R/reuters/news_feb_2018.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_feb_18$Date, "%d.%m.%Y")
news_feb_18_xts <- xts(news_feb_18[2:5], x1.Date)

news_mar_18 <- read.csv(
  file = "c:/QRG/R/reuters/news_mar_2018.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)
x1.Date <- as.Date(news_mar_18$Date, "%d.%m.%Y")
news_mar_18_xts <- xts(news_mar_18[2:5], x1.Date)

news <- rbind(
  news_sep_17_xts,
  news_oct_17_xts,
  news_nov_17_xts,
  news_dec_17_xts,
  news_jan_18_xts,
  news_feb_18_xts,
  news_mar_18_xts
)

news <- rbind(
  news_sep_17,
  news_oct_17,
  news_nov_17,
  news_dec_17,
  news_jan_18,
  news_feb_18,
  news_mar_18
)

#news classification (usdrub - yes/no)
STOP_WORDS = c(
  "ax",
  "i",
  "you",
  "edu",
  "s",
  "t",
  "m",
  "subject",
  "can",
  "lines",
  "re",
  "what",
  "there",
  "all",
  "we",
  "one",
  "the",
  "a",
  "an",
  "of",
  "or",
  "in",
  "for",
  "by",
  "on",
  "but",
  "is",
  "in",
  "a",
  "not",
  "with",
  "as",
  "was",
  "if",
  "they",
  "are",
  "this",
  "and",
  "it",
  "have",
  "from",
  "at",
  "my",
  "be",
  "by",
  "not",
  "that",
  "to",
  "from",
  "com",
  "org",
  "like",
  "likes",
  "so"
)

data2 <- read.csv(
  file = "c:/QRG/R/reuters/news_train.csv",
  header = TRUE,
  sep = ";",
  dec = ".",
  stringsAsFactors = FALSE
)

train.hex <- as.h2o(data2)
data2.news <- as.h2o(data2$News)
words <- h2o.tokenize(as.character(data2.news), "\\\\W+")
tokenized.lower <- h2o.tolower(words)
# remove short words (less than 2 characters)
tokenized.lengths <- h2o.nchar(tokenized.lower)
tokenized.filtered <-
  tokenized.lower[is.na(tokenized.lengths) ||
                    tokenized.lengths >= 2,]
# remove words that contain numbers
tokenized.words <-
  tokenized.filtered[h2o.grep("[0-9]",
                              tokenized.filtered,
                              invert = TRUE,
                              output.logical = TRUE),]

# remove stop words
words <-
  tokenized.words[is.na(tokenized.words) ||
                    (!tokenized.words %in% STOP_WORDS),]
#word2vec
model.word2vec <-
  h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)
vecs <-
  h2o.transform(model.word2vec, words, aggregate_method = "AVERAGE")
valid <- !is.na(vecs$C1)

#GBM
data <-
  h2o.cbind(as.factor(train.hex[valid, "Class"]), vecs[valid,])

data.split <- h2o.splitFrame(data, ratios = 0.95)
gbm.model <- h2o.gbm(
  x = names(vecs),
  y = "Class",
  training_frame = data.split[[1]],
  validation_frame = data.split[[2]]
)

#prediction!
pred.hex <- as.h2o(news)
words <- h2o.tokenize(as.character(pred.hex), "\\\\W+")
tokenized.lower <- h2o.tolower(words)
tokenized.lengths <- h2o.nchar(tokenized.lower)
tokenized.filtered <-
  tokenized.lower[is.na(tokenized.lengths) ||
                    tokenized.lengths >= 2,]
tokenized.words <-
  tokenized.filtered[h2o.grep("[0-9]",
                              tokenized.filtered,
                              invert = TRUE,
                              output.logical = TRUE),]
words <-
  tokenized.words[is.na(tokenized.words) ||
                    (!tokenized.words %in% STOP_WORDS),]
model.word2vec <-
  h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)
vecs <-
  h2o.transform(model.word2vec, words, aggregate_method = "AVERAGE")
valid <- !is.na(vecs$C1)

data_new <- vecs[valid,]
data_new2 <- vecs

#forecast for news (for dollars and others)
pred_gbm <- h2o.predict(gbm.model, data_new2)
pred_gbm

news <- pred.hex[pred_gbm$predict == "yes", ]

#transformation h2o.frame--->data.frame
news <- as.data.frame(news)

#merge datasets
#data_news <-
#  merge(news[1:5], usdrub["2017-09-01/2017-09-30"], join = "inner")
#data_news <- rbind(news[1:5], usdrub)
news$Date <- as.Date(news$Date, format = "%d.%m.%Y")
data_usdrub$Date <- as.Date(data_usdrub$Date, format = "%Y-%m-%d")
data_news <- inner_join(news[1:5], data_usdrub)

#filter news by ticker
#второй датасет - по тикеру, первый датасет отобран через wodr2vec и GBM
data_news <-
  filter(
    data_news,
    data_news$Ticker == "RUBUTSTN=MCX" |
      data_news$Ticker == "RUB=" |
      data_news$Ticker == "OFCB.MM" |
      data_news$Ticker == "RUBUTSTN=MCX"
  )

usdrub_sep_17 <- x["2017-09-01/2017-09-30"]
usdrub_oct_17 <- x["2017-10-01/2017-10-31"]
usdrub_nov_17 <- x["2017-11-01/2017-11-31"]
usdrub_dec_17 <- x["2017-12-01/2017-12-31"]
usdrub_jan_18 <- x["2018-01-01/2018-01-31"]
usdrub_feb_18 <- x["2018-02-01/2018-02-28"]
usdrub_mar_18 <- x["2018-03-01/2018-03-31"]

#sentiment analisys/lexicon-approach

#tokenizer for machine learning
#f <- MC_tokenizer(e)

#r-sentiment for each days
start <- as.Date("2017-09-01", format = "%Y-%m-%d")
end   <- as.Date("2018-03-31", format = "%Y-%m-%d")

theDate <- start

g <- data_news#filter(data_news, data_news$Date == "2017-09-01")
h <-  g$News
#pre-processing text-corpus
h <- removeNumbers(h)
h <- removePunctuation(h)
h <- removeWords(h, stopwords("english"))
#stemming in a text document using Porter`s algorithm
h <- stemDocument(h)
h <- tolower(h)
#sent <- calculate_total_presence_sentiment(h)

#инициализация сентимента и цены для первой даты
news_frame <- data.frame(t(sent[2,]), g$Date[1], g$USDRUB_TOM[1])
colnames(news_frame) <-
  c("X1", "X2", "X3", "X4", "X5", "X6", "Date", "Price")
news_frame_all <- rbind(news_frame)

#запуск объединения цены и сентимента по всем датам
while (theDate <= end)
{
  #theDate = "2017-09-01"
  g <- filter(data_news, data_news$Date == theDate)
  h <-  g$News
  #pre-processing text-corpus
  h <- removeNumbers(h)
  h <- removePunctuation(h)
  h <- removeWords(h, stopwords("english"))
  #stemming in a text document using Porter`s algorithm
  h <- stemDocument(h)
  
  if (nrow(g) != 0)
  {
    sent <- calculate_total_presence_sentiment(h)
    
    news_frame <-
      data.frame(t(sent[2, ]), g$Date[1], g$USDRUB_TOM[1])
    colnames(news_frame) <-
      c("X1", "X2", "X3", "X4", "X5", "X6", "Date", "Price")
    news_frame_all <- rbind(news_frame, news_frame_all)
  }
  
  theDate <- theDate + 1
}

#news_frame_all<-na.omit(news_frame_all)
Y <- diff(as.numeric(news_frame_all$Price))
X1 <- as.numeric(news_frame_all$X1[2:nrow(news_frame_all)])
X2 <- as.numeric(news_frame_all$X2[2:nrow(news_frame_all)])
X3 <- as.numeric(news_frame_all$X3[2:nrow(news_frame_all)])
X4 <- as.numeric(news_frame_all$X4[2:nrow(news_frame_all)])
X5 <- as.numeric(news_frame_all$X5[2:nrow(news_frame_all)])
model.sent <- lm(Y ~ X2 + X3 + X4 + X5)
summary(model.sent)
