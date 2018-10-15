#******************************************
library(twitteR)
library(ngram)
library(tm)
library(SnowballC) #stemming Porters
library(RSentiment)
library(Quandl)
library(quantmod)
#******************************************

setup_twitter_oauth("zngU250W8JuxNSXnAMn4bJdSH", "4ox4xnnIZuQanRZ2xkXrNgXIxwHuTJPv09i80NnV0ADv4umPQW","2669848952-NVwkw7wRLbTYXX7lgQe5GGXH8ZHOjhAGB457jp5","OH1isZMxNKcrQ93CNTXod3gMFIk2WeXMrH4nSGRf5ZLuF")
fav1 = favorites("Amena__Bakr",n=10)
fav2=favorites("anasalhajji",n=10)
fav3=favorites("TankerTrackers",n=10)
fav4=favorites("JKempEnergy",n=10)
fav5=favorites("Lee_Saks",n=10)
fav6=favorites("JavierBlas2",n=10)
fav7=favorites("WSJenergy",n=10)
fav8=favorites("ErikSTownsend",n=10)
fav9=favorites("Statoil",n=10)
fav10=favorites("JoeMcMonigle",n=10)
fav11=favorites("IEABirol",n=10)
fav12=favorites("ArgusMediaOil",n=10)
fav13=favorites("Khalid_AlFalih",n=10)
fav14=favorites("FredericGonand",n=10)
fav15=favorites("Lisa_Ward1990",n=10)
fav16=favorites("AlexAndlauer",n=10)
fav17=favorites("WMALHittle",n=10)
fav18=favorites("Studebaker1963",n=10)
fav19=favorites("HamadeRiad",n=10)
fav20=favorites("StuartLWallace",n=10)
fav21=favorites("JLeeEnergy",n=10)
fav22=favorites("R_Zandi",n=10)
fav23=favorites("CollinEatonHC",n=10)
fav24=favorites("vsoldatkin",n=10)
fav25=favorites("ronbousso1",n=10)
fav26=favorites("ncitayim",n=10)
fav27=favorites("golnarM",n=10)
fav28=favorites("RowenaCaine",n=10)
fav29=favorites("EnergzdEconomy",n=10)
fav30=favorites("chris1reuters",n=10)
fav31=favorites("arascouet",n=10)
fav32=favorites("C_Barraud",n=10)
fav33=favorites("staunovo",n=10)
fav34=favorites("Ole_S_Hansen",n=10)
fav35=favorites("alexlongley1",n=10)
fav36=favorites("ValerieMarcel",n=10)
fav37=favorites("hgloystein",n=10)
fav38=favorites("summer_said",n=10)
fav39=favorites("ftcommodities",n=10)
fav40=favorites("Samir_Madani",n=10)
fav41=favorites("ja_herron",n=10)
fav42=favorites("wenkennedy",n=10)
fav43=favorites("WaelMahdi",n=10)
fav44=favorites("nayrazz",n=10)
fav45=favorites("T_Mason_H",n=10)
fav46=favorites("petromatrix",n=10)
fav47=favorites("AnjliRaval",n=10)
fav48=favorites("ErnestScheyder",n=10)
fav49=favorites("robinenergy",n=10)
fav50=favorites("Ed_Crooks",n=10)
fav51=favorites("Rory_Johnston",n=10)
fav52=favorites("OilandEnergy",n=10)
fav53=favorites("GBeleris",n=10)
fav54=favorites("IEA",n=10)
fav55=favorites("ReutersCommods",n=10)
fav56=favorites("humenm",n=10)
fav57=favorites("AlexLawler100",n=10)
fav58=favorites("OilSheppard",n=10)
fav59=favorites("mattpiotrowski",n=10)
fav60=favorites("EIAgov",n=10)
fav61=favorites("ArgusMedia",n=10)
fav62=favorites("GasBuddyGuy",n=10)
fav63=favorites("anasalhajji",n=10)
fav64=favorites("gs_squared",n=10)
fav65=favorites("TomKloza",n=10)
twit5=rbind(twListToDF(fav1),twListToDF(fav2),twListToDF(fav3),twListToDF(fav4),twListToDF(fav5),twListToDF(fav6),twListToDF(fav7),twListToDF(fav8),twListToDF(fav9),twListToDF(fav10),twListToDF(fav11),twListToDF(fav12),twListToDF(fav13),twListToDF(fav14),twListToDF(fav15),twListToDF(fav16),twListToDF(fav17),twListToDF(fav18),twListToDF(fav19),twListToDF(fav20),twListToDF(fav21),twListToDF(fav22),twListToDF(fav23),twListToDF(fav24),twListToDF(fav25),twListToDF(fav26),twListToDF(fav27),twListToDF(fav28),
           twListToDF(fav29),twListToDF(fav30),twListToDF(fav31),twListToDF(fav32),twListToDF(fav33),twListToDF(fav34),twListToDF(fav35),twListToDF(fav36),twListToDF(fav37),twListToDF(fav38),twListToDF(fav39),twListToDF(fav40),twListToDF(fav41),twListToDF(fav42),twListToDF(fav43),twListToDF(fav44),twListToDF(fav45),twListToDF(fav46),twListToDF(fav47),twListToDF(fav48),twListToDF(fav49),twListToDF(fav50),twListToDF(fav51),twListToDF(fav52),twListToDF(fav53),twListToDF(fav54),twListToDF(fav55),twListToDF(fav56),
           twListToDF(fav57),twListToDF(fav58),twListToDF(fav59),twListToDF(fav60),twListToDF(fav61),twListToDF(fav62),twListToDF(fav63),twListToDF(fav64),twListToDF(fav65))

word5<-"#oil" #OOTT
twit6<-searchTwitter(word5, n=1500,lang="en", resultType="recent")
#последние 10 дней история
#twit6<-searchTwitter(word5, since='2018-03-09', until='2018-03-10')
twit6<-twListToDF(twit6)
#pre-processing text-corpus
#twit6$text <- removeNumbers(twit6$text)
#twit6$text <- removePunctuation(twit6$text)
twit6$text <- removeWords(twit6$text, stopwords("english"))
#twit6$text <- stemDocument(twit6$text)
sent6<-calculate_total_presence_sentiment(twit6$text)

twit5<-twListToDF(twit5)
#pre-processing text-corpus
#twit5$text <- removeNumbers(twit5$text)
#twit5$text <- removePunctuation(twit5$text)
twit5$text <- removeWords(twit5$text, stopwords("english"))
#twit5$text <- stemDocument(twit5$text)
sent5<-calculate_total_presence_sentiment(twit5$text)

#write.csv(twit6, file = "C:/QRG/R/twit6.csv",row.names=TRUE)
#__________________________________________________________
#__________________________________________________________
sent5
sent6
#__________________________________________________________
#__________________________________________________________



#oil frequency analisys & n-gram

library(twitteR)
library(ngram)
library(tm)
library(SnowballC)
setup_twitter_oauth("zngU250W8JuxNSXnAMn4bJdSH", "4ox4xnnIZuQanRZ2xkXrNgXIxwHuTJPv09i80NnV0ADv4umPQW","2669848952-NVwkw7wRLbTYXX7lgQe5GGXH8ZHOjhAGB457jp5","OH1isZMxNKcrQ93CNTXod3gMFIk2WeXMrH4nSGRf5ZLuF")
twit=searchTwitter("#oil", n=1500,lang="en",resultType="recent")
#oil, usdrub, tax, russia, market, stock, forex, brent, wti, fed
twit=twListToDF(twit)
a=concatenate(twit$text)
a2=MC_tokenizer(twit$text)
a2=removeWords(a2, stopwords("english"))
c=stemDocument(a)
a1=ngram(c, n = 5, sep = "")
b1=get.phrasetable(a1)
d=termFreq(a2)
#sort(d,decreasing = TRUE)
#par(mfrow = c(1, 1));plot(d)
g=weightTf(d)
#n-gram
head(b1,50)
#frequency words
head(sort(g,decreasing = TRUE),50)
#write.csv(twit,"c:/QRG/Python/twit.csv",fileEncoding="utf-8")