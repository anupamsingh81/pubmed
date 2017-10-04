#https://www.r-bloggers.com/how-to-search-pubmed-with-rismed-package-in-r/
#https://rstudio-pubs-static.s3.amazonaws.com/47034_f8430cd23bd84fc2a90f1fe60fa8637c.html
library(RISmed)
library(ggplot2)

#summary term
vitamin_A<-  EUtilsSummary("vitamin A", type="esearch", db="pubmed", datetype='pdat', mindate=1950, maxdate=2017, retmax=60000)

#object download,takes a lot of time

fetch <- EUtilsGet(vitamin_A,type="efetch", db="pubmed")

?EUtilsGet

?YearPubmed

count <- table(YearPubmed(fetch))

count<-as.data.frame(count)

# plot year without downloading object

search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}


papers <- sapply(year, search_year, term="Connectome", USE.NAMES=FALSE)
#https://ropensci.org/tutorials/rentrez_tutorial.html
3
year <- 1960:2017

papersa <- sapply(year, FUN= search_year, term="vitamin A", USE.NAMES=FALSE)

papersd <- sapply(year, FUN= search_year, term="vitamin D", USE.NAMES=FALSE)

paperse <- sapply(year, FUN= search_year, term="vitamin E", USE.NAMES=FALSE)

papersc <- sapply(year, FUN= search_year, term="vitamin C", USE.NAMES=FALSE)


papersa
search_year(1960,"vitamin D")

plot(year, paperse, type='b', main="vitamin D")

paper = papersa

paper = paper[-58]

papersa = papersa[-58]
papersc = papersc[-58]
papersd = papersd[-58]
paperse = paperse[-58]
year= year[-58]

count = c(papersa,papersc,papersd,paperse)
publication_year = c(year,year,year,year)
vitamin = c(rep("Vitamin A",57),rep("Vitamin C",57),rep("Vitamin D",57),rep("Vitamin E",57))

df = data.frame(vitamin,publication_year,count)

library(ggplot2)

ggplot(df,aes(x=publication_year,y=count,group=vitamin))+geom_line(aes(color=vitamin))

library(tidyverse)
df1 =df %>% group_by(publication_year,vitamin)  %>% spread(key = vitamin, value = count, fill=0) 
row_sum <- rowSums(df1[,2:5])

df2<- mutate_all(df1[,2:5],funs(./row_sum)) %>% bind_cols(df1)
# better

df3 =    mutate_all(df1[,2:5],funs(./row_sum)) %>% mutate(year = 1960:2016) %>% gather(key=vitamin, value= Proportion, 1:4)

# very good data wrangling tutorial
# https://rpubs.com/bradleyboehmke/data_wrangling

library(ggthemes)

ggplot(df3,aes(x=year,y=Proportion,group=vitamin))+geom_line(aes(color=vitamin)) + scale_x_continuous(breaks=seq(1960,2016,5))+theme_economist()

# Never use %>%  in ggplot otherwise cant coerce environment error