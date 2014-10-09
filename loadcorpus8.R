library(tm)
ptm <- proc.time()
setwd('/Users/raoulsullivan/Documents/datascience/capstone')
set.seed('1337')

#subcorpus <- readRDS("corpus.rds")
print('corpus - reloaded')
print (proc.time() - ptm)

#unigrams <- readRDS("unigrams.rds")
#bigrams <- readRDS("bigrams.rds")
#trigrams <- readRDS("newtrigrams.rds")
print('Ngrams ready')
print (proc.time() - ptm)

#hugeframe <- data.frame(trigrams[1:length(trigrams)])
#colnames(hugeframe) <- 'freq'
#hugeframe<-hugeframe[order(hugeframe$freq,decreasing=T),]
#hugeframe$ngram <- row.names(hugeframe)
#hugeframe$pregram <- sapply(hugeframe[,'ngram'],function(x){paste(strsplit(x," ")[[1]][1:2],collapse = " " )})
#hugeframe$finalword <- sapply(hugeframe[,'ngram'],function(x){strsplit(x," ")[[1]][3]})

#hugeframe <- readRDS("hugeframe.rds")
hugeframe <- readRDS("hugetable.rds")
#Discard anything with a count less than 1
#hugeframe <- hugeframe[hugeframe$freq > 1,]
#hugeframe <- data.table(hugeframe,key="pregram")
countallwordtypes <- nrow(hugeframe)

absolute_probability <- function(input){
    ptm <- proc.time()
    #Absolute discounting smoothing
    #Take the count of each term, less .75 (becasue GT calculation is boring) over the count of all stem terms
    trigramsofinterest <- hugeframe[grepl(paste("^",input,"$",sep=""),hugeframe$pregram),]
    print(paste("Total number of trigrams of interest: ",nrow(trigramsofinterest)))
    totaltrigramfrequencycount <- sum(trigramsofinterest$freq)
    print(paste("Total frequency count: ",totaltrigramfrequencycount))
    for (i in 1:nrow(trigramsofinterest)) {
        term <- trigramsofinterest[i]$finalword
        freq <- trigramsofinterest[i]$freq
        #term <- trigramsofinterest[i,'finalword']
        #freq <- trigramsofinterest[i,'freq']
        print(paste("Word is '",term,"' with a frequency of ",freq))
        pabs <- max((freq - 0.75),0)/totaltrigramfrequencycount
        cont_prob <- continuation_probablility(term)
        lambda <- (0.75/totaltrigramfrequencycount) * nrow(trigramsofinterest)
        trigramsofinterest[i]$pabs <- pabs
        trigramsofinterest[i]$cont_prob <- cont_prob
        trigramsofinterest[i]$lambda <- lambda
        trigramsofinterest[i]$knprob <- pabs + (lambda*cont_prob)
        #trigramsofinterest[i,'pabs'] <- pabs
        #trigramsofinterest[i,'cont_prob'] <- cont_prob
        #trigramsofinterest[i,'lambda'] <- lambda
        #trigramsofinterest[i,'knprob'] <- pabs + (lambda*cont_prob)
    }
    print (proc.time() - ptm)
    return(trigramsofinterest)  
}

continuation_probablility <- function(input){
    countallwordtypesprecedinginput<-nrow(hugeframe[hugeframe$finalword == input,])
    return (countallwordtypesprecedinginput / countallwordtypes)
}