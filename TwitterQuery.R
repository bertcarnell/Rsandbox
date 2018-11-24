#######################################################
# SEE
# http://willchernoff.com/2013/05/27/authorizing-a-twitter-request-in-r/
# http://willchernoff.com/2013/05/25/download-twitter-data-using-json-in-r/

# set up an authorized access at
#  https://dev.twitter.com/apps
#  create a new application
#  Name:  R queries for testing
#  Description:  test twitter feeds using R
#  Website: only has to be a placeholder if you are just querying data - http://www.github.com
#  after accepting, create an access token also

## Install R packages
#install.packages(c('bitops', 'digest', 'RCurl', 'ROAuth', 'RJSONIO', 'plyr'))

## Load R packages
library(c('bitops', 'digest', 'RCurl', 'ROAuth', 'RJSONIO', 'plyr'))

## Set decimal precision
options(digits=22)

## OAuth application values
# consumer key, consumer secret, access token, and access token secret
#   from https://dev.twitter.com/apps
oauth <- data.frame(consumerKey='YourKeyHere',
                    consumerSecret='YourKeyHere',
                    accessToken='YourKeyHere',
                    accessTokenSecret='YourKeyHere')
# private key file
source(file.path("C:","Users","carnellr","Documents","Repositories",
                 "RSandbox","TwitterQueryOauth.DoNotCheckIn"))

################################################################################
## Download user tweets
user <- 'Reuters'

kv <- keyValues(httpmethod='GET',
                baseurl='https://api.twitter.com/1.1/statuses/user_timeline.json',
                par1a='count=100&include_rts=1&',
                par1b=paste('&screen_name=',user,sep=''))

theData1 <- fromJSON(
  getURL(
    paste(kv$bu,'?','oauth_consumer_key=',oauth$consumerKey,
          '&oauth_nonce=',kv$nonce,
          '&oauth_signature=',kv$osign,
          '&oauth_signature_method=HMAC-SHA1&oauth_timestamp=',kv$timestamp,
          '&oauth_token=',oauth$accessToken,
          '&oauth_version=1.0','&',kv$p,sep=''),
    ssl.verifypeer=FALSE
  )
)

################################################################################

kv <- createKeyValues(
  httpmethod='GET',
  baseurl='https://api.twitter.com/1.1/statuses/user_timeline.json',
  paramList=list
  (
    'count=100',
    'include_rts=1',
    'screen_name=dicknarby'
  ),
  oauth=oauth
)

theData1 <- fromJSON(
  getURL(
    paste(kv$bu,'?','oauth_consumer_key=', oauth$consumerKey,
          '&oauth_nonce=', kv$nonce,
          '&oauth_signature=', kv$osign,
          '&oauth_signature_method=HMAC-SHA1',
          '&oauth_timestamp=', kv$timestamp,
          '&oauth_token=', oauth$accessToken,
          '&oauth_version=1.0',
          '&', kv$p, sep=''),
    ssl.verifypeer=FALSE
  )
)

data <- as.data.frame(t(sapply(theData1, function(x) c(x$text, x$created_at, x$retweet_count, !is.null(x$retweeted_status)), USE.NAMES=FALSE)), stringsAsFactors=FALSE)
data$time <- strptime(data$V2, format="%a %b %d %H:%M:%S %z %Y")

windows()
plot(data$time, data$V3, xlab="Date", ylab="Number of Retweets", pch=19, 
     col=ifelse(data$V4, "red", "blue"))
legend("topleft", legend=c("Retweet", "Nick Original"), pch=19, col=c("red","blue"))

################################################################################

kv <- createKeyValues(
  httpmethod='GET',
  baseurl='https://api.twitter.com/1.1/search/tweets.json',
  paramList=list
  (
    'q=#realtalk',
    'count=100'
  ),
  oauth=oauth
)

theData1 <- fromJSON(
  getURL(
    paste(kv$bu,'?','oauth_consumer_key=', oauth$consumerKey,
          '&oauth_nonce=', kv$nonce,
          '&oauth_signature=', kv$osign,
          '&oauth_signature_method=HMAC-SHA1',
          '&oauth_timestamp=', kv$timestamp,
          '&oauth_token=', oauth$accessToken,
          '&oauth_version=1.0',
          '&', kv$p, sep=''),
    ssl.verifypeer=FALSE
  )
)

data <- as.data.frame(t(sapply(theData1$statuses, function(x) c(x$text, x$created_at, x$retweet_count, !is.null(x$retweeted_status)), USE.NAMES=FALSE)), stringsAsFactors=FALSE)
data$time <- strptime(data$V2, format="%a %b %d %H:%M:%S %z %Y")
data$isNick <- ifelse(sapply(theData1$statuses, function(x) c(x$user$screen_name), USE.NAMES=FALSE)=="dicknarby", TRUE, FALSE)

windows()
plot(data$time, data$V3, xlab="Date", ylab="Number of Retweets", pch=19, 
     col=ifelse(data$isNick, "red", "blue"))
legend("topleft", legend=c("Nick", "Other"), pch=19, col=c("red","blue"))

queryResults <- vector("list", length=4)
extractedResults <- vector("list", length=4)
for (i in 1:4) # iterations to keep from being rate limited
{
  kv <- createKeyValues(
    httpmethod='GET',
    baseurl='https://api.twitter.com/1.1/search/tweets.json',
    paramList=list
    (
      'q=#realtalk',
      'count=100'
    ),
    oauth=oauth
  )

  queryResults[[i]] <- fromJSON(
    getURL(
      paste(kv$bu,'?','oauth_consumer_key=', oauth$consumerKey,
            '&oauth_nonce=', kv$nonce,
            '&oauth_signature=', kv$osign,
            '&oauth_signature_method=HMAC-SHA1',
            '&oauth_timestamp=', kv$timestamp,
            '&oauth_token=', oauth$accessToken,
            '&oauth_version=1.0',
            '&', kv$p, sep=''),
      ssl.verifypeer=FALSE
    )
  )
  extractedResults[[i]] <- as.data.frame(
    t(sapply(
      queryResults[[i]]$statuses, 
      function(x) c(x$text, x$created_at, x$retweet_count, !is.null(x$retweeted_status)), 
      USE.NAMES=FALSE)), stringsAsFactors=FALSE)
  extractedResults[[i]]$time <- as.POSIXct(strptime(extractedResults[[i]]$V2, format="%a %b %d %H:%M:%S %z %Y"))
  extractedResults[[i]]$isNick <- ifelse(sapply(queryResults[[i]]$statuses, function(x) c(x$user$screen_name), USE.NAMES=FALSE)=="dicknarby", TRUE, FALSE)
}


windows()
plot(unlist(sapply(extractedResults, function(x) x$time, USE.NAMES=FALSE)),
     sapply(extractedResults, function(x) x$V3), 
     xlab="Date", ylab="Number of Retweets", pch=19, 
     col=ifelse(data$isNick, "red", "blue"))
legend("topleft", legend=c("Nick", "Other"), pch=19, col=c("red","blue"))

# http://www.rdatamining.com/examples/text-mining
# http://www.rdatamining.com/docs

require(tm)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(c(sapply(extractedResults, function(x) x$V1))))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, tolower)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
if (length(idx) != 0)
  myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
require(Snowball)
require(RWeka)
require(rJava)
require(RWekajars)
require(SnowballC)
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
inspect(myCorpus[1:3])
# stem completion
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
inspect(myDtm[266:270,31:40])

findFreqTerms(myDtm, lowfreq=10)
# which words are associated with "r"?
findAssocs(myDtm, 'graduate', 0.30)

require(wordcloud)
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
windows()
wordcloud(d$word, d$freq, min.freq=3)


################################################################################
# information from 8/12/2013

# https://dev.twitter.com/docs/api/1.1/get/statuses/user_timeline

# Call:
# https://api.twitter.com/1.1/statuses/user_timeline.json

#Always specify either an user_id or screen_name when requesting a user timeline.

# user_id optional(integer)         The ID of the user for whom to return results for.
# screen_name optional(string)      The screen name of the user for whom to return results for.
# since_id optional(integer)        Returns results with an ID greater than (that is, more recent than) the specified ID. There are limits to the number of Tweets which can be accessed through the API. If the limit of Tweets has occured since the since_id, the since_id will be forced to the oldest ID available.
# count optional(integer)           Specifies the number of tweets to try and retrieve, up to a maximum of 200 per distinct request. The value of count is best thought of as a limit to the number of tweets to return because suspended or deleted content is removed after the count has been applied. We include retweets in the count, even if include_rts is not supplied. It is recommended you always send include_rts=1 when using this API method.
# max_id optional(integer)          Returns results with an ID less than (that is, older than) or equal to the specified ID.
# trim_user optional(0/1)           When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID. Omit this parameter to receive the complete user object.
# exclude_replies optional(0/1)     This parameter will prevent replies from appearing in the returned timeline. Using exclude_replies with the count parameter will mean you will receive up-to count tweets - this is because the count parameter retrieves that many tweets before filtering out retweets and replies. This parameter is only supported for JSON and XML responses.
# contributor_details optional(0/1) This parameter enhances the contributors element of the status response to include the screen_name of the contributor. By default only the user_id of the contributor is included.
# include_rts optional(0/1)         When set to false, the timeline will strip any native retweets (though they will still count toward both the maximal length of the timeline and the slice selected by the count parameter). Note: If you're using the trim_user parameter in conjunction with include_rts, the retweets will still contain a full user object.

# https://dev.twitter.com/docs/api/1.1/get/search/tweets

# call:
# https://api.twitter.com/1.1/search/tweets.json

# q required(string)              A UTF-8, URL-encoded search query of 1,000 characters maximum, including operators. Queries may additionally be limited by complexity.
#     (ex. @noradio)
# geocode optional(comma sep)     Returns tweets by users located within a given radius of the given latitude/longitude. The location is preferentially taking from the Geotagging API, but will fall back to their Twitter profile. The parameter value is specified by "latitude,longitude,radius", where radius units must be specified as either "mi" (miles) or "km" (kilometers). Note that you cannot use the near operator via the API to geocode arbitrary locations; however you can use this geocode parameter to search near geocodes directly. A maximum of 1,000 distinct "sub-regions" will be considered when using the radius modifier.
#     (ex. 37.781157, -122.398720, 1mi)
# lang optional (string)          Restricts tweets to the given language, given by an ISO 639-1 code. Language detection is best-effort.
#     (ex. eu)
# locale optional(string)         Specify the language of the query you are sending (only ja is currently effective). This is intended for language-specific consumers and the default should work in the majority of cases.
#     (ex. ja)
# result_type optional(enum)      Specifies what type of search results you would prefer to receive. The current default is "mixed." Valid values include:
#                                 * mixed: Include both popular and real time results in the response.
#                                 * recent: return only the most recent results in the response
#                                 * popular: return only the most popular results in the response.
#     (ex. mixed, recent, popular)
# count optional(integer)         The number of tweets to return per page, up to a maximum of 100. Defaults to 15. This was formerly the "rpp" parameter in the old Search API.
# until optional(YYYY-MM-DD)      Returns tweets generated before the given date. Date should be formatted as YYYY-MM-DD. Keep in mind that the search index may not go back as far as the date you specify here.
# since_id optional(integer)      Returns results with an ID greater than (that is, more recent than) the specified ID. There are limits to the number of Tweets which can be accessed through the API. If the limit of Tweets has occured since the since_id, the since_id will be forced to the oldest ID available.
# max_id optional(integer)        Returns results with an ID less than (that is, older than) or equal to the specified ID.
# include_entities optional(0/1)  The entities node will be disincluded when set to false.
# callback optional               If supplied, the response will use the JSONP format with a callback of the given name. The usefulness of this parameter is somewhat diminished by the requirement of authentication for requests to this endpoint.
#     (ex processTweets)



