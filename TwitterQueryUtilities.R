keyValues <- function(httpmethod, baseurl, par1a, par1b)
{    
  # Generate a random string of letters and numbers
  # Generate random string of alphanumeric characters of length 32
  string <- paste(sample(c(letters[1:26],0:9), size=32, replace=T), collapse='')
  # Convert string to base64
  string2 <- base64(string,encode=TRUE,mode='character')
  # Remove non-alphanumeric characters (match anything not (^) a-z A-z or 0-9 and replace with nothing)
  nonce <- gsub('[^a-zA-Z0-9]', '', string2, perl=TRUE) 
  
  # Get the current GMT system time in seconds 
  timestamp <- as.character(floor(as.numeric(as.POSIXct(Sys.time(),tz='GMT'))))
  
  # Percent encode parameters 1
  par2a <- gsub(',','%2C',par1a,perl=TRUE) # Percent encode par
  par2b <- gsub(',','%2C',par1b,perl=TRUE) # Percent encode par
  
  # Percent ecode parameters 2
  # Order the key/value pairs by the first letter of each key
  ps <- paste(par2a,'oauth_consumer_key=',oauth$consumerKey,'&oauth_nonce=',nonce[1],'&oauth_signature_method=HMAC-SHA1&oauth_timestamp=',timestamp,'&oauth_token=',oauth$accessToken,'&oauth_version=1.0',par2b,sep='')
  ps2 <- gsub('%','%25',ps,perl=TRUE) 
  ps3 <- gsub('&','%26',ps2,perl=TRUE)
  ps4 <- gsub('=','%3D',ps3,perl=TRUE)
  
  # Percent encode parameters 3
  url1 <- baseurl
  url2 <- gsub(':','%3A',url1,perl=TRUE) 
  url3 <- gsub('/','%2F',url2,perl=TRUE) 
  
  # Create signature base string
  signBaseString <- paste(httpmethod,'&',url3,'&',ps4,sep='') 
  
  # Create signing key
  signKey <- paste(oauth$consumerSecret,'&',oauth$accessTokenSecret,sep='')
  
  # oauth_signature
  osign <- hmac(key=signKey,object=signBaseString,algo='sha1',serialize=FALSE,raw=TRUE)
  osign641 <- base64(osign,encode=TRUE,mode='character')
  osign642 <- gsub('/','%2F',osign641,perl=TRUE)
  osign643 <- gsub('=','%3D',osign642,perl=TRUE)
  osign644 <- gsub('[+]','%2B',osign643,perl=TRUE)
  
  return(data.frame(hm=httpmethod,
                    bu=baseurl,
                    p=paste(par1a,par1b,sep=''),
                    nonce=nonce[1],
                    timestamp=timestamp,
                    osign=osign644[1]))
}

createKeyValues <- function(httpmethod, baseurl, paramList, oauth)
{    
  # Generate a random string of letters and numbers
  # Generate random string of alphanumeric characters of length 32
  string <- paste(sample(c(letters[1:26],0:9), size=32, replace=T), collapse='')
  # Convert string to base64
  string2 <- base64(string,encode=TRUE,mode='character')
  # Remove non-alphanumeric characters (match anything not (^) a-z A-z or 0-9 and replace with nothing)
  nonce <- gsub('[^a-zA-Z0-9]', '', string2, perl=TRUE) 
  
  # Get the current GMT system time in seconds 
  timestamp <- as.character(floor(as.numeric(as.POSIXct(Sys.time(), tz='GMT'))))
  
  # Percent encoded parameters
  paramList2 <- lapply(paramList, function(x) gsub(',', '%2C', x, perl=TRUE))
  paramList3 <- lapply(paramList2, function(x) gsub('#', '%23', x, perl=TRUE))
  
  # Percent encode parameters 2
  # Order the key/value pairs by the first letter of each key
  ps <- c(unlist(paramList3),
          paste('oauth_consumer_key=', oauth$consumerKey, sep=""),
          paste('oauth_nonce=', nonce[1], sep=""),
          'oauth_signature_method=HMAC-SHA1',
          paste('oauth_timestamp=', timestamp, sep=""),
          paste('oauth_token=', oauth$accessToken, sep=""),
          'oauth_version=1.0')
  ps <- paste(ps[order(ps)], collapse="&",sep="")
  #ps <- paste(par2a,
  #            'oauth_consumer_key=', oauth$consumerKey,
  #            '&oauth_nonce=', nonce[1],
  #            '&oauth_signature_method=HMAC-SHA1&oauth_timestamp=', timestamp,
  #            '&oauth_token=',oauth$accessToken,
  #            '&oauth_version=1.0',
  #            par2b, sep='')
  ps2 <- gsub('%', '%25', ps,  perl=TRUE) 
  ps3 <- gsub('&', '%26', ps2, perl=TRUE)
  ps4 <- gsub('=', '%3D', ps3, perl=TRUE)
  
  # Percent encode parameters 3
  url1 <- baseurl
  url2 <- gsub(':', '%3A', url1, perl=TRUE) 
  url3 <- gsub('/', '%2F', url2, perl=TRUE) 
  
  # Create signature base string
  signBaseString <- paste(httpmethod, '&', url3, '&', ps4, sep='') 
  
  # Create signing key
  signKey <- paste(oauth$consumerSecret, '&', oauth$accessTokenSecret, sep='')
  
  # oauth_signature
  osign <- hmac(key=signKey, object=signBaseString, algo='sha1', 
                serialize=FALSE, raw=TRUE)
  osign641 <- base64(osign, encode=TRUE, mode='character')
  osign642 <- gsub('/',   '%2F', osign641, perl=TRUE)
  osign643 <- gsub('=',   '%3D', osign642, perl=TRUE)
  osign644 <- gsub('[+]', '%2B', osign643, perl=TRUE)
  
  return(
    list(hm=httpmethod,
         bu=baseurl,
         p=paste(unlist(paramList3), collapse="&", sep=""),
         nonce=nonce[1],
         timestamp=timestamp,
         osign=osign644[1]
    )
  )
}
