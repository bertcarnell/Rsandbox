require(RCurl)

url <- "https://login.verizonwireless.com/amserver/UI/Login"

listCurlOptions()

han <- getCurlHandle()
curlSetOpt(.opts=list(cookiejar="c:/cookie.txt",
                      useragent="Firefox/23.0",
                      ssl.verifypeer=FALSE,
                      ssl.verifyhost=FALSE),
           curl=han)


returnPage <- getURL(url, ssl.verifypeer=FALSE, curl=han)
regexpr("User ID or Mobile Number", returnPage)

ind <- gregexpr("<input", returnPage)
substring(returnPage, ind[[1]], ind[[1]]+100)

returnPage <- postForm(url, 
                       .params=list(IDToken1="5132553340",
                                    realm="vzw",
                                    goto="",
                                    gotoOnFail="",
                                    gx_charset="UTF-8",
                                    rememberUserNameCheckBoxExists="Y",
                                    iLType="",
                                    userNameOnly="true",
                                    rememberUserName="Y"), 
                       style="POST", 
                       curl=han)
cat(returnPage, file="~/test.html")
browseURL("~/test.html")
if (regexpr("What is the name of your first pet", returnPage) > 0)
{
  print("pet question")
  returnPage <- postForm(url, 
                         .params=list(userID="5132553340",
                                      goto="",
                                      displayLoginStart="true",
                                      IDToken1="Tiki",
                                      rememberComputer="Y",
                                      rememberComputerCheckBoxExists="Y"),
                         style="POST", 
                         curl=han)
  cat(returnPage, file="~/test.html")
  browseURL("~/test.html")
}
  
returnPage <- postForm(url, 
                       .params=list(IDToken1="5132553340",
                                    realm="vzw",
                                    goto="",
                                    gotoOnFail="",
                                    gx_charset="UTF-8",
                                    rememberUserNameCheckBoxExists="",
                                    displayLoginStart="true",
                                    IDToken2="NJMrcc2008"), 
                       .opts=list(referer=url),
                       style="HTTPPOST", 
                       curl=han)
cat(returnPage, file="~/test.html")
browseURL("~/test.html")


