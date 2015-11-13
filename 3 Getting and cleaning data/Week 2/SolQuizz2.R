Question 1
library(httr)
library(httpuv)

# 1. Find OAuth settings for github:
# http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications
# Insert your values below - if secret is omitted, it will look it up in
# the GITHUB_CONSUMER_SECRET environmental variable.
#
# Use http://localhost:1410 as the callback url
myapp <- oauth_app("github", key="1028159b1765a2663241",secret="fdcf0d0f5d96a6708e63ad5b8e5b2ae4908515ce")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
stop_for_status(req)
content(req)


stuff<-GET("https://api.github.com/users/jtleek/repos",authenticate("74fb2a1d12c96823fa655289dbecce7ad6c7d761","x-oauth-basic","basic"))
cat(content(stuff, "text"), "\n")


Question 2
acs <- read.table("getdata_data_ss06pid.csv", sep=",", header=TRUE)
library(sqldf)

d1 <- sqldf("select * from acs where AGEP < 50 and pwgtp1")


Question 4:
con = url("http://biostat.jhsph.edu/~jleek/contact.html")
htmlCode = readLines(con)
close(con)
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])


Question 5:

file1 <- read.fwf("getdata_wksst8110.for",width=c(10,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)
sum(file1[4])