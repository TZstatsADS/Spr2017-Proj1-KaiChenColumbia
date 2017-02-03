


presidentnames <-  c("George","Washington", "George",     "Washington", "John" ,      "Adams"  ,   
  "Thomas"  ,   "Jefferson" , "Thomas"  ,   "Jefferson" , "James" ,    "Madison"   ,
"James"   ,   "Madison" ,   "James" ,     "Monroe"    , "James"   ,   "Monroe"    ,
 "John"   ,    "Quincy",     "Adams" ,     "Andrew"   ,  "Jackson",    "Andrew"    ,
"Jackson"  ,  "Martin"  ,   "van"    ,    "Buren"     , "William" ,   "Henry"     ,
 "Harrison" ,  "James"   ,   "K."    ,     "Polk"     ,  "Zachary" ,   "Taylor"    ,
"Franklin"  , "Pierce"    , "James"   ,   "Buchanan"  , "Abraham" ,   "Lincoln"   ,
 "Abraham"  ,  "Lincoln" ,   "Ulysses" ,   "S."       ,  "Grant"  ,    "Ulysses"   ,
"S."        , "Grant"    ,  "Rutherford", "B."        , "Hayes"   ,   "James"     ,
 "Garfield" ,  "Grover"  ,   "Cleveland" , "-"        ,  "I"      ,    "Benjamin"  ,
 "Harrison" ,  "Grover"  ,   "Cleveland" , "-"        ,  "II"     ,    "William"   ,
"McKinley"  , "William"   , "McKinley"  , "Theodore"  , "Roosevelt",  "William"   ,
 "Howard"   ,  "Taft"   ,    "Woodrow"  ,  "Wilson"   ,  "Woodrow" ,   "Wilson"    ,
 "Warren"   ,  "G."      ,   "Harding"  ,  "Calvin"   ,  "Coolidge" ,  "Herbert"   ,
 "Hoover"   ,  "Franklin" ,  "D."  ,       "Roosevelt",  "Franklin"  , "D."        ,
"Roosevelt" , "Franklin" ,  "D."   ,      "Roosevelt" , "Franklin" ,  "D."        ,
 "Roosevelt",  "Harry"   ,   "S."  ,       "Truman"   ,  "Dwight"  ,   "D."        ,
"Eisenhower", "Dwight"   ,  "D."   ,      "Eisenhower" ,"John"     ,  "F."        ,
"Kennedy"   , "Lyndon"   ,  "B."   ,      "Johnson"   , "Richard"  ,  "Nixon"     ,
"Richard"   , "Nixon"    ,  "Jimmy" ,     "Carter"   ,  "Ronald"   ,  "Reagan"   , 
"Ronald"    , "Reagan"   ,  "George" ,    "Bush"    ,   "William"  ,  "J."      ,  
"Clinton"   , "William" ,   "J."     ,    "Clinton" ,   "George"   ,  "W."     ,   
"Bush"      , "George" ,    "W."     ,    "Bush" ,      "Barack"   ,  "Obama" ,    
"Barack"     ,"Obama" ,     "Donald"  ,   "J."  ,       "Trump" )  

main.page <- read_html(x = "http://www.presidency.ucsb.edu/inaugurals.php")
inaug=f.speechlinks(main.page)

inaug.temp=read.csv("../data/inauglist.csv", stringsAsFactors = FALSE)

# We need the year of each speech
inaug <- inaug[-nrow(inaug),] 
inaug.time <- as.Date(inaug[,1], format="%B %e, %Y")



findpresident <- function(year){
  i.pres <- sum(inaug.time < paste0(year+1,"-01-01")) + 1
  return(inaug.temp[i.pres,])
}

Create_processed_corpus <- function(folder.path, tfidf = T, tidyornot = T){

	ff.all<-Corpus(DirSource(folder.path))	
			
	ff.all<-tm_map(ff.all, stripWhitespace)
	ff.all<-tm_map(ff.all, removeWords, presidentnames)
	ff.all<-tm_map(ff.all, content_transformer(tolower))
	ff.all<-tm_map(ff.all, removePunctuation)
	ff.all<-tm_map(ff.all, removeWords, stop_words$word)
	ff.all<-tm_map(ff.all, removeWords, character(0))
	return(ff.all)

}	
#	if (tfidf)
#	tdm <- TermDocumentMatrix(ff.all, 
#								control = list(weighting = function(x)
#                                             weightTfIdf(x, 
 #                                                        normalize =F),
#                                        stopwords = TRUE))
#    else
 #   tdm <- TermDocumentMatrix(ff.all)
#	if (tidyornot)
#    	return(tidy(tdm))
#    else
#    	return(tdm)                                
#}

#Create_tdm_for_comparison <- function(source1, source2){
	

#}

