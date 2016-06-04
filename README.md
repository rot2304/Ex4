# דו"ח מסכם- עבודה 4

##שלב ראשון- ניקוי נתוני האימון:
ניקוי העמודה "שאילתא":
```{r}
train_data <- read.csv("train.csv")
 train_data$query <-  gsub("<.*?>", "", train_data$query) 
    train_data$query <-  gsub("&nbsp;", " ", train_data$query)
    dfCorpus = Corpus(VectorSource(train_data$query)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    train_data$query<-tolower(dataframe$text) 
```

ניקוי העמודה "תיאור מוצר":
```{r}
  train_data$product_description <-  gsub("<.*?>", "", train_data$product_description) 
    train_data$product_description <-  gsub("&nbsp;", " ", train_data$product_description)
    dfCorpus = Corpus(VectorSource(train_data$product_description)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    train_data$product_description<-tolower(dataframe$text) 
```

ניקוי העמודה "שם מוצר":
```{r}
 train_data$product_title <-  gsub("<.*?>", "", train_data$product_title) 
    train_data$product_title <-  gsub("&nbsp;", " ", train_data$product_title)
    dfCorpus = Corpus(VectorSource(train_data$product_title)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    train_data$product_title<-tolower(dataframe$text) 
```

##שלב שני- חישוב מדד הדימיון בין פיצ'רים שונים:
דימיון בין עמודת "שאילתא" לבין עמודת "שם מוצר":
```{r}
  for (i in 1:length(train_data$query)){ 
    if(train_data$product_title[i]!="")
    {
     temp <- c(train_data$query[i],train_data$product_title[i])
     myDfm <- dfm(temp, verbose = FALSE)
     sim <- similarity(myDfm, docnames(myDfm), margin = "documents", method = "cosine")
     train_data$sim_query_title[i] <- sim$text2[["text1"]]
    }
      else
      {
        train_data$sim_query_title[i]=0
      }
    }
```

דימיון בין המאפיין "שאילתא" לבין המאפיין "תיאור מוצר":
```{r}
for (z in 1:length(train_data$query)){ 
      if(train_data$product_description[z]!="")
      {
      temp <- c(train_data$query[z],train_data$product_description[z])
      myDfm <- dfm(temp, verbose = FALSE)
      sim <- similarity(myDfm, docnames(myDfm), margin = "documents", method = "cosine")
      train_data$sim_query_description[z] <- sim$text2[["text1"]]
      }
      else
        {
          train_data$sim_query_description[z] = 0
        }
    } 
    ```
    
דימיון q-gram
בין "שאילתא" ל"שם מוצר":
```{r}
  for (z in 1:length(train_data$query)){ 
      if(train_data$product_title[z]!="")
      {
       qgra_sim <- stringsim(train_data$query[z],train_data$product_title[z],method='qgram', q=4)
       train_data$simq_query_title[z] <- qgra_sim
      }
      else
      {
        train_data$simq_query_title[z] = 0
      }
    } 
       ```
       
    דימיון q-gram 
    בין "שאילתא" ל"תיאור מוצר":
    ```{r}
     for (z in 1:length(train_data$query)){ 
      if(train_data$product_description[z]!="")
      {
        qgra_sim <- stringsim(train_data$query[z],train_data$product_description[z],method='qgram', q=4)
        train_data$simq_query_description[z] <- qgra_sim
      }
      else
      {
        train_data$simq_query_description[z] = 0
      }
    } 
      ```
      
      מרחק לוינשטין בין "שאילתא" ל"שם מוצר":
       ```{r}
      for (z in 1:length(train_data$query)){ 
      if(train_data$product_title[z]!="")
      {
        lv_sim <- levenshteinSim(train_data$query[z],train_data$product_title[z])
        train_data$simlv_query_title[z] <- lv_sim
        lv_distance <- levenshteinDist(train_data$query[z],train_data$product_title[z])
        train_data$dislv_query_title[z] <- lv_distance
      }
      else
      {
        train_data$simlv_query_title[z] = 0
        train_data$dislv_query_title[z] = 0
      }
    } 
      ```
