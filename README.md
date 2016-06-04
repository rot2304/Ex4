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

