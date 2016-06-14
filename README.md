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

מרחק לוינשטיין בין "שאילתא" ל"תיאור מוצר":
 ```{r}
      for (z in 1:length(train_data$query)){ 
      if(train_data$product_description[z]!="")
      {
        lv_sim <- levenshteinSim(train_data$query[z],train_data$product_description[z])
        train_data$simlv_query_description[z] <- lv_sim
        lv_distance <- levenshteinDist(train_data$query[z],train_data$product_description[z])
        train_data$dislv_query_description[z] <- lv_distance
      }
      else
      {
        train_data$simlv_query_description[z] = 0
        train_data$dislv_query_description[z] = 0
      }
    } 
```

דימיון ג'אקרד בין "שאילתא" ל"שם מוצר":
 ```{r}
  for (z in 1:length(train_data$query)){ 
      if(train_data$product_title[z]!="")
      {
        jac_sim <- stringsim(train_data$query[z],train_data$product_title[z],method='jaccard', q=4)
        train_data$simjac_query_title[z] <- jac_sim
      }
      else
      {
        train_data$simjac_query_title[z] = 0
      }
    } 
    
```

דימיון ג'אקרד בין "שאילתא" ל"תיאור מוצר":
 ```{r}
  for (z in 1:length(train_data$query)){ 
      if(train_data$product_description[z]!="")
      {
        jac_sim <- stringsim(train_data$query[z],train_data$product_description[z],method='jaccard', q=4)
        train_data$simjac_query_description[z] <- jac_sim
      }
      else
      {
        train_data$simjac_query_description[z] = 0
      }
    } 
```

##שלב שלישי- יצירת הפיצ'ר "מילים משותפות" בין מאפיינים שונים:
מציאת מילים משותפות  בין "שאילתא" ל"שם המוצר":
 ```{r}
  for (z in 1:length(train_data$query)){ 
      if(train_data$product_title[z]!="")
      {
        a <- train_data$query[z]
        b <- train_data$product_title[z]
        a_split <- unlist(strsplit(a, split=" "))
        b_split <- unlist(strsplit(b, split=" "))
        train_data$sim_intresect[z]  <- length(intersect(a_split, b_split))
       
      }
      else
      {
        train_data$sim_intresect[z] = 0
      }
    } 
```

## שלב רביעי- ניקוי נתוני המבחן:
ניקוי העמודה שאילתא:
 ```{r}
   test_data <- read.csv("test.csv", header=TRUE)
    test_data$query <-  gsub("<.*?>", "", test_data$query) 
    test_data$query <-  gsub("&nbsp;", " ", test_data$query)
    dfCorpus = Corpus(VectorSource(test_data$query)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    test_data$query<-tolower(dataframe$text) 
```
ניקוי העמודה "תיאור מוצר":
```{r}
    test_data$product_description <-  gsub("<.*?>", "", test_data$product_description) 
    test_data$product_description <-  gsub("&nbsp;", " ", test_data$product_description)
    dfCorpus = Corpus(VectorSource(test_data$product_description)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    test_data$product_description<-tolower(dataframe$text) 
```
ניקוי העמודה "שם מוצר":
```{r}
   test_data$product_title <-  gsub("<.*?>", "", test_data$product_title) 
    test_data$product_title <-  gsub("&nbsp;", " ", test_data$product_title)
    dfCorpus = Corpus(VectorSource(test_data$product_title)) 
    dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))   
    dfCorpus <- tm_map(dfCorpus, removePunctuation) 
    dataframe<-data.frame(text=unlist(sapply(dfCorpus, `[`, "content")), stringsAsFactors=F)
    test_data$product_title<-tolower(dataframe$text) 
```

##שלב חמישי- חישוב מדד הדימיון בין מאפיינים שונים בנתוני המבחן:
דימיון בין עמודת "שאילתא" לבין עמודת "שם מוצר":
```{r}
   for (i in 1:length(test_data$query)){ 
      if(test_data$product_title[i]!="")
      {
        temp <- c(test_data$query[i],test_data$product_title[i])
        myDfm <- dfm(temp, verbose = FALSE)
        sim <- similarity(myDfm, docnames(myDfm), margin = "documents", method = "cosine")
        test_data$sim_query_title[i] <- sim$text2[["text1"]]
      }
      else
      {
        test_data$sim_query_title[i]=0
      }
    }
```

דימיון בין המאפיין "שאילתא" לבין המאפיין "תיאור מוצר":
```{r}
 for (z in 1:length(test_data$query)){ 
      if(test_data$product_description[z]!="")
      {
        temp <- c(test_data$query[z],test_data$product_description[z])
        myDfm <- dfm(temp, verbose = FALSE)
        sim <- similarity(myDfm, docnames(myDfm), margin = "documents", method = "cosine")
        test_data$sim_query_description[z] <- sim$text2[["text1"]]
      }
      else
      {
        test_data$sim_query_description[z] = 0
      }
    } 
```


דימיון q-gram
בין "שאילתא" ל"שם מוצר":
```{r}
   for (z in 1:length(test_data$query)){ 
      if(test_data$product_title[z]!="")
      {
        qgra_sim <- stringsim(test_data$query[z],test_data$product_title[z],method='qgram', q=4)
        test_data$simq_query_title[z] <- qgra_sim
      }
      else
      {
        test_data$simq_query_title[z] = 0
      }
    } 
```

דימיון q-gram
בין "שאילתא" ל"תיאור מוצר":
```{r}
 for (z in 1:length(test_data$query)){ 
      if(test_data$product_description[z]!="")
      {
        qgra_sim <- stringsim(test_data$query[z],test_data$product_description[z],method='qgram', q=4)
        test_data$simq_query_description[z] <- qgra_sim
      }
      else
      {
        test_data$simq_query_description[z] = 0
      }
    } 
```


 מרחק לוינשטין בין "שאילתא" ל"שם מוצר":
 ```{r}
 for (z in 1:length(test_data$query)){ 
      if(test_data$product_title[z]!="")
      {
        lv_sim <- levenshteinSim(test_data$query[z],test_data$product_title[z])
        test_data$simlv_query_title[z] <- lv_sim
        lv_distance <- levenshteinDist(test_data$query[z],test_data$product_title[z])
        test_data$dislv_query_title[z] <- lv_distance
      }
      else
      {
        test_data$simlv_query_title[z] = 0
        test_data$dislv_query_title[z] = 0
      }
    } 
```

מרחק לוינשטיין בין "שאילתא" ל"תיאור מוצר":
 ```{r}
 for (z in 1:length(test_data$query)){ 
      if(test_data$product_description[z]!="")
      {
        lv_sim <- levenshteinSim(test_data$query[z],test_data$product_description[z])
        test_data$simlv_query_description[z] <- lv_sim
        lv_distance <- levenshteinDist(test_data$query[z],test_data$product_description[z])
        test_data$dislv_query_description[z] <- lv_distance
      }
      else
      {
        test_data$simlv_query_description[z] = 0
        test_data$dislv_query_description[z]= 0
      }
    } 
```


דימיון ג'אקרד בין "שאילתא" ל"שם מוצר":
 ```{r}
    for (z in 1:length(test_data$query)){ 
      if(test_data$product_title[z]!="")
      {
        jac_sim <- stringsim(test_data$query[z],test_data$product_title[z],method='jaccard', q=4)
        test_data$simjac_query_title[z] <- jac_sim
      }
      else
      {
        test_data$simjac_query_title[z] = 0
      }
    } 
```
דימיון ג'אקרד בין "שאילתא" ל"תיאור מוצר":
 ```{r}
  for (z in 1:length(test_data$query)){ 
      if(test_data$product_description[z]!="")
      {
        jac_sim <- stringsim(test_data$query[z],test_data$product_description[z],method='jaccard', q=4)
        test_data$simjac_query_description[z] <- jac_sim
      }
      else
      {
        test_data$simjac_query_description[z] = 0
      }
    } 
```

##שלב שישי- מציאת מילים משותפות בין מאפיינים בנתוני המבחן:
מציאת מילים משותפות  בין "שאילתא" ל"שם המוצר":
 ```{r}
 for (z in 1:length(test_data$query)){ 
      if(test_data$product_title[z]!="")
      {
        a <- test_data$query[z]
        b <- test_data$product_title[z]
        a_split <- unlist(strsplit(a, split=" "))
        b_split <- unlist(strsplit(b, split=" "))
        test_data$sim_intresect[z]  <- length(intersect(a_split, b_split))
        
      }
      else
      {
        test_data$sim_intresect[z] = 0
      }
    } 
```
##שלב שביעי- סיווג:
חילוץ המאפיינים עבור נתוני האימון ונתוני המבחן:
 ```{r}
 train_data_features <- select(train_data, sim_query_title, sim_query_description,simlv_query_title,simlv_query_description,simq_query_title,simq_query_description,simjac_query_title,simjac_query_description,sim_intresect,median_relevance)
    test_data_features <- select(test_data, sim_query_title, sim_query_description, simlv_query_title,simlv_query_description,simq_query_title,simq_query_description,simjac_query_title,simjac_query_description,sim_intresect)
```
סיווג עם מודל j48:
 ```{r}
 train_data_features$median_relevance<-as.factor(train_data_features$median_relevance)
    fit <- J48(median_relevance~., data=train_data_features)
    summary(fit)
    predictions <- predict(fit, test_data_features)
    submit_data <- read.csv("test.csv", header=TRUE)
    submit_data <- select(submit_data,id)
    submit_data["prediction"] <- predictions
    write.csv(submit_data, file = "Submission.csv")
```
סיווג עם מודל SVM:
 ```{r}
 train_data_features$median_relevance<-as.factor(train_data_features$median_relevance)
    model <- svm(median_relevance ~ ., data = train_data_features)
    pred <- predict(model, test_data_features)
    submit_data <- read.csv("test.csv", header=TRUE)
    submit_data<- select(submit_data,id)
    submit_data["prediction"] <- pred
    write.csv(submit_data, file = "Submission.csv")
```
סיווג עם מודל רשת נוירונים:
```{r}
 train_data_features$median_relevance<-as.factor(train_data_features$median_relevance)
    model<- nnet(median_relevance~., train_data_features,size=50,type="class")
    pred <- predict(model, test_data_features)
    predict <- colnames(pred)[apply(pred,1,which.max)]
    submit_data <- read.csv("test.csv", header=TRUE)
    submit_data<- select(submit_data,id)
    submit_data["prediction"] <- predict
    write.csv(submit_data, file = "Submission.csv")
```
סיווג עם מודל RANDOM FOREST:
```{r}
 train_data_features$median_relevance<-as.factor(train_data_features$median_relevance)
    fit <- randomForest(median_relevance~., data=train_data_features, importance=TRUE, ntree=2000)
    summary(fit)
    predictions <- predict(fit, test_data_features)
    submit_data <- read.csv("test.csv", header=TRUE)
    submit_data <- select(submit_data,id)
    submit_data["prediction"] <- predictions
    write.csv(submit_data, file = "Submission1.csv")
```

##הגשות שבוצעו:

[Submission1](https://github.com/rot2304/Ex4/blob/master/Submission1.zip)
