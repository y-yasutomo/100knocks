
library(tidyverse)


if(F){
  df_customer <- dbGetQuery(con,"SELECT * FROM customer")
  df_category <- dbGetQuery(con,"SELECT * FROM category")
  df_product <- dbGetQuery(con,"SELECT * FROM product")
  df_receipt <- dbGetQuery(con,"SELECT * FROM receipt")
  df_store <- dbGetQuery(con,"SELECT * FROM store")
  df_geocode <- dbGetQuery(con,"SELECT * FROM geocode")
}

df_receipt <- read_csv("receipt.csv")
df_customer <- read_csv("customer.csv")
colnames(df_receipt)
colnames(df_customer)

#43 -------------------------------------------------------------------------

unique(df_customer$gender_cd)

df_customer$generation<-trunc(df_customer$age/10)

tmp<-df_customer %>% inner_join(.,df_receipt,by="customer_id") %>%
  select(.,gender_cd,generation,amount) %>% 
  group_by(generation,gender_cd) %>% 
  summarise(.,sum=sum(amount))

res<-data.frame(matrix(NA,9,4,dimnames = list(c(),c("generation",
                                               "male",
                                               "female",
                                               "mid"))))
res$generation<-unique(tmp$generation)
for(i in 1:length(tmp[tmp$gender_cd==0,]$generation)){
  res$male[tmp[tmp$gender_cd==0,]$generation[i]]<-tmp[tmp$gender_cd==0,]$sum[i]
}
for(i in 1:length(tmp[tmp$gender_cd==1,]$generation)){
  res$female[tmp[tmp$gender_cd==1,]$generation[i]]<-tmp[tmp$gender_cd==1,]$sum[i]
}
for(i in 1:length(tmp[tmp$gender_cd==9,]$generation)){
  res$mid[tmp[tmp$gender_cd==9,]$generation[i]]<-tmp[tmp$gender_cd==9,]$sum[i]
}

res

##ans

df_sales_summary <- df_customer[c("customer_id", "gender_cd", "birth_day" , "age")] %>%
  mutate(era = trunc(age / 10) * 10) %>%
  inner_join(df_receipt, by="customer_id") %>%
  group_by(gender_cd, era) %>%
  summarise(sum_amount=sum(amount)) %>%
  spread(gender_cd, sum_amount, fill=0) %>% 
  rename(male='0', female='1', unknown='9')

# 44-------------------------------------------------------------------------

 df_sales_summary %>% 
  gather(.,key=gender_code,value = sum_amount,-era) %>% 
  mutate(.,gender_code=case_when(gender_code=="male"~"00",
                                 gender_code=="female"~"11",
                                 gender_code=="mid"~"99"))

# 45-------------------------------------------------------------------------
 colnames(df_customer)
 df_customer %>% select(.,birth_day,customer_id) %>% 
   mutate(birth_day = strftime(birth_day,format = "%Y%m%d")) %>% 
   slice(1:10)
  

# 46-------------------------------------------------------------------------

  df_customer$application_date<-strptime(df_customer$application_date, '%Y%m%d')

  df_customer %>% select(.,application_date,customer_id) %>% 
   slice(1:10)

# 47-------------------------------------------------------------------------

  glimpse(df_receipt)
  df_receipt$sales_ymd<-strptime(df_receipt$sales_ymd,
                                 '%Y%m%d')
  df_receipt %>% select(.,sales_ymd,receipt_no,receipt_sub_no) %>% 
    slice(1:10)

# 48-------------------------------------------------------------------------
  
  df_receipt$sales_epoch<-
    as.POSIXct(df_receipt$sales_epoch,origin = "1970-01-01")
  df_receipt %>% select(.,sales_epoch,receipt_no,receipt_sub_no) %>% 
    slice(1:10)
  
# 49-------------------------------------------------------------------------
  
  df_receipt %>% select(.,receipt_no,receipt_sub_no) %>%  
    mutate(date=substr(df_receipt$sales_epoch,1,4)) %>% 
    slice(1:10)
  
# 50-------------------------------------------------------------------------
  
  df_receipt %>% select(.,receipt_no,receipt_sub_no) %>%  
    mutate(date=substr(df_receipt$sales_epoch,6,7)) %>% 
    slice(1:10)

# 51-------------------------------------------------------------------------
  
  df_receipt %>% select(.,receipt_no,receipt_sub_no) %>%  
    mutate(date=substr(df_receipt$sales_epoch,9,10)) %>% 
    slice(1:10)
  
# 52-------------------------------------------------------------------------
  colnames(df_receipt)
  df_receipt %>% 
    select(.,customer_id,amount) %>% 
    group_by(.,customer_id) %>% 
    summarise(.,sum_amount = sum(amount)) %>% 
    mutate(ans = if_else(sum_amount>2000,1,0)) %>% 
    filter(!grepl("^Z",customer_id)) %>% 
    slice(1:10)

# 53-------------------------------------------------------------------------
  

    
    
      