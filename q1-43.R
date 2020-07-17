
library(tidyverse)

if(F){
  df_customer <- dbGetQuery(con,"SELECT * FROM customer")
  df_category <- dbGetQuery(con,"SELECT * FROM category")
  df_product <- dbGetQuery(con,"SELECT * FROM product")
  df_receipt <- dbGetQuery(con,"SELECT * FROM receipt")
  df_store <- dbGetQuery(con,"SELECT * FROM store")
  df_geocode <- dbGetQuery(con,"SELECT * FROM geocode")
}

# No.1 --------------------------------------------------------------------

 df_receipt <- read_csv("receipt.csv")
 head(df_receipt,n=10)

# No2 ---------------------------------------------------------------------

 colnames(df_receipt)
 head(df_receipt[,c("sales_ymd","customer_id","product_cd","amount")],n=10)

 #No.3 
 
 df_receipt %>% select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   rename(.,sales_date=sales_ymd) %>% 
   head(.,n=10)

 #No.4
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   filter(.,customer_id == "CS018205000001")

 #No.5
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   filter(.,customer_id == "CS018205000001" ,
            amount >= 1000)
 
 #No.6
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,quantity,amount) %>% 
   filter(.,customer_id == "CS018205000001" &
            (amount >= 1000 |
               quantity >= 5))
 
 #No.7
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   filter(.,customer_id == "CS018205000001" &
            (amount >= 1000 &
             amount <= 2000))
 
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   filter(.,customer_id == "CS018205000001" &
          between(amount,1000,2000))

 #No.8
 df_receipt %>% 
   select(.,sales_ymd,customer_id,product_cd,amount) %>% 
   filter(.,customer_id == "CS018205000001" &
            product_cd != "P071401019")
 
 #No.9
 df_store <- read_csv("store.csv")
 
 df_store %>%
   filter(!(prefecture_cd == "13" | floor_area > 900))
 
 df_store %>%
   filter((prefecture_cd != "13" & floor_area <= 900))
 
 #No.10
 head(df_store %>% filter(., grepl("^S14",store_cd)),n=10)
 
 #No.11
 df_customer <- read_csv("customer.csv")
 head(df_customer %>% 
        filter(., grepl("1$",customer_id)),n=10)

 #No.12
 df_store %>% 
        filter(., grepl("横浜市",address))
 
 # No.13
 df_customer %>% 
    filter(., grepl("^[A-F]",status_cd)) %>% 
    head(n=10)

 # No.14
 df_customer %>% 
    filter(., grepl("[1-9]$",status_cd)) %>% 
    head(n=10)
 
 # No.15
 df_customer %>% 
    filter(., grepl("^[A-F].*[1-9]$",status_cd)) %>% 
    head(n=10)
 
 # No.16
 df_store %>% 
    filter(., grepl("[0-9]{3}-[0-9]{3}-[0-9]{4}",tel_no)) 

 # No.17
 df_customer %>% 
    arrange(birth_day) %>% 
    head(n=10)

 # No.18
 df_customer %>% 
    arrange(desc(birth_day)) %>% 
    head(n=10)
 
 # No.19
 df_receipt %>% 
    mutate(rank=min_rank(desc(amount))) %>% 
    arrange(rank) %>% 
    select(customer_id,amount,rank) %>% 
    head(n=10) 
 
 # No.20
 df_receipt %>% 
    mutate(rank=row_number(desc(amount))) %>% 
    arrange(rank) %>% 
    select(customer_id,amount,rank) %>% 
    head(n=10) 

 # No.21
 df_receipt %>% 
    mutate(rank=row_number(desc(amount))) %>% 
    arrange(rank) %>% 
    select(customer_id,amount,rank) %>% 
    head(n=10) 
 
 # No.21
 nrow(df_receipt)
 df_receipt %>%  count()

 # No.22
 df_receipt %>%  group_by(customer_id) %>% 
    count() %>% nrow()
 
 # No.23
 df_receipt %>%  group_by(store_cd) %>% 
    summarise(amount = sum(amount),
              quantity = sum(quantity))
 
 # No.24
 df_receipt %>%  group_by(customer_id) %>% 
    summarise(max=max(sales_ymd)) %>% 
    slice(1:10)
 
 
 # No.25
 df_receipt %>%  group_by(customer_id) %>% 
    summarise(max=min(sales_ymd)) %>% 
    slice(1:10)
 
 # No.26
 df_receipt %>%  group_by(customer_id) %>% 
    summarise(max=max(sales_ymd),
              min=min(sales_ymd)) %>% 
    filter(max!=min) %>% 
    slice(1:10)

 # No.27
 df_receipt %>%  group_by(store_cd) %>% 
    summarise(mean=mean(amount)) %>% 
    arrange(desc(mean)) %>% 
    slice(1:5)
 
 # No.28
 df_receipt %>%  group_by(store_cd) %>% 
    summarise(med=median(amount)) %>% 
    arrange(desc(med)) %>% 
    slice(1:5)

 # No.29
 df<-data.frame(matrix(NA,length(unique(df_receipt$store_cd)),2,
                       dimnames = list(c(),c("cusid","max")))) 
 
 for (i in 1:nrow(df)){
   tmp<-df_receipt[df_receipt$store_cd==unique(df_receipt$store_cd)[i],c("product_cd")]
   df[i,1]<-unique(df_receipt$store_cd)[i]
   df[i,2]<-names(table(tmp)[which.max(table(tmp))])
 }
 
 arrange(df,cusid)
  
 # No.30
 var_sample<-function(x){
   var(x)*(length(x)-1)/length(x)
 }
 
 df_receipt %>% 
   group_by(.,store_cd) %>% 
   summarise(sd = var_sample(amount)) %>% 
   arrange(desc(sd)) %>% 
   slice(1:5)
 
 # No.31
 df_receipt %>% 
   group_by(.,store_cd) %>% 
   summarise(sd = sqrt(var_sample(amount))) %>% 
   arrange(desc(sd)) %>% 
   slice(1:5)
 
 # No.32
 quantile(df_receipt$amount)[-1]
 

 # No.33
 df_receipt %>% 
   group_by(.,store_cd) %>% 
   summarise(mean = mean(amount)) %>% 
   filter(mean>=330)

 # No.34
 tmp<-df_receipt %>% 
   filter(!grepl("^Z",customer_id,)) %>% 
   group_by(.,customer_id) %>% 
   summarise(sum = sum(amount))
 mean(tmp$sum)

 # No.35
 df_receipt %>% 
   filter(!grepl("^Z",customer_id)) %>% 
   group_by(.,customer_id) %>% 
   summarise(sum = sum(amount)) %>% 
   filter(sum>=mean(sum)) %>% 
   slice(1:10)
 
 # No.36
 colnames(df_receipt)
 colnames(df_store)
 df_receipt %>% 
   inner_join(.,df_store[,c("store_cd","store_name")],by="store_cd") %>% 
   slice(1:10) 

 # No.37
 df_category <- read_csv("category.csv")
 df_product <- read_csv("product.csv")
 colnames(df_product)
 colnames(df_category)
 
 df_product %>% 
   inner_join(.,df_category[,c("category_small_cd","category_small_name")],by="category_small_cd") %>% 
   slice(1:10) 
 
 # No.38
 tmp<-df_receipt %>% 
   group_by(customer_id) %>% 
   summarise(sum_amount=sum(amount)) 
 
 df_customer %>% left_join(.,tmp,by= 'customer_id') %>% 
   filter(!grepl("^Z",customer_id)) %>%
   filter(gender_cd == 1) %>% 
   replace_na(list(sum_amount=0)) %>% 
   select(.,customer_id,sum_amount) %>% 
      slice(1:10)
 
 # No.39
 ndate <-df_receipt %>% 
   filter(!grepl("^Z",customer_id)) %>% 
   group_by(customer_id) %>% 
   distinct(sales_ymd) %>% 
   summarise(ndate=n_distinct(sales_ymd)) %>% 
   arrange(desc(ndate)) %>% 
   slice(1:20)

 namount <- df_receipt %>% 
   filter(!grepl("^Z",customer_id)) %>% 
   group_by(customer_id) %>% 
   summarise(namount=sum(amount)) %>% 
   arrange(desc(namount), customer_id)%>% 
   slice(1:20)
  
 full_join(ndate,namount,by="customer_id") %>% 
   arrange(desc(namount))
             
 # No.40
 df_product <-read_csv("product.csv")
 nrow(df_store)*nrow(df_product)

 df_store_tmp <- df_store
 df_product_tmp <- df_product
 df_store_tmp['key'] <- 0
 df_product_tmp['key'] <- 0
 nrow(full_join(df_store_tmp, df_product_tmp, by = "key"))
 
 
 tbl_1 <- tibble(val_1 = 1:5)
 tbl_2 <- tibble(val_2 = 10:15)
 
 tbl_1$fake <- 1
 tbl_2$fake <- 1
 my_cross_join <- full_join(tbl_1, tbl_2, by = "fake") %>%
   select(-fake)
 
 # No.41,42
 colnames(df_receipt)
 nr<-nrow(df_receipt)
 tmp<- df_receipt %>% 
   group_by(sales_ymd) %>% 
   summarise(sum=sum(amount)) %>% 
   mutate(d1=row_number(),
          d2=row_number()-1,
          d3=row_number()-2,
          d4=row_number()-3) 
 
 tmp %>% 
   inner_join(tmp[,c("d2","sum")],by=c("d1" = "d2"),copy=F) %>% 
   rename(diff1=sum.y) %>% 
   inner_join(tmp[,c("d3","sum")],by=c("d1" = "d3")) %>% 
   rename(diff2=sum) %>% 
   inner_join(tmp[,c("d4","sum")],by=c("d1" = "d4")) %>% 
   rename(diff3=sum) %>% 
   select(-d1,-d2,-d3,-d4) 

 # No.43
 df_sales_summary <- df_receipt %>% 
   inner_join(df_customer,by="customer_id") %>% 

 colnames(df_sales_summary) 
 df_sales_summary %>% mutate(era=trunc(age/10)*10) %>% 
    group_by(gender_cd,era) %>% 
    summarise(sum=sum(amount)) %>% 
    tidyr::spread(key = gender_cd, value = sum,fill=0) %>% 
    rename(male='0', female='1', unknown='9')

 
 
 
  
 
 