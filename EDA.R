#### READ DATA INTO R #######################
    before_co <- read.csv("data/uncommongoods/before_co.csv",stringsAsFactors = T)
    before_ugids <- read.csv("data/uncommongoods/before_co_ugids.csv",stringsAsFactors = T)
    before_wl <- read.csv("data/uncommongoods/before_wl.csv",stringsAsFactors = F)
    before_wl_ugids <- read.csv("data/uncommongoods/before_wl_ugids.csv",stringsAsFactors = T)
    
    after_co <- read.csv("data/uncommongoods/after_co.csv",stringsAsFactors = T)
    after_ugids <- read.csv("data/uncommongoods/after_co_ugids.csv",stringsAsFactors = T)
    after_wl <- read.csv("data/uncommongoods/after_wl.csv",stringsAsFactors = F)
    after_wl_ugids <- read.csv("data/uncommongoods/after_wl_ugids.csv",stringsAsFactors = T)
    
    test_co <- read.csv("data/uncommongoods/test_co.csv",stringsAsFactors = T)
    test_ugids <- read.csv("data/uncommongoods/test_co_ugids.csv",stringsAsFactors = T)
    test_wl <- read.csv("data/uncommongoods/test_wl.csv",stringsAsFactors = F)
    test_wl_ugids <- read.csv("data/uncommongoods/test_wl_ugids.csv",stringsAsFactors = T)

#### EDA ##############################
    # checn NAs.it turns out there no missing value. 
    sum(is.na(before_co));sum(is.na(before_wl));sum(is.na(before_ugids));sum(is.na(before_wl_ugids))
    sum(is.na(test_co));sum(is.na(test_wl));sum(is.na(test_ugids));sum(is.na(test_wl_ugids))
    sum(is.na(after_co));sum(is.na(after_wl));sum(is.na(after_ugids));sum(is.na(after_wl_ugids))
    
    # work on check out data
        #login? it turns out social login actually didn't encourage user to create login in account, loing rate decrease.  
        prop.table(table(before_co$reg_success))
        prop.table(table(test_co$reg_success))
        prop.table(table(after_co$reg_success))
        
        # FACEBOOK VS GOOGLE, FB has more users login creation. 1040 create new account, from which 191 use social. 
        prop.table(table(test_co$themodule))
        table(test_co$themodule)
        table(test_co$reg_success)
        
        # eventcount
        prop.table(table(before_co$eventcount))
        prop.table(table(test_co$eventcount))
        prop.table(table(after_co$eventcount))
        
        library(dplyr)
        a1 <- group_by(before_co, thedevice)
        a2 <- group_by(test_co, thedevice)
        a3 <- group_by(after_co, thedevice)
        avg1 <- summarize(a1, avg = mean(eventcount))
        avg2 <- summarize(a2, avg = mean(eventcount))
        avg3 <- summarize(a3, avg = mean(eventcount))
        
        # devices
        
        
        # unique identifier for site visitors
        length(unique(before_co$theugid));dim(before_co)
        
        
        #b <- unique(before_co$theugid)
        #for (i in b){
        #    before_co[before_co$theugid==i,]$times <- dim(before_co[before_co$theugid==i,])[1]
        #}
        
    # work on wish list data
        # research on the login_or_creation values. 
        prop.table(table(before_wl$reg_success))
        prop.table(table(test_wl$reg_success))
        prop.table(table(after_wl$reg_success))
        
        # FB VS GOOGLE. FB looks better than google. 
        table(test_wl$themodule)
    
    
        # eventcount avg
        a1 <- group_by(before_wl, thedevice)
        a2 <- group_by(test_wl, thedevice)
        a3 <- group_by(after_wl, thedevice)
        avg1 <- summarize(a1, avg = mean(eventcount))
        avg2 <- summarize(a2, avg = mean(eventcount))
        avg3 <- summarize(a3, avg = mean(eventcount))
    
        table(before_wl$eventcount)
       
        # user by session
        length(unique(before_wl$eventcount))
        length(unique(before_wl$isloggedin_r))
        length(unique(before_wl$login_or_create))
        
        a <- before_wl[, c(2,6,8)]
        b <- grep("C", a$login_or_create)
        d <- grep("L", a$login_or_create)
        dim(a[b,]) # 877 try to creat a user
        dim(a[d,]) # 812 try to login in. 
        a$isloggedin_r <- gsub("user by-session", "no", a$isloggedin_r)
        c <- grep("user", a$isloggedin_r)
        dim(a[c,]) #767, sucessfully login in 
        
        a1 <- test_wl[,c(2,6,8)]
        b1 <- grep("C", a1$login_or_create)
        d1 <- grep("L", a1$login_or_create)
        dim(a1[b1,]) # 832 try to creat a user
        dim(a1[d1,]) # 761 try to login in. 
        a1$isloggedin_r <- gsub("user by-session", "no", a1$isloggedin_r)
        c1 <- grep("user", a1$isloggedin_r)
        dim(a1[c1,]) #740, sucessfully login in 
        
        a2 <- after_wl[,c(2,6,8)]
        b2 <- grep("C", a2$login_or_create)
        d2 <- grep("L", a2$login_or_create)
        dim(a2[b2,]) # 117 try to creat a user
        dim(a2[d2,]) # 212 try to login in. 
        a2$isloggedin_r <- gsub("user by-session", "no", a2$isloggedin_r)
        c2 <- grep("user", a2$isloggedin_r)
        dim(a2[c2,]) #176, sucessfully login in 
        
        
        # how to identify a user sucessfuly create a new account. 
        # C,N,N;

        s <- a[c,]
        s$new <- sub("user","login",s$isloggedin_r)
        position <- rep(0,dim(s)[1])
        for (i in 1:dim(s)[1]){
        s1 <-unlist(strsplit(s$new[i], split = ","))
        position[i] <- which(s1=="login")-1
        }
        
        c_l <- rep(NA, dim(s)[1])
        for (i in 1:dim(s)[1]){
            s1 <-unlist(strsplit(s$login_or_create[i], split = ","))
            c_l[i] <- s1[position[i]]
        }
        table(c_l)
        
        
        # DATA VISUALIZATION
        