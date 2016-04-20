#### READ DATA INTO R #######################
    before_co <- read.csv("data/uncommongoods/before_co.csv",stringsAsFactors = T)
    before_ugids <- read.csv("data/uncommongoods/before_co_ugids.csv",stringsAsFactors = T)
    before_wl <- read.csv("data/uncommongoods/before_wl.csv",stringsAsFactors = T)
    before_wl_ugids <- read.csv("data/uncommongoods/before_wl_ugids.csv",stringsAsFactors = T)
    
    after_co <- read.csv("data/uncommongoods/after_co.csv",stringsAsFactors = T)
    after_ugids <- read.csv("data/uncommongoods/after_co_ugids.csv",stringsAsFactors = T)
    after_wl <- read.csv("data/uncommongoods/after_wl.csv",stringsAsFactors = T)
    after_wl_ugids <- read.csv("data/uncommongoods/after_wl_ugids.csv",stringsAsFactors = T)
    
    test_co <- read.csv("data/uncommongoods/test_co.csv",stringsAsFactors = T)
    test_ugids <- read.csv("data/uncommongoods/test_co_ugids.csv",stringsAsFactors = T)
    test_wl <- read.csv("data/uncommongoods/test_wl.csv",stringsAsFactors = T)
    test_wl_ugids <- read.csv("data/uncommongoods/test_wl_ugids.csv",stringsAsFactors = T)

#### EDA ##############################
    # checn NAs.it turns out there no missing value. 
    sum(is.na(before_co));sum(is.na(before_wl));sum(is.na(before_ugids));sum(is.na(before_wl_ugids))
    sum(is.na(test_co));sum(is.na(test_wl));sum(is.na(test_ugids));sum(is.na(test_wl_ugids))
    sum(is.na(after_co));sum(is.na(after_wl));sum(is.na(after_ugids));sum(is.na(after_wl_ugids))
    
    # work on check out data
        #login? it turns out social login actually didn't encourage user to create login in account. 
        prop.table(table(before_co$reg_success))
        prop.table(table(test_co$reg_success))
        prop.table(table(after_co$reg_success))
        
        # FACEBOOK VS GOOGLE, FB has more users login creation. 
        prop.table(table(test_co$themodule))
        
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
    
    