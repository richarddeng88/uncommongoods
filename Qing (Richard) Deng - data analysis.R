library(dplyr);library(ggplot2)

#### READ DATA INTO R ######################################################
    before_co <- read.csv("data/uncommongoods/before_co.csv",stringsAsFactors = T)
    before_ugids <- read.csv("data/uncommongoods/before_co_ugids.csv",stringsAsFactors = T)
    before_wl <- read.csv("data/uncommongoods/before_wl.csv",stringsAsFactors = F)
    before_wl_ugids <- read.csv("data/uncommongoods/before_wl_ugids.csv",stringsAsFactors = T)
    
    after_co <- read.csv("data/uncommongoods/after_co.csv",stringsAsFactors = T)
    after_ugids <- read.csv("data/uncommongoods/after_co_ugids.csv",stringsAsFactors = T)
    after_wl <- read.csv("data/uncommongoods/after_wl.csv",stringsAsFactors = F)
    after_wl_ugids <- read.csv("data/uncommongoods/after_wl_ugids.csv",stringsAsFactors = T)
    
    test_co <- read.csv("data/uncommongoods/test_co.csv",stringsAsFactors = F)
    test_ugids <- read.csv("data/uncommongoods/test_co_ugids.csv",stringsAsFactors = T)
    test_wl <- read.csv("data/uncommongoods/test_wl.csv",stringsAsFactors = F)
    test_wl_ugids <- read.csv("data/uncommongoods/test_wl_ugids.csv",stringsAsFactors = T)

##### EDA ################################################################
    # checK NAs.it turns out there no missing value. 
    sum(is.na(before_co));sum(is.na(before_wl));sum(is.na(before_ugids));sum(is.na(before_wl_ugids))
    sum(is.na(test_co));sum(is.na(test_wl));sum(is.na(test_ugids));sum(is.na(test_wl_ugids))
    sum(is.na(after_co));sum(is.na(after_wl));sum(is.na(after_ugids));sum(is.na(after_wl_ugids))

    ######### work on CO data###################################################
        # have a general idea of the rate of sucessful creation
        # it turns out social login actually didn't encourage user to create login in account,the reate decrease.
        prop.table(table(before_co$reg_success))
        prop.table(table(test_co$reg_success))
        prop.table(table(after_co$reg_success))

        # FACEBOOK VS GOOGLE, it looks that FB has more users click. 
        prop.table(table(test_co$themodule))
        table(test_co$themodule)   # F 124, G67
        table(test_co$reg_success) # 1040 new account

        # see how many avg eventcount by device during CO. 
        a1 <- group_by(before_co, thedevice)
        a2 <- group_by(test_co, thedevice)
        a3 <- group_by(after_co, thedevice)
        summarize(a1, avg = mean(eventcount))
        summarize(a2, avg = mean(eventcount))
        summarize(a3, avg = mean(eventcount))

        # devices analysis - we find that across all device, the creation percent decrease.
        dev_before_co <- before_co %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))
        dev_test_co <- test_co %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))
        dev_after_co <- after_co %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))

        d1 <- data.frame(dev_before_co, time="before", type="CO")
        d2 <- data.frame(dev_test_co, time="test", type="CO")
        d3 <- data.frame(dev_after_co, time="after", type="CO")
        dev_co <- rbind(d1,d2,d3) # for later visualiztion. 


    #### work on wish list data ################################################

        # FB VS GOOGLE. FB looks better than google. 
        table(test_wl$themodule)


        # see how many avg eventcount by device during WL. 
        a1 <- group_by(before_wl, thedevice)
        a2 <- group_by(test_wl, thedevice)
        a3 <- group_by(after_wl, thedevice)
        avg1 <- summarize(a1, avg = mean(eventcount))
        avg2 <- summarize(a2, avg = mean(eventcount))
        avg3 <- summarize(a3, avg = mean(eventcount))

        # user by session, have a general idea of uniqueness. 
        length(unique(before_wl$eventcount))
        length(unique(before_wl$isloggedin_r))
        length(unique(before_wl$login_or_create))

        # we don't know the new creation rate. so here we try to find out how many user sucessfuly login 
        # either by C or BY L, creating a new variable "reg_sucess"
        before_wl$isloggedin_r <- gsub("user by-session", "no", before_wl$isloggedin_r) #replace "suer by-seesion" to "no" 
        before_success <- grep("user", before_wl$isloggedin_r) # replace the first "user" to login. 
        dim(before_wl[before_success,]) #total there are 767 sucessfully login. 

            # we don't know in these successful login, how many are from creation, and how many are from existing account. 
            # to I try to defferentiate C and L, by identifying if it is "C" OR "L" before the first "user" position in varialbe isloggedin_r.  
                # identify the position
                s <- before_wl[before_success,]
                s$new <- sub("user","login",s$isloggedin_r)
                position <- rep(0,dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <-unlist(strsplit(s$new[i], split = ","))
                    position[i] <- which(s1=="login")-1
                }
                
                # identify if C or L
                c_l <- rep(NA, dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <-unlist(strsplit(s$login_or_create[i], split = ","))
                    c_l[i] <- s1[position[i]]
                }
                table(c_l) # 508 are C, 259 are L, totally 767 logedin users. 

        # add a new variable named "reg_success: 1 if user created an account, else 0"
        before_wl$user_login <- "no"
        before_wl[before_success,]$user_login <- c_l
        before_wl$reg_success <- 0
        before_wl$reg_success[before_wl$user_login=="C"] <- 1
        prop.table(table(before_wl$reg_success))


        # I repeated the same process on test_wl data
                test_wl$isloggedin_r <- gsub("user by-session", "no", test_wl$isloggedin_r)
                test_success <- grep("user", test_wl$isloggedin_r)
                dim(test_wl[test_success,]) #740, sucessfully login in 
                
                ##test 
                s <- test_wl[test_success,]
                s$new <- sub("user","login",s$isloggedin_r)
                position <- rep(0,dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <-unlist(strsplit(s$new[i], split = ","))
                    position[i] <- which(s1=="login")-1
                }
                
                c_l <- rep(NA, dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <- unlist(strsplit(s$login_or_create[i], split = ","))
                    c_l[i] <- s1[position[i]]
                }
                table(c_l) # 527 are C, 213 are L, totally 740
                
                # add a new variable named "reg_success: 1 if user created an account, else 0"
                test_wl$user_login <- "no"
                test_wl[test_success,]$user_login <- c_l
                test_wl$reg_success <- 0
                test_wl$reg_success[test_wl$user_login=="C"] <- 1
                prop.table(table(test_wl$reg_success))

        # repeated again on after_wl data
                after_wl$isloggedin_r <- gsub("user by-session", "no", after_wl$isloggedin_r)
                after_success <- grep("user", after_wl$isloggedin_r)
                dim(after_wl[after_success,]) #176, sucessfully login in 
                
                # after
                s <- after_wl[after_success,]
                s$new <- sub("user","login",s$isloggedin_r)
                position <- rep(0,dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <-unlist(strsplit(s$new[i], split = ","))
                    position[i] <- which(s1=="login")-1
                }
                
                c_l <- rep(NA, dim(s)[1])
                for (i in 1:dim(s)[1]){
                    s1 <- unlist(strsplit(s$login_or_create[i], split = ","))
                    c_l[i] <- s1[position[i]]
                }
                table(c_l) # 527 are C, 213 are L, totally 740
                
                # add a new variable named "reg_success: 1 if user created an account, else 0"
                after_wl$user_login <- "no"
                after_wl[after_success,]$user_login <- c_l
                after_wl$reg_success <- 0
                after_wl$reg_success[after_wl$user_login=="C"] <- 1
                prop.table(table(after_wl$reg_success))

        # device analysis: 
        dev_before_wl <- before_wl %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))
        dev_test_wl <- test_wl %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))
        dev_after_wl <- after_wl %>% group_by(thedevice) %>% summarize(ave=mean(reg_success))

        d1 <- data.frame(dev_before_wl, time="before", type="WL")
        d2 <- data.frame(dev_test_wl, time="test", type="WL")
        d3 <- data.frame(dev_after_wl, time="after", type="WL")
        dev_wl <- rbind(d1,d2,d3)

##### DATA VISUALIZATION ##############################################

        # Q1: impact on the creation of new users - WL and CO , bar chart
        percent <- c(mean(before_wl$reg_success),mean(test_wl$reg_success),mean(after_wl$reg_success),
                     mean(before_co$reg_success),mean(test_co$reg_success),mean(after_co$reg_success))
        
        impact <- data.frame(percent = percent, time=c("1.before","2.test","3.after","1.before","2.test","3.after"),
                             type= c("WL","WL","WL","CO","CO","CO"))

        ggplot(impact, aes(x=time,y=percent,fill=time)) + geom_bar(stat="identity",position="dodge", colour="black")+
            facet_grid(.~type)+scale_fill_manual(values=c("#669933", "#FFCC66","#669933"))+
            xlab("Time Period") + ylab("Sucessful Account Creation Percent")


        #Q2: device analysis : desktop vs mobile vs tablet. 
        dev <- rbind(dev_co,dev_wl); dev <- dev[dev$thedevice!="NULL",]
        ggplot(dev, aes(x=reorder(thedevice,ave),y=ave,fill=time)) + geom_bar(stat="identity",position="dodge", colour="black")+
            facet_grid(.~type)+scale_fill_manual(values=c("#669933", "#FFCC66","#669933"))+
            xlab("Device Type") + ylab("Sucessful Account Creation Percent")

        
        # Q3: FB VS GOOGLE, PIE chart
        
        table(test_co$themodule);table(test_co$reg_success)
        table(test_wl$themodule);table(test_wl$reg_success)
        
        # an first glance : the click caomparison of FB vs GOOGLE, 
        #but many of them are unsucessfull creating a new account. 
        f1 <- test_co[test_co$themodule!=0,]$themodule
        f2 <- test_wl[test_wl$themodule!=0,]$themodule

                # creating pie chart for the first click on FB OR GOOGLE
        
                # CO 
                slices <- c(table(f1)[1],table(f1)[2])
                lbls <- c("FB", "GOOGLE")
                lbls <- paste(lbls, slices) 
                pie(slices, labels = lbls, col=c("blue","lightgreen"),main="CO - Pie Chart of clikc")
                # WL
                slices <- c(table(f2)[1],table(f2)[2])
                lbls <- c("FB", "GOOGLE")
                lbls <- paste(lbls, slices) 
                pie(slices, labels = lbls, col=c("blue","lightgreen"),main="WL - Pie Chart of clikc")
                
         #with in these successful account creation, there is a different story. 
                f1 <- test_co[test_co$reg_success==1,]$themodule
                f2 <- test_wl[test_wl$reg_success==1,]$themodule
                f1[f1=="0"] <- "email"; 
                f2[f2=="0"] <- "email"; 
                
                # creating pie chrt
                
                #CO
                slices <- c(table(f1)[1],table(f1)[2],table(f1)[3])
                lbls <- c("email","FB", "GOOGLE")
                lbls <- paste(lbls, slices) 
                pie(slices, labels = lbls, col=rainbow(length(lbls)),main="CO - successful account creation")
                # WL
                slices <- c(table(f2)[1],table(f2)[2],table(f2)[3])
                lbls <- c("email","FB", "GOOGLE")
                lbls <- paste(lbls, slices) 
                pie(slices, labels = lbls, col=rainbow(length(lbls)),main="WL - successful account creation")
                
                
