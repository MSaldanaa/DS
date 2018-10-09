library(data.table)



cal_hotel_rfm <- function(no_visits, last_visit,earnings) {
  
  mean_freq = quantile(hotel_seg$no_visits)
  mean_recency = quantile(hotel_seg$last_visit,type = 1)
  mean_monetary = quantile(hotel_seg$hotel_earnings)
  
  
  if(last_visit <= mean_recency[2]) {
    rfm=1*100
  }
  else if(last_visit <= mean_freq[3]) {
    rfm=2*100
  }
  else {
    rfm=3*100   
  }
  
  
  if(no_visits <= mean_freq[2]) {
    rfm=rfm+10
  }
  else if(no_visits <= mean_freq[3]) {
    rfm=rfm+20
  }
  else {
    rfm=rfm+30 
  }
  
  
  
  if(earnings <= mean_monetary[2]) {
    rfm=rfm+1
  }
  else if(earnings <= mean_monetary[3]) {
    rfm=rfm+2
  }
  else {
    rfm=rfm+3   
  }
  
  return(rfm)
}



setwd ("C:/RajeshOfficial/Technical/GreatLakes/CRM/MGM")
getwd()
hotel_tbl=fread("MGM Hotel Clean.csv",header = T,sep = ',')
mgm_tbl=fread("MGM Grand Clean.csv",header = T,sep = ',')

summary(hotel_tbl)
str(hotel_tbl)

summary(mgm_tbl)

str(mgm_tbl)

hotel_stay = data.table(hotel_tbl[!is.na(hotel_tbl$PLAYER_ID)])

mgm_casino = data.table(mgm_tbl[!is.na(mgm_tbl$PLAYER_ID)])



mgm_casino$netearnings = mgm_casino$TOTALTHEO - mgm_casino$TOTALCOMP

mgm_casino$MAXDATE =  as.Date(mgm_casino$MAXDATE, format = "%d-%b-%Y")

hotel_stay$roomchr = hotel_stay$ROOMDEBIT - hotel_stay$ROOMCREDIT
hotel_stay$fnbchr = hotel_stay$FNBDEBIT - hotel_stay$FNBCREDIT
hotel_stay$enter  = hotel_stay$ENTERTNDEBIT - hotel_stay$ENTERTNCREDIT
hotel_stay$others = hotel_stay$OTHERDEBIT - hotel_stay$OTHERCREDIT
hotel_stay$retail = hotel_stay$RETLDEBIT - hotel_stay$RETLCREDIT

hotel_stay$roomchr_er = hotel_stay$roomchr * 0.20
hotel_stay$fnbchr_er = hotel_stay$fnbchr * 0.28
hotel_stay$enter_er  = hotel_stay$enter * 0.50
hotel_stay$others_er = hotel_stay$others * 0.20
hotel_stay$retail_er = hotel_stay$retail  * 0.25

hotel_stay$FOL_ACT_DEP_DATE =  as.Date(hotel_stay$FOL_ACT_DEP_DATE, format = "%d-%b-%Y")


## Charges from hotal stay
hotel_stay$total_er = hotel_stay$roomchr_er + hotel_stay$fnbchr_er + hotel_stay$enter_er +
                        hotel_stay$others_er + hotel_stay$retail_er



hotel_seg = hotel_stay[, list(no_visits=.N,last_visit=min(FOL_ACT_DEP_DATE),
                              hotel_earnings=sum(total_er)),
                       by = PLAYER_ID]

rfm = hotel_seg[, list(RFM=cal_hotel_rfm(no_visits,last_visit,hotel_earnings)),
           by = PLAYER_ID]

hotel_seg = merge(hotel_seg, rfm, by = "PLAYER_ID")

mgm_seg = mgm_casino[, list(no_visits=.N,last_visit=min(MAXDATE),casino_earnings=sum(netearnings), casino_theo=sum(TOTALTHEO)), by=PLAYER_ID]

cust_seg = data.table(merge(hotel_seg, mgm_seg, by="PLAYER_ID", all = TRUE))


cust_seg[is.na(cust_seg)] = 0

cust_seg$total_er = cust_seg$hotel_earnings + cust_seg$casino_earning

sort(cust_seg$total_er)

