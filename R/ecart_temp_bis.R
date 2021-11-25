# # ecart_temp_part_func
#
#
#
# # chercher les segments----
#
# f1 <- function(dif_test,l){
# dif<- dif_test[[l]]
# breakpoint <- which(dif>1)
# if(length(breakpoint)==1){
#   start <- c(1,breakpoint+1)
#   end <- c(breakpoint,length(dif)+1)
# }
# else if (length(breakpoint)>1){
#   start <- c(1,breakpoint)
#   end <- c(breakpoint-1,length(dif))
#   if(dif[1]==1){
#     start <- c(1,start[-1]+1)
#     end <- end+1}
#   else{
#     start <- start+1
#     end <- end+1
#   }}
# else{
#   start <- 1
#   end <- length(dif)+1
# }
#   return(list(start=start,end=end,dif=dif,breakpoint=breakpoint))
# }
#
# # calculer delta T/s-----
# modeling <- function(table,ecart_secj,t_ambj,n){
#   x <- x_axis_ecart(ecart_secj,table$start[n],table$end[n])
#   y <- t_ambj[table$start[n]:table$end[n]]
#   mod <- lm(y ~ x)
#   if(!is.na(mod$coefficients[2])){
#     val <- mod$coefficients[2]
#   }
#   else{
#     val <- 0
#   }
#   return(val)
# }
#
# # calculate delta T/s for each period------
# deltaT_period <- function(j,temp,t_amb,ecart_sec,plage,rest,s_d){
#
# # initialiser les valeurs
# tempj <- temp[rest$start[j]:rest$end[j]]
# t_ambj <- t_amb[rest$start[j]:rest$end[j]]
# ecart_secj <- ecart_sec[rest$start[j]:rest$end[j]]
# ind <- rep(NA,length(tempj))
# for (k in 1:length(tempj)){
#   if(s_d>0.1){
#   ind[k] <- which((round(plage-tempj[k],digits = 1)<=(s_d-0.1)) & (round(plage-tempj[k],digits = 1)>=0))}
#   else{
#   ind[k] <- which((round(plage-tempj[k],digits = 2)<=(s_d-0.01)) & (round(plage-tempj[k],digits = 2)>=0))}
# }
# dup <- unique(ind[duplicated(ind)])
#
# if (length(dup)==0){
#   result <- NA
# }
#
# # table
# else{
#   list_plage <- c()
#   for (p in 1:length(dup)){
#     list_plage[[p]] <- which(ind==dup[p])
#   }
#
#   dif_test <- sapply(list_plage,diff,simplify = FALSE)
#   tab <- c()
#
#   for (l in 1:length(dif_test)){
#
#     #table
#     res <- f1(dif_test,l)
#     dif <- res$dif
#     breakpoint <- res$breakpoint
#     start <- res$start
#     end <- res$end
#     table <- data.frame(start,end)
#
#     #clean table + update inds
#     if (length(which(start>=end))>0){
#       table <- table[-which(start>=end),]}
#     if(nrow(table)>0){
#     table$start <- list_plage[[l]][table$start]
#     table$end <- list_plage[[l]][table$end]
#
#     # calculate delta T/s
#     dif_temp <- rep(NA,nrow(table))
#     for (n in 1:nrow(table)){
#       dif_temp[n]<-modeling(table,ecart_secj,t_ambj,n)
#     }
#
#     #rebind table
#     plage_t_ext <- rep(dup[l],nrow(table))
#     table <- cbind(plage_t_ext,table,dif_temp)
#     tab[[l]]<-table}
#     else{
#     tab[[l]]<-NA}
#   }
#   tab_res <- c()
#   for(p in 1:length(tab)){
#     if(is.data.frame(tab[[p]])){
#       tab_res <- rbind(tab[[p]])
#     }
#   }
#   result <- tab_res}
#   return(result)
# }
