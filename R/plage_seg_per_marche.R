# plage_seg_per_marche <- function(tab_ad,data,s_d){
#   plage <- round(seq(-20,45,s_d),digits = 1)
#   tab_ad <- as.data.frame(tab_ad)
#   data2 <- tab_ad[tab_ad$signe<1,]
#   data2 <- data2[data2$conclusion != "RAS",]
#   nom <- data2$seg_period
#   prob_per <- c()
#   for (l in 1:nrow(data2)){
#     if (data2$seg_demar[l]==1){
#       prob_per[[l]] <- p1(l,data2,data,s_d,nom)
#     }
#   }
#   t <- c()
#   for(a in 1:length(prob_per)){
#     if(is.data.frame(prob_per[[a]])){
#       t <- rbind(t,prob_per[[a]])
#     }
#   }
#   return(t)
# }
#
# p1 <- function(l,data2,data,s_d,nom){
#     t_extl <- data$ext_smooth[data2$indice_s[l]:data2$indice_e[l]]
#     t_ambl <- data$data_smooth[data2$indice_s[l]:data2$indice_e[l]]
#     ecartl <- data$Ecard[data2$indice_s[l]:data2$indice_e[l]]
#     ind <- rep(NA,length(t_extl))
#     for (k in 1:length(t_extl)){
#       if(s_d>0.1){
#         ind[k] <- which((round(plage-t_extl[k],digits = 1)<=(s_d-0.1)) & (round(plage-t_extl[k],digits = 1)>=0))}
#       else{
#         ind[k] <- which((round(plage-t_extl[k],digits = 2)<=(s_d-0.01)) & (round(plage-t_extl[k],digits = 2)>=0))}
#     }
#     dup <- unique(ind[duplicated(ind)])
#
#     if (length(dup)==0){
#       segm <- mod[l]
#       plage_t_ext <- NA
#       start <- 1
#       end <- length(t_ext)
#       deltaT <- data2$coef_seg[data2$seg_period == nom[l]]
#       result <- data.frame(segm,plage_t_ext,start,end,deltaT)
#     }
#
#     # table
#     else{
#     list_plage <- c()
#     for (p in 1:length(dup)){
#       list_plage[[p]] <- which(ind==dup[p])
#     }
#
#     dif_test <- sapply(list_plage,diff,simplify = FALSE)
#     tab <- c()
#
#     for (m in 1:length(dif_test)){
#
#       #table
#       res <- f1(dif_test,m)
#       dif <- res$dif
#       breakpoint <- res$breakpoint
#       start <- res$start
#       end <- res$end
#       table <- data.frame(start,end)
#
#       #clean table + update inds
#       if (length(which(start>=end))>0){
#         table <- table[-which(start>=end),]}
#       if(nrow(table)>0){
#         table$start <- list_plage[[m]][table$start]
#         table$end <- list_plage[[m]][table$end]
#
#         #rebind table
#         plage_t_ext <- rep(dup[m],nrow(table))
#         segm <- rep(nom[l],nrow(table))
#         deltaT <- rep(data2$coef_seg[data2$seg_period == nom[l]],nrow(table))
#         table <- cbind(segm,plage_t_ext,table,deltaT)
#         tab[[m]]<-table}
#       else{
#         tab[[m]]<-NA}
#     }
#     tab_r <- c()
#     for(p in 1:length(tab)){
#       if(is.data.frame(tab[[p]])){
#         tab_r <- rbind(tab[[p]])
#       }
#     }
#     result <- tab_r}
#   return(result)
# }
#
