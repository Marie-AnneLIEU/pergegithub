#'
#' rest.period <- function(list.data,num_chambre){
#'   per <- list.period(list.data,num_chambre)
#'   list_period <- per$list_period
#'   list.data <- per$list.data
#'   period_rest <- c()
#'   for (j in 1:length(list.data)){
#'     data <- list.data[[j]]
#'     period <- list_period[[j]]
#'     if (is.null(dim(period))){
#'       if (length(data$cond[data$cond ==1])>=1){
#'         ind <- which(data$cond ==1)
#'         start <- c(1,ind+1)
#'         end <- c(ind-1,nrow(data))
#'       }
#'       else{
#'         start <- 1
#'         end <- nrow(data)
#'       }
#'     }
#'     else if(nrow(period)==1){
#'       if((period$on_start[1]==1) && (period$on_end[1]==nrow(data))){
#'         start <- NA
#'         end <- NA
#'       }
#'       else{
#'         start <-c(1,period$on_end+1)
#'         end <-c(period$on_start -1, nrow(data))
#'       }
#'     }
#'     else if (nrow(period)==0){
#'       if (length(data$cond[data$cond ==1])>=1){
#'         ind <- which(data$cond ==1)
#'         start <- c(1,ind+1)
#'         end <- c(ind-1,nrow(data))
#'       }
#'       else{
#'         start <- 1
#'         end <- nrow(data)
#'       }
#'     }
#'     else{
#'         start <-c(1,period$on_end+1)
#'         end <-c(period$on_start -1, nrow(data))
#'     }
#'     rest <- data.frame(start,end)
#'     if (length(which(start>=end))>0){
#'     rest <- rest[-which(start>=end),]}
#'     period_rest[[j]] <- rest}
#'   return(period_rest)
#' }
