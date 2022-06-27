pie_chart_result<-function(tab_res){
  library(plotly)

  tab_res$Conclusion[tab_res$Pertinence==0] <- "Conclusions non pertinentes"
  data <- data.frame(table(tab_res$Conclusion))
  colnames(data) <- c("Conclusion","Freq")

  fig <- plot_ly(data, labels = ~Conclusion, values = ~Freq, type = 'pie',
                 textposition = 'inside',
                 textinfo = 'percent',
                 insidetextfont = list(color = '#FFFFFF'),
                 hoverinfo = 'text',
                 text = ~paste(Freq,'/',nrow(tab_res)),
                 marker = list(colors = colors,
                               line = list(color = '#FFFFFF', width = 1)),
                 #The 'pull' attribute can also be used to create space between the sectors
                 showlegend = TRUE)
  fig <- fig %>% layout(title = 'Résultat du diagnostic',
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

  fig
  return(fig)
}
