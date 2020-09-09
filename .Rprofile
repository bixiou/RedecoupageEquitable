linux <- F
if (linux) { options(download.file.method = "wget") # For Ubuntu 14.04
} else options(download.file.method = "auto")

library(utils)
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

package <- function(p) { 
  if (!is.element(p, installed.packages()[,1])) {
    install.packages(p); 
  }
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package('foreign')
package("readxl")
package("mfx")
package("Hmisc")
package("plyr")
package("ramify")
package("electoral")
# package("grDevices")
package("stringr")
# package("magick") # Bug sur Ubuntu, ne surtout pas décommenter sur Ubuntu
library(magick) # TODO
# Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "/home/adrien/anaconda3/bin", sep = .Platform$path.sep))
package("plotly")

# setwd("/var/www/We give the 99 percents/Redécoupage") # /!\ Changer le chemin d'accès
setwd("./Annexes") # /!\ Changer le chemin d'accès
load("env.RData")


##### Graphiques #####
stack_bars <- function(vars, data=s, miss=T, labels=NA, title=NA, accord=FALSE, en = FALSE, margin=c(2.5,17,0,3), cex=1, width=0.77/length(vars), weights=FALSE) {
  matrice <- c()
  colors <-   c(rainbow(4, end=4/15)[1:3], "green", "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]]) & !is.na(data[[var]]))))
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(data[[var]]==-2))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==-1))/length(which(!is.missing(data[[var]]))), length(which(data[[var]]==0))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==1))/length(which(!is.missing(data[[var]]))),  length(which(data[[var]]==2))/length(which(!is.missing(data[[var]]))))    
      if (weights) { mat <- c(sum(data[['weight']][which(data[[var]]==-2)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==-1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==0)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==1)])/sum(data[['weight']][!is.missing(data[[var]])]), sum(data[['weight']][which(data[[var]]==2)])/sum(data[['weight']][!is.missing(data[[var]])])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (is.na(labels)) { labels <- vars }
  if (accord) { values <- c("Pas du tout", "Pas vraiment d'accord", "Indifférent-e", "Assez", "Tout à fait d'accord")
  if (miss) { widths <- c(0.16,0.16,0.13,0.125,0.145,0.05) }
  else { widths <- c(0.18,0.185,0.15,0.14,0.2) } }
  else { values <- c("Baisser fortement", "légèrement", "Maintenir", "Augmenter légèrement", "fortement")
  if (miss) { widths <- c(0.153,0.14,0.14,0.15,0.083,0.05) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (en) {values <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
  if (accord) values <- c("Totally disagree", "Disagree", "Indifferent", "Agree", "Totally agree")
  if (miss) { widths <- c(0.16,0.15,0.14,0.13,0.12,0.06) }
  else { widths <- c(0.173,0.16,0.165,0.19,0.095) } }
  if (miss) { 
    if (en) values <- c(values, "PNR")
    else values <- c(values, "NSP") }
  # if (accord) { values <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord") }
  # else { values <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau actuel", "Augmenter légèrement", "Augmenter fortement") }
  # if (miss) { values <- c(values, "NSP (Ne sait pas, ne se prononce pas)")} # TODO: trouver widths pour ceux-là et les mettre
  before_par <- par()
  titre <- 0
  if (!is.na(title)) { titre <- 1.5 }
  par(mar=margin, oma=c(0,0,titre,0))
  frame()
  abline(v=seq(0,1,by=0.1), lty=3, col="grey")
  axis(1, at = seq(0,1,by=0.1))
  barplot(matrice, width=width, horiz=TRUE, add=TRUE, col=colors, names.arg = labels, cex.names = cex, border=NA, ylim=c(0,1), legend.text=values, las=1, args.legend=list(horiz=TRUE, bty='o', box.lwd=0, xjust=1, text.width=widths, x.intersp=0.3, x="topright")) # ncol=3, inset=-0.3
  title(title, outer=TRUE)
  par(before_par)
  # legend("topright", fill=colors, legend=values, ncol=2)
}
oui_non <- function(vars, file, labels = vars, data = s, display_value = T, sort=T, colors=color(2), weights=T, margin_r=0, margin_l=NA, title="", en=FALSE, NSP=FALSE) { # 250 l
  margin_t <- 30
  if (title!="") { margin_t <- 80 }
  if (grepl("<br>", title)) { margin_t <- 130 }
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  oui <- non <- nsp <- c()
  for (var in vars) {
    if (weights) {
      oui <- c(oui, sum(data[['weight']][which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      non <- c(non, sum(data[['weight']][which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv")])/sum(data[['weight']][which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) )
      nsp <- c(nsp, sum(data[['weight']][which(data[[var]]=="NSP" | data[[var]]==-1)])/sum(data[['weight']][which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")]) ) #  | data[[var]]==-1 | data[[var]]=="NSP"
    }
    else {
      oui <- c(oui, length(which(data[[var]]==T | data[[var]]=="Oui" | data[[var]]=="Pour" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="quotient"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      non <- c(non, length(which(data[[var]]==FALSE | data[[var]]=="Non" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="indiv"))/length(which(data[[var]]==T | data[[var]]==FALSE | data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="NSP"| data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
      nsp <- c(nsp, length(which(data[[var]]=="NSP" | data[[var]]==-1))/length(which(data[[var]]=="Oui" | data[[var]]=="Non" | data[[var]]=="Pour" | data[[var]]=="Contre" | data[[var]]=="Taxer davantage le capital" | data[[var]]=="Taxer davantage le travail" | data[[var]]=="quotient" | data[[var]]=="indiv")) )
    }  
  }
  true_nsp <- round(100 * nsp*(oui+non))
  oui <- round(100 * oui)
  non <- round(100 * non)
  nsp <- round(100 * nsp)
  if (sort) order_as <- order(oui/(oui+non))
  else order_as <- 1:length(oui)
  y <- labels[order_as]
  non <- non[order_as]
  nsp <- nsp[order_as]
  true_nsp <- true_nsp[order_as]
  if (sort) oui <- sort(oui)
  o <- round(100 * oui / (oui + non))
  n <- round(100 * non / (oui + non))
  
  if (en==T) {
    hover_oui <- paste('Yes<br>', oui, '% of answers<br>', o, '% of expressed answers')
    hover_non <- paste('No<br>', non, '% of answers<br>',n, '% of expressed answers')
    hover_nsp <- paste('PNR<br>', true_nsp, '% of answers')  
    Text <- c("Yes", "No", "PNR")      }
  else if (en==FALSE) {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- c("Oui", "Non", "NSP") }
  else {
    hover_oui <- paste('Oui<br>', oui, '% des réponses<br>', o, '% des réponses exprimées')
    hover_non <- paste('Non<br>', non, '% des réponses<br>',n, '% des réponses exprimées')
    hover_nsp <- paste('NSP<br>', true_nsp, '% des réponses')  
    Text <- en
    if (length(Text ==2)) Text <- c(Text, 'PNR')}
  if (display_value) {
    hover_oui <- paste(oui, '%')
    hover_non <- paste(non, '%')
    hover_nsp <- paste(true_nsp, '%')
  }
  if (!(NSP)) Text[3] <- ''
  print(oui)
  print(non)
  print(nsp)
  print(o)
  print(n)
  data <- data.frame(y, oui, non, nsp, o, n)
  data$y <- factor(data$y, levels = data[["y"]])
  y <- c(y, '')
  bars <- plot_ly(data, x = ~o, y = ~y, type = 'bar', orientation = 'h', text = hover_oui, textposition = 'auto', # last one displays values; colors were forestgreen and darkred
                  hoverinfo = 'text', marker = list(color = colors[1], line = list(color = 'white', width = 1))) %>%
    add_trace(x = ~n, text = hover_non, hoverinfo = 'text', marker = list(color = colors[2])) %>%
    add_trace(x = ~nsp, text = hover_nsp, hoverinfo = 'text', marker = list(color = 'lightgrey')) %>%
    layout(xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           hovermode = 'closest',
           barmode = 'stack',
           title = title,
           titlefont = list(color='black'),
           font = list(color='black'),
           # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = margin_l, r = margin_r, t = margin_t, b = 0),
           showlegend = FALSE) %>%
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = y,
                    xanchor = 'right',
                    text = y,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') %>%
    # labeling the first Likert scale (on the top)
    add_annotations(xref = 'x', yref = 'paper',
                    x = c(10, 90, 110),
                    y = 1.1,
                    text = Text,
                    font = list(family = 'Arial', size = 15, color = 'black'),
                    showarrow = FALSE) # %>%
  # labeling the percentages of each bar (x_axis)
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o / 2, y = y,
  #                 text = paste(data[,"oui"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n / 2, y = y,
  #                 text = paste(data[,"non"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # add_annotations(xref = 'x', yref = 'y',
  #                 x = o + n + nsp / 2, y = y,
  #                 text = paste(data[,"nsp"], '%'),
  #                 font = list(family = 'Arial', size = 14, color = 'white'),
  #                 showarrow = FALSE) %>%
  # api_create(bars, filename=file, sharing="public")
  return(bars) # bugs most often than not
}
data5 <- function(vars, data=e, miss=T, weights=T, rev=FALSE) {
  matrice <- c()
  colors <-  c(rainbow(4, end=4/15), "forestgreen") # c("red", "orange", "yellow", "green", "darkgreen") # rainbow(5, end=1/3)
  for (var in vars) {
    if (miss) {
      if (is.null(annotation(data[[var]]))) {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.na(data[[var]])))/length(which(!is.missing(n(data[[var]]))))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.na(data[[var]]))])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
      else {
        mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))),length(which(is.missing(data[[var]]) & !is.na(data[[var]])))/length(which(!is.missing(data[[var]])))) # removed "n()"
        if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]),sum(data[['weight']][which(is.missing(data[[var]]) & !is.na(data[[var]]))])/sum(data[['weight']][!is.missing(data[[var]])])) } }
      colors <- c(colors, "lightgrey")    }
    else {
      mat <- c(length(which(n(data[[var]])==-2))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==-1))/length(which(!is.missing(n(data[[var]])))), length(which(n(data[[var]])==0))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==1))/length(which(!is.missing(n(data[[var]])))),  length(which(n(data[[var]])==2))/length(which(!is.missing(n(data[[var]])))))    
      if (weights) { mat <- c(sum(data[['weight']][which(n(data[[var]])==-2)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==-1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==0)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==1)])/sum(data[['weight']][!is.missing(n(data[[var]]))]), sum(data[['weight']][which(n(data[[var]])==2)])/sum(data[['weight']][!is.missing(n(data[[var]]))])) } }
    matrice <- c(matrice, mat) }
  matrice <- matrix(c(matrice), ncol=length(vars))
  if (rev & !(miss)) return(matrice[5:1,])
  else if (rev & miss) return(matrice[c(5:1,6),])
  else return(matrice)
  # return(as.data.frame(matrice))
}
data1 <- function(vars, data=e, weights=T) {
  res <- c()
  for (var in vars) {
    if (weights) { res <- c(res, sum(data[['weight']][which(data[[var]]==TRUE)])/sum(data[['weight']][which(data[[var]]==TRUE | data[[var]]==FALSE)])) }
    else { res <- c(res, length(which(data[[var]]==T))/length(which(data[[var]]==T | data[[var]]==FALSE))) }
  }
  return( matrix(res, ncol=length(vars)) )
}
dataN <- function(var, data=e, miss=T, weights = T, return = "", fr=T, rev=FALSE, rev_legend = FALSE) {
  if (is.null(data[['weight']])) weights <- F # TODO? warning
  mat <- c()
  if (is.character(data[[var]]) | (is.numeric(data[[var]]) & !grepl("item", class(data[[var]])))) v <- as.factor(data[[var]])
  else v <- data[[var]]
  if (is.null(annotation(v))) levels <- levels(v)
  else levels <- labels(v)@.Data
  levels <- levels[!(levels %in% c("NSP", "PNR", "Non concerné·e"))]
  if (rev_legend) levels <- rev(levels) # new (05/20)
  for (val in levels) { # before: no %in% nowherer below
    if (weights) mat <- c(mat, sum(data[['weight']][which(v==val)])/sum(data[['weight']][!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))]))
    else mat <- c(mat, length(which(v==val))/length(which(!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))))) }
  if (rev) mat <- rev(mat)
  if (miss) {
    if (is.null(annotation(v))) {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.na(v) | v %in% c("NSP", "Non concerné·e"))])/sum(data[['weight']][!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e")))]))
      else mat <- c(mat, length(which(is.na(v) | v %in% c("NSP", "Non concerné·e")))/length(which(!is.missing(v) & (!(v %in% c("NSP", "Non concerné·e"))))))
    } else  {
      if (weights) mat <- c(mat, sum(data[['weight']][which(is.missing(v) & !is.na(v))])/sum(data[['weight']][!is.missing(v)]))
      else mat <- c(mat, length(which(is.missing(v) & !is.na(v)))/length(which(!is.missing(v)))) } }
  if (max(nchar(levels))==3 & 'Oui' %in% levels & 'Non' %in% levels) { if (which(levels=='Non') < which(levels=='Oui')) mat[2:1] <- mat[1:2]; levels[c(which(levels=='Oui'),which(levels=='Non'))] <- c('Non', 'Oui') }
  if ((return %in% c("levels", "legend")) & miss & fr) return(c(levels, 'NSP'))
  else if ((return %in% c("levels", "legend")) & miss & (!(fr))) return(c(levels, 'PNR'))
  else if ((return %in% c("levels", "legend")) & (!(miss))) return(levels)
  else return(matrix(mat, ncol=1))
}
dataKN <- function(vars, data=e, miss=T, weights = T, return = "", fr=T, rev=FALSE) {
  if (is.logical(data[[vars[1]]])) return(data1(vars, data, weights))
  else {
    res <- c()
    for (var in vars) res <- c(res, dataN(var, data, miss, weights, return, fr, rev))
    return(matrix(res, ncol=length(vars))) }
}
dataN2 <- function(var, df = list(c, e), miss=T, weights = T, fr=T, rev=FALSE, return = "") {
  if (return %in% c("levels", "legend")) return(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev, return = return))
  else return(cbind(dataN(var, df[[1]], miss = miss, weights = weights, fr = fr, rev = rev), dataN(var, df[[2]], miss = miss, weights = weights, fr = fr, rev = rev))) }
color5 <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
color <- function(v, grey=FALSE, grey_replaces_last = T, rev_color = FALSE, theme='RdBu') {
  if (is.matrix(v)) n <- nrow(v)
  else if (length(v) > 1) n <- length(v)
  else n <- v # cf. http://research.stowers.org/mcm/efg/R/Color/Chart/ColorChart.pdf
  if (grey & grey_replaces_last & n > 1) n <- n-1
  if (theme=='rainbow') {
    if (n == 1) cols <- c("#66B3B3") # "brown": #A52A2A Presentation Teal: #008096 (title) #1A8C8C (dark) #66B3B3 #99CCCC (light)
    else if (n == 2) cols <- c("#66B3B3", "#A52A2A") # c("lightgreen", "plum") = c("#90EE90", "#DDA0DD")
    else if (n == 3) cols <- color5[c(1,3,5)]
    else if (n == 4) cols <- c(rainbow(4, end=4/15)[1:3], "#228B22")
    else if (n == 5) cols <- c(rainbow(4, end=4/15)[1:3], "#00FF00", "#228B22") # the last two are: green, forestgreen
    else if (n == 6) cols <- rainbow(6)
    else if (n == 7) cols <- c("#000000", rainbow(7)[c(1:3,5:7)])
    else cols <- rainbow(n) # diverge_hcl green2red brewer.pal(n, Spectral/RdBu...)  https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
  } else {
    cols <- rev(brewer.pal(max(n, 3), theme))
    if (n == 1) cols <- cols[1]
    # if (n == 2) cols <- cols[c(1,3)]
    else if (n %% 2 == 0) cols <- rev(brewer.pal(n+2, theme))[c(1:(n/2),(n/2+2):(n+1))] }
  if (rev_color) cols <- rev(cols)
  if (grey & n > 1) return(c(cols, "#D3D3D3")) # lightgrey
  else return(cols)
}
# accord5 <- c("Pas du tout d'accord", "Pas vraiment d'accord", "Indifférent-e", "Assez d'accord", "Tout à fait d'accord")
oui_non5 <- c("Non, pas du tout", "Non, pas vraiment", "Indifférent-e/NSP", "Oui, plutôt", "Oui, tout à fait")
yes_no5 <- c("Not at all", "Not really", "Indifferent/PNR", "Rather yes", "Yes, completely")
# agree5 <- c("Strongly disagree", "Disagree", "Indifferent", "Agree", "Strongly agree")
# evol5 <- c("Baisser fortement", "Baisser légèrement", "Maintenir au niveau", "Augmenter légèrement", "Augmenter fortement")
# evolve5 <- c("Strongly decrease", "Slightly decrease", "Maintain", "Slightly increase", "Strongly increase")
barres <- function(data, vars, file, title="", labels, color=c(), rev_color = FALSE, hover=legend, nsp=TRUE, sort=TRUE, legend=hover, showLegend=T, margin_r=0, margin_l=NA, online=FALSE, 
                   display_values=T, thin=T, legend_x=NA, show_ticks=T, xrange=NA, save = FALSE, df=e, miss=T, weights = T, fr=T, rev=T, grouped = F) {
  if (missing(vars) & missing(legend) & missing(hover)) warning('hover or legend must be given')
  if (!missing(miss)) nsp <- miss
  if (missing(data) & !missing(vars)) {
    data <- dataKN(vars, data=df, miss=miss, weights = weights, return = "", fr=fr, rev=rev)
    if (missing(legend) & missing(hover)) { 
      if (is.logical(df[[vars[1]]])) hover <- legend <- labels # data1(var = vars[1], data=df, weights = weights)
      else hover <- legend <- dataN(var = vars[1], data=df, miss=miss, weights = weights, return = "legend", fr=fr, rev_legend = rev) } }
  if (length(color)==0) color <- color(data, nsp, rev_color = rev_color)
  margin_t <- 0 + 25*(!(thin))
  if (title!="") { margin_t <- 100 }
  if (grepl("<br>", title)) { margin_t <- 150 }
  legendSize <- 15 # 10, 13
  legendY <- 1.1  + 0.3*thin/(ncol(data)-1) # last term may be problematic
  legendX <- 0.2
  # legendFont <- 'Open Sans'
  if (is.na(margin_l)) { margin_l <- 4.7*max(nchar(labels)/(1 + str_count(labels, '<br>'))) }
  if (max(nchar(labels)) > 25) { legendSize <- 15 } # 9, 13
  # if (max(nchar(labels)) > 50) { legendSize <- 8 }
  # if (max(nchar(labels)) > 60) { legendSize <- 7 }
  if (max(nchar(labels)) > 50) { # 70
    legendSize <- 13 # 11
    # legendY = 1.2
    legendX= -0.2 # 1
    # if (ncol(data)>1) margin_t = 170
  }
  if (!is.na(legend_x)) legendX <- legend_x
  if (!showLegend) { margin_t <- max(0, margin_t - 70) }
  if (ncol(data)==1) legendY = 1.5 + 0.3*thin
  if (sort) {
    agree <- c()
    if (!missing(miss)) {
      if (miss) for (i in 1:length(labels)) agree <- c(agree, sum(data[floor(nrow(data)/2+1):max(1,(nrow(data)-1)),i]))
      else for (i in 1:length(labels)) agree <- c(agree, sum(data[ifelse(nrow(data)==1,1,ceiling(nrow(data)/2+1)):nrow(data),i]))
    } else {
      if (nrow(data)==5 | nrow(data)==6) { for (i in 1:length(labels)) { agree <- c(agree, data[4, i] + data[5, i]) } }
      else if (nrow(data)==7) { for (i in 1:length(labels)) { agree <- c(agree, data[6, i] + data[7, i]) } }
      else { for (i in 1:length(labels)) { agree <- c(agree, data[1, i]) } } }
    labels <- labels[order(agree, decreasing = rev)]
    data <- matrix(data[, order(agree, decreasing = rev)], nrow=nrow(data))
  }
  if (nrow(data)==1 & sort) {  
    hover <- hover[order(agree)]
    value <- c()
    for (i in 1:length(hover)) { 
      hover[i] <- paste(hover[i], "<br>Choisi dans ", round(100*data[1, i]), "% des réponses", sep="")
      value[i] <- paste(round(100*data[1, i]), '%', sep='') }
    hovers <- matrix(hover, nrow=length(hover))
    values <- matrix(value, nrow=length(hover))
  }
  else {
    hovers <- values <- c()
    if (nsp) {
      for (i in 1:(length(hover)-1)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]/(1+data[length(hover), j])), '% des réponses<br>', round(100*data[i, j]), '% des réponses exprimées') )
          values <- c(values, paste(round(100*data[i, j]/(1+data[length(hover), j])), '%', sep=''))
        }
      }
      for (j in 1:length(labels)) {
        hovers <- c(hovers, paste(hover[length(hover)], '<br>', round(100*data[length(hover), j]/(1+data[length(hover), j])), '% des réponses<br>') )
        values <- c(values, paste(round(100*data[length(hover), j]/(1+data[length(hover), j])), '%', sep=''))
      }
    }
    else {
      if (is.element(hover[length(hover)],c("PNR", "NSP"))) hover <- hover[1:(length(hover)-1)]
      if (is.element(legend[length(legend)],c("PNR", "NSP"))) legend <- legend[1:(length(legend)-1)]
      for (i in 1:length(hover)) { 
        for (j in 1:length(labels)) {
          hovers <- c(hovers, paste(hover[i], '<br>', round(100*data[i, j]), '% des réponses exprimées<br>') )
          values <- c(values, paste(round(100*data[i, j]), '%', sep=''))
        }
      }  
    }
    hovers <- matrix(hovers, ncol=length(hover))
    values <- matrix(values, ncol=length(hover))
  }
  if (!(display_values)) values <- replace(values, T, '')
  
  bars <- plot_ly(x = data[1,], y = labels, type = 'bar', orientation = 'h', text = values[,1], textposition = 'auto', # sort=FALSE, 
                  hoverinfo = hovers[,1], name=legend[1], marker = list(color = color[1], line = list(color = 'white'))) %>% # , width = 0
    
    layout(xaxis = list(title = "",
                        showgrid = show_ticks,
                        showline = FALSE,
                        showticklabels = show_ticks,
                        gridcolor = toRGB("gray70"), # + noir, + proche de 0
                        gridwidth = 1,
                        griddash = "dot",
                        autotick = FALSE,
                        ticks = "outside",
                        tick0 = 0,
                        dtick = 0.1,
                        ticklen = 5*show_ticks,
                        tickwidth = 1,
                        tickcolor = toRGB("gray70"),
                        zeroline = T, 
                        range = xrange,
                        domain = c(0.01 + 0.14*(!(" " %in% labels)), 1)
    ),
    yaxis = list(title = "",
                 showgrid = FALSE,
                 showline = FALSE,
                 showticklabels = FALSE,
                 categoryorder = "trace",
                 # automargin = T,
                 zeroline = FALSE),
    hovermode = 'closest',
    barmode = ifelse(grouped, 'group', 'stack'),
    title = list(text = title, font = list(color = 'black')),
    # title = title,
    # titlefont = list(color='black'),
    font = list(color='black', size=legendSize-1),
    # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
    margin = list(l = margin_l, r = margin_r, t = margin_t, b = 24, autoexpand = thin), # 21, autoexpand=FALSE removes useless margin at bottom but creates bug with legend
    # margin = list(b = 20, t = margin_t),
    legend = list(orientation='h', y=legendY, x=legendX, traceorder='normal', font=list(size=legendSize, color='black')), # family='Balto',  , family=legendFont
    # showlegend = (showLegend & !((("Yes" %in% legend) | ("Oui" %in% legend)) & (length(legend)<4)))) %>% 
    showlegend = (showLegend & !(setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP'))))) %>%
    
    # labeling the y-axis
    add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = labels,
                    xanchor = 'right',
                    text = labels,
                    font = list(family = 'Arial', size = 14, color = 'black'),
                    showarrow = FALSE, align = 'right') # %>%
    # Legend in the Yes/No case
    if ((setequal(legend, c('Yes', 'No', 'PNR')) | setequal(legend, c('Oui', 'Non', 'NSP')))) { 
      bars <- bars %>% add_annotations(xref = 'x', yref = 'paper',
                    x = c(0.1, 0.9, 1.1),
                    y = 1.5,
                    text = legend,
                    font = list(family = 'Arial', size = 16, color = 'black'),
                    showarrow = FALSE) } # %>%
  # print(nrow(data))
  # print(hover)
  # print(nrow(hovers))
  # print(ncol(hovers))
  if (nrow(data)>1) { for (i in 2:nrow(data)) { # evaluate=TRUE, 
    bars <- add_trace(bars, x = data[i,], name=legend[i], text = values[,i], hoverinfo = 'text', hovertext = hovers[,i], marker = list(color = color[i]))
  } }
  if (online) { api_create(bars, filename=file, sharing="public") }
  if (!missing(file) & save) save_plotly(bars, filename = file) # new
  return(bars)
}
# plot(1:3,1:3) # example
# dev.copy(png, filename="test.png") # save plot from R (not plotly)
# dev.off()
# orca(example, file = "image.png") # BEST METHOD, cf. below
save_plotly <- function(plot, filename = deparse(substitute(plot)), folder = '../Images/', width = dev.size('px')[1], height = dev.size('px')[2], method='orca', trim = T) {
  file <- paste(folder, filename, ".png", sep='')
  if (grepl('webshot', method)) { # four times faster: 2.5s (vs. 10s) but saves useless widgets and doesn't exactly respect the display
    saveWidget(politiques_1, 'temp.html')
    webshot('temp.html', file, delay = 0.1, vwidth = width, vheight = height)  
    file.remove('temp.html')}
  else orca(plot, file = file, width = width, height = height)
  if (trim) image_write(image_trim(image_read(file)), file)
}


