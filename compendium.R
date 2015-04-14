#Vergil's R Cookbook

#This document contains code, snippets, etc.



# Files -------------------------------------------------------------------

showPDF <- function(site) {
  path <- g.fulcrum$fulcrum_pdf[g.fulcrum$site_id == site]
  print(path)
  shell.exec(path)
}

# Strings -----------------------------------------------------------------

propCase <- Vectorize(function(x) { #change ANyThINg to Proper Case
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
})

#change multiple . in names to one _ (variable...name to variable_name)
fixNames <- function(x) {
  names(x) <- gsub('\\.+','_', tolower(names(x)))
  #strip trailing . from names
  names(x) <- gsub('\\.$','',names(x))
  return(x)}
  
  #get notifications when code is finished
library(notifyR); userkey <- 'xxxxx' #look up api key by logging in on pushover.net 
send_push(userkey,"R code finished")


# ODBC --------------------------------------------------------------------

#download all tables from a database
e.ODBC <- odbcDriverConnect('DSN=???;database=???;case=nochange')
e.odbctables<-sqlQuery(e.ODBC4, 'SHOW TABLES FROM ???')
for (i in 1:nrow(e.odbctables)){
  e.tablename <- e.odbctables[i,1]
  e.query <- paste('SELECT * FROM', e.tablename, 'WHERE site_id <> 1')
  print(paste0('Copying ', e.tablename, '...'))
  assign(paste0('e.',e.tablename), 
         sqlQuery(e.ODBC4,e.query))
}
odbcCloseAll()
#keep only scheduled sites
e.recruits <- subset(e.sites, sistatus1=='Scheduled')
#convert FACT dates to R
e.recruits$measure_install_date <- as.Date(e.recruits$measure_install_date, 
                                           origin='1900-01-01')


# Maps and Geocode --------------------------------------------------------


#bubble chart map by number of participants in a zipcode
library(ggmap)
a.cityfreq <- table(paste(a.sites2013$City, a.sites2013$State, sep=', '),
                    dnn='cityzip')
a.cityfreq <- as.data.frame(a.cityfreq)
a.citylatlon <- cbind(city=a.cityfreq[,1],
                      freq=a.cityfreq[,2],
                      geocode(as.character(a.cityfreq[,1]))) #geocode

a.cityfreq <- table(paste(a.sites2013$City, a.sites2013$State, sep=', '),
                    dnn='cityzip')
a.cityfreq <- as.data.frame(a.cityfreq)
a.citylatlon <- cbind(city=a.cityfreq[,1],
                      freq=a.cityfreq[,2],
                      geocode(as.character(a.cityfreq[,1]))) #geocode



# Sampling ----------------------------------------------------------------

#figure out proportions of strata
c.prop <- table(b.current[,c('Region', 'AC.Units.Category')])/nrow(b.current)
c.target <- round(c.prop*110) 
c.target <- melt(c.target)
#View(arrange(c.target,Region))


#order sitespop by strata (and premise, for reproducibility)
c.sitespop <- arrange(c.custphone,AC.Units.Category,Region,Site.ID)
set.seed(123) #for reproducibilty
#from the 'sampling' package (requires df to be sorted by strata)
c.samp <- strata(c.sitespop, 
                 c('AC.Units.Category','Region'), 
                 size=c.target$value*20,
                 method='srswor')
c.sample <- getdata(c.sitespop,c.samp)
count(subset(c.sample,,c(AC.Units.Category,Region))) #looks good



#see how much memory you are using:
.ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel 
  # (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

#source: http://www.statmethods.net/advgraphs/parameters.html
plot(1:10,1:10,type="n")
windowsFonts(
A=windowsFont("Arial Black"),
B=windowsFont("Bookman Old Style"),
C=windowsFont("Comic Sans MS"),
D=windowsFont("Symbol")
)
text(3,3,"Hello World Default")
text(4,4,family="A","Hello World from Arial Black")
text(5,5,family="B","Hello World from Bookman Old Style")
text(6,6,family="C","Hello World from Comic Sans MS")
text(7,7,family="D", "Hello World from Symbol")
