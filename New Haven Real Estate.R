# New Haven Real Estate
# Web scraping with R
myresult <- data.frame(1:27307, rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307),
                       rep(NA, times=27307), rep(NA, times=27307), rep(NA, times=27307), 
                       rep(NA, times=27307), rep(NA, times=27307), 
                       stringsAsFactors =FALSE)
names(myresult) <- c("pid", "multibuilding", "location", "totval", "address", 
                     'buyer1', 'date1', 'price1', 'buyer2', 'date2', 'price2',
                     'buyer3', 'date3', 'price3', 'buyer4', 'date4', 'price4',
                     'buyer5', 'date5', 'price5', "yearbuilt", "sqft", "replcost", 
                     "pctgood", "style", "model", "grade", "occupancy", "actype", 
                     "bedrooms", "nineplus", "bathrooms", "halfbaths", "bathstyle", 
                     "kstyle", "exval", "acres", "zone", "neighborhood", "landval", 
                     "garagesqft")

getdata <- function ( x ){
  # multiple building indicator
  build_line <- grep("MainContent_ctl.*_lblHeading", x) # .* means everything
  if (length(build_line) > 1){
    build_indicator <- TRUE
  } else{
    build_indicator <- FALSE
  }
  
  # location
  location_line <- grep( "MainContent_lblLocation\"", x ) 
  location <- x[location_line]
  location <- gsub("<[^<>]*>", "", location)
  location <- gsub("^\\s+", "", location)
  
  # totval
  totval_line <- grep ( "MainContent_lblGenAppraisal\"", x)
  totval <- x[totval_line]
  totval <- as.numeric(gsub("\\D", "", totval))
  
  # owner add
  add_line <- grep("MainContent_lblAddr1", x)
  add <- x[add_line]
  add <- gsub("<[^<>]*>", "", add)
  add <- gsub("\t\t\tAddress", "", add)
  
  # owner history
  thisline <- try(grep("Ownership History", x))
  thisline <- thisline[2]
  thisline2 <- try(grep("Building Information", x))
  minus <- thisline2 - thisline
  a <- rep(NA, 5)
  if (minus >= 24){
    for (i in 1:5){
      a[i] <- x[thisline+2*(i+1)]
    }
  }else{
    for (j in 1:4){
      if (minus==2*j+14){
        for (i in 1:j){
          a[i] <- x[thisline+2*(i+1)]
        }
      }
    }
  }
  d <- c()
  for (k in 1:5){
    text <- strsplit(a[k], "</td>")
    tar<-c()
    for (i in c(1,2,6)){
      t <- text[[1]][i]
      t<- stringr::str_trim(t, side = "both") # delete the blank space at the beginning&end
      t <- gsub("<[^<>]*>","",t)
      tar <- c(tar,t)}
    tar[2] <- as.numeric(gsub("\\D", "", tar[2]))
    d <- c(d,tar[1],tar[3],tar[2])
  }
  
  # year build
  year_line <- grep("lblYearBuilt", x)
  year_line <- year_line[1]
  year <- x[year_line]
  year <- gsub("<[^<>]*>","",year)
  year <- gsub("^\\s+", "", year)
  year <- as.numeric(year)
  
  # get sqft from x
  sqft_line <- grep("MainContent_ctl01_lblBldArea", x)
  sqft <- x[sqft_line]
  sqft <- gsub("<[^<>]*>", "", sqft)
  sqft <- gsub("\\D", "", sqft)
  sqft <- as.numeric(sqft)
  
  #get replcost from x
  repl_line <- grep("MainContent_ctl01_lblRcn", x)
  repl_line <- repl_line[1]
  repl <- x[repl_line]
  repl <- gsub("<[^<>]*>", "", repl)
  repl <- gsub("\\D", "", repl)
  repl <- as.numeric(repl)
  
  #get pctgood from x
  pct_line <- grep("MainContent_ctl01_lblPctGood", x)
  pct <- x[pct_line]
  pct <- gsub("<[^<>]*>", "", pct)
  pct <- gsub("\\D", "", pct)
  pct <- as.numeric(pct)
  
  #get style from x
  sty_line <- grep("Building Attributes", x)
  sty_line <- sty_line[1]
  sty_line <- sty_line+4
  sty <- x[sty_line]
  sty <- strsplit(sty, "</td>")
  sty <- sty[[1]][2]
  sty <- gsub("<[^<>]*>", "", sty)
  sty <- stringr::str_trim(sty, side = "both")
  if (sty==""){sty <- NA}

  #get model from x
  mod_line <- grep("Building Attributes", x)
  mod_line <- mod_line[1]
  mod_line <- mod_line+6
  mod <- x[mod_line]
  mod <- strsplit(mod, "</td>")
  mod <- mod[[1]][2]
  mod <- gsub("<[^<>]*>", "", mod)
  mod <- stringr::str_trim(mod, side = "both")
  if (mod==""){mod <- NA}
  
  #get grade from x
  grade_line <- c(grep("Grade", x), grep("GRADE", x))
  if (length(grade_line)==0){
    grade <- NA
  }else{
    grade <- x[grade_line]
    grade <- strsplit(grade, "</td>")
    grade <- grade[[1]][2]
    grade <- gsub("<[^<>]*>", "", grade)
    grade <- stringr::str_trim(grade, side = "both")
    if (grade==""){grade <- NA}
  }
  
  #get occupancy from x
  occu_line <- grep("Building Attributes", x)
  occu_line <- occu_line[1]
  occu_line <- occu_line+12
  occu <- x[occu_line]
  occu <- strsplit(occu, "</td>")
  occu <- occu[[1]][2]
  occu <- gsub("<[^<>]*>", "", occu)
  occu <- as.numeric(occu)
  
  # ac type
  ac_line <- grep("AC Type", x)
  if (length(ac_line)==0){
    ac <- NA
  } else {
    ac_line <- (ac_line)[1]
    ac <- x[ac_line]
    ac <- strsplit(ac, "</td>")
    ac <- ac[[1]][2]
    ac <- gsub("<[^<>]*>", "", ac)
    ac <- stringr::str_trim(ac, side = "both") 
    if (ac==""){ac <- NA}
  }
  
  # bedrooms
  bed_line <- c(grep("Ttl Bedrms", x),grep("Total Bedrooms", x), grep("Total Bedrms",x))
  bed_line <- (bed_line)[1]
  bed <- x[bed_line]
  bed <- gsub("<[^<>]*>","",bed) # building 1
  if (length(grep("9+", bed)) >= 1){
    b <- TRUE
  }else{
    b <- FALSE}
  bed <- as.numeric(gsub("\\D", "", bed))
  
  # bathrooms
  bath_line <- c(grep("Ttl Bathrms", x),grep("Total Bthrms", x),grep("Total Baths",x))
  bath_line <- (bath_line)[1]
  bath <- x[bath_line]
  bath <- strsplit(bath, "</td>")
  bath <- bath[[1]][2]
  bath <- as.numeric(gsub("<[^<>]*>", "", bath))
  
  # halfbathrooms
  half_line <- c(grep("Ttl Half Bths", x),grep("Total Half Baths", x))
  half_line <- (half_line)[1]
  half <- x[half_line]
  half <- as.numeric(gsub("\\D", "", half))
  
  # bathstyle
  bstyle_line <- grep("Bath Style", x)
  if (length(bstyle_line)==0){
    bstyle <- NA
  } else {
    bstyle_line <- (bstyle_line)[1]
    bstyle <- x[bstyle_line]
    bstyle <- gsub("<[^<>]*>", "", bstyle)
    bstyle <- gsub("\t\t\t\t\tBath Style:", "", bstyle)
    bstyle <- stringr::str_trim(bstyle, side = "both") 
    if (bstyle==""){bstyle <- NA}
  }
  
  # kstyle
  kstyle_line <- grep("Kitchen Style", x)
  if (length(kstyle_line)==0){
    kstyle <- NA
  } else {
    kstyle_line <- (kstyle_line)[1]
    kstyle <- x[kstyle_line]
    kstyle <- gsub("<[^<>]*>", "", kstyle)
    kstyle <- gsub("\t\t\t\t\tKitchen Style:", "", kstyle)
    kstyle <- stringr::str_trim(kstyle, side = "both") 
    if (kstyle==""){kstyle <- NA}
  }
  
  # extra value
  exval <- c()
  exval_line_start <- grep("tabs-5", x)
  exval_line_end <- grep("tabs-6", x)
  if (exval_line_end - exval_line_start == 20){
    exval <- NA
  }else{
    number <- (exval_line_end - exval_line_start - 17)/2
    for(e in 1:number){
      t <- x[exval_line_start+11+2*e]
      t <- strsplit(t, "</td>")
      t <- t[[1]][4]
      t <- gsub("\\D", "", t) 
      t <- as.numeric(t)
      exval <- c(exval, t)
    }
  }
  exval <- sum(exval)
  
  # land size
  line_acre <- grep("MainContent_lblLndAcres",x)
  acre <- x[line_acre]
  acre <- stringr::str_trim(acre, side = "both")
  acre <- gsub("<[^<>]*>","",acre)
  acre <- gsub("[^.0-9]","",acre) # only keep the figures!
  
  # zone
  line_zone <- grep("MainContent_lblZone", x)
  zone <- x[line_zone]
  zone <- stringr::str_trim(zone, side = "both")
  zone <- gsub("Zone","",zone)
  zone <- gsub("<[^<>]*>","",zone)
  
  # neighborhood
  line_nb <- grep("MainContent_lblNbhd", x)
  nb <- x[line_nb]
  nb <- stringr::str_trim(nb, side = "both")
  nb <- gsub("Neighborhood","",nb)
  nb <- gsub("<[^<>]*>","",nb)
  
  # landval
  val_line <- grep("MainContent_lblLndAppr",x)
  val <- x[val_line]
  val <- stringr::str_trim(val, side = "both")
  val <- gsub("[^.0-9]","",val)    #same with \\D
  
  # garage
  Gar_line <- grep("FGR</td><td>Garage", x)
  Gar_line <- Gar_line[1]
  if (length(Gar_line)==0){
    garage <- NA
  }
  else (length(Gar_line)>=1)
  { garage <- x[Gar_line+1]
    garage <- gsub("\\D", "", garage)
    garage <- as.numeric(garage)
  }
  
  return (list(build_indicator, location, totval, add, d[1], d[2], d[3], d[4],
               d[5], d[6], d[7], d[8], d[9], d[10], d[11], d[12], d[13], d[14], 
               d[15], year, sqft, repl, pct, sty, mod, grade, occu, ac, bed, 
               b, bath, half, bstyle, kstyle, exval, acre, zone, nb, val, 
               garage))
}

bad <- dget(file ="bad.txt")

for ( i in (1:27307)){
  if ( !(i %in% bad) ){
    # Set file path
    filename <- try(file.path('newdata2016', paste(i, '.html', sep="")))
    cat(i)
    # Read file 
    x <- try(scan( file = filename, what="", sep="\n" ))
    
    # Extract data into "myresult"
    data <- try(getdata(x))
    for ( j in (1:40)){
      try(myresult[i, j+1] <- data[[j]])
    }
  }
}


write.csv(myresult, file = "625_xy224.csv", row.names=FALSE)
