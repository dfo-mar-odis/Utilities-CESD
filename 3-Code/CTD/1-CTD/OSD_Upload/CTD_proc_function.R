CTD_proc <- function(x, station = NA, deploy = lastDeploy + 1, lat = NA, lon = NA , 
                     confile = NA, hexfile = NA, project = NA, area = NA, 
                     tranid) {
  require(tidyverse)
  require(dbplyr)
  require(lubridate)
  require(ggmap)
  
  # Connect to PTRAN DFO database
  PTRAN <- DBI::dbConnect(odbc::odbc(),
                 dsn   = "PTRAN 64bit",
                 PWD    = rstudioapi::askForPassword("Database password"),
                 Port   = 1521)
  
  # Pull in deployment information from LOSIERR.SABS_OSDINF
  OSDINF <- tbl(PTRAN, in_schema("LOSIERR", "SABS_OSDINF"))
  
  # determine last deployment number and save it as a variable lastDeploy
  lastDeploy <- OSDINF %>%
    summarise(DEPLOY = max(DEPLOY, na.rm = TRUE)) %>%
    collect() %>%
    as.numeric(.)
  
  # give path to .ASC file to be processed by script
  file <- read_delim(x, delim = "\t", col_types = cols()) %>%
    data.frame()
  
  OSD_INF <- file %>%
    transmute(DEPLOY = deploy,
              STATION = station,
              SMONTH = toupper(MMM),
              SYEAR = format(as.Date(as.character(YYYY), "%Y"), "%y"),
              SDATE = paste(DD, SMONTH, SYEAR, sep = "-"),
              DATETIME = ymd_hms(paste(`YYYY`, `MMM`, `DD`, `HH.MM.SS`, sep = "")),
              YEAR = year(DATETIME),
              MONTH = month(DATETIME),
              DAY = day(DATETIME),
              HOUR = hour(DATETIME),
              MINUTE = minute(DATETIME),
              DEPTH = max(PrSM),
              LATITUDE = lat,
              LONGITUDE = lon,
              CONFILE = confile,
              HEXFILE = hexfile,
              PROJECT = project,
              AREA = area,
              TRANID = tranid) %>%
    filter(MINUTE == first(MINUTE)) %>%
    select(-SMONTH, - SYEAR, -DATETIME) %>%
    data.frame()
  
    glimpse(OSD_INF) %>%
    write_csv(paste((lastDeploy + 1), "OSD_INF.csv", sep = "_"))
  
  map <- ggmap(get_map(location = c(OSD_INF$LONGITUDE, OSD_INF$LATITUDE))) +
    geom_point(aes(x = LONGITUDE, y = LATITUDE), data = OSD_INF, colour = 'red', size = 2)
  print(map)
  
  readline(prompt="Does this look correct? If yes, press [enter] to continue or [Esc] to cancel")
  
  OSD_HYD <- file %>% 
    transmute(DEPLOY = deploy,
              SDEPTH = PrSM,
              TEMP = T090C,
              SAL = Sal00,
              DENSITY = Sigma..e9.00,
              OXYMLL = Sbeox0ML.L,
              OXYMGL = Sbeox0Mg.L,
              OXYPSAT = Sbeox0PS,
              OXYSATMGL = OxsatMg.L,
              TURBFTU = SeaTurbMtr,
              WETLABS = WetStar,
              PAR = Par,
              TRANSX = Flag) %>%
    data.frame() 
  
  
  # if the Tubridity is out of range, recode to -9999 so it can fit into the Oracle tables
  OSD_HYD$TURBFTU[OSD_HYD$TURBFTU == '-9.99e-29'] <- -99
  OSD_HYD$WETLABS[OSD_HYD$WETLABS == '-9.99e-29'] <- -99
  OSD_HYD$PAR[OSD_HYD$PAR == '-9.99e-29'] <- -99
  
  
  # Show dataframe
  glimpse(OSD_HYD)
  
  # Write CSV
  write_csv(OSD_HYD, paste((lastDeploy + 1), "OSD_HYD.csv", sep = "_"))
  
  OSD_HYD <- gather(OSD_HYD, key = parameter, value = value, TEMP:TRANSX)

  ggplot(data = OSD_HYD, aes(y = -SDEPTH, x = value, group = parameter)) +
    geom_point() +
    facet_grid(~parameter, scales = "free") +
    ggtitle(paste("Project = ", OSD_INF$PROJECT, ", Site = ",OSD_INF$STATION,
                  ", Date = ", OSD_INF$SDATE, sep = ""))
}