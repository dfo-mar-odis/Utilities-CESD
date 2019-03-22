# Script to process PRINCE CTD into both COERS.PRINCE tables as well as drop a
# copy of files for upload to MEDS

prince_proc <- function(file, station) {
  # Check to see if necessary packages are installed and load them
  req_packages <- c('tidyverse',
                    'data.table',
                    'lubridate',
                    'gridExtra'
                    )
  
  missing_packages <-
    req_packages[!(req_packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
    message("Installing required packages.")
  } else {
    message("All required packages are installed.")
  }
  
  packages_to_load <- req_packages[!req_packages %in% .packages()]
  if (length(packages_to_load) > 0) {
    lapply(packages_to_load, library, character.only = TRUE)
  } else {
    message("all packages are loaded and ready to use.")
  }
  # All packages are now installed and loaded. Good to start loading data
  
  # Read data from file specified in prince_pro function.
  read_file <- fread(file) %>%
    transmute(STATION = station,
              date = ymd_hms(paste(YYYY, MMM, DD, `HH:MM:SS`)),
              SDATE = paste(DD, toupper(MMM), substr(YYYY, 3, 4), sep = "-"),
              YEAR = year(date),
              MONTH = month(date),
              DAY = DD,
              HOUR = substr(`HH:MM:SS`, 1, 2),
              MINUTE = substr(`HH:MM:SS`, 4, 5),
              DEPTH = PrSM,
              TEMP = T090C,
              SAL = Sal00,
              DENSITY = `Sigma-é00`,
              OXYMLL = `Sbeox0ML/L`,
              OXYMGL = `Sbeox0Mg/L`,
              OXYPSAT = `Sbeox0PS`,
              OXYSATMGL = `OxsatMg/L`,
              TURBFTU = `SeaTurbMtr`,
              WETLABS = `WetStar`,
              PAR = Par,
              XMISS = ""
              ) 
  raw_file <- read_file %>%
    select(-date)
  
  if (dim(raw_file)[2] != 19) {
    message( "\n
              ################################################################################
              ################################################################################
              #######       DATA DOES NOT HAVE THE APPROPRIATE NUMBER OF COLUMNS!      #######
              #######       DO NOT UPLOAD UNTIL YOU HAVE FIXED THIS!!!!                #######
              ################################################################################
              ################################################################################\n \n")
  }
   
  
  write_location <- paste0(
    "//ent.dfo-mpo.ca/ATLShares/Science/CESD/COERS/FPage/data/CTD/prince/reformatted/",
    paste(paste(
      paste(
        raw_file$YEAR[1],
        str_pad(
          raw_file$MONTH[1],
          width = 2,
          side = 'left',
          pad = '0'
        ),
        str_pad(
          raw_file$DAY[1],
          width = 2,
          side = 'left',
          pad = "0"
        ),
        sep = "_"
      ),
      paste0("PRINCE", station),
      sep = "_"
    ) , "csv", sep = ".")
  )
  write_csv(raw_file, write_location)
  message(paste("File created:", write_location))
  
  profile_data <- raw_file %>%
    mutate(SDATE = read_file$date) %>%
    select(-STATION, -YEAR:-MINUTE, -XMISS) %>%
    mutate(CAST = case_when(DEPTH - lag(DEPTH) < 0 ~ "UP",
                            TRUE ~ "DOWN")) %>%
    gather(key = 'variable', value = 'value', -DEPTH, -SDATE, -CAST) %>%
    mutate(value = as.numeric(value)) %>%
    arrange(SDATE)
    
  
  ggplot(data = profile_data) +
    geom_path(aes(x = value, y = -DEPTH, col = CAST)) +
    facet_wrap(vars(variable), scales = 'free') +
    ggtitle(paste0("Prince ", station, " - ", format(profile_data$SDATE, "%B %d, %Y @ %H:%M UTC")))

  ggsave(
    paste0(
      "//ent.dfo-mpo.ca/ATLShares/Science/CESD/COERS/FPage/data/CTD/prince/reformatted/plots/",
      paste(paste(
        paste(
          raw_file$YEAR[1],
          str_pad(
            raw_file$MONTH[1],
            width = 2,
            side = 'left',
            pad = '0'
          ),
          str_pad(
            raw_file$DAY[1],
            width = 2,
            side = 'left',
            pad = "0"
          ),
          sep = "_"
        ),
        paste0("PRINCE", station),
        sep = "_"
      ) , "png", sep = ".")
    ),
    plot = last_plot(),
    width = 8,
    height = 8,
    units = "in"
  )
}
