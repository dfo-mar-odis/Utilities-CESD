# RScript_CASTAWAY_QC_v10
# written by F.H.Page

# -------------------------------------------------------------------
# remove all variable names and dataframes from within the workspace
# -------------------------------------------------------------------

rm(list=ls())

# -----------------------------------------------------------------------------
# DEFINE FUNCTIONS
# -----------------------------------------------------------------------------

# 
# ---- FUNCTION to calculate the vertical differences ie gradients ----
#         Note: a negative difference means the deeper value is greater than the shallow value

  delta_y <- function(y) {
               a <- head(y,-1) # removes the last row in y;  ie keeps all rows except last
               b <- tail(y,-1) # removes the first row in y; ie keeps all rows except first
               delta <- a-b
               delta
            } # end of function

# ---- FUNCTION to calculate the mean depths between depth records ----

  avg_y <- function(y) {
               a <- head(y,-1) # removes the last row in y;  ie keeps all rows except last
               b <- tail(y,-1) # removes the first row in y; ie keeps all rows except first
               avg <- (a+b)/2
               avg              
             } # end of function

# ---- FUNCTION to estimate the beginning of the downcast

    DNCAST_Begin_Time <- function(y) {
                           # --- determine the row with the maximum pressure
                             row_max_Pres <- which(y$Pres_db == max(y$Pres_db)) 
                           # --- select the downcast part of the dataframe
                             DN_CAST <- y[1:row_max_Pres,]
                           # --- sort the dataframe into reverse time
                           #  DN_CAST_DATA_rev    <- DN_CAST_DATA_descend[order(-DN_CAST_DATA_descend$Time_s),]
                           # --- calculate the rate of change in pressure 
                             P1      <- DN_CAST$Pres_db[-length(DN_CAST$Pres_db)] # removes the last record
                             P2      <- DN_CAST$Pres_db[-1]                       # removes the first record
                             Delta_P <- P1-P2                                     # should be negative(positive) if instrument descending(ascending)
                             T1      <- DN_CAST$Time_s[-length(DN_CAST$Time_s)]   # removes the last record
                             T2      <- DN_CAST$Time_s[-1]                        # removes the first record
                             Delta_T <- T1-T2                                     # should always be negative
                             W_CAST  <- Delta_P/Delta_T                           # should be positive(ascending) when descending(ascending)
                           # --- identify the row number
                             begin_DN_row <- max(which(W_CAST < 0.25))                            # threshold velocity
                           # --- convert row number to a time
                             DN_CAST$Time_s[begin_DN_row]
                           }

# ------------------------------------------------------------
# choose the data file containing a profile of CASTAWAY data
# ------------------------------------------------------------

cat("\n")
cat("Import a CASTAWAY data file\n")
cat("\n")
cat("    The filename should have one of the following forms:                   \n")
cat("\n")
cat("        CCxxx...xxR.txt  should contain the CASTAWAY Generated RAW Data         data \n")
cat("        CCxxx...xxD.txt  should contain the CASTAWAY Generated DOWN CAST        data \n")
cat("        CCxxx...xxU.txt  should contain the CASTAWAY Generated UP CAST          data \n")
cat("        CCxxx...xxDU.txt should contain the CASTAWAY Generated DOWN and UP CAST data \n")
cat("        CCxxx...xxP.txt  should contain the CASTAWAY Generated PROCESSED        data \n")

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")
cat("     Select any one of the file forms to continue. \n")
cat("       The Script will look for all of the forms associated with the selected cast. \n")
cat(" \n")

INPUT.DATA.FILE.NAME <- file.choose()

# ----------------------------------------------------------------------------------
# extract the header information from the data file 
# ----------------------------------------------------------------------------------

INPUT.Device          <- scan(INPUT.DATA.FILE.NAME, skip=0,  nlines=1, sep="\t", what=list("",""))
INPUT.FileName        <- scan(INPUT.DATA.FILE.NAME, skip=1,  nlines=1, sep="\t", what="character")
INPUT.CastTimeUTC     <- scan(INPUT.DATA.FILE.NAME, skip=2,  nlines=1, sep="\t", what="character")
#INPUT.ProcessingType  <- scan(INPUT.DATA.FILE.NAME, skip=5,  nlines=1, sep="\t", what="character")
INPUT.StartLat        <- scan(INPUT.DATA.FILE.NAME, skip=9,  nlines=1, sep="\t", what="character")
INPUT.StartLon        <- scan(INPUT.DATA.FILE.NAME, skip=10, nlines=1, sep="\t", what="character")
INPUT.EndLat          <- scan(INPUT.DATA.FILE.NAME, skip=15, nlines=1, sep="\t", what="character")
INPUT.EndLon          <- scan(INPUT.DATA.FILE.NAME, skip=16, nlines=1, sep="\t", what="character")

# ************************************************************************************
# ===================================================================================
# Read all types of CASTAWAY output files associated with the choosen CASTAWAY profile
#
# Note:
#   - the CASTAWAY software default gives a file name that does not include an 
#       up, down, raw or processed indicator in the title
#   - this R Script requires the user to have specified filenames that have the 
#       up, down, raw or processed indicator in the title
#
# ===================================================================================
# ************************************************************************************

# -----------------------------------------------------------------------------------------
# ---- generate the filenames for each Processing Type from the choosen input filename ----
# -----------------------------------------------------------------------------------------

INPUT.DATA.FILE.NAME.R  <- noquote(paste(substr(INPUT.DATA.FILE.NAME,1,90), "_R.txt",sep=""))
INPUT.DATA.FILE.NAME.DN <- noquote(paste(substr(INPUT.DATA.FILE.NAME,1,90), "_D.txt",sep=""))
INPUT.DATA.FILE.NAME.UP <- noquote(paste(substr(INPUT.DATA.FILE.NAME,1,90), "_U.txt",sep=""))
INPUT.DATA.FILE.NAME.DU <- noquote(paste(substr(INPUT.DATA.FILE.NAME,1,90),"_DU.txt",sep=""))
INPUT.DATA.FILE.NAME.P  <- noquote(paste(substr(INPUT.DATA.FILE.NAME,1,90), "_P.txt",sep=""))

# -------------------------------------------------------------------------
# ---- import the complete data files for each of the Processing Types ----
# -------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.R   <- read.table(INPUT.DATA.FILE.NAME.R,header=FALSE,skip=29)
INPUT.CASTAWAY.PROFILE.DN  <- read.table(INPUT.DATA.FILE.NAME.DN,header=FALSE,skip=29)
INPUT.CASTAWAY.PROFILE.UP  <- read.table(INPUT.DATA.FILE.NAME.UP,header=FALSE,skip=29)
INPUT.CASTAWAY.PROFILE.DU  <- read.table(INPUT.DATA.FILE.NAME.DU,header=FALSE,skip=29)
INPUT.CASTAWAY.PROFILE.P   <- read.table(INPUT.DATA.FILE.NAME.P,header=FALSE,skip=29)

# ---------------------------------------------------------------------------------------
# ---- test to see if the imported data files contain the expected CASTAWAY filename ----
# ---------------------------------------------------------------------------------------

# ---- extract the filename header information from the imported data files ----

INPUT.FileName.R        <- scan(INPUT.DATA.FILE.NAME.R,  skip=1,  nlines=1, sep="\t", what="character")
INPUT.FileName.DN       <- scan(INPUT.DATA.FILE.NAME.DN, skip=1,  nlines=1, sep="\t", what="character")
INPUT.FileName.UP       <- scan(INPUT.DATA.FILE.NAME.UP, skip=1,  nlines=1, sep="\t", what="character")
INPUT.FileName.DU       <- scan(INPUT.DATA.FILE.NAME.DU, skip=1,  nlines=1, sep="\t", what="character")
INPUT.FileName.P        <- scan(INPUT.DATA.FILE.NAME.P,  skip=1,  nlines=1, sep="\t", what="character")

# ---- test to see if the filename in the imported data files matches the expected file name ----

    cat("********************************************************************************************\n")

    FileNameFlagg    <- "NotTripped"                          # set the flaggs to the default value
    FileNameFlagg.R  <- "NotTripped"
    FileNameFlagg.DN <- "NotTripped"
    FileNameFlagg.UP <- "NotTripped"
    FileNameFlagg.DU <- "NotTripped"
    FileNameFlagg.P  <- "NotTripped"

if (INPUT.FileName.R[2] != INPUT.FileName[2]) {
    cat("***************************************************************************\n")
    cat("!!!!! WARNING: Imported CASTAWAY RAW data file DOES NOT contain RAW data   \n")
    FileNameFlagg   <- "Tripped" 
    FileNameFlagg.R <- "Tripped" 
    } # end of if
if (INPUT.FileName.DN[2] != INPUT.FileName[2]) {
    cat("***************************************************************************\n")
    cat("!!!!! WARNING: Imported CASTAWAY DOWN data file DOES NOT contain DOWN data  \n")
    FileNameFlagg    <- "Tripped"
    FileNameFlagg.DN <- "Tripped" 
    } # end of if
if (INPUT.FileName.UP[2] != INPUT.FileName[2]) {
    cat("***************************************************************************\n")
    cat("!!!!! WARNING: Imported CASTAWAY UP data file DOES NOT contain UP data    \n")
    FileNameFlagg    <- "Tripped"
    FileNameFlagg.UP <- "Tripped"  
    } # end of if
if (INPUT.FileName.DU[2] != INPUT.FileName[2]) {
    cat("***************************************************************************\n")
    cat("!!!!! WARNING: Imported CASTAWAY DOWN and UP data file DOES NOT contain Down and Up data   \n")
    FileNameFlagg    <- "Tripped"
    FileNameFlagg.DU <- "Tripped"  
    } # end of if
if (INPUT.FileName.P[2] != INPUT.FileName[2]) {
    cat("***************************************************************************\n")
    cat("!!!!! WARNING: Imported CASTAWAY PROCESSED data file DOES NOT contain PROCESSED data   \n")
    FileNameFlagg   <- "Tripped"  
    FileNameFlagg.P <- "Tripped"
    } # end of if

if (FileNameFlagg == "NotTripped") {   
    cat("\n") 
    cat("      NOTE: All of the Input files contain the expected data processing type \n")
    cat("\n")
    cat("      ACTIONS: R Script will continue                                                       \n")
    cat("********************************************************************************************\n")
    cat("\n")
    } # end of if

if (FileNameFlagg == "Tripped") {
    cat("\n")
    cat("      NOTE: One or more of the Input files do not contain the expected filename \n")
    cat("\n")
    cat("      ACTIONS: R Script has been terminated                                                 \n")
    cat("               Check file names and correct as appropriate             \n")
    cat("********************************************************************************************\n")
    cat("\n")

    # ---- write script status to this point on a plot page

    win.graph()			# make a new graph window
    par(mfrow=c(1,1))		# makes plot on page appear in 1 rows of 1 column
    par(mar=c(5,4,4,1))       # sets margins of plot as the default values

   # ------ blank plot to provide space for text ------

   plot(0,xaxt="n",yaxt="n",bty="n",pch="",xlab="",ylab="")

   # ----- calculate some statistics and print on plot page -----

   plottext <- paste("CASTAWAY SERIAL Numebr:",INPUT.Device[2])
     mtext(plottext, adj=0, side=3, line= -1, family="sans", cex=0.8)

   plottext <- paste("CAST DATE and TIME (DMY HM in UTC):",INPUT.CastTimeUTC[2])
     mtext(plottext, adj=0, side=3, line= -2, family="sans", cex=0.8)

   plottext <- paste("CAST LOCATION START (Lat, Lon):",round(as.numeric(INPUT.StartLat[2]),7),"  ",
                                                       round(as.numeric(INPUT.StartLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -3, family="sans", cex=0.8)

   plottext <- paste("CAST LOCATION END     (Lat, Lon):",round(as.numeric(INPUT.EndLat[2]),7),"  ",
                                                         round(as.numeric(INPUT.EndLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -4, family="sans", cex=0.8)

   plottext <- paste("QC Checks on Locating CASTAWAY OUTPUT files")
     mtext(plottext, adj=0, side=3, line= -6, family="sans", font=3, cex=0.8)

   if (FileNameFlagg.R == "Tripped") {
       plottext <- paste("      RAW file:                         NOT read")
         mtext(plottext, adj=0, side=3, line= -7, family="sans", cex=0.8)
      } else {
       plottext <- paste("      RAW file:                             Read")
         mtext(plottext, adj=0, side=3, line= -7, family="sans", cex=0.8)
      } # end of if

   if (FileNameFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN CAST file:             NOT read")
         mtext(plottext, adj=0, side=3, line= -8, family="sans", cex=0.8)
      } else {
       plottext <- paste("      DOWN CAST file:                 Read")
         mtext(plottext, adj=0, side=3, line= -8, family="sans", cex=0.8)
      } # end of if

   if (FileNameFlagg.UP == "Tripped") {
       plottext <- paste("      UP CAST file:                  NOT read")
         mtext(plottext, adj=0, side=3, line= -9, family="sans", cex=0.8)
      } else {
       plottext <- paste("      UP CAST file:                       Read")
         mtext(plottext, adj=0, side=3, line= -9, family="sans", cex=0.8)
      } # end of if

   if (FileNameFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN and UP CAST file: NOT read")
         mtext(plottext, adj=0, side=3, line= -10, family="sans", cex=0.8)
      } else {
       plottext <- paste("      DOWN and UP CAST file:     Read")
         mtext(plottext, adj=0, side=3, line= -10, family="sans", cex=0.8)
      } # end of if

   if (FileNameFlagg.P == "Tripped") {
       plottext <- paste("      PROCESSED file:            NOT read")
         mtext(plottext, adj=0, side=3, line= -11, family="sans", cex=0.8)
      } else {
       plottext <- paste("      PROCESSED file:                Read")
         mtext(plottext, adj=0, side=3, line= -11, family="sans", cex=0.8)
      } # end of if

   title(main=paste("CASTAWAY DATA PROCESSING and QC LOG"),
      outer=T, line=-1,   cex.main=1,)
   title(main=paste("Cast Being Considered (Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-3, cex.main=1, font.main=3)

   # --- end of plot

    stop("Script has been terminated")
    } # end of if

# -----------------------------------------------------------------------------------------------
# ---- test to see if the imported data files contain the expected CASTAWAY processing types ----
# -----------------------------------------------------------------------------------------------

# ---- extract the processing type header information from the imported data files ----

INPUT.ProcessingType.R  <- scan(INPUT.DATA.FILE.NAME.R,  skip=5,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.DN <- scan(INPUT.DATA.FILE.NAME.DN, skip=5,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.UP <- scan(INPUT.DATA.FILE.NAME.UP, skip=5,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.DU <- scan(INPUT.DATA.FILE.NAME.DU, skip=5,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.P  <- scan(INPUT.DATA.FILE.NAME.P,  skip=5,  nlines=1, sep="\t", what="character")

    cat("********************************************************************************************\n")

    ProcessingTypeFlagg    <- "NotTripped"                          # set the flagg to the default value
    ProcessingTypeFlagg.R  <- "NotTripped"
    ProcessingTypeFlagg.DN <- "NotTripped"
    ProcessingTypeFlagg.UP <- "NotTripped"
    ProcessingTypeFlagg.DU <- "NotTripped"
    ProcessingTypeFlagg.P  <- "NotTripped"

if (INPUT.ProcessingType.R[2] != "Raw") {
    cat("!!!!! WARNING: Imported CASTAWAY RAW data file DOES NOT contain RAW data   \n")
    ProcessingTypeFlagg    <- "Tripped"
    ProcessingTypeFlagg.R  <- "Tripped"
    } # end of if
if (INPUT.ProcessingType.DN[2] != "Down") {
    cat("!!!!! WARNING: Imported CASTAWAY DOWN data file DOES NOT contain DOWN data  \n")
    ProcessingTypeFlagg    <- "Tripped"
    ProcessingTypeFlagg.DN <- "Tripped"
    } # end of if
if (INPUT.ProcessingType.UP[2] != "Up") {
    cat("!!!!! WARNING: Imported CASTAWAY UP data file DOES NOT contain UP data    \n")
    ProcessingTypeFlagg    <- "Tripped"
    ProcessingTypeFlagg.UP <- "Tripped"
    } # end of if
if (INPUT.ProcessingType.DU[2] != "Down & up") {
    cat("!!!!! WARNING: Imported CASTAWAY DOWN and UP data file DOES NOT contain Down and Up data   \n")
    ProcessingTypeFlagg    <- "Tripped"
    ProcessingTypeFlagg.DU <- "Tripped"
    } # end of if
if (INPUT.ProcessingType.P[2] != "Processed") {
    cat("!!!!! WARNING: Imported CASTAWAY PROCESSED data file DOES NOT contain PROCESSED data   \n")
    ProcessingTypeFlagg   <- "Tripped"
    ProcessingTypeFlagg.P <- "Tripped"
    } # end of if

if (ProcessingTypeFlagg == "NotTripped") {   
    cat("\n") 
    cat("      NOTE: All of the Input files contain the expected data processing type \n")
    cat("\n")
    cat("      ACTIONS: R Script will continue                                                       \n")
    cat("********************************************************************************************\n")
    cat("\n")

    # ---- write script status to this point on a plot page

    win.graph()			# make a new graph window
    par(mfrow=c(1,1))		# makes plot on page appear in 1 rows of 1 column
    par(mar=c(5,4,4,1))       # sets margins of plot as the default values

   # ------ blank plot to provide space for text ------

   plot(0,xaxt="n",yaxt="n",bty="n",pch="",xlab="",ylab="")

   # ----- calculate some statistics and print on plot page -----

   plottext <- paste("CASTAWAY SERIAL Numebr:",INPUT.Device[2])
     mtext(plottext, adj=0, side=3, line= -1, family="sans", cex=0.8)
   plottext <- paste("CAST DATE and TIME (DMY HM in UTC):",INPUT.CastTimeUTC[2])
     mtext(plottext, adj=0, side=3, line= -2, family="sans", cex=0.8)
   plottext <- paste("CAST LOCATION START (Lat, Lon):",round(as.numeric(INPUT.StartLat[2]),7),"  ",
                                                       round(as.numeric(INPUT.StartLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -3, family="sans", cex=0.8)
   plottext <- paste("CAST LOCATION END     (Lat, Lon):",round(as.numeric(INPUT.EndLat[2]),7),"  ",
                                                         round(as.numeric(INPUT.EndLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -4, family="sans", cex=0.8)

# ---- QC Checks on being able to locate and read files 

   plottext <- paste("QC Checks on Locating and Reading CASTAWAY OUTPUT files")
     mtext(plottext, adj=0, side=3, line= -6, family="sans", font=3, cex=0.8)

   if (ProcessingTypeFlagg.R == "Tripped") {
       plottext <- paste("      RAW file:                         Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -7, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      RAW file:                             Read")
         mtext(plottext, adj=0, side=3, line= -7, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN CAST file:             Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -8, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      DOWN CAST file:                 Read")
         mtext(plottext, adj=0, side=3, line= -8, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.UP == "Tripped") {
       plottext <- paste("      UP CAST file:                  Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -9, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      UP CAST file:                       Read")
         mtext(plottext, adj=0, side=3, line= -9, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN and UP CAST file: Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -10, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      DOWN and UP CAST file:     Read")
         mtext(plottext, adj=0, side=3, line= -10, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.P == "Tripped") {
       plottext <- paste("      PROCESSED file:            Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -11, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      PROCESSED file:                Read")
         mtext(plottext, adj=0, side=3, line= -11, family="sans",  cex=0.8, col="green")
      } # end of if

   title(main=paste("CASTAWAY DATA PROCESSING and QC LOG"),
      outer=T, line=-1,   cex.main=1,)
   title(main=paste("Cast Being Considered (Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-3, cex.main=1, font.main=3)

   # --- end of plot

    } # end of if

if (ProcessingTypeFlagg == "Tripped") {
    cat("\n")
    cat("      NOTE: One or more of the Input files do not contain the expected data processing type \n")
    cat("\n")
    cat("      ACTIONS: R Script has been terminated                                                 \n")
    cat("               Check file names and processing types and correct as appropriate             \n")
    cat("********************************************************************************************\n")
    cat("\n")

    # ---- write script status to this point on a plot page

    win.graph()			# make a new graph window
    par(mfrow=c(1,1))		# makes plot on page appear in 1 rows of 1 column
    par(mar=c(5,4,4,1))       # sets margins of plot as the default values

   # ------ blank plot to provide space for text ------

   plot(0,xaxt="n",yaxt="n",bty="n",pch="",xlab="",ylab="")

   # ----- calculate some statistics and print on plot page -----

   plottext <- paste("CASTAWAY SERIAL Numebr:",INPUT.Device[2])
     mtext(plottext, adj=0, side=3, line= -1, family="sans", cex=0.8)
   plottext <- paste("CAST DATE and TIME (DMY HM in UTC):",INPUT.CastTimeUTC[2])
     mtext(plottext, adj=0, side=3, line= -2, family="sans", cex=0.8)
   plottext <- paste("CAST LOCATION START (Lat, Lon):",round(as.numeric(INPUT.StartLat[2]),7),"  ",
                                                       round(as.numeric(INPUT.StartLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -3, family="sans", cex=0.8)
   plottext <- paste("CAST LOCATION END     (Lat, Lon):",round(as.numeric(INPUT.EndLat[2]),7),"  ",
                                                         round(as.numeric(INPUT.EndLon[2]),7))
     mtext(plottext, adj=0, side=3, line= -4, family="sans", cex=0.8)

   plottext <- paste("QC Checks on Locating and Reading CASTAWAY OUTPUT files")
     mtext(plottext, adj=0, side=3, line= -6, family="sans", font=3, cex=0.8)

   if (ProcessingTypeFlagg.R == "Tripped") {
       plottext <- paste("      RAW file:                         Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -7, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      RAW file:                             Read")
         mtext(plottext, adj=0, side=3, line= -7, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN CAST file:             Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -8, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      DOWN CAST file:                 Read")
         mtext(plottext, adj=0, side=3, line= -8, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.UP == "Tripped") {
       plottext <- paste("      UP CAST file:                  Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -9, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      UP CAST file:                       Read")
         mtext(plottext, adj=0, side=3, line= -9, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.DN == "Tripped") {
       plottext <- paste("      DOWN and UP CAST file: Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -10, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      DOWN and UP CAST file:     Read")
         mtext(plottext, adj=0, side=3, line= -10, family="sans",  cex=0.8, col="green")
      } # end of if

   if (ProcessingTypeFlagg.P == "Tripped") {
       plottext <- paste("      PROCESSED file:            Read, contains WRONG PROCESSING Type")
         mtext(plottext, adj=0, side=3, line= -11, family="sans",  cex=0.8, col="red")
      } else {
       plottext <- paste("      PROCESSED file:                Read")
         mtext(plottext, adj=0, side=3, line= -11, family="sans",  cex=0.8, col="green")
      } # end of if

   title(main=paste("CASTAWAY DATA PROCESSING and QC LOG"),
      outer=T, line=-1,   cex.main=1,)
   title(main=paste("Cast Being Considered (Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-3, cex.main=1, font.main=3)

   # --- end of plot

    stop("Script has been terminated")
    } # end of if

# ----------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# generate plots, summaries and some data QC related to the CASTAWAY RAW data file 
# --------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

cat("\n")
cat("This portion of the script plots, summarizes and conducts QC tests  \n")
cat("     on some aspects of the CASTAWAY RAW data file. \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue")
cat(" \n")

# ---------------------------------------------------------------------------
# replace the header names in the imported RAW data frame with shorter names
# ---------------------------------------------------------------------------

PROFILE.NAMES.R <- scan(INPUT.DATA.FILE.NAME.R, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile
names(INPUT.CASTAWAY.PROFILE.R) <- PROFILE.NAMES.R

if (PROFILE.NAMES.R[1]=="Time (Seconds)"                                     &
    PROFILE.NAMES.R[2]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.R[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.R[4]=="Conductivity (MicroSiemens per Centimeter)")         
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.R) <- c("Time_s","Pres_db","Temp_C","Cond_mScm")
   } else {
          stop("Column Headers and/or their order in the RAW data file DO NOT match expectations")
          } # end of if

cat("\n")
cat(" A CASTAWAY RAW data file is ready for plotting, analyses and QC tests. \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# ---- add QC check output to text plot window

    plottext <- paste("QC Range Checks on Contents of CASTAWAY RAW Output file")
       mtext(plottext, adj=0, side=3, line= -13, family="sans", font=3, cex=0.8)

# ---- determine the minimum and maximum time stamps in the data set

    Time_min <- min(INPUT.CASTAWAY.PROFILE.R$Time_s)
    Time_max <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)

    cat("\n")
    cat("  NOTE: The first time stamp in the data file is ", Time_min, "s \n")
    cat("        The last  time stamp in the data file is ", Time_max, "s \n")
    cat("\n")

# -----------------------------------------------------------------------------------
# ---- QC CHECK on the frequency of sampling
# ---- The CASTAWAY samples at a rate of 5 Hz ie takes measurements once every 0.2 s
# -----------------------------------------------------------------------------------

    # ---- calculate the rates of change in time

    Time_grad.R                  <- delta_y( INPUT.CASTAWAY.PROFILE.R$Time_s )
    Time_grad.R_min              <- round(min( Time_grad.R ),2)
    Time_grad.R_max              <- round(max( Time_grad.R ),2)
    Time_interval_center.R       <- avg_y( INPUT.CASTAWAY.PROFILE.R$Time_s )

    # ---- test to see if the rates of change meet expectations
    #if (  Time_grad.R_min == Time_grad.R_max) {cat("TEST \n")}
    #if (  Time_grad.R_min == -0.20) {cat("TEST \n")}
    
    if (  Time_grad.R_min == Time_grad.R_max
        & abs(Time_grad.R_min) == 0.2) {
    cat("\n")
    cat("               The time interval between records ranges from a minimum of", abs(Time_grad.R_min), "s \n")
    cat("                                                          to a maximum of", abs(Time_grad.R_max), "s \n")
    cat("\n")
    cat("      QC NOTE: The time steps in data file are equal to 0.2 seconds ie 5Hz. \n")
    cat("               This is the expected time step for a properly functioning CASTAWAY. \n")
    cat("\n")
    cat("      QC ACTIONS: No action needed \n") 
    cat("\n")  

    # ---- add QC check output to text plot window
      plottext <- paste("     Time Steps in the data file are equal to 0.2 seconds ie 5Hz.")
        mtext(plottext, adj=0, side=3, line= -14, family="sans", cex=0.8, col="green")
                
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

    if (  Time_grad.R_min == Time_grad.R_max
        & abs(Time_grad.R_min) != 0.2) {
    cat("\n")
    cat("               The time interval between records is", abs(Time_grad.R_min), "s \n")
    cat("\n")
    cat("      QC WARNING: The time steps in data file are equal but not 0.2 seconds ie 5Hz. \n")
    cat("                  A properly functioning CASTAWAY samples at 5Hz      \n")
    cat("\n")
    cat("      QC ACTIONS: Check the file and instrument \n") 
    cat("\n")
    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     Time Steps in the data file are NOT THE EXPECTED 0.2 seconds ie 5Hz.")
        mtext(plottext, adj=0, side=3, line= -14, family="sans", cex=0.8, col="red")
                  
    } # end of if

    # ---- test for missing records
    #      i.e. is the final time stamp as expected based on the number of records

    Number_of_Records_in_File <- length(INPUT.CASTAWAY.PROFILE.R$Time_s)
    Duration_of_File          <- round(0.2*Number_of_Records_in_File,2)

    if (  Time_max == Duration_of_File) {
    cat("\n")
    cat("               The number of records in the file is", Number_of_Records_in_File, "\n")
    cat("\n")
    cat("      QC Note: The last time step is as expected based on the number of records and the time step. \n")
    cat("               This is the expected time step for a properly functioning CASTAWAY. \n")
    cat("\n")
    cat("      QC ACTIONS: No action needed \n") 
    cat("\n")

    # ---- add QC check output to text plot window
      plottext <- paste("     File appears to contain the expected number of records.")
        mtext(plottext, adj=0, side=3, line= -15, family="sans",  cex=0.8, col="green")
                  
    } # end of if

    if (Time_grad.R_min != Time_grad.R_max) {
    cat("\n")
    cat("               The time interval between records ranges from a minimum of", abs(Time_grad.R_min), "s \n")
    cat("                                                          to a maximum of", abs(Time_grad.R_max), "s \n")
    cat("\n")
    cat("      QC WARNING: The data file contains a range of time steps. \n")
    cat("                  The minimum and maximum time steps are not equal. \n")
    cat("                  The data file likely contains missing records or \n") 
    cat("                      the instrument has a low battery or clock problem. \n")
    cat("\n")
    cat("      QC ACTIONS: Check the file \n") 
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     File appears to contain missing records.")
        mtext(plottext, adj=0, side=3, line= -15, family="sans",  cex=0.8, col="red")
                        
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

# -----------------------------------------------------------------------------
# ---- Conduct Range checks on the RAW pressurre, temperature and conductivity
# -----------------------------------------------------------------------------

# ---- QC RANGE CHECK: pressure
#
#      According to the CASTAWAY manual the CASTAWAY should not be deployed to depths greater than 100 db
#      Note: For division of a CASTWAY Cast into up and down casts, the CASTAWAY manual defines the 
#                maximum pressure as the maximum pressure derived from data records with a CASTAWAY 
#                sinking rate of 0.05 db/s or greater. This range check is only concerned with the maximum
#                pressure no matter what the sinking rate is because exceedence of the pressure max could
#                damage the pressure sensor. 

    Pres_max     <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db)
    Pres_max_row <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max) 

    if (Pres_max < 100.0) {
    cat("\n")
    cat("               The maximum pressure recorded is", Pres_max, "db \n")
    cat("\n")
    cat("      QC Note: The maximum pressure does not exceed 100 db. \n")
    cat("               The CASTAWAY pressure maximum has not been exceeded. \n")
    cat("\n")
    cat("      QC ACTIONS: None. \n")
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     Pressure maximum does not exceed the instrument limit of 100 db.")
        mtext(plottext, adj=0, side=3, line= -16, family="sans",  cex=0.8, col="green") 
                       
    } # end of if

    if (Pres_max >= 100.0) {
    cat("\n")
    cat("               The maximum pressure recorded is", Pres_max, "db \n")
    cat("\n")
    cat("      QC WARNING: The maximum pressure exceeds 100 db. \n")
    cat("                  The CASTAWAY could be damaged. \n")
    cat("\n")
    cat("      QC ACTIONS: Have the instrument serviced. \n")
    cat("                  Only the downcast data in this file should be used. \n") 
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     WARNING: The maximum pressure EXCEEDS the instrument limit of 100 db.")
        mtext(plottext, adj=0, side=3, line= -16, family="sans", cex=0.8, col="red")
                        
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")
 
# ---- QC RANGE CHECK: temperature

    Temp_min <- min(INPUT.CASTAWAY.PROFILE.R$Temp_C)
    Temp_max <- max(INPUT.CASTAWAY.PROFILE.R$Temp_C)

    if (  Temp_min > -5
        & Temp_max <  45 ) {
    cat("\n")
    cat("               The minimum temperature recorded is", Temp_min, "C \n")
    cat("               The maximum temperature recorded is", Temp_max, "C \n")
    cat("\n")
    cat("      QC Note: The temperatures are within the instrument specification range. \n")
    cat("               The CASTAWAY specifies a temperature operating range of -5 to 45 C. \n")
    cat("               The data file contains in air and in water temperatures. \n")
    cat("\n")
    cat("      QC ACTIONS: None. \n")
    cat("   \n")  

    # ---- add QC check output to text plot window
      plottext <- paste("     Temperature minimum and maximum are within the instrument's operational range.")
        mtext(plottext, adj=0, side=3, line= -17, family="sans", cex=0.8, col="green")
                      
    } # end of if

    if (  Temp_min < -5
        & Temp_max >  45 ) {
    cat("\n")
    cat("               The minimum temperature recorded is", Temp_min, "C \n")
    cat("               The maximum temperature recorded is", Temp_max, "C \n")
    cat("\n")
    cat("      QC WARNING: The temperatures are NOT WITHIN the instrument specification range. \n")
    cat("                  The CASTAWAY specifies a temperature operating range of -5 to 45C. \n")
    cat("                  The data file contains in air and in water temperatures. \n")
    cat("\n")
    cat("      QC ACTIONS: Instrument should be sent for recalibration \n")
    cat("                  Thermistor may be damaged. \n")
    cat("   \n") 

    # ---- add QC check output to text plot window
      plottext <- paste("     WARNING: Temperature values are OUTSIDE the instrument's operational range.")
        mtext(plottext, adj=0, side=3, line= -17, family="sans", cex=0.8, col="red")
                       
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

# ---- QC RANGE CHECK: conductivity

    Cond_min <- min(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)
    Cond_max <- max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)

    if (  Cond_min > 0
        & Cond_max < 40000 ) {
    cat("\n")
    cat("               The minimum conductivity recorded is", Cond_min, "mS/cm \n")
    cat("               The maximum conductivity recorded is", Cond_max, "mS/cm \n")
    cat("\n")
    cat("      QC Note: The conductivities are within the instrument specification range. \n")
    cat("               The CASTAWAY specifies a conductivity operating range of to mS/cm. \n")
    cat("               The data file contains in air and in water temperatures. \n")
    cat("\n")
    cat("      QC ACTIONS: None. \n")
    cat("   \n")  

    # ---- add QC check output to text plot window
      plottext <- paste("     Conductivities are within the instrument specification range.")
        mtext(plottext, adj=0, side=3, line= -18, family="sans", cex=0.8, col="green")
                      
    } # end of if

    if (  Cond_min < 0
        & Cond_max > 40000 ) {
    cat("\n")
    cat("               The minimum conductivity recorded is", Cond_min, "mS/cm \n")
    cat("               The maximum conductivity recorded is", Cond_max, "mS/cm \n")
    cat("\n")
    cat("      QC WARNING: The conductivities are NOT WITHIN the instrument specification range. \n")
    cat("                  The CASTAWAY specifies a conductivity operating range of to mS/cm. \n")
    cat("                  The data file contains in air and in water temperatures. \n")
    cat("\n")
    cat("      QC ACTIONS: Instrument should be sent for recalibration. \n")
    cat("                  Conductivity cell may be damaged. \n")
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     WARNING: Conductivities are OUTSIDE the instrument specification range.")
        mtext(plottext, adj=0, side=3, line= -18, family="sans", cex=0.8, col="red")
                        
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

# -----------------------------------------------------
# derive the times of some features in the time series
# -----------------------------------------------------

# ---- find when the CASTAWAY enters the water and the time at which this occurs ----
#      Note: the entry time is indicated by the increase in conductivity to a value
#            greater than 10,000.  

    Enters_Water_row  <- min(which(INPUT.CASTAWAY.PROFILE.R$Cond_mS > 10000))  # this selects the first row with
                                                                               # a value > 10000. This value 
                                                                               # indicates the instrument is in the
                                                                               # the water. 
    Enters_Water_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Enters_Water_row]

# ---------------------------------------------------------------------------------------------
# ---- QC RANGE CHECK: conductivity continued
#                      conductivity before entering the water should be less than 500
#                      conductivity in a damp cell can be > 0
#                      Source: CASTAWAY manual
# ---------------------------------------------------------------------------------------------

    Cond_air_max <- max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[1:Enters_Water_row- 1])  # The Enters_Water_Row is the first row
                                                                                    # in which the instrument is in the
                                                                                    # the water. The minus one restricts 
                                                                                    # the data selection to the in air
                                                                                    # values only.                                                                                      

    if (  Cond_air_max < 500) {
    cat("\n")
    cat("               The maximum conductivity recorded prior to entering water is", Cond_air_max, "mS/cm \n")
    cat("\n")
    cat("      QC Note: The in air conductivities are within the instruments specification range. \n")
    cat("               The CASTAWAY specifies an in air dry conductivity operating range of <500 mS/cm. \n")
    cat("\n")
    cat("      QC ACTIONS: None. \n")
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     Conductivities in air are within the instruments specification range.")
        mtext(plottext, adj=0, side=3, line= -19, family="sans", cex=0.8, col="green")
                        
    } # end of if

    if (  Cond_air_max > 500) {
    cat("\n")
    cat("               The maximum conductivity recorded prior to entering water is", Cond_air_max, "mS/cm \n")
    cat("\n")
    cat("      QC WARNING: The conductivities in air are NOT WITHIN the instrument specification range. \n")
    cat("                  The CASTAWAY specifies a conductivity operating range within air of < 500 mS/cm. \n")
    cat("\n")
    cat("      QC ACTIONS: Instrument conductivity cell should be checked. \n")
    cat("                  Conductivity cell may be blocked or damaged. \n")
    cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     Conductivities in air are NOT WITHIN the instruments specification range.")
        mtext(plottext, adj=0, side=3, line= -19, family="sans",  cex=0.8, col="red")
                        
    } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

    # ---- Note: the CASTAWAY manual recommeds a soak time of 5-10 seconds

      Pres_grad.R  <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
    
    # ---- select the down cast minus last ten rows so reductions in descent speed near the bottom are not included

      Pres_grad.R.DN  <- Pres_grad.R[Enters_Water_row:Pres_max_row] 

    # ---- identify the row ontaining the first descent rate that is greater than -0.5 db/s

      First_Row_Pres_grad_LT_Thres <- min(which(Pres_grad.R.DN < -0.5))

      Downcast_begin_time <- DNCAST_Begin_Time(INPUT.CASTAWAY.PROFILE.R)

      Soak_Time_min_CAST_Rec     <- 5    
      Soak_Time_max_CAST_Rec     <- 10
      Soak_Time_actual           <- Downcast_begin_time - Enters_Water_time

      if ( Soak_Time_actual < Soak_Time_max_CAST_Rec) {
      cat("\n")
      cat("               The estimated soak time is",Soak_Time_actual,"sec \n")
      cat("\n")
      cat("      QC WARNING: The estimated soak time is LESS THAN RECOMMENDED. \n")
      cat("                  The CASTAWAY specifies an estimated soak time of",Soak_Time_max_CAST_Rec,"sec. \n")
      cat("                  The recommended soak time may not be sufficient if \n") 
      cat("                      near surface values are changing rapidly with depth. \n")
      cat("                  The near surface portions of the downcast profile should be considered of low quality. \n")
      cat("\n")
      cat("      QC ACTIONS: Examine the DOWN, UP and PROCESSED profiles carefully. \n")
      cat("                  Consider using the UP CAST only. \n")
      cat("                  Consider discarding the entire CAST. \n")
      cat("   \n") 

    # ---- add QC check output to text plot window
      plottext <- paste("     WARNING: Soak Time",Soak_Time_actual,"is LESS THAN the RECOMMENDED.",Soak_Time_max_CAST_Rec,"s")
        mtext(plottext, adj=0, side=3, line= -20, family="sans",  cex=0.8, col="red")
                       
      } # end of if

      if ( Soak_Time_actual > Soak_Time_max_CAST_Rec) {
      cat("\n")
      cat("               The estimated soak time is",Soak_Time_actual,"sec \n")
      cat("\n")
      cat("      QC WARNING: The estimated soak time is GREATER THAN RECOMMENDED. \n")
      cat("                  The CASTAWAY specifies an estimated soak time of",Soak_Time_max_CAST_Rec,". \n")
      cat("\n")
      cat("      QC ACTIONS: The shallow portions of the downcast profile may be of good quality.\n")
      cat("                  Examine the DOWN, UP and PROCESSED profiles for near surface gradients. \n")
      cat("   \n")

    # ---- add QC check output to text plot window
      plottext <- paste("     Soak Time is",Soak_Time_actual,"s and is greater than the recommended",Soak_Time_max_CAST_Rec,"s")
        mtext(plottext, adj=0, side=3, line= -20, family="sans", cex=0.8, col="green")
                        
      } # end of if

    cat("\n")
    option_Y_N <- readline("PRESS ENTER to continue ")
    cat(" \n")

# ======================================================================================
# ------------------------------------------------------------------------------------
# plot the time series of RAW Pressure, Temperature and Conductivity data on one page 
#      page layout includes:
#                            panel of text, 
#                            panel showing pressure time series
#                            panel showing temperature time series
#                            panel showing conductivity time series
# ------------------------------------------------------------------------------------
# ======================================================================================

win.graph()			# make a new graph window
par(mfrow=c(4,1))		# makes plots on page appear in 3 rows of 1 column
par(mar=c(5,4,4,1))

# ------ blank plot to provide space for text ------

plot(0,xaxt="n",yaxt="n",bty="n",pch="",xlab="",ylab="")

# ----- calculate statistics for the down and up cast data and print on plot page -----

# --- select the rows bracketting the downcast and up cast

start_row  <- which(INPUT.CASTAWAY.PROFILE.R$Time_s == Downcast_begin_time)
end_row    <- max(which(INPUT.CASTAWAY.PROFILE.R$Cond_mS > 10000))
 
plottext <- paste("                                              Statistics based on Down and Up Cast data")
mtext(plottext, adj=0, side=3, line= -1, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("                                 Time (s)        Press (db)        Temp (C)         Cond (mS/cm)")
mtext(plottext, adj=0, side=3, line= -2, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   n                              ",
                                length(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),"                ",
                                length(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),"                ",
                                length(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]) ,"                   ",
                                length(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]))
mtext(plottext, adj=0, side=3, line= -4, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   minimum                   ",
                                min(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),   "                 ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),3),"             ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]),2),"                  ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000),2))
mtext(plottext, adj=0, side=3, line= -5, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   maximum                  ",
                                max(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),   "             ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),3),"             ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]),2),"                ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000),2))
mtext(plottext, adj=0, side=3, line= -6, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   minimum abs(delta)    ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]))),2),   "               ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]))),3),"                 ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]))),2),"                     ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000))),2))
mtext(plottext, adj=0, side=3, line= -8, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   maximum abs(delta)   ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]))),2),   "               ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]))),3),"               ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]))),2),"                 ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000))),2))
mtext(plottext, adj=0, side=3, line= -9, 	      
	family="sans", font=3, cex=0.8)

# --- end of text plot
 
    Pres_max_CASTAWAY <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     Pres_max_CASTAWAY_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max_CASTAWAY)
     Pres_max_CASTAWAY_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_CASTAWAY_row] 

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]


# ------ plot of pressure time series ------

par(mar=c(4,5,2,1))     # plot margins c(bottom,left,top,right)

plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="n",col="black",lwd=3,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-6,6),
     #xlab="n",
     xlab="Time (s)",
     ylab="Pressure (db)",
     cex.lab=1.5,
     las=1)
     box(col="black", lwd=2)

     # ---- add a polygon indicating the time in which the CASTAWAY is in the water ----

     px <- c(Enters_Water_time,    Enters_Water_time, 
             Exits_Water_time, Exits_Water_time,
             Enters_Water_time) 
     py <- c(-Pres_max-6,0,0,-Pres_max-6,
             -Pres_max-6)
     polygon(px,py,border=NA,col="light blue")

     # ---- add a line for the sea surface -----

     lines(c(-5,max(INPUT.CASTAWAY.PROFILE.R$Time_s)+5),c(0,0),lty="dotted",col="blue",lwd=1)

     # ---- add an arrow on the plot indicating the time of entry into the water ----

     arrows( Enters_Water_time,2, Enters_Water_time, -Pres_max-6, code=2, col="blue", lwd=2, length=0.15)
     text(Enters_Water_time,5,"Water Entry", col="blue",cex=1)

     # add a line indicating the recommended soak time range

     #lines( c(Enters_Water_time + Soak_Time_min_CAST_Rec, Enters_Water_time + Soak_Time_min_CAST_Rec),
     #       c(2, -Pres_max-6), 
     #       code=2, col="light blue", lwd=2, lty="dashed", length=0.15)
     lines( c(Enters_Water_time + Soak_Time_max_CAST_Rec, Enters_Water_time + Soak_Time_max_CAST_Rec),
            c(2, -Pres_max-6), 
            code=2, col="blue", lwd=2, lty="dashed", length=0.15)
     Soak_Time_Mean_Recommended <- (5+10)/2
     arrows(Enters_Water_time,(0 + -Pres_max-6)/2,
            Enters_Water_time + Soak_Time_max_CAST_Rec,(0 + -Pres_max-6)/2,
            code=3,col="blue",lwd=2,length=0.15)
     text(Enters_Water_time + Soak_Time_min_CAST_Rec,
          (0 + -Pres_max-6)/2+11,"Minimum", col="blue",cex=1)
     text(Enters_Water_time + Soak_Time_min_CAST_Rec,
          (0 + -Pres_max-6)/2+5,"Soak", col="blue",cex=1)
     text(Enters_Water_time + Soak_Time_min_CAST_Rec,
          (0 + -Pres_max-6)/2-3,"Time", col="blue",cex=1)

     # ---- add an arrow on the plot indicating the time at which downcast begins

     arrows( Downcast_begin_time,2,Downcast_begin_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Downcast_begin_time,5,"DN Begins", col="red",cex=1)

     # add an arrow on the plot indicating the time of maximum pressure

     arrows( Pres_max_time,2,Pres_max_time, -Pres_max-6, code=2, col="grey", lwd=2, length=0.15)
     text(Pres_max_time,5,"Press Max", col="grey",cex=1)

     # ---- add an arrow on the plot indicating the time of exit from the water ----

     arrows( Exits_Water_time,2, Exits_Water_time, -Pres_max-6, 
             code=1, col="red", lwd=2, length=0.15)
     text(Exits_Water_time,5,"UP Ends & Water Exit", col="red",cex=1)

     # ---- add the pressure time series on top of the other info ----

     lines(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,col="black",lwd=2)

# ------ plot of temperature time series ------

par(mar=c(4,5,2,1))     # plot margins c(bottom,left,top,right)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,
     type="n",col="black",lwd=3,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Temp (C)",
     cex.lab=1.5,
     las=1)
     box(col="black", lwd=2)

     # ---- add a polygon indicating the recommended soak time ----

       #px <- c(Enters_Water_time,Enters_Water_time,Enters_Water_time+10,Enters_Water_time+10,Enters_Water_time) 
       #py <- c(min(INPUT.CASTAWAY.PROFILE.R$Temp_C), max(INPUT.CASTAWAY.PROFILE.R$Temp_C),
       #        max(INPUT.CASTAWAY.PROFILE.R$Temp_C), min(INPUT.CASTAWAY.PROFILE.R$Temp_C),
       #        min(INPUT.CASTAWAY.PROFILE.R$Temp_C))
       #polygon(px,py,border=NA,col="light blue")

     # ---- add a polygon indicating the time in which the CASTAWAY is in the water ----

       px <- c(Enters_Water_time,    Enters_Water_time, 
               Exits_Water_time, Exits_Water_time,
               Enters_Water_time) 
       py <- c(min(INPUT.CASTAWAY.PROFILE.R$Temp_C), max(INPUT.CASTAWAY.PROFILE.R$Temp_C),
               max(INPUT.CASTAWAY.PROFILE.R$Temp_C), min(INPUT.CASTAWAY.PROFILE.R$Temp_C),
               min(INPUT.CASTAWAY.PROFILE.R$Temp_C))
     polygon(px,py,border=NA,col="light blue")

     # ---- add an arrow on the plot indicating the time of entry into the water ----

       arrows( Enters_Water_time,max(INPUT.CASTAWAY.PROFILE.R$Temp_C), Enters_Water_time, min(INPUT.CASTAWAY.PROFILE.R$Temp_C), code=2, col="blue", lwd=2, length=0.15)

     # ---- add an arrow indicating the end of the recommended soak time

       lines( c(Enters_Water_time + Soak_Time_max_CAST_Rec, Enters_Water_time + Soak_Time_max_CAST_Rec),
              c(min(INPUT.CASTAWAY.PROFILE.R$Temp_C), max(INPUT.CASTAWAY.PROFILE.R$Temp_C)), 
              code=2, col="blue", lwd=2, lty="dashed", length=0.15)

     # ---- add an arrow on the plot indicating the time at which downcast begins

       arrows( Downcast_begin_time, min(INPUT.CASTAWAY.PROFILE.R$Temp_C),
               Downcast_begin_time, max(INPUT.CASTAWAY.PROFILE.R$Temp_C), 
               code=1, col="red", lwd=2, length=0.15)

     # add an arrow on the plot indicating the time of maximum pressure

       arrows( Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Temp_C),Pres_max_time, min(INPUT.CASTAWAY.PROFILE.R$Temp_C), code=2, col="grey", lwd=2, length=0.15)

     # ---- add an arrow on the plot indicating the time of exit from the water ----

       arrows( Exits_Water_time,max(INPUT.CASTAWAY.PROFILE.R$Temp_C), 
               Exits_Water_time, min(INPUT.CASTAWAY.PROFILE.R$Temp_C), 
               code=1, col="red", lwd=2, length=0.15)

     # ---- add the time series of water temperature ----

       lines(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,col="black",lwd=2)

# ------ plot of conductivity time series ------

par(mar=c(4,5,2,1))     # plot margins c(bottom,left,top,right)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,
     type="n",col="black",lwd=3,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Cond (mS/1000)",
     cex.lab=1.5,
     las=1)
     box(col="black", lwd=2)

     # ---- add a polygon indicating the recommended soak time ----

       #px <- c(Enters_Water_time,Enters_Water_time,Enters_Water_time+10,Enters_Water_time+10,Enters_Water_time) 
       #py <- c(min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000), max(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000),
       #        max(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000), min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000),
       #        min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000))
       #polygon(px,py,border=NA,col="light blue")

     # ---- add a polygon indicating the time in which the CASTAWAY is in the water ----

       px <- c(Enters_Water_time,    Enters_Water_time, 
               Exits_Water_time, Exits_Water_time,
               Enters_Water_time) 
       py <- c(min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000), max(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000),
               max(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000), min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000),
               min(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000))
       polygon(px,py,border=NA,col="light blue")

     # ---- add an arrow on the plot indicating the time of entry into the water ----

       arrows( Enters_Water_time, max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000, 
               Enters_Water_time, 0, code=2, col="blue", lwd=2, length=0.15)

     # ---- add an arrow indicating the end of the recommended soak time

       lines( c(Enters_Water_time + Soak_Time_max_CAST_Rec, Enters_Water_time + Soak_Time_max_CAST_Rec),
              c(min(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000, max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000), 
              code=2, col="blue", lwd=2, lty="dashed", length=0.15)

     # ---- add an arrow on the plot indicating the time at which downcast begins

       arrows( Downcast_begin_time, min(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000,
               Downcast_begin_time, max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000, 
               code=1, col="red", lwd=2, length=0.15)

     # add an arrow on the plot indicating the time of maximum pressure

       arrows( Pres_max_time, max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000,
               Pres_max_time, 0, 
               code=2, col="grey", lwd=2, length=0.15)

     # ---- add an arrow on the plot indicating the time of exit from the water ----

       arrows( Exits_Water_time, max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm)/1000, 
               Exits_Water_time, 0, 
               code=1, col="red", lwd=2, length=0.15)

     # ---- add horizontal arrows indicating when CASTAWAY is in air and in water

       arrows(0,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="grey", lwd=2, length=0.15)
       text((0+Enters_Water_time)/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in air",col="grey",cex=1.1)
 
       arrows(Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="blue", lwd=2, length=0.15)
       text((0+max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in water",col="blue",cex=1.1)

       arrows(Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              max(INPUT.CASTAWAY.PROFILE.R$Time_s),(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="grey", lwd=2, length=0.15)
       text((Exits_Water_time + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in air",col="grey",cex=1.1)

     # ---- add time series of conductivity ----

       lines(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,col="black",lwd=2)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.3,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# =============================================================================
# ----------------------------------------------------------------------------
# plot the time series of RAW Pres,Temp and Cond data on one page
#      3 panels
#      - top panel contains text
#      - middle panel contains time series of pressure and pressure gradient
#      - lower panel temperature and conductivity time series
# ----------------------------------------------------------------------------
# =============================================================================

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue plotting the time series of RAW T,C & P & dP")
cat(" \n")

win.graph()			# make a new graph window

# ------ Middle Panel: pressure time series ------

par(fig=c(0,1,0.4,0.8), mar=c(1,4,5,4)+0.1)            # mar=c(bottom,left,top,right)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="l",col="black",lwd=3,
     xlab="",
     #xaxt="n",
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     #xlab="Time (s)",
     ylab="Pres (db)", col.lab="black",
     las=1)
     box(col="black", lwd=2)

     # ---- find when the CASTAWAY enters the water and the time at which this occurs ----
     #      Note: the entry time is indicated by the increase in conductivity to a value
     #            greater than 0.  

     Enters_Water_row  <- min(which(INPUT.CASTAWAY.PROFILE.R$Cond_mS > 10000))
     Enters_Water_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Enters_Water_row] 

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     # ---- add a polygon indicating the time in which the CASTAWAY is in the water ----

       px <- c(Enters_Water_time,    Enters_Water_time, 
               Exits_Water_time, Exits_Water_time,
               Enters_Water_time) 
       py <- c(-Pres_max-6,0,0,-Pres_max-6,
               -Pres_max-6)
       polygon(px,py,border=NA,col="light blue")

     # ---- replot the pressure time series to bring it to the top ----

       lines(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,col="black",lwd=2)

     # ---- add an arrow on the plot indicating the time of entry into the water ----

       arrows( Enters_Water_time,2, Enters_Water_time, -Pres_max-6, code=2, col="blue", lwd=2, length=0.15)
       text(Enters_Water_time,3.5,"Water Entry", col="blue",cex=0.7)

     # ---- find the beginning of the DOWNCAST and the time at which this occurs ----
 
     Soak_Time_Min <- 5                              # units seconds
     Estimated_Soak_end_time     <- Enters_Water_time + Soak_Time_Min
     Estimated_Soak_end_time_row <- which(INPUT.CASTAWAY.PROFILE.R$Time_s == Estimated_Soak_end_time)
                                                     # the CASTAWAY collects RAW data every 0.2 sec (ie
                                                     # 5 recordsper second) and recommends an initial
                                                     # soak time of at least 5-10 seconds. 
                                                     # This corresponds to 25 to 50 records.

     Pres_grad.R            <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     
     Pres_grad.R.DN               <- Pres_grad.R[Estimated_Soak_end_time_row:Pres_max_row]
     First_Row_Pres_grad_LT_Thres <- min(which(Pres_grad.R.DN < -0.5))
     Downcast_Begin_row           <- Estimated_Soak_end_time_row + First_Row_Pres_grad_LT_Thres
     Downcast_begin_time          <- INPUT.CASTAWAY.PROFILE.R$Time_s[Downcast_Begin_row]
 
     # add an arrow on the plot indicating the time at which downcast begins

     arrows( Downcast_begin_time,2,Downcast_begin_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Downcast_begin_time,3.5,"Begin DN", col="red",cex=0.7)

     # ---- find the end of the DOWNCAST and the time at which this occurs ----

     Pres_max      <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     # add an arrow on the plot indicating the time of maximum pressure

     arrows( Pres_max_time,2,Pres_max_time, -Pres_max-6, code=2, col="black", lwd=2, length=0.15)
     text(Pres_max_time,3.5,"Press Max", col="black",cex=0.7)

     # add an arrow on the plot indicating the time of exit from the water

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     arrows( Exits_Water_time,2,Exits_Water_time, -Pres_max-6, code=1, col="blue", lwd=2, length=0.15)
     text(Exits_Water_time,3.5,"End UP & Water Exit", col="red",cex=0.7)

  
# ------ Middle Panel: rate of change in pressure time series ------

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)

par(new=T)
plot(Time_interval_center.R, Pres_grad.R,
     type="l",col="grey",lwd=1,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="",
     #ylab="Pres Grad (db/s)",
     las=1)
     axis(side=4,las=1,col.axis="grey")
     mtext(side=4,line=2.5,"Pres Grad (db/s)",col="dark grey")
     box(col="black", lwd=2)

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0),lwd=2,col="grey",lty="dashed")

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(1,1),lwd=1,col="grey",lty="dotted")
     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-1,-1),lwd=1,col="grey",lty="dotted")

     points(Time_interval_center.R, Pres_grad.R,pch=16,cex=0.5,col="dark grey")


# ----- Top Panel: calculate some statistics and print on plot page -----

plottext <- paste("                                              Statistics based on Down and Up Cast data")
mtext(plottext, adj=0, side=3, line= 8, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("                                   Time (s)        Press (db)         Temp (C)         Cond (mS/cm)")
mtext(plottext, adj=0, side=3, line= 7, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   n                                ",
                                length(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),"                ",
                                length(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),"                ",
                                length(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]) ,"                   ",
                                length(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]))
mtext(plottext, adj=0, side=3, line= 6, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   minimum                     ",
                                min(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),   "                 ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),3),"           ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]),2),"                  ",
                                round(min(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000),2))
mtext(plottext, adj=0, side=3, line= 5, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   maximum                    ",
                                max(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]),   "               ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]),3),"           ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]),2),"                ",
                                round(max(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000),2))
mtext(plottext, adj=0, side=3, line= 4, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   minimum abs(delta)     ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]))),2),   "                ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]))),3),"                     ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]))),2),"                      ",
                                round(min(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000))),2))
mtext(plottext, adj=0, side=3, line= 3, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   maximum abs(delta)    ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s[start_row:end_row]))),2),   "                ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Pres_db[start_row:end_row]))),3),"          ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Temp_C[start_row:end_row]))),2),"                 ",
                                round(max(abs(delta_y(INPUT.CASTAWAY.PROFILE.R$Cond_mScm[start_row:end_row]/1000))),2))
mtext(plottext, adj=0, side=3, line= 2, 	      
	family="sans", font=3, cex=0.8)

plottext <- paste("   maximum descent(ascent) speed: ",
                                round(min(Pres_grad.R),2),   "(",
                                round(max(Pres_grad.R),2),")"
                  )
mtext(plottext, adj=0, side=3, line= 1, 	      
	family="sans", font=3, cex=0.8)

# ------ Bottom panel: temperature time series ------

par(fig=c(0,1,0,0.4),mar=c(4,4,2,4)+0.1, new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,
     type="l",col="red",lwd=2,
     #xaxt="n",
     #axes=F,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     #ylab="Temp (C)", 
     ylab="",
     #col.lab="red",
     col.axis="red",
     las=1)
     #axis(2,las=1)
     mtext(side=2,line=2.5,"Temp (C)", col="red")
     box(col="black", lwd=2)

# ------ Bottom Panel: conductivity time series ------

par(new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,round(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,2),
     type="l",col="blue",lwd=2,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="",
     #xlab="Time (s)",
     ylab="",
     las=1)
     axis(4,las=1,col.axis="blue")
     mtext(side=4,line=2.5,"Cond (mS)",col="blue")
     box(col="black", lwd=2)

     time_duration <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)
     time_midway   <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)/2

     # ---- add horizontal arrows indicating when CASTAWAY is in air and in water

       arrows(0,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="grey", lwd=2, length=0.15)
       text((0+Enters_Water_time)/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in air",col="grey",cex=1.1)
 
       arrows(Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="blue", lwd=2, length=0.15)
       text((0+max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in water",col="blue",cex=1.1)

       arrows(Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              max(INPUT.CASTAWAY.PROFILE.R$Time_s),(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
              code=3, col="grey", lwd=2, length=0.15)
       text((Exits_Water_time + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
            (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+4,"in air",col="grey",cex=1.1)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.1,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.25, cex.main=0.9, font.main=3)

# -------------------------------------------------
# ---- QC Comparison of RAW DOWN and UP Cast data
# -------------------------------------------------

# --- interpolate up cast data to down cast pressures

# --- plot vertical profiles of up and down cast data
#          i.e. x = down temperature and up temperature
#               y = pressure associated with down and up data separately
#               
#          i.e. x = down temperature and up temperature 
#               y = pressure up data intrpolated to down pressures
#
#          i.e. x = down temperature and up temperature interpolated to constant pressure increments
#               y = pressure interpolted to constant pressure increments         



# ----------------------------------------------------------------
# plot the time series of RAW Pres,Temp and Cond data on one page
#      2 panels
#      - top panel pressure and pressure gradient time series
#      - lower panel temperature and conductivity time series
# ----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,2))		# makes plots on page appear in 1 row of 2 columns 

# plot down cast temperature profile

plot(INPUT.CASTAWAY.PROFILE.R$Temp_C[Downcast_Begin_row:Pres_max_row],
    -INPUT.CASTAWAY.PROFILE.R$Pres_db[Downcast_Begin_row:Pres_max_row],
     pch=16,col="red",cex=0.5,
     xlab="Temp (C)",
     ylab="Pressure (db)",
     las=1
     )

# overlay up cast temperature profile ontop of downcast profile

points(INPUT.CASTAWAY.PROFILE.R$Temp_C[Pres_max_row:(length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row)+1],
      -INPUT.CASTAWAY.PROFILE.R$Pres_db[Pres_max_row:(length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row)+1],
       pch=16,col="blue",cex=0.5
       )

text(max(INPUT.CASTAWAY.PROFILE.R$Temp_C[Downcast_Begin_row:(length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row)+1]),
     -max(INPUT.CASTAWAY.PROFILE.R$Pres_db[Downcast_Begin_row:(length(INPUT.CASTAWAY.PROFILE.R$Pres_db)-Exits_Water_row)+1]),
     "n downcast =",length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row+1 - Pres_max_row)

# right hand panel: depth profile of difference between up and down cast

T_UP_Interp <- approx(INPUT.CASTAWAY.PROFILE.R$Pres_db[Pres_max_row:(length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row)+1],
                      INPUT.CASTAWAY.PROFILE.R$Temp_C[Pres_max_row:(length(INPUT.CASTAWAY.PROFILE.R$Temp_C)-Exits_Water_row)+1],
                      INPUT.CASTAWAY.PROFILE.R$Pres_db[Downcast_Begin_row:Pres_max_row],rule=2
                      )

T_Diff <- INPUT.CASTAWAY.PROFILE.R$Temp_C[Downcast_Begin_row:Pres_max_row] - T_UP_Interp$y

plot(T_Diff,
    -INPUT.CASTAWAY.PROFILE.R$Pres_db[Downcast_Begin_row:Pres_max_row],
     pch=16,col="red",cex=0.5,
     xlab="Tdn - Tup (C)",
     ylab="Pressure (db)",
     xlim=c(-max(abs(T_Diff)),max(abs(T_Diff))),
     las=1
     )

lines(c(0,0), c(0,-max(INPUT.CASTAWAY.PROFILE.R$Pres_db[Downcast_Begin_row:Pres_max_row])-2) ) 

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.25,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# beginning of old plot; erase when done

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue plotting the time series of RAW T,C & P & dP")
cat(" \n")

win.graph()			# make a new graph window

# ------ blank plot to provide space for text ------

plot(0,xaxt="n",yaxt="n",bty="n",pch="",xlab="",ylab="")


# ------ pressure time series ------

par(fig=c(0,1,0.5,1), mar=c(2,4,5,4)+0.1)            # mar=c(bottom,left,top,right)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="l",col="black",lwd=3,
     xlab="",
     #xaxt="n",
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     #xlab="Time (s)",
     ylab="Pres (db)", col.lab="black",
     las=1)
     box(col="black", lwd=2)

     plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
     mtext(plottext, adj=0, side=3, line= 0.25, family="sans", font=3, cex=0.8)

     # ---- find when the CASTAWAY enters the water and the time at which this occurs ----
     #      Note: the entry time is indicated by the increase in conductivity to a value
     #            greater than 0.  

     Enters_Water_row  <- min(which(INPUT.CASTAWAY.PROFILE.R$Cond_mS > 10000))
     Enters_Water_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Enters_Water_row] 

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     # ---- add an arrow on the plot indicating the time of entry into the water ----

     arrows( Enters_Water_time,2, Enters_Water_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Enters_Water_time,3.5,"Water Entry", col="red",cex=0.5)

     # ---- find the beginning of the DOWNCAST and the time at which this occurs ----
 
     Soak_Time_Min <- 5                              # units seconds
     Estimated_Soak_end_time  <- Enters_Water_time + 5
     Estimated_Soak_end_time_row <- which(INPUT.CASTAWAY.PROFILE.R$Time_s == Estimated_Soak_end_time)
                                                     # the CASTAWAY collects RAW data every 0.2 sec (ie
                                                     # 5 recordsper second) and recommends an initial
                                                     # soak time of at least 5-10 seconds. 
                                                     # This corresponds to 25 to 50 records.
     Pres_grad.R            <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     
     Pres_grad.R.DN               <- Pres_grad.R[Estimated_Soak_end_time_row:Pres_max_row]
     First_Row_Pres_grad_LT_Thres <- min(which(Pres_grad.R.DN < -0.5))
     Downcast_Begin_row           <- Estimated_Soak_end_time_row + First_Row_Pres_grad_LT_Thres
     Downcast_begin_time          <- INPUT.CASTAWAY.PROFILE.R$Time_s[Downcast_Begin_row]
 
 
     # add an arrow on the plot indicating the time at which downcast begins

     arrows( Downcast_begin_time,2,Downcast_begin_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Downcast_begin_time,3.5,"Begin DN", col="red",cex=0.5)

     # ---- find the end of the DOWNCAST and the time at which this occurs ----

     Pres_max      <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     # add an arrow on the plot indicating the time of maximum pressure

     arrows( Pres_max_time,2,Pres_max_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,3.5,"Press Max", col="red",cex=0.5)

     # add an arrow on the plot indicating the time of exit from the water

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     arrows( Exits_Water_time,2,Exits_Water_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Exits_Water_time,3.5,"End UP & Water Exit", col="red",cex=0.5)

  
# ------ rate of change in pressure time series ------

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)


par(new=T)
plot(Time_interval_center.R, Pres_grad.R,
     type="l",col="grey",lwd=1,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="",
     #ylab="Pres Grad (db/s)",
     las=1)
     axis(side=4,las=1,col.axis="grey")
     mtext(side=4,line=2.5,"Pres Grad (db/s)",col="dark grey")
     box(col="black", lwd=2)

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0),lwd=1,col="grey",lty="dashed")

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(1,1),lwd=1,col="red",lty="dotted")
     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-1,-1),lwd=1,col="red",lty="dotted")

     points(Time_interval_center.R, Pres_grad.R,pch=16,cex=0.5,col="dark grey")

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ temperature time series ------

par(fig=c(0,1,0,0.5),mar=c(4,4,3,4)+0.1, new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,
     type="l",col="red",lwd=2,
     #xaxt="n",
     #axes=F,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     #ylab="Temp (C)", 
     ylab="",
     #col.lab="red",
     col.axis="red",
     las=1)
     #axis(2,las=1)
     mtext(side=2,line=2.5,"Temp (C)", col="red")
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ conductivity time series ------

par(new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,round(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,2),
     type="l",col="blue",lwd=2,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="",
     #xlab="Time (s)",
     ylab="",
     las=1)
     axis(4,las=1,col.axis="blue")
     mtext(side=4,line=2.5,"Cond (mS)",col="blue")
     box(col="black", lwd=2)

     text(3,4,"in air")
     text(max(INPUT.CASTAWAY.PROFILE.R$Time_s)-5,4,"in air")
     text((0+max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
          (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+3,"in water")
 
     time_duration <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)
     time_midway   <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)/2

     arrows(Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
            Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
            code=3, col="black", lwd=2, length=0.15)

     #arrows(time_midway-0.4*time_midway,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000),
     #       time_midway+0.4*time_midway,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000),
     #       code=3, col="black", lwd=2, length=0.15)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.25,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---------------------------------------------------------------------------------------
# plot the time series of RAW Pressure and calcualted Pressure Gradient data on one page 
# ---------------------------------------------------------------------------------------

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue")
cat(" \n")

win.graph()			# make a new graph window
par(mfrow=c(3,1))		# makes plots on page appear in 3 rows of 1 column

# ---- calculate pressure gradients and times mid-way between time stamps

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)

# ------ raw pressure time series ------

par(mar=c(0,4,4,1)+0.1)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     #type="l",col="black",lwd=2,
     #pch=16,
     xaxt="n",
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     # ---- find the end of the DOWNCAST and the time at which this occurs ----

     Pres_max      <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     # add an arrow on the plot indicating the time of maximum pressure
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,-4,"Press Max", col="red")
     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")

     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"CASTAWAY DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"CASTAWAY UPCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(INPUT.CASTAWAY.PROFILE.R$Time_s),"     all raw pressure and time data")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# ---- time series of presure data used in the DOWNCAST and UPCAST
#      NOTE: the CASTAWAY manual indicates that values of < 0.025 db/s are removed from further processing

# ---- select DOWNCAST data based on default CASTAWAY criteria

CASTAWAY.DOWNCAST.DATA <- INPUT.CASTAWAY.PROFILE.R[1:Pres_max_row,]      #select data rows collected before max pressure reached
CASTAWAY.UPCAST.DATA   <- INPUT.CASTAWAY.PROFILE.R[(Pres_max_row+1):length(INPUT.CASTAWAY.PROFILE.R$Time_s),]      #select data rows collected before max pressure reached

CASTAWAY.DOWNCAST.DATA.Pres_grad.R <- delta_y(-CASTAWAY.DOWNCAST.DATA$Pres_db)/delta_y(CASTAWAY.DOWNCAST.DATA$Time_s)

CASTAWAY.DOWNCAST.DATA.ROWS.PGGT25 <- which(abs(CASTAWAY.DOWNCAST.DATA.Pres_grad.R) > 0.025)+1  # identify row numbers with pressure gradients > 0.025
CASTAWAY.DOWNCAST.DATA             <- CASTAWAY.DOWNCAST.DATA[CASTAWAY.DOWNCAST.DATA.ROWS.PGGT25,]

par(mar=c(2,4,2,1)+0.1)
plot(CASTAWAY.DOWNCAST.DATA$Time_s,-CASTAWAY.DOWNCAST.DATA$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xaxt="n",
     #xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")
     #lines( c(Pres_max_time,Pres_max_time),c(0,-Pres_max), col="red",lwd=2)
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,-4,"Press Max", col="red")
     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"CASTAWAY DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"CASTAWAY UPCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(CASTAWAY.DOWNCAST.DATA$Time_s),
                       "   CASTAWAY criteria: dp/dt > 0.025 db/s ")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# ---- select DOWNCAST data based on FP criteria

CASTAWAY.DOWNCAST.DATA.FP <- INPUT.CASTAWAY.PROFILE.R[1:Pres_max_row,]      #select data rows collected before max pressure reached
CASTAWAY.UPCAST.DATA.FP   <- INPUT.CASTAWAY.PROFILE.R[(Pres_max_row+1):length(INPUT.CASTAWAY.PROFILE.R$Time_s),]      #select data rows collected before max pressure reached

CASTAWAY.DOWNCAST.DATA.Pres_grad.R <- delta_y(-CASTAWAY.DOWNCAST.DATA$Pres_db)/delta_y(CASTAWAY.DOWNCAST.DATA$Time_s)

Grad.Critical.Threshold.FP <- 0.05
CASTAWAY.DOWNCAST.DATA.ROWS.PGGT.FP <- which(abs(CASTAWAY.DOWNCAST.DATA.Pres_grad.R) > Grad.Critical.Threshold.FP)+1  # identify row numbers with gradients > threshold
CASTAWAY.DOWNCAST.DATA.FP           <- CASTAWAY.DOWNCAST.DATA[CASTAWAY.DOWNCAST.DATA.ROWS.PGGT.FP,]
CASTAWAY.DOWNCAST.DATA.FP           <- CASTAWAY.DOWNCAST.DATA.FP[CASTAWAY.DOWNCAST.DATA.FP$Time_s > Downcast_begin_time,]

par(mar=c(5,4,0,1)+0.1)
plot(CASTAWAY.DOWNCAST.DATA.FP$Time_s,-CASTAWAY.DOWNCAST.DATA.FP$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")  # sea surface line
     #lines( c(Pres_max_time,Pres_max_time),c(0,-Pres_max), col="red",lwd=2)
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15) # max pressure arrow
     text(Pres_max_time,-4,"Press Max", col="red")
     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"FP DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"FP PCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(CASTAWAY.DOWNCAST.DATA.FP$Time_s),
                       "   Alternate Criteria: descent begin < time < press max & dp/dt >",Grad.Critical.Threshold.FP," dp/s")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# WILL NEED to CALCULATE DEPTH and SALINITY since data has not been binned 

# ---------------------------------------------------------------------------------------------
# skip the header lines and import the CASTAWAY produced DOWN profile data as a data frame and 
#      add short form header names to the data frame
# ---------------------------------------------------------------------------------------------
# already done above

INPUT.CASTAWAY.PROFILE.DN         <- read.table(INPUT.DATA.FILE.NAME.DN,header=FALSE,skip=29) # skip the header lines
# read the column headings in the CASTAWAY generated file
PROFILE.NAMES.DN                  <- scan(INPUT.DATA.FILE.NAME.DN, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile

# -----------------------------------------------------------------------
# replace the header names in the imported data frame with shorter names
# -----------------------------------------------------------------------

if (PROFILE.NAMES.DN[1]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.DN[2]=="Depth (Meter)"                                      &
    PROFILE.NAMES.DN[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.DN[4]=="Conductivity (MicroSiemens per Centimeter)"         &
    PROFILE.NAMES.DN[5]=="Specific conductance (MicroSiemens per Centimeter)" &
    PROFILE.NAMES.DN[6]=="Salinity (Practical Salinity Scale)"                &
    PROFILE.NAMES.DN[7]=="Sound velocity (Meters per Second)"                 &
    PROFILE.NAMES.DN[8]=="Density (Kilograms per Cubic Meter)" )
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.DN) <- c("Pres_db","Depth_m","Temp_C","Cond_mScm",
                                        "SpecCond_mScm","Sal_psu","SoundVel_ms","Den_kgm3")
   } else {
          stop("Down Profile Names and/or their order in the input file do not match expectations")
          }# end of if

cat("\n")
cat(" A CASTAWAY DOWN CAST data file has been imported \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue plotting T,S & Den vertical profiles")
cat(" \n")

# ------------------------------------------
# plot the T,S,Den profile data on one page 
# ------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Temp_C,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DN$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Sal_psu,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DN$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.DN[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DN[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

cat("\n")
cat("Calculate and plot profiles of CASTAWAY DOWN CAST gradients or rates of change \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")


# -----------------------------------------------------------------
# plot the DOWN Temp, Sal & Den vertical gradient data on one page 
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 row of 3 columns 

Depth_increment.DN       <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)
Depth_interval_center.DN <-   avg_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

# ------ temperature gradient profile ------

Temp_grad.DN <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Temp_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.DN)),max(abs(Temp_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")

# ------ salinity gradient profile ------

Sal_grad.DN <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Sal_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.DN)),max(abs(Sal_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad.DN <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Den_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.DN)),max(abs(Den_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"stable",col="black")

# --------------------------------------------------------
# 2nd row containing fixed gradient scale limits in plots
# --------------------------------------------------------

# ------ temperature gradient profile ------

Temp_grad.DN <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Temp_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(-0.5,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(0.5,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")


# ------ salinity gradient profile ------

Sal_grad.DN <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Sal_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(-0.5,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(0.5,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(Den_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-3),lwd=2)

lines(c(0,-max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(-0.5,-max(Depth_interval_center.DN)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text(0.5,-max(Depth_interval_center.DN)-1-0.7,"stable",col="black")


title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.DN[2]),"PROFILE DATA: vertical gradients"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DN[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)


cat("\n")
cat("Calculate and plot profiles of CASTAWAY Pressure and Depth increments \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# -----------------------------------------------------------------
# Calculate and Plot the Pressure and Depth increments on one page   
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 3 rows of 1 column 

# ------ pressure increment profile ------

Press_increment.DN       <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Pres_db)
Press_interval_center.DN <-   avg_y(INPUT.CASTAWAY.PROFILE.DN$Pres_db)

plot(-Press_increment.DN,-Press_interval_center.DN,
     pch=16,
     xlab="Pressure Increment (db)",
     ylab="Pressure (db)",
     xlim=c(0,0.5),
     ylim=c(-max(Press_interval_center.DN)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Press_increment.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0.3,0.3),c(0,-max(Press_interval_center.DN)-1),lwd=2,col="grey")
text(0.3,-max(Press_interval_center.DN)-1-0.5,"default",col="black")

# ------ depth increment profile ------

Depth_increment.DN       <- delta_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)
Depth_interval_center.DN <-   avg_y(INPUT.CASTAWAY.PROFILE.DN$Depth_m)

plot(-Depth_increment.DN,-Depth_interval_center.DN,
     pch=16,
     xlab="Depth Increment (m)",
     ylab="Depth(m)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Depth_increment.DN))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)),lwd=2)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.DN[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DN[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)

cat("\n")
cat("Plot T,S, Den Profile and Vertical Gradient data on one page \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# -----------------------------------------------------------------
# Plot the T,S, Den Profile and Vertical Gradient data on one page 
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ plot the upper row: T, S and Den profiles -----

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Temp_C,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DN$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Sal_psu,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.DN$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3,-INPUT.CASTAWAY.PROFILE.DN$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DN$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.DN$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.DN[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DN[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---- plot the second row of T, S and density gradients ----

# ------ temperature gradient profile ------

plot(Temp_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.DN)),max(abs(Temp_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-1),lwd=2)

lines(c(0,-max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")

# ------ salinity gradient profile ------

plot(Sal_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.DN)),max(abs(Sal_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-1),lwd=2)

lines(c(0,-max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

plot(Den_grad.DN,-Depth_interval_center.DN,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.DN)),max(abs(Den_grad.DN))),
     ylim=c(-max(Depth_interval_center.DN)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.DN))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.DN)-1),lwd=2)

lines(c(0,-max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.DN))),c(-max(Depth_interval_center.DN)-1,-max(Depth_interval_center.DN)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.DN) ) ))/2,-max(Depth_interval_center.DN)-1-0.7,"stable",col="black")

cat("\n")
cat("Import a CASTAWAY UPCAST for the same cast as the CASTAWAY DOWNCAST \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# -----------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# import the CASTAWAY UPCAST file 
#    i.e. same file name but has a U at the end of the name rather than a D, or P
# ---------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

cat("\n")
cat("Import the CASTAWAY UP CAST data file corresponding to the DOWNCAST file ie.CCxxx...xx_U.txt \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# generate the upcast filename from the choosen downcast filename

INPUT.DATA.FILE.NAME.UP <- noquote(paste(substr(INPUT.DATA.FILE.NAME.DN,1,90),"_U.txt",sep=""))

# --------------------------------------------------------------------
# extract the header information from the data file 
#
# Note:
#   - the CASTAWAY software gives a file name that does not include an 
#       up, down, raw or processed indicator in the title
# --------------------------------------------------------------------

INPUT.FileName.UP        <- scan(INPUT.DATA.FILE.NAME.UP, skip=1,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.UP  <- scan(INPUT.DATA.FILE.NAME.UP, skip=5,  nlines=1, sep="\t", what="character")

# ---------------------------------------------------------
# test to see if the input file is infact an UP cast file
# ---------------------------------------------------------

if (INPUT.ProcessingType.UP[2] != "Up") {
    cat("********************************************************\n")
    cat("!!!!! WARNING !!!!!  File imported is NOT an UP CAST    \n")
    cat("                     R Script will be terminated        \n")
    cat("      ACTION: Check file name and modify if appropriate \n")
    cat("********************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

# ---------------------------------------------------------
# test to see if the UP and DOWN input files are for the
#      same  CASTAWAY profile
# ---------------------------------------------------------

if (INPUT.FileName.UP[2] != INPUT.FileName.DN[2]) {
    cat("*********************************************************\n")
    cat("!!!!! WARNING !!!!!  imported UP file is NOT from the    \n")
    cat("                     same cast as the imported DOWN file \n")
    cat("                     R Script will be terminated         \n")
    cat("      ACTION: Check file name and modify if appropriate  \n")
    cat("*********************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

INPUT.CASTAWAY.PROFILE.HEADERS.UP <- scan(INPUT.DATA.FILE.NAME.UP, skip=28, nlines=1, sep="\t", what=list("","","","","","","","","","")) 
PROFILE.NAMES.UP <- scan(INPUT.DATA.FILE.NAME.UP, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile

# ----------------------------------------------------------------------------------------
# import the CASTAWAY UP profile data as a data frame and add header names to the data frame
# ----------------------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.UP        <- read.table(INPUT.DATA.FILE.NAME.UP,header=FALSE,skip=29)
names(INPUT.CASTAWAY.PROFILE.UP) <- PROFILE.NAMES.UP

# ----------------------------------------------------------------------------------------
# import the CASTAWAY profile data as a data frame and add header names to the data frame
# ----------------------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.UP        <- read.table(INPUT.DATA.FILE.NAME.UP,header=FALSE,skip=29)
names(INPUT.CASTAWAY.PROFILE.UP) <- PROFILE.NAMES.UP

# -----------------------------------------------------------------------
# replace the header names in the imported data frame with shorter names
# -----------------------------------------------------------------------

if (PROFILE.NAMES.UP[1]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.UP[2]=="Depth (Meter)"                                      &
    PROFILE.NAMES.UP[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.UP[4]=="Conductivity (MicroSiemens per Centimeter)"         &
    PROFILE.NAMES.UP[5]=="Specific conductance (MicroSiemens per Centimeter)" &
    PROFILE.NAMES.UP[6]=="Salinity (Practical Salinity Scale)"                &
    PROFILE.NAMES.UP[7]=="Sound velocity (Meters per Second)"                 &
    PROFILE.NAMES.UP[8]=="Density (Kilograms per Cubic Meter)" )
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.UP) <- c("Pres_db","Depth_m","Temp_C","Cond_mScm",
                                        "SpecCond_mScm","Sal_psu","SoundVel_ms","Den_kgm3")
   } else {
          stop("Profile Names and/or their order in the input file do not match expectations")
          }# end of if

cat("\n")
cat("A CASTAWAY UP CAST data file has been imported \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue with a plot of T,S & Den UP profiles")
cat(" \n")

# ---------------------------------------------
# plot the T,S,Den UP profile data on one page 
# ---------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Temp_C,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.UP$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Sal_psu,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.UP$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.UP[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.UP[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

cat("\n")
cat("Plot the T, S & Den vertical gradient data for the CASTAWAY UPCAST\n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# ------------------------------------------------------------
# plot the Temp, Sal & Den vertical gradient data on one page 
# ------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 row of 3 columns 

Depth_increment.UP       <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)
Depth_interval_center.UP <-   avg_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

# ------ temperature gradient profile ------

Temp_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Temp_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.UP)),max(abs(Temp_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-max(abs(Temp_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")


# ------ salinity gradient profile ------

Sal_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Sal_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.UP)),max(abs(Sal_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-max(abs(Sal_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Den_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.UP)),max(abs(Den_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-max(abs(Den_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"stable",col="black")

# -------------------------------------------------
# 2nd row containing fixed gradient scale in plots
# -------------------------------------------------

# ------ temperature gradient profile ------

Temp_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Temp_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(0.5,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")

# ------ salinity gradient profile ------

Sal_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Sal_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(0.25,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(Den_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.UP)-1-0.7,"inversion",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text(0.25,-max(Depth_interval_center.UP)-1-0.7,"stable",col="black")


title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.UP[2]),"PROFILE DATA: vertical gradients"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.UP[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)

cat("\n")
cat("Plot the Pressure and Depth increments for the CASTAWAY UPCAST\n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# -----------------------------------------------------------------
# Calculate and Plot the Pressure and Depth increments on one page   
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 3 rows of 1 column 

# ------ pressure increment profile ------

Press_increment.UP       <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Pres_db)
Press_interval_center.UP <-   avg_y(INPUT.CASTAWAY.PROFILE.UP$Pres_db)

plot(-Press_increment.UP,-Press_interval_center.UP,
     pch=16,
     xlab="Pressure Increment (db)",
     ylab="Pressure (db)",
     xlim=c(0,0.5),
     ylim=c(-max(Press_interval_center.UP)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Press_increment.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0.3,0.3),c(0,-max(Press_interval_center.UP)-1),lwd=2,col="grey")
text(0.3,-max(Press_interval_center.UP)-1-0.5,"default",col="black")

# ------ depth increment profile ------

Depth_increment.UP       <- delta_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)
Depth_interval_center.UP <-   avg_y(INPUT.CASTAWAY.PROFILE.UP$Depth_m)

plot(-Depth_increment.UP,-Depth_interval_center.UP,
     pch=16,
     xlab="Depth Increment (m)",
     ylab="Depth(m)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Depth_increment.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)),lwd=2)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.UP[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.UP[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)

cat("\n")
cat("Plot the T,S and Den and gradients for the CASTAWAY UPCAST \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# -----------------------------------------------------------------
# Plot the T,S, Den Profile and Vertical Gradient data on one page 
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ plot the upper row: T, S and Den profiles -----

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Temp_C,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.UP$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Sal_psu,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.UP$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3,-INPUT.CASTAWAY.PROFILE.UP$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.UP$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.UP$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.UP[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.UP[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---- plot the second row of T, S and density gradients ----

# ------ temperature gradient profile ------

plot(Temp_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.UP)),max(abs(Temp_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-1),lwd=2)

lines(c(0,-max(abs(Temp_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")

# ------ salinity gradient profile ------

plot(Sal_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.UP)),max(abs(Sal_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-1),lwd=2)

lines(c(0,-max(abs(Sal_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

plot(Den_grad.UP,-Depth_interval_center.UP,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.UP)),max(abs(Den_grad.UP))),
     ylim=c(-max(Depth_interval_center.UP)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.UP))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.UP)-1),lwd=2)

lines(c(0,-max(abs(Den_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.UP))),c(-max(Depth_interval_center.UP)-1,-max(Depth_interval_center.UP)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.UP) ) ))/2,-max(Depth_interval_center.UP)-1-0.7,"stable",col="black")

# -----------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# import the CASTAWAY DOWN and UP CAST file 
#    i.e. same file name but has a DU at the end of the name rather than a D, U or P
# ---------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

cat("\n")
cat("Import the CASTAWAY DOWN and UP CAST data file corresponding to the DOWNCAST file ie.CCxxx...xx_DU.txt \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# generate the upcast filename from the choosen downcast filename

INPUT.DATA.FILE.NAME.DU <- noquote(paste(substr(INPUT.DATA.FILE.NAME.DN,1,90),"_DU.txt",sep=""))

# --------------------------------------------------------------------
# extract the header information from the data file 
#
# Note:
#   - the CASTAWAY software gives a file name that does not include an 
#       up, down, raw or processed indicator in the title
# --------------------------------------------------------------------

INPUT.FileName.DU        <- scan(INPUT.DATA.FILE.NAME.DU, skip=1,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.DU  <- scan(INPUT.DATA.FILE.NAME.DU, skip=5,  nlines=1, sep="\t", what="character")

# ---------------------------------------------------------
# test to see if the input file is infact an UP cast file
# ---------------------------------------------------------

if (INPUT.ProcessingType.DU[2] != "Down & up") {
    cat("****************************************************************\n")
    cat("!!!!! WARNING !!!!!  File imported is NOT a DOWN and UP CAST    \n")
    cat("                     R Script will be terminated                \n")
    cat("      ACTION: Check file name and modify if appropriate         \n")
    cat("****************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

# ---------------------------------------------------------
# test to see if the DOWN and UP input file is for the
#      same CASTAWAY profile as the separate DOWN and UP
#      files
# ---------------------------------------------------------

if (INPUT.FileName.DU[2] != INPUT.FileName.DN[2]) {
    cat("***************************************************************\n")
    cat("!!!!! WARNING !!!!!  imported DOWN & UP file is NOT from the   \n")
    cat("                     same cast as the imported DOWN file       \n")
    cat("                     R Script will be terminated               \n")
    cat("      ACTION: Check file name and modify if appropriate        \n")
    cat("***************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

INPUT.CASTAWAY.PROFILE.HEADERS.DU <- scan(INPUT.DATA.FILE.NAME.UP, skip=28, nlines=1, sep="\t", what=list("","","","","","","","","","")) 
PROFILE.NAMES.DU <- scan(INPUT.DATA.FILE.NAME.UP, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile

# ----------------------------------------------------------------------------------------------------
# import the CASTAWAY DOWN and UP profile data as a data frame and add header names to the data frame
# ----------------------------------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.DU        <- read.table(INPUT.DATA.FILE.NAME.DU,header=FALSE,skip=29)
names(INPUT.CASTAWAY.PROFILE.DU) <- PROFILE.NAMES.DU

# -----------------------------------------------------------------------
# replace the header names in the imported data frame with shorter names
# -----------------------------------------------------------------------

if (PROFILE.NAMES.DU[1]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.DU[2]=="Depth (Meter)"                                      &
    PROFILE.NAMES.DU[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.DU[4]=="Conductivity (MicroSiemens per Centimeter)"         &
    PROFILE.NAMES.DU[5]=="Specific conductance (MicroSiemens per Centimeter)" &
    PROFILE.NAMES.DU[6]=="Salinity (Practical Salinity Scale)"                &
    PROFILE.NAMES.DU[7]=="Sound velocity (Meters per Second)"                 &
    PROFILE.NAMES.DU[8]=="Density (Kilograms per Cubic Meter)" )
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.DU) <- c("Pres_db","Depth_m","Temp_C","Cond_mScm",
                                        "SpecCond_mScm","Sal_psu","SoundVel_ms","Den_kgm3")
   } else {
          stop("Profile Names and/or their order in the input file do not match expectations")
          }# end of if

# ---------------------------------------------
# plot the T,S,Den DU profile data on one page 
# ---------------------------------------------

cat("\n")
cat(" a CASTAWAY DOWN & UP CAST data file has been imported \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue with a plot of T,S,Den DOWN & UP CAST data")
cat(" \n")

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.DU$Temp_C,-INPUT.CASTAWAY.PROFILE.DU$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DU$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DU$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.DU$Sal_psu,-INPUT.CASTAWAY.PROFILE.DU$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DU$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DU$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.DU$Den_kgm3,-INPUT.CASTAWAY.PROFILE.DU$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.DU$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.DU$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.DU[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DU[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# -----------------------------------------------------------
# calculate and plot the differences between the DOWN and UP
#   values in T,S & Den on one page 
# -----------------------------------------------------------

cat("\n")
cat("Calcualte and Plot the profile of differences between the CASTAWAY DOWN & UP CAST data\n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

INPUT.CASTAWAY.PROFILE.UP.reorder <- INPUT.CASTAWAY.PROFILE.UP[order(INPUT.CASTAWAY.PROFILE.UP$Depth_m),]

INPUT.CASTAWAY.PROFILE.Temp.Diff  <- INPUT.CASTAWAY.PROFILE.DN$Temp_C   - INPUT.CASTAWAY.PROFILE.UP.reorder$Temp_C
INPUT.CASTAWAY.PROFILE.Sal.Diff   <- INPUT.CASTAWAY.PROFILE.DN$Sal_psu  - INPUT.CASTAWAY.PROFILE.UP.reorder$Sal_psu
INPUT.CASTAWAY.PROFILE.Den.Diff   <- INPUT.CASTAWAY.PROFILE.DN$Den_kgm3 - INPUT.CASTAWAY.PROFILE.UP.reorder$Den_kgm3
INPUT.CASTAWAY.PROFILE.Depth.Diff <- INPUT.CASTAWAY.PROFILE.DN$Depth_m  - INPUT.CASTAWAY.PROFILE.UP.reorder$Depth_m
INPUT.CASTAWAY.PROFILE.Depth.Avg  <- (INPUT.CASTAWAY.PROFILE.DN$Depth_m + INPUT.CASTAWAY.PROFILE.UP.reorder$Depth_m)/2

# ------ temperature difference profile ----

plot(INPUT.CASTAWAY.PROFILE.Temp.Diff,-INPUT.CASTAWAY.PROFILE.Depth.Avg,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp Difference (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.Depth.Avg)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.Temp.Diff))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.Sal.Diff,-INPUT.CASTAWAY.PROFILE.Depth.Avg,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal Difference(psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.Depth.Avg)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.Sal.Diff))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.Den.Diff,-INPUT.CASTAWAY.PROFILE.Depth.Avg,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den Difference (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.Depth.Avg)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.Den.Diff))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY: PROFILE DOWN minus UP Data"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.DU[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# -----------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------
# import the CASTAWAY PROCESSED CAST file 
#    i.e. same file name but has a P at the end of the name rather than a D, U or DU
# ---------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

cat("\n")
cat("Import the CASTAWAY PROCESSED CAST data file corresponding to the DOWNCAST file ie.CCxxx...xx_DU.txt \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# generate the upcast filename from the choosen downcast filename

INPUT.DATA.FILE.NAME.P <- noquote(paste(substr(INPUT.DATA.FILE.NAME.DN,1,90),"_P.txt",sep=""))

# --------------------------------------------------------------------
# extract the header information from the data file 
#
# Note:
#   - the CASTAWAY software gives a file name that does not include an 
#       up, down, raw or processed indicator in the title
# --------------------------------------------------------------------

INPUT.FileName.P        <- scan(INPUT.DATA.FILE.NAME.P, skip=1,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.P  <- scan(INPUT.DATA.FILE.NAME.P, skip=5,  nlines=1, sep="\t", what="character")

# ---------------------------------------------------------
# test to see if the input file is infact an UP cast file
# ---------------------------------------------------------

if (INPUT.ProcessingType.P[2] != "Processed") {
    cat("****************************************************************\n")
    cat("!!!!! WARNING !!!!!  File imported is NOT a DOWN and UP CAST    \n")
    cat("                     R Script will be terminated                \n")
    cat("      ACTION: Check file name and modify if appropriate         \n")
    cat("****************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

# ---------------------------------------------------------
# test to see if the DOWN and UP input file is for the
#      same CASTAWAY profile as the separate DOWN and UP
#      files
# ---------------------------------------------------------

if (INPUT.FileName.P[2] != INPUT.FileName.DN[2]) {
    cat("***************************************************************\n")
    cat("!!!!! WARNING !!!!!  imported PROCESSED file is NOT from the   \n")
    cat("                     same cast as the imported DOWN file       \n")
    cat("                     R Script will be terminated               \n")
    cat("      ACTION: Check file name and modify if appropriate        \n")
    cat("***************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if


INPUT.CASTAWAY.PROFILE.HEADERS.P <- scan(INPUT.DATA.FILE.NAME.P, skip=28, nlines=1, sep="\t", what=list("","","","","","","","","","")) 
PROFILE.NAMES.P <- scan(INPUT.DATA.FILE.NAME.P, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile

# ----------------------------------------------------------------------------------------------------
# import the CASTAWAY PROCESSED profile data as a data frame and add header names to the data frame
# ----------------------------------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.P        <- read.table(INPUT.DATA.FILE.NAME.P,header=FALSE,skip=29)
names(INPUT.CASTAWAY.PROFILE.P) <- PROFILE.NAMES.P

# -----------------------------------------------------------------------
# replace the header names in the imported data frame with shorter names
# -----------------------------------------------------------------------

if (PROFILE.NAMES.P[1]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.P[2]=="Depth (Meter)"                                      &
    PROFILE.NAMES.P[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.P[4]=="Conductivity (MicroSiemens per Centimeter)"         &
    PROFILE.NAMES.P[5]=="Specific conductance (MicroSiemens per Centimeter)" &
    PROFILE.NAMES.P[6]=="Salinity (Practical Salinity Scale)"                &
    PROFILE.NAMES.P[7]=="Sound velocity (Meters per Second)"                 &
    PROFILE.NAMES.P[8]=="Density (Kilograms per Cubic Meter)" )
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.P) <- c("Pres_db","Depth_m","Temp_C","Cond_mScm",
                                        "SpecCond_mScm","Sal_psu","SoundVel_ms","Den_kgm3")
   } else {
          stop("Profile Names and/or their order in the input file do not match expectations")
          }# end of if

cat("\n")
cat(" a CASTAWAY PROCESSED CAST data file has been imported \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# ---------------------------------------------
# plot the T,S,Den PROCESSED profile data on one page 
# ---------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Temp_C,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.P$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Sal_psu,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.P$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Den_kgm3,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.P$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.P[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.P[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ----------------------------------------------------------------------
# plot the PROCESSED Temp, Sal & Den vertical gradient data on one page 
# ----------------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 row of 3 columns 

Depth_increment.P       <- delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)
Depth_interval_center.P <-   avg_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

# ------ temperature gradient profile ------

Temp_grad.P <- delta_y(INPUT.CASTAWAY.PROFILE.P$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Temp_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.P)),max(abs(Temp_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-max(abs(Temp_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")


# ------ salinity gradient profile ------

Sal_grad.P <- delta_y(INPUT.CASTAWAY.PROFILE.P$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Sal_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.P)),max(abs(Sal_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-max(abs(Sal_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad.P <- delta_y(INPUT.CASTAWAY.PROFILE.P$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Den_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.P)),max(abs(Den_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-max(abs(Den_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"stable",col="black")

# -------------------------------------------------
# 2nd row containing fixed gradient scale in plots
# -------------------------------------------------

# ------ temperature gradient profile ------

Temp_grad.P <- delta_y(INPUT.CASTAWAY.PROFILE.P$Temp_C)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Temp_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(0.5,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")


# ------ salinity gradient profile ------

Sal_grad.P <- delta_y(INPUT.CASTAWAY.PROFILE.P$Sal_psu)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Sal_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(0.25,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

Den_grad.UP <- delta_y(INPUT.CASTAWAY.PROFILE.P$Den_kgm3)/delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(Den_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-0.5,+0.5),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("Fixed Gradient Scale Limits")
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-3),lwd=2)

lines(c(0,-0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(-0.25,-max(Depth_interval_center.P)-1-0.7,"inversion",col="black")

lines(c(0,+0.5),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text(0.25,-max(Depth_interval_center.P)-1-0.7,"stable",col="black")


title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.P[2]),"PROFILE DATA: vertical gradients"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.P[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)

# -----------------------------------------------------------------
# Calculate and Plot the Pressure and Depth increments on one page   
# -----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 3 rows of 1 column 

# ------ pressure increment profile ------

Press_increment.P       <- delta_y(INPUT.CASTAWAY.PROFILE.P$Pres_db)
Press_interval_center.P <-   avg_y(INPUT.CASTAWAY.PROFILE.P$Pres_db)

plot(-Press_increment.P,-Press_interval_center.P,
     pch=16,
     xlab="Pressure Increment (db)",
     ylab="Pressure (db)",
     xlim=c(0,0.5),
     ylim=c(-max(Press_interval_center.P)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Press_increment.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0.3,0.3),c(0,-max(Press_interval_center.P)-1),lwd=2,col="grey")
text(0.3,-max(Press_interval_center.P)-1-0.5,"default",col="black")

# ------ depth increment profile ------

Depth_increment.P       <- delta_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)
Depth_interval_center.P <-   avg_y(INPUT.CASTAWAY.PROFILE.P$Depth_m)

plot(-Depth_increment.P,-Depth_interval_center.P,
     pch=16,
     xlab="Depth Increment (m)",
     ylab="Depth(m)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Depth_increment.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)),lwd=2)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.P[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.P[2])),  outer=T, line=-2.5, cex.main=1, font.main=3)

# ---------------------------------------------------------------------------
# Plot the PROCESSED T,S, Den Profile and Vertical Gradient data on one page 
# ---------------------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(2,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ plot the upper row: T, S and Den profiles -----

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Temp_C,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="red",lwd=2,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.P$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Sal_psu,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.P$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE.P$Den_kgm3,-INPUT.CASTAWAY.PROFILE.P$Depth_m,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.P$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE.P$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.P[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.P[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---- plot the second row of T, S and density gradients ----

# ------ temperature gradient profile ------

plot(Temp_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Temp Gradient (C/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Temp_grad.P)),max(abs(Temp_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Temp_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-1),lwd=2)

lines(c(0,-max(abs(Temp_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Temp_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Temp_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Temp_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")


# ------ salinity gradient profile ------

plot(Sal_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Salinity Gradient (psu/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Sal_grad.P)),max(abs(Sal_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Sal_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-1),lwd=2)

lines(c(0,-max(abs(Sal_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Sal_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"decrease",col="black")

lines(c(0, max(abs(Sal_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Sal_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"increase",col="black")

# ------ density gradient profile ------

plot(Den_grad.P,-Depth_interval_center.P,
     type="l",col="black",lwd=2,
     #pch=16,
     xlab="Density Gradient (kg/m3/m)",
     ylab="Depth (m)",
     xlim=c(-max(abs(Den_grad.P)),max(abs(Den_grad.P))),
     ylim=c(-max(Depth_interval_center.P)-2,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(Den_grad.P))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(c(0,0),c(0,-max(Depth_interval_center.P)-1),lwd=2)

lines(c(0,-max(abs(Den_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +(-max( abs(Den_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"inversion",col="black")

lines(c(0, max(abs(Den_grad.P))),c(-max(Depth_interval_center.P)-1,-max(Depth_interval_center.P)-1),lwd=2,col="black")
text((0 +( max( abs(Den_grad.P) ) ))/2,-max(Depth_interval_center.P)-1-0.7,"stable",col="black")

# ----------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------
# import the CASTAWAY RAW CAST file 
#    i.e. same file name but has a R at the end of the name rather than a D, U, DU or P
# --------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------

cat("\n")
cat("Import the CASTAWAY RAW CAST data file corresponding to the DOWNCAST file ie.CCxxx...xx_DU.txt \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# generate the RAW filename from the choosen downcast filename

INPUT.DATA.FILE.NAME.R <- noquote(paste(substr(INPUT.DATA.FILE.NAME.DN,1,90),"_R.txt",sep=""))

# --------------------------------------------------------------------
# extract the header information from the data file 
#
# Note:
#   - the CASTAWAY software gives a file name that does not include an 
#       up, down, raw or processed indicator in the title
# --------------------------------------------------------------------

INPUT.FileName.R        <- scan(INPUT.DATA.FILE.NAME.R, skip=1,  nlines=1, sep="\t", what="character")
INPUT.ProcessingType.R  <- scan(INPUT.DATA.FILE.NAME.R, skip=5,  nlines=1, sep="\t", what="character")

# ---------------------------------------------------------
# test to see if the input file is infact an UP cast file
# ---------------------------------------------------------

if (INPUT.ProcessingType.R[2] != "Raw") {
    cat("****************************************************************\n")
    cat("!!!!! WARNING !!!!!  File imported does NOT contain RAW data    \n")
    cat("                     R Script will be terminated                \n")
    cat("      ACTION: Check file name and modify if appropriate         \n")
    cat("****************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

# ---------------------------------------------------------
# test to see if the RAW input file is for the
#      same CASTAWAY profile as the separate DOWN, UP, 
#      DOWN-UP and PROCESSED files
# ---------------------------------------------------------

if (INPUT.FileName.R[2] != INPUT.FileName.DN[2]) {
    cat("***************************************************************\n")
    cat("!!!!! WARNING !!!!!  imported RAW file is NOT from the         \n")
    cat("                     same cast as the imported DOWN file       \n")
    cat("                     R Script will be terminated               \n")
    cat("      ACTION: Check file name and modify if appropriate        \n")
    cat("***************************************************************\n")
    cat("\n")
    stop("Script has been terminated")
    } # end of if

INPUT.CASTAWAY.PROFILE.HEADERS.R <- scan(INPUT.DATA.FILE.NAME.R, skip=28, nlines=1, sep="\t", what=list("","","","","","","","","","")) 
PROFILE.NAMES.R <- scan(INPUT.DATA.FILE.NAME.R, skip=28, nlines=1, sep="\t", what="character", quiet=T) # reads tab delimited textfile

# ----------------------------------------------------------------------------------------------------
# import the CASTAWAY RAW profile data as a data frame and add header names to the data frame
# ----------------------------------------------------------------------------------------------------

INPUT.CASTAWAY.PROFILE.R        <- read.table(INPUT.DATA.FILE.NAME.R,header=FALSE,skip=29)
names(INPUT.CASTAWAY.PROFILE.R) <- PROFILE.NAMES.R

# -----------------------------------------------------------------------
# replace the header names in the imported data frame with shorter names
# -----------------------------------------------------------------------

if (PROFILE.NAMES.R[1]=="Time (Seconds)"                                     &
    PROFILE.NAMES.R[2]=="Pressure (Decibar)"                                 &
    PROFILE.NAMES.R[3]=="Temperature (Celsius)"                              &
    PROFILE.NAMES.R[4]=="Conductivity (MicroSiemens per Centimeter)")         
   {
     # replace the CASTAWAY data variable headers with shorter names
     names(INPUT.CASTAWAY.PROFILE.R) <- c("Time_s","Pres_db","Temp_C","Cond_mScm")
   } else {
          stop("Profile Names and/or their order in the input file do not match expectations")
          } # end of if

cat("\n")
cat(" a CASTAWAY RAW CAST data file has been imported \n")
cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

# ----------------------------------------------------------------
# plot the time series of RAW Pres,Temp and Cond data on one page 
# ----------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(4,1))		# makes plots on page appear in 3 rows of 1 column

# ------ pressure time series ------

plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="l",col="red",lwd=2,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ rate of change in pressure time series ------

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)

plot(Time_interval_center.R, Pres_grad.R,
     type="l",col="red",lwd=2,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Pres Grad (db/s)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ temperature time series ------

plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,
     type="l",col="blue",lwd=2,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Temp (C)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ conductivity time series ------

plot(INPUT.CASTAWAY.PROFILE.R$Time_s,round(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,2),
     type="l",col="blue",lwd=2,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="Cond (mS/1000)",
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ----------------------------------------------------------------
# plot the time series of RAW Pres,Temp and Cond data on one page
#      2 panels
#      - top panel pressure and pressure gradient time series
#      - lower panel temperature and conductivity time series
# ----------------------------------------------------------------

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue plotting the time series of RAW T,C & P & dP")
cat(" \n")

win.graph()			# make a new graph window

# ------ pressure time series ------

par(fig=c(0,1,0.5,1), mar=c(2,4,5,4)+0.1)            # mar=c(bottom,left,top,right)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="l",col="black",lwd=3,
     xlab="",
     #xaxt="n",
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     #xlab="Time (s)",
     ylab="Pres (db)", col.lab="black",
     las=1)
     box(col="black", lwd=2)

     plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
     mtext(plottext, adj=0, side=3, line= 0.25, family="sans", font=3, cex=0.8)

     # ---- find when the CASTAWAY enters the water and the time at which this occurs ----
     #      Note: the entry time is indicated by the increase in conductivity to a value
     #            greater than 0.  

     Enters_Water_row  <- min(which(INPUT.CASTAWAY.PROFILE.R$Cond_mS > 10000))
     Enters_Water_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Enters_Water_row] 

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     # ---- add an arrow on the plot indicating the time of entry into the water ----

     arrows( Enters_Water_time,2, Enters_Water_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Enters_Water_time,3.5,"Water Entry", col="red",cex=0.5)

     # ---- find the beginning of the DOWNCAST and the time at which this occurs ----
 
     Soak_Time_Min <- 5                              # units seconds
     Estimated_Soak_end_time  <- Enters_Water_time + 5
     Estimated_Soak_end_time_row <- which(INPUT.CASTAWAY.PROFILE.R$Time_s == Estimated_Soak_end_time)
                                                     # the CASTAWAY collects RAW data every 0.2 sec (ie
                                                     # 5 recordsper second) and recommends an initial
                                                     # soak time of at least 5-10 seconds. 
                                                     # This corresponds to 25 to 50 records.
     Pres_grad.R            <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
     
     Pres_grad.R.DN               <- Pres_grad.R[Estimated_Soak_end_time_row:Pres_max_row]
     First_Row_Pres_grad_LT_Thres <- min(which(Pres_grad.R.DN < -0.5))
     Downcast_Begin_row           <- Estimated_Soak_end_time_row + First_Row_Pres_grad_LT_Thres
     Downcast_begin_time          <- INPUT.CASTAWAY.PROFILE.R$Time_s[Downcast_Begin_row]
 
 
     # add an arrow on the plot indicating the time at which downcast begins

     arrows( Downcast_begin_time,2,Downcast_begin_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Downcast_begin_time,3.5,"Begin DN", col="red",cex=0.5)

     # ---- find the end of the DOWNCAST and the time at which this occurs ----

     Pres_max      <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     # add an arrow on the plot indicating the time of maximum pressure

     arrows( Pres_max_time,2,Pres_max_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,3.5,"Press Max", col="red",cex=0.5)

     # add an arrow on the plot indicating the time of exit from the water

     Exits_Water_row   <- min(which(rev(INPUT.CASTAWAY.PROFILE.R$Cond_mS) > 10000))
     Exits_Water_time  <- rev(INPUT.CASTAWAY.PROFILE.R$Time_s)[Exits_Water_row]

     arrows( Exits_Water_time,2,Exits_Water_time, -Pres_max-6, code=2, col="red", lwd=2, length=0.15)
     text(Exits_Water_time,3.5,"End UP & Water Exit", col="red",cex=0.5)

  
# ------ rate of change in pressure time series ------

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)


par(new=T)
plot(Time_interval_center.R, Pres_grad.R,
     type="l",col="grey",lwd=1,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     ylab="",
     #ylab="Pres Grad (db/s)",
     las=1)
     axis(side=4,las=1,col.axis="grey")
     mtext(side=4,line=2.5,"Pres Grad (db/s)",col="dark grey")
     box(col="black", lwd=2)

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0),lwd=1,col="grey",lty="dashed")

     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0.75,0.75),lwd=1,col="red",lty="dotted")
     lines(c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-0.75,-0.75),lwd=1,col="red",lty="dotted")

     points(Time_interval_center.R, Pres_grad.R,pch=16,cex=0.5,col="dark grey")

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ temperature time series ------

par(fig=c(0,1,0,0.5),mar=c(4,4,3,4)+0.1, new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,INPUT.CASTAWAY.PROFILE.R$Temp_C,
     type="l",col="red",lwd=2,
     #xaxt="n",
     #axes=F,
     #pch=16,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="Time (s)",
     #ylab="Temp (C)", 
     ylab="",
     #col.lab="red",
     col.axis="red",
     las=1)
     #axis(2,las=1)
     mtext(side=2,line=2.5,"Temp (C)", col="red")
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE.R$Time_s))
mtext(plottext, adj=0, side=3, line= 0.25, 	      
	family="sans", font=3, cex=0.8)

# ------ conductivity time series ------

par(new=T)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,round(INPUT.CASTAWAY.PROFILE.R$Cond_mS/1000,2),
     type="l",col="blue",lwd=2,
     #pch=16,
     axes=F,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     xlab="",
     #xlab="Time (s)",
     ylab="",
     las=1)
     axis(4,las=1,col.axis="blue")
     mtext(side=4,line=2.5,"Cond (mS)",col="blue")
     box(col="black", lwd=2)

     text(3,4,"in air")
     text(max(INPUT.CASTAWAY.PROFILE.R$Time_s)-5,4,"in air")
     text((0+max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,
          (0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000)+3,"in water")
 
     time_duration <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)
     time_midway   <- max(INPUT.CASTAWAY.PROFILE.R$Time_s)/2

     arrows(Enters_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
            Exits_Water_time,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2/1000),
            code=3, col="black", lwd=2, length=0.15)

     #arrows(time_midway-0.4*time_midway,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000),
     #       time_midway+0.4*time_midway,(0+max(INPUT.CASTAWAY.PROFILE.R$Cond_mS)/2000),
     #       code=3, col="black", lwd=2, length=0.15)

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType.R[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.25,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---------------------------------------------------------------------------------------
# plot the time series of RAW Pressure and calcualted Pressure Gradient data on one page 
# ---------------------------------------------------------------------------------------

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue")
cat(" \n")

win.graph()			# make a new graph window
par(mfrow=c(3,1))		# makes plots on page appear in 3 rows of 1 column

# ---- calculate pressure gradients and times mid-way between time stamps

Pres_grad.R <- delta_y(-INPUT.CASTAWAY.PROFILE.R$Pres_db)/delta_y(INPUT.CASTAWAY.PROFILE.R$Time_s)
Time_interval_center.R <-   avg_y(INPUT.CASTAWAY.PROFILE.R$Time_s)

# ------ raw pressure time series ------

par(mar=c(0,4,4,1)+0.1)
plot(INPUT.CASTAWAY.PROFILE.R$Time_s,-INPUT.CASTAWAY.PROFILE.R$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     #type="l",col="black",lwd=2,
     #pch=16,
     xaxt="n",
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     # ---- find the end of the DOWNCAST and the time at which this occurs ----

     Pres_max      <- max(INPUT.CASTAWAY.PROFILE.R$Pres_db[which(Pres_grad.R < -0.05)+1])
                                       # In the above statement which(Pres_grad.R < -0.05)+1      
                                       # identifies row numbers in the Pres_grad.R vector that
                                       # correspond with a pressure gradient value exceeding -0.05 db/s,
                                       # ie the instrument fall rate exceeds 0.05 db/s
                                       # 0.05 is the critical value stated in the CASTAWAY manual.
                                       # Since the gradient is calculated using two adjacent rows of data,
                                       # a pressure gradient > 0.05 means the measured values in the 
                                       # larger row number have been measured when the
                                       # instrument has been falling at a rate > 0.05 db/s
                                       # To identify these row numbers the row numbers identified by which
                                       # in the Pres_grad.R file need to be augmented by +1. 
                                       # NOTE: by using a critical value of -0.05 bd/s only the descending
                                       #       or DOWNCAST values are retained, UPCAST data is not identified

     Pres_max_row  <- which(INPUT.CASTAWAY.PROFILE.R$Pres_db == Pres_max)
     Pres_max_time <- INPUT.CASTAWAY.PROFILE.R$Time_s[Pres_max_row] 

     # add an arrow on the plot indicating the time of maximum pressure
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,-4,"Press Max", col="red")
     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")

     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"CASTAWAY DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"CASTAWAY UPCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(INPUT.CASTAWAY.PROFILE.R$Time_s),"     all raw pressure and time data")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# ---- time series of presure data used in the DOWNCAST and UPCAST
#      NOTE: the CASTAWAY manual indicates that values of < 0.025 db/s are removed from further processing

# ---- select DOWNCAST data based on default CASTAWAY criteria

CASTAWAY.DOWNCAST.DATA <- INPUT.CASTAWAY.PROFILE.R[1:Pres_max_row,]      #select data rows collected before max pressure reached
CASTAWAY.UPCAST.DATA   <- INPUT.CASTAWAY.PROFILE.R[(Pres_max_row+1):length(INPUT.CASTAWAY.PROFILE.R$Time_s),]      #select data rows collected before max pressure reached

CASTAWAY.DOWNCAST.DATA.Pres_grad.R <- delta_y(-CASTAWAY.DOWNCAST.DATA$Pres_db)/delta_y(CASTAWAY.DOWNCAST.DATA$Time_s)

CASTAWAY.DOWNCAST.DATA.ROWS.PGGT25 <- which(abs(CASTAWAY.DOWNCAST.DATA.Pres_grad.R) > 0.025)+1  # identify row numbers with pressure gradients > 0.025
CASTAWAY.DOWNCAST.DATA             <- CASTAWAY.DOWNCAST.DATA[CASTAWAY.DOWNCAST.DATA.ROWS.PGGT25,]

par(mar=c(2,4,2,1)+0.1)
plot(CASTAWAY.DOWNCAST.DATA$Time_s,-CASTAWAY.DOWNCAST.DATA$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xaxt="n",
     #xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")
     #lines( c(Pres_max_time,Pres_max_time),c(0,-Pres_max), col="red",lwd=2)
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15)
     text(Pres_max_time,-4,"Press Max", col="red")
     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"CASTAWAY DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"CASTAWAY UPCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(CASTAWAY.DOWNCAST.DATA$Time_s),
                       "   CASTAWAY criteria: dp/dt > 0.025 db/s ")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# ---- select DOWNCAST data based on FP criteria

CASTAWAY.DOWNCAST.DATA.FP <- INPUT.CASTAWAY.PROFILE.R[1:Pres_max_row,]      #select data rows collected before max pressure reached
CASTAWAY.UPCAST.DATA.FP   <- INPUT.CASTAWAY.PROFILE.R[(Pres_max_row+1):length(INPUT.CASTAWAY.PROFILE.R$Time_s),]      #select data rows collected before max pressure reached

CASTAWAY.DOWNCAST.DATA.Pres_grad.R <- delta_y(-CASTAWAY.DOWNCAST.DATA$Pres_db)/delta_y(CASTAWAY.DOWNCAST.DATA$Time_s)

Grad.Critical.Threshold.FP <- 0.05
CASTAWAY.DOWNCAST.DATA.ROWS.PGGT.FP <- which(abs(CASTAWAY.DOWNCAST.DATA.Pres_grad.R) > Grad.Critical.Threshold.FP)+1  # identify row numbers with gradients > threshold
CASTAWAY.DOWNCAST.DATA.FP           <- CASTAWAY.DOWNCAST.DATA[CASTAWAY.DOWNCAST.DATA.ROWS.PGGT.FP,]
CASTAWAY.DOWNCAST.DATA.FP           <- CASTAWAY.DOWNCAST.DATA.FP[CASTAWAY.DOWNCAST.DATA.FP$Time_s > Downcast_begin_time,]

par(mar=c(5,4,0,1)+0.1)
plot(CASTAWAY.DOWNCAST.DATA.FP$Time_s,-CASTAWAY.DOWNCAST.DATA.FP$Pres_db,
     type="p",col="black",lwd=2,
     pch=16,cex=0.5,
     xlim=c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),
     ylim=c(-max(INPUT.CASTAWAY.PROFILE.R$Pres_db)-5,4),
     xlab="Time (s)",
     ylab="Pressure (db)",
     las=1)
     box(col="black", lwd=2)

     lines( c(0,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(0,0), col="blue",lwd=1,lty="solid")  # sea surface line
     #lines( c(Pres_max_time,Pres_max_time),c(0,-Pres_max), col="red",lwd=2)
     arrows( Pres_max_time,-6,Pres_max_time, -Pres_max, code=2, col="red", lwd=2, length=0.15) # max pressure arrow
     text(Pres_max_time,-4,"Press Max", col="red")
     lines(c(0,Pres_max_time),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dashed")
     text((0+Pres_max)/2,-Pres_max-3,"FP DOWNCAST",cex=0.75,col="blue")
     lines(c(Pres_max_time,max(INPUT.CASTAWAY.PROFILE.R$Time_s)),c(-Pres_max-1,-Pres_max-1), col="blue", lwd=2, lty="dotted")
     text((Pres_max + max(INPUT.CASTAWAY.PROFILE.R$Time_s))/2,-Pres_max-3,"FP PCAST",cex=0.75,col="blue")

     plottext <- paste("   n =",length(CASTAWAY.DOWNCAST.DATA.FP$Time_s),
                       "   Alternate Criteria: descent begin < time < press max & dp/dt >",Grad.Critical.Threshold.FP," dp/s")
     mtext(plottext, adj=0, side=3, line= -1.25, 	      
	     family="sans", font=3, cex=0.8)

# WILL NEED to CALCULATE DEPTH and SALINITY since data has not been binned 

# ---------------------------------------------------------------------------------
# Plot profiles of T, C associated with the Alternate DOWNCAST processing criteria
# ---------------------------------------------------------------------------------

cat("\n")
option_Y_N <- readline("PRESS ENTER to continue ")
cat(" \n")

win.graph()			# make a new graph window
par(mfrow=c(1,1))		# makes plots on page appear in 3 rows of 1 column

plot(CASTAWAY.DOWNCAST.DATA.FP$Temp_C,-CASTAWAY.DOWNCAST.DATA.FP$Pres_db,
     type="l",col="red",lwd=1,
     #pch=16,
     xlab="Temp (C)",
     ylab="Pres (db)",
     ylim=c(-max(CASTAWAY.DOWNCAST.DATA.FP$Pres_db)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n =",length(CASTAWAY.DOWNCAST.DATA.FP$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

# plot a histogram showing the number of records per 0.3 db pressure bins

hist(CASTAWAY.DOWNCAST.DATA.FP$Pres_db, 
     breaks = seq(from=0,to=max(CASTAWAY.DOWNCAST.DATA.FP$Pres_db)+0.3,by=0.3),
     freq=TRUE, # plots counts
     plot=TRUE,
     col="grey")

     # calculate the minimum and maximum counts

     hist(CASTAWAY.DOWNCAST.DATA.FP$Pres_db, 
          breaks = seq(from=0,to=max(CASTAWAY.DOWNCAST.DATA.FP$Pres_db)+0.3,by=0.3),
          freq=TRUE, # plots counts
          col="grey")

# bin average the data with equal weighting ie no weighting for descent rate since all descent rates adequate 

cut(CASTAWAY.DOWNCAST.DATA.FP$Pres_db,
    breaks=seq(from=0,to=max(CASTAWAY.DOWNCAST.DATA.FP$Pres_db)+0.3,by=0.3), # bins used by CASTAWAY
    labels=seq(from=0.15,to=max(CASTAWAY.DOWNCAST.DATA.FP$Pres_db),by=0.3),
    include.lowest=TRUE, right=TRUE)

# add line for bin averaged data

title(main=paste("CASTAWAY: Alternate Criteria DOWNCAST DATA"),
      outer=T, line=-1,   cex.main=1.3,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName.R[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

# ---------------------------------
# output the editing actions taken
# ---------------------------------
 
# remove last record because depth increment is different from the rest of the profile

# ------------------------------------------------------
# append the profile data to the CASTAWAY database file
# ------------------------------------------------------

cat("Append the profile data to a database \n")

# ---- ask if profile is to be added to the CASTAWAY database ---- 
                
# ---- add columns to the profile data.frame ----

# ---- add filename as a column to the profile data ----

Prof_ID          <- rep(INPUT.FileName[2],nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(INPUT.CASTAWAY.PROFILE,Prof_ID)

# ---- add processing data type to the profile data ----

CastType         <- rep(INPUT.ProcessingType[2],nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastType)

# ---- add year, month, day, hour, min, sec to tje profile data ----

CastDay <- noquote(substr(INPUT.CastTimeUTC[2],1,2))
CastDay <- rep(CastDay,nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastDay)

CastMn  <- noquote(substr(INPUT.CastTimeUTC[2],4,5))
CastMn  <- rep(CastMn,nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastMn)

CastYr  <- noquote(substr(INPUT.CastTimeUTC[2],7,10))
CastYr  <- rep(CastYr,nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastYr)

CastHr  <- noquote(substr(INPUT.CastTimeUTC[2],12,13))
CastHr  <- rep(CastHr,nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastHr)

CastMin <- noquote(substr(INPUT.CastTimeUTC[2],15,16))
CastMin  <- rep(CastMin,nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,CastMin)


# ----- add average of start and end lon to the profile data -----

Lon_Mean         <- rep((as.numeric(noquote(INPUT.StartLon[2])) + as.numeric(noquote(INPUT.EndLon[2])))/2,
                         nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,Lon_Mean)

# ----- add average of start and end lat to the profile data -----

Lat_Mean         <- rep((as.numeric(noquote(INPUT.StartLat[2])) + as.numeric(noquote(INPUT.EndLat[2])))/2,
                         nrow(INPUT.CASTAWAY.PROFILE))
CASTAWAY.PROFILE <- cbind(CASTAWAY.PROFILE,Lat_Mean)

# ------------------------------------------------------
# append the profile data to the CASTAWAY database file
# ------------------------------------------------------

# ---- determine if this is the first profile to add to the database ----

CASTAWAY.DATABASE <- CASTAWAY.PROFILE

# --------------------------------------------------------------------
# choose the CASTAWAY database file to add new profile to
# --------------------------------------------------------------------

INPUT.DATA.FILE.NAME    <- file.choose()
CASTAWAY.DATABASE <- read.table(INPUT.DATA.FILE.NAME,header=TRUE)

# ---- add new profile to the CASTAWAY database ----

#rbind(CASTAWAY.DATABASE,CASTAWAY.PROFILE)

# ---- write updated database to file ----

#write.table(INPUT.CASTAWAY.DATABASE,file=INPUT.DATA.FILE.NAME)

# --------------------------------------------------------------------
# smooth the profile using a 3 pt running median
# --------------------------------------------------------------------

Temp.smooth <- runmed(INPUT.CASTAWAY.PROFILE$Temp_C,k=3,endrule=c("median"))
Sal.smooth  <- runmed(INPUT.CASTAWAY.PROFILE$Sal_psu,k=3,endrule=c("median"))
Den.smooth  <- runmed(INPUT.CASTAWAY.PROFILE$Den_kgm3,k=3,endrule=c("median"))

# should density be recalculated from the smoothed T and S

# ---- define function to calculate density from T,S and P

# -------------------------------------------------------------------
# Generate and Plot the Profile of processed and median smoothed T,S 
# -------------------------------------------------------------------

win.graph()			# make a new graph window
par(mfrow=c(1,3))		# makes plots on page appear in 1 rows of 3 columns 

# ------ temperature profile ------

plot(INPUT.CASTAWAY.PROFILE$Temp_C,-INPUT.CASTAWAY.PROFILE$Depth_m,
     type="l",col="light grey",lwd=5,
     #pch=16,
     xlab="Temp (C)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("   n=",length(INPUT.CASTAWAY.PROFILE$Temp_C))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(Temp.smooth,-INPUT.CASTAWAY.PROFILE$Depth_m,col="black") # add smoothed line to plot of CASTAWAY output

# ------ salinity profile ------

plot(INPUT.CASTAWAY.PROFILE$Sal_psu,-INPUT.CASTAWAY.PROFILE$Depth_m,
     type="l",col="light grey",lwd=5,
     #pch=16,
     xlab="Sal (psu)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE$Sal_psu))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(Sal.smooth,-INPUT.CASTAWAY.PROFILE$Depth_m,col="black")

# ------ density profile ------

plot(INPUT.CASTAWAY.PROFILE$Den_kgm3,-INPUT.CASTAWAY.PROFILE$Depth_m,
     type="l",col="light grey",lwd=5,
     #pch=16,
     xlab="Den (kg/m3)",
     ylab="Depth(m)",
     ylim=c(-max(INPUT.CASTAWAY.PROFILE$Depth_m)-1,0),
     las=1)
     box(col="black", lwd=2)

plottext <- paste("     n=",length(INPUT.CASTAWAY.PROFILE$Den_kgm3))
mtext(plottext, adj=0, side=3, line= -1.25, 	      
	family="sans", font=3, cex=0.8)

lines(Den.smooth,-INPUT.CASTAWAY.PROFILE$Depth_m,col="black")

title(main=paste("CASTAWAY:", noquote(INPUT.ProcessingType[2]),"PROFILE DATA"),
      outer=T, line=-1,   cex.main=1.5,)
title(main=paste("filename(Instrument SN_YMD_HMS):",noquote(INPUT.FileName[2])),
      outer=T, line=-2.5, cex.main=1, font.main=3)

 
