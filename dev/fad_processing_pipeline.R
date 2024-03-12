###################################
####### Entire processing pipeline for the paper
###################################

find_project_root <- function() {
  # Start in the current directory
  path <- getwd()

  while (TRUE) {
    # Look for a .Rproj file in the current directory
    files <- list.files(path, pattern = "\\.Rproj$", full.names = TRUE)

    if (length(files) > 0) {
      # If found, return the directory containing the .Rproj file
      return(dirname(files[1]))
    }

    # If not found, go to the parent directory
    parent <- dirname(path)

    if (parent == path) {
      # If there's no parent directory, stop searching
      stop("No .Rproj file found")
    }

    path <- parent
  }
}

basedir <- find_project_root()

###################################
# 1. tidy raw data and generate uber_df.

source(file.path(basedir, "code/preprocessing/pipeline/tidy_raw_gen_uber_df.R"))

## This script reads in all of the raw facereader files from roman and compiles them into the og uber_df file.
## Just pulls everything into a single object for further processing.
## In this script I also seemed to remove a number of duplicate files, but dont seem to have resolved where they all came from. I do remember them seeming to line up though so I think they are truly just duplicates.
## 12/19/21 added "dx3" codes into this file (1 = SZ, 2 = OP, 3 = HC)..
## 1/21/22 change raw data file from 5122 to 5222 re: email from Roman on subject id error. Changes can be found here /proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/sub5222.R
## 1/22/22 add additional function read_dat2 which utilizes fread functionality and skips txt header

###################################
# 2. Basic combination, and interpolation, and calculation/visualization of descriptive statistics per subject.

source("/proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/run_descriptives.R")

## Combines all subjects that have multiple videos into the same file, separating each by 1000 empty samples
## Interpolates over missing samples, via stine interpolation in tsimpute package. maximum gap between interpolated timepoints is 30 samples (1 sec).
## Caclulate frame-by frame entropy.
## Generate descriptive statistics per subject, plot descriptives in giant forest plots.

## DONE make sure complete matches across entropy and other emotions


###################################
# 3. Plot subject-level timecourse of emotion and circumplex timeseries.

source("/proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/plot_subject_timecourses.R")

## TODO need to tweak code to include entropy, and lump together circumplex measures.
    # 1/1/22 kinda done but needs to be tidied up before considering as a figure.

###################################
# 4. Exclude subjects with too few time points or very low percentage present

source("/proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/exclude_badsubs.R")

## Initial n going into this script: 240
## Drop 28 subjects who have less than 5 min of complete data or less than 10% complete data

###################################
# 5. Downsample data on good subjects (dropping those that were previously excluded)

source("/proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/downsample_full.R")

## Performs 3 types of downsampling
##  - simple: keep every n observation
##  - mean: compute the mean of each subsequent chunk of n frames
##  - mean_narm: same as mean, only if there are missing frames tagged as NA, will return the mean of those chunks that are present (even if only 1 sample)

## mean_narm performs the best at maximizing and preserving the typically choppy temporal structure of the data. This is what subsequent analyses are based on.

## downsampled at rates of 15, 30, 45, 60, 75, 90. (.5-3 seconds per bin)

###################################
# 5.5 Update wonky subject number

source("/proj/mnhallqlab/users/nate/facial_affect_dynamics/code/preprocessing/pipeline/sub5222.R")

## at some point during the project, Roman let me know that one subject (5222) was labelled improperly as 5122 in the data so we went through and renamed all previously generated files.

###################################
# 6.


load("data/uber_df_withtimecourses_goodsubs.RData")
