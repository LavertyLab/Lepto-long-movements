# Lepto-long-movements/data
Data files used in analyzing and plotting lesser long-nosed bat (_Leptonycteris yerbabuenae_) long-distance movements defined as greater than 178 km (i.e., more than two standard deviations above the mean foraging distance reported by a GPS tracking study at Pinacate Cave; Goldshtein et al. 2020). 

## Tagging data (/raw-tagging-data/)
Tagging data (taggedLeptos.csv) includes information from bat captures and tagging. For each individual, the unique tag number in decimal, hexidecimal, and character format are included. We also include the site name, date of capture, species, age, sex, reproductive status of each individual on that date of capture, and contact information for the person responsible for those tagging events. Tag sites ending in "_h2o" indicate waterholes, while those ending in "_F" indicate foraging areas (either natural or hummingbird/oriole feeders). A list of tags with PIT tag detections at any monitored roost is also found in this folder (tags_w_detections_monitoredroosts.csv).

## Reader data (/raw-reader-data/)
Due to data sensitivity concerns, roost locations have been withheld, but distances between relevant roosts have been provided (roostdistance.csv). Additionally, given the size of our raw data files, we have only provided PIT tag reader data for the tagged bats that have made long-distance movements (PITtagreads.csv). 

## Pinacate tag loss data (/tag-loss/)
Tags recovered or detected on the floor of Pinacate cave are listed in a spreadsheet (pinacate_tag_loss.csv) along with their retrieval or detection date.

## Code
R script (process-bat-data.R) contains the code needed for generating Figure 1d, Table S3, and Table S4. We also include tag loss calculations for lesser long-nosed bats from Pinacate Cave and determining maximum length of time between tagging and last detection among bats that have made long-distance movements.

Literature Cited:
Goldshtein, A., M. Handel, O. Eitan, A. Bonstein, T. Shaler, S. Collet, S. Greif, R. A. Medellín, Y. Emek, and A. Korman. 2020. Reinforcement learning enables resource partitioning in foraging bats. Current Biology 30:4096–4102.