[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/mit) [![Status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

# {athletegps}

### STATSports GPS R Package

## Intro

The athletegps package exists to allow users seamless access to thier STATSports data through a series of functions that act as wrappers around API queries. 
It helps democratize the ability to access and customize data, as well as easy the creation of advanced data science and analytical projects among sport science practitioners, researchers, and other advanced users. 
Information about how to use the athletegps package is below.

## Installation

```         
# Install from GitHub  
install.packages("devtools")
devtools::install_github("a-kikhia11/athletegps")
```

## Prerequisite

In order to be able to use this package and access your athlete and teams GPS data, you must first set this up within the SONRA Software. Refer to this link for further information:
https://elitesupport.statsports.com/hc/en-us/articles/13494019543965-Share-Data

## Set User Credentials

Once you get your ThirdPartyAPI Key from SONRA, copy it and store it within this function below.

```
set_credentials(api_key = "YOUR-API-KEY-HERE")
```

This will securely store your STATSports API credentials in the system keyring and saves non-sensitive configuration to a JSON config file within your home directory for reuse across R sessions.
You can check your configuration by running

```
get_config()
```

## Get All Available GPS Metrics

If your unsure of what GPS metrics are available, or what metrics you want to filter by, you can run this code below that returns a dataframe of all available metrics

```
get_metrics()
```

## Get Athlete Profiles

Once your credentials are set, you can get all athlete/player profile data by running

```
get_profiles()
```

## Get Session Data (via Session Date)

To get session data, run the code below including the session date as a parameter. You can optionally choose to automatically drilldown session data (default to TRUE; change to FALSE if you dont want to drilldown)

```
get_session(session_date = "YYYY-MM-DD", drills = TRUE, metrics = NULL)
```

You can also filter drilldown data by passing the metrics you want to the corresponding parameter, if no metrics are given then all metrics will be returned

```
get_session(session_date = "YYYY-MM-DD", drills = TRUE, metrics = c("AverageSpeed","DistancePerMin","DistanceTotal"))
```

## Get Raw Data of GPS Unit

You can also pull Raw GPS data using the corresponding `rawDataId` given for a player within a session using the code below

```
get_raw(rawDataId = "RAW-DATA-ID-HERE")
```

## Get ONLY Raw IMU Data of GPS Data

If you ONLY want the Raw IMU data then use the function below

```
get_imu_only(rawDataId = "RAW-DATA-ID-HERE")
```

## Citing this package

```         
To cite the package ‘athletegps’ in publications, use:

  Abdurrahman Kikhia (2025). athletegps: STATSports GPS R Package. R package version 1.0.0.
  https://github.com/a-kikhia11/athletegps
```
