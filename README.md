# Selection of departure times

Rscript to generate `.dbf` file with departure times for accessibility analyses with temporal resolution (e.g **GTFS**)

___

### Intro

This script was inspired by [Owen & Murphy (2018)](https://trid.trb.org/view/1497217) study and was developed when working on [Stępniak et al. (2019)](https://doi.org/10.1016/j.jtrangeo.2019.01.007) paper:

Stępniak, M., Pritchard, J.P., Geurs K.T., Goliszek S., 2019, *The impact of temporal resolution on public transport accessibility measurement: review and case study in Poland*, Journal of Transport Geography.

___

### Table of Contents
[Intro](#intro)<br>
[Description](#description)<br>
[Dependencies](#dependencies)<br>
[Function syntax](#function-syntax)<br>
[Function variables](#function-variables)<br>
[Output](#output)<br>
[Examples](#examples)<br>
[Funding statement](#funding-statement)<br>

___

### Description

`DepartureTime.R` is a script to generate departure times, using user-defined:

+ sampling method
+ date
+ time-window
+ path and file name

Supported sampling methods (in version 1.0):

+ **Systematic** sampling method: departure times are selected using a regular interval defined by the frequency 
+ **Simple Random** sampling method: a specified number of departure times (defined by the frequency) is randomly selected from the time window
+ **Hybrid** sampling method: departure times are randomly selected from given time intervals (resulted from applied temporal resolution)
+ **Constrained Random Walk Sampling** sampling method: a first departure time is randomly selected from the subset of the length defined by the frequency and beginning of the time window; then, the next departure time is randomly selected from the subset limited by $Tn+f/2$ and $Tn+f+f/2$

Examples (temporal resolution 20 minutes, time window 07:00 - 08:00)

Sampling method | Departure times | Comments
------------ | ------------- | -------------
**Systematic** | 07:00; 07:20, 7:40, 08:00 | regular interval of 20 minutes<sup>1</sup>
**Simple Random** | 07:18; 07:51; 07:55 | 3 randomly selected departure times from the time window<sup>2</sup>
**Hybrid** | 07:02; 07:23; 07:50 | One randomly selected departure time from each time interval period<sup>3</sup>
**Random Walk** | 07:15; 07:36; 07:49 | on average there should be 20-minute interval between departure times<sup>4</sup>


<sup>1</sup>  as 20-minute interval fits to 60 minute time window it provides 4 departure times.   
<sup>2</sup>  i.e. one per each 20 min. in 60-minute time window.  
<sup>3</sup>  i.e. one from 07:00-07:19, one from 07:20-07:39 and one from 07:40-07:59.  
<sup>4</sup>  due to the nature of the sampling procedure, the number of departure times might differ.

For details please consult [Owen & Murphy (2018)](https://trid.trb.org/view/1497217).



### Dependencies

+ `{foreign}` package needs to be installed (in order to save an output as dbf file).

### Function syntax

```
DepartureTime <- function(method = "H",
                          dy = format(Sys.Date(), "%Y"),  
                          dm = format(Sys.Date(), "%m"), 
                          dd = format(Sys.Date(), "%d"),
                          tmin = 0, tmax = 24,
                          res = 5,
                          MMDD = TRUE,
                          ptw = FALSE,
                          path = getwd(),
                          file = "DepTime")
```

### Function variables

+ `method` - sampling method; Options:
    + `R` OR `Random`: Simple random sampling;
    + `S` OR `Systematic`: Systematic sampling;
    + `H` OR `Hybrid`: Hybrid sampling;
    + `W` OR `Walk`: Constrained random walk sampling;
+ `dy`, `dm` and `dd` - date of the analysis (formats: YYYY, MM, DD); **default: system date**;
+ `tmin` and `tmax` - limits of the time window (format: HH); **default: full day** (00:00 - 24:00);
+ `res` - temporal resolution; **default: 5 minutes**
+ `MMDD` - date format of the output (TRUE / FALSE) **default: TRUE**
    + `TRUE`: MM/DD/YYYY;
    + `FALSE`: DD/MM/YYYY;
+ `ptw` - print limits of subsetted time-windows; **default: FALSE**;
+ `path` - path where the output will be saved (absolute or relative paths availables); **default: working directory**
+ `file` - name of the *.dbf* file, where departure times will be saved; **default: "DepTime"**

### Output

`.dbf` file which contains generated departure times (to be used e.g. in ArcGIS Network to generated ODs with time-dependent transport data, e.g. GTFS). File structure:

ID | Date
------------ | -------------
rowID (integer), starts with 0 | Departure date & hour
**Example** |   
0 | 05/14/2017  00:00
1 | 05/14/2017  00:09
2 | 05/14/2017  00:10

### Examples

```
DepartureTime() # working example, uses all default variables and hybrid sampling method
```

```
DepartureTime(method = "S",   # systematic sampling method
  dm = 5, dd = 15,  # user-defined date: 15th May, 2019 (current year)
  tmin = 7, tmax = 10,          # user-defined time window (07:00 - 10:00)
  res = 15,                     # user-defined temporal resolution (15 minutes)
  path = "Data/StartTime",      # user-defined relavive path and file name (next line)
  file = "DepTime_H15_0710")    # the file will be saved as: "Data/StartTime/DepTime_H15_0710.dbf" 
                                # (relative path from working directory)

```



___


### Funding statement

This document is created within the **MSCA CAlCULUS** project.  

*This project has received funding from the European Union's Horizon 2020 research and innovation Programme under the Marie Sklodowska-Curie Grant Agreement no. 749761.*  
*The views and opinions expressed herein do not necessarily reflect those of the European Commission.*


