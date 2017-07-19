# MALTEM effectiveness papers

www.economicsofmalaria.com

## What

This repository contains data, code, documentation
and writing pertaining to the economic evaluation
of the interventions carried out by the
Mozambican Alliance Towards the Elimination 
of Malaria (MALTEM). 

## Who

This repository is maintained by [Joe Brew](mailto:joebrew@gmail.com). Collaborators include:

- Laia Cirera
- Menno Pradhan
- Elisa Sicuri
- Ranjeeta Thomas

## Configuration

In order to reproduce all the analysis for this project, you'll need to do two things first.

1. Create a `credentials/credentials.yaml` with the following parameters. Authorized collaborators can request credentials from joebrew@gmail.com:

```
dbname: xxx
host: xxx
port: xxx
user: xxx
password: xxx
redcap_name: xxx
redcap_password: xxx
openclinica_name: xxx
openclinica_password: xxx
```

2. Ensure that your `data` folder is populated with the following structure. Authorized collaborators can request data from joebrew@gmail.com

```
├── binaries
├── ine
│   └── Censo_Database.sav
├── outputs
├── Projeccoes_distritais_2007_20240
│   ├── Cabo_delgado_.xlsx
│   ├── Gaza - Distritos.xls
│   ├── Inhambane 1   - Distritos.xls
│   ├── Manica   - Distritos.xls
│   ├── Maputo - Cidade -   - Distritos.xls
│   ├── Nampula__distritos.xlsx
│   ├── Niassa_distritos.xls
│   ├── Província  Maputo - - Distritos.xls
│   ├── Sofala    - Distritos.xlsx
│   ├── TETE_.xls
│   └── Zambézia.xls
├── surveillance_ministry
│   ├── BES_Gaza
│   │   ├── modulo_basico
│   │   │   ├── 2010
│   │   │   │   ├── Bilene.xlsx
│   │   │   │   ├── Chibuto.xlsx
│   │   │   │   ├── Chicualacuala.xlsx
│   │   │   │   ├── Chigubo.xlsx
│   │   │   │   ├── Chokwe.xlsx
│   │   │   │   ├── Distrito XaiXai.xlsx
│   │   │   │   ├── Guija.xlsx
│   │   │   │   ├── Mabalane.xlsx
│   │   │   │   ├── Manjacaze.xlsx
│   │   │   │   ├── Massangena.xlsx
│   │   │   │   ├── Massingir.xlsx
│   │   │   │   └── XaiXai.xlsx
│   │   │   ├── 2011
│   │   │   │   ├── Bilene.xlsx
│   │   │   │   ├── Chibuto.xlsx
│   │   │   │   ├── Chicualacuala.xlsx
│   │   │   │   ├── Chigubo.xlsx
│   │   │   │   ├── Chokwe.xlsx
│   │   │   │   ├── Distrito XaiXai.xlsx
│   │   │   │   ├── Guija.xlsx
│   │   │   │   ├── Mabalane.xlsx
│   │   │   │   ├── Manjacaze.xlsx
│   │   │   │   ├── Massangena.xlsx
│   │   │   │   ├── Massingir.xlsx
│   │   │   │   └── XaiXai.xlsx
│   │   │   ├── 2012
│   │   │   │   ├── Bilene.xlsx
│   │   │   │   ├── Chibuto.xlsx
│   │   │   │   ├── Chicualacuala.xlsx
│   │   │   │   ├── Chigubo.xlsx
│   │   │   │   ├── Chokwe.xlsx
│   │   │   │   ├── D Xaixai.xlsx
│   │   │   │   ├── Guija.xlsx
│   │   │   │   ├── Mabalane.xlsx
│   │   │   │   ├── Manjacaze.xlsx
│   │   │   │   ├── Massangena.xlsx
│   │   │   │   ├── Massingir.xlsx
│   │   │   │   └── XaiXai.xlsx
│   │   │   └── 2013
│   │   │       ├── Bilene.xlsx
│   │   │       ├── Chibuto.xlsx
│   │   │       ├── Chicualacuala.xlsx
│   │   │       ├── Chigubo.xlsx
│   │   │       ├── Chokwe.xlsx
│   │   │       ├── D XaiXai.xlsx
│   │   │       ├── Guija.xlsx
│   │   │       ├── Mabalane.xlsx
│   │   │       ├── Manjacaze.xlsx
│   │   │       ├── Massangena.xlsx
│   │   │       ├── Massingir.xlsx
│   │   │       └── XaiXai.xlsx
│   │   └── SIS-MA
│   │       ├── BES_GAZA_2014_2016.xlsx
│   │       └── SIS-MA_2016-17.xls
│   ├── BES_Maputo
│   │   ├── Modulo Básico
│   │   │   ├── Boane 2010.xlsx
│   │   │   ├── Boane 2011.xlsx
│   │   │   ├── Boane 2012.xlsx
│   │   │   ├── Boane 2013.xlsx
│   │   │   ├── Boane 2014.xlsx
│   │   │   ├── Boane 2015.xlsx
│   │   │   ├── Boane 2016.xlsx
│   │   │   ├── Magude 2010.xlsx
│   │   │   ├── Magude 2011.xlsx
│   │   │   ├── Magude 2012.xlsx
│   │   │   ├── Magude 2013.xlsx
│   │   │   ├── Magude 2014.xlsx
│   │   │   ├── Magude 2015.xlsx
│   │   │   ├── Magude 2016.xlsx
│   │   │   ├── Manhica 2010.xlsx
│   │   │   ├── Manhica 2011.xlsx
│   │   │   ├── Manhica 2012.xlsx
│   │   │   ├── Manhica 2013.xlsx
│   │   │   ├── Manhica 2014.xlsx
│   │   │   ├── Manhica 2015.xlsx
│   │   │   ├── Manhica 2016.xlsx
│   │   │   ├── Marracuene 2010.xlsx
│   │   │   ├── Marracuene 2011.xlsx
│   │   │   ├── Marracuene 2012.xlsx
│   │   │   ├── Marracuene 2013.xlsx
│   │   │   ├── Marracuene 2014.xlsx
│   │   │   ├── Marracuene 2015.xlsx
│   │   │   ├── Marracuene 2016.xlsx
│   │   │   ├── Matola 2010.xlsx
│   │   │   ├── Matola 2011.xlsx
│   │   │   ├── Matola 2012.xlsx
│   │   │   ├── Matola 2013.xlsx
│   │   │   ├── Matola2014.xlsx
│   │   │   ├── Matola 2015.xlsx
│   │   │   ├── Matola 2016.xlsx
│   │   │   ├── Matutuine 2010.xlsx
│   │   │   ├── Matutuine 2011.xlsx
│   │   │   ├── Matutuine 2012.xlsx
│   │   │   ├── Matutuine 2013.xlsx
│   │   │   ├── Matutuine 2014.xlsx
│   │   │   ├── Matutuine 2015.xlsx
│   │   │   ├── Matutuine 2016.xlsx
│   │   │   ├── Matutuione 2013.xlsx
│   │   │   ├── Moamba 2010.xlsx
│   │   │   ├── Moamba 2011.xlsx
│   │   │   ├── Moamba 2012.xlsx
│   │   │   ├── Moamba 2013.xlsx
│   │   │   ├── Moamba 2014.xlsx
│   │   │   ├── Moamba 2015.xlsx
│   │   │   ├── Moamba 2016.xlsx
│   │   │   ├── Namahacha 2010.xlsx
│   │   │   ├── Namahacha 2011.xlsx
│   │   │   ├── Namahacha 2012.xlsx
│   │   │   ├── Namahacha 2013.xlsx
│   │   │   ├── Namahacha 2014.xlsx
│   │   │   ├── Namahacha 2015.xlsx
│   │   │   └── Namahacha 2016.xlsx
│   │   └── SIS-MA 2016_2017
│   │       ├── BES de 2016 SIS-MA.xls
│   │       └── BES de 2017 SIS-MA.xls
│   └── district_names_matcher.csv
└── vector_control_ministry
    ├── irs_district_matcher.csv
    ├── IRS_Maputo Province_clean.xlsx
    ├── IRS_Mozambique_clean.csv
    ├── IRS_mozambique_clean_final.csv
    ├── IRS_mozambique_clean_final.xlsx
    ├── IRS_Mozambique_clean - IRS_Maputo.csv
    ├── IRS_Mozambique_clean.xlsx
    ├── ITN_Mozambique_clean.csv
    ├── ITN_Mozambique_clean.xls
    ├── ITN_Mozambique.csv
    └── ITN_Mozambique.xls
```

## Outputs

Running `master.R` will call the component code to generate the following data outputs (ie, cleaned, analyzable datasets), in the `data/outputs` directory:

```
├── cases_only.csv
├── cases_population_weather.csv
├── cases_population_weather_itn_irs.csv
├── irs.csv
├── itn.csv
├── population.csv
├── weather_daily.csv
└── weather_weekly.csv
```

## Headers

Running `master.R` produces a data.frame named `df`, which is the tabular equivalent of "cases_population_weather_itn_irs.csv". Below if a an explanation of each column.

- `year`: The year of the observation.  
- `week`: The week of the observation.  
- `date`: The date of the Saturday in the week of the observation.  
- `month`: The (numeric) month of the observation.  
- `day`: The day of the month of the observation.  
- `province`: The province of the observation; one of "Gaza" or "Maputo".
- `district`: The district of the observation.  
- `age_group`: The age group of the observation. Groups are 0 to 4 years of age, and 5+ years of age.  
- `disease`: The disease of the number of cases in question. Diseases are one of "malaria" or "diarrhea".  
- `cases`: The number of cases of the observation.   
- `population`: The population of the age group in question, estimated using INE figures at the yearly level.   
- `p`: The "proportion", ie, the number of cases divided by the population.  
- `pk`: The above "proportion" multiplied by 1,000 (ie, cases per 1,000).  
- `itn_coverage`: The number of insecticide treated nets (ITNs) distributed in that district in the year of the observation as a percentage of the population of that district in the year of the observation. In some cases, this exceeds 100% do to difference in population estimates by the INE and the MoH.  
- `irs_houses`: The number of houses which were subject to indoor residual spraying (IRS) in the last IRS campaign in that district. This variable should be considered only in conjunction with the "weeks_since_last_irs_campaign_end" variable.   
- `irs_people`: The number of people who resided in the houses described in the above variable.  
- `weeks_since_last_irs_campaign_end`: The number of weeks since the end of the last IRS campaign in that district. If `NA`, this means that we have no record of a previous IRS campaign; if 0, this means that the date in question was _during_ and IRS campaign; if greater than 0, this means that the date in question was _after_ an IRS campaign, and the `irs_houses`, `irs_people`, `irs_coverage_houses`, and `irs_coverage_people` variables all pertain to the campaign which took place **n** weeks prior to the date of this observation ("n" being the number in this column).  
- `irs_coverage_houses`: `irs_houses` divided by the `population` of the entire district at the time of the campaign, multiplied by 100. In some cases, this exceeds 100% do to difference in population estimates by the INE and the MoH.  
- `irs_coverage_people`: `irs_people` divided bythe `population` of the entire district at the time of the campaign, multiplied by 100 (ie, the percentage of people that were covered in the last IRS campaign). In some cases, this exceeds 100% do to difference in population estimates by the INE and the MoH.  
- `precipitation`: The total rainfall, in milimeters, for the district during the week in question. Note that this, as with _all_ the weather data, is interpolated based on NOAA weather stations.  
- `temp`: The average temperature of the week in question, in celcius.  
- `temp_max`: The maximum temperature of the week in question, in celcius.  
- `temp_min`: The minimum temperature of the week in question, in celcius.  
- `dew_point`: The average dew point of the week in question, in celcius.  
- `wind_speed`: The average wind speed of the week in question, in miles per hour.  
- `wind_speed_max`: The maximum wind speed of the week in question, in miles per hour.  
- `irs_protection`: The level at which the population is estimated to be protected from IRS. This is a function of both (a) the coverage % of the last campaign and (b) the time since the last campaign.
- `distance_to_land_border`: The number of kilometers from the centroid of the district to the nearest border with either Swaziland or South Africa (as the bird flies / euclidean).

## Data sources

### Outside data

Outside data include:

- `data/outside_data/Projeccoes_distritais_2007_20240`: Ministerio de Saude de Moçambique, Direcção Nacional de Saúde e Pública, Gestor de Dados e Assistente de M&A do Programa Nacional de Controlo da Malária
- `data/outside_data/surveillance_ministry`: Ministerio de Saude de Moçambique, Direcção Nacional de Saúde e Pública, Gestor de Dados e Assistente de M&A do Programa Nacional de Controlo da Malária
- `data/outside_data/vector_control_ministry`: Instituto Nacional de Estadística (projections available online)

### Weather data

We retrieved weather data for all stations in Mozambique for the last decade from NOAA (https://www.ncdc.noaa.gov/cdo-web/). The "raw" data consist of the 3 `.txt` files at https://github.com/joebrew/maltem_cost_effectiveness/tree/master/noaa_data.

We estimated the meterological conditions at the centroid of each district to produce `mozambican_weather.csv` (also at the above link). The code for this is estimation is at https://github.com/joebrew/maltem_cost_effectiveness/blob/master/get_weather_data.R. 

The R function `get_weather_for_location` (https://github.com/joebrew/maltem_cost_effectiveness/blob/master/helpers.R#L85) contains the details of the interpolation method used to estimate conditions. This estimation was necessary since the locations of the weather stations do not coincide with the centroids of the districts. 

The method for interpolation was simple. For each district centroid on each date and for each weather indicator (temperature, humidity), etc, a weighted mean of that indicator was calculated from _all_ Mozambican weather stations, with the weight being equivalent to 1 divided by the distance (in kilometers) from the district's centroid to each station. In other words, a station 5 kilometers from a district centroid would get a weight of 1/5 (0.2) whereas a station 100 kilometers from a district centroid would get a weight of 1/100 (0.01). 