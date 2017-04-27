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
├── ine
│   └── Censo_Database.sav
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
    ├── IRS_Mozambique_clean - IRS_Maputo.csv
    ├── IRS_Mozambique_clean.xlsx
    └── ITN_Mozambique.xls

(15 directories, 134 files)

```

## Outputs

Running `master.R` will call the component code to generate the following data outputs (ie, cleaned, analyzable datasets), also in the `data` directory:

```
├── outputs
│   ├── cases_only.csv
│   ├── cases_population_weather.csv
│   ├── irs.csv
│   ├── itn.csv
│   ├── population.csv
│   ├── weather_daily.csv
│   └── weather_weekly.csv
```

## Data sources

### Outside data

Outside data include:

- `data/outside_data/Projeccoes_distritais_2007_20240`: Ministerio de Saude de Moçambique, Direcção Nacional de Saúde e Pública, Gestor de Dados e Assistente de M&A do Programa Nacional de Controlo da Malária
- `data/outside_data/surveillance_ministry`: Ministerio de Saude de Moçambique, Direcção Nacional de Saúde e Pública, Gestor de Dados e Assistente de M&A do Programa Nacional de Controlo da Malária
- `data/outside_data/vector_control_ministry`: Instituto Nacional de Estadística (projections available online)
