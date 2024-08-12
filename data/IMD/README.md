[OHID](https://fingertips.phe.org.uk/search/deprivation%20index#page/4/gid/1/pat/159/par/K02000001/ati/15/are/E92000001/iid/93553/age/1/sex/4/cat/-1/ctp/-1/yrr/1/cid/4/tbm/1) provides the [Fingertips API](https://fingertips.phe.org.uk/api), which includes practice-level IMD values. However, this data only includes practices that are currently open. We obtained the [data](https://github.com/camappel/HEEC/blob/main/data/IMD/IMD.csv) for GP practices for which IMD score was available at any point directly from the DHSC; the raw file is stored [here](https://github.com/camappel/HEEC/blob/main/data/IMD/IMD_fingertips.csv).

This data only provides values for 2010, 2015, and 2019. We interpolate between the avaiable values to produce a continuous time series from x - 2023.

