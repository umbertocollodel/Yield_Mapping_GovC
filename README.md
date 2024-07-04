# Yield_Mapping_GovC




## Description

This tool aims at assessing the impact quasi real-time of the announcement of the Governing Council’s monetary policy decision on financial markets and, hence, on the monetary policy stance.


## Author

- Umberto Collodel

## Language

R


## Organization

0.0_create_factordata Cleans data from EA-MPD (Altavilla,2019) to prepare for factor extraction.
The code retrieves EA-MPD data from a raw_data directory. 

0.0create_factor Calculates factors scorers and loadings for EA-MPD and extends the methodology to new GovC observations.
To do so, you need to

1) insert the new data manually in the additional_relesase excel workbook, with day and following day

<img width="305" alt="Screenshot 2024-07-04 141346" src="https://github.com/umbertocollodel/Yield_Mapping_GovC/assets/33840988/9b0e8e33-cbaa-4c2f-9a22-4f903c5ae8f2">

2) change the last three parameters of the custom add_risk_free function. Explanation in the function documentation 

![Screenshot 2024-07-04 141719](https://github.com/umbertocollodel/Yield_Mapping_GovC/assets/33840988/00072b8d-5656-48e9-927c-a1d57a227cc8)

and example of how the function is now running

![Screenshot 2024-07-04 141700](https://github.com/umbertocollodel/Yield_Mapping_GovC/assets/33840988/a2ecb068-0267-4755-b94a-bb73aa6a6003)


In the app folder, 02run_app deploys the app with the new observations on local machine.

The app looks as below and a description is embedded:

<img width="941" alt="Screenshot 2024-06-24 120047" src="https://github.com/umbertocollodel/Yield_Mapping_GovC/assets/33840988/4f838dcb-211f-4f6b-b253-efab3f66bf4d">

Resulting files and images can be exported easily.


## License

The data and codes are under the MIT license. This means that you can use everything as you please for research or commercial purposes, as long as you refer back to us.

## Contributing

If you find irregularities or bugs, please open an issue here.
