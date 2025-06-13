# Ediam vFrontiers

This repository contains all supporting scripts and data sets required for replicating the analysis presented in Edmundo Molina-Perez et al, (2020) "Computational intelligence for studying sustainability challenges: tools and methods for dealing with deep uncertainty and complexity"

Scripts:
1. ClimateCalibration.r: describes the method for determining model's climate coefficients using GCMs raw data
2. Ediam_vFrontiers.r : contains mathematical structure of Ediam model an describes how the input from experimental design is processed into the model to estimate the output used in the analysis
3. Main_vFrontiers.r : describes the routines for a) creating the experimental design, b) estimating the optimal policy response across policy regimes, and c) data post-processing  for scenario discovery and data visualization.
4. sdprim_vFrontiers.r : describes routines and analyses carried out for applying the Patient Rule Induction Method (PRIM) algorithm to experimental results.

Data sets can be accessed in the following links:
1. Climate Data Calibration:
     Access link:  https://1drv.ms/u/s!AqjkGBjI6COCg7wb_krnz9SwRiFZ7g?e=fOp2gY
     Data sets: AllGCMs.csv
2. Experimental Design:
     Access link: https://1drv.ms/u/s!AqjkGBjI6COCg7wcph9J5LsRwp8dSw?e=qtVasC
     Data sets:  Exp.design.csv, Exp.design_P0.csv, Exp.design_P1.csv, Exp.design_P2.csv, Exp.design_P3.csv, Exp.design_P4.csv, Exp.design_P5.csv, Exp.design_P6.csv, Exp.design_P7.csv, Exp.design_P8.csv,  Climate.csv, Limits_original.csv, Policies.csv
3. Experimental Results:
     Access link: https://1drv.ms/u/s!AqjkGBjI6COCg7wduV1KXnkrI9ANng?e=ngW96L
     Data sets: model.runs.csv, prim.data.csv, robust_mapping.csv


Fabian´s version 
