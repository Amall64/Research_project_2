# Research_project_2
## Spatial point process analysis investigating EdU spatial association and cell proliferation in MCF-7 dataset 
### Overview and aims
This repository contains the code and analysis for my Bioinformatics MSc research project 2. It explores the spatial relationships between proliferating cells following the treatment of the Palbociclib drug. 

It uses synthetic data to generate and validate the pipeline. The real dataset tested includes microscopic images of MCF-7 breast cancer cell lines. 
The main question was: does EdU+ cells neighbour more than by chance?

The research aims to characterise the spatial locations of EdU+ cells, comparing patterns with complete spatial randomness (CSR), explore the relationship between neighbouring cells and daughter cells. 

## Dataset 

This includes spatial information from 26 microscopic images of MCF-7 cells. 
Dataset headings include:
- ImageID: identifies each image
- ObjectID: number of cells in that image
- Area: pixel area of the detected cells
- Mean_EdU: average pixel intensity of the edu signal
- Cell spatial co ordinates (X, Y)
- Edu pos thresholds

EdU positivity could be represented as 0, EdU negative and 1, EdU positive

## Methods 
This project was conducted in R. 
Methods include:
- Synthetic data generation
- Spatial point process analysis
- CSR
- Ripley's K/L function
- Monte Carlo envelopes

Image processing was conducted using Fiji/ImageJ software 


## Results 
Figures and statistical summaries are included in figures/ directories 

## Software and Packages
- R
- RStudio
- Fiji/ImageJ
- Spatstat
- ggplot2
- dplyr
- tidyr


Author 
Amal Abdi 
MSc Bioinformatics 
University of Bath 





























