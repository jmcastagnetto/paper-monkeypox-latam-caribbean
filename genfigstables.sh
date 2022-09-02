#! /bin/bash

echo ">>> Getting and processing latest data"
#Rscript 03-preproc-monkeypox-data.R
echo ">>> Calculating regression models"
Rscript 04-monkeypox-regmodels.R
echo ">>> Map of incidence by country"
Rscript fig01-monkeypox-map-current-incidence-country.R
echo ">>> Monkeypox timeline"
Rscript fig02-monkeypox-timeline.R
echo ">>> Incidence by subregion"
Rscript fig03-monkeypox-plot-incidence-by-subregion.R
echo ">>> Duplication time and Rt plot"
Rscript fig04ab-monkeypox-td-rt-plot.R
echo ">>> Supplementary materials PDF"
quarto render paper-supplementary-materials.qmd --quiet --to pdf
ls -lh  paper-supplementary-materials.pdf

echo
echo "Making the zip files"
cd output
rm figures.zip
zip -9 figures.zip *.tiff
cd ..
ls -l output/*
