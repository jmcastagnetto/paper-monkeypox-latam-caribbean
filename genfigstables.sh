#! /bin/bash

echo ">>> Getting and processing latest data"
Rscript 03-preproc-monkeypox-data.R
echo ">>> Calculating regression models"
Rscript 04-monkeypox-regmodels.R
echo ">>> Map of incidence by country"
Rscript fig01-monkeypox-map-current-incidence-country.R
echo ">>> Evolution in LATAM & Caribbean"
Rscript fig02-monkeypox-map-evolution-incidence-latamcarib.R
# echo ">>> Monkeypox timeline"
# Rscript fig03-monkeypox-timeline.R
echo ">>> Incidence by subregion"
Rscript fig04-monkeypox-plot-incidence-by-subregion.R
echo ">>> LATAM & Caribbean incidence table"
Rscript tab01-monkeypox-tables-incidence.R
echo ">>> Duplication time plot"
Rscript fig05-monkeypox-duplication-time-plot.R
echo ">>> Rt plot"
Rscript fig06-monkeypox-rt-plot.R
echo ">>> Supplementary materials PDF"
quarto render paper-supplementary-materials.qmd --quiet --to pdf
ls -lh  paper-supplementary-materials.pdf

echo
echo "Making the zip files"
cd output
rm figures.zip
zip -9 figures.zip *.tiff
rm tables.zip
zip -9 tables.zip tab*
cd ..
ls -l output/*
