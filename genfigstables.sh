#! /bin/bash

Rscript 03-preproc-monkeypox-data.R
Rscript fig01-monkeypox-map-current-incidence-country.R
Rscript fig02-monkeypox-map-evolution-incidence-latamcarib.R
Rscript fig03-monkeypox-timeline.R
Rscript fig04-monkeypox-plot-incidence-by-subregion.R
Rscript tab01-monkeypox-tables-incidence.R

cd output
rm figures.zip
zip -9 figures.zip *.tiff
cd ..
ls -l output/*
