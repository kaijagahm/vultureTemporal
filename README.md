# Identifying timescales of change in vulture social networks
### Kaija Gahm, Elvira D'Bastiani, Nili Anglister, Gideon Vaadia, Marta Acácio, Orr Spiegel, and Noa Pinter-Wollman

Data can be found on Dryad, DOI: 10.5061/dryad.mpg4f4rcf. The roost polygons and GPS coordinates have been shifted by a random amount in space in order to protect this sensitive species.

The GPS data are presented as separate CSV files--they need to be loaded into R and then bound together into a list, in numbered order, in order to replicate `alldata`. The may also need to be transformed back into an sf object (CRS 32636).

Data preparation was carried out in a `{targets}` pipeline, which can be found in the file `_targets.R`.

Subsequent analyses and figure preparations took place in the scripts contained in the `analysis_scripts/` folder. See `01_heuristic.qmd` for heuristic analysis, and `02_reducibility_curves.R` for multilayer reducibility analysis.

Multilayer reducibility analysis was conducted using a modified version of the [muxViz package](https://github.com/manlius/muxViz). The modified code can be found in the `muxviz_codebase.R` script and is adapted from Edoardo Pietrangeli and Sandra Smith Aguilar.

Figures can be found in the `fig/` folder. The code is packaged in an RStudio project, which can be accessed through the `vultureTemporal.Rproj` file. Packages needed are listed in `_targets.R`.
