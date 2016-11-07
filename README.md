# sapflux_calc_larix

Code associated with sapflux data and analysis from data collected in the Y4 watershed _Larix cajanderii_ in northeastern Siberia. _sapflow_calc.R_ is a R script for converting raw data on the change in temperature from sapflux probes. _OpenBUGS_model_code.txt_ is a model script for a Bayesian model of canopy stomatal conductance in OpenBUGS. 

### sapflow_calc.R
Script in R for reading in raw sapflux data and calculating canopy transpiration and stomatal conductance. This script also includes initial data flags and a check to look at radial differences in sapflow.



### OpenBUGS_model_code
OpenBUGS model script for a Bayesian model that combines two phenomonological models for canopy stomatal conductance. A sub model for a stochastic estimation of antecedent precipitation is included. 

###### Citations from sapflux calculation script

**Alexander, H. D.**, _et. al_ _(2012)_, Carbon accumulation patterns during post-fire succession in Cajander Larch (Larix cajenderi) forests of Siberia, Ecosystems, 15, 1065–1082, doi:10.1007/s.

**Clearwater, M. J.**_et. al._ _(1999)_, Potential errors in measurement of nonuniform sap flow using heat dissipation probes, Tree Physiol., (19), 681–688.


**Ewers, B. E.**, and R. Oren _(2000)_, Analyses of assumptions and errors in the calculation of stomatal conductance from sap flux measurements., Tree Physiol., 20(9), 579–589, doi:10.1093/treephys/20.9.579.

**Lu, P.**, _et. al._ _(2000)_, Spatial variations in xylem sap flux density in the trunk of orchard-grown, mature mango trees under changing soil water conditions, Tree Physiol., 20(10), 683–692, doi:10.1093/treephys/20.10.683.

**Pearcy, R. W.**, _et. al._ _(1989)_. "Measurement of transpiration and leaf conductance." Plant physiological ecology. Springer Netherlands, 137-160.

**Wang, C.**, _et. al._ _(2001)_, The influence of fire on carbon distribution and net primary production of boreal Larix gmelinii forests in north-eastern China, Glob. Chang. Biol., 7(6), 719–730, doi:10.1046/j.1354-1013.2001.00441.x.

###### Citation from canopy stomatal conductance model code

**Oren, R.**, _et. al._ _(1999)_, Survey and synthesis of intra- and interspecific variation in stomatal sensitivity to vapour pressure deficit, 1515–1526.

**Ogle, K.**, _et. al._ _(2015)_, Quantifying ecological memory in plant and ecosystem processes, Ecol. Lett., 18(3), 221–235, doi:10.1111/ele.12399.
