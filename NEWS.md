# DanielBiostatistics10th 0.2.2
Release for R 4.4.0

# DanielBiostatistics10th 0.2.1
Removes import of package ggplot2

# DanielBiostatistics10th 0.2.0
Move some documentation to internal

# DanielBiostatistics10th 0.1.11
Annual update

# DanielBiostatistics10th 0.1.9
Remove unicode from plots

# DanielBiostatistics10th 0.1.8
LazyData enabled

# DanielBiostatistics10th 0.1.7
Misc minor updates

# DanielBiostatistics10th 0.1.6
Replace all ggplot2::geom_vline with ggrepel::geom_label_repel

# DanielBiostatistics10th 0.1.5
Minor improvements here and there.

# DanielBiostatistics10th 0.1.4
Add new S3 objects 'freqs', 'discreteDistBar', 'binom2pois', 'z_hypothesis', 'power_z'.

Add new S3 methods base::print, ggplot2::autolayer and ggplot2::autoplot for the new S3 objects.

# DanielBiostatistics10th 0.1.3
Improvement: ?predictiveValue now takes vector `prevalence` and returns vector `PVP` and `PVN`.  

Add S3 method: ?autoplot.predictiveValue

Change function names: old names `aggregate_z_test`, `aggregate_t_test`, `prop_test_CLT` and `aggregate_var_test` become ?aggregate_z, ?aggregate_t, ?prop_CLT and ?aggregate_var.

Change behavior: calculate confidence interval only, if `null.value` is missing, in ?prop_CLT, ?aggregate_z, ?aggregate_t and ?aggregate_var 

# DanielBiostatistics10th 0.1.2
Rename parameter `phat` of ?prop_test_CLT to `xbar`
Add function ?print_freqs and ?print_OE

# DanielBiostatistics10th 0.1.1
Remove package \pkg{raster} import.

# DanielBiostatistics10th 0.1.0
First release.
