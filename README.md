xray-diffractometry
===================

Background and peak fitting of x-ray diffractograms with R and the diffractometry package

Workflow
========
- Record an x-ray diffractogram with a diffractometer of your choice
- Export your diffractogram as ASCII with angles vs counts (should be actual counts, i.e., integer values)
- Read your x and y values into an R dataframe using a script suitable for the format of your ASCII-file
- Strip the Cu Ka2 contribution from your diffractogram ***not yet implemented***
- Fit the background using the baselinefit() function of the diffractometry package
- Fit the peaks using the pkdecompint() function of the diffractometry package
- Plot your experimental diffractogram, the fitted background and the fitted peaks using ggplot2
- Make a nice LaTeX table with all the fitted peaks and their physical parameters
- Compare your diffractogram's peaks to reference diffractograms
