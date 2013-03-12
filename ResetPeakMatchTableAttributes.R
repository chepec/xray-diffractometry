### Reset xtable attributes
ResetPeakMatchTableAttributes <- function(table.object) {
   names(table.object) <- 
      c("{No.}", #peak-kernel
        "{Phase}",
        "{$hkl$}",
        "{$\\Delta2\\theta$/\\si{\\degree}}",
        "{$2\\theta_0$/\\si{\\degree}}",
        "{Height/\\si{\\counts}}",
        "{Area/\\si{\\counts\\degree}}",
        "{FWHM/\\si{\\degree}}",
        "{$\\beta$/\\si{\\degree}}",
        "{$m$}",
        "{Accepted}")
   digits(table.object) <- 
      c(0, #row.names,
        0, #peakno
        0, #phase
        0, #hkl
        2, #deltathth
        2, #thth
        2, #height
        3, #area
        3, #fwhm
        3, #beta
        1, #m
        0) #accept
   display(table.object) <- 
      c("s", #row.names
        "s", #peakno
        "s", #phase
        "s", #hkl
        "e", #deltathth
        "f", #thth
        "e", #height
        "e", #area
        "f", #fwhm
        "f", #beta
        "f", #m
        "s") #accept
   align(table.object) <- 
      c("l", #row.names
        "c", #peakno
        "l", #phase
        "c", #hkl
        "S[table-format = +1.2e+2]", #deltathth
        "S[table-format = 3.2]",     #thth
        "S[table-format = 1.2e3]",   #height
        "S[table-format = 1.3e3]",   #area
        "S[table-format = 1.3]",     #fwhm
        "S[table-format = 1.3]",     #beta
        "S[table-format = 3.1]",     #m
        "c")                         #accept
   return(table.object)
}