### Create peak matching table
InitialisePeakMatchTable <- function(peaktable) {
   peak.match.table <- 
      data.frame(stringsAsFactors = FALSE,
        peakno    = paste(peaktable$peak, peaktable$kernel, sep = ", "),
        phase     = paste("\\ce{", peaktable$formula, "}", sep = ""),
        hkl       = peaktable$hkl,
        deltathth = formatC(peaktable$deltathth, digits = 2, format = "e"),
        thth      = peaktable$thth,
        height    = peaktable$height,
        area      = peaktable$area,
        fwhm      = peaktable$fwhm,
        beta      = peaktable$beta,
        m         = peaktable$m,
        accept    = peaktable$accept)
   # Cosmetic fixes to peak.match.table
   #peak.match.table$phase[which(peak.match.table$phase == "\\ce{}")] <- ""
   peak.match.table$deltathth <- gsub("Inf", "{}", peak.match.table$deltathth)
   peak.match.table$accept <- gsub("FALSE", "No", peak.match.table$accept)
   peak.match.table$accept <- gsub("TRUE", "Yes", peak.match.table$accept)
   #
   return(peak.match.table)
}