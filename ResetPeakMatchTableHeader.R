### Reset xtable longtable headers
ResetPeakMatchTableHeader <- function(xmat, caption.text, caption.label) {
   ltab.header <- 
      paste(paste("\\caption{", caption.text, "}", sep = "", collapse = ""),
            paste("\\label{", caption.label, "}\\\\ ", sep = "", collapse = ""),
            "\\toprule ",
            attr(xmat, "names")[1],
            paste(" &", attr(xmat, "names")[2:length(attr(xmat, "names"))], collapse = ""),
            "\\\\\\midrule ",
            "\\endfirsthead ",
            paste("\\multicolumn{",
                  ncol(xmat),
                  "}{c}{{\\tablename\\ \\thetable{} -- continued from previous page}}\\\\ ",
                  sep = ""),
            "\\toprule ",
            attr(xmat, "names")[1],
            paste("&", attr(xmat, "names")[2:length(attr(xmat, "names"))], collapse = ""),
            "\\\\\\midrule ",
            "\\endhead ",
            "\\midrule ",
            paste("\\multicolumn{", 
                  as.character(ncol(xmat)),
                  "}{r}{{Continued on next page}}\\\\ ",
                  sep = "", collapse = ""),
            "\\bottomrule \\endfoot ",
            "\\bottomrule \\endlastfoot ",
            collapse = "")
   return(ltab.header)
}