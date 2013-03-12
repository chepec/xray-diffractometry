xrdml2df <- function(xrdml_source_path) {
   # Function for extracting information from XRDML files
   # 
   # NOTE: 
   #       
   #       
   # ARGS: xrdml_source_path (complete path and filename to XRDML file)
   # VALUE: list (xrd.list) with three dataframes
   #   xrd.data:
   #      angle:
   #      counts:
   #      att.factors:
   #      secperstep:
   #      cps:
   #      cpspermA:
   #      sampleid: *** (used as unique identifier column)
   #      substrateid:
   #      comment:
   #      usedWavelength:
   #      IB.radius:
   #      IB.xraytube:
   #      IB.xray.anode:
   #      IB.xray.tension:
   #      IB.xray.current:
   #      IB.focus.type:
   #      IB.focus.length:
   #      IB.focus.width:
   #      IB.takeoff.angle:
   #      IB.xray.mirror:
   #      IB.soller.slit:
   #      IB.beam.attenuator:
   #      sample.offsets
   #      DB.radius:
   #      DB.parallel.plate.collimator:
   #      DB.detector:
   #      scan.mode:
   #      scan.axis:
   #      start.time:
   #      end.time:
   #      application.software:
   #      control.software:
   #      intensities.unit:
   #      common.counting.time:
   #   axis.data:
   #      sampleid: *** (used as unique identifier column)
   #      axis:
   #      unit:
   #      startPosition:
   #      endPosition:
   #      commonPosition:
   #   xrdml.params:
   #      sampleid: *** (used as unique identifier column)
   #      substrateid:
   #      comment:
   #      usedWavelength:
   #      IB.radius:
   #      IB.xraytube:
   #      IB.xray.anode:
   #      IB.xray.tension:
   #      IB.xray.current:
   #      IB.focus.type:
   #      IB.focus.length:
   #      IB.focus.width:
   #      IB.takeoff.angle:
   #      IB.xray.mirror:
   #      IB.soller.slit:
   #      IB.beam.attenuator:
   #      sample.offsets
   #      DB.radius:
   #      DB.parallel.plate.collimator:
   #      DB.detector:
   #      scan.mode:
   #      scan.axis:
   #      start.time:
   #      end.time:
   #      application.software:
   #      control.software:
   #      intensities.unit:
   #      common.counting.time:
   #
   require(XML)
   require(devtools)
   dev_mode(on = TRUE)   
   require(lubridate)
   # lubridate::ymd_hms_o() not yet in the stable branch
   # we use it to convert the date-time strings into R date-time objects
   # Note: XML file uses ISO 8601 formatted date-time strings.
   #       see http://www.w3.org/TR/NOTE-datetime
   #       the regular lubridate::ymd_hms() cannot parse 8601, 
   #       but ymd_hms_o() specifically handles ISO 8601.
   
   options(stringsAsFactors = FALSE)
   xrdml.tree <- xmlInternalTreeParse(xrdml_source_path)
   xrdml <- xmlRoot(xrdml.tree)
   #
   
   sampleid = gsub("\\.xrdml", "", basename(xrdml_source_path))
   
   xrdml.params <-
   data.frame(substrateid = 
                 xmlValue(xrdml["sample"]$sample),
              sampleid = sampleid,
              comment = 
                 xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["comment"]$comment),
              usedWavelength = 
                 paste(as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$usedWavelength)), ": ",
                       xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$
                          usedWavelength["kAlpha1"]$kAlpha1), " ",
                       as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["usedWavelength"]$
                          usedWavelength["kAlpha1"]$kAlpha1)), 
                       sep = ""),
              IB.radius = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["radius"]$radius),
                       as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["radius"]$radius))),
              IB.xraytube = 
                 paste(xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube, name = "name"),
                       xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube, name = "id")),
              IB.xray.anode = 
                 xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["anodeMaterial"]$anodeMaterial),
              IB.xray.tension = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["tension"]$tension), 
                       unname(xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["incidentBeamPath"]$
                              incidentBeamPath["xRayTube"]$
                              xRayTube["tension"]$tension))),
              IB.xray.current = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["current"]$current), 
                       unname(xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["incidentBeamPath"]$
                              incidentBeamPath["xRayTube"]$
                              xRayTube["current"]$current))),
              IB.focus.type = 
                 as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                    xrdMeasurement["incidentBeamPath"]$
                    incidentBeamPath["xRayTube"]$xRayTube["focus"]$focus)),
              IB.focus.length = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["length"]$length),
                       as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["length"]$length))),
              IB.focus.width = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["width"]$width),
                       as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["width"]$width))),
              IB.takeoff.angle = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["takeOffAngle"]$takeOffAngle),
                       as.character(xmlAttrs(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayTube"]$xRayTube["focus"]$
                          focus["takeOffAngle"]$takeOffAngle))),
              IB.xray.mirror = 
                 paste(xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror, name = "name"),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal, name = "type"),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal, name = "shape"),
                       xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$xRayMirror["crystal"]$
                          crystal),
                       xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["acceptanceAngle"]$acceptanceAngle),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["acceptanceAngle"]$acceptanceAngle, 
                          name = "unit"),
                       xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["length"]$length),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["xRayMirror"]$
                          xRayMirror["length"]$length, 
                                  name = "unit")),
              IB.soller.slit = 
                 xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                            incidentBeamPath["sollerSlit"]$sollerSlit, name = "name"),
              IB.beam.attenuator = 
                 paste(xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["beamAttenuator"]$beamAttenuator, name = "name"),
                       xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["incidentBeamPath"]$
                          incidentBeamPath["beamAttenuator"]$beamAttenuator, name = "id")),
              sample.offsets = 
                 paste(xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, xmlGetAttr, "axis"),
                       xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, xmlValue, "axis"),
                       xmlSApply(xrdml["xrdMeasurement"]$
                          xrdMeasurement["sampleOffset"]$
                          sampleOffset, xmlGetAttr, "unit"),
                       collapse = ", "),
              DB.radius = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["radius"]$radius),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["radius"]$radius, name = "unit")),
              DB.parallel.plate.collimator = 
                 xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["diffractedBeamPath"]$
                            diffractedBeamPath["parallelPlateCollimator"]$
                            parallelPlateCollimator, name = "name"),
              DB.detector = 
                 paste(xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["detector"]$
                          detector, name = "name"),
                       xmlGetAttr(xrdml["xrdMeasurement"]$
                          xrdMeasurement["diffractedBeamPath"]$
                          diffractedBeamPath["detector"]$
                          detector, name = "xsi:type")),
              scan.mode = xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                    scan, name = "mode"),
              scan.axis = xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                    scan, name = "scanAxis"),
              start.time = 
                 ymd_hms_o(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                           scan["header"]$header["startTimeStamp"]$startTimeStamp)),
              end.time = 
                 ymd_hms_o(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                           scan["header"]$header["endTimeStamp"]$endTimeStamp)),
              application.software = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["applicationSoftware"]$applicationSoftware),
                       xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["applicationSoftware"]$applicationSoftware,
                          name = "version")),
              control.software = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["instrumentControlSoftware"]$
                          instrumentControlSoftware),
                       xmlGetAttr(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["header"]$header["source"]$
                          source["instrumentControlSoftware"]$
                          instrumentControlSoftware, name = "version")),
              intensities.unit = 
                 unname(xmlAttrs(xrdml["xrdMeasurement"]$
                                 xrdMeasurement["scan"]$scan["dataPoints"]$
                                 dataPoints["intensities"]$intensities)),
              common.counting.time = 
                 paste(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                          scan["dataPoints"]$dataPoints["commonCountingTime"]$
                          commonCountingTime), 
                       unname(xmlAttrs(xrdml["xrdMeasurement"]$
                              xrdMeasurement["scan"]$scan["dataPoints"]$
                              dataPoints["commonCountingTime"]$commonCountingTime))))
   
   dev_mode(on = FALSE)
   
   
   ## Read settings for all axes (2theta, omega, X, Y, Z, etc.)
   # number of axes (use as counter)
   axes.number <- length(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$scan["dataPoints"]$dataPoints["positions"])
   # initialise 
   axis.value <- list()
   axis.unit <- list()
   for (i in 1:axes.number) {
      # Fetch all values from the current axis (as named char array)
      axis.value[[i]] <- 
         data.frame(t(xmlSApply(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                scan["dataPoints"]$dataPoints["positions"][i]$positions, 
                                xmlValue, "axis")))
      # Fetch the axis and unit from the current axis (as named char array)
      axis.unit[[i]] <- 
         data.frame(t(xmlAttrs(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                         scan["dataPoints"]$dataPoints["positions"][i]$positions)))
   }
   
   # Try to condense axis.value into one dataframe
   # Ok, another approach. Concatetate all names of axis.value and axis.unit
   axis.all.names <- c(unique(names(unlist(axis.unit))), unique(names(unlist(axis.value))))
   axis.data <- data.frame(matrix(data = "", nrow = length(axis.unit), ncol = length(axis.all.names)))
   names(axis.data) <- axis.all.names
   
   # Loop over axis.value and axis.unit and move their contents into axis.data
   for (row in 1:length(axis.unit)) {
      # now loop over each column
      for (k in 1:dim(axis.unit[[row]])[2]) {
         axis.data[row, which(names(axis.data) == names(axis.unit[[row]])[k])] <-
            axis.unit[[row]][, k]
      }
      for (k in 1:dim(axis.value[[row]])[2]) {
         axis.data[row, which(names(axis.data) == names(axis.value[[row]])[k])] <-
            axis.value[[row]][, k]
      }
   }
   
   # Add sampleid column to axis.data
   axis.data$sampleid <- sampleid
   
   
   # Read the intensities data (one long text string, use textConnection)
   zz <- textConnection(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                 scan["dataPoints"]$dataPoints["intensities"]$intensities), "r")
   intensities <- scan(zz, what = numeric())
   close(zz)
   
   # Read the beam attenuation factors (one long text string, use textConnection)
   zz <- textConnection(xmlValue(xrdml["xrdMeasurement"]$xrdMeasurement["scan"]$
                                 scan["dataPoints"]$dataPoints["beamAttenuationFactors"]$beamAttenuationFactors), "r")
   beam.attenuation.factors <- scan(zz, what = numeric())
   close(zz)
   
   
   ## Use the identified scanAxis (in xrdml.params) to identify the current start and stop values
   start.currentaxis <- as.numeric(axis.data[which(axis.data$axis == xrdml.params$scan.axis), "startPosition"])
   end.currentaxis <- as.numeric(axis.data[which(axis.data$axis == xrdml.params$scan.axis), "endPosition"])
   
   exp.data <- data.frame(angle = seq(start.currentaxis, end.currentaxis, length.out = length(intensities)),
                          counts = intensities,
                          att.factors = beam.attenuation.factors)

   # attenuation factors not used for anything, currently  
   #exp.data$counts <- exp.data$intensities * exp.data$att.factors
   exp.data$secperstep <- as.numeric(unlist(strsplit(xrdml.params$common.counting.time, " "))[1])
   exp.data$cps <- exp.data$counts / as.numeric(unlist(strsplit(xrdml.params$common.counting.time, " "))[1])
   exp.data$cpspermA <- exp.data$cps / as.numeric(unlist(strsplit(xrdml.params$IB.xray.current, " "))[1])
   
   
   xrd.data <- cbind(exp.data,
                     sampleid = sampleid,
                     xrdml.params)
   
   # Return xrd.data, axis.data and xrdml.params in a list
   xrd.list <- list(xrd.data = xrd.data, 
                    axis.data = axis.data,
                    xrdml.params = xrdml.params)
      
   return(xrd.list)
}
