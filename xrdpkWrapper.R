xrdpkWrapper <- 
   function(data.exp, 
            run, 
            override = FALSE, 
            kerpk = 1, 
            fitmaxiter = 50, 
            gam = 1.0, 
            scl.factor = 1.2, 
            tau = 2.5,
            maxwdth = 5.0) { 
   # the override flag IS IN USE
      
   print("... Started xrdpkWrapper")
      
   # check if xrdpk has already completed successfully for the current job
   current.dirname <- getwd()
   print(current.dirname)
   current.filename <- "xrd-peak-data.rda"
   xrddatafile <- paste(current.dirname, current.filename, sep = "/")
   
   
   
   if (file.exists(xrddatafile) && !override) {
      # If file DOES EXIST AND override flag is FALSE
      print("... Started if-clause 1")
      
      # Load the existing data from file
      load(file = xrddatafile)
      
      # Only run the peak-fitting algorithm if 
      # <run> is higher than what the file contains
      if (run > length(xrdres)) {
         print("... Started if-clause 1:1")
         xrdres[[run]] <- xrdpk(data.exp, 
                                kerpk = kerpk, 
                                fitmaxiter = fitmaxiter, 
                                gam = gam, 
                                scl.factor = scl.factor,
                                tau = tau,
                                maxwdth = maxwdth)
         save(xrdres, file = xrddatafile)
         print("... Ended if-clause 1:1")
      }
      
      print("... Ended if-clause 1")
   }
   
   if (file.exists(xrddatafile) && override) {
      # If file DOES EXIST AND override flag is TRUE
      print("... Started if-clause 2")
      
      # Load the existing data from file
      load(file = xrddatafile)
      
      xrdres[[run]] <- xrdpk(data.exp, 
                             kerpk = kerpk, 
                             fitmaxiter = fitmaxiter, 
                             gam = gam, 
                             scl.factor = scl.factor,
                             tau = tau,
                             maxwdth = maxwdth)
      save(xrdres, file = xrddatafile)
      print("... Ended if-clause 2")
   }

   # If the file does not exist, 
   # it doesn't really matter what the override flag says...
   if (!file.exists(xrddatafile)) {
      print("... Started if-clause 3")
      
      xrdres <- list()
      print("... xrdres list created")
      
      # Need to call xrdpk() and save its results to file as above
      xrdres[[run]] <- xrdpk(data.exp, 
                             kerpk = kerpk, 
                             fitmaxiter = fitmaxiter, 
                             gam = gam, 
                             scl.factor = scl.factor,
                             tau = tau,
                             maxwdth = maxwdth)
      save(xrdres, file = xrddatafile)
      print("... Ended if-clause 3")
   }

   return(xrdres)
}
