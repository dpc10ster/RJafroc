RJafrocEnv <- new.env()
assign("UNINITIALIZED", -Inf, envir = RJafrocEnv)

assign("minZeta", -4, envir = RJafrocEnv)
assign("maxZeta", 4, envir = RJafrocEnv)

assign("minLambdaP", 0.01, envir = RJafrocEnv)
assign("maxLambdaP", 10, envir = RJafrocEnv)

assign("minNuP", 0.01, envir = RJafrocEnv)
assign("maxNuP", 0.99, envir = RJafrocEnv)

assign("minMu", 0.01, envir = RJafrocEnv)
assign("maxMu", 6, envir = RJafrocEnv)
assign("maxRsmMu", 6, envir = RJafrocEnv)

assign("minAlpha", 0.01, envir = RJafrocEnv)
assign("maxAlpha", 0.99, envir = RJafrocEnv)

assign("minRho", -0.99, envir = RJafrocEnv)
assign("maxRho", +0.99, envir = RJafrocEnv)

# assign("minA", 0.01, envir = RJafrocEnv) # this causes error ...
assign("minA", -4.0, envir = RJafrocEnv) # to avoid error when ROC curve falls below chance diagonal
# i.e., a is negative

assign("maxA", 4, envir = RJafrocEnv)

assign("minB", 0.01, envir = RJafrocEnv)
assign("maxB", 4, envir = RJafrocEnv)

