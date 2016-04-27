


library("Rcpp")
Rcpp::sourceCpp("/Users/travismcarthur/git/misc/authored-functions/unlock-environment.cpp")
# NOTE: Change the path above if on a different computer

replace.fn.in.package.Rcpp <- function(fn.to.replace, replacement.fn, pkg) {
  unlock_environment(as.environment(paste0("package:", pkg)))
  
  if (  exists(fn.to.replace, env=as.environment(paste0("package:", pkg)))) {
    unlockBinding(fn.to.replace, as.environment(paste0("package:", pkg)))
  }
  
  assign(fn.to.replace, get(replacement.fn, envir = parent.frame()), 
    as.environment(paste0("package:", pkg)))
  # Assign in package
  assignInNamespace(fn.to.replace, get(replacement.fn, envir = parent.frame()), 
      ns=asNamespace(pkg), envir=as.environment(paste0("package:", pkg)))
  # Assign in namespace, to make sure we really get it
  rm(list=replacement.fn, pos=parent.frame())
  print( getAnywhere(fn.to.replace) ) 
  # To make sure it worked properly
  invisible(NULL)
}





# Example usage below

do.example <- FALSE

if (do.example) {

try(detach("package:coefplot", unload=TRUE, force=TRUE, character.only =TRUE))
library("coefplot")


coefplot.default <- 
function (model, title = "Coefficient Plot", xlab = "Value", 
    ylab = "Coefficient", innerCI = 1, outerCI = 2, lwdInner = 1, 
    lwdOuter = 0, pointSize = 3, color = "blue", shape = 16, 
    cex = 0.8, textAngle = 0, numberAngle = 0, zeroColor = "grey", 
    zeroLWD = 1, zeroType = 2, facet = FALSE, scales = "free", 
    sort = c("natural", "magnitude", "alphabetical"), decreasing = FALSE, 
    numeric = FALSE, fillColor = "grey", alpha = 1/2, horizontal = FALSE, 
    factors = NULL, only = NULL, shorten = TRUE, intercept = TRUE, 
    interceptName = "(Intercept)", coefficients = NULL, predictors = NULL, 
    strict = FALSE, newNames = NULL, plot = TRUE, ...) 
{
    theDots <- list(...)
    sort <- match.arg(sort)
    modelCI <- buildModelCI(model, outerCI = outerCI, innerCI = innerCI, 
        intercept = intercept, coefficients = coefficients, predictors = predictors, 
        strict = strict, newNames = newNames, numeric = numeric, 
        sort = sort, decreasing = decreasing, factors = factors, 
        only = only, shorten = shorten, ...)
    if (!plot) {
        return(modelCI)
    }

    p <- coefplot.data.frame(model = modelCI, title = title, 
        xlab = xlab, ylab = ylab, lwdInner = lwdInner, lwdOuter = lwdOuter, 
        pointSize = pointSize, color = color, cex = cex, textAngle = textAngle, 
        numberAngle = numberAngle, zeroColor = zeroColor, zeroLWD = zeroLWD, 
        outerCI = outerCI, innerCI = innerCI, multi = FALSE, 
        zeroType = zeroType, numeric = numeric, fillColor = fillColor, 
        alpha = alpha, horizontal = horizontal, facet = facet, 
        scales = scales, ...)

    return(p)
}




coefplot.data.frame <-
function (model, title = "Coefficient Plot", xlab = "Value", 
    ylab = "Coefficient", lwdInner = 1, lwdOuter = 0, pointSize = 3, 
    color = "blue", cex = 0.8, textAngle = 0, numberAngle = 0, 
    shape = 16, linetype = 1, outerCI = 2, innerCI = 1, multi = FALSE, 
    zeroColor = "grey", zeroLWD = 1, zeroType = 2, numeric = FALSE, 
    fillColor = "grey", alpha = 1/2, horizontal = FALSE, facet = FALSE, 
    scales = "free", value = "Value", coefficient = "Coefficient", 
    errorHeight = 0, dodgeHeight = 1, ...) 
{


    buildPlotting.default(modelCI = model, title = title, xlab = xlab, 
        ylab = ylab, lwdInner = lwdInner, lwdOuter = lwdOuter, 
        pointSize = pointSize, color = color, cex = cex, textAngle = textAngle, 
        numberAngle = numberAngle, zeroColor = zeroColor, zeroLWD = zeroLWD, 
        outerCI = outerCI, innerCI = innerCI, multi = FALSE, 
        zeroType = zeroType, numeric = numeric, fillColor = fillColor, 
        alpha = alpha, horizontal = horizontal, facet = facet, 
        scales = scales, ...)
#        scales = scales, xlimit = xlimit)
    # Just adding ... to argument list
}


buildPlotting.default <- function (modelCI, title = "Coefficient Plot", xlab = "Value", 
    ylab = "Coefficient", lwdInner = 1, lwdOuter = 0, pointSize = 3, 
    color = "blue", cex = 0.8, textAngle = 0, numberAngle = 0, 
    shape = 16, linetype = 1, outerCI = 2, innerCI = 1, multi = FALSE, 
    zeroColor = "grey", zeroLWD = 1, zeroType = 2, numeric = FALSE, 
    fillColor = "grey", alpha = 1/2, horizontal = FALSE, facet = FALSE, 
    scales = "free", value = "Value", coefficient = "Coefficient", 
    errorHeight = 0, dodgeHeight = 1, xlimit = xlimit) 
{
    outerCIGeom <- geom_errorbarh(aes_string(xmin = "LowOuter", 
        xmax = "HighOuter", color = "Model", linetype = "Model"), 
        lwd = lwdOuter, height = errorHeight, position = position_dodgev(height = dodgeHeight))
    innerCIGeom <- geom_errorbarh(aes_string(xmin = "LowInner", 
        xmax = "HighInner", color = "Model", linetype = "Model"), 
        lwd = lwdInner, height = errorHeight, position = position_dodgev(height = dodgeHeight))
    pointGeom <- geom_point(aes_string(xmin = value, xmax = value, 
        color = "Model", shape = "Model"), size = pointSize, 
        position = position_dodgev(height = dodgeHeight))
    colorScaleSingle <- scale_color_manual(values = rep(color, 
        length(unique(modelCI$Model))), guide = FALSE)
    shapeScaleSingle <- scale_shape_manual(values = rep(shape, 
        length(unique(modelCI$Model))), guide = FALSE)
    linetypeScaleSingle <- scale_linetype_manual(values = rep(linetype, 
        length(unique(modelCI$Model))), guide = FALSE)
    xScale <- list(None = NULL, Single = scale_x_discrete())
    
    faceting <- list(None = NULL, Display = facet_wrap(~Checkers, 
        scales = scales))
    p <- ggplot(data = modelCI, aes_string(x = value, y = coefficient))
    p <- p + geom_vline(xintercept = 0, colour = zeroColor, linetype = zeroType, 
        lwd = zeroLWD)
    p <- p + outerCIGeom + innerCIGeom
    p <- p + pointGeom
    p <- p + theme(axis.text.y = element_text(angle = textAngle, 
        hjust = 1), axis.text.x = element_text(angle = numberAngle, 
        vjust = 0.5)) + labs(title = title, x = xlab, y = ylab)
    p <- p + if (!multi) {
        list(colorScaleSingle, shapeScaleSingle, linetypeScaleSingle)
    }
    p <- p + faceting[[facet + 1]]
    #if (!missing(xlimit)) 
    p <- p + coord_cartesian(xlim = xlimit) # ylim(xlimit)
    #cat(missing(xlimit))
    p <- p + if (horizontal) 
        coord_flip()
    return(p)
}





replace.fn.in.package.Rcpp(fn.to.replace="buildPlotting.default",  
  replacement.fn="buildPlotting.default", pkg="coefplot")

replace.fn.in.package.Rcpp(fn.to.replace="coefplot.data.frame",  
  replacement.fn="coefplot.data.frame", pkg="coefplot")

replace.fn.in.package.Rcpp(fn.to.replace="coefplot.default",  
  replacement.fn="coefplot.default", pkg="coefplot")
  


data(diamonds)
model1 <- lm(price ~ carat*color, data=diamonds)
coefplot(model1, xlimit = c(-10000, 10000))

}
  


