# ##############################################################################
# Programmer: D. Koster
# Date: 04/2022
# File: Z01_RMD_HTML_FORMATTING_FUNCTIONS.r
# Purpose: Functions for adding HTML wrappers to text for
#          formatting R-markdown.
#
# Note: These rely on the style.css developed for each project. 
#       to make changes to the style, please modify style.css
#
# ##############################################################################

formatTableHeader <- function(fmtTxt = "")
{
  return(paste('<div class="kableHeader">',
               fmtTxt,
               '</div>'))
}

formatValueList <- function(fmtTxt = c())
{
  fmtTxt = paste(fmtTxt, collapse=", ")
  return(paste('<br><div class="colNameBlock">',
         fmtTxt, '</div><br>'))

}

formatSummaryBlock <- function(fmtTxt = "")
{
  return(paste('<div class="summaryBlock">',
               fmtTxt,
               '</div>'))
}

errorMessage <- function(fmtText = "")
{
  return(paste('<p class="errorBlock">', fmtText,'</p>'))
}

warningMessage <- function(fmtText = "")
{
  return(paste('<p class="warningBlock">', fmtText,'</p>'))
}

greenMessage <- function(fmtText = "")
{
  return(paste('<p class="greenBlock">', fmtText,'</p>'))
}

# T-test output formatting.
tTestOutput <- function(t.test, title="t-test results", rounding=TRUE)
{
  if(rounding) {
    pVal = round(t.test$p.value, 3)
    tVal = round(t.test$statistic["t"], 2)
    df = round(t.test$parameter["df"],1)
    lowCI = round(t.test$conf.int[1], 2)
    highCI = round(t.test$conf.int[2], 2)
    mean1 = round(t.test$estimate[1], 2)
    mean2 = round(t.test$estimate[2], 2)
  } else {
    pVal = t.test$p.value
    tVal = t.test$statistic["t"]
    df = t.test$parameter["df"]    
    lowCI = t.test$conf.int[1]
    highCI = t.test$conf.int[1]    
    mean1 = t.test$estimate[1]
    mean2 = t.test$estimate[2]
  }
  
  htmlOut = str_interp('<div class="statsTestBlock">')
  htmlOut = str_interp('${htmlOut} <p class="statsTestTitle">${title}</p>')
  htmlOut = str_interp('${htmlOut} <p class="statsTestBody">')
  htmlOut = str_interp('${htmlOut} Method: ${t.test$method}<br>')
  htmlOut = str_interp('${htmlOut} Means: ${mean1}, ${mean2}<br>')  
  htmlOut = str_interp('${htmlOut} 95% Confidence: ${lowCI}, ${highCI}<br>')
  htmlOut = str_interp('${htmlOut} t(${df}) = ${tVal}, p=${pVal}<br>')
  htmlOut = str_interp('${htmlOut}</p></div>')
  
  return(htmlOut)
}

figCaption <- function(caption="Plot") {
  
  formattedCaption = str_interp('<p class="figcaption"> Figure n-m: ${caption} </p>')
  return(formattedCaption)
}

# Note the below two items aren't working, more effort needs to go into 
#   getting the set up.

beginShowHide <- function(showText = "show", hideText = "hide", uid=1)
{
  shoTxt = paste('<div>
                  <a id="hide', uid,'" href="#hide', uid,'" class="hide">', showText,'</a>
                  <a id="show', uid,'" href="#show', uid,'" class="show">', hideText,'</a>
                  <div class="details">',
                 sep='')
  return(shoTxt)
}

endShowHide <- function()
{
  hidTxt = "</div></div>"
  return(hidTxt)
}
