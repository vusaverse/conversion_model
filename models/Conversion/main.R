testdatum <- today()
nTest_year = vvconverter::academic_year(testdatum + months(1)) + 1

source("models/Conversion/00. Prepare aanmeldingen.R")
source("models/Conversion/01. Conversion prediction.R")
