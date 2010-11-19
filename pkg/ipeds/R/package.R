.First.lib <- function(libname, pkgname) {
	ethnicityLevels <<- c('HispanicAnyRace', 'AmericanIndianOrAlaskaNative', 'Asian', 'BlackOrAfricanAmerican', 'NativeHawaiianOrPacificIslander', 'White', 'TwoOrMoreRaces', 'NonresidentAlien', 'RaceEthnicityUnknown')
	genderLevels <<- c('Male', 'Female')
	ipedsDataUrl <<- 'http://nces.ed.gov/ipeds/datacenter/data/'
	data(surveys)
}

.Last.lib <- function(libname, pkgname) {
}
