getIPEDSSurvey <- function(surveyId, year) {
	s = surveys[which(surveys$SurveyID==surveyId),]
	if(nrow(s) != 1) {
		stop(paste('IPEDS survey with id', surveyId, 'not found'))
	}
	dir = system.file(package="ipeds")
	file = paste(s[1,'DataFilePre'], year, s[1,'DataFilePost'], sep='')
	dest = paste(dir, "/data/downloaded/", file, '.csv', sep="")
	if(!file.exists(dest)) {
		r = downloadIPEDSSurvey(surveyId, year)
	} else {
		r = read.csv(dest)
	}
	r
}

downloadIPEDSSurvey <- function(surveyId, year) {
	s = surveys[which(surveys$SurveyID==surveyId),]
	if(s['YearFormat'] == 4) {
		year = as.character(year)
	} else if(s['YearFormat'] == 2) {
		year = substr(year, 3,4)
	} else if(s['YearFormat'] == 22) {
		year = paste(substr((year-1), 3,4), substr(year, 3,4), sep='')
	} else if(s['YearFormat'] == 44) {
		year = paste((year-1), year, sep='')
	}
	dir = system.file(package="ipeds")
	file = paste(s[1,'DataFilePre'], year, s[1,'DataFilePost'], sep='')
	url = paste(ipedsDataUrl, file, '.zip', sep='')
	dir.create(paste(dir, '/data/downloaded/', sep=''), showWarnings=FALSE)
	dest = paste(dir, "/data/downloaded/", file, '.zip', sep="")
	download.file(url, dest, mode="wb")
	unzip(dest, exdir=paste(dir, "/data/downloaded", sep=""))
	unlink(dest)
	r = read.csv(paste(dir, "/data/downloaded/", file, ".csv", sep=""))
	r
}

downloadAllSurveys <- function(year) {
	for(i in 1:nrow(surveys)) {
		downloadIPEDSSurvey(surveys[i,'SurveyID'], year)
	}
}
