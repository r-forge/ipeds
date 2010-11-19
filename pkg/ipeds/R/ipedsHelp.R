ipedsHelp <- function(surveyId, year) {
	s = surveys[which(surveys$SurveyID==surveyId),]
	if(nrow(s) != 1) {
		stop(paste('IPEDS survey with id', surveyId, 'not found'))
	}
	dir = system.file(package="ipeds")
	dir.create(paste(dir, '/data/dict/', sep=''), showWarnings=FALSE)
	file = tolower(paste(s[1,'DataFilePre'], year, s[1,'DataFilePost'], '.html', sep=''))
	dest = paste(dir, '/data/dict/', file, sep='')
	if(!file.exists(dest)) {
		dest = paste(dir, '/data/dict/', s[1,'DataFilePre'], year, s[1,'DataFilePost'], '_Dict', '.zip', sep='')
		url = paste(ipedsDataUrl, s[1,'DataFilePre'], year, s[1,'DataFilePost'], '_Dict', '.zip', sep='')
		download.file(url, dest, mode="wb")
		unzip(dest, exdir=paste(dir, "/data/dict", sep=""))
		unlink(dest)
	}
	browseURL(paste(dir, '/data/dict/', file, sep=''))
}
