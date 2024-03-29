ipedsHelp <- function(surveyId, year) {
	s = surveys[which(surveys$SurveyID==surveyId),]
	if(nrow(s) != 1) {
		stop(paste('IPEDS survey with id', surveyId, 'not found'))
	}
	dir = system.file(package="ipeds")
	dir.create(paste(dir, '/data/dict/', sep=''), showWarnings=FALSE)
	fileBase = tolower(paste(s[1,'DataFilePre'], formatYear(surveyId, year), s[1,'DataFilePost'], sep=''))
	files = list.files(paste(dir, '/data/dict/', sep=''), pattern=paste(fileBase, '.*', sep=''))
	if(length(files) > 0) {
		file = files[1]
	} else {
		dest = paste(dir, '/data/dict/', s[1,'DataFilePre'], formatYear(surveyId, year), s[1,'DataFilePost'], '_Dict', '.zip', sep='')
		url = paste(ipedsDataUrl, s[1,'DataFilePre'], formatYear(surveyId, year), s[1,'DataFilePost'], '_Dict', '.zip', sep='')
		download.file(url, dest, mode="wb")
		unzip(dest, exdir=paste(dir, "/data/dict", sep=""))
		unlink(dest)
		files = list.files(paste(dir, '/data/dict/', sep=''), pattern=paste(fileBase, '.*', sep=''))
		file = files[1]
	}
	browseURL(paste(dir, '/data/dict/', file, sep=''))
}

downloadHelp <- function(year) {
	dir = system.file(package="ipeds")
	dir.create(paste(dir, '/data/dict/', sep=''), showWarnings=FALSE)
	for(i in 1:nrow(surveys)) {
		s = surveys[i,]
		file = tolower(paste(s[1,'DataFilePre'], year, s[1,'DataFilePost'], '.html', sep=''))
		dest = paste(dir, '/data/dict/', file, sep='')
		if(!file.exists(dest)) {
			dest = paste(dir, '/data/dict/', s[1,'DataFilePre'], year, s[1,'DataFilePost'], '_Dict', '.zip', sep='')
			url = paste(ipedsDataUrl, s[1,'DataFilePre'], year, s[1,'DataFilePost'], '_Dict', '.zip', sep='')
			download.file(url, dest, mode="wb")
			unzip(dest, exdir=paste(dir, "/data/dict", sep=""))
			unlink(dest)
		}
	}
}
