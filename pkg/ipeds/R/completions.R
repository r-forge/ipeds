completions <- function(data, mappings=NULL, institutionId=NA, institutionName=NA, documentId='a', createdDate=format(Sys.time(), '%Y-%m-%dT%H:%M:%S'), noteMessage=NA, transmissionType='Original', documentType='Application') {
	#Possible values for transmissionType: Duplicate, MutuallyDefined, Original, Reissue, Replace, Resubmission
	mappings$MappedColumn = as.character(mappings$MappedColumn)

	if(is.na(institutionId)) { stop('institutionalId is required') }
	if(is.null(data)) { stop('Data is required') }
	if(nrow(data[which(is.na(mappings['ProgramCIPCode','MappedColumn'])),]) > 0) { stop('All record must have a valid CIP code') }

	doc = newXMLDoc(namespace="http://www.w3.org/2001/XMLSchema-instance")
	root = newXMLNode("CN:EducationalAwards", doc=doc, namespace=c('schemaLocation'="urn:org:pesc:message:CompletionIPEDS:v1.0.0 Completions.xsd", 'xsi'="http://www.w3.org/2001/XMLSchema-instance", 'CN'="urn:org:pesc:message:CompletionIPEDS:v1.0.0"))

	td = newXMLNode('TransmissionData', namespace='')
	newXMLNode('DocumentID', text=documentId, parent=td)
	newXMLNode('CreatedDateTime', text=createdDate, parent=td)
	newXMLNode('TransmissionType', text=transmissionType, parent=td)
	newXMLNode('DocumentType', text=documentType, parent=td)
	addChildren(root, td)

	awards = newXMLNode('EducationalAwardsByInstitution', namespace='')
	inst = newXMLNode('Institution', parent=awards)
	newXMLNode('IPEDSUnitID', text=institutionId, parent=inst)
	if(!is.na(institutionName)) { newXMLNode('OrganizationName', text=institutionName, parent=inst) }
	if(!is.na(noteMessage)) { newXMLNode('NoteMessage', text=noteMessage, parent=inst) }

	for(i in unique(data[,mappings['ProgramCIPCode','MappedColumn']])) {
		tmp1 = data[which(data[,mappings['ProgramCIPCode','MappedColumn']] == i),]
		for(l in unique(tmp1[,mappings['EducationalAwardLevel','MappedColumn']])) {
			tmp = tmp1[which(tmp1[,mappings['ProgramCIPCode','MappedColumn']] == i & tmp1[,mappings['EducationalAwardLevel','MappedColumn']] == l),]
			award = newXMLNode('EducationalAwardsByProgram', parent=awards)
			program = newXMLNode('EducationalProgram', parent=award)
			newXMLNode('ProgramCIPCode', text=i, parent=program)
			newXMLNode('EducationalProgramType', text=tmp[1,mappings['EducationalProgramType','MappedColumn']], parent=program)
			programName = tmp[1,mappings['EducationalProgramName','MappedColumn']]
			if(!is.na(programName) & length(programName) > 1) {
				newXMLNode('EducationalProgramName', text=substr(programName,1,60), parent=program)
			}
			newXMLNode('EducationalAwardLevel', text=l, parent=award)
			distribution = newXMLNode('RaceGenderDistribution', parent=award)
			tab = as.data.frame(table(tmp[,mappings['Ethnicity','MappedColumn']],tmp[,mappings['Gender','MappedColumn']],exclude=NULL))
			for(e in ethnicityLevels) {
				eNode = newXMLNode(e, parent=distribution)
				tmp2 = tab[which(tab$Var1 == e),]
				newXMLNode('CountMale', text=tmp2[which(tmp2$Var2 == 'MALE'),]$Freq, parent=eNode)
				newXMLNode('CountFemale', text=tmp2[which(tmp2$Var2 == 'FEMALE'),]$Freq, parent=eNode)
			}
		}
	}
	
	addChildren(root, awards)

	doc
}

