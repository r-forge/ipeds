mapIPEDSFields <- function(programCIPCode, educationalProgramType, educationalProgramName, educationalAwardLevel, ethnicityColumn, genderColumn) {
	fields = c('ProgramCIPCode', 'EducationalProgramType', 'EducationalProgramName', 'EducationalAwardLevel', 'Ethnicity', 'Gender')
	mapping = data.frame(MappedColumn=rep(NA, length(fields)), row.names=fields)
	mapping['ProgramCIPCode', 'MappedColumn'] = programCIPCode
	mapping['EducationalProgramType', 'MappedColumn'] = educationalProgramType
	mapping['EducationalProgramName', 'MappedColumn'] = educationalProgramName
	mapping['EducationalAwardLevel', 'MappedColumn'] = educationalAwardLevel
	mapping['Ethnicity', 'MappedColumn'] = ethnicityColumn
	mapping['Gender', 'MappedColumn'] = genderColumn
	mapping
}

