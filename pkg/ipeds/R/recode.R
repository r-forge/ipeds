recodeImputation <- function(col) {
	factor(col, levels=c('A','B','C','D','G','H','J','K','L','N','P','R','Z'),
		   labels=c('Not applicable',
				'Institution left item blank',
				'Analyst corrected reported value',
				'Do not know',
				'Data generated from other data values',
				'Value not derived - data not usable',
				'Logical imputation',
				'Ratio adjustment',
				'Imputed using the Group Median procedure',
				'Imputed using Nearest Neighbor procedure',
				'Imputed using Carry Forward procedure',
				'Reported',
				'Implied zero'))
}

recodeGeographicRegion <- function(col) { #OBEREG
	factor(col, levels=c(0,1,2,3,4,5,6,7,8,9),
		   labels=c('US Service schools',
				'New England CT ME MA NH RI VT',
				'Mid East DE DC MD NJ NY PA',
				'Great Lakes IL IN MI OH WI',
				'Plains IA KS MN MO NE ND SD',
				'Southeast AL AR FL GA KY LA MS NC SC TN VA WV',
				'Southwest AZ NM OK TX',
				'Rocky Mountains CO ID MT UT WY',
				'Far West AK CA HI NV OR WA',
				'Outlying areas AS FM GU MH MP PR PW VI'))
}

recodeControl <- function(col) { #CONTROL
	factor(col, levels=c(1,2,3,-3),
		   labels=c('Public',
				'Private not-for-profit',
				'Private for-profit',
				'Not available'))
}

recodeTitleIVEligibility <- function(col) { #OPEFLAG
	factor(col, levels=c(1,2,3,8,5,6,7),
		   labels=c('Participates in Title IV federal financial aid programs',
				'Branch campus of a main campus that participates in Title IV',
				'Deferment only - limited participation',
				'New participants (became eligible during spring collection)',
				'Not currently participating in Title IV, has an OPE ID number',
				'Not currently participating in Title IV, does not have OPE ID number',
				'Stopped participating during the survey year'))
}

recodeSector <- function(col) {  #SECTOR
	factor(col, levels=c(0,1,2,3,4,5,6,7,8,9,99),
		   labels=c('Administrative Unit',
				'Public, 4-year or above',
				'Private not-for-profit, 4-year or above',
				'Private for-profit, 4-year or above',
				'Public, 2-year',
				'Private not-for-profit, 2-year',
				'Private for-profit, 2-year',
				'Public, less-than 2-year',
				'Private not-for-profit, less-than 2-year',
				'Private for-profit, less-than 2-year',
				'Sector unknown (not active)'))
}


recodeLevelOfInstitution <- function(col) { #ICLEVEL
	factor(col, levels=c(1,2,3,-3),
		   labels=c('Four or more years',
				'At least 2 but less than 4 years',
				'Less than 2 years (below associate)',
				'{Not available}'))
}

recodeHighestLevelOfOffering <- function(col) { #HLOFFER
	factor(col, levels=c(1,2,3,4,5,6,7,8,9,-3),
		   labels=c('Award of less than one academic year',
				'At least 1, but less than 2 academic yrs',
				"Associate's degree",
				'At least 2, but less than 4 academic yrs',
				"Bachelor's degree",
				'Postbaccalaureate certificate',
				"Master's degree",
				"Post-master's certificate",
				"Doctor's degree",
				'{Not available}'))
}

recodeUndergraduateOffering <- function(col) { #UGOFFER
	factor(col, levels=c(1,2,-3),
		   labels=c('Undergraduate degree or certificate offering',
				'No undergraduate offering',
				'{Not available}'))
}

recodeGraduateOffering <- function(col) { #GROFFER
	factor(col, levels=c(1,2,-3),
		   labels=c('Graduate degree or certificate offering',
				'No graduate offering',
				'{Not available}'))
}

recodeHighestDegreeOffered <- function(col) { #HDEGOFR
	factor(col, levels=c(0,11,12,13,14,20,30,40,-3),
		   labels=c('Non-degree granting',
				"Doctor's degree - research/scholarship and professional practice",
				"Doctor's degree - research/scholarship",
				"Doctor's degree - professional practice",
				"Doctor's degree - other",
				"Master's degree",
				"Bachelor's degree",
				"Associate's degree",
				"{Not available}"))
}

recodeOpenPublic <- function(col) { #OPENPUBL
	factor(col, levels=c(1,0),
		   labels=c('Institution is open to the public', 'Institution is not open to the public'))
}

recodeTitleIVIndicator <- function(col) { #PSET4FLG
	factor(col, levels=c(1,2,3,4,6,9),
		   labels=c("Title IV postsecondary institution",
				"Non-Title IV postsecondary institution",
				"Title IV NOT primarily postsecondary institution",
				"Non-Title IV NOT primarily postsecondary institution",
				"Non-Title IV postsecondary institution that is NOT open to the public",
				"Institution is not active in current universe"))
}

recodeInstitutionSize <- function(col) { #INSTSIZE
	factor(col, levels=c(1,2,3,4,5,-1,-2),
		   labels=c("Under 1,000",
				"1,000 - 4,999",
				"5,000 - 9,999",
				"10,000 - 19,999",
				"20,000 and above",
				"Not reported",
				"Not applicable"))
}