#Install the package.
install.packages('ipeds', repos=c('http://R-Forge.R-project.org', 'http://lib.stat.cmu.edu/R/CRAN'), dep=TRUE, type='source')
install.packages(c('ggplot2', 'xtable', 'maps'), repos='http://lib.stat.cmu.edu/R/CRAN', dep=TRUE)

#Load the ipeds and ggplot2 (for plotting) packages
library(ipeds)
library(ggplot2)
library(xtable)
library(maps)

#This will list the currently available functions
ls('package:ipeds')

#The surveys that are available to download
data(surveys)
surveys[,c('SurveyID', 'Title')]

#List of all available CIP codes
data(cipcodes)
names(cipcodes)

#This dataframe provides a crosswalk between 2000 CIP codes and 2010 CIP codes
data(crosswalk)
names(crosswalk)

#The ipedsHelp function will return the data dictionary for the given surveys.
#Beginning in 2010, the data dictionaries are in Excel format. These functions
#will still download the data dictionaries but a program that can read Excel
#files is necessary.
ipedsHelp('HD', 2009)
ipedsHelp('HD', 2010)
ipedsHelp('IC', 2009)
ipedsHelp('EFD', 2009)

directory = getIPEDSSurvey('HD', 2009)
admissions = getIPEDSSurvey('IC', 2009)
retention = getIPEDSSurvey('EFD', 2009)

#This is the first of what will be a set of recodeXXX functions. This function
#recodes the values setting up the appropriate factors and variable types.
directory = recodeDirectory(directory)

##### SAT vs Retention Rate #####################################################
directory = directory[,c('unitid', 'instnm', 'sector', 'control')]

admissions = admissions[,c('unitid', 'admcon1', 'admcon2', 'admcon7', 'applcnm', 'applcnw', 'applcn', 'admssnm', 'admssnw', 'admssn', 'enrlftm', 'enrlftw', 'enrlptm', 'enrlptw', 'enrlt', 'satnum', 'satpct', 'actnum', 'actpct', 'satvr25', 'satvr75', 'satmt25', 'satmt75', 'satwr25', 'satwr75', 'actcm25', 'actcm75', 'acten25', 'acten75', 'actmt25', 'actmt75', 'actwr25', 'actwr75')]
admissions$admcon1 = factor(admissions$admcon1, levels=c(1,2,3,4,-1,-2), labels=c('Required', 'Recommended', 'Neither requiered nor recommended', 'Do not know', 'Not reported', 'Not applicable'))
admissions$admcon2 = factor(admissions$admcon2, levels=c(1,2,3,4,-1,-2), labels=c('Required', 'Recommended', 'Neither requiered nor recommended', 'Do not know', 'Not reported', 'Not applicable'))
admissions$admcon7 = factor(admissions$admcon7, levels=c(1,2,3,4,-1,-2), labels=c('Required', 'Recommended', 'Neither requiered nor recommended', 'Do not know', 'Not reported', 'Not applicable'))
names(admissions) = c('unitid', 'UseHSGPA', 'UseHSRank', 'UseAdmissionTestScores', 'ApplicantsMen', 'ApplicantsWomen', 'ApplicantsTotal', 'AdmissionsMen', 'AdmissionsWomen', 'AdmissionsTotal', 'EnrolledFullTimeMen', 'EnrolledFullTimeWomen', 'EnrolledPartTimeMen', 'EnrolledPartTimeWomen', 'EnrolledTotal', 'NumSATScores', 'PercentSATScores', 'NumACTScores', 'PercentACTScores', 'SATReading25', 'SATReading75', 'SATMath25', 'SATMath75', 'SATWriting25', 'SATWriting75', 'ACTComposite25', 'ACTComposite75', 'ACTEnglish25', 'ACTEnglish75', 'ACTMath25', 'ACTMath75', 'ACTWriting25', 'ACTWriting75')
 
retention = retention[,c('unitid', 'ret_pcf', 'ret_pcp')]
names(retention) = c('unitid', 'FullTimeRetentionRate', 'PartTimeRetentionRate')
 
#Merge the data frames. Note that schools that do not appear in all three data frames will not be included in the final analysis.
ret = merge(directory, admissions, by='unitid')
ret = merge(ret, retention, by='unitid')
 
ret2 = ret
ret2 = ret2[-which(ret2$FullTimeRetentionRate < 20),] #Remove schools with low retention rates. Are these errors in the data?
ret2$SATMath = (ret2$SATMath75 + ret2$SATMath25) / 2
ret2$SATWriting = (ret2$SATWriting75 + ret2$SATWriting25) / 2
ret2$SATTotal = ret2$SATMath + ret2$SATWriting
ret2$AcceptanceTotal = ret2$AdmissionsTotal / ret2$ApplicantsTotal
ret2$UseAdmissionTestScores = as.factor(as.character(ret2$UseAdmissionTestScores))
 
ggplot(ret2, aes(x=FullTimeRetentionRate)) + geom_histogram(binwidth=1, alpha=.6)
ggplot(ret2, aes(x=SATMath)) + geom_histogram(binwidth=10, alpha=.6)
ggplot(ret2, aes(x=SATWriting)) + geom_histogram(binwidth=10, alpha=.6)
 
retMath = ret2[,c('unitid', 'SATMath25', 'SATMath75', 'SATMath')]
retMath = melt(retMath, id='unitid')
ggplot(retMath, aes(x=value)) + geom_histogram(binwidth=10, alpha=.6) + facet_wrap(~ variable, ncol=1)
 
retWriting = ret2[,c('unitid', 'SATWriting25', 'SATWriting75', 'SATWriting')]
retWriting = melt(retWriting, id='unitid')
ggplot(retWriting, aes(x=value)) + geom_histogram(binwidth=10, alpha=.6) + facet_wrap(~ variable, ncol=1)

ggplot(ret2, aes(x=SATTotal, y=FullTimeRetentionRate, size=NumSATScores)) + geom_point(alpha=.6) + geom_smooth(aes(weight=NumSATScores))
ggplot(ret2, aes(x=SATTotal, y=PartTimeRetentionRate, size=NumSATScores)) + geom_point(alpha=.6) + geom_smooth(aes(weight=NumSATScores))
ggplot(ret2, aes(x=SATTotal, y=FullTimeRetentionRate, size=NumSATScores, color=AcceptanceTotal)) + geom_point() + geom_smooth(aes(weight=NumSATScores))
ggplot(ret2, aes(x=AcceptanceTotal, y=FullTimeRetentionRate, size=NumSATScores)) + geom_point() + geom_smooth(aes(weight=NumSATScores))

#Regression
fit = lm(FullTimeRetentionRate ~ SATWriting + SATMath + AcceptanceTotal + UseAdmissionTestScores, data=ret2, weights=NumSATScores)
summary(fit)


##### Peer analysis #############################################################
directory = getIPEDSSurvey('HD', 2009)
names(directory) = tolower(names(directory))
directory = recodeDirectory(directory)
directory[which(directory$instnm == 'Excelsior College'),]
unitid = directory[which(directory$instnm == 'Excelsior College'),'unitid']
peerIds = c(105668,127918,128780,144777,163204,183257,187046,202806,206279,223816,260901,413413,433387,444158,445027,449339)
peers = directory[which(directory$unitid %in% peerIds),]

#Print a table with basic information from the directory
p = peers[,c('unitid', 'instnm', 'webaddr', 'stabbr', 'control')]
names(p) = c('IPEDS ID', 'Institution', 'Web Address', 'State', 'Sector')
x = xtable(p, caption='Peer Institutions', label='tab:peers')
print(x, include.rownames=FALSE, caption.placement='bottom')

#Plot a map with the locations of the peer institutions
usa = data.frame(map('state', plot=FALSE)[c('x','y')])
ggplot(usa, aes(x=x,y=y)) + geom_path() + coord_map() + geom_point(data=peers, aes(x=longitud, y=latitude, colour=control), size=4) + geom_text(data=peers, aes(x=longitud, y=latitude, label=instnm, colour=control), size=2.5, hjust=-0.1) + labs(colour='Sector') + xlab(NULL) + ylab(NULL) + opts(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank())

enrollment = getIPEDSSurvey('EFFY', 2010)
names(enrollment) = tolower(names(enrollment))
enrollment = enrollment[,c('unitid', 'effylev', 'efytotlt',
	'efytotlm', 'efytotlw',
	'efynralm', 'efynralw', 'efynralt',
	'efyunknm', 'efyunknw', 'efyunknt',
	'dveyhsm', 'dveyhsw', 'dveyhst',
	'dveyaim', 'dveyaiw', 'dveyait',
	'dveybkm', 'dveybkw', 'dveybkt',
	'dveyapm', 'dveyapw', 'dveyapt',
	'dveywhm', 'dveywhw', 'dveywht',
	'efy2morm', 'efy2morw', 'efy2mort')]
enrollment.plot = enrollment[which(enrollment$unitid %in% peerIds),]
enrollment.plot = enrollment.plot[which(enrollment.plot$effylev == 1),]
enrollment.plot = merge(enrollment.plot, directory[,c('unitid', 'instnm')], by='unitid', all.x=TRUE, sort=FALSE)
names(enrollment) = c('unitid', 'Level', 'Total',
	'Total men', 'Total women',
	'Nonresident Alien Men', 'Nonresident Alien Women', 'Nonresident Alien',
	'Race/ethnicity unknown men', 'Race/ethnicity unknown women', 'Race/ethnicity unknown',
	'Hispanic men', 'Hispanic women', 'Hispanic',
	'American Indian or\nAlaska Native men', 'American Indian or\nAlaska Native women', 'American Indian or\nAlaska Native',
	'Black or African American men', 'Black or African American women', 'Black or\nAfrican American',
	'Asian/Native Hawaiian/\nPacific Islander men', 'Asian/Native Hawaiian/\nPacific Islander women', 'Asian/Native Hawaiian/\nPacific Islander',
	'White men', 'White women', 'White',
	'Two or more races men', 'Two or more races women', 'Two or more races')
enrollment.peers = enrollment[which(enrollment$unitid %in% peerIds),]
enrollment.peers = rbind(enrollment[which(enrollment$unitid == unitid),], enrollment.peers)
enrollment.peers = merge(enrollment.peers, directory[,c('unitid', 'instnm', 'control')], by='unitid', all.x=TRUE, sort=FALSE)
enrollment.peers = enrollment.peers[which(enrollment.peers$Level == 1),]
enrollment.peers = enrollment.peers[,c(ncol(enrollment.peers), 1:(ncol(enrollment.peers)-1))]

#Plot enrollment
ggplot(enrollment.peers, aes(y=Total, x=factor(instnm), label=Total, fill=control)) + geom_bar(stat='identity', alpha=.8) + coord_flip() + xlab('') + ylab('12-Month Unduplicated Headcount') + geom_text(hjust=.7, vjust=0.5, angle=0, size=3) + labs(fill='Sector')


#Create a CSV with all the completions by CIP code
data(cipcodes); data(surveys)
completions = getIPEDSSurvey('C_A', 2010)
ec = completions[which(completions$UNITID==unitid & completions$CIPCODE < 99),]
names(ec); nrow(ec)
ec = merge(ec, cipcodes[,c('CIPCode', 'CIPTitle')], by.x='CIPCODE', by.y='CIPCode', all.x=TRUE)
ec = ec[ ,c('CIPCODE', 'AWLEVEL', 'CTOTALT', 'CIPTitle')]
table(ec$AWLEVEL)
sum(ec$CTOTALT)
write.csv(ec, 'Completions by CIP Code FY2010.csv')

#Completions
graduates = getIPEDSSurvey('C_A', 2010)
graduates = graduates[,c('UNITID','CIPCODE','AWLEVEL',
	'CTOTALM', 'CTOTALW', 'CTOTALT',
	'CNRALM', 'CNRALW', 'CNRALT',
	'CUNKNM', 'CUNKNW', 'CUNKNT',
	'DVCAIM', 'DVCAIW', 'DVCAIT',
	'DVCAPM', 'DVCAPW', 'DVCAPT',
	'DVCBKM', 'DVCBKW', 'DVCBKT',
	'DVCHSM', 'DVCHSW', 'DVCHST',
	'DVCWHM', 'DVCWHW', 'DVCWHT')]
graduates$AWLEVEL = recodeAwardLevel(graduates$AWLEVEL)
names(graduates) = c('unitid', 'cipcode', 'awardlevel',
	'Men', 'Women', 'Total',
	'Nonresident alien men', 'Nonresident alien women', 'Nonresident alien',
	'Race/ethnicity unknown men', 'Race/ethnicity unknown women', 'Race/ethnicity unknown',
	'American Indian or Alaska Native men', 'American Indian or Alaska Native women', 'American Indian or Alaska Native',
	'Asian/Native Hawaiian/Other Pacific Islander men', 'Asian/Native Hawaiian/Other Pacific Islander women', 'Asian/Native Hawaiian/Other Pacific Islander',
	'Black or African American men', 'Black or African American women', 'Black or African American',
	'Hispanic or Latino men', 'Hispanic or Latino women', 'Hispanic or Latino',
	'White men', 'White women', 'White'	)
graduates.peers = graduates[which(graduates$unitid %in% peerIds),]
graduates.peers = rbind(graduates[which(graduates$unitid == unitid),], graduates.peers)
graduates.peers = merge(graduates.peers, directory[,c('unitid', 'instnm')], by='unitid', all.x=TRUE, sort=FALSE)
graduates.peers$cipcode = format(graduates.peers$cipcode, width=7, nsmall=4)
graduates.peers$cip2 = unlist(strsplit(as.character(graduates.peers[,'cipcode']), "\\."))[seq(1,2*nrow(graduates.peers), by=2)]
graduates.peers$instnm = breakColumn(graduates.peers$instnm)
graduates.peers$awardlevel = breakColumn(graduates.peers$awardlevel, mod=4)
aggr = aggregate(graduates.peers[,c('Total')], by=list(graduates.peers$instnm, graduates.peers$cip2), FUN="sum")
graduates.peers2 = graduates.peers[-which(graduates.peers$cip2 == '99'),]

p = ggplot(graduates.peers2, aes(y=Total, x=factor(instnm), fill=factor(awardlevel))) + geom_bar(stat='identity', alpha=.8) + coord_flip() + xlab('') + ylab('Degrees Awarded') + labs(fill='Award Level')



##### Generate Completions XML File for upload ##################################
grads = read.csv('/Users/jbryer/Dropbox/School/Submissions & Publications/2011-AIR/Presentation/grads.fy2010.csv')
grads = read.csv('C:/Dropbox/My Dropbox/School/Submissions & Publications/2011-AIR/Presentation/grads.fy2010.csv')
names(grads); nrow(grads)
mapping = mapIPEDSFields(programCIPCode='X2010.CIP.code', 
	educationalProgramType='ProgramType', 
	educationalProgramName='EC.Program.Name', 
    educationalAwardLevel='AwardLevel', 
    ethnicityColumn='ETHNICITY_SUMMARY', 
    genderColumn='GENDER')
xml = completions(grads, mapping, institutionId=unitid, institutionName='Excelsior College')
cat(saveXML(xml), file='completions.xml')

