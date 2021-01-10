* NOTE: format names are the same as variable names, except for
        variable names that end in a number.  For these a 1 is replaced
        by an A, a 2 is replaced by a B, and so on.
  Edit the infile statement to reference the data file on your computer.
*;
data atuscps_2014;
infile "c:\atuscps_2014.dat" firstobs=2 dsd missover lrecl=16384 dlm=",";
length
TUCASEID $14
TULINENO 8
GEPSEUCL 8
GEPSEUST 8
GEREG 8
GEDIV 8
GESTFIPS 8
GTMETSTA 8
HEFAMINC 8
HEHOUSUT 8
HEPHONEO 8
HETELAVL 8
HETELHHD 8
HETENURE 8
HRHHID $15
HRHHID2 $5
HRHTYPE 8
HRINTSTA 8
HRLONGLK 8
HRMIS 8
HRMONTH 8
HRNUMHOU 8
HRYEAR4 8
HUBUS 8
HUBUSL1 8
HUBUSL2 8
HUBUSL3 8
HUBUSL4 8
HUFINAL 8
HUINTTYP 8
HUPRSCNT 8
HURESPLI 8
HUTYPB 8
HUTYPC 8
HUTYPEA 8
HXFAMINC 8
HXHOUSUT 8
HXPHONEO 8
HXTELAVL 8
HXTELHHD 8
HXTENURE 8
OCCURNUM 8
PEABSPDO 8
PEABSRSN 8
PEAFEVER 8
PEAFNOW 8
PEAFWHN1 8
PEAFWHN2 8
PEAFWHN3 8
PEAFWHN4 8
PECOHAB 8
PECYC 8
PEDADTYP 8
PEDIPGED 8
PEDISDRS 8
PEDISEAR 8
PEDISEYE 8
PEDISOUT 8
PEDISPHY 8
PEDISREM 8
PEDW4WK 8
PEDWAVL 8
PEDWAVR 8
PEDWLKO 8
PEDWLKWK 8
PEDWRSN 8
PEDWWK 8
PEDWWNTO 8
PEEDUCA 8
PEERN 8
PEERNCOV 8
PEERNH1O 8
PEERNH2 8
PEERNHRO 8
PEERNHRY 8
PEERNLAB 8
PEERNPER 8
PEERNRT 8
PEERNUOT 8
PEERNWKP 8
PEFNTVTY 8
PEGR6COR 8
PEGRPROF 8
PEHGCOMP 8
PEHRACT1 8
PEHRACT2 8
PEHRACTT 8
PEHRAVL 8
PEHRFTPT 8
PEHRRSN1 8
PEHRRSN2 8
PEHRRSN3 8
PEHRUSL1 8
PEHRUSL2 8
PEHRUSLT 8
PEHRWANT 8
PEHSPNON 8
PEIO1COW 8
PEIO1ICD 8
PEIO1OCD 8
PEIO2COW 8
PEIO2ICD 8
PEIO2OCD 8
PEJHRSN 8
PEJHWANT 8
PEJHWKO 8
PELAYAVL 8
PELAYDUR 8
PELAYFTO 8
PELAYLK 8
PELKAVL 8
PELKDUR 8
PELKFTO 8
PELKLL1O 8
PELKLL2O 8
PELKLWO 8
PELKM1 8
PELNDAD 8
PELNMOM 8
PEMARITL 8
PEMJNUM 8
PEMJOT 8
PEMLR 8
PEMNTVTY 8
PEMOMTYP 8
PEMS123 8
PENATVTY 8
PENLFACT 8
PENLFJH 8
PENLFRET 8
PEPARENT 8
PERET1 8
PERRP 8
PESCHENR 8
PESCHFT 8
PESCHLVL 8
PESEX 8
PESPOUSE 8
PRABSREA 8
PRAGNA 8
PRCITFLG 8
PRCITSHP 8
PRCIVLF 8
PRCOW1 8
PRCOW2 8
PRCOWPG 8
PRDISC 8
PRDISFLG 8
PRDTCOW1 8
PRDTCOW2 8
PRDTHSP 8
PRDTIND1 8
PRDTIND2 8
PRDTOCC1 8
PRDTOCC2 8
PREMP 8
PREMPHRS 8
PREMPNOT 8
PRERELG 8
PRERNHLY 8
PRERNWA 8
PREXPLF 8
PRFAMNUM 8
PRFAMREL 8
PRFAMTYP 8
PRFTLF 8
PRHERNAL 8
PRHRUSL 8
PRIMIND1 8
PRIMIND2 8
PRINUYER 8
PRIOELG 8
PRJOBSEA 8
PRMARSTA 8
PRMJIND1 8
PRMJIND2 8
PRMJOCC1 8
PRMJOCC2 8
PRMJOCGR 8
PRNAGPWS 8
PRNAGWS 8
PRNLFSCH 8
PRNMCHLD 8
PRPERTYP 8
PRPTHRS 8
PRPTREA 8
PRSJMJ 8
PRTAGE 8
PRTFAGE 8
PRUNEDUR 8
PRUNTYPE 8
PRWERNAL 8
PRWKSCH 8
PRWKSTAT 8
PRWNTJOB 8
PTDTRACE 8
PTHR 8
PTOT 8
PTWK 8
PUABSOT 8
PUBUS1 8
PUBUS2OT 8
PUBUSCK1 8
PUBUSCK2 8
PUBUSCK3 8
PUBUSCK4 8
PUCHINHH 8
PUDIS 8
PUDIS1 8
PUDIS2 8
PUDWCK1 8
PUDWCK2 8
PUDWCK3 8
PUDWCK4 8
PUDWCK5 8
PUERN2 8
PUERNH1C 8
PUHRCK1 8
PUHRCK12 8
PUHRCK2 8
PUHRCK3 8
PUHRCK4 8
PUHRCK5 8
PUHRCK6 8
PUHRCK7 8
PUHROFF1 8
PUHROFF2 8
PUHROT1 8
PUHROT2 8
PUIO1MFG 8
PUIO2MFG 8
PUIOCK1 8
PUIOCK2 8
PUIOCK3 8
PUIODP1 8
PUIODP2 8
PUIODP3 8
PUJHCK1 8
PUJHCK2 8
PUJHCK3 8
PUJHCK4 8
PUJHCK5 8
PUJHDP1O 8
PULAY 8
PULAY6M 8
PULAYAVR 8
PULAYCK1 8
PULAYCK2 8
PULAYCK3 8
PULAYDT 8
PULINENO 8
PULK 8
PULKAVR 8
PULKDK1 8
PULKDK2 8
PULKDK3 8
PULKDK4 8
PULKDK5 8
PULKDK6 8
PULKM2 8
PULKM3 8
PULKM4 8
PULKM5 8
PULKM6 8
PULKPS1 8
PULKPS2 8
PULKPS3 8
PULKPS4 8
PULKPS5 8
PULKPS6 8
PUNLFCK1 8
PUNLFCK2 8
PURETOT 8
PUSLFPRX 8
PUWK 8
PXABSPDO 8
PXABSRSN 8
PXAFEVER 8
PXAFNOW 8
PXAFWHN1 8
PXAGE 8
PXCOHAB 8
PXCYC 8
PXDADTYP 8
PXDIPGED 8
PXDISDRS 8
PXDISEAR 8
PXDISEYE 8
PXDISOUT 8
PXDISPHY 8
PXDISREM 8
PXDW4WK 8
PXDWAVL 8
PXDWAVR 8
PXDWLKO 8
PXDWLKWK 8
PXDWRSN 8
PXDWWK 8
PXDWWNTO 8
PXEDUCA 8
PXERN 8
PXERNCOV 8
PXERNH1O 8
PXERNH2 8
PXERNHRO 8
PXERNHRY 8
PXERNLAB 8
PXERNPER 8
PXERNRT 8
PXERNUOT 8
PXERNWKP 8
PXFNTVTY 8
PXGR6COR 8
PXGRPROF 8
PXHGCOMP 8
PXHRACT1 8
PXHRACT2 8
PXHRACTT 8
PXHRAVL 8
PXHRFTPT 8
PXHRRSN1 8
PXHRRSN2 8
PXHRRSN3 8
PXHRUSL1 8
PXHRUSL2 8
PXHRUSLT 8
PXHRWANT 8
PXHSPNON 8
PXINUSYR 8
PXIO1COW 8
PXIO1ICD 8
PXIO1OCD 8
PXIO2COW 8
PXIO2ICD 8
PXIO2OCD 8
PXJHRSN 8
PXJHWANT 8
PXJHWKO 8
PXLAYAVL 8
PXLAYDUR 8
PXLAYFTO 8
PXLAYLK 8
PXLKAVL 8
PXLKDUR 8
PXLKFTO 8
PXLKLL1O 8
PXLKLL2O 8
PXLKLWO 8
PXLKM1 8
PXLNDAD 8
PXLNMOM 8
PXMARITL 8
PXMJNUM 8
PXMJOT 8
PXMLR 8
PXMNTVTY 8
PXMOMTYP 8
PXMS123 8
PXNATVTY 8
PXNLFACT 8
PXNLFJH 8
PXNLFRET 8
PXPARENT 8
PXRACE1 8
PXRET1 8
PXRRP 8
PXSCHENR 8
PXSCHFT 8
PXSCHLVL 8
PXSEX 8
PXSPOUSE 8
QSTNUM 8
TRATUSR 8
PRDASIAN 8
PEPDEMP1 8
PTNMEMP1 8
PEPDEMP2 8
PTNMEMP2 8
PXPDEMP1 8
PXNMEMP1 8
PXPDEMP2 8
PXNMEMP2 8
;
input
TUCASEID
TULINENO
GEPSEUCL
GEPSEUST
GEREG
GEDIV
GESTFIPS
GTMETSTA
HEFAMINC
HEHOUSUT
HEPHONEO
HETELAVL
HETELHHD
HETENURE
HRHHID
HRHHID2
HRHTYPE
HRINTSTA
HRLONGLK
HRMIS
HRMONTH
HRNUMHOU
HRYEAR4
HUBUS
HUBUSL1
HUBUSL2
HUBUSL3
HUBUSL4
HUFINAL
HUINTTYP
HUPRSCNT
HURESPLI
HUTYPB
HUTYPC
HUTYPEA
HXFAMINC
HXHOUSUT
HXPHONEO
HXTELAVL
HXTELHHD
HXTENURE
OCCURNUM
PEABSPDO
PEABSRSN
PEAFEVER
PEAFNOW
PEAFWHN1
PEAFWHN2
PEAFWHN3
PEAFWHN4
PECOHAB
PECYC
PEDADTYP
PEDIPGED
PEDISDRS
PEDISEAR
PEDISEYE
PEDISOUT
PEDISPHY
PEDISREM
PEDW4WK
PEDWAVL
PEDWAVR
PEDWLKO
PEDWLKWK
PEDWRSN
PEDWWK
PEDWWNTO
PEEDUCA
PEERN
PEERNCOV
PEERNH1O
PEERNH2
PEERNHRO
PEERNHRY
PEERNLAB
PEERNPER
PEERNRT
PEERNUOT
PEERNWKP
PEFNTVTY
PEGR6COR
PEGRPROF
PEHGCOMP
PEHRACT1
PEHRACT2
PEHRACTT
PEHRAVL
PEHRFTPT
PEHRRSN1
PEHRRSN2
PEHRRSN3
PEHRUSL1
PEHRUSL2
PEHRUSLT
PEHRWANT
PEHSPNON
PEIO1COW
PEIO1ICD
PEIO1OCD
PEIO2COW
PEIO2ICD
PEIO2OCD
PEJHRSN
PEJHWANT
PEJHWKO
PELAYAVL
PELAYDUR
PELAYFTO
PELAYLK
PELKAVL
PELKDUR
PELKFTO
PELKLL1O
PELKLL2O
PELKLWO
PELKM1
PELNDAD
PELNMOM
PEMARITL
PEMJNUM
PEMJOT
PEMLR
PEMNTVTY
PEMOMTYP
PEMS123
PENATVTY
PENLFACT
PENLFJH
PENLFRET
PEPARENT
PERET1
PERRP
PESCHENR
PESCHFT
PESCHLVL
PESEX
PESPOUSE
PRABSREA
PRAGNA
PRCITFLG
PRCITSHP
PRCIVLF
PRCOW1
PRCOW2
PRCOWPG
PRDISC
PRDISFLG
PRDTCOW1
PRDTCOW2
PRDTHSP
PRDTIND1
PRDTIND2
PRDTOCC1
PRDTOCC2
PREMP
PREMPHRS
PREMPNOT
PRERELG
PRERNHLY
PRERNWA
PREXPLF
PRFAMNUM
PRFAMREL
PRFAMTYP
PRFTLF
PRHERNAL
PRHRUSL
PRIMIND1
PRIMIND2
PRINUYER
PRIOELG
PRJOBSEA
PRMARSTA
PRMJIND1
PRMJIND2
PRMJOCC1
PRMJOCC2
PRMJOCGR
PRNAGPWS
PRNAGWS
PRNLFSCH
PRNMCHLD
PRPERTYP
PRPTHRS
PRPTREA
PRSJMJ
PRTAGE
PRTFAGE
PRUNEDUR
PRUNTYPE
PRWERNAL
PRWKSCH
PRWKSTAT
PRWNTJOB
PTDTRACE
PTHR
PTOT
PTWK
PUABSOT
PUBUS1
PUBUS2OT
PUBUSCK1
PUBUSCK2
PUBUSCK3
PUBUSCK4
PUCHINHH
PUDIS
PUDIS1
PUDIS2
PUDWCK1
PUDWCK2
PUDWCK3
PUDWCK4
PUDWCK5
PUERN2
PUERNH1C
PUHRCK1
PUHRCK12
PUHRCK2
PUHRCK3
PUHRCK4
PUHRCK5
PUHRCK6
PUHRCK7
PUHROFF1
PUHROFF2
PUHROT1
PUHROT2
PUIO1MFG
PUIO2MFG
PUIOCK1
PUIOCK2
PUIOCK3
PUIODP1
PUIODP2
PUIODP3
PUJHCK1
PUJHCK2
PUJHCK3
PUJHCK4
PUJHCK5
PUJHDP1O
PULAY
PULAY6M
PULAYAVR
PULAYCK1
PULAYCK2
PULAYCK3
PULAYDT
PULINENO
PULK
PULKAVR
PULKDK1
PULKDK2
PULKDK3
PULKDK4
PULKDK5
PULKDK6
PULKM2
PULKM3
PULKM4
PULKM5
PULKM6
PULKPS1
PULKPS2
PULKPS3
PULKPS4
PULKPS5
PULKPS6
PUNLFCK1
PUNLFCK2
PURETOT
PUSLFPRX
PUWK
PXABSPDO
PXABSRSN
PXAFEVER
PXAFNOW
PXAFWHN1
PXAGE
PXCOHAB
PXCYC
PXDADTYP
PXDIPGED
PXDISDRS
PXDISEAR
PXDISEYE
PXDISOUT
PXDISPHY
PXDISREM
PXDW4WK
PXDWAVL
PXDWAVR
PXDWLKO
PXDWLKWK
PXDWRSN
PXDWWK
PXDWWNTO
PXEDUCA
PXERN
PXERNCOV
PXERNH1O
PXERNH2
PXERNHRO
PXERNHRY
PXERNLAB
PXERNPER
PXERNRT
PXERNUOT
PXERNWKP
PXFNTVTY
PXGR6COR
PXGRPROF
PXHGCOMP
PXHRACT1
PXHRACT2
PXHRACTT
PXHRAVL
PXHRFTPT
PXHRRSN1
PXHRRSN2
PXHRRSN3
PXHRUSL1
PXHRUSL2
PXHRUSLT
PXHRWANT
PXHSPNON
PXINUSYR
PXIO1COW
PXIO1ICD
PXIO1OCD
PXIO2COW
PXIO2ICD
PXIO2OCD
PXJHRSN
PXJHWANT
PXJHWKO
PXLAYAVL
PXLAYDUR
PXLAYFTO
PXLAYLK
PXLKAVL
PXLKDUR
PXLKFTO
PXLKLL1O
PXLKLL2O
PXLKLWO
PXLKM1
PXLNDAD
PXLNMOM
PXMARITL
PXMJNUM
PXMJOT
PXMLR
PXMNTVTY
PXMOMTYP
PXMS123
PXNATVTY
PXNLFACT
PXNLFJH
PXNLFRET
PXPARENT
PXRACE1
PXRET1
PXRRP
PXSCHENR
PXSCHFT
PXSCHLVL
PXSEX
PXSPOUSE
QSTNUM
TRATUSR
PRDASIAN
PEPDEMP1
PTNMEMP1
PEPDEMP2
PTNMEMP2
PXPDEMP1
PXNMEMP1
PXPDEMP2
PXNMEMP2
;
label GEPSEUCL = "Scrambled pseudo primary sampling unit (PSU) cluster";
label GEPSEUST = "Scrambled pseudo primary sampling unit (PSU) collapsed stratum";
label GEREG = "Region";
label GEDIV = "Division";
label GESTFIPS = "Federal Processing Information Standards (FIPS) state code";
label GTMETSTA = "Metropolitan status (2000 definitions)";
label HEFAMINC = "Edited: Family Income";
label HEHOUSUT = "Edited: type of housing unit";
label HEPHONEO = "Edited: is a telephone interview acceptable?";
label HETELAVL = "Edited: is there a telephone elsewhere on which people in this household can be contacted?";
label HETELHHD = "Edited: is there a telephone in this house/apartment?";
label HETENURE = "Edited: are your living quarters owned, rented for cash, or occupied without payment of cash rent?";
label HRHHID = "Household ID (15-digit identifier)";
label HRHHID2 = "Household ID part 2 (5-digit identifier)";
label HRHTYPE = "Household type";
label HRINTSTA = "Interview status";
label HRLONGLK = "Longitudinal link indicator";
label HRMIS = "Month in sample";
label HRMONTH = "Month of interview";
label HRNUMHOU = "Total number of persons in the household (household members)";
label HRYEAR4 = "Year of interview";
label HUBUS = "Does anyone in this household have a business or a farm?";
label HUBUSL1 = "PULINENO of farm or business owner (first owner)";
label HUBUSL2 = "PULINENO of farm or business owner (second owner)";
label HUBUSL3 = "PULINENO of farm or business owner (third owner)";
label HUBUSL4 = "PULINENO of farm or business owner (fourth owner)";
label HUFINAL = "Final outcome code";
label HUINTTYP = "Type of interview";
label HUPRSCNT = "Number of actual and attempted personal contacts";
label HURESPLI = "PULINENO of the current respondent";
label HUTYPB = "Type B non-interview categories";
label HUTYPC = "Type C non-interview categories";
label HUTYPEA = "Type A non-interview categories";
label HXFAMINC = "HEFAMINC: allocation flag";
label HXHOUSUT = "HEHOUSUT: allocation flag";
label HXPHONEO = "HEPHONEO: allocation flag";
label HXTELAVL = "HETELAVL: allocation flag";
label HXTELHHD = "HETELHHD: allocation flag";
label HXTENURE = "HETENURE: allocation flag";
label OCCURNUM = "Unique person identifier";
label PEABSPDO = "Edited: are you being paid by your employer for any of the time off last week?";
label PEABSRSN = "Edited: what was the main reason you were absent from work last week?";
label PEAFEVER = "Edited: did you ever serve on active duty in the U.S. Armed Forces?";
label PEAFNOW = "Edited: are you now in the Armed Forces?";
label PEAFWHN1 = "Edited: when did you serve in the Armed Forces? (first period)";
label PEAFWHN2 = "Edited: when did you serve in the Armed Forces? (second period)";
label PEAFWHN3 = "Edited: when did you serve in the Armed Forces? (third period)";
label PEAFWHN4 = "Edited: when did you serve in the Armed Forces? (fourth period)";
label PECOHAB = "Edited: PULINENO of cohabiting partner";
label PECYC = "Edited: how many years of college credit have you completed?";
label PEDADTYP = "Edited: Is household child a biological, step or adopted child?";
label PEDIPGED = "Edited: how did you get your high school diploma?";
label PEDISDRS = "Edited: Does this person have difficulty dressing or bathing?";
label PEDISEAR = "Edited: Is this person deaf or does this person have serious difficulty hearing?";
label PEDISEYE = "Edited: Is this person blind or does this person have serious difficulty seeing even when wearing glasses?";
label PEDISOUT = "Edited: Because of a physical, mental, or emotional condition does this person have difficulty doing errands alone such as visiting a doctor's office or shopping?";
label PEDISPHY = "Edited: Does this person have serious difficulty walking or climbing stairs?";
label PEDISREM = "Edited: Because of a physical, mental, or emotional condition, does this person have serious difficulty concentrating, remembering, or making decisions?";
label PEDW4WK = "Edited: did you do any of this work during the last 4 weeks?";
label PEDWAVL = "Edited: last week, could you have started a job if one had been offered?";
label PEDWAVR = "Edited: why could you not have started a job if one had been offered last week?";
label PEDWLKO = "Edited: did you look for work any time in the last 12 months?";
label PEDWLKWK = "Edited: and since you left that job or business have you looked for work?";
label PEDWRSN = "Edited: what is the main reason you were not looking for work during the last 4 weeks?";
label PEDWWK = "Edited: did you actually work at a job or business during the last 12 months?";
label PEDWWNTO = "Edited: do you currently want a job, either full or part time?";
label PEEDUCA = "Edited: what is the highest level of school you have completed or the highest degree you have received?";
label PEERN = "Edited: total weekly overtime earnings (2 implied decimals)";
label PEERNCOV = "Edited: on this job, are you covered by a union or employee association contract?";
label PEERNH1O = "Edited: excluding overtime pay, tips, and commissions, what is your hourly rate of pay on your main job? (2 implied decimals)";
label PEERNH2 = "Edited: excluding overtime pay, tips, and commissions, what is your hourly rate of pay on your main job? (2 implied decimals)";
label PEERNHRO = "Edited: how many hours do you usually work per week at this rate?";
label PEERNHRY = "Edited: hourly/non-hourly status";
label PEERNLAB = "Edited: on this job, are you a member of a labor union or of an employee association similar to a union?";
label PEERNPER = "Edited: for your main job, what is the easiest way for you to report your total earnings before taxes or other deductions: hourl";
label PEERNRT = "Edited: even though you told me it is easier to report your earnings another way, are you paid at an hourly rate on your main jo";
label PEERNUOT = "Edited: do you usually receive overtime pay, tips, or commissions at your job?";
label PEERNWKP = "Edited: how many weeks a year do you get paid?";
label PEFNTVTY = "Edited: in what country was your father born?";
label PEGR6COR = "Edited: did you complete six or more graduate or professional school courses?";
label PEGRPROF = "Edited: since completing your bachelor's degree, have you taken any graduate or professional school courses for credit?";
label PEHGCOMP = "Edited: what was the highest grade of regular school you completed before receiving your GED?";
label PEHRACT1 = "Edited: last week, how many hours did you actually work at your main job?";
label PEHRACT2 = "Edited: last week, how many hours did you actually work at your other job(s)?";
label PEHRACTT = "Edited: total hours actually worked last week (sum of PEHRACT1 and PEHRACT2)";
label PEHRAVL = "Edited: last week, could you have worked full time if the hours had been available?";
label PEHRFTPT = "Edited: do you usually work more than 35 hours per week at your job(s)/family business?";
label PEHRRSN1 = "Edited: what is your main reason for working part time?";
label PEHRRSN2 = "Edited: what is the main reason you do not want to work full time?";
label PEHRRSN3 = "Edited: what is the main reason you worked less than 35 hours last week?";
label PEHRUSL1 = "Edited: how many hours per week do you usually work at your main job?";
label PEHRUSL2 = "Edited: how many hours per week do you usually work at your other job(s)?";
label PEHRUSLT = "Edited: total hours usually worked per week (sum of PEHRUSL1 and PEHRUSL2)";
label PEHRWANT = "Edited: do you want to work a full time work week of 35 hours or more per week?";
label PEHSPNON = "Edited: are you Spanish, Hispanic, or Latino?";
label PEIO1COW = "Edited: individual class of worker (main job)";
label PEIO1ICD = "Edited: industry code (main job)";
label PEIO1OCD = "Edited: occupation code (main job)";
label PEIO2COW = "Edited: individual class of worker (second job)";
label PEIO2ICD = "Edited: industry code (second job)";
label PEIO2OCD = "Edited: occupation code (second job)";
label PEJHRSN = "Edited: what is the main reason you left your last job?";
label PEJHWANT = "Edited: do you intend to look for work during the next 12 months?";
label PEJHWKO = "Edited: have you worked at a job or business at any time in the last 12 months?";
label PELAYAVL = "Edited: could you have returned to work during the last seven days if you had been recalled?";
label PELAYDUR = "Edited: duration of layoff (number of weeks)";
label PELAYFTO = "Edited: is the job from which you are on layoff a full time job of 35 hours or more per week?";
label PELAYLK = "Edited: even though you are to be called back to work, have you been looking for work during the last 4 weeks?";
label PELKAVL = "Edited: could you have started a job in the last seven days if one had been offered?";
label PELKDUR = "Edited: duration of job seeking (number of weeks)";
label PELKFTO = "Edited: are you seeking a full time or part time job?";
label PELKLL1O = "Edited: before you started looking for work, what were you doing: working, going to school, or something else?";
label PELKLL2O = "Edited: did you lose or quit that job, or was it a temporary job that ended?";
label PELKLWO = "Edited: when did you last work?";
label PELKM1 = "Edited: what are all of the things you have done to find work during the last 4 weeks? (first method)";
label PELNDAD = "Edited: PULINENO of father";
label PELNMOM = "Edited: PULINENO of mother";
label PEMARITL = "Edited: are you now married, widowed, divorced, separated, or never married?";
label PEMJNUM = "Edited: altogether, how many jobs did you have?";
label PEMJOT = "Edited: in the last seven days, did you have more than one job?";
label PEMLR = "Edited: monthly labor force recode";
label PEMNTVTY = "Edited: in what country was your mother born?";
label PEMOMTYP = "Edited: Is household child a biological, step, or adopted child?";
label PEMS123 = "Edited: was your master's degree program a one-year, two-year, or three-year program?";
label PENATVTY = "Edited: in what country were you born?";
label PENLFACT = "Edited: what best describes your situation at this time? For example, are you disabled, ill, in school, taking care of house or";
label PENLFJH = "Edited: when did you last work at a job or business?";
label PENLFRET = "Edited: are you retired from a job or business?";
label PEPARENT = "Edited: PULINENO of parent";
label PERET1 = "Edited: do you currently want a job, either full or part time?";
label PERRP = "Edited: how is this person related to you?";
label PESCHENR = "Edited: last week, were you enrolled in a high school, college, or university?";
label PESCHFT = "Edited: are you enrolled in school as a full-time or part-time student?";
label PESCHLVL = "Edited: would that be high school, college, or university?";
label PESEX = "Edited: sex";
label PESPOUSE = "Edited: PULINENO of spouse";
label PRABSREA = "Reason not at work by pay status";
label PRAGNA = "Agricultural/non-agricultural industry";
label PRCITFLG = "PRCITSHP: allocation flag";
label PRCITSHP = "Citizenship status";
label PRCIVLF = "Civilian labor force";
label PRCOW1 = "Class of worker recode (main job)";
label PRCOW2 = "Class of worker recode (second job)";
label PRCOWPG = "Class of worker - private or government";
label PRDISC = "Discouraged worker recode";
label PRDISFLG = "Does this person have any of these disability conditions?";
label PRDTCOW1 = "Detailed class of worker recode (main job)";
label PRDTCOW2 = "Detailed class of worker recode (second job)";
label PRDTHSP = "Detailed Hispanic origin group";
label PRDTIND1 = "Detailed industry recode (main job)";
label PRDTIND2 = "Detailed industry recode (second job)";
label PRDTOCC1 = "Detailed occupation recode (main job)";
label PRDTOCC2 = "Detailed occupation recode (second job)";
label PREMP = "Employed persons recode";
label PREMPHRS = "Reason not at work or hours at work";
label PREMPNOT = "Employed, unemployed, or not in the labor force";
label PRERELG = "Earnings edit eligibility flag";
label PRERNHLY = "Hourly earnings at main job (2 implied decimals)";
label PRERNWA = "Weekly earnings at main job (2 implied decimals)";
label PREXPLF = "Experienced labor force employment";
label PRFAMNUM = "Family number recode";
label PRFAMREL = "Family relationship code";
label PRFAMTYP = "Family type recode";
label PRFTLF = "Full time labor force";
label PRHERNAL = "PRERNHLY: allocation flag";
label PRHRUSL = "Usual hours worked weekly";
label PRIMIND1 = "Intermediate industry recode (main job)";
label PRIMIND2 = "Intermediate industry recode (second job)";
label PRINUYER = "Immigrant's year of entry into the U.S.";
label PRIOELG = "Industry and occupation edit eligibility flag";
label PRJOBSEA = "Job search recode";
label PRMARSTA = "Marital status based on Armed Forces participation";
label PRMJIND1 = "Major industry recode (main job)";
label PRMJIND2 = "Major industry recode (second job)";
label PRMJOCC1 = "Major occupation recode (main job)";
label PRMJOCC2 = "Major occupation recode (second job)";
label PRMJOCGR = "Major occupation categories (main job)";
label PRNAGPWS = "Non-agricultural private wage and salary workers recode";
label PRNAGWS = "Non-agricultural wage and salary workers recode";
label PRNLFSCH = "Not in labor force activity - in school or not in school";
label PRNMCHLD = "Number of own children < 18 years of age";
label PRPERTYP = "Type of person recode";
label PRPTHRS = "At work 1-34 hours by hours at work";
label PRPTREA = "Detailed reason for part time work";
label PRSJMJ = "Single/multiple jobholder";
label PRTAGE = "Age";
label PRTFAGE = "Topcode for age (PRTAGE)";
label PRUNEDUR = "Duration of unemployment (number of weeks)";
label PRUNTYPE = "Reason for unemployment";
label PRWERNAL = "PRERNWA: allocation flag";
label PRWKSCH = "Labor force by time worked or lost";
label PRWKSTAT = "Full time or part time work status";
label PRWNTJOB = "Not in labor force recode - want a job or other not in labor force";
label PTDTRACE = "Race (topcoded)";
label PTHR = "Hourly pay topcode flag";
label PTOT = "Weekly overtime amount topcode flag";
label PTWK = "Weekly earnings topcode flag";
label PUABSOT = "In the last seven days, did you have a job either full or part time?";
label PUBUS1 = "Last week, did you do any unpaid work in the family business or farm?";
label PUBUS2OT = "Do you receive any payments or profits from the business?";
label PUBUSCK1 = "Business check item 1: filter for questions on unpaid work";
label PUBUSCK2 = "Business check item 2: skips owners of family business who did not work last week";
label PUBUSCK3 = "Business check item 3: filter for disabled";
label PUBUSCK4 = "Business check item 4: filter for retired";
label PUCHINHH = "Change in household composition";
label PUDIS = "Last time we spoke to someone in this household, you were reported to have a disability. Does your disability continue to preven";
label PUDIS1 = "Does your disability prevent you from accepting any kind of work during the next six months?";
label PUDIS2 = "Do you have a disability that prevents you from accepting any kind of work during the next six months?";
label PUDWCK1 = "Discouraged worker check item 1: filter for discouraged worker";
label PUDWCK2 = "Discouraged worker check item 2: filter for disabled";
label PUDWCK3 = "Discouraged worker check item 3: filter for retired";
label PUDWCK4 = "Discouraged worker check item 4: filter for passive job seekers";
label PUDWCK5 = "Discouraged worker check item 5: filter for passive job seekers";
label PUERN2 = "Weekly overtime earnings (2 implied decimals)";
label PUERNH1C = "What is your hourly rate of pay on this job, excluding overtime pay, tips, or commissions? (2 implied decimals)";
label PUHRCK1 = "Hours respondent check item 1: labor force - remove groups from actual hours series";
label PUHRCK12 = "Hours respondent check item 12: filter for < 15 hours to go to looking series";
label PUHRCK2 = "Hours respondent check item 2: labor force - skips persons responding yes to PUHRFTPT out of PT series";
label PUHRCK3 = "Hours respondent check item 3: labor force - filter for persons going through industry and occupation series";
label PUHRCK4 = "Hours respondent check item 4: labor force - skips persons";
label PUHRCK5 = "Hours respondent check item 5: labor force - filter for multiple jobholders for second job hours";
label PUHRCK6 = "Hours respondent check item 6: labor force - filter for actual job hours, jobs 1 and 2";
label PUHRCK7 = "Hours respondent check item 7: labor force - filter for hours worked paths";
label PUHROFF1 = "Last week, did you lose or take off any hours from your job for any reason such as illness, slack work, vacation, or holiday?";
label PUHROFF2 = "How many hours did you take off?";
label PUHROT1 = "Last week, did you work any overtime or extra hours? (main job)";
label PUHROT2 = "How many additional hours did you work?";
label PUIO1MFG = "Is this business or organization mainly manufacturing, retail trade, wholesale trade, or something else? (main job)";
label PUIO2MFG = "Is this business or organization mainly manufacturing, retail trade, wholesale trade, or something else? (second job)";
label PUIOCK1 = "Industry and occupation check item 1: filter for dependent industry and occupation";
label PUIOCK2 = "Industry and occupation check item 2: filter for previous month's industry and occupation cases";
label PUIOCK3 = "Industry and occupation check item 3: filter for previous month's unknown occupation";
label PUIODP1 = "Last month you were reported to work for (employer's name). Do you still work for (employer's name)?";
label PUIODP2 = "Have the usual activities and duties of your job changed since last month?";
label PUIODP3 = "Last time you were reported as (occupation) and your usual duties were (activities). Is this an accurate description of your cur";
label PUJHCK1 = "Job history check item 1: filter for outgoing rotations";
label PUJHCK2 = "Job history check item 2: filter for persons going through industry and occupation series";
label PUJHCK3 = "Job history check item 3: filter for unemployed job history";
label PUJHCK4 = "Job history check item 4: filter for dependent not in the labor force";
label PUJHCK5 = "Job history check item 5: filter/carryover for dependent not in the labor force";
label PUJHDP1O = "Did you do any of this work in the last 4 weeks?";
label PULAY = "During the last seven days, were you on layoff from a job?";
label PULAY6M = "Have you been given any indication that you will be recalled to work within the next 6 months?";
label PULAYAVR = "Why could you not have started a job in the last week?";
label PULAYCK1 = "Layoff check item 1: filter for previous month layoff status";
label PULAYCK2 = "Layoff check item 2: filter for dependent layoff";
label PULAYCK3 = "Layoff check item 3: dependent industry and occupation filter/carryover";
label PULAYDT = "Has your employer given you a date to return to work? (to layoff job)";
label PULINENO = "Person line number";
label PULK = "Have you been doing anything to find work during the last 4 weeks?";
label PULKAVR = "Why could you not have started a job last week?";
label PULKDK1 = "You said you have been trying to find work. How did you go about looking? (first method)";
label PULKDK2 = "PULKDK1 text: (second method)";
label PULKDK3 = "PULKDK1 text: (third method)";
label PULKDK4 = "PULKDK1 text: (fourth method)";
label PULKDK5 = "PULKDK1 text: (fifth method)";
label PULKDK6 = "PULKDK1 text: (sixth method)";
label PULKM2 = "What are all of the things you have been doing to find work during the last 4 weeks? (second method)";
label PULKM3 = "PULKM2 text: (third method)";
label PULKM4 = "PULKM2 text: (fourth method)";
label PULKM5 = "PULKM2 text: (fifth method)";
label PULKM6 = "PULKM2 text: (sixth method)";
label PULKPS1 = "Can you tell me more about what you did to search for work? (first method)";
label PULKPS2 = "PULKPS1 text: (second method)";
label PULKPS3 = "PULKPS1 text: (third method)";
label PULKPS4 = "PULKPS1 text: (fourth method)";
label PULKPS5 = "PULKPS1 text: (fifth method)";
label PULKPS6 = "PULKPS1 text: (sixth method)";
label PUNLFCK1 = "Not in labor force check item 1: age filter for retirement question";
label PUNLFCK2 = "Not in labor force check item 2: outgoing rotation filter";
label PURETOT = "Last month you were reported to be retired. Are you still retired?";
label PUSLFPRX = "Labor force information collected by self or proxy response";
label PUWK = "Last week, did you do any work for either pay or profit?";
label PXABSPDO = "PEABSPDO: allocation flag";
label PXABSRSN = "PEABSRSN: allocation flag";
label PXAFEVER = "PEAFEVER: allocation flag";
label PXAFNOW = "PEAFNOW: allocation flag";
label PXAFWHN1 = "PEAFWHN1: allocation flag";
label PXAGE = "PRTAGE: allocation flag";
label PXCOHAB = "PECOHAB: allocation flag";
label PXCYC = "PECYC: allocation flag";
label PXDADTYP = "PEDADTYP: allocation flag";
label PXDIPGED = "PEDIPGED: allocation flag";
label PXDISDRS = "PEDISDRS: allocation flag";
label PXDISEAR = "PEDISEAR: allocation flag";
label PXDISEYE = "PEDISEYE: allocation flag";
label PXDISOUT = "PEDISOUT: allocation flag";
label PXDISPHY = "PEDISPHY: allocation flag";
label PXDISREM = "PEDISREM: allocation flag";
label PXDW4WK = "PEDW4WK: allocation flag";
label PXDWAVL = "PEDWAVL: allocation flag";
label PXDWAVR = "PEDWAVR: allocation flag";
label PXDWLKO = "PEDWLKO: allocation flag";
label PXDWLKWK = "PEDWLKWK: allocation flag";
label PXDWRSN = "PEDWRSN: allocation flag";
label PXDWWK = "PEDWWK: allocation flag";
label PXDWWNTO = "PEDWWNTO: allocation flag";
label PXEDUCA = "PEEDUCA: allocation flag";
label PXERN = "PEERN: allocation flag";
label PXERNCOV = "PEERNCOV: allocation flag";
label PXERNH1O = "PEERNH1O: allocation flag";
label PXERNH2 = "PEERNH2: allocation flag";
label PXERNHRO = "PEERNHRO: allocation flag";
label PXERNHRY = "PEERNHRY: allocation flag";
label PXERNLAB = "PEERNLAB: allocation flag";
label PXERNPER = "PEERNPER: allocation flag";
label PXERNRT = "PEERNRT: allocation flag";
label PXERNUOT = "PEERNUOT: allocation flag";
label PXERNWKP = "PEERNWKP: allocation flag";
label PXFNTVTY = "PEFNTVTY: allocation flag";
label PXGR6COR = "PEGR6COR: allocation flag";
label PXGRPROF = "PEGRPROF: allocation flag";
label PXHGCOMP = "PEHGCOMP: allocation flag";
label PXHRACT1 = "PEHRACT1: allocation flag";
label PXHRACT2 = "PEHRACT2: allocation flag";
label PXHRACTT = "PEHRACTT: allocation flag";
label PXHRAVL = "PEHRAVL: allocation flag";
label PXHRFTPT = "PEHRFTPT: allocation flag";
label PXHRRSN1 = "PEHRRSN1: allocation flag";
label PXHRRSN2 = "PEHRRSN2: allocation flag";
label PXHRRSN3 = "PEHRRSN3: allocation flag";
label PXHRUSL1 = "PEHRUSL1: allocation flag";
label PXHRUSL2 = "PEHRUSL2: allocation flag";
label PXHRUSLT = "PEHRUSLT: allocation flag";
label PXHRWANT = "PEHRWANT: allocation flag";
label PXHSPNON = "PEHSPNON: allocation flag";
label PXINUSYR = "PRINUYER: allocation flag";
label PXIO1COW = "PEIO1COW: allocation flag";
label PXIO1ICD = "PEIO1ICD: allocation flag";
label PXIO1OCD = "PEIO1OCD: allocation flag";
label PXIO2COW = "PEIO2COW: allocation flag";
label PXIO2ICD = "PEIO2ICD: allocation flag";
label PXIO2OCD = "PEIO2OCD: allocation flag";
label PXJHRSN = "PEJHRSN: allocation flag";
label PXJHWANT = "PEJHWANT: allocation flag";
label PXJHWKO = "PEJHWKO: allocation flag";
label PXLAYAVL = "PELAYAVL: allocation flag";
label PXLAYDUR = "PELAYDUR: allocation flag";
label PXLAYFTO = "PELAYFTO: allocation flag";
label PXLAYLK = "PELAYLK: allocation flag";
label PXLKAVL = "PELKAVL: allocation flag";
label PXLKDUR = "PELKDUR: allocation flag";
label PXLKFTO = "PELKFTO: allocation flag";
label PXLKLL1O = "PELKLL1O: allocation flag";
label PXLKLL2O = "PELKLL2O: allocation flag";
label PXLKLWO = "PELKLWO: allocation flag";
label PXLKM1 = "PELKM1: allocation flag";
label PXLNDAD = "PELNDAD: allocation flag";
label PXLNMOM = "PELNMOM: allocation flag";
label PXMARITL = "PEMARITL: allocation flag";
label PXMJNUM = "PEMJNUM: allocation flag";
label PXMJOT = "PEMJOT: allocation flag";
label PXMLR = "PEMLR: allocation flag";
label PXMNTVTY = "PEMNTVTY: allocation flag";
label PXMOMTYP = "PEMOMTYP: allocation flag";
label PXMS123 = "PEMS123: allocation flag";
label PXNATVTY = "PENATVTY: allocation flag";
label PXNLFACT = "PENLFACT: allocation flag";
label PXNLFJH = "PENLFJH: allocation flag";
label PXNLFRET = "PENLFRET: allocation flag";
label PXPARENT = "PEPARENT: allocation flag";
label PXRACE1 = "PTDTRACE: allocation flag";
label PXRET1 = "PERET1: allocation flag";
label PXRRP = "PERRP: allocation flag";
label PXSCHENR = "PESCHENR: allocation flag";
label PXSCHFT = "PESCHFT: allocation flag";
label PXSCHLVL = "PESCHLVL: allocation flag";
label PXSEX = "PESEX: allocation flag";
label PXSPOUSE = "PESPOUSE: allocation flag";
label QSTNUM = "Unique household identifier";
label TRATUSR = "ATUS respondent";
label TUCASEID = "ATUS Case ID (14-digit identifier)";
label TULINENO = "ATUS person line number";
label PRDASIAN = "Detailed Asian race recode";
label PEPDEMP1 = "Does this person usually have any paid employees?";
label PTNMEMP1 = "Excluding all owners, how many paid employees does this person usually have?";
label PEPDEMP2 = "Does this person usually have any paid employees?";
label PTNMEMP2 = "Excluding all owners, how many paid employees does this person usually have?";
label PXPDEMP1 = "PEPDEMP1: allocation flag";
label PXNMEMP1 = "PTNMEMP1: allocation flag";
label PXPDEMP2 = "PEPDEMP2: allocation flag";
label PXNMEMP2 = "PTNMEMP2: allocation flag";
run;
 
proc format;
value GEREG /*GEREG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Northeast"
2 = "Midwest (formerly North Central)"
3 = "South"
4 = "West"
;
value GEDIV /*GEDIV*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "New England"
2 = "Middle Atlantic"
3 = "East North Central"
4 = "West North Central"
5 = "South Atlantic"
6 = "East South Central"
7 = "West South Central"
8 = "Mountain"
9 = "Pacific"
;
value GTMETSTA /*GTMETSTA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Metropolitan"
2 = "Non-metropolitan"
3 = "Not identified"
;
value HEFAMINC /*HEFAMINC*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Less than $5,000"
2 = "$5,000 to $7,499"
3 = "$7,500 to $9,999"
4 = "$10,000 to $12,499"
5 = "$12,500 to $14,999"
6 = "$15,000 to $19,999"
7 = "$20,000 to $24,999"
8 = "$25,000 to $29,999"
9 = "$30,000 to $34,999"
10 = "$35,000 to $39,999"
11 = "$40,000 to $49,999"
12 = "$50,000 to $59,999"
13 = "$60,000 to $74,999"
14 = "$75,000 to $99,999"
15 = "$100,000 to $149,999"
16 = "$150,000 and over"
;
value HEHOUSUT /*HEHOUSUT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "House, apartment, flat"
2 = "Housing unit in nontransient hotel, motel, etc."
3 = "Housing unit permanent in transient hotel, motel"
4 = "Housing unit in rooming house"
5 = "Mobile home or trailer with no permanent room added"
6 = "Mobile home or trailer with 1 or more rooms added"
7 = "Housing unit not specified above"
8 = "Quarters not housing unit in rooming/boarding house"
9 = "Unit not permanent in transient hotel/motel"
10 = "Unoccupied tent site or trailer site"
11 = "Student quarters in college dorm"
12 = "Other unit not specified above"
;
value HEPHONEO /*HEPHONEO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Unknown"
1 = "Yes"
2 = "No"
;
value HETELAVL /*HETELAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value HETELHHD /*HETELHHD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value HETENURE /*HETENURE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Owned or being bought by a household member"
2 = "Rented for cash"
3 = "Occupied without payment of cash rent"
;
value HRHTYPE /*HRHTYPE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Husband/wife primary family (neither Armed Forces)"
2 = "Husband/wife primary family (either/both Armed Forces)"
3 = "Unmarried civilian male - primary family householder"
4 = "Unmarried civilian female - primary family householder"
5 = "Primary family householder - respondent in Armed Forces, unmarried"
6 = "Civilian male primary individual"
7 = "Civilian female primary individual"
8 = "Primary individual householder - respondent in Armed Forces"
9 = "Group quarters with family"
10 = "Group quarters without family"
;
value HRINTSTA /*HRINTSTA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Interview"
;
value HRLONGLK /*HRLONGLK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Replacement household - no link to prior month"
2 = "Link to previous month"
;
value HRMIS /*HRMIS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
8 = "Month in sample 8 (MIS-8)"
;
value HUBUS /*HUBUS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value HUFINAL /*HUFINAL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Fully complete CATI"
2 = "Partially complete CATI"
4 = "Partial CATI - not complete at closeout"
5 = "Labor force complete, supplement incomplete, CATI"
6 = "Fully completed labor force interview with supplement complete, Don't Know items incomplete at closeout, ASEC only"
201 = "Fully complete CAPI"
203 = "Sufficient partial CAPI - pre-closeout"
204 = "Sufficient partial CAPI - at closeout"
205 = "Labor force complete, supplement incomplete, CAPI"
;
value HUINTTYP /*HUINTTYP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Personal"
2 = "Telephone"
;
value HUTYPB /*HUTYPB*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Vacant regular"
2 = "Temporarily occupied by persons with usual residence elsewhere"
3 = "Vacant storage of household furniture"
4 = "Unfit or to be demolished"
9 = "Other type B"
;
value HXFAMINC /*HXFAMINC*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value HXHOUSUT /*HXHOUSUT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value HXPHONEO /*HXPHONEO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value HXTELAVL /*HXTELAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value HXTELHHD /*HXTELHHD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PEABSPDO /*PEABSPDO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEABSRSN /*PEABSRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "On layoff"
2 = "Slack work/business conditions"
3 = "Waiting for a new job to begin"
4 = "Vacation/personal days"
5 = "Own illness/injury/medical problems"
6 = "Childcare problems"
7 = "Other family/personal obligation"
8 = "Maternity/paternity leave"
9 = "Labor dispute"
10 = "Weather affected job"
11 = "School/training"
12 = "Civic/military duty"
13 = "Does not work in the business"
14 = "Other"
;
value PEAFEVER /*PEAFEVER*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEAFNOW /*PEAFNOW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEAFWHNA /*altered: PEAFWHN1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "September 2001 or later"
2 = "August 1990 to August 2001"
3 = "May 1975 to July 1990"
4 = "Vietnam Era (August 1964 to April 1975)"
5 = "February 1955 to July 1964"
6 = "Korean War (July 1950 to January 1955)"
7 = "January 1947 to June 1950"
8 = "World War II (December 1941 to December 1946)"
9 = "November 1941 or earlier"
;
value PEAFWHNB /*altered: PEAFWHN2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "September 2001 or later"
2 = "August 1990 to August 2001"
3 = "May 1975 to July 1990"
4 = "Vietnam Era (August 1964 to April 1975)"
5 = "February 1955 to July 1964"
6 = "Korean War (July 1950 to January 1955)"
7 = "January 1947 to June 1950"
8 = "World War II (December 1941 to December 1946)"
9 = "November 1941 or earlier"
;
value PEAFWHNC /*altered: PEAFWHN3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "September 2001 or later"
2 = "August 1990 to August 2001"
3 = "May 1975 to July 1990"
4 = "Vietnam Era (August 1964 to April 1975)"
5 = "February 1955 to July 1964"
6 = "Korean War (July 1950 to January 1955)"
7 = "January 1947 to June 1950"
8 = "World War II (December 1941 to December 1946)"
9 = "November 1941 or earlier"
;
value PEAFWHND /*altered: PEAFWHN4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "September 2001 or later"
2 = "August 1990 to August 2001"
3 = "May 1975 to July 1990"
4 = "Vietnam Era (August 1964 to April 1975)"
5 = "February 1955 to July 1964"
6 = "Korean War (July 1950 to January 1955)"
7 = "January 1947 to June 1950"
8 = "World War II (December 1941 to December 1946)"
9 = "November 1941 or earlier"
;
value PECYC /*PECYC*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Less than 1 year (includes 0 years completed)"
2 = "The first or freshman year"
3 = "The second or sophomore year"
4 = "The third or junior year"
5 = "Four or more years"
;
value PEDADTYP /*PEDADTYP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Biological"
2 = "Step"
3 = "Adopted"
;
value PEDIPGED /*PEDIPGED*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Graduation from high school"
2 = "GED or other equivalent"
;
value PEDISDRS /*PEDISDRS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDISEAR /*PEDISEAR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDISEYE /*PEDISEYE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDISOUT /*PEDISOUT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDISPHY /*PEDISPHY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDISREM /*PEDISREM*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDW4WK /*PEDW4WK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDWAVL /*PEDWAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDWAVR /*PEDWAVR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Own temporary illness"
2 = "Going to school"
3 = "Other"
;
value PEDWLKO /*PEDWLKO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDWLKWK /*PEDWLKWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDWRSN /*PEDWRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Believes no work available in area of expertise"
2 = "Couldn`t find any work"
3 = "Lacks necessary schooling/training"
4 = "Employers think too young or too old"
5 = "Other types of discrimination"
6 = "Can`t arrange childcare"
7 = "Family responsibilities"
8 = "In school or other training"
9 = "Ill-health, physical disability"
10 = "Transportation problems"
11 = "Other"
;
value PEDWWK /*PEDWWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEDWWNTO /*PEDWWNTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes, or maybe/it depends"
2 = "No"
3 = "Retired"
4 = "Disabled"
5 = "Unable to work"
;
value PEEDUCA /*PEEDUCA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
31 = "Less than 1st grade"
32 = "1st, 2nd, 3rd, or 4th grade"
33 = "5th or 6th grade"
34 = "7th or 8th grade"
35 = "9th grade"
36 = "10th grade"
37 = "11th grade"
38 = "12th grade - no diploma"
39 = "High school graduate - diploma or equivalent (GED)"
40 = "Some college but no degree"
41 = "Associate degree - occupational/vocational"
42 = "Associate degree - academic program"
43 = "Bachelor's degree (BA, AB, BS, etc.)"
44 = "Master's degree (MA, MS, MEng, MEd, MSW, etc.)"
45 = "Professional school degree (MD, DDS, DVM, etc.)"
46 = "Doctoral degree (PhD, EdD, etc.)"
;
value PEERNCOV /*PEERNCOV*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEERNHRY /*PEERNHRY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Hourly worker"
2 = "Non-hourly worker"
;
value PEERNLAB /*PEERNLAB*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEERNPER /*PEERNPER*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Hourly"
2 = "Weekly"
3 = "Bi-weekly"
4 = "Twice monthly"
5 = "Monthly"
6 = "Annually"
7 = "Other"
;
value PEERNRT /*PEERNRT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEERNUOT /*PEERNUOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEFNTVTY /*PEFNTVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
57 = "United States"
66 = "Guam"
73 = "Puerto Rico"
78 = "U.S. Virgin Islands"
96 = "U.S. Outlying Area"
100-554 = "Foreign country or at sea"
555 = "Abroad, country not known"
;
value PEGR6COR /*PEGR6COR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEGRPROF /*PEGRPROF*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEHGCOMP /*PEHGCOMP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Less than 1st grade"
2 = "1st, 2nd, 3rd, or 4th grade"
3 = "5th or 6th grade"
4 = "7th or 8th grade"
5 = "9th grade"
6 = "10th grade"
7 = "11th grade"
8 = "12th grade - no diploma"
;
value PEHRAVL /*PEHRAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEHRFTPT /*PEHRFTPT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time"
2 = "Part time"
3 = "Hours vary"
;
value PEHRRSNA /*altered: PEHRRSN1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Slack work/business conditions"
2 = "Could only find part time work"
3 = "Seasonal work"
4 = "Childcare problems"
5 = "Other family/personal obligations"
6 = "Health/medical limitations"
7 = "School/training"
8 = "Retired/Social Security limit on earnings"
9 = "Full time work week is less than 35 hours"
10 = "Other"
;
value PEHRRSNB /*altered: PEHRRSN2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Childcare problems"
2 = "Other family/personal obligations"
3 = "Health/medical limitations"
4 = "School/training"
5 = "Retired/Social Security limit on earnings"
6 = "Full time work week is less than 35 hours"
7 = "Other"
;
value PEHRRSNC /*altered: PEHRRSN3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Slack work/business conditions"
2 = "Seasonal work"
3 = "Job started or ended during week"
4 = "Vacation/personal day"
5 = "Own illness/injury/medical appointment"
6 = "Holiday (legal or religious)"
7 = "Childcare problems"
8 = "Other family/personal obligations"
9 = "Labor dispute"
10 = "Weather affected job"
11 = "School/training"
12 = "Civic/military duty"
13 = "Other reason"
;
value PEHRWANT /*PEHRWANT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Regular hours are full time"
;
value PEHSPNON /*PEHSPNON*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Hispanic"
2 = "Non-Hispanic"
;
value PEIO1COW /*PEIO1COW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Government, federal"
2 = "Government, state"
3 = "Government, local"
4 = "Private, for profit"
5 = "Private, nonprofit"
6 = "Self-employed, incorporated"
7 = "Self-employed, unincorporated"
8 = "Without pay"
;
value PEIO2COW /*PEIO2COW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Government, federal"
2 = "Government, state"
3 = "Government, local"
4 = "Private, for profit"
5 = "Private, nonprofit"
6 = "Self-employed, incorporated"
7 = "Self-employed, unincorporated"
8 = "Without pay"
;
value PEJHRSN /*PEJHRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Personal/family (including pregnancy)"
2 = "Return to school"
3 = "Health"
4 = "Retirement or old age"
5 = "Temporary, seasonal, or intermittent job completed"
6 = "Slack work/business conditions"
7 = "Unsatisfactory work arrangements (hours, pay, etc.)"
8 = "Other"
;
value PEJHWANT /*PEJHWANT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes, or it depends"
2 = "No"
;
value PEJHWKO /*PEJHWKO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PELAYAVL /*PELAYAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PELAYFTO /*PELAYFTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PELAYLK /*PELAYLK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PELKAVL /*PELKAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PELKFTO /*PELKFTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Doesn't matter"
;
value PELKLL1O /*PELKLL1O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Working"
2 = "School"
3 = "Left military service"
4 = "Something else"
;
value PELKLL2O /*PELKLL2O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Lost job"
2 = "Quit job"
3 = "Temporary job ended"
;
value PELKLWO /*PELKLWO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Within the last 12 months"
2 = "More than 12 months ago"
3 = "Never worked"
;
value PELKMA /*altered: PELKM1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
12 = "Nothing"
13 = "Other passive"
;
value PEMARITL /*PEMARITL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Married - spouse present"
2 = "Married - spouse absent"
3 = "Widowed"
4 = "Divorced"
5 = "Separated"
6 = "Never married"
;
value PEMJNUM /*PEMJNUM*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
2 = "Two jobs"
3 = "Three jobs"
4 = "Four or more jobs"
;
value PEMJOT /*PEMJOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEMLR /*PEMLR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed - at work"
2 = "Employed - absent"
3 = "Unemployed - on layoff"
4 = "Unemployed - looking"
5 = "Not in labor force - retired"
6 = "Not in labor force - disabled"
7 = "Not in labor force - other"
;
value PEMNTVTY /*PEMNTVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
57 = "United States"
66 = "Guam"
72 = "Puerto Rico"
73 = "Puerto Rico"
78 = "U.S. Virgin Islands"
96 = "U.S. Outlying Area"
100-554 = "Foreign country or at sea"
555 = "Abroad, country not known"
;
value PEMOMTYP /*PEMOMTYP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Biological"
2 = "Step"
3 = "Adopted"
;
value PEMS12C /*altered: PEMS123*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "One-year program"
2 = "Two-year program"
3 = "Three-year program (or longer)"
;
value PENATVTY /*PENATVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
57 = "United States"
66 = "Guam"
72 = "Puerto Rico"
73 = "Puerto Rico"
78 = "U.S. Virgin Islands"
96 = "U.S. Outlying Area"
100-554 = "Foreign country or at sea"
555 = "Abroad, country not known"
;
value PENLFACT /*PENLFACT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Disabled"
2 = "Ill"
3 = "In school"
4 = "Taking care of house or family"
5 = "In retirement"
6 = "Something else/other"
;
value PENLFJH /*PENLFJH*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Within the last 12 months"
2 = "More than 12 months ago"
3 = "Never worked"
;
value PENLFRET /*PENLFRET*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PERETA /*altered: PERET1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Has a job"
;
value PERRP /*PERRP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Reference person living with relatives"
2 = "Reference person living without relatives"
3 = "Spouse"
4 = "Own child"
5 = "Grandchild"
6 = "Parent"
7 = "Brother/sister"
8 = "Other relative of reference person"
9 = "Foster child"
10 = "Nonrelative of reference person living with relatives"
12 = "Nonrelative of reference person living without relatives"
13 = "Unmarried partner living with relatives"
14 = "Unmarried partner living without relatives"
15 = "Housemate/roommate living with relatives"
16 = "Housemate/roommate living without relatives"
17 = "Roomer/boarder living with relatives"
18 = "Roomer/boarder living without relatives"
;
value PESCHENR /*PESCHENR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PESCHFT /*PESCHFT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time"
2 = "Part time"
;
value PESCHLVL /*PESCHLVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "High school"
2 = "College or university"
;
value PESEX /*PESEX*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Male"
2 = "Female"
;
value PRABSREA /*PRABSREA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time paid - vacation"
2 = "Full time paid - own illness"
3 = "Full time paid - childcare problems"
4 = "Full time paid - other family/personal obligation"
5 = "Full time paid - maternity/paternity leave"
6 = "Full time paid - labor dispute"
7 = "Full time paid - weather affected job"
8 = "Full time paid - school/training"
9 = "Full time paid - civic/military duty"
10 = "Full time paid - other"
11 = "Full time unpaid - vacation"
12 = "Full time unpaid - own illness"
13 = "Full time unpaid - childcare problems"
14 = "Full time unpaid - other family/personal obligation"
15 = "Full time unpaid - maternity/paternity leave"
16 = "Full time unpaid - labor dispute"
17 = "Full time unpaid - weather affected job"
18 = "Full time unpaid - school/training"
19 = "Full time unpaid - civic/military duty"
20 = "Full time unpaid - other"
21 = "Part time paid - vacation"
22 = "Part time paid - own illness"
23 = "Part time paid - childcare problems"
24 = "Part time paid - other family/personal obligation"
25 = "Part time paid - maternity/paternity leave"
26 = "Part time paid - labor dispute"
27 = "Part time paid - weather affected job"
28 = "Part time paid - school/training"
29 = "Part time paid - civic/military duty"
30 = "Part time paid - other"
31 = "Part time unpaid - vacation"
32 = "Part time unpaid - own illness"
33 = "Part time unpaid - childcare problems"
34 = "Part time unpaid - other family/personal obligation"
35 = "Part time unpaid - maternity/paternity leave"
36 = "Part time unpaid - labor dispute"
37 = "Part time unpaid - weather affected job"
38 = "Part time unpaid - school/training"
39 = "Part time unpaid - civic/military duty"
40 = "Part time unpaid - other"
;
value PRAGNA /*PRAGNA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agricultural"
2 = "Non-agricultural"
;
value PRCITFLG /*PRCITFLG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Longitudinal Value"
31 = "Blank To Allocated Longitudinal Value"
32 = "Dk To Allocated Longitudinal Value"
33 = "Refused To Allocated Longitudinal Value"
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PRCITSHP /*PRCITSHP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Native, born in United States"
2 = "Native, born in Puerto Rico or U.S. Outlying Area"
3 = "Native, born abroad of American parent or parents"
4 = "Foreign born, U.S. citizen by naturalization"
5 = "Foreign born, not a U.S. citizen"
;
value PRCIVLF /*PRCIVLF*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "In civilian labor force"
2 = "Not in civilian labor force"
;
value PRCOWA /*altered: PRCOW1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Federal government"
2 = "State government"
3 = "Local government"
4 = "Private (including incorporated self-employed)"
5 = "Self-employed, unincorporated"
6 = "Without pay"
;
value PRCOWB /*altered: PRCOW2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Federal government"
2 = "State government"
3 = "Local government"
4 = "Private (including incorporated self-employed)"
5 = "Self-employed, unincorporated"
6 = "Without pay"
;
value PRCOWPG /*PRCOWPG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Private"
2 = "Government"
;
value PRDISC /*PRDISC*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Discouraged worker"
2 = "Conditionally interested"
3 = "Not available"
;
value PRDISCFLG /*PRDISFLG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PRDTCOWA /*altered: PRDTCOW1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agricultural, wage/salary, private"
2 = "Agricultural, wage/salary, government"
3 = "Agricultural, self-employed"
4 = "Agricultural, unpaid"
5 = "Non-agricultural, wage/salary, private households"
6 = "Non-agricultural, wage/salary, other private"
7 = "Non-agricultural, wage/salary, federal government"
8 = "Non-agricultural, wage/salary, state government"
9 = "Non-agricultural, wage/salary, local government"
10 = "Non-agricultural, self-employed"
11 = "Non-agricultural, unpaid"
;
value PRDTCOWB /*altered: PRDTCOW2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agricultural, wage/salary, private"
2 = "Agricultural, wage/salary, government"
3 = "Agricultural, self-employed"
4 = "Agricultural, unpaid"
5 = "Non-agricultural, wage/salary, private households"
6 = "Non-agricultural, wage/salary, other private"
7 = "Non-agricultural, wage/salary, federal government"
8 = "Non-agricultural, wage/salary, state government"
9 = "Non-agricultural, wage/salary, local government"
10 = "Non-agricultural, self-employed"
11 = "Non-agricultural, unpaid"
;
value PRDTHSP /*PRDTHSP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Mexican"
2 = "Puerto Rican"
3 = "Cuban"
4 = "Central/South American (before 2014) or Dominican (starting in 2014)"
5 = "Other Spanish (before 2014) or Salvadoran (starting in 2014)"
6 = "Other Central American, excluding Salvadoran (starting in 2014)"
7 = "South American (starting in 2014)"
8 = "Other Spanish (starting in 2014)"
;
value PRDTINDA /*altered: PRDTIND1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture"
2 = "Forestry, logging, fishing, hunting, and trapping"
3 = "Mining"
4 = "Construction"
5 = "Nonmetallic mineral product manufacturing"
6 = "Primary metals and fabricated metal products"
7 = "Machinery manufacturing"
8 = "Computer and electronic product manufacturing"
9 = "Electrical equipment, appliance manufacturing"
10 = "Transportation equipment manufacturing"
11 = "Wood product manufacturing"
12 = "Furniture and fixtures manufacturing"
13 = "Miscellaneous and not specified manufacturing"
14 = "Food manufacturing"
15 = "Beverage and tobacco product manufacturing"
16 = "Textile, apparel, and leather manufacturing"
17 = "Paper manufacturing and printing"
18 = "Petroleum and coal products manufacturing"
19 = "Chemical manufacturing"
20 = "Plastics and rubber products manufacturing"
21 = "Wholesale trade"
22 = "Retail trade"
23 = "Transportation and warehousing"
24 = "Utilities"
25 = "Publishing industries (except internet)"
26 = "Motion picture and sound recording industries"
27 = "Broadcasting (except internet)"
28 = "Internet publishing and broadcasting"
29 = "Telecommunications"
30 = "Internet service providers and data processing services"
31 = "Other information services"
32 = "Finance"
33 = "Insurance"
34 = "Real estate"
35 = "Rental and leasing services"
36 = "Professional, scientific, and technical services"
37 = "Management of companies and enterprises"
38 = "Administrative and support services"
39 = "Waste management and remediation services"
40 = "Educational services"
41 = "Hospitals"
42 = "Health care services, except hospitals"
43 = "Social assistance"
44 = "Arts, entertainment, and recreation"
45 = "Traveler accommodation"
46 = "Food services and drinking places"
47 = "Repair and maintenance"
48 = "Personal and laundry services"
49 = "Membership associations and organizations"
50 = "Private households"
51 = "Public administration"
52 = "Armed Forces"
;
value PRDTINDB /*altered: PRDTIND2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture"
2 = "Forestry, logging, fishing, hunting, and trapping"
3 = "Mining"
4 = "Construction"
5 = "Nonmetallic mineral product manufacturing"
6 = "Primary metals and fabricated metal products"
7 = "Machinery manufacturing"
8 = "Computer and electronic product manufacturing"
9 = "Electrical equipment, appliance manufacturing"
10 = "Transportation equipment manufacturing"
11 = "Wood product manufacturing"
12 = "Furniture and fixtures manufacturing"
13 = "Miscellaneous and not specified manufacturing"
14 = "Food manufacturing"
15 = "Beverage and tobacco product manufacturing"
16 = "Textile, apparel, and leather manufacturing"
17 = "Paper manufacturing and printing"
18 = "Petroleum and coal products manufacturing"
19 = "Chemical manufacturing"
20 = "Plastics and rubber products manufacturing"
21 = "Wholesale trade"
22 = "Retail trade"
23 = "Transportation and warehousing"
24 = "Utilities"
25 = "Publishing industries (except internet)"
26 = "Motion picture and sound recording industries"
27 = "Broadcasting (except internet)"
28 = "Internet publishing and broadcasting"
29 = "Telecommunications"
30 = "Internet service providers and data processing services"
31 = "Other information services"
32 = "Finance"
33 = "Insurance"
34 = "Real estate"
35 = "Rental and leasing services"
36 = "Professional, scientific, and technical services"
37 = "Management of companies and enterprises"
38 = "Administrative and support services"
39 = "Waste management and remediation services"
40 = "Educational services"
41 = "Hospitals"
42 = "Health care services, except hospitals"
43 = "Social assistance"
44 = "Arts, entertainment, and recreation"
45 = "Traveler accommodation"
46 = "Food services and drinking places"
47 = "Repair and maintenance"
48 = "Personal and laundry services"
49 = "Membership associations and organizations"
50 = "Private households"
51 = "Public administration"
52 = "Armed Forces"
;
value PRDTOCCA /*altered: PRDTOCC1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Management occupations"
2 = "Business and financial operations occupations"
3 = "Computer and mathematical science occupations"
4 = "Architecture and engineering occupations"
5 = "Life, physical, and social science occupations"
6 = "Community and social service occupations"
7 = "Legal occupations"
8 = "Education, training, and library occupations"
9 = "Arts, design, entertainment, sports, and media occupations"
10 = "Healthcare practitioner and technical occupations"
11 = "Healthcare support occupations"
12 = "Protective service occupations"
13 = "Food preparation and serving related occupations"
14 = "Building and grounds cleaning and maintenance occupations"
15 = "Personal care and service occupations"
16 = "Sales and related occupations"
17 = "Office and administrative support occupations"
18 = "Farming, fishing, and forestry occupations"
19 = "Construction and extraction occupations"
20 = "Installation, maintenance, and repair occupations"
21 = "Production occupations"
22 = "Transportation and material moving occupations"
23 = "Armed Forces"
;
value PRDTOCCB /*altered: PRDTOCC2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Management occupations"
2 = "Business and financial operations occupations"
3 = "Computer and mathematical science occupations"
4 = "Architecture and engineering occupations"
5 = "Life, physical, and social science occupations"
6 = "Community and social service occupations"
7 = "Legal occupations"
8 = "Education, training, and library occupations"
9 = "Arts, design, entertainment, sports, and media occupations"
10 = "Healthcare practitioner and technical occupations"
11 = "Healthcare support occupations"
12 = "Protective service occupations"
13 = "Food preparation and serving related occupations"
14 = "Building and grounds cleaning and maintenance occupations"
15 = "Personal care and service occupations"
16 = "Sales and related occupations"
17 = "Office and administrative support occupations"
18 = "Farming, fishing, and forestry occupations"
19 = "Construction and extraction occupations"
20 = "Installation, maintenance, and repair occupations"
21 = "Production occupations"
22 = "Transportation and material moving occupations"
23 = "Armed Forces"
;
value PREMP /*PREMP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed persons (excluding agriculture and private households)"
;
value PREMPHRS /*PREMPHRS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Unemployed and not in the labor force"
1 = "With job, not at work - illness"
2 = "With job, not at work - vacation"
3 = "With job, not at work - weather affected job"
4 = "With job, not at work - labor dispute"
5 = "With job, not at work - childcare problems"
6 = "With job, not at work - family/personal obligation"
7 = "With job, not at work - maternity/paternity"
8 = "With job, not at work - school/training"
9 = "With job, not at work - civic/military duty"
10 = "With job, not at work - does not work in business"
11 = "With job, not at work - other"
12 = "At work, 1-4 hours"
13 = "At work, 5-14 hours"
14 = "At work, 15-21 hours"
15 = "At work, 22-29 hours"
16 = "At work, 30-34 hours"
17 = "At work, 35-39 hours"
18 = "At work, 40 hours"
19 = "At work, 41-47 hours"
20 = "At work, 48 hours"
21 = "At work, 49-59 hours"
22 = "At work, 60 hours or more"
;
value PREMPNOT /*PREMPNOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed"
2 = "Unemployed"
3 = "Not in labor force - discouraged"
4 = "Not in labor force - other"
;
value PRERELG /*PRERELG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not eligible for edit"
1 = "Eligible for edit"
;
value PREXPLF /*PREXPLF*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Employed"
2 = "Unemployed"
;
value PRFAMNUM /*PRFAMNUM*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not a family member"
1 = "Primary family member only"
2 = "Subfamily no. 2 member"
3 = "Subfamily no. 3 member"
4 = "Subfamily no. 4 member"
5 = "Subfamily no. 5 member"
6 = "Subfamily no. 6 member"
7 = "Subfamily no. 7 member"
8 = "Subfamily no. 8 member"
9 = "Subfamily no. 9 member"
10 = "Subfamily no. 10 member"
11 = "Subfamily no. 11 member"
12 = "Subfamily no. 12 member"
13 = "Subfamily no. 13 member"
14 = "Subfamily no. 14 member"
15 = "Subfamily no. 15 member"
16 = "Subfamily no. 16 member"
17 = "Subfamily no. 17 member"
18 = "Subfamily no. 18 member"
19 = "Subfamily no. 19 member"
;
value PRFAMREL /*PRFAMREL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not a family member"
1 = "Reference person"
2 = "Spouse"
3 = "Child"
4 = "Other relative (primary family only)"
;
value PRFAMTYP /*PRFAMTYP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Primary family"
2 = "Primary individual"
3 = "Related subfamily"
4 = "Unrelated subfamily"
5 = "Secondary individual"
;
value PRFTLF /*PRFTLF*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Full time labor force"
2 = "Part time labor force"
;
value PRHERNAL /*PRHERNAL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "PRERNHLY does not contain allocated information"
1 = "PRERNHLY contains allocated information"
;
value PRHRUSL /*PRHRUSL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "0-20 hours"
2 = "21-34 hours"
3 = "35-39 hours"
4 = "40 hours"
5 = "41-49 hours"
6 = "50 or more hours"
7 = "Varies - full time"
8 = "Varies - part time"
;
value PRIMINDA /*altered: PRIMIND1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture, forestry, fishing, and hunting"
2 = "Mining"
3 = "Construction"
4 = "Manufacturing - durable goods"
5 = "Manufacturing - non-durable goods"
6 = "Wholesale trade"
7 = "Retail trade"
8 = "Transportation and warehousing"
9 = "Utilities"
10 = "Information"
11 = "Finance and insurance"
12 = "Real estate and rental and leasing"
13 = "Professional and technical services"
14 = "Management, administrative and waste management services"
15 = "Educational services"
16 = "Health care and social services"
17 = "Arts, entertainment, and recreation"
18 = "Accommodation and food services"
19 = "Private households"
20 = "Other services, except private households"
21 = "Public administration"
22 = "Armed Forces"
;
value PRIMINDB /*altered: PRIMIND2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture, forestry, fishing, and hunting"
2 = "Mining"
3 = "Construction"
4 = "Manufacturing - durable goods"
5 = "Manufacturing - non-durable goods"
6 = "Wholesale trade"
7 = "Retail trade"
8 = "Transportation and warehousing"
9 = "Utilities"
10 = "Information"
11 = "Finance and insurance"
12 = "Real estate and rental and leasing"
13 = "Professional and technical services"
14 = "Management, administrative and waste management services"
15 = "Educational services"
16 = "Health care and social services"
17 = "Arts, entertainment, and recreation"
18 = "Accommodation and food services"
19 = "Private households"
20 = "Other services, except private households"
21 = "Public administration"
22 = "Armed Forces"
;
value PRINUYER /*PRINUYER*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not foreign born"
1 = "Before 1950"
2 = "1950-1959"
3 = "1960-1964"
4 = "1965-1969"
5 = "1970-1974"
6 = "1975-1979"
7 = "1980-1981"
8 = "1982-1983"
9 = "1984-1985"
10 = "1986-1987"
11 = "1988-1989"
12 = "1990-1991"
13 = "1992-1993"
14 = "1994-1995"
15 = "1996-1997"
16 = "1998-1999"
17 = "2000-2001"
18 = "2002-2003"
19 = "2004-2005"
20 = "2006-2007"
21 = "2008-2009"
22 = "2010-2013 or 2010-2011 (see note)"
23 = "2012-2014"
;
value PRIOELG /*PRIOELG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not eligible for edit"
1 = "Eligible for edit"
;
value PRJOBSEA /*PRJOBSEA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Looked last 4 weeks - not worked"
2 = "Looked last 4 weeks - worked"
3 = "Looked last 4 weeks - layoff"
4 = "Unavailable job seekers"
5 = "No recent job search"
;
value PRMARSTA /*PRMARSTA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Married, civilian spouse present"
2 = "Married, Armed Forces spouse present"
3 = "Married, spouse absent (except separated)"
4 = "Widowed"
5 = "Divorced"
6 = "Separated"
7 = "Never married"
;
value PRMJINDA /*altered: PRMJIND1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture, forestry, fishing, and hunting"
2 = "Mining"
3 = "Construction"
4 = "Manufacturing"
5 = "Wholesale and retail trade"
6 = "Transportation and utilities"
7 = "Information"
8 = "Financial activities"
9 = "Professional and business services"
10 = "Educational and health services"
11 = "Leisure and hospitality"
12 = "Other services"
13 = "Public administration"
14 = "Armed Forces"
;
value PRMJINDB /*altered: PRMJIND2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Agriculture, forestry, fishing, and hunting"
2 = "Mining"
3 = "Construction"
4 = "Manufacturing"
5 = "Wholesale and retail trade"
6 = "Transportation and utilities"
7 = "Information"
8 = "Financial activities"
9 = "Professional and business services"
10 = "Educational and health services"
11 = "Leisure and hospitality"
12 = "Other services"
13 = "Public administration"
14 = "Armed Forces"
;
value PRMJOCCA /*altered: PRMJOCC1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Management, business, and financial occupations"
2 = "Professional and related occupations"
3 = "Service occupations"
4 = "Sales and related occupations"
5 = "Office and administrative support occupations"
6 = "Farming, fishing, and forestry occupations"
7 = "Construction and extraction occupations"
8 = "Installation, maintenance, and repair occupations"
9 = "Production occupations"
10 = "Transportation and material moving occupations"
11 = "Armed Forces"
;
value PRMJOCCB /*altered: PRMJOCC2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Management, business, and financial occupations"
2 = "Professional and related occupations"
3 = "Service occupations"
4 = "Sales and related occupations"
5 = "Office and administrative support occupations"
6 = "Farming, fishing, and forestry occupations"
7 = "Construction and extraction occupations"
8 = "Installation, maintenance, and repair occupations"
9 = "Production occupations"
10 = "Transportation and material moving occupations"
11 = "Armed Forces"
;
value PRMJOCGR /*PRMJOCGR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Management, professional, and related occupations"
2 = "Service occupations"
3 = "Sales and office occupations"
4 = "Farming, fishing, and forestry occupations"
5 = "Construction and maintenance occupations"
6 = "Production, transportation, and material moving occupations"
7 = "Armed Forces"
;
value PRNAGPWS /*PRNAGPWS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Non-agricultural private wage and salary"
;
value PRNAGWS /*PRNAGWS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Non-agricultural wage and salary workers"
;
value PRNLFSCH /*PRNLFSCH*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "In school"
2 = "Not in school"
;
value PRNMCHLD /*PRNMCHLD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
13 = "13 Own Child Under 18 Years Of Age"
14 = "14 Own Child Under 18 Years Of Age"
15 = "15 Own Child Under 18 Years Of Age"
16 = "16 Own Child Under 18 Years Of Age"
17 = "17 Own Child Under 18 Years Of Age"
18 = "18 Own Child Under 18 Years Of Age"
19 = "19 Own Child Under 18 Years Of Age"
;
value PRPERTYP /*PRPERTYP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Child household member"
2 = "Adult civilian household member (15+ years old)"
3 = "Adult Armed Forces household member"
;
value PRPTHRS /*PRPTHRS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Usually full time, part time for non-economic reasons"
1 = "Usually full time, part time for economic reasons, 1-4 hours"
2 = "Usually full time, part time for economic reasons, 5-14 hours"
3 = "Usually full time, part time for economic reasons, 15-29 hours"
4 = "Usually full time, part time for economic reasons, 30-34 hours"
5 = "Usually part time for economic reasons, 1-4 hours"
6 = "Usually part time for economic reasons, 5-14 hours"
7 = "Usually part time for economic reasons, 15-29 hours"
8 = "Usually part time for economic reasons, 30-34 hours"
9 = "Usually part time for non-economic reasons, 1-4 hours"
10 = "Usually part time for non-economic reasons, 5-14 hours"
11 = "Usually part time for non-economic reasons, 15-29 hours"
12 = "Usually part time for non-economic reasons, 30-34 hours"
;
value PRPTREA /*PRPTREA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Usually full time - slack work/business conditions"
2 = "Usually full time - seasonal work"
3 = "Usually full time - job started/ended during week"
4 = "Usually full time - vacation/personal day"
5 = "Usually full time - own illness/injury/medical appointment"
6 = "Usually full time - holiday (religious or legal)"
7 = "Usually full time - childcare problems"
8 = "Usually full time - other family/personal obligation"
9 = "Usually full time - labor dispute"
10 = "Usually full time - weather affected job"
11 = "Usually full time - school/training"
12 = "Usually full time - civic/military duty"
13 = "Usually full time - other reason"
14 = "Usually part time - slack work/business conditions"
15 = "Usually part time - could only find part time work"
16 = "Usually part time - seasonal work"
17 = "Usually part time - childcare problems"
18 = "Usually part time - other family/personal obligation"
19 = "Usually part time - health/medical limitations"
20 = "Usually part time - school/training"
21 = "Usually part time - retired/Social Security limit on earnings"
22 = "Usually part time - work week less than 35 hours"
23 = "Usually part time - other reason"
;
value PRSJMJ /*PRSJMJ*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Single jobholder"
2 = "Multiple jobholder"
;
value PRTFAGE /*PRTFAGE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Age not topcoded"
1 = "Age topcoded"
;
value PRUNTYPE /*PRUNTYPE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Job loser/on layoff"
2 = "Other job loser"
3 = "Temporary job ended"
4 = "Job leaver"
5 = "Re-entrant"
6 = "New entrant"
;
value PRWERNAL /*PRWERNAL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "PRERNWA does not contain allocated information"
1 = "PRERNWA contains allocated information"
;
value PRWKSCH /*PRWKSCH*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not in labor force"
1 = "At work"
2 = "With job, not at work"
3 = "Unemployed, seeks full time"
4 = "Unemployed, seeks part time"
;
value PRWKSTAT /*PRWKSTAT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Not in labor force"
2 = "Full time hours (35+), usually full time"
3 = "Part time for economic reasons, usually full time"
4 = "Part time for non-economic reasons, usually full time"
5 = "Not at work, usually full time"
6 = "Part time hours, usually part time for economic reasons"
7 = "Part time hours, usually part time for non-economic reasons"
8 = "Full time hours, usually part time for economic reasons"
9 = "Full time hours, usually part time for non-economic reasons"
10 = "Not at work, usually part time"
11 = "Unemployed full time"
12 = "Unemployed part time"
;
value PRWNTJOB /*PRWNTJOB*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Want a job"
2 = "Other not in labor force"
;
value PTDTRACE /*PTDTRACE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "White only"
2 = "Black only"
3 = "American Indian, Alaskan Native only"
4 = "Asian only"
5 = "Hawaiian/Pacific Islander only"
6 = "White-Black"
7 = "White-American Indian"
8 = "White-Asian"
9 = "White-Hawaiian"
10 = "Black-American Indian"
11 = "Black-Asian"
12 = "Black-Hawaiian"
13 = "American Indian-Asian"
14 = "American Indian-Hawaiian"
15 = "Asian-Hawaiian"
16 = "White-Black-American Indian"
17 = "White-Black-Asian"
18 = "White-Black-Hawaiian"
19 = "White-American Indian-Asian"
20 = "White-American Indian-Hawaiian"
21 = "White-Asian-Hawaiian"
22 = "Black-American Indian-Asian"
23 = "White-Black-American Indian-Asian"
24 = "White-American Indian-Asian-Hawaiian"
25 = "Other 3 race combinations"
26 = "Other 4 and 5 race combinations"
;
value PTHR /*PTHR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not topcoded"
1 = "Topcoded"
;
value PTOT /*PTOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not topcoded"
1 = "Topcoded"
;
value PTWK /*PTWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Not topcoded"
1 = "Topcoded"
;
value PUABSOT /*PUABSOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Retired"
4 = "Disabled"
5 = "Unable to work"
;
value PUBUSA /*altered: PUBUS1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUBUS2OT /*PUBUS2OT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUBUSCKA /*altered: PUBUSCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUBUS1"
2 = "Go to PURETCK1"
;
value PUBUSCKB /*altered: PUBUSCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRUSL1"
2 = "Go to PUBUS2"
;
value PUBUSCKC /*altered: PUBUSCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUABSRSN"
2 = "Go to PULAY"
;
value PUBUSCKD /*altered: PUBUSCK4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRUSL1"
2 = "Go to PUABSPD"
;
value PUCHINHH /*PUCHINHH*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Person added"
2 = "Person added - usually resides elsewhere"
3 = "Person undeleted"
4 = "Person died"
5 = "Deleted for reason other than death"
6 = "Person joined Armed Forces"
7 = "Person no longer in Armed Forces"
9 = "Change in demographic information"
;
value PUDIS /*PUDIS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Did not have disability last month"
;
value PUDISA /*altered: PUDIS1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUDISB /*altered: PUDIS2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUDWCKA /*altered: PUDWCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUSCHCK"
2 = "Go to PUNLFCK1"
3 = "Go to PUDWCK4"
4 = "Go to PUDWWANT"
;
value PUDWCKB /*altered: PUDWCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUJHCK1"
2 = "Go to PUDIS1"
3 = "Go to PUDIS2"
4 = "Go to PUDWCK4"
;
value PUDWCKC /*altered: PUDWCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUDWCK4"
2 = "Go to PUNLFCK2"
;
value PUDWCKD /*altered: PUDWCK4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUDWCK5"
2 = "Go to PUDWCK5"
3 = "Go to PUDWCK5"
4 = "Go to PUDWRSN"
;
value PUDWCKE /*altered: PUDWCK5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUDWWK"
2 = "Go to PUDWLK"
;
value PUHRCKA /*altered: PUHRCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRUSL2"
2 = "Go to PUHRUSLT"
;
value PUHRCK1B /*altered: PUHRCK12*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PULK"
2 = "Go to PUIOCK1"
;
value PUHRCKB /*altered: PUHRCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRFTPT"
2 = "Go to PUHRFTPT"
3 = "Go to PUHRFTPT"
4 = "Go to PUHRFTPT"
5 = "Go to PUHRCK3"
;
value PUHRCKC /*altered: PUHRCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRCK8"
2 = "Go to PUHRCK8"
3 = "Go to PUHRACT1"
4 = "Go to PUHRCK4"
5 = "Go to PUHROFF1"
6 = "Go to PUHRCK4"
;
value PUHRCKD /*altered: PUHRCK4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRACT1"
2 = "Go to PUHROFF1"
3 = "Go to PUHRWANT"
4 = "Go to PUHRWANT"
5 = "Go to PUHRACT1"
;
value PUHRCKE /*altered: PUHRCK5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUHRACT2"
2 = "Go to PUHRCK6"
;
value PUHRCKF /*altered: PUHRCK6*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PULK"
2 = "Go to PUABSRSN"
3 = "Go to PUHRACTT"
;
value PUHRCKG /*altered: PUHRCK7*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "If PUBUS2=2,D,R and PUHRACT1<15 go to PUHRCK8"
2 = "If PUBUS2=2,D,R and PUHRACT1>=15 go to PUHRCK8"
3 = "If PUHRUSLT>=35 and PUHRACT1 <> D,R and PUHRACT2 <> D,R go to PUHRRSN3"
4 = "If PUHRWANT=1 and PUHRACTT <35 and PUHRRSN1=1-3 go to PUHRAVL"
5 = "Go to PUHRCK8"
;
value PUHROFFA /*altered: PUHROFF1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUHROTA /*altered: PUHROT1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUIO1MFG /*PUIO1MFG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Manufacturing"
2 = "Retail trade"
3 = "Wholesale trade"
4 = "Something else"
;
value PUIO2MFG /*PUIO2MFG*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Manufacturing"
2 = "Retail trade"
3 = "Wholesale trade"
4 = "Something else"
;
value PUIOCKA /*altered: PUIOCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUIO1INT"
2 = "Go to PUIO1INT"
3 = "Go to PUIO1INT"
4 = "Go to PUIODP1"
5 = "Go to PUIO1INT"
;
value PUIOCKB /*altered: PUIOCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUIO1IND"
2 = "Go to PUIO1OCC"
3 = "Go to PUIODP2"
;
value PUIOCKC /*altered: PUIOCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUIO1OCC"
2 = "Go to PUIO1OCC"
3 = "Go to PUIODP3"
;
value PUIODPA /*altered: PUIODP1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUIODPB /*altered: PUIODP2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUIODPC /*altered: PUIODP3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PUJHCKA /*altered: PUJHCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUNLFCK2"
2 = "Go to PUJHCK2"
3 = "Go to PUNLFCK1"
;
value PUJHCKB /*altered: PUJHCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUJHRSN"
2 = "Go to PUJHWANT"
3 = "Go to PUJHWK"
;
value PUJHCKC /*altered: PUJHCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUJHDP1"
2 = "Go to PUJHRSN"
;
value PUJHCKD /*altered: PUJHCK4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUJHCK5"
2 = "Go to PUIO1INT"
3 = "Go to PUJHCK5"
4 = "Go to PUIO1INT"
5 = "Go to PUNLFCK1"
;
value PUJHCKE /*altered: PUJHCK5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUIO1INT"
2 = "Go to PUIOCK5"
;
value PUJHDP1O /*PUJHDP1O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PULAY /*PULAY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Retired"
4 = "Disabled"
5 = "Unable to work"
;
value PULAY6M /*PULAY6M*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PULAYAVR /*PULAYAVR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Own temporary illness"
2 = "Going to school"
3 = "Other"
;
value PULAYCKA /*altered: PULAYCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PULAYCK3"
2 = "Go to PULAYFT"
3 = "Go to PULAYDR"
;
value PULAYCKB /*altered: PULAYCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PULAYDR3"
2 = "Go to PULAYFT"
;
value PULAYCKC /*altered: PULAYCK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUIO1INT"
2 = "Go to PUIO1INT"
3 = "Go to PUSCHCK"
;
value PULAYDT /*PULAYDT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PULK /*PULK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Retired"
4 = "Disabled"
5 = "Unable to work"
;
value PULKAVR /*PULKAVR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Waiting for new job to begin"
2 = "Own temporary illness"
3 = "Going to school"
4 = "Other"
;
value PULKDKA /*altered: PULKDK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
12 = "Nothing"
13 = "Other passive"
;
value PULKDKB /*altered: PULKDK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
13 = "Other passive"
;
value PULKDKC /*altered: PULKDK3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
13 = "Other passive"
;
value PULKDKD /*altered: PULKDK4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKDKE /*altered: PULKDK5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKDKF /*altered: PULKDK6*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKMB /*altered: PULKM2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
13 = "Other passive"
;
value PULKMC /*altered: PULKM3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKMD /*altered: PULKM4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKME /*altered: PULKM5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKMF /*altered: PULKM6*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKPSA /*altered: PULKPS1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
12 = "Nothing"
13 = "Other passive"
;
value PULKPSB /*altered: PULKPS2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted employer directly/interview"
2 = "Contacted public employment agency"
3 = "Contacted private employment agency"
4 = "Contacted friends or relatives"
5 = "Contacted school/university employment center"
6 = "Sent out resumes/filled out applications"
7 = "Checked union/professional registers"
8 = "Placed or answered ads"
9 = "Other active"
10 = "Looked at ads"
11 = "Attended job training programs/courses"
13 = "Other passive"
;
value PULKPSC /*altered: PULKPS3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKPSD /*altered: PULKPS4*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKPSE /*altered: PULKPS5*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PULKPSF /*altered: PULKPS6*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Contacted Employer Directly/Interview"
2 = "Contacted Public Employment Agency"
3 = "Contacted Private Employment Agency"
4 = "Contacted Friends Or Relatives"
5 = "Contacted School/University Empl Center"
6 = "Sent Out Resumes/Filled Out Application"
7 = "Checked Union/Professional Registers"
8 = "Placed Or Answered Ads"
9 = "Other Active"
10 = "Looked At Ads"
11 = "Attended Job Training Programs/Courses"
13 = "Other Passive"
;
value PUNLFCKA /*altered: PUNLFCK1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUNLFACT"
2 = "Go to PUNLFRET"
;
value PUNLFCKB /*altered: PUNLFCK2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Go to PUNLFJH"
2 = "End questions"
;
value PURETOT /*PURETOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Was not retired last month"
;
value PUSLFPRX /*PUSLFPRX*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Self"
2 = "Proxy"
3 = "Both self and proxy"
;
value PUWK /*PUWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
3 = "Retired"
4 = "Disabled"
5 = "Unable to work"
;
value PXABSPDO /*PXABSPDO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXABSRSN /*PXABSRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXAFNOW /*PXAFNOW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXAGE /*PXAGE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXCYC /*PXCYC*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDIPGED /*PXDIPGED*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISDRS /*PXDISDRS*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISEAR /*PXDISEAR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISEYE /*PXDISEYE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISOUT /*PXDISOUT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISPHY /*PXDISPHY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDISREM /*PXDISREM*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDW4WK /*PXDW4WK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWAVL /*PXDWAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWAVR /*PXDWAVR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWLKO /*PXDWLKO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWLKWK /*PXDWLKWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWRSN /*PXDWRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWWK /*PXDWWK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXDWWNTO /*PXDWWNTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXEDUCA /*PXEDUCA*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERN /*PXERN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNCOV /*PXERNCOV*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNH1O /*PXERNH1O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNHB /*altered: PXERNH2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNHRO /*PXERNHRO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNHRY /*PXERNHRY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNLAB /*PXERNLAB*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNPER /*PXERNPER*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNRT /*PXERNRT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNUOT /*PXERNUOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXERNWKP /*PXERNWKP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXFNTVTY /*PXFNTVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXGR6COR /*PXGR6COR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXGRPROF /*PXGRPROF*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHGCOMP /*PXHGCOMP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRACTA /*altered: PXHRACT1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRACTB /*altered: PXHRACT2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRACTT /*PXHRACTT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRAVL /*PXHRAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRFTPT /*PXHRFTPT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRRSNA /*altered: PXHRRSN1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRRSNB /*altered: PXHRRSN2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRRSNC /*altered: PXHRRSN3*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRUSLA /*altered: PXHRUSL1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRUSLB /*altered: PXHRUSL2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRUSLT /*PXHRUSLT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHRWANT /*PXHRWANT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXHSPNON /*PXHSPNON*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long"
31 = "Blank To Allocated Value Long"
32 = "Don`t Know To Allocated Value Long"
33 = "Refused To Allocated Value Long"
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXINUSYR /*PXINUSYR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO1COW /*PXIO1COW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO1ICD /*PXIO1ICD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO1OCD /*PXIO1OCD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO2COW /*PXIO2COW*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO2ICD /*PXIO2ICD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXIO2OCD /*PXIO2OCD*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXJHRSN /*PXJHRSN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXJHWANT /*PXJHWANT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXJHWKO /*PXJHWKO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLAYAVL /*PXLAYAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLAYDUR /*PXLAYDUR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLAYFTO /*PXLAYFTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLAYLK /*PXLAYLK*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKAVL /*PXLKAVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKDUR /*PXLKDUR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKFTO /*PXLKFTO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKLL1O /*PXLKLL1O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKLL2O /*PXLKLL2O*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKLWO /*PXLKLWO*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXLKMA /*altered: PXLKM1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMARITL /*PXMARITL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMJNUM /*PXMJNUM*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMJOT /*PXMJOT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMLR /*PXMLR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMNTVTY /*PXMNTVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXMS12C /*altered: PXMS123*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNATVTY /*PXNATVTY*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNLFACT /*PXNLFACT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNLFJH /*PXNLFJH*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNLFRET /*PXNLFRET*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXPARENT /*PXPARENT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXRACEA /*altered: PXRACE1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Dont Know  No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long"
31 = "Blank To Allocated Value Long"
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long"
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Dont Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXRETA /*altered: PXRET1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXRRP /*PXRRP*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXSCHENR /*PXSCHENR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXSCHFT /*PXSCHFT*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXSCHLVL /*PXSCHLVL*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXSEX /*PXSEX*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXSPOUSE /*PXSPOUSE*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value TRATUSR /*TRATUSR*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Did not respond to ATUS"
1 = "Responded to ATUS"
;
value PRDASIAN /*PRDASIAN*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Asian Indian"
2 = "Chinese"
3 = "Filipino"
4 = "Japanese"
5 = "Korean"
6 = "Vietnamese"
7 = "Other"
;
value PEPDEMPA /*PEPDEMP1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PEPDEMPB /*PEPDEMP2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
1 = "Yes"
2 = "No"
;
value PXPDEMPA /*PXPDEMP1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNMEMPA /*PXNMEMP1*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXPDEMPB /*PXPDEMP2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
value PXNMEMPB /*PXNMEMP2*/
-1 = "Blank"
-2 = "Don't Know"
-3 = "Refused"
0 = "Value - No Change"
1 = "Blank - No Change"
2 = "Don`t Know - No Change"
3 = "Refused - No Change"
10 = "Value To Value"
11 = "Blank To Value"
12 = "Don`t Know To Value"
13 = "Refused To Value"
20 = "Value To Longitudinal Value"
21 = "Blank To Longitudinal Value"
22 = "Don`t Know To Longitudinal Value"
23 = "Refused To Longitudinal Value"
30 = "Value To Allocated Value Long."
31 = "Blank To Allocated Value Long."
32 = "Don`t Know To Allocated Value Long."
33 = "Refused To Allocated Value Long."
40 = "Value To Allocated Value"
41 = "Blank To Allocated Value"
42 = "Don`t Know To Allocated Value"
43 = "Refused To Allocated Value"
50 = "Value To Blank"
52 = "Don`t Know To Blank"
53 = "Refused To Blank"
;
proc contents data=atuscps_2014; run;
