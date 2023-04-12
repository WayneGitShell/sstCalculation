options(sstCalculation.debugMode = T)


testCorrectEnvironment <- function(){
  expect_identical(names(packageEnv), c("pb", "excelStructure", "errorLog", "param"))
  expect_null(packageEnv$pb)
  expect_true(is.data.table(packageEnv$excelStructure))
  expect_identical(nrow(packageEnv$errorLog), 0L)
}


test_that("Package initialization", {

  # Correct initialization is done after loading package
  testCorrectEnvironment()
})

################
# Health
################
# Correlation
labelHealth <- c("Individual health", "Collective compensation")
correlationHealth <- matrix(c(1, 0.25, 0.25, 1), 2, 2, dimnames = list(labelHealth, labelHealth))
# Scenario
scenarioHealth <- fread(
  "label,effect,probability,rowNumber
  Scenario AS,0,0.005,7
  Scenario KTG,0,0.005,8
  Scenario AS,100,0,9
  Scenario KTG,5.5,0.01,10
  Scenario KTG1,5.5,0.99,11
  Scenario KTG2,5.5,0.5,12
  none,0,0.99,13"
)

# Sensitivity
sensitivityHealth <- setNames(c(0, 0, 10, 5.5, 2), nm = c(labelHealth, labelHealth, "abc"))


################
# Life
################

# Correlation
correlationLife <- fread(
  "Mortality,Longevity,Disability,Reactivation,Costs,Lapses,Capital option,Cost BVG,Lapses BVG
  1,-0.75,0.25,0,0,0,0,0,0
  -0.75,1,0,0,0,0,0.25,0,0
  0.25,0,1,-0.75,0.25,0,0,0.25,0
  0,0,-0.75,1,0,0,0,0,0
  0,0,0.25,0,1,0.5,0,0.5,0.5
  0,0,0,0,0.5,1,0,0.5,0.5
  0,0.25,0,0,0,0,1,0,-0.5
  0,0,0.25,0,0.5,0.5,0,1,0.5
  0,0,0,0,0.5,0.5,-0.5,0.5,1"
)
correlationLife <- as.matrix(correlationLife)
rownames(correlationLife) <- colnames(correlationLife)

# Exposure
exposureLife <- fread(
  "Mortality,Longevity,Disability,Reactivation,Costs,Lapses,Capital option,Cost BVG,Lapses BVG
  1,2.5,0,0,0,0,0,0,0"
)
exposureLife <- setNames(unlist(exposureLife), nm = names(exposureLife))



################
# Captive
################

captiveCY_stochastic <- fread(
"branch,branchType,maximumPossibleLoss,expectedLoss,normalLossNumber,normalLossSize,normalLossSigma,largeLossNumber,largeLossThreshold,largeLossShape,AAL,EEL,AAD,EED,QS,rowNumber
GenLiab,Ground-up loss,,,2,1,0.2,0.5,0.1,1.2,1,0.5,0,0,0.5,8
Worker,Ground-up loss,,,2,1,0.2,0.5,0.1,1.2,NA,NA,0,0,NA,9
Test_NormalOnly,Ground-up loss,,,2,5,0.2,,,,NA,NA,0,0,NA,10
Test_LargeOnly,Ground-up loss,,,,,,1,1,2,NA,NA,0,0,NA,11
Test_IncorrectNormal,Ground-up loss,,,2,,0.2,0.5,0.1,1.2,NA,NA,0,0,NA,12
Test_IncorrectLarge,Ground-up loss,,,2,2,0.2,0.5,0.1,NA,NA,NA,0,0,NA,13
Test_IncorrectNormalLarge,Ground-up loss,,,,,,,,,NA,NA,0,0,NA,14
")
captiveCY_stochastic[, AAL := as.numeric(AAL)]

captiveCY_deterministic <- fread(
  "branch,branchType,maximumPossibleLoss,expectedLoss,normalLossNumber,normalLossSize,normalLossSigma,largeLossNumber,largeLossThreshold,largeLossShape,AAL,EEL,AAD,EED,QS,rowNumber
Property,Maximum possible loss,10,5,,,,,,,,,,,,7
Worker,Maximum possible loss,100,10,,,,,,,,,,,,8
Test_Equality,Maximum possible loss,100,100,,,,,,,,,,,,8
Test_StrictlyLarger,Maximum possible loss,100,200,,,,,,,,,,,,8
")
captiveCY_deterministic[, maximumPossibleLoss := as.numeric(maximumPossibleLoss)]
captiveCY_deterministic[, expectedLoss := as.numeric(expectedLoss)]

captivePY <- fread(
"branch,reserve,cov,rowNumber
Motor,5,0.2,7
Worker,10,0.1,8"
)

nonlifeSimulation <- data.table(simulation = c(4, 2, 3, 1))
nonlifeCDF <- data.table(x = -2:2, cdf = c(1:5)/5)

# Results from Captive processing
captiveCY_stochastic_Result <- copy(captiveCY_stochastic)[1:4]
captiveCY_stochastic_Result[, normalLossScale := c(0.04, 0.04, 0.008, NA)]
captiveCY_stochastic_Result[, normalLossShape := c(25, 25, 625, NA)]
captiveCY_stochastic_Result[, AAL := c(1, Inf, Inf, Inf)]
captiveCY_stochastic_Result[, EEL := c(0.5, Inf, Inf, Inf)]
captiveCY_stochastic_Result[, QS := c(0.5, 1, 1, 1)]

captivePY_Result <- captivePY[, .(branch, rowNumber, sd = sqrt(log(cov^2+1)),  mu = log(reserve) - 0.5*log(cov^2+1))]

# NL Scenario
scenarioNL <- data.table(label = c("A", "B"), effect = c(1, 3), probability = c(0.1, 0.2))

################
# Credit
################

creditRiskExposure <- data.table(weightedExposure = c(1, 50, 100, 200))
creditRiskExposureReinsurance <- data.table(weightedExposure = c(1))
creditRiskExposureHypo <- data.table(weightedExposure = c(50, 100))

spreadCurve <- fread(
  "category,curve,rowNumber
  1,0,7
  2,0.0015,8
  3,0.004,9
  4,0.009,10
  5,0.025,11
  6,0.035,12
  7,0.045,13
  8,0.045,14")

LGDMap <- fread(
  "class,LGD,rowNumber
  A7.1,0.5,9
  A8.1,2,10
  A8.2,10,11"
)

migrationMatrix <- fread(
  "category,1,2,3,4,5,6,7,8,9,rowNumber
  1,0.907855619798419,0.0812666502864791,0.00825731880724353,0.00199638973694116,0.000303116766341851,2.09046045753001e-05,0,0,3e-04,7
  2,0.0111624341894916,0.895923809016306,0.0822803474368539,0.00769015027176809,0.00168288607055313,0.000489954172439518,0.000127814131940744,4.26047106469146e-05,6e-04,8
  3,0.000721345351466701,0.0286204376214288,0.90234999458547,0.0590018065420263,0.00678913271968659,0.0012305303054432,0.000381888715482371,8.48641589960824e-05,0.00082,9
  4,0.000376260655896712,0.00249407063337249,0.0451405284031512,0.891243240470314,0.0489031349621184,0.00779397072928903,0.00137603897013655,0.00018275517572126,0.00249,10
  5,6.71226489826404e-05,0.000794284679627911,0.00544812167575764,0.0688230894235339,0.82840535952742,0.0765981295973564,0.00745061403707308,0.000973278410248285,0.01144,11
  6,5.70744536566381e-05,0.000490840301447088,0.00179213784481844,0.00699732801830383,0.0635124520291069,0.819075484426413,0.0706810034083806,0.00527367951787336,0.03212,12
  7,0,0.000129600317797968,0.000294546176813563,0.00134313056626985,0.00603230570114177,0.0790915393979779,0.797831338211361,0.0346975396286377,0.08058,13
  8,0,0.00019655947092084,0.0013103964728056,0.00045863876548196,0.00749546782444804,0.0369269726036618,0.107360783016963,0.624941181845719,0.22131,14"
  , header = TRUE
)

migrationMatrix_adjusted <- copy(migrationMatrix)
for(j in 2:10){
  set(migrationMatrix_adjusted, j = j, value = migrationMatrix_adjusted[[j]]*0.5 + 1/18)
}

initialInterestRate <- fread(
  "currency,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Y20,Y21,Y22,Y23,Y24,Y25,Y26,Y27,Y28,Y29,Y30,Y31,Y32,Y33,Y34,Y35,Y36,Y37,Y38,Y39,Y40,Y41,Y42,Y43,Y44,Y45,Y46,Y47,Y48,Y49,Y50,rowNumber
  CHF,-0.00616200849473833,-0.00713375881251108,-0.00743043689358056,-0.00724374212795548,-0.00683894404072751,-0.00635587557941435,-0.00586331733948939,-0.00539411469427959,-0.004962540108193,-0.00457314394642934,-0.0042958830724121,-0.00407271878469659,-0.0038386971974774,-0.00355185274404032,-0.00318452104203905,-0.00272656405914234,-0.00220292981619316,-0.00163860393035582,-0.0010513641265324,-0.000453909077656633,0.000144686550389736,0.000737961226557272,0.00132135408472662,0.00189169634538412,0.00244684418150446,0.00298541105626218,0.00350657088210124,0.00400991216722529,0.00449532924171439,0.00496294069585272,0.00541302795415275,0.00584598885972423,0.00626230252296666,0.00666250267372878,0.00704715746646125,0.00741685420468436,0.00777218783016681,0.00811375230240405,0.00844213420257232,0.00875790805245344,0.00906163295670149,0.00935385026622395,0.00963508202862021,0.0099058300438624,0.0101665753836228,0.0104177782637491,0.0106598781835276,0.0108932942641753,0.0111184257337,0.0113356525167838,6
  EUR,-0.00624948743001229,-0.00593759266681362,-0.00540457845892247,-0.00477136487894935,-0.00412851058383447,-0.00348606928488292,-0.00284404045178302,-0.00217235786170217,-0.00153117164527262,-0.000870378669683648,-0.000255794754923249,0.000306972150095402,0.00083002565338272,0.00132181557731751,0.00178839985918824,0.00222687192826151,0.00260856533139934,0.00290774883469731,0.00310698481983133,0.00319489089649837,0.0031753434060245,0.00309201625082909,0.00298637880730526,0.00288860389571609,0.00282033949219693,0.00279691639755915,0.00282908643510478,0.00292438015354009,0.00308816414981711,0.00332446782800848,0.00363277366978811,0.0039984352549866,0.00440662893814739,0.00484571653275457,0.00530652727781542,0.00578181668807166,0.00626585446930904,0.00675410778323257,0.00724299574840304,0.00772969770684894,0.00821200244921838,0.00868818890838149,0.00915693121959974,0.00961722278406841,0.0100683152515041,0.0105096692870661,0.0109409146992667,0.0113618180428072,0.0117722562191773,0.0121721949113008,7
  USD,0.01453386977701,0.0138831811085637,0.0137648285756816,0.0139423521226751,0.0142085783672537,0.0145437254408122,0.0148984646619394,0.0152530780877751,0.015578029963294,0.0159324025306923,0.0162513408829279,0.0165246743752891,0.0167606128177112,0.0169650308242132,0.0171422288272301,0.0172961717681188,0.0174326494494009,0.0175566188344064,0.0176717926512696,0.0177809772950731,0.0178849949651832,0.0179795126780696,0.0180599678666457,0.0181227447689875,0.0181649392878621,0.0181841866337,0.0181785347281366,0.0181463514388849,0.0180862571702494,0.0179970767016458,0.0178798871951985,0.0177434052832558,0.0175964601853377,0.0174460664372554,0.0172977531003107,0.0171558421110754,0.0170236821706558,0.016903844224964,0.0167982841279195,0.0167084775518378,0.016635531655151,0.0165802774706036,0.0165433464613657,0.0165252342238349,0.0165263539032337,0.0165470815356818,0.0165877952394759,0.0166489099486928,0.0167309092137061,0.0168343754866446,8
  GBP,0.00523626679523031,0.00500744181051549,0.00522631897155758,0.005544600255328,0.0058428969832366,0.00612122700491538,0.00641935180216393,0.00667765475322251,0.00694582183285259,0.00721391701817921,0.00744862316758498,0.00764490944249716,0.00780434298174288,0.00792825570305668,0.00801777159357203,0.00807688909995361,0.00811968298113554,0.00815851042275921,0.00820193278818474,0.00825582668461463,0.00832127444577348,0.00838799789807007,0.00844558495233269,0.00848603962418872,0.00850319710383909,0.00849229941753958,0.00844968585629141,0.00837256632323997,0.00825885510231451,0.00810704889389248,0.00791955105280388,0.00771115764529098,0.00749622650671261,0.00728561278468203,0.00708737332242816,0.00690735861352195,0.0067497024090743,0.00661722144467272,0.00651173837111494,0.00643434047127887,0.006385585622677,0.00636566556868098,0.00637453511481611,0.00641201450202764,0.00647787099568083,0.00657188469650316,0.00669390273355525,0.00684388533368569,0.00702194676687774,0.00722839383393976,9
  JPY,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10"
)

portfolio <- fread(
  "positionId,positionName,includeInCreditRisk,counterpartyId,counterpartyName,rating,basel,migration,currency,scalingCF,scalingLGD,marketValue,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Y20,Y21,Y22,Y23,Y24,Y25,Y26,Y27,Y28,Y29,Y30,Y31,Y32,Y33,Y34,Y35,Y36,Y37,Y38,Y39,Y40,Y41,Y42,Y43,Y44,Y45,Y46,Y47,Y48,Y49,Y50,rowNumber
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABC,Pfefferminzia Schweiz AG,5,A7.1,TRUE,CHF,NA,0.6,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,7
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A8.1,TRUE,CHF,0.5,0.6,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,8
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,0.5,NA,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,9
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,NA,NA,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,10
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,NA,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,11
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,NA,1,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,12
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,1,100000000,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,13
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,0,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,CHF,1,1,5,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,CHF,1,1,15,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,JPY,1,1,51,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,FALSE,EUR,1,1,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14

    ")
for(i in paste0("Y", 1:50)){
  portfolio[, (i) := as.numeric(portfolio[[i]])]
}

portfolioFX <- fread(
  "positionId,positionName,includeInCreditRisk,counterpartyId,counterpartyName,rating,basel,migration,currency,scalingCF,scalingLGD,marketValue,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18,Y19,Y20,Y21,Y22,Y23,Y24,Y25,Y26,Y27,Y28,Y29,Y30,Y31,Y32,Y33,Y34,Y35,Y36,Y37,Y38,Y39,Y40,Y41,Y42,Y43,Y44,Y45,Y46,Y47,Y48,Y49,Y50,rowNumber
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABC,Pfefferminzia Schweiz AG,5,A7.1,TRUE,CHF,1,0.6,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,7
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A8.1,TRUE,CHF,0.5,0.6,4.52474604,0.100625,3.600625,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,8
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,0.5,1,4.52474604,0.100625,3.600625,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,9
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,1,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,10
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,1,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,11
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,1,9.04949208,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,12
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia Schweiz AG,4,A7.1,TRUE,CHF,1,1,100000000,0.20125,7.20125,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,13
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,18.09898416,0.4025,14.4025,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,20,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,0,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,EUR,1,1,0,20,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,CHF,1,1,5,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,CHF,1,1,15,10,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,TRUE,JPY,1,1,102,4,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,14
  CH9999999999,Pfefferminzia 2.875%  - 04/02/2031,TRUE,ABCD,Pfefferminzia TEST AG,4,A7.1,FALSE,EUR,1,1,20,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,14

"
)
for(i in paste0("Y", 1:50)){
  portfolioFX[, (i) := as.numeric(portfolioFX[[i]])]
}

initialFX <- fread(
  "from,to,fx,rowNumber
  CHF,EUR,0.5,8
  EUR,CHF,2,15
  CHF,JPY,0.5,16
  JPY,CHF,2,16"
)

expectedFinancialResultTable <- data.table(type = LETTERS[1:4], return = c(0.01, 0, -0.01, 0.02), exposure = c(100, 200, 300, 400))


