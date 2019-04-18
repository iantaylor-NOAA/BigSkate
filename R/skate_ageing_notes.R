ageing.dir <- 'c:/SS/skates/bio/ageing/'
double1 <- read.csv(file.path(ageing.dir,
                              'AgeData_Exchange_BSKT_20190327_CARE.csv'),
                    skip=3)
double1 <- double1[,1:7]

double2 <- read.csv(file.path(ageing.dir,
                              'AgeData_Exchange_BSKT_20190327_NWFSC-in-house.csv'),
                    skip=1)
double2 <- double2[,1:6]
