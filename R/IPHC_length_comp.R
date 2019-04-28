iphc.lens <- read.csv('C:/SS/skates/bio/IPHC_comps/Big-Sk_IPHC-unaged.csv',
                      skip = 4, fill = TRUE, stringsAsFactors = FALSE)
iphc.lens$SEX[iphc.lens$SEX == "B"] <- "M"
iphc.lens$SEX[iphc.lens$SEX == "G"] <- "F"

# boxplot shows smaller average size on U.S. West coast compared to other areas
boxplot(iphc.lens$TOTAL.LENGTH ~ iphc.lens$AREA)

# binning for U.S. west coast
# breaks need to go to 205 since hist doesn't treat last bin as plus group
hist.f <- hist(iphc.lens$TOTAL.LENGTH[iphc.lens$SEX == "F" &
                                        iphc.lens$AREA == "WC"],
               breaks = seq(5,205,5))
hist.m <- hist(iphc.lens$TOTAL.LENGTH[iphc.lens$SEX == "M" &
                                        iphc.lens$AREA == "WC"],
               breaks = seq(5,205,5))

SSinput <- data.frame(year = 2014, month = 7, fleet = 7, sex = 3, part = 2,
                      Nsamps = sum(hist.f$counts) + sum(hist.m$counts),
                      t(c(hist.f$counts, hist.m$counts)))
names(SSinput)[-(1:6)] <- c(paste0("F",seq(5,200,5)),paste0("M",seq(5,200,5)))
options(width=400)
SSinput
##   year month fleet sex part Nsamps X1 X2 X3 X4 X5 X6 X7 X8 X9 X10 X11 X12 X13 X14 X15 X16 X17 X18 X19 X20 X21 X22 X23 X24 X25 X26 X27 X28 X29 X30 X31 X32 X33 X34 X35 X36 X37 X38 X39
## 1 2014     7     7   1    2     16  0  0  0  0  0  0  0  0  0   0   0   0   2   0   0   0   2   2   0   0   2   0   2   2   0   0   0   0   0   0   0   0   0   2   0   0   2   0   0
## 2 2014     7     7   2    2     30  0  0  0  0  0  0  0  0  0   0   0   0   0   2   2   0   4   0   5   0   5   1   4   1   4   0   0   2   0   0   0   0   0   0   0   0   0   0   0
options(width=80)
