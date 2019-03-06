library(haven)

ANES_2018.df <- read_spss('./ANES_2018.sav')

Interest_18.df <- ANES_2018.df[,c('house18p','house18t',
                                  'online',
                                  'ftblack','ftwhite','fthisp','ftasian',
                                  'ftgay','fttrans',
                                  'ftobama',
                                  'ftmetoo',
                                  'ftcapitalists','ftsocialists',
                                  'ftimmig',
                                  'ftpolice',
                                  'ftjournal',
                                  'ftmuslim',
                                  'ftaltright','ftantifa',
                                  'russia16',
                                  'lcself','lcd','lcr',
                                  'amount_dir','amount_ind',
                                  'whiteid','whitework','whitejob',
                                  'work',
                                  'guncheck','gunsar',
                                  'gunteach',
                                  'pk_sen','pk_cjus','pk_cjus_correct',
                                  'pid7x','pid1d','pid1r','pidstr','pidlean',
                                  'marital',
                                  'sp_w','sp_b',
                                  'sp_gender',
                                  'honest',
                                  'birthyr','gender','educ','marstat','employ',
                                  'faminc_new','region','votereg',
                                  'inputstate',
                                  'race','race_other',
                                  'pdl_inputzip',
                                  'pdl_region',
                                  'pdl_cassregcountyname',
                                  'rand_pid',
                                  'pew_religimp','pew_churatd','pew_bornagain',
                                  'pew_prayer','religpew'
                                  )
                               ]

#--Boxplot of Ideology by Religion
boxplot(unclass(ANES_2018.df$lcself)~unclass(ANES_2018.df$religpew),
        xlab = '',
        ylab = 'Ideology: 1 Very Liberal, 7 Very Conservative',
        main = 'ANES 2018 Religion and Political Ideology',
        xaxt = "n",
        sub = "Religion")
axis(side = 1,at = c(1:10),
     labels = c('Protestant','Catholic','Mormon',
                'Orthodox','Jewish','Muslim',
                'Buddhist','Agnostic','Nothing','Other'),
     las= 2)

#--Boxplot of Ideology by Religion for blacks
ANES_2018_Black.df <- ANES_2018.df[ANES_2018.df$race == 2,]
boxplot(unclass(ANES_2018_Black.df$lcself)~unclass(ANES_2018_Black.df$religpew),
        xlab = '',
        ylab = 'Ideology: 1 Very Liberal, 7 Very Conservative',
        main = 'ANES 2018 Religion and Political Ideology, Black',
        xaxt = "n",
        sub = "Religion")
axis(side = 1,at = c(1:10),
     labels = c('Protestant','Catholic','Mormon',
                'Orthodox','Jewish','Muslim',
                'Buddhist','Agnostic','Nothing','Other'),
     las= 2)

#--Boxplot of Ideology by Religion for hispanics
ANES_2018_Hisp.df <- ANES_2018.df[ANES_2018.df$race == 3,]
boxplot(unclass(ANES_2018_Hisp.df$lcself)~unclass(ANES_2018_Hisp.df$religpew),
        xlab = '',
        ylab = 'Ideology: 1 Very Liberal, 7 Very Conservative',
        main = 'ANES 2018 Religion and Political Ideology, Hispanic',
        xaxt = "n",
        sub = "Religion")
axis(side = 1,at = c(1:10),
     labels = c('Protestant','Catholic','Mormon',
                'Orthodox','Jewish','Muslim',
                'Buddhist','Agnostic','Nothing','Other'),
     las= 2)

#--Boxplot of WhiteIdentity by Religion
boxplot(unclass(ANES_2018.df$whiteid)~unclass(ANES_2018.df$religpew)#,
        xlab = '',
        ylab = 'White Identity: 1 Not Important, 7 Very Important',
        main = 'ANES 2018 Religion and White Identity',
        xaxt = "n",
        sub = "Religion"
        )
axis(side = 1,at = c(1:10),
     labels = c('Protestant','Catholic','Mormon',
                'Orthodox','Jewish','Muslim',
                'Buddhist','Agnostic','Nothing','Other'),
     las= 2)