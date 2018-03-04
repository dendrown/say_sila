say_sila
=====

Analysis of Sentiment in Tweets on Climate Change.  This project is the core of the research portion
of my Ph.D. studies in _Informatique Cognitive_ at the _Université du Québec à Montréal_.

Build
-----

We're not at the point where we're handling releases.  For the time being, build and run the main Erlang
node from the repo:

    $ rebar3 compile
    $ ERL_LIBS=_build/default/lib  erl -smp -sname sila -config ./say_sila.config

    (sila@devbox)1> sila:start().

Or for a node that will be collecting the tweet stream, we can shortcut the Twitter authentication
process:

    (sila@devbox)1> sila:start(twitter).


TODO
-----
- Don't reprocess (psql/emote) tweets for days we've already processed
- Compare tracking reports using different lexicons (allow combinatiions)
- Automate track reporting for daily updates
- Keep track report history
- Make ecsv behave when it encounters bad CSV data
- ~~Allow **raven** to handle both hashtags at the same time (or distribute across procs/nodes)~~
- ~~Reformat WUI as GRAPH:HITS, scrolling vertically for the four emotions~~
- ~~In/exclude RTs in runs~~
- ~~Check language tag in tweet metadata~~

# GW Q4 2017: pct[15%] cnts[{tt,10149}, {rt,5345}, {tm,3154}]

### `#globalwarming` communications (TT : tweet): pct[15.08%] cnt[10200]

| SCREEN NAME                                              |   TT |  RT |  TM  |  ANGR |  FEAR |  SAD  |  JOY  |
| -------------------------------------------------------- | ----:|----:| ----:| -----:| -----:| -----:| -----:|
| [EHFoundation237](https://twitter.com/EHFoundation237)   | 1421 |     |   17 |  0.11 |  0.15 |  0.10 |  0.11 |
| [ErroldMoody](https://twitter.com/ErroldMoody)           | 1384 |  38 |    2 |  0.11 |  0.17 |  0.09 |  0.10 |
| [CreativeCivil](https://twitter.com/CreativeCivil)       |  665 |  29 |    1 |  0.13 |  0.16 |  0.11 |  0.12 |
| [gridpointwx](https://twitter.com/gridpointwx)           |  540 |  24 |      |  0.12 |  0.28 |  0.09 |  0.10 |
| [denybot6000](https://twitter.com/denybot6000)           |  520 |  40 |    2 |  0.07 |  0.21 |  0.08 |  0.21 |
| [JoyfullyECO](https://twitter.com/JoyfullyECO)           |  502 |1370 |    2 |  0.07 |  0.18 |  0.07 |  0.14 |
| [TimMelino](https://twitter.com/TimMelino)               |  489 | 219 |      |  0.12 |  0.28 |  0.09 |  0.10 |
| [TrumpvsScience](https://twitter.com/TrumpvsScience)     |  253 |     |      |  0.10 |  0.17 |  0.07 |  0.08 |
| [SandorGerendasK](https://twitter.com/SandorGerendasK)   |  248 | 107 |      |       |  0.03 |  0.01 |  0.03 |
| [arnabch01](https://twitter.com/arnabch01)               |  243 | 345 |      |  0.37 |  0.69 |  0.29 |  0.07 |
| [MassCreativity](https://twitter.com/MassCreativity)     |  243 | 153 |      |       |  0.11 |  0.00 |       |
| [ILuvCO2](https://twitter.com/ILuvCO2)                   |  226 |  85 |    1 |  0.09 |  0.10 |  0.12 |  0.05 |
| [BreathingDelhi](https://twitter.com/BreathingDelhi)     |  210 |  43 |      |  0.09 |  0.14 |  0.08 |  0.13 |
| [artyny59](https://twitter.com/artyny59)                 |  188 | 263 |      |       |  0.23 |  0.08 |  0.01 |
| [Nicolasgwet](https://twitter.com/Nicolasgwet)           |  183 |  16 |      |  0.11 |  0.20 |  0.14 |  0.12 |
| [GreenFraud](https://twitter.com/GreenFraud)             |  162 | 674 |      |  0.00 |  0.01 |  0.01 |       |
| [ClimateTreaty](https://twitter.com/ClimateTreaty)       |  156 |  91 |    7 |  0.12 |  0.22 |  0.10 |  0.11 |
| [julianluna7009](https://twitter.com/julianluna7009)     |  154 |   8 |      |       |       |       |       |
| [2pollution](https://twitter.com/2pollution)             |  150 |   8 |      |  0.07 |  0.23 |  0.09 |       |
| [gbibuildingco](https://twitter.com/gbibuildingco)       |  146 |  94 |      |  0.65 |  0.70 |       |       |
| [Bbsoe123Henry](https://twitter.com/Bbsoe123Henry)       |  140 |   4 |      |       |  0.30 |       |       |
| [ruisaldanha](https://twitter.com/ruisaldanha)           |  124 |  76 |   32 |  0.09 |  0.12 |  0.10 |  0.01 |
| [NikolovScience](https://twitter.com/NikolovScience)     |  123 | 331 |  152 |  0.07 |       |       |  0.36 |
| [SavePl21174455](https://twitter.com/SavePl21174455)     |  113 |  28 |      |  0.24 |  0.44 |  0.29 |  0.15 |
| [green_conc](https://twitter.com/green_conc)             |  110 |     |      |  0.06 |  0.11 |  0.08 |  0.09 |
| [Climate_ch](https://twitter.com/Climate_ch)             |  108 |     |      |  0.09 |  0.17 |  0.09 |  0.09 |
| [Coffeewarblers](https://twitter.com/Coffeewarblers)     |  106 |  39 |      |  0.00 |  0.03 |  0.11 |  0.02 |
| [SERAPHIM003](https://twitter.com/SERAPHIM003)           |  102 | 276 |      |  0.03 |  0.05 |  0.12 |  0.06 |
| [world_dang](https://twitter.com/world_dang)             |   94 |     |      |  0.06 |  0.13 |  0.06 |  0.12 |
| [grisanik](https://twitter.com/grisanik)                 |   91 | 188 |    2 |  0.04 |  0.06 |  0.07 |  0.09 |
| [nasowasaberooch](https://twitter.com/nasowasaberooch)   |   90 |  19 |      |       |       |       |       |
| [alevergara78](https://twitter.com/alevergara78)         |   86 |     |      |  0.01 |  0.02 |       |  0.08 |
| [Greentechsystem](https://twitter.com/Greentechsystem)   |   82 |  24 |      |       |  0.12 |       |  0.08 |
| [Piers_Corbyn](https://twitter.com/Piers_Corbyn)         |   82 | 165 |    4 |  0.69 |  0.35 |  0.43 |  0.02 |
| [KathleenConnell](https://twitter.com/KathleenConnell)   |   78 |   3 |      |       |       |       |       |
| [Carbongate](https://twitter.com/Carbongate)             |   76 |     |   54 |  0.04 |  0.05 |  0.05 |  0.04 |
| [fabveggievegan](https://twitter.com/fabveggievegan)     |   75 |  82 |      |       |       |       |       |
| [natural_sci](https://twitter.com/natural_sci)           |   74 |     |      |  0.07 |  0.12 |  0.07 |  0.08 |
| [eco_ec](https://twitter.com/eco_ec)                     |   74 |     |      |  0.07 |  0.11 |  0.07 |  0.08 |
| [WeCareFarming](https://twitter.com/WeCareFarming)       |   73 |  34 |      |       |  0.42 |       |       |
| [KatherynHale15](https://twitter.com/KatherynHale15)     |   73 |   1 |      |  0.07 |  0.04 |  0.03 |  0.39 |
| [FCalciu](https://twitter.com/FCalciu)                   |   72 |   6 |    1 |  0.11 |  0.15 |  0.06 |  0.13 |
| [NRGrenaissance](https://twitter.com/NRGrenaissance)     |   71 |     |    2 |  0.10 |  0.21 |  0.10 |  0.10 |


### `#globalwarming` communications (RT : retweet): pct[15.12%] cnt[5387]

| SCREEN NAME                                              |   TT |  RT |  TM  |  ANGR |  FEAR |  SAD  |  JOY  |
| -------------------------------------------------------- | ----:|----:| ----:| -----:| -----:| -----:| -----:|
| [JoyfullyECO](https://twitter.com/JoyfullyECO)           |  502 |1370 |    2 |  0.07 |  0.18 |  0.07 |  0.14 |
| [anttilip](https://twitter.com/anttilip)                 |    1 |1149 |    5 |       |       |       |  0.44 |
| [belugasolar](https://twitter.com/belugasolar)           |    9 |1057 |      |  0.07 |  0.27 |  0.13 |  0.17 |
| [NASAClimate](https://twitter.com/NASAClimate)           |    4 | 949 |    3 |       |       |  0.12 |       |
| [HealthRanger](https://twitter.com/HealthRanger)         |   23 | 862 |    2 |  0.10 |  0.08 |  0.06 |  0.02 |


### `#globalwarming` communications (TM : mention): pct[15.48%] cnt[3256]

| SCREEN NAME                                              |   TT |  RT |  TM  |  ANGR |  FEAR |  SAD  |  JOY  |
| -------------------------------------------------------- | ----:|----:| ----:| -----:| -----:| -----:| -----:|
| [realDonaldTrump](https://twitter.com/realDonaldTrump)   |      |     | 1153 |       |       |       |       |
| [algore](https://twitter.com/algore)                     |      |     |  369 |       |       |       |       |
| [BerkeleyEarth](https://twitter.com/BerkeleyEarth)       |      |     |  322 |       |       |       |       |
| [CarbonBrief](https://twitter.com/CarbonBrief)           |    5 | 163 |  286 |       |       |  0.30 |       |
| [POTUS](https://twitter.com/POTUS)                       |      |     |  227 |       |       |       |       |
| [potus](https://twitter.com/potus)                       |      |     |  181 |       |       |       |       |
| [epascottpruitt](https://twitter.com/epascottpruitt)     |      |     |  156 |       |       |       |       |
| [NikolovScience](https://twitter.com/NikolovScience)     |  123 | 331 |  152 |  0.07 |       |       |  0.36 |
| [JWSpry](https://twitter.com/JWSpry)                     |   50 | 220 |  145 |  0.29 |  0.30 |  0.23 |  0.06 |
| [CNN](https://twitter.com/CNN)                           |      |     |  136 |       |       |       |       |
| [acc_grannot](https://twitter.com/acc_grannot)           |      |     |  129 |       |       |       |       |




### `#climatechange` Big Player tweet/retweet percentages for Oct. 1-31

| Screen Name                                              | Total| TT% | RT% |RT Accts|
| -------------------------------------------------------- | ----:| ---:| ---:|-------:|
| [EHFoundation237](https://twitter.com/EHFoundation237)   | 5711 |   0 | 100 |    2865 |
| [TrumpvsScience](https://twitter.com/TrumpvsScience)     | 3164 |   0 | 100 |    2122 |
| [NRGrenaissance](https://twitter.com/NRGrenaissance)     | 2549 |   0 | 100 |    1589 |
| [eljmkt_daily](https://twitter.com/eljmkt_daily)         | 2358 | 100 |   0 |         |
| [eljmkt_climate](https://twitter.com/eljmkt_climate)     | 1933 | 100 |   0 |         |
| [Nicolasgwet](https://twitter.com/Nicolasgwet)           | 1880 |   0 | 100 |    1166 |
| [green_conc](https://twitter.com/green_conc)             | 1700 |   5 |  95 |     925 |
| [Climate_ch](https://twitter.com/Climate_ch)             | 1623 |   6 |  94 |     836 |
| [eco_ec](https://twitter.com/eco_ec)                     | 1423 |   7 |  93 |     778 |
| [annemariayritys](https://twitter.com/annemariayritys)   | 1392 |  95 |   5 |       3 |
| [natural_sci](https://twitter.com/natural_sci)           | 1357 |   7 |  93 |     747 |
| [world_dang](https://twitter.com/world_dang)             | 1325 |   7 |  93 |     676 |
| [MassCreativity](https://twitter.com/MassCreativity)     | 1315 |  98 |   2 |      17 |
| [LeadingWPassion](https://twitter.com/LeadingWPassion)   | 1230 | 100 |   0 |         |
| [AroundOMedia](https://twitter.com/AroundOMedia)         | 1195 | 100 |   0 |         |
| [BeingFarhad](https://twitter.com/BeingFarhad)           |  986 | 100 |   0 |       2 |
| [mabhishek45](https://twitter.com/mabhishek45)           |  867 |   0 | 100 |     841 |
| [latinamerica_ac](https://twitter.com/latinamerica_ac)   |  697 |   0 | 100 |     414 |
| [DrCnfzd](https://twitter.com/DrCnfzd)                   |  614 |  91 |   9 |      19 |
| [GCCThinkActTank](https://twitter.com/GCCThinkActTank)   |  601 |  13 |  87 |       3 |
| [ENCISIN](https://twitter.com/ENCISIN)                   |  462 | 100 |   0 |         |
| [Hurshal](https://twitter.com/Hurshal)                   |  418 | 100 |   0 |         |
| [KwameGilbert](https://twitter.com/KwameGilbert)         |  417 | 100 |   0 |         |
| [StopAdaniCairns](https://twitter.com/StopAdaniCairns)   |  387 |  30 |  70 |      87 |
| [Jackthelad1947](https://twitter.com/Jackthelad1947)     |  351 |  32 |  68 |      71 |
| [ZEROCO2_](https://twitter.com/ZEROCO2_)                 |  342 |  95 |   5 |       6 |
| [CLIME_IT](https://twitter.com/CLIME_IT)                 |  322 | 100 |   0 |         |
| [KarenBeishuizen](https://twitter.com/KarenBeishuizen)   |  319 | 100 |   0 |         |
| [ClimateChangeTT](https://twitter.com/ClimateChangeTT)   |  314 | 100 |   0 |         |
| [rockykistner1](https://twitter.com/rockykistner1)       |  295 |  97 |   3 |       8 |
| [1o5CleanEnergy](https://twitter.com/1o5CleanEnergy)     |  290 |   3 |  97 |      44 |
| [6esm](https://twitter.com/6esm)                         |  281 |  95 |   5 |      13 |
| [enviroish](https://twitter.com/enviroish)               |  280 | 100 |   0 |         |


### `#globalwarming` Big Player tweet/retweet percentages for Oct. 1-31

| Screen Name                                              | Total| TT% | RT% |RT Accts|
| -------------------------------------------------------- | ----:| ---:| ---:|-------:|
| [CreativeCivil](https://twitter.com/CreativeCivil)       |  505 | 100 |   0 |        |
| [ErroldMoody](https://twitter.com/ErroldMoody)           |  475 | 100 |   0 |        |
| [EHFoundation237](https://twitter.com/EHFoundation237)   |  473 |   0 | 100 |    313 |
| [denybot6000](https://twitter.com/denybot6000)           |  169 | 100 |   0 |        |
| [gridpointwx](https://twitter.com/gridpointwx)           |  159 | 100 |   0 |        |
| [Nicolasgwet](https://twitter.com/Nicolasgwet)           |  157 |   0 | 100 |    131 |
| [JoyfullyECO](https://twitter.com/JoyfullyECO)           |  152 | 100 |   0 |        |
| [TimMelino](https://twitter.com/TimMelino)               |  140 | 100 |   0 |        |
| [TrumpvsScience](https://twitter.com/TrumpvsScience)     |  120 |   0 | 100 |     89 |
| [MassCreativity](https://twitter.com/MassCreativity)     |  118 |  98 |   2 |      2 |
| [Bbsoe123Henry](https://twitter.com/Bbsoe123Henry)       |   97 | 100 |   0 |        |
| [artyny59](https://twitter.com/artyny59)                 |   81 |   1 |  99 |      2 |
| [ILuvCO2](https://twitter.com/ILuvCO2)                   |   72 |  97 |   3 |      2 |
| [grisanik](https://twitter.com/grisanik)                 |   71 |  23 |  77 |      3 |
| [ClimateTreaty](https://twitter.com/ClimateTreaty)       |   69 | 100 |   0 |        |
| [ruisaldanha](https://twitter.com/ruisaldanha)           |   67 |  36 |  64 |      9 |
| [BreathingDelhi](https://twitter.com/BreathingDelhi)     |   65 |  98 |   2 |      1 |
| [trend_auditor](https://twitter.com/trend_auditor)       |   57 | 100 |   0 |        |
| [GreenFraud](https://twitter.com/GreenFraud)             |   55 |  98 |   2 |      1 |
| [NRGrenaissance](https://twitter.com/NRGrenaissance)     |   53 |   0 | 100 |     41 |
| [davomoelbryn](https://twitter.com/davomoelbryn)         |   51 |  88 |  12 |      2 |

## Questions

- Who are the RT-100 big players retweeting?
- Are people retweeting the non TT big players?
