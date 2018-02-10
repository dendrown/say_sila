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
- Compare tracking reports using different lexicons (allow combinatiions)
- Allow **raven** to handle both hashtags at the same time (or distribute across procs/nodes)
- Automate track reporting for daily updates
- Keep track report history
- Make ecsv behave when it encounters bad CSV data
- ~~Reformat WUI as GRAPH:HITS, scrolling vertically for the four emotions~~
- ~~In/exclude RTs in runs~~
- ~~Check language tag in tweet metadata~~

To Ponder
---------
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
