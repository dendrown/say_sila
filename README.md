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

    (sila@devbox)1> sila:go().

Or for a node that will be collecting the tweet stream, we can shortcut the Twitter authentication
process:

    (sila@devbox)1> sila:go(twitter).


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
### `#climatechange` Big Player tweet/retweet percentages for Oct. 1-27.

| Screen Name                                              | Total| TT% | RT% |
| -------------------------------------------------------- | ----:| ---:| ---:|
| [EHFoundation237](https://twitter.com/EHFoundation237)   | 5293 |   0 | 100 |
| [TrumpvsScience](https://twitter.com/TrumpvsScience)     | 2740 |   0 | 100 |
| [NRGrenaissance](https://twitter.com/NRGrenaissance)     | 2204 |   0 | 100 |
| [eljmkt_daily](https://twitter.com/eljmkt_daily)         | 2081 | 100 |   0 |
| [Nicolasgwet](https://twitter.com/Nicolasgwet)           | 1770 |   0 | 100 |
| [eljmkt_climate](https://twitter.com/eljmkt_climate)     | 1662 | 100 |   0 |
| [green_conc](https://twitter.com/green_conc)             | 1448 |   5 |  95 |
| [Climate_ch](https://twitter.com/Climate_ch)             | 1379 |   6 |  94 |
| [eco_ec](https://twitter.com/eco_ec)                     | 1214 |   7 |  93 |
| [natural_sci](https://twitter.com/natural_sci)           | 1172 |   7 |  93 |
| [world_dang](https://twitter.com/world_dang)             | 1145 |   7 |  93 |
| [MassCreativity](https://twitter.com/MassCreativity)     | 1144 |  99 |   1 |
| [annemariayritys](https://twitter.com/annemariayritys)   | 1105 |  94 |   6 |
| [LeadingWPassion](https://twitter.com/LeadingWPassion)   |  957 | 100 |   0 |
| [AroundOMedia](https://twitter.com/AroundOMedia)         |  926 | 100 |   0 |
| [mabhishek45](https://twitter.com/mabhishek45)           |  866 |   0 | 100 |
| [BeingFarhad](https://twitter.com/BeingFarhad)           |  861 | 100 |   0 |
| [DrCnfzd](https://twitter.com/DrCnfzd)                   |  606 |  92 |   8 |
| [latinamerica_ac](https://twitter.com/latinamerica_ac)   |  569 |   0 | 100 |
| [GCCThinkActTank](https://twitter.com/GCCThinkActTank)   |  473 |  14 |  86 |
| [ENCISIN](https://twitter.com/ENCISIN)                   |  380 | 100 |   0 |
| [KwameGilbert](https://twitter.com/KwameGilbert)         |  370 | 100 |   0 |
| [Hurshal](https://twitter.com/Hurshal)                   |  360 | 100 |   0 |
| [StopAdaniCairns](https://twitter.com/StopAdaniCairns)   |  326 |  28 |  72 |
| [ZEROCO2_](https://twitter.com/ZEROCO2_)                 |  290 |  98 |   2 |
| [Jackthelad1947](https://twitter.com/Jackthelad1947)     |  284 |  30 |  70 |
| [CLIME_IT](https://twitter.com/CLIME_IT)                 |  282 | 100 |   0 |
| [rockykistner1](https://twitter.com/rockykistner1)       |  279 |  97 |   3 |
| [1o5CleanEnergy](https://twitter.com/1o5CleanEnergy)     |  277 |   3 |  97 |
| [ClimateChangeTT](https://twitter.com/ClimateChangeTT)   |  270 | 100 |   0 |
| [belugasolar](https://twitter.com/belugasolar)           |  259 |   1 |  99 |
| [6esm](https://twitter.com/6esm)                         |  246 |  96 |   4 |
| [LetHumanismRing](https://twitter.com/LetHumanismRing)   |  243 | 100 |   0 |
| [maggiesmumbles](https://twitter.com/maggiesmumbles)     |  230 |   0 | 100 |


### `#globalwarming` Big Player tweet/retweet percentages for Oct. 1-27.

| Screen Name                                              | Total| TT% | RT% |
| -------------------------------------------------------- | ----:| ---:| ---:|
| [CreativeCivil](https://twitter.com/CreativeCivil)       |  479 | 100 |   0 |
| [EHFoundation237](https://twitter.com/EHFoundation237)   |  441 |   0 | 100 |
| [ErroldMoody](https://twitter.com/ErroldMoody)           |  414 | 100 |   0 |
| [denybot6000](https://twitter.com/denybot6000)           |  146 | 100 |   0 |
| [Nicolasgwet](https://twitter.com/Nicolasgwet)           |  143 |   0 | 100 |
| [gridpointwx](https://twitter.com/gridpointwx)           |  140 | 100 |   0 |
| [JoyfullyECO](https://twitter.com/JoyfullyECO)           |  128 | 100 |   0 |
| [TimMelino](https://twitter.com/TimMelino)               |  121 | 100 |   0 |
| [MassCreativity](https://twitter.com/MassCreativity)     |  106 |  98 |   2 |
| [TrumpvsScience](https://twitter.com/TrumpvsScience)     |  106 |   0 | 100 |
| [Bbsoe123Henry](https://twitter.com/Bbsoe123Henry)       |   91 | 100 |   0 |
| [grisanik](https://twitter.com/grisanik)                 |   70 |  21 |  79 |
| [ruisaldanha](https://twitter.com/ruisaldanha)           |   66 |  35 |  65 |
| [ILuvCO2](https://twitter.com/ILuvCO2)                   |   61 |  97 |   3 |
| [trend_auditor](https://twitter.com/trend_auditor)       |   57 | 100 |   0 |
| [ClimateTreaty](https://twitter.com/ClimateTreaty)       |   56 | 100 |   0 |
| [BreathingDelhi](https://twitter.com/BreathingDelhi)     |   55 |  98 |   2 |
| [davomoelbryn](https://twitter.com/davomoelbryn)         |   51 |  88 |  12 |
| [petra_bones](https://twitter.com/petra_bones)           |   48 | 100 |   0 |
| [GreenFraud](https://twitter.com/GreenFraud)             |   48 |  98 |   2 |


## Questions

- Who are the RT-100 big players retweeting?
- Are people retweeting the non TT big players?
