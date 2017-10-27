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

| Screen Name      | Total| TT% | RT% |
| ---------------- | ----:| ---:| ---:|
| EHFoundation237  | 5290 |   0 | 100 |
| TrumpvsScience   | 2735 |   0 | 100 |
| NRGrenaissance   | 2201 |   0 | 100 |
| eljmkt_daily     | 2079 | 100 |   0 |
| Nicolasgwet      | 1769 |   0 | 100 |
| eljmkt_climate   | 1661 | 100 |   0 |
| green_conc       | 1446 |   5 |  95 |
| Climate_ch       | 1377 |   6 |  94 |
| eco_ec           | 1212 |   7 |  93 |
| natural_sci      | 1172 |   7 |  93 |
| world_dang       | 1143 |   7 |  93 |
| MassCreativity   | 1144 |  99 |   1 |
| annemariayritys  | 1104 |  94 |   6 |
| LeadingWPassion  |  956 | 100 |   0 |
| AroundOMedia     |  924 | 100 |   0 |
| mabhishek45      |  866 |   0 | 100 |
| BeingFarhad      |  861 | 100 |   0 |
| DrCnfzd          |  606 |  92 |   8 |
| latinamerica_ac  |  569 |   0 | 100 |
| GCCThinkActTank  |  473 |  14 |  86 |
| ENCISIN          |  380 | 100 |   0 |
| KwameGilbert     |  370 | 100 |   0 |
| Hurshal          |  360 | 100 |   0 |
| StopAdaniCairns  |  326 |  28 |  72 |
| ZEROCO2_         |  288 |  98 |   2 |
| Jackthelad1947   |  284 |  30 |  70 |
| CLIME_IT         |  282 | 100 |   0 |
| rockykistner1    |  279 |  97 |   3 |
| 1o5CleanEnergy   |  277 |   3 |  97 |
| ClimateChangeTT  |  269 | 100 |   0 |
| belugasolar      |  259 |   1 |  99 |
| 6esm             |  243 |  96 |   4 |
| LetHumanismRing  |  243 | 100 |   0 |
| maggiesmumbles   |  230 |   0 | 100 |


### `#globalwarming` Big Player tweet/retweet percentages for Oct. 1-27.

| Screen Name      | Total| TT% | RT% |
| ---------------- | ----:| ---:| ---:|
| CreativeCivil    |  479 | 100 |   0 |
| EHFoundation237  |  441 |   0 | 100 |
| ErroldMoody      |  414 | 100 |   0 |
| denybot6000      |  145 | 100 |   0 |
| Nicolasgwet      |  143 |   0 | 100 |
| gridpointwx      |  140 | 100 |   0 |
| JoyfullyECO      |  128 | 100 |   0 |
| TimMelino        |  121 | 100 |   0 |
| MassCreativity   |  106 |  98 |   2 |
| TrumpvsScience   |  106 |   0 | 100 |
| Bbsoe123Henry    |   91 | 100 |   0 |
| grisanik         |   70 |  21 |  79 |
| ruisaldanha      |   66 |  35 |  65 |
| ILuvCO2          |   61 |  97 |   3 |
| trend_auditor    |   57 | 100 |   0 |
| ClimateTreaty    |   56 | 100 |   0 |
| BreathingDelhi   |   55 |  98 |   2 |
| davomoelbryn     |   51 |  88 |  12 |
| petra_bones      |   48 | 100 |   0 |
| GreenFraud       |   48 |  98 |   2 |


