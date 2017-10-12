say_sila
=====

Analysis of Sentiment in Tweets on Climate Change.  This project is the core of the research portion
of my Ph.D. studies in _Informatique Cognitive_ at the _Université du Québec à Montréal_.

Build
-----

    $ rebar3 compile

TODO
-----
- Add BIG/REG player counts to WUI reports
- Reformat WUI as GRAPH:HITS, scrolling vertically for the four emotions
- In/exclude RTs in runs
- Check language tag in tweet metadata
- Compare tracking reports using different lexicons (allow combinatiions)
- Allow **raven** to handle both hashtags at the same time (or distribute across procs/nodes)
- Automate track reporting for daily updates
- Keep track report history
