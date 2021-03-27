# Creating a Dataset:
The process to creating a new dataset of tweets is (currently) at best semi-automated.
There is a fair amount of back-and-forth between the Erlang and the Clojure sides.
The idea, of course, is to automate the process once we have a final data format,
and once it is clear how the architecture should handle all the moving parts.

```erlang
bash: ./dev_sila
Mnesia: "/srv/say_sila/dev/Mnesia.sila@zeus"
Erlang/OTP 22 [erts-10.7.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [hipe]

Eshell V10.7.1  (abort with ^G)
(sila@zeus)1> sila:start().
...
09:17:07.258 [notice] <say_sila_app> Say hello to Say Sila
...
09:17:07.711 [info] <twitter> Connecting to sila_tweets database as sila@zeus.dendrown.net

% green:go() is a debug wrapper around green:start/2, which handles tracker and options for you.
(sila@zeus)2> green:start(gw, [no_retweet, {start, {2020,  1, 1}}, {stop, {2020, 12, 31}}]).
09:22:34.289 [notice] <green> Initializing analysis of enviromentalism
09:22:34.289 [debug] <green> Reading base deniers: _build/default/lib/say_sila/priv/resources/accounts/deniers.lst
09:22:34.290 [debug] <green> Reading base greens: _build/default/lib/say_sila/priv/resources/accounts/greens.lst
{ok,<0.233.0>}

(sila@zeus)3> green:make_arff().
09:22:34.289 [info] <arff> Creating ARFF: /srv/say_sila/weka/tweets/tweets.gw.env.arff
{ok,<<"/srv/say_sila/weka/tweets/tweets.gw.env.arff">>}
09:24:43.338 [info] <arff> ARFF<create>: path[/srv/say_sila/weka/tweets/tweets.gw.env.arff] stat[ok]
(sila@zeus)4>
```

We generally change the default `env` tag that the Erlang `green` module gives the file
to an identifier for the dataset.  For the `b1` 2019 tweets with  #globalwarming, we have:

```bash
bash: cd /srv/say_sila/weka/tweets
bash: mv tweets.gw.env.arff tweets.gw.b1.2019.T00.arff
```

We then need to convert the T00 dataset structure, which is the unlabelled output from the Erlang system,
to the T01 structure, which includes a target attribute in the ARFF: `@attribute stance {green,denier}`.
We do this from the Clojure system.  (This part is a quick candidate for better automatation.)

When considering the dataset codes, note that the T denotes the initial pull of Twitter data, while
the 00 and 01 are simply the historical revision of the T dataset.  The difference between the 00 and 01
revisions is only the presence of the target `stance` attribute.  The dataset codes and revisions are
defined in the Clojure source module
[dataset.clj](https://github.com/dendrown/say_sila/blob/master/apps/say_sila/priv/fnode/say/src/weka/dataset.clj#L53).

```clojure
bash: cd fnode/say
bash: lein repl

2021-03-22 08:12:07.408  DEBUG: Config: config/say.config
...
nREPL server started on port 41681 on host 127.0.0.1 - nrepl://127.0.0.1:41681
REPL-y 0.4.3, nREPL 0.6.0
Clojure 1.10.1
OpenJDK 64-Bit Server VM 1.8.0_272-b10
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

say.core=> (in-ns 'weka.dataset)
#object[clojure.lang.Namespace 0x159c1d03 "weka.dataset"]

weka.dataset=> (def A "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T00.arff")
#'weka.dataset/A

weka.dataset=> (def I (weka/load-arff A))
#'weka.dataset/I

weka.dataset=> (.numAttributes I)
6

weka.dataset=> (target-stance! I)
; The function updates the Instances I in place.
; The return value is the updated I and the REPL spews out the whole dataset.

weka.dataset=> (.numAttributes I)
7

weka.dataset=> (weka/save-file "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.arff" I)
"/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.arff"

```

For processing tweets on Clojure, we need to convert the T01 (Twitter) dataset we just created
into an S02 status (tweet) dataset and a U01 user profile dataset.  We continue in the Clojure REPL:


```clojure
weka.dataset=> (t->su "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.arff")
("/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.S02.arff" "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.U01.arff")
```

Although these are the two dataset formats we need for processing tweet and user profile data.
The dependent attribute, the stances, is unknown for all the instances.  We also need to TweeboParser
to generate dependency parse trees for all the tweets and user profiles.  We can get these simply by
attempting to create the Say Sila world representation on the Clojure side.

We will have to do this several times as the TweeboParser output has issues with quotes in the tweets,
and we will need to run a utility to correct these.  Therefore, we generally start with the `:sila => :min-statuses`
configuration parameter set to 20.  Also, we are free to restart the Clojure REPL as needed to free
system memory.

```clojure
bash: cd fnode/say
bash: lein repl
2021-03-22 08:12:07.408  DEBUG: Config: config/say.config
...

say.core=> (in-ns 'say.sila)
#object[clojure.lang.Namespace 0x73bf6a7f "say.sila"]

say.sila=> (create-world! :b1 "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.U01.arff" "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.S02.arff")
2021-03-23 08:52:16.528   INFO: Dataset user:b1: /srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.U01.arff
2021-03-23 08:52:16.530   INFO: Dataset text:b1: /srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.S02.arff
2021-03-23 08:52:17.345   INFO: Using lexicon: affective.core.NRCEmotionLexiconEvaluator
2021-03-23 08:52:20.143   INFO: Using lexicon: affective.core.NRCEmotionLexiconEvaluator
2021-03-23 08:52:20.207   INFO: Converting 98081 instances
2021-03-23 08:52:22.025  DEBUG: Finalizing activity :b1
2021-03-23 08:52:23.707  DEBUG: Parsing dependencies: cnt[0] fp[/srv/say_sila/tweebo/IK/ProfileOf_IknowNo24994942]
2021-03-23 08:52:33.400  DEBUG: Tweebo on ProfileOf_IknowNo24994942: Tokenized and tagged 1 tweets (6 tokens) in 0.5 seconds: 1.9 tweets/sec, 11.6 tokens/sec
...

```

Depending on the size of the dataset and the `:min-statuses` configuration setting, this process will
generally take a number of hours or days.  When complete, it will try to create the Say Sila ontology,
but will likely error out because of a rogue quote sequence in the Tweebo dependency parse output for
a tweet.  We have a `requote` tool which searches for these problematic quote sequences and corrects
them in the dependency parse files.

```bash
    bash: cd /srv/say_sila/tweebo
    bash: ./requote
    Processing: 248/t1164317335185973248.predict
    ...
    Processing: CH/ProfileOf_charlie.predict
    Processing: 928/t1088189032541052928.predict
    ...
```

After running requote, we go back into Clojure to ensure that `say.sila/create-world!` is now able
to create the world ontology.
Although we continually update the `requote` utility to fix newly discovered quote sequences which
the Clojure csv library is unable to handle, fairly often there will be a sequence which the tool
does not yet know about.  The error in Clojure will give the relative filepath of the offending
output file.  Ideally, we update the `requote` tool to handle the quote sequence.  If this is not
feasible for some reason, we may also update the output file manually, replacing the quote sequence
with the text "QUøTE", taking care not to disturb the tab characters delimiting the columns in the
file.  (The `requote` tool uses the token "QUOTE", which we change slightly as a
marker for the manual update, though our Clojure system makes no differentiation when creating the
ontology.)

Once `say.sila/create-world!` is able to successfully process all the tweets for a given minimum
status level, we need to save the associated accounts so that we may attempt to determine the
green/denier stance of each account.

```clojure
say.sila=> (create-world! :b1 "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.U01.arff" "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.T01.S02.arff")
...
:b1

say.sila=> (save-accounts)
{"/tmp/say_sila/accounts.lst" 1307}
```

Since this process is still considered to be in the design stage, we are saving the accounts to
a temporary directory.  However, we also want to keep track of the accounts, so we move them to
into a subdirectory under `docs` in the project repository.  However, we do not commit the file
to the repository because it contains real user account names on Twitter.  (We have a line in
the project's `.gitignore` so they do not show as active files for `git status`, etc.)

```bash
# From the project root directory
bash: cd docs/green/accounts/b1
bash: mv /tmp/say_sila/accounts.lst accounts.gw.b1.2019.min07.lst
```

However, to avoid the long path name when accessing the file from Erlang in the next step,
we recreate a link in `/tmp/` with a short filename.  This step is done merely for convenience.

```bash
bash: cd /tmp
bash: ln -s /home/dend/prog/say_sila/docs/green/accounts/b1/accounts.gw.b1.2019.min07.lst b1-min07.lst
```
Also, to facilitate lookups by hand if the need should occur, we sort the account names using the `:sort`
command in `vim`.  Of course, this operation can be performed on the symbolic link or the original file
as is convenient.

Now, on the Erlang side, we instruct a running green server to query Twitter to find which of the
leader accounts the users in our list are following (if any).  If no green server is running, one
may be started by calling `green:start(gw, green:opts(day)).` in an Erlang shell running the `say_sila`
application as explained at the beginning of this guide, but only pulling a day's worth of tweets.

```erlang
(sila@zeus)28> G07 = green:load_stances("/tmp/b1-min07.lst").
08:35:57.570 [info] <green> alice: undefined
08:37:15.834 [info] <green> bob: undefined
08:38:34.045 [info] <green> charlie: denier
08:39:52.444 [info] <green> dave: green
...
#{<<"alice">> => undefined,
  <<"bob">> => undefined,
  <<"charlie">> => denier,
  <<"dave">> => green,...}
```

This step may also run for hours or days, depending on how many accounts we are handling.
After it finishes,
we perform manual checks for users following both green and denier leader accounts to see if their stance
is obvious from their tweets and their profile.  To find these users, we simply search the console log
generated by the `green:load_stances/1` call above:

```bash
bash: cd /srv/say_sila/dev/log
bash: grep! 'following both' console.log* >> following.gw.b1.2019.log

```

We set up the "following" log file as so:

```
;; --------------------------------------------------------------------------
;; DENIER:
console.log.0:2021-03-22 09:28:07.595 [warning] <0.228.0> <green> User dave is following both sides: denier[2] green[1]

;; --------------------------------------------------------------------------
;; GREEN:
console.log.0:2021-03-22 10:11:10.344 [warning] <0.228.0> <green> User charlie is following both sides: denier[4] green[7]

;; --------------------------------------------------------------------------
;; UNDEFINED:
console.log.0:2021-03-26 13:37:59.361 [warning] <0.13897.0> <green> User bob is following both sides: denier[2] green[2] ; advertising

;; --------------------------------------------------------------------------
;; INCOMING:
console.log.0:2021-03-26 18:32:07.189 [warning] <0.13897.0> <green> User alice is following both sides: denier[1] green[2]

```

The output of the redirected `grep` command will add the new entries to the INCOMING category.  We first delete any of
the incoming entries that have already been processed and are present under either the DENIER, GREEN, or UNDEFINED
sections.  (We are searching through all the Erlang console logs, including the logs most recently "rolled over" so
that we do not miss any users.) We check each of the remaining users manually, both by going to the user's Twitter
profile page ( `www.twitter.com/bob` ) and by looking at their tweets in our dataset:

```bash
bash: cd /srv/say_sila/weka/tweets
bash: grep grep alice tweets.gw.b1.2019.T01.arff

```

In each case
if a human can trivially determine that a user belongs in the GREEN or DENIER section, we move their log line to that
section.  Otherwise, we move them to the UNDEFINED section.  At the end of the process the INCOMING section should be
empty and ready for the next batch when we are processing the dataset based on a smaller minimum tweet count.  Finally,
we save a copy of the edited following.*.log file for later reference, just as we did for the account lists.

```bash
bash: cp following.gw.b1.2019.log ~/prog/say_sila/docs/green/accounts/b1/following.gw.b1.2019.log
```

 We can now update the users we just checked manually with the green server on the Erlang side:

```erlang
(sila@zeus)35> green:set_stance("alice", green).
ok
08:05:48.457 [info] <green> Setting user alice stance: undefined -> green
```

Then we can have the green server save all the users it knows to be `green` or `denier` based on
the results of checking the leader accounts and our manual checks for users following leader accounts
for both stances.

```erlang
(sila@zeus)40> green:get_stances(uqam).
08:18:03.081 [info] <green> Saving stances to /tmp/say_sila/stances.json
<<"{\"denier\":[\"charlie\",...,],{\"green\":[\"bob\", \"dave\",...]}>>
```

Finally, as a last step we return to the Clojure side to create the labelled
S02 status (tweet) and U01 (profile) datasets.

```clojure
bash: cd /srv/say_sila/weka/tweets
bash: cp tweets.gw.b1.2019.arff tweets.gw.b1.2019.min05.arff

say.sila=> (in-ns 'weka.dataset)
#object[clojure.lang.Namespace 0x68eab0be "weka.dataset"]

weka.dataset=> (label! :t00 "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.min05.arff")
"/srv/say_sila/weka/tweets/tweets.gw.b1.2019.min05.T01.arff"

weka.dataset=> (t->su "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.min05.T01.arff")
("/srv/say_sila/weka/tweets/tweets.gw.b1.2019.min05.T01.S02.arff" "/srv/say_sila/weka/tweets/tweets.gw.b1.2019.min05.T01.U01.arff")
```

Although we repeat this process several times, working our way from a minimum tweet count of 20 down to
a minimum of 2, not every step needs to be performed for every tweet count.  Additionally, a labelled dataset
created at an intermediate phase of the overall process (say, with a tweet count of 10), may be used to
conduct experiments even as the remaining data is being processed to create dependency parse trees and
determine whom users are following.


# TopN20 Big Players for green run

For a human to make a decision:
1. Read name/profile (keywords, known entity)
2. Search for climate (re)tweets (keywords)
3. Search for liberal (re)tweets (keywords)
4. Look at followers (known entities)
5. Look at geo-location
6. Follow link off Twitter
7. Compare w/ beliefs about covid-19
8. Who is user retweeting?
9. "Green" emoji in screen-name/profile (ex. [Nick Bridge](https://twitter.com/FCOClimate))

Use tweet metadata:
- entities->user_mentions


# Dataset: tweets.all.env.2020-Q1.arff

- **Tweets**: 57368
- **Users**:  25524
- **EnergyConservationText1**:  307/16618 @ min20 tweets
- **EnergyConservationText2**:  125/16618 @ min20 tweets


|Run| Size  | Axioms | Strategy              |
|---| -----:| ------:|:--------------------- |
| 00| 3.6GB |        | Base                  |
| 01| 3.1GB |        | No links              |
| 02| 1.4GB |   6.7M | Only meaninful tokens |
| 03| ≈50KB | ≈1000  | Individual ontologies |


## Minimum tweet activity
```clojure
;; Tweets & Profiles
2020-10-22 19:17:03.024   INFO: Minimum status count: 1
2020-10-22 19:17:03.024   INFO: Community size: 25682
2020-10-22 19:22:54.733   INFO: HumanCauseBelieverAccount:env: 201 of 25682 (0.01%)
2020-10-22 19:22:54.734   INFO: NaturalCauseBelieverAccount:env: 118 of 25682 (0.00%)

;; Profiles only
2020-10-22 19:31:31.894   INFO: HumanCauseBelieverAccount:env: 0 of 23774 (0.00%)
2020-10-22 19:31:31.894   INFO: NaturalCauseBelieverAccount:env: 0 of 23774 (0.00%)

;; Tweets only
2020-10-22 20:38:00.329   INFO: HumanCauseBelieverAccount:env: 201 of 25682 (0.01%)
2020-10-22 20:38:00.330   INFO: NaturalCauseBelieverAccount:env: 118 of 25682 (0.00%)

;; At least 2 or 3 tweets
2020-10-22 20:45:51.927   INFO: Minimum status count: 2
2020-10-22 20:45:51.927   INFO: Community size: 6483
2020-10-22 21:01:36.144   INFO: HumanCauseBelieverAccount:env: 85 of 6483 (0.01%)
2020-10-22 21:01:36.145   INFO: NaturalCauseBelieverAccount:env: 52 of 6483 (0.01%)

2020-10-22 22:17:55.274   INFO: Minimum status count: 3
2020-10-22 22:17:55.274   INFO: Community size: 3050
2020-10-22 22:56:59.935   INFO: HumanCauseBelieverAccount:env: 63 of 3050 (0.02%)
2020-10-22 22:56:59.935   INFO: NaturalCauseBelieverAccount:env: 37 of 3050 (0.01%)

;; Minimum of 4 tweets
2020-10-23 --:--:--.---   INFO: Community size: 1884
2020-10-23 15:49:00.150   INFO: HumanCauseBelieverAccount:env: 53 of 1884 (0.03%)
2020-10-23 15:49:00.152   INFO: NaturalCauseBelieverAccount:env: 30 of 1884 (0.02%)
```

## WordNet synset constraints + Energy Conservation Account
```clojure
;; Min tweet count: 1
2020-10-25 21:37:19.338   INFO: EnergyConservationAccount:env: 1240 of 25682 (0.05%)
2020-10-25 21:37:19.341   INFO: HumanCauseBelieverAccount:env: 103 of 25682 (0.00%)
2020-10-25 21:37:19.341   INFO: NaturalCauseBelieverAccount:env: 65 of 25682 (0.00%)

;; Min tweet count: 5
2020-10-26 14:37:44.311   INFO: EnergyConservationAccount:env: 309 of 1317 (0.23%)
2020-10-26 14:37:44.313   INFO: HumanCauseBelieverAccount:env: 20 of 1317 (0.02%)
2020-10-26 14:37:44.314   INFO: NaturalCauseBelieverAccount:env: 17 of 1317 (0.01%)

;; Min tweet count: 10
2020-10-26 15:30:36.937   INFO: EnergyConservationAccount:env: 178 of 527 (0.34%)
2020-10-26 15:30:36.939   INFO: HumanCauseBelieverAccount:env: 10 of 527 (0.02%)
2020-10-26 15:30:36.939   INFO: NaturalCauseBelieverAccount:env: 8 of 527 (0.02%)

;; Min tweet count: 15
2020-10-27 09:36:34.388   INFO: EnergyConservationAccount:env: 137 of 300 (0.46%)
2020-10-27 09:36:34.389   INFO: HumanCauseBelieverAccount:env: 8 of 300 (0.03%)
2020-10-27 09:36:34.390   INFO: NaturalCauseBelieverAccount:env: 5 of 300 (0.02%)

;; Min tweet count: 20
2020-10-28 20:10:41.932   INFO: say.sila/EnergyConservationAccount:env: 118 of 226 (0.52%)
2020-10-28 20:10:41.934   INFO: say.sila/HumanCauseBelieverAccount:env: 7 of 226 (0.03%)
2020-10-28 20:10:41.935   INFO: say.sila/NaturalCauseBelieverAccount:env: 4 of 226 (0.02%)
```

