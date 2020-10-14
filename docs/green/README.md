## Dataset: tweets.all.env.2020-Q1.arff

- **Tweets**: 57378
- **Users**:  25524

|Run| Size  | Axioms | Strategy              |
|:-:|.-----:|:------:|----------------------:|
| 00| 3.6GB |        | Base                  |
| 01| 3.1GB |        | No links              |
| 02| 1.4GB |   6.7M | Only meaninful tokens |
| 03| ≈50KB | ≈1000  | Individual ontologies |

```clojure
;; Initial run with HermiT [JVM using 6.7GB before reasoning]
say.sila=> (time (report-accounts))
Execution error (OutOfMemoryError) at java.util.HashMap$KeySet/iterator (HashMap.java:917).
GC overhead limit exceeded

;; Rework: Disposing of HermiT after reasoning [34 minutes on 8 cores]
say.sila=> (time (report-accounts))
2020-10-12 16:09:19.235   INFO: HumanCauseBelieverAccount:env: 197
2020-10-12 16:09:19.235   INFO: NaturalCauseBelieverAccount:env: 115
nil
"Elapsed time: 2068176.991149 msecs"
```

## TopN20 Big Players for green run

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

