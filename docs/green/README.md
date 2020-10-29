## Dataset: tweets.all.env.2020-Q1.arff

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


### Minimum tweet activity
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

### WordNet synset constraints + Energy Conservation Account
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

