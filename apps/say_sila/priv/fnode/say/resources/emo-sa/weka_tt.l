#!/usr/bin/picolisp /usr/lib/picolisp/lib.l
#; ---------------------------------------------------------------------------
#; Combine train+test datasets for Weka Experimenter
#;
(argv *Dataset *Seed)

(def 'λ quote)
(def 'DATA-SRC "sentiment-analysis")
(def 'DSET-CNT 10)

#; ---------------------------------------------------------------------------
(de ->arff (tag rseed)
  (pack DATA-SRC "." *Dataset ".weka.r" rseed "." tag ".arff"))


#; ---------------------------------------------------------------------------
(de combine (rseed)
  #; Combine train & test datasets for this count's PRNG seed
  (let ((dtrain
         dtest
         data-TT) (mapcar (λ (tag)
                            (->arff tag rseed))
                          '(train test TT)))

    #; Have we got them both
    (if2 (info dtrain) (info dtest)
      (prog
        (println 'Processing: data-TT)
        (prinl)

        #; The training dataset can be copied as is
        (call 'cp dtrain data-TT)

        #; The test data needs to be appended
        (out (pack '+ data-TT)
           (println '%)
           (println '% 'TEST 'SET)
           (println '%)
           (in dtest
             #; Skip test header
             (from "@data")
             (line)
             #; Bring in the testing data
             (until (eof)
               (echo))))

        #; All done.  Give some feedback!
        (call 'ls "-lh" dtrain dtest data-TT)
        (prinl))

      (msg dtest  '?)
      (msg dtrain '?)
      (msg 'Test/train " data for r" rseed '?))))


#; ---------------------------------------------------------------------------
(if2 *Dataset *Seed

  #; Combine a range of test & train datasets
  (let (rseed  (format *Seed))
    (mapc (λ (c)
            (combine (+ rseed c)))
          (range 0 (dec DSET-CNT))))

  #; We need somthing more...
  (msg 'seed?)
  (msg 'dataset?)
  (msg 'dataset/seed?))
(bye)
#; vim: syntax=picolisp:
