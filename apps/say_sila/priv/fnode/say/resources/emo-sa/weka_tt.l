#!/usr/bin/picolisp /usr/lib/picolisp/lib.l
#; ---------------------------------------------------------------------------
#; Combine train+test datasets for Weka Experimenter
#;
(argv *Dataset)

(def 'DATA-SRC "sentiment-analysis")

#; ---------------------------------------------------------------------------
(de ->arff (tag)
  (pack DATA-SRC "." *Dataset "." 'weka "." tag ".arff"))
  

#; ---------------------------------------------------------------------------
(if *Dataset
  #; Combine train & test datasets
  (let ((dtrain
         dtest
         data-TT) (mapcar ->arff '(train test TT)))

    #; Have we got them both
    (if2 (info dtrain) (info dtest)
      (prog
        (println 'Processing: DATA-SRC)
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

      (prinl dtest  '?)
      (prinl dtrain '?)
      (println 'Test/train 'datasets?)))

  #; Right now we require something
  (println 'dataset?))

(bye)
#; vim: syntax=picolisp:
