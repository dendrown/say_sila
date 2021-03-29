#!/usr/bin/picolisp /usr/lib/picolisp/lib.l
#; ---------------------------------------------------------------------------
#; Change « " » to « QUOTE » in Tweebo output files
#;
(def 'λ quote)

#; ---------------------------------------------------------------------------
(de requoteln (Ln)
  (match '(@Ndx "^I" @Tok "^I" @Rest) (chop Ln))
  (when (or (= @Tok '("\""))
            (= @Tok '( "\"" "'"))
            (= @Tok '("\"" "‘"))
            (= @Tok '( "\"" "\""))
            (= @Tok '( "\"" "\"" "\"")))
    (append @Ndx '("^I" "QUOTE" "^I") @Rest)))



#; ---------------------------------------------------------------------------
(de requote (FPath)
  #; Reads in the file and replaces a double quote token  « " » with « QUOTE »
  (prinl "Processing: " FPath)

  #; Make a copy...just in case
  (let (FOrig (pack FPath ".orig"))
    (if (call 'mv FPath FOrig)
      #; Read from the (moved) original
      (in FOrig
        #; Write to the file path they sent us
        (out FPath
          (until (eof)
            #; Check each line for quotes needing conversion
            (let (Ln  (line T)
                  Qn (requoteln Ln))
              (prinl (if Qn Qn Ln)))))))))



#; ---------------------------------------------------------------------------
# Handle files from command line
(mapc requote (argv))

# Handle files piped to us
(in NIL
  (until (eof)
    (requote (trim (line T)))))

(bye)
#; vim: syntax=picolisp:
