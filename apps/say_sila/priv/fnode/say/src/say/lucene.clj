;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Climate survey modelling (i.e., the Six Americas)
;;;;
;;;; @copyright 2021 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.lucene
  (:require [say.genie          :refer :all]
            [say.log            :as log]
            [say.wordnet        :as word]
            [clojure.string     :as str])
  (:import  (java.nio.file Paths)
            (org.apache.lucene LucenePackage)
            (org.apache.lucene.analysis Analyzer
                                        Analyzer$TokenStreamComponents
                                        LowerCaseFilter
                                        StopFilter)
            (org.apache.lucene.analysis.en EnglishAnalyzer
                                           PorterStemFilter)
            (org.apache.lucene.analysis.standard StandardAnalyzer
                                                 StandardFilter
                                                 StandardTokenizer)
            (org.apache.lucene.document Document
                                        Field
                                        Field$Store
                                        FieldType
                                        StringField
                                        TextField)
            (org.apache.lucene.index DirectoryReader
                                     IndexOptions
                                     IndexWriter
                                     IndexWriterConfig
                                     IndexWriterConfig$OpenMode)
            (org.apache.lucene.queryparser.classic QueryParser)
            (org.apache.lucene.search IndexSearcher
                                      Query
                                      ScoreDoc)
            (org.apache.lucene.search.similarities ClassicSimilarity)
            (org.apache.lucene.store FSDirectory)))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Index-Dir      "resources/lucene-ndx")
(def ^:const Problem-Fix    "-")


;;; --------------------------------------------------------------------------
(defn check-engine
  "Displays Lucene engine version information."
  []
  (log/notice "Lucene: " (.toString (LucenePackage/get))))


;;; --------------------------------------------------------------------------
(defn- make-local-path
  "Creates a path capable of placating java.nio"
  [parts & opts]
  (let [path (Paths/get (System/getProperty "user.dir") (into-array parts))]
    (if (some #{:uri} opts)
        (.toUri path)
        path)))


;;; --------------------------------------------------------------------------
(defn- make-repo-path
  "Creates a path to the Lucene index repository."
  [repo opts]
  (let [tags (str/join "-" (map name
                                (filter #{:baseline :english :porter :substd}
                                        opts)))]
    (log/debug "TAGS:" tags)
    (make-local-path [Index-Dir (str (name repo) "-" tags)] opts)))


;;; --------------------------------------------------------------------------
(defn- make-analyzer
  "Returns an object representing one of the Analyzers supported with the project."
  [opts]
  (cond
    (some #{:english}  opts) (EnglishAnalyzer.)
    (some #{:baseline} opts) (StandardAnalyzer.)
    (some #{:porter}   opts)
      (proxy [Analyzer]
        []
        (createComponents [field]
          (let [tokens  (StandardTokenizer.)
                filters (-> (StandardFilter. tokens)
                            (StopFilter.     StandardAnalyzer/ENGLISH_STOP_WORDS_SET)
                            (LowerCaseFilter.)
                            (PorterStemFilter.))]
            (log/info "Using Porter-Stemming-Stop Analyzer")
            (Analyzer$TokenStreamComponents. tokens filters))))
    (some #{:substd} opts)
      (proxy [Analyzer]
        []
        (createComponents [field]
          (let [tokens  (StandardTokenizer.)
                filters (-> (StandardFilter. tokens))]
            (log/info "Using Base Analyzer")
            (Analyzer$TokenStreamComponents. tokens filters))))

    ; DEFAULT: same as baseline
    :else (StandardAnalyzer.)))


;;; --------------------------------------------------------------------------
(defn- make-indexer
  "Creates an index writer for a document store."
  ([repo opts]
    (log/debug "OPTS:" opts)
    (let [path (make-repo-path repo opts)
          dir  (FSDirectory/open path)
          ann  (make-analyzer opts)
          cfg  (IndexWriterConfig. ann)]

      ; BM25 is the default similarity, check for an override
      (when (some #{:tfidf} opts)
        (.setSimilarity cfg (ClassicSimilarity.)))

      ; Make it so...
      (log/info (log/<> "INDEX" (.getSimilarity cfg)) "Creating on" path)
      (.setOpenMode cfg IndexWriterConfig$OpenMode/CREATE)
      (IndexWriter. dir cfg))))


;;; --------------------------------------------------------------------------
(defn- vtext-field-type
  "Returns a term-vector-enabled field type for indexingtokenizing text."
  [store?]
  (let [ftype (doto (FieldType.)
                     (.setIndexOptions IndexOptions/DOCS_AND_FREQS_AND_POSITIONS)
                     (.setTokenized true)
                     (.setStoreTermVectors true))]

    (when (= store? Field$Store/YES)
      (.setStored ftype true))

    (.freeze ftype)
    ftype))

(def vtext-field-type# (memoize vtext-field-type))



;;; --------------------------------------------------------------------------
(defn- make-vtext-field
  "Creates what basically amounts to a TextField, but with term-vectors."
  [^String  label
   ^String  value
            store?]
  (Field. label value ^FieldType (vtext-field-type# store?)))



;;; --------------------------------------------------------------------------
(defn- index-doc
  "Convert an XML document map into Lucene fields for a Document, added to
  the specified index writer."
  [indexer id text]
  (let [id  (name id)      ; Un-keywordize
        doc (Document.)]

    (doseq [fld [(StringField. "ID" id Field$Store/YES)
                 (make-vtext-field "TEXT" text Field$Store/YES)]]
      (.add doc fld))

    ;; We support a test mode that doesn't really create an index
    (if (= indexer :test)
        (log/debug (log/<> "ID" id) (log/brief text))
        (do
          (.addDocument ^IndexWriter indexer doc)
           (log/debug (log/<> "DOC" (.get doc "ID"))
                      (log/fmt "~2d fields" (count (.getFields doc))))))))


;;; --------------------------------------------------------------------------
(defn create-index
  "Indexes all the documents in the specified repository (directory)."
  ([repo txtmap]
  (create-index repo txtmap [:baseline]))

  ([repo txtmap opts]
  (with-open [indexer ^IndexWriter (make-indexer repo opts)]

    (doseq [[id txt] txtmap]
      (index-doc indexer id txt))

       ; How did we do?
      (log/info (log/<> "INDEX" (.numDocs indexer))
                (.toString (.getDirectory indexer)))

      (doseq [field (.getFieldNames indexer)]
        (log/debug "Field:" field)))))


;;; --------------------------------------------------------------------------
(defn boost
  "Accepts a string, containing a space-separated series of words, and returns an
  expanded series, representing members of the synsets for all the input words."
  [words]
  (log/debug "BOOST:" words)

  (letfn [(alpha-num? [preword]
            ; Slashes kill Lucene, but we may want to keep the word
            (let [word (str/replace preword #"/" Problem-Fix)]

            ; Allow alpha/numeric/hyphen and nothing else
            (if (re-find #"[^a-zA-Z\-\d:]" word)
                false
                word)))

          (scrub [words]
             ;; Remove non alpha-numeric terms
             (filter identity (map alpha-num? words)))]

       (into #{} (scrub (mapcat word/synonyms words)))))


;;; --------------------------------------------------------------------------
(defn- ^Query make-query
  "Returns a Lucene query, boosted if the appropriate option is included in opts.
  Of course, the only currently recognized boosting option is :wordnet"
  [^QueryParser parser
                words
                opts]

  ; Build a first-pass for the query so that Lucene can remove stop words, etc.
  ;(log/debug "WORDS:" words)
  (let [qline (if (string? words)
                  words
                  (str/join " " words))

        qtext    (-> qline
                     (str/replace #"(?:http)[^\s]+" " ")
                     (str/replace #"QUOTE|QUøTE|\"|&|/|:|\?|\!|\{|\}|\[|\]|\(|\)|\.|\*|\+" " ")
                     (str/replace #"-+" "-")
                     (str/trim)
                     (str/replace #"^-|-$" ""))
        _        (log/debug "SHINY:" qtext)
        prequery (.parse parser qtext)]

    (if (some #{:wordnet} opts)

      ; They want a WordNet boost
      (let [qwords (str/split (.toString prequery "TEXT") #" ")
            qboost (apply str (interpose " " (boost qwords)))]

        (log/debug "BOOST:" qboost)
        (.parse parser qboost))

      ; No boost requested, so just go with the preliminary step.
      prequery)))


;;; --------------------------------------------------------------------------
(defn- run-search
  "Performs the one query against the specified index.  This function may call
  itself to repeat a query after expanding it, adding terms from the top document
  results from the first request."
  [^DirectoryReader reader
   ^IndexSearcher   searcher
   ^QueryParser     parser
                    qwords
                    opts]
  (let [query    (make-query parser qwords opts)
        top-docs (.search searcher query 1)]

   ;(log/debug "QUERY" (.toString query))
   ;(log/debug "QUERY" (.toString query "TEXT"))

    ;; Return the ID of the best document
    (when (pos? (.-totalHits top-docs))
      (let [hit ^ScoreDoc (first (.-scoreDocs top-docs))
            ndx (.-doc hit)
            doc (.document reader ndx)
            id  (.get doc "ID")]
        (log/fmt-info "Found document ~a: ndx[~a] score[~a]" id ndx (.-score hit))
        (log/debug "----------------------------------------------------------------")
        id))))


;;; --------------------------------------------------------------------------
(defn run-searches
  "Runs a series of requests (queries) for the specified repository (directory).
  The create-index function must have already been called for the repository."
  ([repo queries]
  (run-searches repo queries [:baseline]))

  ([repo queries opts]
  (with-open [dir    (FSDirectory/open (make-repo-path repo opts))
              reader (DirectoryReader/open dir)]

    (let [searcher (IndexSearcher. reader)
          parser   (QueryParser. "TEXT" (make-analyzer opts))
          qmap?    (map? queries)]

      ;; BM25 is the default similarity, check for an override
      (when (some #{:tfidf} opts)
        (.setSimilarity searcher (ClassicSimilarity.)))
      (log/info (log/<> "SEARCH" (.getSimilarity searcher true)) "Querying against" repo)

      (into (if qmap? {} '())
            (map (fn [[tag query]]
                   (log/debug "Running query:" tag)
                   [tag (run-search reader searcher parser query opts)])
            (if qmap?
                queries                     ; We have a batch to run
                [[:query queries]])))))))   ; We have a singleton  query


