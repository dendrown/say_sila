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
            (org.apache.lucene.index IndexOptions
                                     IndexWriter
                                     IndexWriterConfig
                                     IndexWriterConfig$OpenMode)
            (org.apache.lucene.search.similarities ClassicSimilarity)
            (org.apache.lucene.store FSDirectory)
  ))


;;; --------------------------------------------------------------------------
(set! *warn-on-reflection* true)

(def ^:const Index-Dir "resources/lucene-ndx")


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
    (let [path (make-local-path [Index-Dir
                                 (name repo)
                                 (str/join "-" (map name opts))])
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
(defn index-doc
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
        (log/debug field)))))

