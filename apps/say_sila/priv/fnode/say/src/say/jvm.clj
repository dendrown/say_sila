;;;; -------------------------------------------------------------------------
;;;;
;;;;        _/_/_/  _/_/_/  _/          _/_/
;;;;     _/          _/    _/        _/    _/
;;;;      _/_/      _/    _/        _/_/_/_/
;;;;         _/    _/    _/        _/    _/
;;;;  _/_/_/    _/_/_/  _/_/_/_/  _/    _/
;;;;
;;;; Java Virtual Machine (JVM) utilities
;;;;
;;;; @copyright 2020 Dennis Drown et l'Université du Québec à Montréal
;;;; -------------------------------------------------------------------------
(ns say.jvm)

(set! *warn-on-reflection* true)


;;; --------------------------------------------------------------------------
(defn memory-used
  "Returns the memory the application is using on the JVM.  The default return
  is in bytes, but the caller can specify :KB, :MB or :GB as desired."
  ([]
  (let [jvm   (Runtime/getRuntime)]
    (- (.totalMemory jvm)
       (.freeMemory jvm))))


  ([unit]
  (quot (memory-used)
        (get {:KB 1024
              :MB 1048576
              :GB 1073741824}
             unit))))

