(ns meme.alpha.emit.pprint
  "Pretty-printer: width-aware meme formatting.
   Thin wrapper over printer/to-doc + printer/layout."
  (:require [meme.alpha.emit.printer :as printer]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(def ^:private default-width 80)

(defn pprint-form
  "Pretty-print a single Clojure form as meme text.
   Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([form] (pprint-form form nil))
  ([form opts]
   (let [width (or (:width opts) default-width)]
     (printer/layout (printer/to-doc form :meme) width))))

(defn pprint-forms
  "Pretty-print a sequence of Clojure forms as meme text,
   separated by blank lines. Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([forms] (pprint-forms forms nil))
  ([forms opts]
   (let [trailing-ws (:trailing-ws (meta forms))
         trailing-comments (when trailing-ws
                             (printer/extract-comments trailing-ws))
         body (str/join "\n\n" (map #(pprint-form % opts) forms))]
     (if trailing-comments
       (str body "\n\n" (str/join "\n" trailing-comments))
       body))))
