;; Trimmed from real code: multiple top-level forms with fn* (map + format) so
;; clj->meme->clj can assign different autogensym slots than clj->read.
(ns meme-lang.test.gensym-multi-fn
  (:require [clojure.string :as str])
  (:import [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]
           [java.security MessageDigest]))

(def ^:private secret (or (System/getenv "EVALEVAL_SECRET") "dev-secret"))

(defn- hmac [s]
  (let [mac (Mac/getInstance "HmacSHA256")
        key (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256")]
    (.init mac key)
    (let [b (.doFinal mac (.getBytes s "UTF-8"))]
      (apply str (map #(format "%02x" (bit-and % 0xff)) b)))))

(defn sign [code-str]
  (str code-str "|" (hmac code-str)))
