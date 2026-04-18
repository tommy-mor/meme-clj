(ns wlj-lang.stages
  "Composable pipeline stages for wlj-lang.  See `meme-lang.stages` for
   the three-layer pipeline model and contract conventions.

   | Key     | Type    | Written by | Read by    |
   |---------|---------|------------|------------|
   | :source | String  | caller     | parse      |
   | :opts   | Map/nil | caller     | parse, read|
   | :cst    | Vector  | parse      | read       |
   | :forms  | Vector  | read       | caller     |"
  (:require [clojure.string :as str]
            [meme.tools.parser :as pratt]
            [wlj-lang.grammar :as grammar]
            [wlj-lang.cst-reader :as cst-reader]))

(def stage-contracts
  "Machine-readable pipeline contract for wlj-lang stages."
  {:step-parse {:requires #{:source} :produces #{:cst}}
   :step-read  {:requires #{:cst}    :produces #{:forms}}})

(defn- check-contract!
  [stage-name ctx]
  (when-let [required (get-in stage-contracts [stage-name :requires])]
    (let [missing (remove #(contains? ctx %) required)]
      (when (seq missing)
        (throw (ex-info
                 (str "Pipeline stage " stage-name " missing required ctx key(s): "
                      (str/join ", " (map pr-str missing))
                      ". Did you skip an earlier stage?")
                 {:type :wlj-lang/pipeline-error
                  :stage stage-name
                  :missing (vec missing)
                  :ctx-keys (vec (keys ctx))}))))))

(defn step-parse [ctx]
  (check-contract! :step-parse ctx)
  (assoc ctx :cst (pratt/parse (:source ctx) grammar/grammar)))

(defn step-read [ctx]
  (check-contract! :step-read ctx)
  (assoc ctx :forms (cst-reader/read-forms (:cst ctx) (:opts ctx))))

(defn run [source opts]
  (-> {:source source :opts opts} step-parse step-read))
