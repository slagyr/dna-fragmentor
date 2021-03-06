(ns dna-fragmentor.core
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combinatorics]))

(defn log [& s] (println (apply str s)))

(def -config (atom {}))

(defn config [id] (get (deref -config) id))

(defn load-config [config-file]
  (let [config-data (slurp config-file)
        config (edn/read-string config-data)]
    (reset! -config config)))

(defn structure-tag [[id tag]]
  {:id id :tag tag :locations []})

(defn structure-tags [tags]
  (reduce
    (fn [tags [id tag]] (assoc tags id (structure-tag [id tag])))
    {} tags))

(defn find-locations [tag sequence]
  (loop [locations [] index 0]
    (if-let [location (str/index-of sequence tag index)]
      (recur (conj locations location) (inc location))
      locations)))

(defn populate-locations [tags sequence]
  (reduce
    (fn [tags [id tag]]
      (let [locations (find-locations (:tag tag) sequence)]
        (log "Tag " (:id tag) ". " (:tag tag) " " (count locations) " occurrences: " locations)
        (assoc-in tags [id :locations] locations)))
    tags
    (sort-by first (seq tags))))

(defn compile-tags [config]
  (let [tags (structure-tags (:tags config))
        tags (populate-locations tags (:sequence config))]
    (assoc config :tags tags)))

(defn expand-tag-locations [tag]
  (map
    (fn [location] {:id (:id tag) :location location})
    (:locations tag)))

(defn expand-tags-locations [tags]
  (filter seq (map expand-tag-locations tags)))

(defn fragment-lengths-ok? [max-fragment-length sequence-length solution]
  (let [locations (concat [0] (map :location solution) [sequence-length])]
    (every?
      (fn [[a b]]
        (and
          (> b a)
          (not (> (- b a 4) max-fragment-length))))
      (partition 2 1 locations))))

(defn shortcuts [products tag-count min-tags]
  (mapcat
    (fn [p]
      (map
        #(take % p)
        (range min-tags (inc (min (count p) tag-count)))))
    products))

(defn resolve-fragment [sequence tags solution]
  (if-not (seq solution)
    [sequence]
    (let [fragments (map
                      (fn [[a b]] [(+ 4 (:location a)) (:location b) (:tag (get tags (:id b)))])
                      (partition 2 1 solution))
          first (first solution)
          last (last solution)
          fragments (concat [[0 (:location first) (:tag (get tags (:id first)))]]
                            fragments
                            [[(+ 4 (:location last)) (.length sequence) ""]])]
      (map (fn [[s e t]] (str (subs sequence s e) t)) fragments))))

(defn check-sufficient-tags-found [sequence-length max-fragment-length tags-locations]
  (let [found-tags (->> tags-locations
                        (map second)
                        (map :locations)
                        (map seq)
                        (remove nil?)
                        count)
        required-fragment-length (int (+ 0.999 (/ sequence-length found-tags)))]
    (when (> required-fragment-length max-fragment-length)
      (throw (ex-info "Insufficient tags found" {:found-tags found-tags
                                                 :required-fragment-length required-fragment-length})))))

(defn find-solutions [config]
  (let [sequence (:sequence config)
        sequence-length (.length sequence)
        _ (log "Sequence: " sequence-length " bp\n" sequence "\n")
        max-fragment-length (:max-fragment-length config)
        _ (log "Max Fragment Length: " max-fragment-length "\n")
        config (compile-tags config)
        _ (check-sufficient-tags-found sequence-length max-fragment-length (:tags config))
        tags (vals (:tags config))
        tags-locations (expand-tags-locations tags)
        min-tags (dec (int (+ 0.99999 (/ sequence-length (double max-fragment-length)))))
        perms (combinatorics/permutations tags-locations)
        products (mapcat #(apply combinatorics/cartesian-product %) perms)
        possible-solutions (shortcuts products (count tags) min-tags)
        ;_ (log "\nPossible solutions: " (count possible-solutions))
        solutions (set (filter (partial fragment-lengths-ok? max-fragment-length sequence-length) possible-solutions))]
    (log "Valid solutions: " (count solutions))
    (let [sol-strings (map #(resolve-fragment sequence (:tags config) %) solutions)]
      (doseq [[n sol] (partition 2 (interleave (range) sol-strings))]
        (log "\n" (inc n) ".")
        (doseq [frag sol]
          (log "\t" frag)))
      sol-strings
      )))

(defn -main
  [& args]
  (let [config (load-config (first args))]
    (find-solutions config)))

