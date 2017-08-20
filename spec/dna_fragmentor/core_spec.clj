(ns dna-fragmentor.core-spec
  (:require [speclj.core :refer :all]
            [dna-fragmentor.core :refer :all]))

(def sample-config
  {:sequence            "TATGCCGGCTGCGGAAACTAGCCCCGAGTTTTGAAACCTGGAACGTCGCAAGCTGATGACCACTAGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA"
   :max-fragment-length 42
   :tags                {"A" "AGAA"
                         "B" "ACTA"
                         "C" "GATT"
                         "D" "CCTG"
                         "E" "ATCA"
                         "F" "GCGT"
                         "G" "CTCC"
                         "H" "CAGC"
                         "I" "TCGG"
                         "K" "CGAC"}})


(describe "DNA Framgmentor"

  (it "reads config"
    (load-config "spec/test-config.edn")
    (should= 42 (config :max-fragment-length))
    (should= {"A" "ATCG" "B" "AAAA" "C" "CCCC"} (config :tags)))

  (it "structure a tag"
    (should= {:id "A" :tag "AGAA" :locations []} (structure-tag ["A" "AGAA"])))

  (it "structures all tags"
    (let [tags (structure-tags (:tags sample-config))]
      (should= (structure-tag ["A" "AGAA"]) (get tags "A"))
      (should= (structure-tag ["B" "ACTA"]) (get tags "B"))))

  (it "finds locations of tag"
    (should= [36] (find-locations "CCTG" (:sequence sample-config)))
    (should= [16 61] (find-locations "ACTA" (:sequence sample-config))))

  (it "populates all locations"
    (let [tags (structure-tags (:tags sample-config))
          tags (populate-locations tags (:sequence sample-config))]
      (should [36 (:locations (get "D" tags))])
      (should [[16 61] (:locations (get "B" tags))])))

  (it "finds solutions (42)"
    (let [solutions (find-solutions sample-config)]
      (prn "solutions: " solutions)
      (should= 1 (count solutions))
      (should-contain ["TATGCCGGCTGCGGAAACTAGCCCCGAGTTTTGAAACCTG"
                       "GAACGTCGCAAGCTGATGACCACTA"
                       "GGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAA"
                       "CGTTAACCAA"]
                      solutions)))

  (it "finds solutions (172)"
      (let [solutions (find-solutions (assoc sample-config :max-fragment-length 172))]
        (should= 12 (count solutions))
        (should-contain ["TATGCCGGCTGCGGAAACTAGCCCCGAGTTTTGAAACCTGGAACGTCGCAAGCTGATGACCACTAGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA"]
                        solutions)))

  )

(comment
  TATGCCGGCTGCGGAAacta
  actaGCCCCGAGTTTTGAAAcctgGAACGTCGCAAGCTGATGACCactaGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA

  TATGCCGGCTGCGGAAacta
  actaGCCCCGAGTTTTGAAAcctg
  cctgGAACGTCGCAAGCTGATGACCactaGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA

  TATGCCGGCTGCGGAAactaGCCCCGAGTTTTGAAAcctg
  cctgGAACGTCGCAAGCTGATGACCactaGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA

  TATGCCGGCTGCGGAAactaGCCCCGAGTTTTGAAAcctg
  cctgGAACGTCGCAAGCTGATGACCacta
  actaGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA

  TATGCCGGCTGCGGAAactaGCCCCGAGTTTTGAAAcctgGAACGTCGCAAGCTGATGACCacta
  actaGGTGTTAGCGAACATGCGGCGGCGGCGGTGGGTAAAGAACGTTAACCAA
  )