(ns p-p-p-pokerface)

(def rank-map {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn suit [card]
  (str (second card)))

(defn rank [card]
  (let [r (first card)]
    (cond
      (Character/isDigit r) (Integer/valueOf (str r))
      :else (get rank-map r))))

(defn n-kind? [hand n suit-or-rank]
  (true? (some (fn [x] (= x n))
               (vals (frequencies (map suit-or-rank hand))))))

(defn high-card? [hand]
  true) ; All hands have a high card.

(defn pair? [hand]
  (n-kind? hand 2 rank))

(defn three-of-a-kind? [hand]
  (n-kind? hand 3 rank))

(defn four-of-a-kind? [hand]
  (n-kind? hand 4 rank))

(defn flush? [hand]
  (n-kind? hand 5 suit))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (or (four-of-a-kind? hand)
      (= (count (filter (fn [x] (= x 2)) (vals (frequencies (map rank hand)))))
         2)))

(defn contains-straight? [ranks]
    (= ranks
       (range (nth ranks 0) (+ (nth ranks 4) 1))))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))]
    (or (contains-straight? ranks)
        (contains-straight? (sort (replace {14 1} ranks))))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (let [applicable-hands (filter (fn [x] (apply (first x) [hand])) checkers)]
      (apply max (map second applicable-hands)))))
