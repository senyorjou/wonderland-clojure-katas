(ns alphabet-cipher.coder)

(def abc "abcdefghijklmnopqrstuvwxyz")

(defn column [keyword pos]
  (- (int (first (drop pos (flatten (repeatedly #(seq keyword)))))) 97 ))


(defn ichar [c]
  (- (int c) 97))

(defn take-from-abc
  [secret pos chr]
  (let [delta (column secret pos)
        second (- (int chr) 97)
        letter (column abc (+ delta second))]
    (char (+ 97 letter))))

(defn take-for-abc
  [secret pos chr]
  (let [delta (column secret pos)
        second (mod (+ (- 26 delta) (ichar chr)) 26)]
    (char (+ 97 second))))

(defn encode [keyword message]
  (let [encode-for-kw (partial take-from-abc keyword)]
    (apply str (map-indexed encode-for-kw message))))

(defn decode [keyword message]
  (let [decode-for-kw (partial take-for-abc keyword)]
    (apply str (map-indexed decode-for-kw message))))




(defn decipher [cipher message]
  (let [key-stream (map (fn [a b]
                          (char (+ 97 (mod (- (ichar a) (ichar b)) 26))))
                        cipher message)]
    key-stream))
  ;; "decypherme")



(comment

  ;; (int \s) 115
  ;; (int \m) 109
  ;; (int \e) 101

  (column abc (+ (int \e) (- 26 18)))
  (char (+ (int \e) (- 26 18)))
  (char (+ (int \i) (- 26 2)))

  (char (+ 97 (mod (- (ichar \g) (ichar \e)) 26)))


  (apply str (map-indexed (fn [i v]
                            (take-for-abc "scones" i v))
                          "egsgqwtahuiljgs"))


  (take-for-abc "scones" 1 \g)

  (map (fn [a b] b) [1 2 3] [9 8 7])

 ;;   ABCDEFGHIJKLMNOPQRSTUVWXYZ
 ;; A abcdefghijklmnopqrstuvwxyz
 ;; B bcdefghijklmnopqrstuvwxyza
 ;; C cdefghijklmnopqrstuvwxyzab

;; sconessconessco
;; meetmebythetree
;; egsgqwtahuiljgs

  )
