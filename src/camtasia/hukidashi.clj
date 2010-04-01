(ns camtasia.hukidashi
  (:require [clojure.zip :as z]
            [clojure.contrib.zip-filter :as zf]
            [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.contrib.str-utils :as s]
            [clojure.contrib.duck-streams :as io])
  (:gen-class))

(declare get-hukidashi get-overlay-array-objects get-start get-end
         get-sjis-string get-content parse-content re-map re-map-double
         get-byte-array remove-nils remove-empty-strings get-timing
         remove-consecutive-duplicates $1 get-clips get-clipid
         get-clip-map-index get-dshowcontrol-clipmap-objects
         get-media-start get-media-end)

(defn -main [filename]
  (let [zxml           (z/xml-zip (clojure.xml/parse (str filename ".camproj")))
        clips          (get-clips zxml)
        hukidashies    (get-hukidashi zxml clips)
        time-format    (java.text.DecimalFormat. "##.0")
        times          (distinct (map #(.format time-format
                                                (float (get-timing clips %))) hukidashies))
        movie-filename (str filename ".mov")
        output-format  (java.text.DecimalFormat. "000")
        filenames      (map #(. output-format format %)
                            (iterate inc 0))]

    (io/spit (str filename "output.txt")
             (s/str-join ""
                         (interleave
                          (repeat " ")
                          (map :content hukidashies)
                          ;; (repeat " ")
                          ;; hukidashies
                          (repeat "\n"))))

    (io/spit (str filename "capture.sh")
             (s/str-join ""
                         (interleave
                          (map #(str "ffmpeg -i " movie-filename ;input filename
                                     " -f image2 -ss " %1        ;time
                                     " " filename %2 ".png")     ;output filename
                               times
                               filenames)
                          (repeat "\n"))))))

(defn- get-hukidashi [zxml clips]
  (remove-consecutive-duplicates
   (remove-empty-strings
    (sort-by #(get-timing clips %)
             (map (fn [%]
                    {:start   (get-start %)
                     :end      (get-end %)  
                     :content  (get-content %)
                     :clipid   (get-clipid %) })
                  (get-overlay-array-objects zxml))))))

(defn- get-clips [zxml]
  (let [dscco (get-dshowcontrol-clipmap-objects zxml)]
    (apply hash-map
           (interleave
            (map get-clip-map-index dscco)
            (map (fn [a b c d] {:start a :end b :media-start c :media-end d})
                 (map get-start dscco)
                 (map get-end dscco)
                 (map get-media-start dscco)
                 (map get-media-end dscco))))))

(defn- get-overlay-array-objects [zxml]
  (zfx/xml-> zxml zf/children :Overlay_Array zf/children))

(defn- get-dshowcontrol-clipmap-objects [zxml]
  (zfx/xml-> zxml zf/children :DShowControl_ClipMap zf/children))

(defn- get-start [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :Start z/down z/node))))

(defn- get-end [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :End z/down z/node))))

(defn- get-media-start [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :MediaStart z/down z/node))))

(defn- get-media-end [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :MediaEnd z/down z/node))))

(defn- get-content [zxml]
  (parse-content (str (first (zfx/xml-> zxml zf/children :strOverlayRichText z/down z/node)))))

(defn- get-clipid [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :ClipID z/down z/node))))

(defn- get-clip-map-index [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :ClipMapIndex z/down z/node))))

(defn- get-timing [clips hukidashi] ;;camproj files are intricated
  (let [clipid (:clipid hukidashi)]
    (/ (+ (:start (clips clipid))
          (/ (+ (:start hukidashi) (:end hukidashi)) 2)
          (- (:media-start (clips clipid))))
       10000000)))

(defn- remove-empty-strings [coll]
  (filter #(not= (:content %) "") coll))

(defn- remove-consecutive-duplicates [coll]
  (map last
       (filter (fn [[a b]] (not= (:content a) (:content b)))
               (partition 2 1 (cons nil coll)))))

(defn- parse-content [content]
  (->> content
       (s/re-gsub #"\n+" " ")
       (s/re-gsub #"\\\{" "{")
       (s/re-gsub #"\\\}" "}")
       (s/re-gsub #"\\\?" "?")
       (re-find #"\\viewkind4(.+)}")
       $1
       (s/re-gsub #"\\[a-z0-9]+ ?" "")
       (re-map get-sjis-string #"(\\'[0-9a-f][0-9a-f])+")
       (s/str-join "")))

(defn- $1 [x] (if x
                (fnext x)
                ""))

(defn- get-sjis-string [str]
  (let [arr (remove-nils
             (re-map-double #(Integer/parseInt % 16)
                            (fn [%] nil)
                            #"[0-9a-f][0-9a-f]"
                            (first str)))]
    (String. (get-byte-array arr) "SJIS")))

(defn- get-byte-array [coll]
  (let [res (make-array Byte/TYPE (count coll))]
    (amap res idx ret
          (byte (nth coll idx)))))

(defn- remove-nils [coll]
  (filter identity coll))

(defn- re-map [f regex str]
  "apply f to the substrings regex matched"
  (let [rp (s/re-partition regex str)
        matches (map last (partition 2 rp))
        mismatches (map first (partition 1 2 rp))]
    (interleave mismatches
                (concat (map f matches) [""]))))

(defn- re-map-double [f1 f2 regex str]
  "apply f1 to the substrings regex matched
   apply f2 to the substrings regex did not match"
  (let [rp (s/re-partition regex str)
        matches (map last (partition 2 rp))
        mismatches (map first (partition 1 2 rp))]
    (interleave (map f2 mismatches)
                (concat (map f1 matches) [""]))))
