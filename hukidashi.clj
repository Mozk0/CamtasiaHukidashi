(ns camtasia.hukidashi
  (:require [clojure.zip :as z]
            [clojure.contrib.zip-filter :as zf]
            [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.contrib.str-utils :as s]
            [clojure.contrib.duck-streams :as io])
  (:gen-class))

(declare get-hukidashi
         get-overlay-array-objects
         get-start
         get-sjis-string
         get-content
         parse-content
         re-map
         re-map-double)

(defn -main [filename]
  (io/spit "output.txt"
           (s/str-join ""
                       (interleave
                        (repeat " ")
                        (get-hukidashi filename)
                        (repeat "\n")))))

(defn- get-hukidashi [filename]
  (let [zxml (z/xml-zip (clojure.xml/parse filename))]
    (filter #(not= % "")
            (map parse-content
                 (sort-by :start
                          (map (fn [%]  {:start (get-start %), :content (get-content %)})
                               (get-overlay-array-objects zxml)))))))

(defn- get-overlay-array-objects [zxml]
  (zfx/xml-> zxml zf/children :Overlay_Array zf/children))

(defn- get-start [zxml]
  (BigInteger. (first (zfx/xml-> zxml zf/children :Start z/down z/node))))

(defn- get-content [zxml]
  (str (first (zfx/xml-> zxml zf/children :strOverlayRichText z/down z/node))))

(defn $1 [x] (if x
               (fnext x)
               ""))

(defn- parse-content [content]
  (->> content
       :content
       (s/re-gsub #"\n+" " ")
       (s/re-gsub #"\\\{" "{")
       (s/re-gsub #"\\\}" "}")
       (s/re-gsub #"\\\?" "?")
       (re-find #"\\viewkind4(.+)}")
       $1
       (s/re-gsub #"\\[a-z0-9]+ ?" "")
       (re-map get-sjis-string #"(\\'[0-9a-f][0-9a-f])+")
       (s/str-join "")))

(defn- get-sjis-string [str]
  (let [arr (filter identity ;;; remove nils
                    (re-map-double #(Integer/parseInt % 16)
                                   (fn [%] nil)
                                   #"[0-9a-f][0-9a-f]"
                                   (first str)))
        len  (count arr)
        arr2 (make-array Byte/TYPE len)]
    (String.
     (amap arr2 idx ret
           (byte (nth arr idx)))
     "SJIS")))

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
