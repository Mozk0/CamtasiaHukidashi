(ns camtasia.hukidashi
  (:require [clojure.zip :as z]
            [clojure.contrib.zip-filter :as zf]
            [clojure.contrib.zip-filter.xml :as zfx]
            [clojure.contrib.str-utils :as s]
            [clojure.contrib.duck-streams :as io])
  (:gen-class))

(declare get-serif
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
                        (get-serif filename)
                        (repeat "\n")))))

(defn- get-serif [filename]
  (let [zxml (z/xml-zip (clojure.xml/parse filename))]
    (map parse-content
         (sort-by :start
                  (map (fn [%] {:start (get-start %), :content (get-content %)})
                       (get-overlay-array-objects zxml))))))

(defn- get-overlay-array-objects [zxml]
  (zfx/xml-> zxml zf/children :Overlay_Array zf/children))

(defn- get-start [zxml]
  (BigInteger.
   (first
    (zfx/xml-> zxml zf/children :Start z/down z/node))))

(defn- get-content [zxml]
  (str
   (first
    (zfx/xml-> zxml zf/children :strOverlayRichText z/down z/node))))

(def $1 fnext) ;;; accessor

(defn- parse-content [content]
  (let [c (->> content
               :content
               (s/re-gsub #"\n+" " ")
               (s/re-gsub #"\\\{" "{")
               (s/re-gsub #"\\\}" "}")
               (s/re-gsub #"\\\?" "?"))
        serif ($1 (re-find #"\\viewkind4(.+)}" c))]
    (s/str-join ""
                (re-map get-sjis-string
                        #"(\\'[0-9a-f][0-9a-f])+"
                        (s/re-gsub #"\\[a-z0-9]+ ?" "" serif)))))

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

(defn- re-map [fn regex str]
  "apply fn to the substrings regex matched"
  (let [rp (s/re-partition regex str)
        matches (map last (partition 2 rp))
        mismatches (map first (partition 1 2 rp))]
    (interleave mismatches
                (concat (map fn matches) [""]))))

(defn- re-map-double [fn1 fn2 regex str]
  "apply fn1 to the substrings regex matched
   apply fn2 to the substrings regex did not match"
  (let [rp (s/re-partition regex str)
        matches (map last (partition 2 rp))
        mismatches (map first (partition 1 2 rp))]
    (interleave (map fn2 mismatches)
                (concat (map fn1 matches) [""]))))
