(ns reflection-test.core)

(defn ^java.io.BufferedReader to-reader [f]
  (cond
    (string? f)
    (recur (java.io.File. ^String f))
    (instance? java.net.URL f)
    (recur (.getFile ^java.net.URL f))
    (instance? java.io.File f)
    (let [^java.io.File f f]
      (recur (java.io.FileReader. f)))
    (instance? java.io.FileReader f)
    (java.io.BufferedReader. f)
    :else
    (throw (ex-info "I don't know what to do with this." {:value f}))))

(defn ^java.io.BufferedWriter to-writer [f]
  (cond
    (string? f)
    (let [^String s f]
      (recur (java.io.File. s)))
    (instance? java.net.URL f)
    (let [^java.net.URL u f]
      (recur (.getFile u)))
    (instance? java.io.File f)
    (let [^java.io.File f f]
      (recur (java.io.FileWriter. f)))
    (instance? java.io.FileWriter f)
    (java.io.BufferedWriter. f)
    :else
    (throw (ex-info "I don't know what to do with this." {:value f}))))

(defn ^String my-slurp [f]
  (with-open [rdr (to-reader f)]
    (let [out (StringBuffer.)]
      (loop []
        (when-let [line (.readLine rdr)]
          (.append out line)
          (.append out "\n")
          (recur)))
      (.toString out))))

(defn parse-csv [filename]
  (let [string (my-slurp filename)
        lines (.split string "\n")]
    (map (fn [^String s] (vec (.split s ","))) lines)))

(defn output-csv [records filename]
  (with-open [wtr (to-writer filename)]
    (doseq [record records]
      (let [^String col (first record)]
        (.write wtr col))
      (doseq [^String column (rest record)]
        (.write wtr ",")
        (.write wtr column))
      (.write wtr "\n"))))
