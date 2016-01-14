(ns reflection-test.core)

(defn to-reader [f]
  (cond
    (string? f)
    (recur (java.io.File. f))
    (instance? java.net.URL f)
    (recur (.getFile f))
    (instance? java.io.File f)
    (recur (java.io.FileReader. f))
    (instance? java.io.FileReader f)
    (java.io.BufferedReader. f)
    :else
    (throw (ex-info "I don't know what to do with this." {:value f}))))

(defn to-writer [f]
  (cond
    (string? f)
    (recur (java.io.File. f))
    (instance? java.net.URL f)
    (recur (.getFile f))
    (instance? java.io.File f)
    (recur (java.io.FileWriter. f))
    (instance? java.io.FileWriter f)
    (java.io.BufferedWriter. f)
    :else
    (throw (ex-info "I don't know what to do with this." {:value f}))))

(defn my-slurp [f]
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
    (map #(vec (.split % ",")) lines)))

(defn output-csv [records filename]
  (with-open [wtr (to-writer filename)]
    (doseq [record records]
      (let [col (first record)]
        (.write wtr col))
      (doseq [column (rest record)]
        (.write wtr ",")
        (.write wtr column))
      (.write wtr "\n"))))
