(ns reflection-test.core)

(defn to-reader ^java.io.BufferedReader [f]
  (cond
    (string? f)
    (let [^String s f]
      (recur (java.io.File. s)))
    (instance? java.net.URL f)
    (let [^java.net.URL u f]
      (recur (.getFile u)))
    (instance? java.io.File f)
    (let [^java.io.File f f]
      (recur (java.io.FileReader. f)))
    (instance? java.io.FileReader f)
    (java.io.BufferedReader. f)
    :else
    (throw (ex-info "I don't know what to do with this." {:value f}))))

(defn to-writer ^java.io.BufferedWriter [f]
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

(defn my-slurp ^String [f]
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
