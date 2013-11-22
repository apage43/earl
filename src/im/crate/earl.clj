(ns im.crate.earl
  (:import [java.net URI]
           [com.ericsson.otp.erlang
            OtpErlangAtom OtpErlangBinary
            OtpErlangList OtpErlangString
            OtpErlangLong OtpErlangTuple
            OtpErlangDouble
            OtpErlangObject
            OtpPeer OtpConnection OtpSelf]
           )
  (:require [clj-http.client :as http]
            [clojure.walk]
            [lonocloud.synthread :as ->]))

(def cburl "http://localhost:8091/")

(defn get-cb-erlcreds [cburl user pass]
  (let [defpool (str (.resolve (URI. cburl) "/pools/default"))]
    (some-> (http/get defpool {:basic-auth [user pass] :as :json})
            :body :nodes (->/each (select-keys [:otpCookie :otpNode])))))

(defn erl-self [cookie nodename]
  (OtpSelf. nodename cookie))

(defn erl-connect [^OtpSelf self nodename]
  (.connect self (OtpPeer. nodename)))

(comment
  (def cburl "http://localhost:8091/")
  (def cburl "http://172.23.96.14:8091/")
  (def cburl "http://10.4.2.4:8091/")
  (def cburl "http://mango:8091/")
  (def cbcreds (get-cb-erlcreds cburl "Administrator" "asdasd"))
  (def cbcreds (get-cb-erlcreds cburl "Administrator" "password"))
  (def self (some-> cbcreds first :otpCookie (erl-self "earl@127.0.0.1")))
  (def self (some-> cbcreds first :otpCookie (erl-self "earl@10.17.14.53")))
  (def lconn (erl-connect self (-> cbcreds first :otpNode)))
  (do lconn)
  )


(defn b "short for .getBytes" [^String s]
  (.getBytes s))

(def byte-array-class (class (byte-array 0)))

(defn erl->clj [o]
  (cond
    (instance? OtpErlangAtom o)
    (keyword (.atomValue o))

    (instance? OtpErlangBinary o)
    (.binaryValue o)

    (instance? OtpErlangString o)
    (.stringValue o)

    (instance? OtpErlangList o)
    (mapv erl->clj (iterator-seq (.iterator o)))

    (instance? OtpErlangTuple o)
    (with-meta (mapv erl->clj (.elements o))
               {:tuple true})

    (instance? OtpErlangLong o)
    (.bigIntegerValue o)

    (instance? OtpErlangDouble o)
    (.doubleValue o)

    :else o))

(declare clj->erl)
(defn erl-array [elist]
  (into-array OtpErlangObject (mapv clj->erl elist)))

(defn clj->erl [o]
  (cond
    (instance? OtpErlangObject o) o

    (instance? byte-array-class o) (OtpErlangBinary. 0)

    (map? o) (clj->erl (map (fn [e] (with-meta [(key e) (val e)]
                                               {:tuple true})) o))

    (string? o) (OtpErlangString. o)

    (:tuple (meta o)) (OtpErlangTuple. (erl-array o))
    (coll? o) (OtpErlangList. (erl-array o))

    (integer? o) (OtpErlangLong. (biginteger o))
    (float? o) (OtpErlangDouble. (double o))

    (keyword? o) (OtpErlangAtom. (name o))


    :else (throw (ex-info "Can't clj->erl" {:object o}))
    ))

(defn erl-rpc [^OtpConnection conn m f & args]
  (.sendRPC conn (name m) (name f)
            (into-array OtpErlangObject (mapv clj->erl args)))
  (.receiveRPC conn))

(defn erl-eval [^OtpConnection conn expr]
  (erl-rpc conn :eshell :eval expr []))

(comment
  (.sendRPC lconn "erlang" "nodes" (into-array OtpErlangObject [])))

(defn proplist->map [m]
  (into {} m))

(defn propwalk
  "Walk a structure and convert any lists consisting of 2-tuples
   into maps."
  [m]
  (clojure.walk/postwalk
    (fn [i]
      (if
        (and (coll? i)
             (pos? (count i))
             (every? (fn [e] (and (:tuple (meta e))
                                  (= 2 (count e))))
                     i))
        (proplist->map i)
        i)) m))

(defn get-mod-funs [conn m]
  (doseq [[fun arity]
          (-> (erl-rpc conn m :module_info)
              erl->clj
              proplist->map
              :exports)]
    (println (str (name fun) "/" arity))))

(defn humanize-bytes [s]
  (cond
    (< s 10) (str s)
    :else (let [base 1000
                e (int (Math/floor (/ (Math/log s)
                                      (Math/log base))))
                v (/ s (Math/pow base e))
                fstr (if (< v 10) "%.1f" "%.0f")]
            (str (format fstr v) (get ["B" "KB" "MB" "GB" "TB" "PB" "EB"] e "!!?")))))

(comment
  (humanize-bytes 3000)
  ;; get word size from erlang vm
  (def wordsize (erl->clj (erl-rpc lconn :erlang :system_info :wordsize)))
  (defn totalheap [procs] (reduce + (map #(:total_heap_size % 0) procs)))
  (defn totalreductions [procs] (reduce + (map #(:reductions % 0) procs)))
  (def eprocs (-> (erl-eval lconn "[erlang:process_info(P) || P <- erlang:processes()]") erl->clj propwalk second))
  (def idle-gen-servers (filter #(= [:gen_server :loop 6] (:current_function %)) eprocs))
  (def hibernated ((group-by :current_function eprocs) [:erlang :hibernate 3]))
  (defn kw-or-nil [k?] (when (keyword? k?) k?))
  (defn sbsup-bucket [proc]
    (-> proc :dictionary :$ancestors first name (subs 18)))
  (def sbsup (filter
               (fn [proc]
                 (some-> proc :dictionary :$ancestors first
                         kw-or-nil name (.startsWith "single_bucket_sup-")))
               eprocs))
  (use 'clojure.pprint)
  (defn top-procs-by [eprocs nbr cat orderby]
    (->> eprocs
         (group-by cat)
         (sort-by (comp orderby val))
         (take-last nbr)
         (map (fn [[k v]]
                (let [bytes-size (* wordsize (reduce + (map #(:total_heap_size % 0) v)))]
                {:group (pr-str k)
                 :avg-size-per (humanize-bytes (long (/ bytes-size (count v))))
                 :reds (totalreductions v)
                 :count (count v) :heap-size (humanize-bytes bytes-size)})))
         (print-table)))

  )
