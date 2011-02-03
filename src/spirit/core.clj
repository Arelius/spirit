(ns spirit.core
  (:require
   [clojureql.core :as cql]
   [clojure.contrib.sql :as sql]
   [spirit.config :as conf])
  (:import
   jBCrypt.BCrypt))

(def db
  {:classname "org.postgresql.Driver"
   :subprotocol "postgresql"
   :subname "torrent"
   :user "spirit"
   :password conf/db-password})

(first (deref (cql/table db :users)))

;; Check for unique violations.
(defn create-user [username password email]
  (let [user
        {:login username
         :email email
         :password (BCrypt/hashpw password (BCrypt/gensalt 12))}]
   (cql/conj!
    (cql/table db :users)
    user)
   user))

;; Need to build case insensitive indicies?
(defn get-user [username password]
  (let
      [user
       (deref (cql/select (cql/table db :users)
                          (cql/where (= (:lower :login) (:lower username)))))]
    (if (empty? user)
      false
      (if
          (BCrypt/checkpw password ((first user) :password))
        (first user)
        false))))

(defn random-bytes [len]
  (let [rand (java.security.SecureRandom/getInstance "SHA1PRNG")
        buf (make-array Byte/TYPE len)]
    (.nextBytes rand buf)
    buf))

(bytes-to-hex-string (random-bytes 10))

(defn bytes-to-hex-string [ary]
  (apply str (map (fn [n] (let [ret (Integer/toHexString (bit-and n 0xff))]
                            (if (= (.length ret) 1)
                              (str "0" ret)
                              ret))) ary)))

(defn integer-to-byte-array [num]
  (byte-array
   [(byte (bit-and (bit-shift-right num 24) 0xff))
    (byte (bit-and (bit-shift-right num 16) 0xff))
    (byte (bit-and (bit-shift-right num 8) 0xff))
    (byte (bit-and (bit-shift-right num 0) 0xff))]))

;; The security of this function needs to be gone over, Do we even have enough bits of entropy?
(defn generate-session-id [user-id client-address]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (.update digest (integer-to-byte-array user-id))
    (.update digest (.getBytes client-address))
    (.update digest (random-bytes 20))
    (bytes-to-hex-string (.digest digest))))

(defn create-session [user-id client-address]
  (let* [session-id (generate-session-id user-id client-address)
         session
         {:session_id
          session-id
          :user_id user-id
          :client_address client-address
          :expiration nil}]
   (cql/conj!
    (cql/table db :sessions)
    session)
   session-id))

(defn get-session [session-id]
  (let
      [session
       (deref (cql/select (cql/table db :sessions)
                          (cql/where (= :session_id session-id))))]
    (if (empty? session)
      false
      (first session))))

(defn get-user-from-session [session-id]
  (let
      [user
       (deref (cql/select (cql/join (cql/table db :users)
                                    (cql/project (cql/table db :sessions) [])
                                    (cql/where (= :users.id :sessions.user_id)))
                          (cql/where (= :sessions.session_id
                                        session-id))))]
    (if (empty? user)
      false
      (first user))))

(get-user-from-session tmp)

(get-session tmp)

(def tmp (create-session 4 "127.0.0.1"))

(generate-session-id 4 "127.0.0.1")

(get-user "indy" "pass")


