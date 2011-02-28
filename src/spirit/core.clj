(ns spirit.core
  (:require
   [spirit.prequel :as pql]
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

;; Check for unique violations.
(defn create-user [username password email]
  (let [hashpass (BCrypt/hashpw password (BCrypt/gensalt 12))]
   (pql/insert
    db
    :users
    {:login username
     :email email
     :password hashpass})))

;; Need to build case insensitive indicies?
(defn get-user [username password]
  (let
      [user
       (pql/query-first-result
        db
        (select :* :from :users :where (= (lower :login) (lower username))))]
    (if user
      (if
          (BCrypt/checkpw password (user :password))
        user
        false)
      false)))

;(create-user "test" "testpw" "testemail@not.here")

(defn random-bytes [len]
  (let [rand (java.security.SecureRandom/getInstance "SHA1PRNG")
        buf (make-array Byte/TYPE len)]
    (.nextBytes rand buf)
    buf))

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
         db-null nil]
        (pql/insert
         db
         :sessions
         {:session_id session-id
          :user_id user-id
          :client_address client-address
          :expiration db-null})
        session-id))

(defn get-session [session-id]
  (pql/query-first-result
   db
   (select :* :from :sessions :where (= :session_id session-id))))

(defn get-user-from-session [session-id]
  (pql/query-first-result
   db
   (select :users.* :from :users \, :sessions :where
           (= :sessions.session_id session-id))))


;(get-user-from-session tmp)

;(def tmp (create-session 4 "127.0.0.1"))

;(generate-session-id 4 "127.0.0.1")

;(get-user "indy" "pass")


