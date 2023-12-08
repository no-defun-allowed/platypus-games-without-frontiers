(in-package :platypus-magic)

(defvar *state*)
(defstruct (state (:constructor %make-state))
  lock
  database
  bitmap
  (cursor 0)
  (chunk-size 0 :read-only t)
  (count 0 :read-only t)
  machines
  machine-data
  weights)

(defvar *telemetry* (make-hash-table :test 'equal))

(defun make-state (database-name machines weights &key (chunk-size 128))
  (let* ((database (sqlite:connect database-name))
         (machine-data (numpy-file-format:load-array machines))
         (machine-count (length machine-data))
         (bitmap (make-array (ceiling machine-count chunk-size)
                             :element-type 'bit :initial-element 0)))
    (sqlite:execute-non-query database "CREATE TABLE IF NOT EXISTS platypus (id INTEGER PRIMARY KEY, wins INTEGER NOT NULL, points INTEGER NOT NULL)")
    ;; Create a bitmap
    (loop with statement = (sqlite:prepare-statement database "SELECT id FROM platypus")
          while (sqlite:step-statement statement)
          do (setf (bit bitmap (floor (sqlite:statement-column-value statement 0) chunk-size)) 1)
          finally (sqlite:finalize-statement statement))
    (%make-state
     :lock (bt:make-lock)
     :database database
     :bitmap bitmap
     :chunk-size chunk-size
     :count machine-count
     :machines machines
     :machine-data machine-data
     :weights weights)))

(defun next-work (state)
  (let ((attempt (or (position 0 (state-bitmap state) :start (state-cursor state))
                     (position 0 (state-bitmap state) :end (state-cursor state)))))
    (unless (null attempt) (setf (state-cursor state) (1+ attempt)))
    attempt))

(defun store-work (state id wins points)
  (sqlite:execute-non-query (state-database state)
                            "INSERT INTO platypus (id, wins, points) VALUES (?, ?, ?)"
                            id wins points)
  (setf (bit (state-bitmap state) (floor id (state-chunk-size state))) 1))

(hunchentoot:define-easy-handler (handle-work :uri "/work") ()
  (let ((work (next-work *state*)))
    (cond
      ((null work)
       (setf (hunchentoot:return-code*) 404)
       "All done!")
      (t (shasht:write-json
          `(:object-alist ("index" . ,work) ("size" . ,(state-chunk-size *state*)))
          nil)))))

(hunchentoot:define-easy-handler (handle-store :uri "/store") ()
  (let ((data (shasht:read-json (hunchentoot:raw-post-data :force-text t))))
    (bt:with-lock-held ((state-lock *state*))
      (sqlite:with-transaction (state-database *state*)
        (loop for result across (gethash "results" data)
              do (store-work *state*
                             (gethash "id" result)
                             (gethash "wins" result)
                             (gethash "points" result)))))
    (unless (null (gethash "name" data))
      (let ((telemetry (gethash "telemetry" data)))
        (setf (gethash "time" telemetry) (get-internal-real-time)
              (gethash (gethash "name" data) *telemetry*) telemetry)))
    "ok"))

;; TODO: bzip2 gets about a 5:1 compression ratio on machines.
(hunchentoot:define-easy-handler (handle-machines :uri "/machines") ()
  (hunchentoot:handle-static-file (state-machines *state*)))
(hunchentoot:define-easy-handler (handle-weights :uri "/weights") ()
  (hunchentoot:handle-static-file (state-weights *state*)))

(defun start (state)
  (setf *state* state)
  (hunchentoot:start
   (make-instance 'hunchentoot:easy-acceptor
     :port 8080
     :access-log-destination nil)))
