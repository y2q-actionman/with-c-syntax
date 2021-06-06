(in-package #:with-c-syntax.core)

;;; Our own input stream -- for translation phase 1, phase 2

(defvar *with-c-syntax-reader-process-trigraph* :auto
  "Determines whether #{ }# reader replaces C trigraphs.
 Replacement occurs if this is T, or :auto and reader level >= 2.")

(defvar *with-c-syntax-reader-process-backslash-newline* :auto
  "Determines #{ }# reader deletes backslash-newline sequence.
 Deletion occurs if this is T, or :auto and reader level >= 2.")

(defclass physical-source-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :initarg :stream
           :type input-stream
           :documentation "The parental stream.")
   ;; Internal buffers
   (unread-char :type (or null character)
                :initform nil
                :documentation "Used when `cl:unread-char' called.")
   (trigraph-keep-char :type (or null character)
                       :initform nil
                       :documentation "A buffer for treating consective trigraphs, like '???='. See `translation-early-phase'.")
   (newline-gap :type integer
                :initform 0
                :documentation "Counts deleted newlines for __LINE__ .")
   ;; switches
   (target-readtable :type readtable
                     :initarg :target-readtable)
   (process-phase-1 :type boolean
                    :initarg :phase-1 :initform t)
   (process-phase-2 :type boolean
                    :initarg :phase-2 :initform t))
  (:documentation "An input stream for doing translation phase 1
  (replacing trigraph) and translation phase 2 (deleting
  backslash-newline sequence)."))

(defun find-trigraph-character-by-last-character (char)
  (case char
    (#\= #\#)
    (#\( #\[)
    (#\/ #\\)
    (#\) #\])
    (#\' #\^)
    (#\< #\{)
    (#\! #\|)
    (#\> #\})
    (#\- #\~)
    (otherwise nil)))

(defmethod translation-early-phase ((cp-stream physical-source-input-stream))
  "Do translation phase 1 and 2, returns a character."
  (with-slots (stream trigraph-keep-char newline-gap
               process-phase-1 process-phase-2)
      cp-stream
    (let ((c (read-char stream nil :eof)))
      ;; Translation Phase 1 -- Trigraph
      (cond ((and process-phase-1
                  (eql #\? c))
             (when (and (null trigraph-keep-char)
                        (eql #\? (peek-char nil stream nil))) ; '??' appeared.
               (read-char stream)
               (shiftf trigraph-keep-char c #\?)) ; trigraph-keep-char == c == #\?
             (when (eql #\? trigraph-keep-char)
               (let* ((next (peek-char nil stream nil))
                      (replaced-char
                        (if next
                            (find-trigraph-character-by-last-character next))))
                 (cond (replaced-char
                        (read-char stream)
                        (setf c replaced-char ; Brings the replaced char to phase-2
                              trigraph-keep-char nil))
                       (t
                        ;; Like '??a' or '???='. For treating the latter, '?' is kept.
                        (assert (eql trigraph-keep-char #\?))
                        (return-from translation-early-phase #\?))))))
            (trigraph-keep-char
             (unread-char c stream)
             (shiftf c trigraph-keep-char nil)))
      ;; Translation Phase 2 -- Backslash newline
      (when (and process-phase-2
                 (eql #\\ c)
                 (eql #\newline (peek-char nil stream nil)))
        (read-char stream)              ; eat the newline.
        (incf newline-gap)
        ;; To get a new character, do itself again.
        (return-from translation-early-phase
          (translation-early-phase cp-stream)))
      ;; with-c-syntax specific. Adjusting new line gaps here.
      (cond ((and (plusp newline-gap)
                  (member c '(:eof #\newline)))
             (when (characterp c)
               (unread-char c stream))
             (decf newline-gap)
             (setf c #\newline)))
      c)))                              ; done

(defmethod trivial-gray-streams:stream-read-char ((cp-stream physical-source-input-stream))
  (with-slots (stream unread-char target-readtable
               newline-gap trigraph-keep-char)
      cp-stream
    (cond
      (unread-char
       (shiftf unread-char nil))
      ((not (eq *readtable* target-readtable))
       ;; In other readtable, C translations should be suppressed except counting newlines.
       (if trigraph-keep-char
           (shiftf trigraph-keep-char nil)
           (let ((char (read-char stream nil :eof)))
             (when (eql char #\newline)
               (incf newline-gap))
             char)))
      (t
       (translation-early-phase cp-stream)))))

(defmethod trivial-gray-streams:stream-peek-char ((cp-stream physical-source-input-stream))
  (with-slots (stream unread-char target-readtable
               newline-gap trigraph-keep-char)
      cp-stream
    (cond
      (unread-char
       unread-char)
      ((not (eq *readtable* target-readtable))
       (if trigraph-keep-char
           trigraph-keep-char
           (peek-char nil stream nil :eof)))
      (t
       (setf unread-char
             (translation-early-phase cp-stream))))))

(defmethod trivial-gray-streams:stream-unread-char ((cp-stream physical-source-input-stream)
                                                    character)
  (with-slots (unread-char) cp-stream
    (when unread-char
      (error 'with-c-syntax-reader-error
             :format-control "Unreading too many chars. (stored ~C, unreaded ~C)"
             :format-arguments (list unread-char character)))
    (setf unread-char character))
  nil)

;;; These are required for compiling our test codes in Allegro CL 10.1

(defmethod count-physical-source-buffered-char ((cp-stream physical-source-input-stream))
  (with-slots (stream unread-char trigraph-keep-char newline-gap)
      cp-stream
    (+ (if unread-char 1 0)
       (if trigraph-keep-char 1 0)
       newline-gap)))

(defmethod trivial-gray-streams:stream-file-position ((cp-stream physical-source-input-stream))
  (with-slots (stream) cp-stream
    (- (file-position stream)
       (count-physical-source-buffered-char cp-stream))))

(defmethod (setf trivial-gray-streams:stream-file-position)
    (value (cp-stream physical-source-input-stream))
  (with-slots (stream unread-char trigraph-keep-char newline-gap)
      cp-stream
    (file-position stream value)
    ;; flush buffers
    (setf unread-char nil
          trigraph-keep-char nil
          newline-gap 0)
    value))

#+allegro
(defmethod excl:file-character-position ((cp-stream physical-source-input-stream))
  (with-slots (stream) cp-stream
    (excl:file-character-position stream)))
