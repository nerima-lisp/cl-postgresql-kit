(in-package #:cl-postgresql-kit)

(defparameter *maximum-scram-iterations* 1000000
  "Maximum SCRAM PBKDF2 iteration count accepted by the client.")

(defun %crypto-octets (value)
  (cond ((stringp value)
         (cl-codec-kit:string-to-octets value :encoding :utf-8))
        ((typep value '(vector (unsigned-byte 8))) value)
        (t (coerce value '(vector (unsigned-byte 8))))))

(defun %crypto-concat (&rest vectors)
  (let ((result (make-array (reduce #'+ vectors :key #'length :initial-value 0)
                            :element-type '(unsigned-byte 8))))
    (loop with position = 0
          for vector in vectors
          do (replace result vector :start1 position)
             (incf position (length vector)))
    result))

(defun %u32 (value)
  (logand value #xffffffff))

(defun %rol32 (value count)
  (let ((value (%u32 value)))
    (%u32 (logior (ash value count)
                  (ash value (- count 32))))))

(defun %ror32 (value count)
  (let ((value (%u32 value)))
    (%u32 (logior (ash value (- count))
                  (ash value (- 32 count))))))

(defun %little-u64 (value)
  (let ((result (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for index from 0 below 8
          do (setf (aref result index) (ldb (byte 8 (* 8 index)) value)))
    result))

(defun %big-u64 (value)
  (let ((result (make-array 8 :element-type '(unsigned-byte 8))))
    (loop for index from 0 below 8
          do (setf (aref result index)
                   (ldb (byte 8 (* 8 (- 7 index))) value)))
    result))

(defun %padded-message (octets &key little-endian)
  (let* ((length (length octets))
         (padding (mod (- 56 (1+ length)) 64))
         (result (make-array (+ length 1 padding 8)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (replace result octets)
    (setf (aref result length) #x80)
    (replace result (if little-endian
                       (%little-u64 (* length 8))
                       (%big-u64 (* length 8)))
             :start1 (+ length 1 padding))
    result))

(defparameter +md5-shifts+
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22
    5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
    4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23
    6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))

(defparameter +md5-table+
  #( #xd76aa478 #xe8c7b756 #x242070db #xc1bdceee
     #xf57c0faf #x4787c62a #xa8304613 #xfd469501
     #x698098d8 #x8b44f7af #xffff5bb1 #x895cd7be
     #x6b901122 #xfd987193 #xa679438e #x49b40821
     #xf61e2562 #xc040b340 #x265e5a51 #xe9b6c7aa
     #xd62f105d #x02441453 #xd8a1e681 #xe7d3fbc8
     #x21e1cde6 #xc33707d6 #xf4d50d87 #x455a14ed
     #xa9e3e905 #xfcefa3f8 #x676f02d9 #x8d2a4c8a
     #xfffa3942 #x8771f681 #x6d9d6122 #xfde5380c
     #xa4beea44 #x4bdecfa9 #xf6bb4b60 #xbebfbc70
     #x289b7ec6 #xeaa127fa #xd4ef3085 #x04881d05
     #xd9d4d039 #xe6db99e5 #x1fa27cf8 #xc4ac5665
     #xf4292244 #x432aff97 #xab9423a7 #xfc93a039
     #x655b59c3 #x8f0ccc92 #xffeff47d #x85845dd1
     #x6fa87e4f #xfe2ce6e0 #xa3014314 #x4e0811a1
     #xf7537e82 #xbd3af235 #x2ad7d2bb #xeb86d391))

(defun %md5-word (octets position)
  (+ (aref octets position)
     (ash (aref octets (+ position 1)) 8)
     (ash (aref octets (+ position 2)) 16)
     (ash (aref octets (+ position 3)) 24)))

(defun md5-digest (value)
  "Return the MD5 digest of VALUE as 16 octets."
  (let ((message (%padded-message (%crypto-octets value) :little-endian t))
        (a #x67452301)
        (b #xefcdab89)
        (c #x98badcfe)
        (d #x10325476))
    (loop for chunk from 0 below (length message) by 64
          do (let ((aa a) (bb b) (cc c) (dd d)
                   (words (make-array 16)))
               (loop for index from 0 below 16
                     do (setf (aref words index)
                              (%u32 (%md5-word message (+ chunk (* 4 index))))))
               (loop for index from 0 below 64
                     do (let* ((f (cond ((< index 16)
                                         (logior (logand b c)
                                                 (logand (lognot b) d)))
                                        ((< index 32)
                                         (logior (logand d b)
                                                 (logand (lognot d) c)))
                                        ((< index 48) (logxor b c d))
                                        (t (logxor c (logior b (lognot d))))))
                                (g (cond ((< index 16) index)
                                         ((< index 32) (mod (+ (* 5 index) 1) 16))
                                         ((< index 48) (mod (+ (* 3 index) 5) 16))
                                         (t (mod (* 7 index) 16))))
                                (next (%u32 (+ a f (aref +md5-table+ index)
                                               (aref words g)))))
                           (setf a d
                                 d c
                                 c b
                                 b (%u32 (+ b (%rol32 next (aref +md5-shifts+ index)))))))
               (setf a (%u32 (+ a aa))
                     b (%u32 (+ b bb))
                     c (%u32 (+ c cc))
                     d (%u32 (+ d dd)))))
    (let ((result (make-array 16 :element-type '(unsigned-byte 8))))
      (loop for word in (list a b c d)
            for base from 0 by 4
            do (loop for offset from 0 below 4
                     do (setf (aref result (+ base offset))
                              (ldb (byte 8 (* 8 offset)) word))))
      result)))

(defparameter +sha256-k+
  #( #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
     #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
     #xd807aa98 #x12835b01 #x243185be #x550c7dc3
     #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
     #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
     #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
     #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
     #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
     #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
     #x650a7354 #x766a0abb #x81c2c92e #x92722c85
     #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
     #xd192e819 #xd6990624 #xf40e3585 #x106aa070
     #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
     #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
     #x748f82ee #x78a5636f #x84c87814 #x8cc70208
     #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

(defun %sha256-word (octets position)
  (+ (ash (aref octets position) 24)
     (ash (aref octets (+ position 1)) 16)
     (ash (aref octets (+ position 2)) 8)
     (aref octets (+ position 3))))

(defun %sha256-sigma0 (value)
  (logxor (%ror32 value 2) (%ror32 value 13) (%ror32 value 22)))

(defun %sha256-sigma1 (value)
  (logxor (%ror32 value 6) (%ror32 value 11) (%ror32 value 25)))

(defun %sha256-gamma0 (value)
  (logxor (%ror32 value 7) (%ror32 value 18) (ash (%u32 value) -3)))

(defun %sha256-gamma1 (value)
  (logxor (%ror32 value 17) (%ror32 value 19) (ash (%u32 value) -10)))

(defun sha256-digest (value)
  "Return the SHA-256 digest of VALUE as 32 octets."
  (let ((message (%padded-message (%crypto-octets value)))
        (hash (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                     #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
    (loop for chunk from 0 below (length message) by 64
          do (let ((schedule (make-array 64 :initial-element 0)))
               (loop for index from 0 below 16
                     do (setf (aref schedule index)
                              (%u32 (%sha256-word message (+ chunk (* 4 index))))))
               (loop for index from 16 below 64
                     do (setf (aref schedule index)
                              (%u32 (+ (%sha256-gamma1 (aref schedule (- index 2)))
                                       (aref schedule (- index 7))
                                       (%sha256-gamma0 (aref schedule (- index 15)))
                                       (aref schedule (- index 16))))))
               (let ((a (aref hash 0)) (b (aref hash 1))
                     (c (aref hash 2)) (d (aref hash 3))
                     (e (aref hash 4)) (f (aref hash 5))
                     (g (aref hash 6)) (h (aref hash 7)))
                 (loop for index from 0 below 64
                       do (let* ((ch (logxor (logand e f)
                                             (logand (lognot e) g)))
                                 (maj (logxor (logand a b)
                                              (logand a c)
                                              (logand b c)))
                                 (t1 (%u32 (+ h (%sha256-sigma1 e) ch
                                             (aref +sha256-k+ index)
                                             (aref schedule index))))
                                 (t2 (%u32 (+ (%sha256-sigma0 a) maj))))
                            (setf h g
                                  g f
                                  f e
                                  e (%u32 (+ d t1))
                                  d c
                                  c b
                                  b a
                                  a (%u32 (+ t1 t2)))))
                 (loop for index from 0 below 8
                       do (setf (aref hash index)
                                (%u32 (+ (aref hash index)
                                         (nth index (list a b c d e f g h)))))))))
    (let ((result (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for index from 0 below 8
            for word = (aref hash index)
            for base from 0 by 4
            do (loop for offset from 0 below 4
                     do (setf (aref result (+ base offset))
                              (ldb (byte 8 (* 8 (- 3 offset))) word))))
      result)))

(defun hmac-sha256 (key message)
  "Return HMAC-SHA-256 for KEY and MESSAGE as 32 octets."
  (let* ((raw-key (%crypto-octets key))
         (normalized-key (if (> (length raw-key) 64)
                             (sha256-digest raw-key)
                             raw-key))
         (padded-key (make-array 64 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    (replace padded-key normalized-key)
    (let ((inner (make-array 64 :element-type '(unsigned-byte 8)))
          (outer (make-array 64 :element-type '(unsigned-byte 8))))
      (loop for index from 0 below 64
            do (setf (aref inner index) (logxor (aref padded-key index) #x36)
                     (aref outer index) (logxor (aref padded-key index) #x5c)))
      (sha256-digest (%crypto-concat outer
                                     (sha256-digest (%crypto-concat inner
                                                                   (%crypto-octets message))))))))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defun octets-to-base64 (value)
  "Encode VALUE as RFC 4648 base64."
  (let* ((octets (%crypto-octets value))
         (result (make-string (* 4 (ceiling (length octets) 3))
                              :initial-element #\=))
         (position 0))
    (loop for index from 0 below (length octets) by 3
          for remaining = (- (length octets) index)
          for first = (aref octets index)
          for second = (if (> remaining 1) (aref octets (+ index 1)) 0)
          for third = (if (> remaining 2) (aref octets (+ index 2)) 0)
          for block = (logior (ash first 16) (ash second 8) third)
          do (setf (char result position)
                   (char +base64-alphabet+ (ldb (byte 6 18) block))
                   (char result (+ position 1))
                   (char +base64-alphabet+ (ldb (byte 6 12) block)))
             (when (> remaining 1)
               (setf (char result (+ position 2))
                     (char +base64-alphabet+ (ldb (byte 6 6) block))))
             (when (> remaining 2)
               (setf (char result (+ position 3))
                     (char +base64-alphabet+ (ldb (byte 6 0) block))))
             (incf position 4))
    result))

(defun %base64-value (character)
  (or (position character +base64-alphabet+ :test #'char=)
      (error 'parameter-error :message "Invalid base64 character"
             :parameter character)))

(defun base64-to-octets (string)
  "Decode padded or unpadded RFC 4648 base64 into octets."
  (unless (stringp string)
    (error 'parameter-error :message "Base64 input must be a string"
           :parameter string))
  (let* ((clean (remove-if (lambda (character)
                             (member character '(#\Space #\Tab #\Newline #\Return)))
                           string))
         (padding-position (position #\= clean))
         (padding-present-p (not (null padding-position)))
         (data (if padding-present-p
                   (subseq clean 0 padding-position)
                   clean))
         (padding (if padding-present-p
                      (- (length clean) padding-position)
                      0))
         (data-length (length data))
         (data-remainder (mod data-length 4))
         (valid-padding-p
          (or (not padding-present-p)
              (and (<= padding 2)
                   (zerop (mod (length clean) 4))
                   (every (lambda (character) (char= character #\=))
                          (subseq clean padding-position))
                   (or (and (= data-remainder 0) (zerop padding))
                       (and (= data-remainder 2) (= padding 2))
                       (and (= data-remainder 3) (= padding 1)))))))
    (when (or (= data-remainder 1)
              (not valid-padding-p))
      (error 'parameter-error :message "Invalid base64 padding" :parameter string))
    (let ((result (make-array (* 3 (ceiling (length data) 4))
                              :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
      (loop for index from 0 below data-length by 4
            for remaining = (- data-length index)
            for a = (%base64-value (char data index))
            for b = (%base64-value (char data (+ index 1)))
            for c = (if (> remaining 2) (%base64-value (char data (+ index 2))) 0)
            for d = (if (> remaining 3) (%base64-value (char data (+ index 3))) 0)
            for block = (logior (ash a 18) (ash b 12) (ash c 6) d)
            do (vector-push-extend (ldb (byte 8 16) block) result)
               (when (> remaining 2)
                 (vector-push-extend (ldb (byte 8 8) block) result))
               (when (> remaining 3)
                 (vector-push-extend (ldb (byte 8 0) block) result)))
      (coerce result '(vector (unsigned-byte 8))))))

(defun scram-hi (password salt iterations)
  "Compute SCRAM's PBKDF2-HMAC-SHA-256 salted password."
  (unless (and (integerp iterations)
               (integerp *maximum-scram-iterations*)
               (<= 1 iterations *maximum-scram-iterations*))
    (error 'parameter-error :message "SCRAM iteration count is invalid or exceeds the configured maximum"
           :parameter iterations))
  (let* ((salt (%crypto-octets salt))
         (password (%crypto-octets password))
         (block (hmac-sha256 password (%crypto-concat salt #(0 0 0 1))))
         (result (copy-seq block)))
    (loop for count from 2 to iterations
          for current = (hmac-sha256 password block)
          do (setf block current)
             (loop for index from 0 below (length result)
                   do (setf (aref result index)
                            (logxor (aref result index) (aref current index)))))
    result))
