(in-package :aura)

(defparameter *json* #p"/home/colin/code/rust/faur/packages-meta-ext-v1.json")

#+nil
(with-open-file (file "filenames.txt" :direction :output)
  (t:transduce
   (t:comp
    (t:map (fn (gethash "PackageBase" %)))
    #'t:unique
    (t:map (fn (format file "~a~%" %))))
   #'t:for-each
   (j:read *json*)))
