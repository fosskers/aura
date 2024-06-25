(in-package :aura)

;; Alter these filepaths as necessary.
(defparameter *english* #p"/home/colin/code/haskell/aura/rust/aura-pm/i18n/en-US/aura_pm.ftl")
(defparameter *aura* "/home/colin/code/haskell/aura/rust/aura-pm/")
(defparameter *loc-dirs* (p:join *aura* "i18n"))

(defun label-matches (term)
  (->> (list "rg" "--type=rust" "--no-heading" term *aura*)
    (funcall (fn (uiop:run-program % :output :string :ignore-error-status t)))
    (str:trim)
    (str:lines)))

#+nil
(label-matches "A-i-repo")

(defun msg-labels (file)
  "All localisation string labels in a given FILE."
  (t:transduce
   (t:comp
    (t:filter (fn (not (str:blankp %))))
    (t:filter (fn (not (str:starts-with-p #\# %))))
    (t:filter (fn (alpha-char-p (aref % 0))))
    (t:map (fn (str:split " = " %)))
    (t:map #'car))
   #'t:cons file))

#+nil
(msg-labels *english*)

(defun lenient-msg-labels (file)
  "All localisation string albels in a given FILE, even if they commented out."
  (t:transduce
   (t:comp
    (t:filter (fn (not (str:blankp %))))
    (t:map (fn (str:trim-left % :char-bag "# ")))
    (t:filter (fn (str:containsp " = " %)))
    (t:map (fn (str:split " = " %)))
    (t:map #'car))
   #'t:cons file))

#+nil
(lenient-msg-labels (p:join *loc-dirs* "ja-JP" "aura_pm.ftl"))

(defun all-unused-labels (file)
  "Every fluent label unused in the given file."
  (t:transduce
   (t:filter (fn (null (label-matches %))))
   #'t:cons
   (msg-labels file)))

#+nil
(all-unused-labels *english*)

(defun unused-localisations ()
  "All localisations left over which are no longer used in the original English."
  (let ((localisation-dirs (uiop:subdirectories *loc-dirs*)))
    (format t "--- ALL UNUSED LOCALISATIONS --~%")
    (dolist (dir localisation-dirs)
      (format t "~A~%" dir)
      (let* ((path (p:join dir "aura_pm.ftl"))
             (missing (all-unused-labels path)))
        (dolist (label missing)
          (format t "  => ~A~%" label))))))

#+nil
(unused-localisations)

(defun uncopied-localisations ()
  "Localisation strings which were set in the English, but not copied over to the others."
  (let ((localisation-dirs (uiop:subdirectories *loc-dirs*))
        (english (msg-labels *english*)))
    (format t "--- ALL MISSING LOCALISATIONS (~A English) --~%" (length english))
    (dolist (dir localisation-dirs)
      (format t "~A~%" dir)
      (let* ((path (p:join dir "aura_pm.ftl"))
             (lbls (lenient-msg-labels path))
             (diff (set-difference english lbls :test #'string-equal)))
        (dolist (label diff)
          (format t "  ~A~%" label))))))

#+nil
(uncopied-localisations)
