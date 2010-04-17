;;trotr http://d.hatena.ne.jp/trotr
(eval-when-compile (require 'cl) (require 'lisp-mode))
(require 'clojure-mode)
(require 'eldoc)
(require 'anything)

(defvar clojure-init-file (concat (getenv "HOME")  "/.clojurerc"))
(defvar clojure-class-path-file (concat (getenv "HOME") "/.clojure"))
;;;util
(defmacro clojure-let1 (var val &rest body)
  (declare (indent 2) (debug t))
  `(let ((,var ,val))
     ,@body))

(defmacro clojure-rlet1 (var val &rest body)
  (declare (indent 2) (debug t))
  `(clojure-let1 ,var ,val
     ,@body
     ,var))

(defmacro clojure-and-let* (exp &rest body)
  (declare (indent 1) (debug t))
  `(%clojure-and-let* ,exp ,body))

(defun clojure-singlep (pair)
  (and (consp pair) (null (cdr pair))))

(defmacro %clojure-and-let* (exp body)
  (cond ((null exp) `(progn ,@body))
	((singlep (car exp))
	 `(if ,@(car exp)
	      (%clojure-and-let* ,(cdr exp)
				 ,body)))
	(t `(let (,(car exp))
	      (and ,(caar exp) 
		   (%clojure-and-let* ,(cdr exp)
				      ,body))))))

;;;
(defvar clojure-program-name-default "clojure")
(defvar clojure-program-name clojure-program-name-default)
(defvar clojure-repl-name "*clojure*")
(defun clojure-get-files-from-buffer (buf pattern)
  (with-current-buffer buf
    (loop initially (goto-char (point-min))
	  until (eobp)
	  for file = (buffer-substring-no-properties (point-at-bol) (point-at-eol))
	  if (file-directory-p file) nconc (directory-files file t pattern t)
	  else collect file
	  do (forward-line 1))))

(defun clojure-class-path-list ()
  (if (get-process "clojure")
      (read
       (clojure-eval 
	'(do (use 'clojure.contrib.jar)
	     (let [dir (System/getProperty "user.dir")] 
	       (map (fn [jar] 
			(let [name (.getName jar)]
			  (if (.startsWith name "/") name (str dir "/" name))))
		    (classpath-jarfiles))))))
      (clojure-get-files-from-buffer (find-file-noselect clojure-class-path-file) "\\.jar$")))


(defvar clojure-libraries-cache nil)
(defun clojure-libraries (file &optional forcep)
  (if (not forcep)
      (or (assoc file clojure-libraries-cache)
	  (clojure-libraries file t))
      (let* ((cmd (format "jar -tf %s | grep '\.clj$' | sed 's/\.clj$//g; s/\\//./g; s/_/-/g'" file))
	     (libraries (butlast (split-string (shell-command-to-string cmd) "\n"))))
	(clojure-rlet1 item (list file libraries)
	  (push item clojure-libraries-cache)))))

;;;repl
(defun run-clojure (cmd) 
  (interactive (list (if current-prefix-arg
			 (read-string "Run Clojure: " clojure-program-name)
			 clojure-program-name-default)))
  (clojure-let1 buf clojure-repl-name
    (unless  (comint-check-proc buf)
      (clojure-let1 cplist (mapconcat 'identity (cons "." (cons "classes" (clojure-class-path-list))) ":")
	(make-comint "clojure" cmd nil "-cp" cplist "-i" clojure-init-file "-r")
	(setq clojure-program-name cmd
	      clojure-buffer buf)))
    (display-buffer buf)))

(lexical-let ((clojure-lein-repl-path ""))
  (defun run-lein-repl (path) (interactive "Glein-path:")
    (let1 buf clojure-repl-name
      (unless (string-equal clojure-lein-repl-path path)
	(let1 x-file  (make-temp-file "clojure-lein-repl")
	  (with-current-buffer (find-file-noselect x-file)
	    (insert (format "#!/bin/sh\n\ncd %s && lein repl" path))
	    (save-buffer)
	    (kill-buffer))
	  (call-process-shell-command (format "chmod u+x %s" x-file))
	  (make-comint "clojure" x-file nil)
	  (clojure-eval `(load-file ,clojure-init-file))
	  (setq clojure-program-name x-file clojure-buffer buf
		clojure-lein-repl-path path)))
      (display-buffer buf))))

(defun clojure-kill-repl () (interactive)
  (clojure-let1 repl-buf (get-buffer clojure-repl-name)
    (kill-process (get-process "clojure"))
    (when repl-buf (kill-buffer repl-buf)
	  (when (> (count-windows) 1) 
	    (delete-other-windows)))))

(defun clojure-repl-alive-p (&optional forcep)
  (or (get-buffer clojure-repl-name)
      (and forcep (run-clojure clojure-program-name))))

(defun clojure-repl-wakeup ()
  (clojure-repl-alive-p t))

;;;connect with repl
;;internal variable
(defvar clojure-output-storage nil)
(defvar clojure-reading-p nil)
(defvar clojure-output-filter-functions  nil)
(defvar clojure-reading-timelimit 5)
(setq clojure-filtering-context-visible-p nil)
(defmacro clojure-filter-function-maker (regexp)
  (clojure-let1 s (gensym)
    `(lambda (,s)
       (cond ((string-match-p ,regexp ,s)
	      (setq clojure-reading-p nil)
	      (push (replace-regexp-in-string ,regexp "" ,s)
		    clojure-output-storage))
	     (t (push ,s clojure-output-storage)))
       (if clojure-filtering-context-visible-p ,s ""))))

(setq clojure-output-filter-functions 
      (list (clojure-filter-function-maker "\n?.+=> $")))

(defun clojure-stop-reading () (interactive) 
  (setq clojure-reading-p nil))

(defsubst clojure-cleanup-storage ()
  (setq clojure-output-storage nil))

(defmacro %clojure-connect-repl (send-action &optional timeout) ;timeout is sec
  (let ((life-time (gensym)) (old-comint-filter (gensym)))
    `(let ((clojure-reading-p t)
	   (,old-comint-filter comint-preoutput-filter-functions)
	   (comint-preoutput-filter-functions clojure-output-filter-functions)
	   (,life-time ,timeout)) ;;
       (run-at-time clojure-reading-timelimit nil 'clojure-stop-reading) ;;for safe(time limit)
       (unwind-protect
	   (progn (clojure-cleanup-storage)
		  ,send-action
		  (while clojure-reading-p 
		    (sleep-for 0 100)
		    (when ,life-time
		      (decf ,life-time 0.1)
		      (when (<= ,life-time 0)
			(signal  'quit "calc time is too long."))))
		  (apply 'concat (reverse clojure-output-storage)))
	 (setq comint-preoutput-filter-functions ,old-comint-filter
	       clojure-reading-p nil)
	 ))))

(defmacro clojure-connect-with-repl (send-func &optional timeout)
  `(cond (clojure-reading-p
	  (run-with-idle-timer 
	   0.2 nil
	   (lambda () (clojure-connect-with-repl ,send-func))))
	 (t
	  (progn (clojure-repl-wakeup)			;prepare
		 (%clojure-connect-repl ,send-func ,timeout)))))

;;eval via (C-x C-e .etc)
(defun clojure-send-string (str &optional newline-p)
  (clojure-let1 p (get-process "clojure")
    (cond (newline-p (comint-simple-send p str))
	  ;;	   (comint-send-string p (concat str "\n")))
	  (t (comint-send-string p str)))))

(defun clojure-send-with-action (beg-ac end-ac &optional newline-p)
  (let* ((end (progn (funcall end-ac) (point)))
	 (beg (progn (funcall beg-ac) (point)))
	 (str (buffer-substring-no-properties beg end))
	 (newline-p* (or newline-p (= (point-max) end))))
    (clojure-send-string str newline-p*)))

(defsubst clojure-end-of-paren-char-p (ch)
  (some (lambda (c) (char-equal ch c)) 
	(string-to-list ")}]")))

(defsubst clojure-end-of-sexp ()
  (clojure-let1 seq (list (char-before) (char-after))
    (unless (some 'clojure-end-of-paren-char-p seq)
      (end-of-sexp))))

(defun clojure-send-last-sexp () (interactive)
  (save-excursion
    (clojure-send-with-action
     'beginning-of-sexp 'clojure-end-of-sexp t)))

(defun clojure-send-last-defun () (interactive)
  (save-excursion
    (clojure-send-with-action
     'beginning-of-defun 'end-of-defun)))

(defun clojure-send-buffer () (interactive)
  (save-excursion
    (clojure-send-with-action 
     'beginning-of-buffer 'end-of-buffer t)))

(defun clojure-eval (sexp &optional timeout)
  (let ((print-length nil)
	(print-level nil))
    (clojure-let1 content (prin1-to-string sexp)
      (clojure-connect-with-repl
       (clojure-send-string 
	(replace-regexp-in-string "\\\\." "." content)	t)
       timeout))))

;;;anything interface
(when (fboundp 'anything)
  ;;insert-use
  (defsubst clojure--insert-use (c)
    (clojure-let1 item (format "(use '%s)" c)
      (or (re-search-backward "(use " nil t)
	  (goto-char (point-min)))
      (insert item "\n") 
      (clojure-send-string item t)))

  (defsubst clojure--describe-library (c)
    (clojure-let1 buf
	(clojure-send-string/sub-buffer
	 (concat (format 
		  "(require '%s)" c)
		 "(binding [*print-length* nil]"
		 "(doseq [[fun ns] (ns-interns '" c ")]"
		 "(printf \"%-25s\t[%s]\n\" fun (print-str ns))))")
	 (clojure-sub-output-buffer)
	 (lambda (r)
	   (dolist (line (split-string r "\n"))
	     (clojure-let1 name&absolute-name (split-string line " +")
	       (unless (null (cdr name&absolute-name))
		 (clojure--insert-describe-button line))))))
      (display-buffer buf)
      (set-window-start (get-buffer-window buf) 0)))

  (defsubst clojure-string-split-at (char str)
    (clojure-let1 n
	(catch 'return 
	  (reduce (lambda (i c) (if (char-equal char c) (throw 'return i) (+ i 1)))
		  str :initial-value 0))
      (values (substring str 0 n) (substring str n))))  

  (defsubst clojure--insert-describe-button (line)
    (multiple-value-bind (left right) 
	(clojure-string-split-at (string-to-char " ") line) 
      (insert-text-button left 
			  'action
			  (lambda (b)
			    (save-excursion 
			      (goto-char (point-at-bol))
			      (when (re-search-forward  "#'" (point-at-eol) t)
				(clojure-let1 str 
				    (buffer-substring-no-properties
				     (point) (progn (skip-chars-forward "^]") (point)))
				  (clojure-source-of-current-symbol str))))))
      (insert right "\n")))

  ;; (defun clojure-source-of-current-symbol () (interactive)
  ;;   (clojure-send-string (format "(source %s)" (current-word)) t))

  (defun clojure-source-of-current-symbol (&optional obj) (interactive "P")
    (display-buffer
     (clojure-send-string/sub-buffer
      (format "(source %s)" (or obj (current-word)))
      "*clojure-find-doc*")))
  
  (defun clojure-library-interns (library)
    (clojure-eval `(require (symbol ,library)))
    (read 
     (clojure-eval `(binding [*print-length* nil]
			     (println (sort (keys (ns-interns (symbol ,library)))))))))



  (defun clojure-library-interns-select/anything (&optional library)
    (let* ((library (or library (read-string "library:"))) ;select/anything?
	   (source 
	    `((name . ,library)
	      (candidates . (lambda ()
			      (mapcar 'symbol-name
				      (clojure-library-interns ,library))))
	      (action . (("marked insert" . 
			  (lambda (c)
			    (clojure-let1 xs  (cons c (anything-marked-candidates)) 
			      (clojure--anything-insert
			       (format "(use '[%s :only (%s)])" ,library
				       (mapconcat 'identity xs " ")) ))))
			 ("marcked source" .
			  (lambda (_)
			    (dolist (fun (anything-marked-candidates))
			      (clojure-source-of-current-symbol
			       (format "%s/%s" library fun))))))))))
      (anything (list source))))

  (defsubst clojure--anything-insert (str)
    (save-excursion
      (goto-char (point-at-bol))
      (insert str "\n")
      (clojure-send-string str t)))
  
  (defsubst clojure--insert-use-with-only (c)
    (run-with-timer 0.1 nil 'clojure-library-interns-select/anything c)) 
  
  (defsubst clojure--library-name-to-path (lib)
    (loop for pat in '("\\." "-")
	  for rep in '("/" "_")
	  do (setq lib (replace-regexp-in-string pat rep lib)))
    (concat lib ".clj"))
  
  (defun clojure-library-make-source (file libraries)
    ;;persistent-actionとか加えていないな。
    `((name . ,file)
      (candidates . ,libraries)
      (action . (("use .. (use '[library])" . clojure--insert-use)
		 ("use only .. (use '[library :only (...)" . clojure--insert-use-with-only)
		 ("describe library" . clojure--describe-library)
		 ("require as .. (require '[library :as <>])" .
		  (lambda (c)
		    (clojure-let1 as (read-string (format "require %s as:" c))
		      (clojure--anything-insert
		       (format "(require '[%s :as %s])" c as)))))
		 ("open library file" . 
		  (lambda (c)
		    (clojure-find-file-other-frame
		     (list ,file)
		     (clojure--library-name-to-path c))))
		 ("insert Marked candidates" .
		  (lambda (_) (mapc 'clojure--insert-use 
				    (anything-marked-candidates))))
		 ("all candidates output buffer" . 
		  (lambda (_) 
		    (clojure-let1 buf "*clojure libraries*"
		      (with-current-buffer (get-buffer-create buf)
			(erase-buffer)
			(dolist (xs anything-candidate-cache)
			  (destructuring-bind (file . libraries) xs
			    (insert (propertize file 'face 'anything-header) "\n")
			    (dolist (lib libraries)
			      (lexical-let ((jar file) (lib lib))
				(insert-text-button lib 'action 
						    (lambda (b) 
						      (clojure-find-file-other-frame
						       (list jar)
						       (clojure--library-name-to-path lib))))
				(insert "\n"))))))
		      (display-buffer buf))))
		 ))))

  (defun clojure-insert-use/anything () (interactive)
    (clojure-let1 sources
	(loop for (file libraries) in (mapcar 'clojure-libraries (clojure-class-path-list))
	      collect (clojure-library-make-source file libraries))
      (anything sources)))

  (defun clojure-available-symbols () ;ns is nil or string
    (clojure-let1 r
	(clojure-eval
	 `(do 
	      (doseq [[fun path] (sort (ns-map *ns*))]
		     (printf "%-25s\t[%s]\n" fun (print-str path)))
	      (doseq [[prefix ns] (ns-aliases *ns*) 
		      [fun path] (sort (ns-interns ns))]
		     (printf "%s/%-25s\t[%s]\n" (str prefix) fun (print-str path)))))
      (split-string r "\n")))

  (defsubst clojure-find-doc-from-candidate (c)
    (clojure-let1 s (car (split-string c " +"))
      (clojure-find-doc (replace-regexp-in-string "^.+/" "" s)))) 

  (defun clojure-find-doc/anything () (interactive)
    (clojure-let1 source
	`((name . "function")
	  (candidates . clojure-available-symbols)
	  (require-patterns . 2)
	  (action . (("find-doc" . clojure-find-doc-from-candidate)
		     ("find-file-other-frame" . 
		      (lambda (c)
			(clojure-let1 s (car (split-string c " +"))
			  (clojure-ffap-other-frame s))))
		     ("print (debug)" . print*))))
      (anything (list source) (current-word))))

  (defun clojure-completion/anything () (interactive)
    (let* ((word (current-word))
	   (source
	    `((name . "completion")
	      (candidates . clojure-available-symbols)
	      (action . (("insert" . (lambda (c)
				       (delete-backward-char ,(length word))
				       (insert (car (split-string c "[\t \[]+")))))
			 ("find-doc" . clojure-find-doc-from-candidate)))
	      (persistent-action .  clojure-find-doc-from-candidate))))
      (anything (list source) (format "^%s.*" word))))
  )


(defvar clojure-find-doc-history nil)
(defsubst clojure-read-arg ()
  (list 
   (read-from-minibuffer 
    "finddoc(M-n,M-p lookup history):" ""
    minibuffer-local-map nil 'clojure-find-doc-history)))

(defun clojure-find-doc (rx)  (interactive (clojure-read-arg))
  (add-to-list 'clojure-find-doc-history rx)
  (with-output-to-temp-buffer "*clojure-find-doc*"
    (princ (clojure-eval `(find-doc ,rx))))
  (clojure-search-forcus-other-window
   "*clojure-find-doc*" rx))

(defun clojure-find-doc-at-point () (interactive)
  (clojure-find-doc (current-word)))

(defun clojure-search-forcus-other-window (buf rx)
  (clojure-and-let* ((w (some-window
			 (lambda (w) (string= buf (buffer-name (window-buffer w))))
			 'no-minibuffer 'current-frame)))
    (with-selected-window w
      (re-search-forward (format "^[^ ]+/%s" rx) nil t)
      (recenter 0)
      ;; (save-excursion
      ;;   (let* ((beg (point))
      ;; 	      (end (progn (re-search-forward "^-+$" nil t)
      ;; 			  (match-beginning 0)))
      ;; 	      (ov (make-overlay beg end)))
      ;; 	 (overlay-put ov 'face 'region)))))))
      )))

(defun clojure-completion () (interactive)
  (clojure-and-let*
      ((word (current-word))
       (word* (completing-read "completion:" (clojure-available-symbols) nil nil word)))
    (delete-backward-char (length word))
    (insert (car (split-string word* " +")))))

(defun clojure-output/arrow () (interactive) ;行頭にカーソルが合ったときに上手くいかない。
  (let ((arrow " ; => ")
	(r (clojure-connect-with-repl (clojure-send-last-defun))))
    (goto-char (point-at-bol))
    (when (re-search-forward arrow (point-at-eol) t)
      (delete-region (match-beginning 0) (point-at-eol)))
    (goto-char (point-at-eol))
    (insert arrow (replace-regexp-in-string "\n" "" r))))


;;; convenient function when insert closing(paren, brace...)
(defun clojure-insert-closing (prefix default-close others) 
  ;;others = ((open . close) ({ . }) ...)
  (insert default-close) 
  (unless prefix 
    (clojure-let1 open-pt (condition-case nil 
			      (scan-sexps (point) -1) 
			    (error (beep) nil)) 
      (when open-pt 
	(clojure-let1 open-char (aref (buffer-substring-no-properties
				       open-pt (1+ open-pt))
				      0)
	  (clojure-and-let* ((other-close (assoc-default open-char others)))
	    (delete-backward-char 1)
	    (insert other-close)))))))

(defun clojure-insert-closing-paren (&optional prefix) (interactive "P") 
  (clojure-insert-closing prefix ?\) '((?\[ . ?\]) (?\{ . ?\}))))

;;;setting
(setq my-clojure-key-bindings
      `(("\C-cS" . run-clojure)
	("\C-c\C-u" . clojure-insert-use/anything)
	("\C-c\C-i" . clojure-output/arrow)
	("\C-c\C-k" . clojure-kill-repl)
	("\C-x\C-e" . clojure-send-last-sexp)
	("\C-c\C-e" . clojure-send-last-defun)
	("\C-c\C-l" . clojure-send-buffer)
	(")" . clojure-insert-closing-paren)
	("\C-hd" . clojure-find-doc-at-point)
	("\C-hf" . ,(if (fboundp 'clojure-find-doc/anything) 
			'clojure-find-doc/anything
			'clojure-find-doc))
	("\C-c\C-j" . ,(if (fboundp 'clojure-completion/anything) 
			   'clojure-completion/anything
			   'clojure-completion))))


(add-hook 'clojure-mode-hook 
	  (lambda ()
	    (loop for (key . cmd) in my-clojure-key-bindings
		  do (define-key clojure-mode-map key cmd))))

;;eldoc
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (make-local-variable 'eldoc-documentation-function)
	    (setq eldoc-documentation-function 'clojure-get-current-symbol-info)
	    (eldoc-mode)))

(defun clojure-get-current-symbol-info () (interactive)
  (clojure-eval
   `(try 
     (when-let [v (meta (resolve (symbol ,(current-word))))]
	       (apply str (:ns v) "." (:name v) "    " (:arglists v)))
     (catch Throwable _ nil))
   0.3))

;;describe-class
(defun clojure-sub-output-buffer (&optional appendp)
  (or (clojure-and-let* ((buf (get-buffer "*clojure-sub-output*")))
	(or appendp (with-current-buffer buf (erase-buffer)))
	buf)
      (get-buffer-create "*clojure-sub-output*")))

(defun* clojure-send-string/sub-buffer
    (str &optional (buf (clojure-sub-output-buffer)) (hook nil))
  (let ((buf (typecase buf 
	       (buffer buf)
	       (string (get-buffer-create buf))))
	(r (clojure-connect-with-repl (clojure-send-string str t))))
    (with-current-buffer buf
      (let ((buffer-modified-p t)
	    (buffer-read-only nil))
	(goto-char (point-min))
	(if hook (funcall hook r) (insert r "\n\n"))))
    buf))

(defun* clojure-describe-current-class (&optional in) (interactive "P")
	(let* ((input (or in (read-string "obj:" (current-word))))
	       (source `((name . ,input)
			 (init . (lambda () 
				   (anything-candidate-buffer
				    (clojure-send-string/sub-buffer
				     (format "(describe-class-for-anything %s)" ,input)))))
			 (candidates-in-buffer)
			 (action . kill-new))))
	  (anything source)))

;;clojure-ffap
(defun clojure-x-to-string (x)
  (typecase x 
    (symbol (symbol-name x))
    (string x)
    (otherwise (prin1-to-string x))))

(defun clojure-get-info-for-ffap (x)
  "return ((dirs (dir ...)) (file file) (line line))"
  (clojure-let1 str (clojure-x-to-string x)
    (read 
     (clojure-eval
      `(when-let [table (meta (resolve (symbol ,str)))]
		 (list (list (quote dirs)
			     (map (fn [url] (.getFile url)) (current-classpath)))
		       (list (quote file) (:file table))
		       (list (quote line) (:line table))))))))

(defun* clojure-ffap-other-frame (&optional (x (current-word)))
  (interactive)
  (clojure-ffap x t))

(defun* clojure-ffap (&optional (x (current-word)) (other-frame-p nil))
  (destructuring-bind (jars file line)
      (mapcar 'cadr (clojure-get-info-for-ffap x))
    (if other-frame-p
	(clojure-find-file-other-frame jars file line)
	(clojure-find-file jars file line))))

(defun clojure-find-file (jars file-path &optional lineno)
  (catch 'return
    (dolist (jar-path jars)      (find-file jar-path)
	    (goto-char (point-min))
	    (when (re-search-forward file-path nil t 1)
	      (archive-extract)
	      (goto-char (point-min))
	      (when lineno (forward-line (1- lineno))) 
	      (throw 'return nil)))))

(defun clojure-find-file-other-frame (jars file-path &optional lineno)
  (clojure-select-frame)
  (clojure-find-file jars file-path lineno))

(defun* clojure-select-frame (&optional (frame-name "clojure-frame"))
  (condition-case err (select-frame-by-name frame-name)
    (select-frame-by-name frame-name)
    (error
     (select-frame 
      (make-frame `((name . ,frame-name) 
		    (width . ,(frame-width))
		    (height . ,(frame-height))
		    (minibuffer . t)))))))
