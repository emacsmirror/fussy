;;; fussy-perf-bench.el --- Diagnostic benchmark for fussy slowness  -*- lexical-binding: t; -*-

;; HOW TO USE:
;;
;; This file is meant to be invoked from `fussy-perf-bench.sh', which
;; spawns two `emacs -Q --batch' children — one for each completion
;; style — bootstraps the relevant config in each, and writes per-style
;; report files that are then concatenated.  Run:
;;
;;   bash /path/to/fussy/fussy-perf-bench.sh
;;
;; The script writes the combined report to `~/fussy-perf-report.txt'.
;;
;; You can also drive it manually for one style:
;;
;;   emacs -Q --batch \
;;     -L /path/to/fussy --load fussy-perf-bench \
;;     --eval "(fussy-perf-bench-bootstrap-load-path)" \
;;     --eval "(fussy-perf-bench-bootstrap-company)" \
;;     --eval "(fussy-perf-bench-bootstrap-fussy)" \
;;     --eval "(fussy-perf-bench-run 'fussy \"/tmp/fussy.txt\")"
;;
;; The bench produces, per style:
;;   - environment / config / advice snapshot
;;   - per-keystroke timing for ("" "s" "se" "set" "setq")
;;     with the buffer arranged as `(QUERY|)' (smartparens-inserted state)
;;   - CPU profiler dumps for the "s" and "setq" cases

;;; Code:

(require 'cl-lib)
(require 'profiler)

(defvar fussy-perf-bench-queries '("" "s" "se" "set" "setq")
  "Queries to benchmark, in keystroke order.")

(defvar fussy-perf-bench-iters 30
  "Iterations per query for steady-state timing.")

(defvar fussy-perf-bench-profile-iters 500
  "Iterations for the profiler run.")

(defvar fussy-perf-bench-typing-sequence "setq"
  "Characters to type, one at a time, inside `(|)' for the typing-sequence pass.
After the user types `(' (smartparens inserts the closing `)' for them
and leaves point between them), each remaining char in this string is
inserted one at a time and the resulting company refresh is timed.
Both an exact-cache hit and a subsumption-cache hit are exercised here:
the bench reuses the same `company-candidates-cache' across chars in a
given iteration, mirroring what happens inside one real popup session.")

(defvar fussy-perf-bench-typing-iters 30
  "Number of times to replay the typing sequence in a fresh buffer.")

;;; --- Bootstrap --------------------------------------------------------------

(defun fussy-perf-bench--glob (pattern)
  (file-expand-wildcards pattern))

(defun fussy-perf-bench-bootstrap-load-path ()
  "Add the elpa subdirectories the bench needs to `load-path'.
Glob-resolves versioned directories so we don't have to hardcode dates."
  (dolist (pat '("~/.emacs.d/elpa/31/fzf-native"
                 "~/.emacs.d/elpa/31/fussy"
                 "~/.emacs.d/elpa/31/flx-*"
                 "~/.emacs.d/elpa/31/company-*"
                 "~/.emacs.d/elpa/compat-*"))
    (dolist (dir (fussy-perf-bench--glob (expand-file-name pat)))
      (when (and (file-directory-p dir)
                 (not (string-suffix-p ".signed" dir)))
        (add-to-list 'load-path dir)))))

(defun fussy-perf-bench-bootstrap-company ()
  "Apply company config matching ~/.emacs.d/user-lisp/jn-completion.el.
Only the bits that could affect per-keystroke timing are mirrored — the
keybindings and tng setup don't matter in batch."
  (require 'company)
  (require 'company-capf)
  (setq company-idle-delay 0.05
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-echo-delay 1
        company-lighter-base ""
        company-frontends nil               ; no UI in batch
        company-require-match nil))

(defun fussy-perf-bench-bootstrap-fussy ()
  "Bootstrap fussy exactly as the user's `use-package fussy' block does."
  (require 'fzf-native)
  (when (fboundp 'fzf-native-load-dyn) (fzf-native-load-dyn))
  (require 'fussy)
  (fussy-setup-fzf)
  (fussy-eglot-setup)
  (fussy-company-setup))

(defun fussy-perf-bench-bootstrap-flex ()
  "Bootstrap a clean flex-only completion style."
  (setq completion-styles '(flex)))

;;; --- Stage instrumentation --------------------------------------------------

(defvar fussy-perf-bench--stage-times (make-hash-table :test 'eq))
(defvar fussy-perf-bench--installed-advice nil)

(defun fussy-perf-bench--record (name elapsed-ms)
  (push elapsed-ms (gethash name fussy-perf-bench--stage-times)))

(defun fussy-perf-bench--make-timer-advice (name)
  (lambda (orig &rest args)
    (let ((start (current-time)))
      (unwind-protect (apply orig args)
        (fussy-perf-bench--record
         name (* 1000.0 (float-time (time-since start))))))))

(defvar fussy-perf-bench--instrumented-fns
  '(company--fetch-candidates
    company--transform-candidates
    company--preprocess-candidates
    company--capf-completions
    completion-all-completions
    fussy-all-completions
    fussy-all-completions-v1
    fussy-filter-by-scoring
    fussy-filter-default
    fussy-filter-flex
    fussy-outer-score
    fussy-fzf-score
    fussy-score
    fussy--sort
    fussy--default-sort
    fussy--adjust-metadata
    fussy-normalize-query
    fussy--history-hash-table
    completion-flex-all-completions))

(defun fussy-perf-bench--instrument ()
  (fussy-perf-bench--uninstrument)
  (dolist (fn fussy-perf-bench--instrumented-fns)
    (when (fboundp fn)
      (let ((adv (fussy-perf-bench--make-timer-advice fn)))
        (advice-add fn :around adv)
        (push (cons fn adv) fussy-perf-bench--installed-advice)))))

(defun fussy-perf-bench--uninstrument ()
  (dolist (entry fussy-perf-bench--installed-advice)
    (advice-remove (car entry) (cdr entry)))
  (setq fussy-perf-bench--installed-advice nil))

(defun fussy-perf-bench--reset-stage-times ()
  (clrhash fussy-perf-bench--stage-times))

(defun fussy-perf-bench--stage-summary ()
  (let (rows)
    (maphash
     (lambda (name times)
       (let* ((sorted (sort (copy-sequence times) #'<))
              (n (length sorted))
              (total (apply #'+ sorted))
              (avg (/ total (float n)))
              (p (lambda (q) (nth (min (1- n) (floor (* q n))) sorted))))
         (push (list name n total avg
                     (funcall p 0.5) (funcall p 0.95)
                     (apply #'max sorted))
               rows)))
     fussy-perf-bench--stage-times)
    (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b))))))

;;; --- Snapshot ---------------------------------------------------------------

(defun fussy-perf-bench--insert-section (title)
  (insert (format "\n=== %s ===\n" title)))

(defun fussy-perf-bench--insert-env (buf style)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section (format "ENVIRONMENT (style=%S)" style))
    (insert (format "emacs-version       : %s\n" emacs-version))
    (insert (format "system-type         : %s\n" system-type))
    (insert (format "system-configuration: %s\n" system-configuration))
    (insert (format "invocation          : emacs -Q --batch\n"))
    (insert (format "fussy library file  : %s\n"
                    (or (locate-library "fussy") "(not loaded)")))
    (insert (format "company library file: %s\n"
                    (or (locate-library "company") "(not loaded)")))
    (insert (format "fzf-native library  : %s\n"
                    (or (locate-library "fzf-native") "(not loaded)")))
    (insert (format "fzf-native dyn loaded: %s\n"
                    (if (fboundp 'fzf-native-score-all) "YES" "NO")))
    (insert (format "obarray size        : %s\n"
                    (let ((n 0)) (mapatoms (lambda (_) (cl-incf n))) n)))))

(defun fussy-perf-bench--insert-fussy-vars (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "FUSSY DEFCUSTOMS")
    (if (not (boundp 'fussy-filter-fn))
        (insert "(fussy not loaded in this process)\n")
      (dolist (sym '(fussy-debug
                     fussy-use-cache
                     fussy-filter-fn
                     fussy-score-fn
                     fussy-score-ALL-fn
                     fussy-default-regex-fn
                     fussy-max-candidate-limit
                     fussy-max-query-length
                     fussy-ignore-case
                     fussy-fzf-case-mode
                     fussy-fzf-native-highlight
                     fussy-compare-same-score-fn
                     fussy-default-sort-fn
                     fussy-AND-component-separator
                     fussy-OR-component-separator
                     fussy-company-prefix-length
                     fussy-company-filter-only-length
                     fussy-adjust-metadata-fn
                     fussy-propertize-fn
                     fussy-prefer-prefix))
        (when (boundp sym)
          (insert (format "%-44s = %S\n" sym (symbol-value sym))))))))

(defun fussy-perf-bench--insert-fzf-vars (buf)
  (when (boundp 'fzf-native-case-mode)
    (with-current-buffer buf
      (fussy-perf-bench--insert-section "FZF-NATIVE VARIABLES")
      (dolist (sym '(fzf-native-case-mode
                     fzf-native-batch-highlight
                     fzf-native-filter-only-length
                     fzf-native-filter-only-logic
                     fzf-native-filter-only-min-pool
                     fzf-native-max-line-length))
        (when (boundp sym)
          (insert (format "%-44s = %S\n" sym (symbol-value sym))))))))

(defun fussy-perf-bench--insert-company-config (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "COMPANY CONFIG")
    (dolist (sym '(company-idle-delay
                   company-minimum-prefix-length
                   company-tooltip-align-annotations
                   company-selection-wrap-around
                   company-frontends
                   company-transformers
                   company-backends
                   company-require-match))
      (when (boundp sym)
        (insert (format "%-44s = %S\n" sym (symbol-value sym)))))))

(defun fussy-perf-bench--insert-completion-config (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "COMPLETION CONFIG")
    (insert (format "completion-styles               : %S\n" completion-styles))
    (insert (format "completion-category-overrides  :\n%s"
                    (pp-to-string completion-category-overrides)))
    (insert (format "completion-styles-alist (keys) : %S\n"
                    (mapcar #'car completion-styles-alist)))
    (when (boundp 'completion-lazy-hilit)
      (insert (format "completion-lazy-hilit          : %S\n"
                      completion-lazy-hilit)))))

(defun fussy-perf-bench--insert-gc-info (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "GC")
    (insert (format "gc-cons-threshold : %s (%.1f MB)\n"
                    gc-cons-threshold
                    (/ gc-cons-threshold 1048576.0)))
    (insert (format "gc-cons-percentage: %s\n" gc-cons-percentage))
    (insert (format "gcs-done so far   : %d\n" gcs-done))
    (insert (format "gc-elapsed so far : %.3f s\n" gc-elapsed))))

(defun fussy-perf-bench--insert-advice (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "ADVICE CHAINS")
    (dolist (fn '(company--fetch-candidates
                  company--transform-candidates
                  company--preprocess-candidates
                  company-auto-begin
                  company--capf-completions
                  company-capf
                  fussy-all-completions
                  fussy-all-completions-v1
                  fussy--sort
                  fzf-native-score-all
                  completion-all-completions
                  completion-flex-all-completions
                  elisp-completion-at-point))
      (when (fboundp fn)
        (let ((names '()))
          (advice-mapc (lambda (a _) (push a names)) fn)
          (insert (format "%s:" fn))
          (if names
              (dolist (n (nreverse names)) (insert (format "\n  - %s" n)))
            (insert " (no advice)"))
          (insert "\n"))))))

(defun fussy-perf-bench--insert-capf-context (buf)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section "LISP-INTERACTION-MODE CAPF CONTEXT")
    (let (caps mode)
      (with-temp-buffer
        (lisp-interaction-mode)
        (insert "(setq")
        (setq caps completion-at-point-functions
              mode major-mode))
      (insert (format "major-mode (in test buffer): %S\n" mode))
      (insert (format "completion-at-point-functions:\n  %S\n" caps)))))

;;; --- Per-keystroke timing ---------------------------------------------------

(defun fussy-perf-bench--run-one-query (style query iters)
  "Run ITERS calls of the company-capf -> STYLE pipeline at QUERY.
Returns plist (:total-ms LIST :gcs N :gc-ms F :n-cands N :prefix-info P).

Reproduces smartparens-inserted state: the test buffer is `(QUERY|)'
with point between QUERY and the closing paren."
  (let* ((times '())
         (start-gcs gcs-done)
         (start-gc-elapsed gc-elapsed)
         (n-cands 0)
         (prefix-info nil))
    (with-temp-buffer
      (lisp-interaction-mode)
      (insert "(" query ")")
      (goto-char (1- (point)))
      (let ((completion-styles (list style)))
        (company-mode 1)
        (let ((company--manual-now t))
          (ignore-errors (company-manual-begin)))
        (unless company-backend
          (setq-local company-backend 'company-capf))
        (setq prefix-info
              (ignore-errors (company-call-backend 'prefix)))
        (let* ((prefix (cond ((stringp prefix-info) prefix-info)
                             ((consp prefix-info)
                              (or (car prefix-info) query))
                             (t query)))
               (suffix (cond ((and (consp prefix-info)
                                   (stringp (cdr prefix-info)))
                              (cdr prefix-info))
                             ((and (consp prefix-info)
                                   (consp (cdr prefix-info))
                                   (stringp (cadr prefix-info)))
                              (cadr prefix-info))
                             (t ""))))
          (setq company-prefix prefix)
          (dotimes (_ iters)
            (let ((t0 (current-time)))
              (let ((cands (company--fetch-candidates prefix suffix)))
                (setq n-cands (length cands)))
              (push (* 1000.0 (float-time (time-since t0))) times))))
        (ignore-errors (company-abort))))
    (list :total-ms (nreverse times)
          :gcs (- gcs-done start-gcs)
          :gc-ms (* 1000.0 (- gc-elapsed start-gc-elapsed))
          :n-cands n-cands
          :prefix-info prefix-info)))

(defun fussy-perf-bench--percentile (sorted q)
  (let ((n (length sorted)))
    (nth (min (1- n) (max 0 (floor (* q n)))) sorted)))

(defun fussy-perf-bench--summarize-times (times)
  (let* ((sorted (sort (copy-sequence times) #'<))
         (n (length sorted))
         (sum (apply #'+ sorted)))
    (list :n n
          :total sum
          :avg (/ sum (float n))
          :min (car sorted)
          :p50 (fussy-perf-bench--percentile sorted 0.5)
          :p95 (fussy-perf-bench--percentile sorted 0.95)
          :max (car (last sorted)))))

(defun fussy-perf-bench--insert-query-block (buf style query iters)
  (fussy-perf-bench--reset-stage-times)
  (fussy-perf-bench--run-one-query style query 3)  ; warm-up
  (fussy-perf-bench--reset-stage-times)
  (let* ((result (fussy-perf-bench--run-one-query style query iters))
         (times (plist-get result :total-ms))
         (s (fussy-perf-bench--summarize-times times))
         (stages (fussy-perf-bench--stage-summary)))
    (with-current-buffer buf
      (insert (format "\n--- style=%S query=%S iters=%d cands=%d ---\n"
                      style query iters (plist-get result :n-cands)))
      (insert (format "Buffer state: \"(%s|)\"   prefix returned: %S\n"
                      query (plist-get result :prefix-info)))
      (insert (format "Total (ms): n=%d avg=%.3f min=%.3f p50=%.3f p95=%.3f max=%.3f sum=%.1f\n"
                      (plist-get s :n) (plist-get s :avg) (plist-get s :min)
                      (plist-get s :p50) (plist-get s :p95) (plist-get s :max)
                      (plist-get s :total)))
      (insert (format "GC during run: count=%d, gc-ms=%.1f\n"
                      (plist-get result :gcs) (plist-get result :gc-ms)))
      (insert "Stage breakdown (sorted by total ms):\n")
      (insert (format "  %-42s %6s %10s %8s %8s %8s %8s\n"
                      "fn" "calls" "total-ms" "avg" "p50" "p95" "max"))
      (dolist (row stages)
        (insert (format "  %-42s %6d %10.3f %8.3f %8.3f %8.3f %8.3f\n"
                        (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row)
                        (nth 4 row) (nth 5 row) (nth 6 row)))))))

(defun fussy-perf-bench--per-keystroke (buf style)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section
     (format "PER-KEYSTROKE TIMING (style=%S, iters=%d each)"
             style fussy-perf-bench-iters))
    (insert "Stage breakdown uses temporary :around advice on each function.\n")
    (insert "All times in milliseconds.\n"))
  (fussy-perf-bench--instrument)
  (unwind-protect
      (dolist (query fussy-perf-bench-queries)
        (fussy-perf-bench--insert-query-block
         buf style query fussy-perf-bench-iters))
    (fussy-perf-bench--uninstrument)))

;;; --- Typing-sequence pass (state carries across chars) ---------------------
;;
;; The single-prefix `--per-keystroke' pass calls `company--fetch-candidates'
;; in isolation — bypassing both `company-candidates-cache' and
;; `company--postprocess-candidates'.  That misses two things real typing
;; goes through:
;;
;;   1. `company-candidates-cache' is populated by `company-calculate-
;;      candidates' after a successful fetch.  If the user types `s' then
;;      `e' then `t' then `q' inside a popup session, the second/third/
;;      fourth keystrokes are eligible to subsume off the previous prefix
;;      via `all-completions' — much cheaper than re-running the style.
;;
;;   2. `company--postprocess-candidates' (transformers + dedup) runs on
;;      every keystroke after the backend returns.
;;
;; We exercise both by typing the sequence char-by-char into a single
;; buffer per iteration and timing `company-calculate-candidates' (which
;; is what `company--continue' / `company--begin-new' call on each post-
;; command).  State carries WITHIN an iteration, resets BETWEEN iterations.
;;
;; Frontend cost (popup draw) is still NOT measured — batch has no display
;; — but everything that runs on the elisp side before the redisplay is.

(defun fussy-perf-bench--type-one-sequence (style)
  "Type `fussy-perf-bench-typing-sequence' into a fresh `(|)' buffer.
Returns a list of plists, one per char:
  (:char STR :prefix STR :cands N :ms FLOAT :cache-hit BOOL)
where :cache-hit reflects whether `company-candidates-cache' had a usable
entry (exact or subsumable) for that char's prefix BEFORE the call."
  (let ((results '()))
    (with-temp-buffer
      (lisp-interaction-mode)
      (insert "()")
      (goto-char (1- (point)))
      (let ((company-frontends nil)
            (company-idle-delay nil)
            (company-minimum-prefix-length 0)
            (completion-styles (list style)))
        (company-mode 1)
        ;; Set the backend directly without `company-manual-begin' — the
        ;; latter performs a full fetch for the empty prefix, which would
        ;; pre-populate `company-candidates-cache' and turn every later
        ;; keystroke into a spurious cache hit.  We want a real cold-start
        ;; for char 0.
        (setq-local company-backend 'company-capf)
        (dolist (ch (string-to-list fussy-perf-bench-typing-sequence))
          (insert (char-to-string ch))
          (let* ((pi (ignore-errors (company-call-backend 'prefix)))
                 (prefix (cond ((stringp pi) pi)
                               ((consp pi) (or (car pi) ""))
                               (t "")))
                 (suffix (cond ((and (consp pi) (stringp (cdr pi))) (cdr pi))
                               ((and (consp pi) (consp (cdr pi))
                                     (stringp (cadr pi)))
                                (cadr pi))
                               (t "")))
                 ;; Snapshot cache state BEFORE the call so we can report
                 ;; whether the keystroke was eligible for subsumption.
                 (cache-hit
                  (and company-candidates-cache
                       (let ((len (length prefix)) hit)
                         (cl-dotimes (i (1+ len) hit)
                           (when (assoc (substring prefix 0 (- len i))
                                        company-candidates-cache)
                             (cl-return t))))))
                 (t0 (current-time)))
            (setq company-prefix prefix
                  company-suffix suffix
                  company-point (point))
            (let ((cands (company-calculate-candidates prefix nil suffix)))
              (setq company-candidates cands))
            (push (list :char (char-to-string ch)
                        :prefix prefix
                        :cands (length company-candidates)
                        :ms (* 1000.0 (float-time (time-since t0)))
                        :cache-hit cache-hit)
                  results)))
        (ignore-errors (company-abort))))
    (nreverse results)))

(defun fussy-perf-bench--typing-sequence-pass (buf style)
  (let* ((seq fussy-perf-bench-typing-sequence)
         (len (length seq))
         (slots (make-vector len nil))
         (start-gcs gcs-done)
         (start-gc-elapsed gc-elapsed))
    (dotimes (i len)
      (aset slots i (list :times nil :cands 0 :prefix ""
                          :hit-count 0)))
    ;; Warm-up — pulls flx/fzf-native caches into a steady state.
    (fussy-perf-bench--type-one-sequence style)
    (dotimes (_ fussy-perf-bench-typing-iters)
      (let ((iter (fussy-perf-bench--type-one-sequence style)))
        (cl-loop for step in iter
                 for i from 0
                 do (let ((slot (aref slots i)))
                      (push (plist-get step :ms)
                            (plist-get slot :times))
                      (plist-put slot :cands (plist-get step :cands))
                      (plist-put slot :prefix (plist-get step :prefix))
                      (when (plist-get step :cache-hit)
                        (plist-put slot :hit-count
                                   (1+ (plist-get slot :hit-count))))))))
    (with-current-buffer buf
      (fussy-perf-bench--insert-section
       (format "TYPING SEQUENCE (style=%S, seq=%S, iters=%d)"
               style seq fussy-perf-bench-typing-iters))
      (insert
       "State (company-candidates-cache + any fussy-internal cache) carries WITHIN\n"
       "an iteration, resets BETWEEN iterations.  Each per-char ms is the cost of\n"
       "`company-calculate-candidates' — cache check + (fetch | subsume) + postprocess.\n"
       "Frontend (popup draw) is still not measured in batch.\n\n")
      (insert (format "%-4s %-10s %8s %12s %6s %8s %8s %8s %8s\n"
                      "idx" "prefix" "cands" "cache-hits" "n" "avg"
                      "p50" "p95" "max"))
      (let ((sum-p50 0.0)
            (sum-avg 0.0))
        (dotimes (i len)
          (let* ((slot (aref slots i))
                 (times (plist-get slot :times))
                 (s (fussy-perf-bench--summarize-times times)))
            (insert (format "  %-2d %-10s %8d %6d/%-5d %6d %8.3f %8.3f %8.3f %8.3f\n"
                            i
                            (format "%S" (plist-get slot :prefix))
                            (plist-get slot :cands)
                            (plist-get slot :hit-count)
                            fussy-perf-bench-typing-iters
                            (plist-get s :n)
                            (plist-get s :avg)
                            (plist-get s :p50)
                            (plist-get s :p95)
                            (plist-get s :max)))
            (cl-incf sum-p50 (plist-get s :p50))
            (cl-incf sum-avg (plist-get s :avg))))
        (insert (format "\nSum of medians (p50) across chars: %.3f ms\n" sum-p50))
        (insert (format "Sum of avgs across chars         : %.3f ms\n" sum-avg))
        (insert (format "GC during typing-sequence pass   : count=%d, gc-ms=%.1f\n"
                        (- gcs-done start-gcs)
                        (* 1000.0 (- gc-elapsed start-gc-elapsed))))))))

;;; --- Interactive bench (real session, includes frontend draw) --------------
;;
;; The batch passes can't measure popup draw cost — there is no display.
;; This pass runs inside your live Emacs, types the sequence into a
;; visible buffer using the same advice/hooks your real session has, and
;; calls `redisplay t' after every keystroke so the pseudo-tooltip
;; overlay and any other frontend work actually executes.
;;
;; For the flex side, we temporarily peel the four fussy advices so flex
;; measures against a clean company pipeline; they're restored when the
;; bench finishes.
;;
;; Run via `M-x fussy-perf-bench-interactive RET'.

(defvar fussy-perf-bench-interactive-iters 20
  "Iterations for the interactive bench (each replays the typing sequence).")

(defconst fussy-perf-bench--fussy-setup-advice
  '((company--fetch-candidates     :around   fussy-company--fetch-candidates)
    (company--transform-candidates :around   fussy-company--transformer)
    (company--preprocess-candidates :override fussy-company--preprocess-candidates)
    (fussy--sort                   :around   fussy-fzf--sort-highlight-advice))
  "Advices that fussy-{setup,setup-fzf,company-setup} install.
Used by the interactive bench to peel them for the flex baseline run.")

(defun fussy-perf-bench--snapshot-fussy-advice ()
  "Return the subset of `fussy-perf-bench--fussy-setup-advice' that is
currently installed on its target function."
  (let (active)
    (dolist (spec fussy-perf-bench--fussy-setup-advice)
      (let ((fn (car spec))
            (adv (nth 2 spec))
            (found nil))
        (when (fboundp fn)
          (advice-mapc (lambda (a _) (when (eq a adv) (setq found t))) fn))
        (when found (push spec active))))
    (nreverse active)))

(defvar fussy-perf-bench--diagnostic-log nil
  "Diagnostic strings emitted by the bench, flushed into the report.")

(defun fussy-perf-bench--diag (fmt &rest args)
  (push (apply #'format fmt args) fussy-perf-bench--diagnostic-log))

(defun fussy-perf-bench-interactive--type-once (buf seq per-char-times per-char-cands &optional diagnose)
  "Type SEQ into BUF one char at a time, measure wall-time per keystroke.

Each char is inserted by simulating the command loop: set
`this-command'/`last-command-event', run `pre-command-hook',
`self-insert-command' (which itself fires `post-self-insert-hook'),
then `post-command-hook' (which fires `company-post-command' →
`company--perform' → fetch).  After each char we force `redisplay t'
so frontend overlays draw.

Pushes per-char wall-time (ms) into PER-CHAR-TIMES if non-nil; updates
PER-CHAR-CANDS with the candidate count seen at each char.  When
DIAGNOSE is non-nil, logs detailed state to
`fussy-perf-bench--diagnostic-log'."
  (with-current-buffer buf
    (erase-buffer)
    (insert "()")
    (goto-char (1- (point)))
    (when diagnose
      (fussy-perf-bench--diag
       "  capf in test buffer  : %S" completion-at-point-functions)
      (fussy-perf-bench--diag
       "  company-mode         : %S"
       (and (boundp 'company-mode) company-mode))
      (fussy-perf-bench--diag
       "  evil-mode / state    : %S / %S"
       (bound-and-true-p evil-mode)
       (bound-and-true-p evil-state))
      (fussy-perf-bench--diag
       "  company-backends     : %S"
       (and (boundp 'company-backends) company-backends))
      (fussy-perf-bench--diag
       "  company-begin-commands: %S"
       (and (boundp 'company-begin-commands) company-begin-commands))
      (fussy-perf-bench--diag
       "  completion-styles    : %S" completion-styles)
      (fussy-perf-bench--diag
       "  post-command-hook (local): %S"
       (let (out)
         (mapatoms (lambda (_)))
         (dolist (h (cdr (assq 'post-command-hook
                               (buffer-local-variables buf))))
           (push h out))
         (nreverse out))))
    (dotimes (i (length seq))
      (let ((ch (aref seq i))
            (t0 (current-time))
            (point-before (point)))
        (setq last-command this-command
              this-command 'self-insert-command
              last-command-event ch)
        (run-hooks 'pre-command-hook)
        (self-insert-command 1)
        (run-hooks 'post-command-hook)
        (redisplay t)
        (let ((elapsed (* 1000.0 (float-time (time-since t0))))
              (cands-now (and (boundp 'company-candidates)
                              company-candidates)))
          (when per-char-times
            (push elapsed (aref per-char-times i)))
          (when per-char-cands
            (aset per-char-cands i (length (or cands-now '()))))
          (when diagnose
            (fussy-perf-bench--diag
             "  char %S: point %d->%d, this-command=%S, company-candidates=%d, elapsed=%.3fms"
             (char-to-string ch) point-before (point) this-command
             (length (or cands-now '())) elapsed)))))))

(defun fussy-perf-bench-interactive--measure-style (style)
  "Run the typing sequence in a visible buffer under STYLE, measuring
each keystroke's full wall-time (hooks + redisplay).  For STYLE = `flex',
fussy advices are peeled for the duration of the measurement.

Returns (:style STYLE :per-char-times VEC :per-char-cands VEC
         :gcs N :gc-ms FLOAT :peeled LIST)."
  (let* ((seq fussy-perf-bench-typing-sequence)
         (len (length seq))
         (per-char-times (make-vector len nil))
         (per-char-cands (make-vector len 0))
         (start-gcs gcs-done)
         (start-gc-elapsed gc-elapsed)
         (peeled (when (eq style 'flex)
                   (fussy-perf-bench--snapshot-fussy-advice)))
         (buf (get-buffer-create "*fussy-perf-bench-typing*")))
    (when peeled
      (dolist (s peeled) (advice-remove (car s) (nth 2 s))))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer buf
            (erase-buffer)
            (lisp-interaction-mode)
            ;; If evil-mode is active in this buffer, put it in
            ;; emacs-state so direct `self-insert-command' calls
            ;; actually trigger the normal company-post-command path
            ;; instead of being intercepted by evil's normal-state
            ;; post-command handler (which suppresses company fetch).
            (when (and (bound-and-true-p evil-mode)
                       (fboundp 'evil-emacs-state))
              (evil-emacs-state 1)))
          ;; `pop-to-buffer' (vs `display-buffer' + `select-window') makes
          ;; sure the buffer is both displayed AND the selected window —
          ;; required for `redisplay t' to draw frontend overlays into it
          ;; and for `execute-kbd-macro' to route the keystroke at the
          ;; right point.
          (pop-to-buffer buf '(display-buffer-pop-up-window))
          (let ((completion-styles (list style))
                ;; Drive the post-command flow ourselves; don't wait
                ;; for the idle timer.
                (company-idle-delay 0)
                (company-minimum-prefix-length 0))
            (when (fboundp 'company-mode) (company-mode 1))
            (fussy-perf-bench--diag "STYLE=%S diagnostic warm-up:" style)
            ;; Warm-up with diagnose=t so we get one detailed trace.
            (fussy-perf-bench-interactive--type-once
             buf seq nil per-char-cands t)
            ;; Measure.
            (dotimes (_ fussy-perf-bench-interactive-iters)
              (fussy-perf-bench-interactive--type-once
               buf seq per-char-times per-char-cands))
            (when (and (fboundp 'company-mode)
                       (bound-and-true-p company-mode))
              (company-mode -1))))
      (when peeled
        (dolist (s peeled) (advice-add (car s) (nth 1 s) (nth 2 s))))
      (when (buffer-live-p buf) (kill-buffer buf)))
    (list :style style
          :per-char-times per-char-times
          :per-char-cands per-char-cands
          :gcs (- gcs-done start-gcs)
          :gc-ms (* 1000.0 (- gc-elapsed start-gc-elapsed))
          :peeled peeled)))

(defun fussy-perf-bench-interactive--write-style (out result)
  (let* ((style (plist-get result :style))
         (seq fussy-perf-bench-typing-sequence)
         (len (length seq))
         (times (plist-get result :per-char-times))
         (cands (plist-get result :per-char-cands))
         (peeled (plist-get result :peeled)))
    (with-current-buffer out
      (insert (format "\n=== STYLE=%S ===\n" style))
      (insert (format "Peeled for clean baseline: %s\n"
                      (if peeled
                          (mapconcat (lambda (s) (symbol-name (nth 2 s)))
                                     peeled ", ")
                        "none (style measured against current session as-is)")))
      (insert (format "%-4s %-6s %8s %6s %8s %8s %8s %8s\n"
                      "idx" "char" "cands" "n" "avg" "p50" "p95" "max"))
      (let ((sum-p50 0.0))
        (dotimes (i len)
          (let* ((tl (aref times i))
                 (s (fussy-perf-bench--summarize-times tl)))
            (insert (format "  %-2d %-6s %8d %6d %8.3f %8.3f %8.3f %8.3f\n"
                            i (format "%c" (aref seq i))
                            (aref cands i)
                            (plist-get s :n)
                            (plist-get s :avg)
                            (plist-get s :p50)
                            (plist-get s :p95)
                            (plist-get s :max)))
            (cl-incf sum-p50 (plist-get s :p50))))
        (insert (format "\nSum of p50 across chars: %.3f ms\n" sum-p50))
        (insert (format "GC during run         : count=%d, gc-ms=%.1f\n"
                        (plist-get result :gcs)
                        (plist-get result :gc-ms)))))))

(defun fussy-perf-bench-interactive--write-comparison (out fussy-r flex-r)
  (let* ((seq fussy-perf-bench-typing-sequence)
         (len (length seq))
         (ft (plist-get fussy-r :per-char-times))
         (xt (plist-get flex-r  :per-char-times)))
    (with-current-buffer out
      (insert "\n=== SIDE-BY-SIDE COMPARISON (p50, ms) ===\n")
      (insert (format "%-4s %-6s %10s %10s %10s\n"
                      "idx" "char" "fussy" "flex" "winner"))
      (let ((sum-f 0.0) (sum-x 0.0))
        (dotimes (i len)
          (let* ((fp (plist-get (fussy-perf-bench--summarize-times
                                 (aref ft i)) :p50))
                 (xp (plist-get (fussy-perf-bench--summarize-times
                                 (aref xt i)) :p50)))
            (insert (format "  %-2d %-6s %10.3f %10.3f %10s\n"
                            i (format "%c" (aref seq i))
                            fp xp
                            (cond ((< fp xp)
                                   (format "fussy %.1fx" (/ xp (max fp 0.001))))
                                  ((> fp xp)
                                   (format "flex  %.1fx" (/ fp (max xp 0.001))))
                                  (t "tie"))))
            (cl-incf sum-f fp)
            (cl-incf sum-x xp)))
        (insert (format "  --- sum %4s %10.3f %10.3f %10s\n"
                        ""
                        sum-f sum-x
                        (cond ((< sum-f sum-x)
                               (format "fussy %.1fx" (/ sum-x (max sum-f 0.001))))
                              ((> sum-f sum-x)
                               (format "flex  %.1fx" (/ sum-f (max sum-x 0.001))))
                              (t "tie"))))))))

;;;###autoload
(defun fussy-perf-bench-interactive (&optional output-file)
  "Run the interactive bench in the current Emacs session.
Types `fussy-perf-bench-typing-sequence' char-by-char inside `(|)' under
both fussy and flex, measuring full per-keystroke wall-time including
the post-command-hook chain and `redisplay t' (so popup draw counts).
Writes the report to OUTPUT-FILE (default
`~/fussy-perf-interactive-report.txt') and opens it."
  (interactive
   (list (read-file-name "Report path: "
                         "~/" "fussy-perf-interactive-report.txt"
                         nil "fussy-perf-interactive-report.txt")))
  (unless (fboundp 'company-calculate-candidates)
    (user-error
     "company-mode must be loaded for the interactive bench; (require 'company) first"))
  (setq fussy-perf-bench--diagnostic-log nil)
  (let ((path (expand-file-name
               (or output-file "~/fussy-perf-interactive-report.txt"))))
    (message "[fussy-perf-bench-interactive] measuring fussy...")
    (let ((fussy-r (fussy-perf-bench-interactive--measure-style 'fussy)))
      (message "[fussy-perf-bench-interactive] measuring flex...")
      (let ((flex-r (fussy-perf-bench-interactive--measure-style 'flex)))
        (with-temp-buffer
          (insert (format "fussy-perf-bench-interactive  generated at %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S %z")))
          (insert (format "Sequence : %S\n" fussy-perf-bench-typing-sequence))
          (insert (format "Iters    : %d\n" fussy-perf-bench-interactive-iters))
          (insert (format "Emacs    : %s\n" emacs-version))
          (insert (format "CAPFs    : %S\n"
                          completion-at-point-functions))
          (insert (format "Frontends: %S\n"
                          (and (boundp 'company-frontends) company-frontends)))
          (insert (format "completion-styles (global): %S\n" completion-styles))
          (insert "\nEach per-char ms is the wall-clock from before pre-command-hook\n")
          (insert "through after `redisplay t' — i.e. the full work a single keystroke\n")
          (insert "would do in your real session, popup draw included.\n")
          (fussy-perf-bench-interactive--write-style (current-buffer) fussy-r)
          (fussy-perf-bench-interactive--write-style (current-buffer) flex-r)
          (fussy-perf-bench-interactive--write-comparison
           (current-buffer) fussy-r flex-r)
          (insert "\n=== DIAGNOSTIC LOG (warm-up state, one trace per style) ===\n")
          (dolist (line (nreverse fussy-perf-bench--diagnostic-log))
            (insert line "\n"))
          (write-region (point-min) (point-max) path nil 'quiet))
        (message "[fussy-perf-bench-interactive] done. Report: %s" path)
        (when (called-interactively-p 'interactive)
          (find-file path))
        path))))

;;; --- Profiler ---------------------------------------------------------------

(defun fussy-perf-bench--profile (buf style query iters)
  (with-current-buffer buf
    (fussy-perf-bench--insert-section
     (format "PROFILER: style=%S query=%S iters=%d" style query iters)))
  (with-temp-buffer
    (lisp-interaction-mode)
    (insert "(" query ")")
    (goto-char (1- (point)))
    (let ((completion-styles (list style)))
      (company-mode 1)
      (let ((company--manual-now t))
        (ignore-errors (company-manual-begin)))
      (unless company-backend
        (setq-local company-backend 'company-capf))
      (let* ((pi (ignore-errors (company-call-backend 'prefix)))
             (prefix (cond ((stringp pi) pi)
                           ((consp pi) (or (car pi) query))
                           (t query)))
             (suffix (cond ((and (consp pi) (stringp (cdr pi))) (cdr pi))
                           ((and (consp pi) (consp (cdr pi))
                                 (stringp (cadr pi)))
                            (cadr pi))
                           (t ""))))
        (setq company-prefix prefix)
        (profiler-reset)
        (profiler-start 'cpu)
        (unwind-protect
            (dotimes (_ iters)
              (company--fetch-candidates prefix suffix))
          (profiler-stop)))
      (ignore-errors (company-abort))
      (let* ((cpu-log (profiler-cpu-log))
             (rep (and cpu-log (profiler-report-cpu))))
        (cond
         ((not cpu-log)
          (with-current-buffer buf
            (insert "  (no CPU samples)\n")))
         ((not rep)
          (with-current-buffer buf
            (insert "  (profiler-report-cpu returned nil)\n")))
         ((bufferp rep)
          (with-current-buffer rep
            (let ((pass 0) (keep-going t))
              (while (and keep-going (< pass 8))
                (cl-incf pass)
                (goto-char (point-min))
                (let ((any-expanded nil))
                  (while (not (eobp))
                    (when (let ((eol (line-end-position)))
                            (save-excursion
                              (beginning-of-line)
                              (re-search-forward "\\+ " eol t)))
                      (when (ignore-errors (profiler-report-expand-entry) t)
                        (setq any-expanded t)))
                    (forward-line 1))
                  (unless any-expanded (setq keep-going nil)))))
            (let ((text (buffer-substring-no-properties
                         (point-min) (point-max))))
              (with-current-buffer buf
                (insert (if (> (length text) 12000)
                            (concat (substring text 0 12000)
                                    "\n  ... (truncated)\n")
                          text))
                (insert "\n"))))
          (kill-buffer rep)))))))

;;; --- Public entry point -----------------------------------------------------

;;;###autoload
(defun fussy-perf-bench-run (style output-file)
  "Run the diagnostic for STYLE and write the report to OUTPUT-FILE.
STYLE is a symbol like `fussy' or `flex'."
  (let ((buf (generate-new-buffer "*fussy-perf-bench*")))
    (with-current-buffer buf
      (insert (format "fussy-perf-bench (%S) generated at %s\n"
                      style (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (insert (format "queries: %S, iters/query: %d, profile iters: %d\n"
                      fussy-perf-bench-queries
                      fussy-perf-bench-iters
                      fussy-perf-bench-profile-iters)))
    (fussy-perf-bench--insert-env buf style)
    (fussy-perf-bench--insert-fussy-vars buf)
    (fussy-perf-bench--insert-fzf-vars buf)
    (fussy-perf-bench--insert-company-config buf)
    (fussy-perf-bench--insert-completion-config buf)
    (fussy-perf-bench--insert-gc-info buf)
    (fussy-perf-bench--insert-advice buf)
    (fussy-perf-bench--insert-capf-context buf)

    (message "[fussy-perf-bench %S] per-keystroke pass..." style)
    (fussy-perf-bench--per-keystroke buf style)

    (message "[fussy-perf-bench %S] typing-sequence pass..." style)
    (fussy-perf-bench--typing-sequence-pass buf style)

    (message "[fussy-perf-bench %S] profiler pass (%S/s)..." style style)
    (fussy-perf-bench--profile buf style "s" fussy-perf-bench-profile-iters)
    (message "[fussy-perf-bench %S] profiler pass (%S/setq)..." style style)
    (fussy-perf-bench--profile buf style "setq" fussy-perf-bench-profile-iters)

    (with-current-buffer buf
      (write-region (point-min) (point-max) output-file nil 'quiet))
    (kill-buffer buf)
    (message "[fussy-perf-bench %S] done. Report fragment: %s"
             style output-file)
    output-file))

(provide 'fussy-perf-bench)
;;; fussy-perf-bench.el ends here
