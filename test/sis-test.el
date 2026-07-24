;;; sis-test.el --- Tests for sis.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sis)

(defmacro sis-test-with-backend (authority &rest body)
  "Run BODY with a fake input source backend using AUTHORITY."
  (declare (indent 1) (debug t))
  `(let* ((get-count 0)
          (set-values nil)
          (sis-input-source-state-authority ,authority)
          (sis-respect-application-focus nil)
          (sis-auto-refresh-seconds 0.01)
          (sis-english-source "1")
          (sis-other-source "2")
          (sis-change-hook nil)
          (sis--ism t)
          (sis--ism-inited t)
          (sis--current nil)
          (sis--previous nil)
          (sis--auto-refresh-timer nil)
          (sis--auto-refresh-manager-timer nil)
          (sis--auto-refresh-timer-scale 1)
          (sis-auto-refresh-mode nil)
          (sis-global-cursor-color-mode nil)
          (sis-global-respect-mode nil)
          (sis-do-get (lambda ()
                        (setq get-count (1+ get-count))
                        "1"))
          (sis-do-set (lambda (source)
                        (push source set-values))))
     (with-temp-buffer
       (setq sis--for-buffer nil
             sis--for-buffer-locked nil)
       ,@body)))

(ert-deftest sis-test-default-state-options ()
  (should (eq (default-value 'sis-input-source-state-authority) 'external))
  (should-not (default-value 'sis-respect-application-focus)))

(ert-deftest sis-test-external-save-adopts-backend-state ()
  (sis-test-with-backend 'external
    (setq sis-do-get (lambda ()
                       (setq get-count (1+ get-count))
                       "2"))
    (sis--save-to-buffer)
    (should (= get-count 1))
    (should (eq sis--current 'other))
    (should (eq sis--for-buffer 'other))))

(ert-deftest sis-test-external-get-adopts-backend-state ()
  (sis-test-with-backend 'external
    (setq sis-do-get (lambda ()
                       (setq get-count (1+ get-count))
                       "2"))
    (should (equal (sis-get) "2"))
    (should (= get-count 1))
    (should (eq sis--current 'other))
    (should (eq sis--for-buffer 'other))))

(ert-deftest sis-test-authoritative-respect-start-does-not-read ()
  (sis-test-with-backend 'sis
    (let ((sis-respect-start 'english)
          (sis-respect-prefix-and-buffer nil))
      (unwind-protect
          (progn
            (sis-global-respect-mode 1)
            (should (eq sis--current 'english))
            (should (equal (nreverse set-values) '("1")))
            (should (= get-count 0))
            (should-not sis-auto-refresh-mode))
        (sis-global-respect-mode -1)))))

(ert-deftest sis-test-authoritative-switch-is-immediate-and-queryless ()
  (sis-test-with-backend 'sis
    (let ((hook-count 0))
      (setq sis--current 'english
            sis--for-buffer 'english
            sis-change-hook (list (lambda ()
                                    (setq hook-count (1+ hook-count)))))
      (sis-switch)
      (should (eq sis--current 'other))
      (should (eq sis--for-buffer 'other))
      (should (= hook-count 1))
      (should (equal set-values '("2")))
      (should (= get-count 0)))))

(ert-deftest sis-test-authoritative-save-respects-buffer-lock ()
  (sis-test-with-backend 'sis
    (setq sis--current 'other
          sis--for-buffer 'english)
    (sis--save-to-buffer)
    (should (eq sis--for-buffer 'other))
    (setq sis--current 'english
          sis--for-buffer-locked t)
    (sis--save-to-buffer)
    (should (eq sis--for-buffer 'other))
    (should (= get-count 0))))

(ert-deftest sis-test-authoritative-prefix-round-trip-is-queryless ()
  (sis-test-with-backend 'sis
    (let ((sis--prefix-handle-stage 'prefix)
          (sis--respect-force-restore nil)
          (sis--respect-go-english nil)
          (sis--respect-post-cmd-timer nil)
          (sis--prefix-override-map-enable t)
          (sis--buffer-before-command (current-buffer))
          (this-command #'ignore))
      (setq sis--current 'other
            sis--for-buffer 'other)
      (sis--respect-pre-command-handler)
      (should (eq sis--current 'english))
      (should (eq sis--for-buffer 'other))
      (should sis--for-buffer-locked)
      (setq sis--respect-force-restore t)
      (sis--respect-post-cmd-timer-fn)
      (should (eq sis--current 'other))
      (should (eq sis--for-buffer 'other))
      (should-not sis--for-buffer-locked)
      (should (equal (nreverse set-values) '("1" "2")))
      (should (= get-count 0)))))

(ert-deftest sis-test-authoritative-get-is-diagnostic-only ()
  (sis-test-with-backend 'sis
    (let ((hook-count 0))
      (setq sis-do-get (lambda ()
                         (setq get-count (1+ get-count))
                         "2")
            sis--current 'english
            sis--for-buffer 'english
            sis-change-hook (list (lambda ()
                                    (setq hook-count (1+ hook-count)))))
      (should (equal (sis-get) "2"))
      (should (= get-count 1))
      (should (eq sis--current 'english))
      (should (eq sis--for-buffer 'english))
      (should (= hook-count 0)))))

(ert-deftest sis-test-authoritative-set-error-does-not-publish-state ()
  (sis-test-with-backend 'sis
    (let ((hook-count 0))
      (setq sis--current 'english
            sis--for-buffer 'english
            sis-change-hook (list (lambda ()
                                    (setq hook-count (1+ hook-count))))
            sis-do-set (lambda (_) (error "set failed")))
      (should-error (sis-set-other) :type 'error)
      (should (eq sis--current 'english))
      (should (eq sis--for-buffer 'english))
      (should (= hook-count 0))
      (should (= get-count 0)))))

(ert-deftest sis-test-external-set-keeps-upstream-error-ordering ()
  (sis-test-with-backend 'external
    (setq sis--current 'english
          sis--for-buffer 'english
          sis-do-set (lambda (_) (error "set failed")))
    (should-error (sis-set-other) :type 'error)
    (should (eq sis--current 'other))
    (should (eq sis--for-buffer 'other))))

(ert-deftest sis-test-reapply-does-not-change-authoritative-state ()
  (sis-test-with-backend 'sis
    (let ((hook-count 0))
      (setq sis--current 'other
            sis--for-buffer 'english
            sis--for-buffer-locked t
            sis-change-hook (list (lambda ()
                                    (setq hook-count (1+ hook-count)))))
      (sis-reapply-current-input-source)
      (should (equal set-values '("2")))
      (should (eq sis--current 'other))
      (should (eq sis--for-buffer 'english))
      (should sis--for-buffer-locked)
      (should (= hook-count 0))
      (should (= get-count 0)))))

(ert-deftest sis-test-authoritative-auto-refresh-cannot-start ()
  (sis-test-with-backend 'sis
    (sis-auto-refresh-mode 1)
    (should-not sis-auto-refresh-mode)
    (should-not sis--auto-refresh-manager-timer)
    (should-not sis--auto-refresh-timer)
    (should (= get-count 0))))

(ert-deftest sis-test-authoritative-stale-refresh-does-not-read-or-reschedule ()
  (sis-test-with-backend 'sis
    (setq sis-auto-refresh-mode t)
    (sis--auto-refresh-timer-function)
    (should (= get-count 0))
    (should-not sis--auto-refresh-timer)))

(ert-deftest sis-test-authoritative-cursor-mode-does-not-enable-refresh ()
  (sis-test-with-backend 'sis
    (unwind-protect
        (progn
          (sis-global-cursor-color-mode 1)
          (should-not sis-auto-refresh-mode)
          (should (= get-count 0)))
      (sis-global-cursor-color-mode -1))))

(ert-deftest sis-test-disabled-application-focus-has-no-side-effects ()
  (sis-test-with-backend 'sis
    (setq sis--current 'other
          sis--for-buffer 'other)
    (sis--respect-focus-out-handler)
    (sis--respect-focus-in-handler)
    (should (eq sis--current 'other))
    (should (eq sis--for-buffer 'other))
    (should-not sis--for-buffer-locked)
    (should-not set-values)
    (should (= get-count 0))))

(ert-deftest sis-test-application-focus-registration-is-opt-in-and-symmetric ()
  (sis-test-with-backend 'sis
    (let ((after-focus-change-function #'ignore)
          (sis-respect-application-focus t)
          (sis-respect-start nil))
      (cl-letf (((symbol-function 'frame-focus-state)
                 (lambda (&optional _) nil)))
        (setq sis--current 'other
              sis--for-buffer 'other)
        (unwind-protect
            (progn
              (sis-global-respect-mode 1)
              (funcall after-focus-change-function)
              (should (eq sis--current 'english))
              (should (equal set-values '("1"))))
          (sis-global-respect-mode -1))
        (setq sis--current 'other
              sis--for-buffer 'other
              set-values nil)
        (funcall after-focus-change-function)
        (should (eq sis--current 'other))
        (should-not set-values)))))

(ert-deftest sis-test-authoritative-focus-restore-remains-available ()
  (sis-test-with-backend 'sis
    (let ((sis-respect-application-focus t))
      (setq sis--current 'other
            sis--for-buffer 'other)
      (sis--respect-focus-out-handler)
      (should (eq sis--current 'english))
      (should (eq sis--for-buffer 'other))
      (should sis--for-buffer-locked)
      (sis--respect-focus-in-handler)
      (should (eq sis--current 'other))
      (should-not sis--for-buffer-locked)
      (should (equal (nreverse set-values) '("1" "2")))
      (should (= get-count 0)))))

(ert-deftest sis-test-authoritative-setter-only-backend ()
  (let* ((sis-input-source-state-authority 'sis)
        (sis-external-ism nil)
        (sis--ism nil)
        (sis--ism-inited nil)
        (sis--current nil)
        (sis--previous nil)
        (sis--for-buffer nil)
        (sis--for-buffer-locked nil)
        (sis-english-source "1")
        (sis-other-source "2")
        (sis-do-get nil)
        (set-values nil)
        (sis-do-set (lambda (source)
                      (push source set-values))))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (sis--set 'english)
      (should (eq sis--ism t))
      (should (eq sis--current 'english))
      (should (equal set-values '("1")))
      (should-error (sis-get) :type 'user-error))))

(ert-deftest sis-test-external-automatic-read-without-backend-is-no-op ()
  (let ((sis-input-source-state-authority 'external)
        (sis-external-ism nil)
        (sis--ism nil)
        (sis--ism-inited nil)
        (sis--current 'other)
        (sis--for-buffer 'other)
        (sis-do-get nil)
        (sis-do-set nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
      (should-not (sis--get))
      (should-not (sis--save-to-buffer))
      (should (eq sis--current 'other))
      (should (eq sis--for-buffer 'other)))))

(ert-deftest sis-test-authoritative-switch-requires-initialized-state ()
  (sis-test-with-backend 'sis
    (should-error (sis-switch) :type 'user-error)
    (should-not set-values)
    (should (= get-count 0))))

(provide 'sis-test)
;;; sis-test.el ends here
