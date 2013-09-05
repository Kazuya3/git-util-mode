;;; -*- Mode: Lisp; Package: EDITOR -*-
;;; git で、特定のファイルのログに関する機能を提供する。
;;;
;;; ベースは Netinstaller の apps.l
;;;
;;; ■初期設定
;;; .bashrc に同梱の gitlogtool.sh をエイリアスとして設定する。
;;;
;;; 例）
;;; alias gitlogtool='gitlogtool.sh'（フルパス）
;;;
;;; ■使い方
;;; 上記のような設定をした場合、実行コマンドは以下のようになる。
;;; gitlogtool file-name


(provide "gitlog")
(in-package "editor")


; -----------------------------------------------------------------------------
; エキスポート
; -----------------------------------------------------------------------------
; 変数
(export '(*gitlog-git-executable-path*))

; 関数
(export '(gitlog-mode))


; -----------------------------------------------------------------------------
; 構造体定義
; -----------------------------------------------------------------------------
(defstruct git-commit-object
  (sha-1	nil)
  (author	nil)
  (mail		nil)
  (date		nil)
  (description	nil)
  (checked	nil)
  )


; -----------------------------------------------------------------------------
; 変数定義
; -----------------------------------------------------------------------------
(defvar *gitlog-git-path* nil)
(defvar *gitlog-mode-map* nil)
(unless *gitlog-mode-map*
  (setf *gitlog-mode-map* (make-sparse-keymap))
  (define-key *gitlog-mode-map* #\RET '_gitlog-compare-commits)
  (define-key *gitlog-mode-map* #\SPC '_gitlog-mark-toggle)
  (define-key *gitlog-mode-map* #\n 'gitlog-show-next)
  (define-key *gitlog-mode-map* #\p 'gitlog-show-prev)
  (define-key *gitlog-mode-map* #\c '_gitlog-checkout)
  )

(defvar *gitlog-git-executable-path* "C:/Program Files/Git/bin/git.exe")


; -----------------------------------------------------------------------------
; 関数定義
; -----------------------------------------------------------------------------
(defun set-window-with-buffer-name (_buffer-name)
  (set-window (get-buffer-window (find-buffer _buffer-name)))
  )

(defun _gitlog-mark-toggle ()
  (interactive)
  (let ((commit (_gitlog-current-commit)))
	(setf (git-commit-object-checked commit) (if (git-commit-object-checked commit) nil t))
	)
  (_git-update-log git-commit-objects)
  )

(defun _gitlog-compare-commits ()
  "@brief	マークのついたコミット間の際を Winmerge で表示。"
  (interactive)
  (let (error-message)
	(setf error-message
		  (block top
			(let ((checked-num 0)
				  (checkeds (list))
				  command
				  )
			  (dolist (commit git-commit-objects)
				(when (git-commit-object-checked commit)
				  (push commit checkeds)
				  (when (< 2 (incf checked-num))
					(return-from top
					  "Comparation more than three is not supported.")
					)
				  )
				)
			  (when (< (length checkeds) 2)
				(return-from top
				  "Target what is selected is no more than one.")
				)
			  (setf command
					(format nil "~A difftool ~A ~A ~A"
							*gitlog-git-executable-path*
							(git-commit-object-sha-1 (nth 0 checkeds))
							(git-commit-object-sha-1 (nth 1 checkeds))
							gitlog-target)
					)
			  (call-process command
							:exec-directory gitlog-exec-directory
							)
			  )
			nil
			)
		  )
	(if error-message
		(msgbox "Error!~%~A" error-message)
	  )
	)
  )

(defun _git-update-log (commit-objects)
  "@brief	ログペイン更新。"
  (set-window-with-buffer-name "Git log")
  (setf buffer-read-only nil)
  (let ((cursor-position (current-line-number)))
	(erase-buffer (selected-buffer))
	(dolist (object commit-objects)
	  (insert (format nil "[~:[ ~;*~]] ~A	~A~%"
					  (git-commit-object-checked object)
					  (git-commit-object-date object)
					  (git-commit-object-author object))
			  )
	  )
	(delete-backward-char)
	(setf buffer-read-only t)
	(if (<= cursor-position (length git-commit-objects))
		(goto-line cursor-position)
	  )
	)
  )

(defun _gitlog-current-commit ()
  "@brief	カーソルがある行に対応したコミットオブジェクトを取得。"
  (nth (- (current-line-number) 1) git-commit-objects)
  )

(defun _gitlog-detail-open ()
  "@brief	コミットログペイン更新。"
  (let ((commit (_gitlog-current-commit)))
	(set-window-with-buffer-name "Git description")
	(setf buffer-read-only nil)
	(erase-buffer (selected-buffer))
	(insert (format nil "SHA-1	~A~%Author	~A ~A~%Date	~A~%~%~A~%"
					(git-commit-object-sha-1 commit)
					(git-commit-object-author commit)
					(git-commit-object-mail commit)
					(git-commit-object-date commit)
					(git-commit-object-description commit)
					)
			)
	)
  (setf buffer-read-only t)
  (set-window-with-buffer-name "Git log")
  )

(defun gitlog-show-next (&optional (lines 1))
  "@brief	コミットログペインの更新を伴ったカーソルの移動。"
  (interactive)
  (or (forward-line lines) (return-from gitlog-show-next))
  (_gitlog-detail-open))

(defun gitlog-show-prev ()
  "@brief	コミットログペインの更新を伴ったカーソルの移動。"
  (interactive)
  (gitlog-show-next -1))

(defun _gitlog-setup-mode-line ()
  "@brief	ウィンドウのステータス表示形式を変更。"
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (format nil "%b"))
  )

(defun _gitlog-checkout()
  (interactive)
  "カーソルがある行でチェックアウトする。"
  (let* ((commit (_gitlog-current-commit))
		 (sha-1 (git-commit-object-sha-1 commit))
		 )
	(when (no-or-yes-p "チェックアウトします。よろしいですか？")
	  (call-process (format nil "~A checkout ~A ~A"
							*gitlog-git-executable-path* sha-1 gitlog-target))
	  )
	)
  )

(defun gitlog-mode (&optional logfile)
  "@param logfile	ログファイル名。フルパスである必要がある。
"
  (interactive)
  (let ((logbuffer (selected-buffer))
		)

	; 開発用にログファイルを指定したい場合は以下をコメントアウトする。
;; 	(setf logfile "D:/myRoot/opt/xyzzy/site-lisp/siteinitlog.gitlog")

	(when (find-pseudo-frame "Git")
	  (select-pseudo-frame (find-pseudo-frame "Git"))
	  (delete-pseudo-frame)
	  )
	(new-pseudo-frame "Git")
	(split-window)
	(split-window-vertically 80)

	; 操作法ペイン初期化
	(switch-to-buffer "Git operating")
	(setup-temp-buffer (selected-buffer))
	(_gitlog-setup-mode-line)
	(insert-file (merge-pathnames "gitlog-mode.readme" *etc-path*))
	(setf buffer-read-only t)

	; ログペイン作成
	(switch-to-buffer-other-window "Git log")
	(setup-temp-buffer (selected-buffer))

	; デスクリプションペイン初期化
	(switch-to-buffer-other-window "Git description")
	(setup-temp-buffer (selected-buffer))
	(make-local-variable 'mode-line-format)
	(_gitlog-setup-mode-line)

	; ログペイン初期化
	(set-window-with-buffer-name "Git log")
	(setup-temp-buffer (selected-buffer))
	(setf buffer-read-only t)
	(set-local-window-flags (selected-buffer)
							*window-flag-line-number* nil)
	(set-local-window-flags (selected-buffer)
							*window-flag-newline* nil)
	(set-local-window-flags (selected-buffer)
							*window-flag-eof* nil)
	(set-local-window-flags (selected-buffer)
							*window-flag-cursor-line* t)
	(set-local-window-flags (selected-buffer)
							*window-flag-ruler* nil)
	(set-local-window-flags (selected-buffer)
							*window-flag-just-inverse* t)
	(set-buffer-fold-width nil)
	(_gitlog-setup-mode-line)
	(use-keymap *gitlog-mode-map*)
	(make-local-variable 'gitlog-exec-directory)
	(make-local-variable 'gitlog-target)
	(make-local-variable 'git-commit-objects)
	(setf git-commit-objects (list))
	(let (notsjis)
	  (with-open-file (file logfile :encoding :text)
		(setf notsjis (not (equal *encoding-sjis* (detect-char-encoding file))))
		)
	  (when notsjis
		(find-file logfile)
		(set-buffer-fileio-encoding *encoding-sjis*)
		(insert " ")
		(save-buffer)
		(delete-backward-char)
		(save-buffer)
		(delete-buffer (selected-buffer))
		)
	  )
	(with-open-file (file logfile :encoding :text)
	  (setf gitlog-exec-directory (read-line file nil nil))
	  ; シェルスクリプトによるパスの出力で、Linux 形式ではなく Windows 形式で
	  ; 吐き出される現象が確認されたので、Linux 形式の時のみ Windows 形式に
	  ; 変換するようにした。
	  (when (char= #\/ (char gitlog-exec-directory 0))
		(setf gitlog-exec-directory (concat (substring gitlog-exec-directory 1 2)
											":"
											(substring gitlog-exec-directory 2)))
		)
	  (setf gitlog-target (read-line file nil nil))
	  (let (line splited sha1 author mail date description)
		(while t
		  (setf line (read-line file nil nil))
		  (if (not line)
			  (return)
			)
		  (if (not (string-match "^commit" line))
			  (return)
			)
		  (setf sha1 (nth 1 (split-string line " ")))
		  (setf line (read-line file nil nil))
		  (setf splited (split-string line " "))
		  (setf author (nth 1 splited))
		  (setf mail (nth 2 splited))
		  (setf line (read-line file nil nil))
		  (setf date (substring line 8))
		  (read-line file nil nil)
		  (while t
			(setf line (read-line file nil nil))
			(if (= 0 (length line))
				(return)
			  )
			(setf description (concat description line "\n"))
			)
		  (push (make-git-commit-object
				 :sha-1 sha1
				 :author author
				 :mail mail
				 :date date
				 :description description)
				git-commit-objects)
		  (setf description "")
		  )
		(setf git-commit-objects (reverse git-commit-objects))
		)
	  )
	(_git-update-log git-commit-objects)
	)
  )

; エントリポイント
(when si:*command-line-args*
  (let (error-message
		(logfile (nth 0 si:*command-line-args*))
		)
	(setf error-message
		  (block top
			(when (string-match ".gitlog$" logfile)
			  (unless (file-exist-p logfile)
				(return-from top "Specified file is not exist")
				)
			  (pop si:*command-line-args*)
			  (gitlog-mode logfile)
			  )
			nil
			)
		  )
	(when error-message
	  (msgbox "Error!~%~A" error-message)
	  )
	)
  )
