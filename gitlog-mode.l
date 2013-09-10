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
  (sha1	nil)
  (reflog-pointer nil)
  (reflog-msg nil)
  (author	nil)
  (mail		nil)
  (unix-time	nil)
  (description	nil)
  (checked	nil)
  )


; -----------------------------------------------------------------------------
; 変数定義
; -----------------------------------------------------------------------------
(defvar *gitlog-git-executable-path* "C:/Program Files/Git/bin/git.exe")


; -----------------------------------------------------------------------------
; 関数定義
; -----------------------------------------------------------------------------
(defun set-window-with-buffer-name (_buffer-name)
  "@brief	set-window をバッファ名で行う。"
  (set-window (get-buffer-window (find-buffer _buffer-name)))
  )

(defun convert-char-code (file-name encode)
  "@brief	指定したファイルのエンコードを指定したエンコードに変換する。"
  (let (different)
	(with-open-file (file file-name :encoding :text)
	  (setf different (not (equal encode (detect-char-encoding file))))
	  )
	(when different
	  (find-file file-name)
	  (set-buffer-fileio-encoding encode)
	  (insert " ")
	  (save-buffer)
	  (delete-backward-char)
	  (save-buffer)
	  (delete-buffer (selected-buffer))
	  )
	)
  )

(defun convert-unix-time-to-utc (unix-time)
  "UNIX時間をユニバーサルタイムに変換する。"
  (let ((unix-epoch (encode-universal-time 0 0 0 1 1 1970))
		utc-epoch
		)
	(+ unix-time unix-epoch)
	)
  )

(defun _gitlog-parse-log (log-file)
  "
@brief	log の結果をパースする。
@param log-file	ログファイルのストリーム
@return	log-file をパースして得たコミットオブジェクトのリスト
"
  (let ((objects (list))
		line splited sha1 author mail unix-time description)
	(loop
	  (setf line (read-line log-file nil nil))
	  (when (not line)
		(return)
		)
	  (setf splited (split-string line " "))
	  (setf unix-time (parse-integer (pop splited)))
	  (setf sha1 (pop splited))
	  (setf author (pop splited))
	  (setf mail (pop splited))
	  ; body 解析
	  (loop
		(setf line (read-line log-file nil nil))
		(if (= 0 (length line))
			(return)
		  )
		(setf description (concat description line "\n"))
		)
	  (push (make-git-commit-object
			 :sha1 sha1
			 :author author
			 :mail mail
			 :unix-time unix-time
			 :description description)
			objects)
	  (setf description "")
	  )
	(reverse objects)
	)
  )

(defun _gitlog-parse-reflog (log-file)
  "
@brief	reflog の結果をバースする。
@param log-file	ログファイルのストリーム
@return	log-file をパースして得たコミットオブジェクトのリスト
"
  (let ((objects (list))
		)
	(loop
	  (let (line splited sha1 reflog-pointer reflog-msg
				 author mail unix-time action description)
		(setf line (read-line log-file nil nil))
		(when (not line)
		  (return)
		  )
		(setf splited (split-string line " "))
		(setf unix-time (parse-integer (pop splited)))
		(setf sha1 (pop splited))
		(setf author (pop splited))
		(setf mail (pop splited))
		(setf sha1 (nth 1 (split-string line " ")))
		(setf reflog-pointer (pop splited))
		(setf reflog-msg (pop splited))
		; body 解析
		(loop
		  (setf line (read-line log-file nil nil))
		  (if (= 0 (length line))
			  (return)
			)
		  (setf description (concat description line "\n"))
		  )
		(push (make-git-commit-object
			   :sha1 sha1
			   :reflog-pointer reflog-pointer
			   :reflog-msg reflog-msg
			   :author author
			   :mail mail
			   :unix-time unix-time
			   :description description)
			  objects)
		)
	  )
	(reverse objects)
	)
  )

(defun _gitlog-make-general-keymap ()
  "@brief	共通キーマップ作成"
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap #\SPC '_gitlog-mark-toggle)
	(define-key keymap #\n 'gitlog-show-next)
	(define-key keymap #\C-n 'gitlog-show-next)
	(define-key keymap #\p 'gitlog-show-prev)
	(define-key keymap #\C-p 'gitlog-show-prev)
	keymap
	)
  )

(defun _gitlog-define-log-mode-keymap (keymap)
  "@brief	log 用のキーマップ定義"
  (define-key keymap #\RET '_gitlog-compare-commits)
  (define-key keymap #\c '_gitlog-checkout)
  )

(defun _gitlog-define-reflog-mode-keymap (keymap)
  "@brief	reflog 用のキーマップ定義"
  (define-key keymap #\r '_gitlog-reset)
  )

(defun _gitlog-mark-toggle ()
  "@brief	ログペインのコミットのマークをトグルする。"
  (interactive)
  (let ((commit (_gitlog-current-commit)))
	(setf (git-commit-object-checked commit) (if (git-commit-object-checked commit) nil t))
	)
  (_gitlog-update-logpane gitlog-commit-objects)
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
			  (dolist (commit gitlog-commit-objects)
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
							(git-commit-object-sha1 (nth 0 checkeds))
							(git-commit-object-sha1 (nth 1 checkeds))
							gitlog-target-file-name)
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

(defun _gitlog-make-date-string-from-unix-time (unix-time)
  "UNIX時間を書式に従った文字列として返す。"
  (format-date-string "%Y/%m/%d %H:%M %z" (convert-unix-time-to-utc unix-time))
  )

(defun _gitlog-create-log-text (object)
  "
@brief	logモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param object	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A~%"
		  (git-commit-object-checked object)
		  (_gitlog-make-date-string-from-unix-time
		   (git-commit-object-unix-time object))
		  (git-commit-object-author object))
  )

(defun _gitlog-create-reflog-text (object)
  "
@brief	reflogモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param object	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A	~A ~A~%"
		  (git-commit-object-checked object)
		  (git-commit-object-reflog-pointer object)
		  (_gitlog-make-date-string-from-unix-time
		   (git-commit-object-unix-time object))
		  (git-commit-object-author object)
		  (git-commit-object-reflog-msg object)
		  )
  )

(defun _gitlog-update-logpane (commit-objects)
  "@brief	ログペイン更新。"
  (set-window-with-buffer-name "Git log")
  (setf buffer-read-only nil)
  (let ((cursor-position (current-line-number))
		(text-creators '(("log" . _gitlog-create-log-text)
						 ("reflog" . _gitlog-create-reflog-text)))
		text-creator
		)
	(setf text-creator (cdr (assoc gitlog-mode text-creators :test 'string=)))
	(erase-buffer (selected-buffer))
	(dolist (object commit-objects)
	  (insert (funcall text-creator object))
	  )
	(delete-backward-char)
	(setf buffer-read-only t)
	(if (<= cursor-position (length gitlog-commit-objects))
		(goto-line cursor-position)
	  )
	)
  )

(defun _gitlog-current-commit ()
  "@brief	カーソルがある行に対応したコミットオブジェクトを取得。"
  (nth (- (current-line-number) 1) gitlog-commit-objects)
  )

(defun _gitlog-detail-open ()
  "@brief	コミットログペイン更新。"
  (let ((commit (_gitlog-current-commit)))
	(set-window-with-buffer-name "Git description")
	(setf buffer-read-only nil)
	(erase-buffer (selected-buffer))
	(insert (format
			 nil "SHA-1	~A~%Author	~A~%Mail	~A~%Date	~A~%~%~A~%"
			 (git-commit-object-sha1 commit)
			 (git-commit-object-author commit)
			 (git-commit-object-mail commit)
			 (_gitlog-make-date-string-from-unix-time
			  (git-commit-object-unix-time commit))
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
		 (sha1 (git-commit-object-sha1 commit))
		 )
	(when (no-or-yes-p "チェックアウトします。よろしいですか？")
	  (call-process (format nil "~A checkout ~A ~A"
							*gitlog-git-executable-path* sha1
							gitlog-target-file-name))
	  )
	)
  )

(defun _gitlog-reset ()
  "カーソルがある行が指すコミットでリセットする。"
  (interactive)
  (let* ((commit (_gitlog-current-commit))
		 (reflog-pointer (git-commit-object-reflog-pointer commit))
		 answer mode
		 )
	(loop
	  (setf answer (read-string
					(format nil "~A でリセットします。[y, n]?"
							reflog-pointer)))
	  (cond
	   ((string= "y" answer)
		(return)
		)
	   ((string= "n" answer)
		(message "リセットを中止しました。")
		(return-from _gitlog-reset)
		)
	   )
	  )
	(loop
	  (setf answer (read-string "モードを指定してください。[s, h, q, ?]?"))
	  (cond
	   ((string= "s" answer)
		(setf mode "soft")
		(return)
		)
	   ((string= "h" answer)
		(setf mode "hard")
		(return)
		)
	   ((string= "q" answer)
		(message "リセットを中止しました。")
		(return-from _gitlog-reset)
		)
	   ((string= "?" answer)
		(msgbox "s - soft\nh - hard\nq - quit")
		)
	   )
	  )
	(call-process (format nil "~A reset --~A ~A"
						  *gitlog-git-executable-path* mode reflog-pointer))
	)
  (message "reset has done.")
  )

(defun gitlog-mode (&optional log-file-name)
  "@param log-file-name	ログファイル名。フルパスである必要がある。
"
  (interactive)
  (let ((logbuffer (selected-buffer))
		)

	; 開発用にログファイルを指定したい場合は以下をコメントアウトする。
;; 	(setf log-file-name "D:/myRoot/opt/xyzzy/site-lisp/siteinitlog.gitlog")

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
	(use-keymap (_gitlog-make-general-keymap))
	(make-local-variable 'gitlog-exec-directory)
	; diff やチェックアウトで使用する。
	(make-local-variable 'gitlog-target-file-name)
	(make-local-variable 'gitlog-commit-objects)
	(make-local-variable 'gitlog-mode)

	; log-file-name が sjis じゃない場合に、sjis に変換する。
	(convert-char-code log-file-name *encoding-sjis*)

	(let (file)
	  (with-open-file (file log-file-name :encoding :text)
		(setf gitlog-mode (read-line file nil nil))
		(setf gitlog-exec-directory (read-line file nil nil))
		(cond
		 ((string= "log" gitlog-mode)
		  (setf gitlog-target-file-name (read-line file nil nil))
		  )
		 )
		; シェルスクリプトによるパスの出力で、Linux 形式ではなく Windows 形式で
		; 吐き出される現象が確認されたので、Linux 形式の時のみ Windows 形式に
		; 変換するようにした。
		(when (char= #\/ (char gitlog-exec-directory 0))
		  (setf gitlog-exec-directory (concat (substring gitlog-exec-directory 1 2)
											  ":"
											  (substring gitlog-exec-directory 2)))
		  )
		(let ((mode-stuff-containers
			   '(("log" .
				  (_gitlog-define-log-mode-keymap _gitlog-parse-log))
				 ("reflog" .
				  (_gitlog-define-reflog-mode-keymap _gitlog-parse-reflog))
				 )
			   mode-stuffs keymap-definer parser)
			  )
		  (setf mode-stuffs
				(cdr (assoc gitlog-mode mode-stuff-containers :test 'string=)))
		  (setf keymap-definer (nth 0 mode-stuffs))
		  (setf parser (nth 1 mode-stuffs))
		  (funcall keymap-definer (local-keymap (selected-buffer)))
		  (setf gitlog-commit-objects (funcall parser file))
		  )
		)
	  )
	(_gitlog-update-logpane gitlog-commit-objects)
	(_gitlog-detail-open)
	)
  )

; エントリポイント
(when si:*command-line-args*
  (let (error-message
		(log-file-name (nth 0 si:*command-line-args*))
		)
	(setf error-message
		  (block top
			(when (string-match ".gitlog$" log-file-name)
			  (unless (file-exist-p log-file-name)
				(return-from top "Specified file is not exist")
				)
			  (pop si:*command-line-args*)
			  (gitlog-mode log-file-name)
			  )
			nil
			)
		  )
	(when error-message
	  (msgbox "Error!~%~A" error-message)
	  )
	)
  )
