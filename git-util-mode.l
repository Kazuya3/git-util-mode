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


(provide "git-util")
(defpackage "git-util" (:use "lisp" "editor"))
(in-package "git-util")


; -----------------------------------------------------------------------------
; エキスポート
; -----------------------------------------------------------------------------
; 変数
(export '(*git-executable-path*))

; 関数
(export '(git-util-mode))


; -----------------------------------------------------------------------------
; 構造体定義
; -----------------------------------------------------------------------------
(defstruct commit
  (splited-info	nil)
  (description	nil)
  (checked	nil)
  (files-status nil)	; 変更種類、ファイル名のタプルのリスト
  )


; -----------------------------------------------------------------------------
; 変数定義
; -----------------------------------------------------------------------------
(defvar *mode* nil)		; 実行中のモードを表す文字列を格納する。
(defvar *commits* nil)	; コミットオブジェクトのリスト
(defvar *git-executable-path* "C:/Program Files/Git/bin/git.exe")


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
		(jst-delta (* -1 (encode-universal-time 0 0 0 1 1 1900)))
		utc-epoch
		)
	(+ unix-time unix-epoch jst-delta)
	)
  )


; -----------------------------------------------------------------------------
; 情報行から情報を取得する関数
; -----------------------------------------------------------------------------
(defun make-formated-date-from-commit (object)
  "
@brief	コミットオブジェクトからフォーマットに従った日時の文字列を取得する。
@param object	コミットオブジェクト
@return	フォーマットに従った日時の文字列
"
  (make-date-string-from-unix-time
   (parse-integer (nth 0 (commit-splited-info object))))
  )

(defun get-sha1-from-commit (object)
  (nth 1 (commit-splited-info object))
  )

(defun get-author-from-commit (object)
  (nth 2 (commit-splited-info object))
  )

(defun get-mail-from-commit (object)
  (nth 3 (commit-splited-info object))
  )

(defun get-reflog-selector-from-commit (object)
  (nth 4 (commit-splited-info object))
  )

(defun get-reflog-subject-from-commit (object)
  (nth 5 (commit-splited-info object))
  )


(defun parse-log (log-file)
  "
@brief	log の結果をパースする。
@param log-file	ログファイルのストリーム
@return	log-file をパースして得たコミットオブジェクトのリスト
"
  (let (line skip-read
			 (objects (list))
			 )
	(loop
	  (let (splited description file-status
					(files-status (list))
					)
		(if skip-read
			(setf skip-read nil)
		  ; else
		  (setf line (read-line log-file nil nil))
		  )
		(when (not line)
		  (return)
		  )
		(setf splited (split-string line "|"))
		; body 解析
		(loop
		  (setf line (read-line log-file nil nil))
		  (if (string-match "^`$" line)
			  (return)
			)
		  (setf description (concat description line "\n"))
		  )
		; 変更ファイル情報解析
		(loop
		  (setf line (read-line log-file nil nil))
		  (cond
		   ((= 0 (length line))
			(return)
			)
		   ; 変更ファイルが無いと情報行を読み込んでしまうため、
		   ; その時は次のループの冒頭での情報行の読み込みをスキップする。
		   ((string-looking-at "^[0-9a-z]+|" line)
			(setf skip-read t)
			(return)
			)
		   )
		  (setf file-status (split-string line "	"))
		  (push `(,(string= "M" (car file-status)) ,(cadr file-status)) files-status)
		  )
		(push (make-commit
			   :splited-info splited
			   :description description
			   :files-status files-status
			   )
			  objects)
		)
	  )
	(reverse objects)
	)
  )

(defun make-general-keymap ()
  "@brief	共通キーマップ作成"
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap #\SPC 'toggle-mark)
	(define-key keymap #\n 'forward-log)
	(define-key keymap #\C-n 'forward-log)
	(define-key keymap #\p 'back-log)
	(define-key keymap #\C-p 'back-log)
	keymap
	)
  )

(defun define-log-mode-keymap (keymap)
  "@brief	log 用のキーマップ定義"
  (define-key keymap #\RET 'compare-commits)
  (define-key keymap #\c 'checkout)
  (define-key keymap #\s 'retroversion)
  )

(defun define-reflog-mode-keymap (keymap)
  "@brief	reflog 用のキーマップ定義"
  (define-key keymap #\r 'reset)
  )

(defun _gitlog-load-log-mode-manual ()
  )

(defun _gitlog-load-reflog-mode-manual ()
  )

(defun setup-manual-pane ()
  "マニュアルペイン設定"
  (save-window-excursion
	(switch-to-buffer "Git operating")
	(setup-temp-buffer (selected-buffer))
	(setup-mode-line)
	(insert-file (merge-pathnames "gitlog/general.txt" *etc-path*))
	(goto-max)
	(cond
	 ((string= "log" *mode*)
	  (insert-file (merge-pathnames "gitlog/log-mode.txt" *etc-path*))
	  )
	 ((string= "reflog" *mode*)
	  (insert-file (merge-pathnames "gitlog/reflog-mode.txt" *etc-path*))
	  )
	 )
	(setf buffer-read-only t)
	)
  )

(defun toggle-mark ()
  "@brief	ログペインのコミットのマークをトグルする。"
  (interactive)
  (let ((object (get-cursor-commit)))
	(setf (commit-checked object) (if (commit-checked object) nil t))
	)
  (update-log-pane *commits*)
  )

(defun compare-commits ()
  "@brief	マークのついたコミット間の際を Winmerge で表示。"
  (interactive)
  (let (error-message)
	(setf error-message
		  (block top
			(let ((checked-num 0)
				  (checkeds (list))
				  command
				  )
			  (dolist (object *commits*)
				(when (commit-checked object)
				  (push object checkeds)
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
							*git-executable-path*
							(get-sha1-from-commit (nth 0 checkeds))
							(get-sha1-from-commit (nth 1 checkeds))
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

(defun make-date-string-from-unix-time (unix-time)
  "UNIX時間を書式に従った文字列として返す。"
  (format-date-string "%Y/%m/%d %H:%M %z" (convert-unix-time-to-utc unix-time))
  )

(defun make-log-text (object)
  "
@brief	logモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param object	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A~%"
		  (commit-checked object)
		  (make-formated-date-from-commit object)
		  (get-author-from-commit object))
  )

(defun make-reflog-text (object)
  "
@brief	reflogモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param object	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A	~A ~A~%"
		  (commit-checked object)
		  (get-reflog-selector-from-commit object)
		  (make-formated-date-from-commit object)
		  (get-author-from-commit object)
		  (get-reflog-subject-from-commit object)
		  )
  )

(defun update-log-pane (commit-objects)
  "@brief	ログペイン更新。"
  (set-window-with-buffer-name "Git log")
  (setf buffer-read-only nil)
  (let ((cursor-position (current-line-number))
		(text-makers '(("log" . make-log-text)
					   ("reflog" . make-reflog-text)))
		text-maker
		)
	(setf text-maker (cdr (assoc *mode* text-makers :test 'string=)))
	(erase-buffer (selected-buffer))
	(dolist (object commit-objects)
	  (insert (funcall text-maker object))
	  )
	(delete-backward-char)
	(setf buffer-read-only t)
	(if (<= cursor-position (length *commits*))
		(goto-line cursor-position)
	  )
	)
  )

(defun get-cursor-commit ()
  "@brief	カーソルがある行に対応したコミットオブジェクトを取得。"
  (save-window-excursion
	(set-window-with-buffer-name "Git log")
	(nth (- (current-line-number) 1) *commits*)
	)
  )

(defun update-description ()
  "@brief	コミットログペイン更新。"
  (let ((object (get-cursor-commit)))
	(set-window-with-buffer-name "Git description")
	(setf buffer-read-only nil)
	(erase-buffer (selected-buffer))
	(insert (format
			 nil "SHA-1	~A~%Author	~A~%Mail	~A~%Date	~A~%~%~A~%"
			 (get-sha1-from-commit object)
			 (get-author-from-commit object)
			 (get-mail-from-commit object)
			 (make-formated-date-from-commit object)
			 (commit-description object)
			 )
			)
	)
  (setf buffer-read-only t)
  (set-window-with-buffer-name "Git log")
  )

(defun update-modfiles-pane ()
  "@brief	変更ファイルリストペイン更新"
  (set-window-with-buffer-name "Modified files")
  (setf buffer-read-only nil)
  (erase-buffer (selected-buffer))
  (let* ((object (get-cursor-commit))
		 (files-status (commit-files-status object))
		 )
	(dolist (file-status files-status)
	  (insert (format nil "~:[追加~;変更~]	~A~%"
					  (car file-status) (cadr file-status)))
	  )
	)
  (setf buffer-read-only t)
  (set-window-with-buffer-name "Git log")
  )

(defun forward-log (&optional (lines 1))
  "@brief	コミットログペインの更新を伴ったカーソルの移動。"
  (interactive)
  (or (forward-line lines) (return-from forward-log))
  (update-description)
  (update-modfiles-pane)
  )

(defun back-log ()
  "@brief	コミットログペインの更新を伴ったカーソルの移動。"
  (interactive)
  (forward-log -1)
  (update-modfiles-pane)
  )

(defun setup-mode-line ()
  "@brief	ウィンドウのステータス表示形式を変更。"
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (format nil "%b"))
  )

(defun checkout()
  (interactive)
  "
@brief	カーソルがある行でチェックアウトする。　
ファイル名を指定していなくても
（ gitlog-target-file-name が nil でも）使用できる。
その場合は管理対象全てがチェックアウト対象となる。"
  (let* ((object (get-cursor-commit))
		 (sha1 (get-sha1-from-commit object))
		 )
	(when (no-or-yes-p "チェックアウトします。よろしいですか？")
	  (call-process (format nil "~A checkout ~A ~A"
							*git-executable-path* sha1
							gitlog-target-file-name))
	  )
	)
  )

(defun reset ()
  "カーソルがある行が指すコミットでリセットする。"
  (interactive)
  (let* ((object (get-cursor-commit))
		 (reflog-pointer (get-reflog-selector-from-commit object))
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
		(return-from reset)
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
		(return-from reset)
		)
	   ((string= "?" answer)
		(msgbox "s - soft\nh - hard\nq - quit")
		)
	   )
	  )
	(call-process (format nil "~A reset --~A ~A"
						  *git-executable-path* mode reflog-pointer))
	)
  (message "reset has done.")
  )

(defun retroversion ()
  "@brief	特定のリビジョンのファイルを取得する。"
  (interactive)
  (when (= 0 (length gitlog-target-file-name))
	(msgbox "ファイル名が指定されていません。")
	(return-from retroversion)
	)
  (let* ((object (get-cursor-commit))
		 (sha1 (get-sha1-from-commit object))
		 (file-name gitlog-target-file-name)
		 answer
		 )
	(setf file-name (read-string "ファイル名を指定してください。>"
								 :default file-name))
	(loop
	  (setf answer (read-string (format nil "~A のリビジョンの ~A の内容を~
~A というファイルに出力します。よろしいですか？ [y, n]?"
										sha1 gitlog-target-file-name
										file-name)))
	  (cond
	   ((string= "y" answer)
		(return)
		)
	   ((string= "n" answer)
		(message "処理を中断しました。")
		(return-from retroversion)
		)
	   )
	  )
	(call-process (format nil "\"~A\" show ~A:~A"
						  *git-executable-path* sha1
						  gitlog-target-file-name) :output file-name)
	)
  )

(defun git-util-mode (&optional log-file-name)
  "@param log-file-name	ログファイル名。フルパスである必要がある。
"
  (interactive)
  (let ((logbuffer (selected-buffer))
		manual-window log-window description-window modfiles-window
		)

	; 開発用にログファイルを指定したい場合は以下をコメントアウトする。
;; 	(setf log-file-name "D:/myRoot/opt/xyzzy/site-lisp/siteinitlog.gitlog")

	(when (find-pseudo-frame "Git")
	  (select-pseudo-frame (find-pseudo-frame "Git"))
	  (delete-pseudo-frame)
	  )
	(new-pseudo-frame "Git")
	(setf manual-window (selected-window))
	(split-window)
	(split-window-vertically 80)
	(set-window (previous-window (selected-window)))
	(split-window-vertically 80)

	; レイアウトのため、ここで操作法ペイン作成。
	(set-window manual-window)
	(switch-to-buffer "Git operating")

	; ログペイン作成
	(switch-to-buffer-other-window "Git log")
	(setf log-window (selected-window))
	(setup-temp-buffer (selected-buffer))

	; デスクリプションペイン初期化
	(switch-to-buffer-other-window "Git description")
	(setf description-window (selected-window))
	(setup-temp-buffer (selected-buffer))
	(setup-mode-line)

	; 変更ファイルリストペイン
	(switch-to-buffer-other-window "Modified files")
	(setf modfiles-window (selected-window))
	(setup-temp-buffer (selected-buffer))
	(setup-mode-line)
	(setf buffer-read-only t)

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
	(setup-mode-line)
	(use-keymap (make-general-keymap))
	(make-local-variable 'gitlog-exec-directory)
	; diff やチェックアウトで使用する。
	(make-local-variable 'gitlog-target-file-name)

	; log-file-name が sjis じゃない場合に、sjis に変換する。
	(convert-char-code log-file-name *encoding-sjis*)

	(let (file)
	  (with-open-file (file log-file-name :encoding :text)
		(setf *mode* (read-line file nil nil))
		(setf gitlog-exec-directory (read-line file nil nil))
		(cond
		 ((string= "log" *mode*)
		  (setf gitlog-target-file-name (read-line file nil nil))
		  )
		 )
		(setup-manual-pane)
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
				  (define-log-mode-keymap))
				 ("reflog" .
				  (define-reflog-mode-keymap))
				 )
			   mode-stuffs keymap-definer)
			  )
		  (setf mode-stuffs
				(cdr (assoc *mode* mode-stuff-containers :test 'string=)))
		  (setf keymap-definer (nth 0 mode-stuffs))
		  (funcall keymap-definer (local-keymap (selected-buffer)))
		  (setf *commits* (parse-log file))
		  )
		)
	  )
	(update-log-pane *commits*)
	(update-description)
	(update-modfiles-pane)
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
			  (git-util-mode log-file-name)
			  )
			nil
			)
		  )
	(when error-message
	  (msgbox "Error!~%~A" error-message)
	  )
	)
  )
