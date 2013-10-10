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
(defvar *exec-directory* nil)
(defvar *target-file-name* nil)	; diff やチェックアウトで使用する。
(defvar *manual-window* nil)
(defvar *log-window* nil)
(defvar *description-window* nil)
(defvar *modfile-window* nil)


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
(defun make-formated-date-from-commit (commit)
  "
@brief	コミットオブジェクトからフォーマットに従った日時の文字列を取得する。
@param commit	コミットオブジェクト
@return	フォーマットに従った日時の文字列
"
  (make-date-string-from-unix-time
   (parse-integer (nth 0 (commit-splited-info commit))))
  )

(defun get-sha1-from-commit (commit)
  (nth 1 (commit-splited-info commit))
  )

(defun get-author-from-commit (commit)
  (nth 2 (commit-splited-info commit))
  )

(defun get-mail-from-commit (commit)
  (nth 3 (commit-splited-info commit))
  )

(defun get-reflog-selector-from-commit (commit)
  (nth 4 (commit-splited-info commit))
  )

(defun get-reflog-subject-from-commit (commit)
  (nth 5 (commit-splited-info commit))
  )

(defun init-window (window)
  "@brief	ウィンドウの表示設定を行う。"
  (save-window-excursion
	(set-window window)
	(setup-temp-buffer (selected-buffer))
	(make-local-variable 'mode-line-format)
	(setq mode-line-format (format nil "%b"))
	(set-local-window-flags (selected-buffer) *window-flag-line-number* nil)
	(set-local-window-flags (selected-buffer) *window-flag-newline* nil)
	(set-local-window-flags (selected-buffer) *window-flag-eof* nil)
	(set-local-window-flags (selected-buffer) *window-flag-cursor-line* t)
	(set-local-window-flags (selected-buffer) *window-flag-ruler* nil)
	(set-local-window-flags (selected-buffer) *window-flag-just-inverse* t)
	(set-buffer-fold-width nil)
	(define-general-keymap (local-keymap))
	)
  )

(defun make-window-layout ()
  "画面レイアウトを作成・初期化する"
  (when (find-pseudo-frame "Git")
	(select-pseudo-frame (find-pseudo-frame "Git"))
	(delete-pseudo-frame)
	)
  (new-pseudo-frame "Git")
  (setf *manual-window* (selected-window))
  (split-window)
  (split-window-vertically 80)
  (set-window (previous-window (selected-window)))
  (split-window-vertically 80)

  ; マニュアルバッファ作成
  (set-window *manual-window*)
  (switch-to-buffer "Git operation")
  (init-window *manual-window*)

  ; ログウィンドウ・バッファ作成・初期化
  (switch-to-buffer-other-window "Git log")
  (setf *log-window* (selected-window))
  (use-keymap (make-log-buffer-keymap))
  (init-window *log-window*)
  (when (string= "reflog" *mode*)
	(make-local-variable 'regexp-keyword-list)
	(setf regexp-keyword-list
		  (compile-regexp-keyword-list
		   '(
			 ("^.* commit: .*" nil (:color 0))
			 ("^.* checkout: .*" nil (:color 2))
			 ("^.* merge [^:]+: .*" nil (:color 3))
			 ("^.* reset: .*" nil (:color 1))
			 )
		   )
		  )
	)

  ; デスクリプションウィンドウ・バッファ作成・初期化
  (switch-to-buffer-other-window "Git description")
  (setf *description-window* (selected-window))
  (setup-temp-buffer (selected-buffer))
  (init-window *description-window*)

  ; 変更ファイルリストバッファ・ウィンドウ作成
  (switch-to-buffer-other-window "Modified files")
  (setf *modfile-window* (selected-window))
  (setup-temp-buffer (selected-buffer))
  (init-window *modfile-window*)
  (define-modfile-buffer-key (local-keymap))
  (make-local-variable 'regexp-keyword-list)
  (setf regexp-keyword-list
		(compile-regexp-keyword-list
		 '(
		   ("^追加	.*" nil (:color 2))
		   ("^変更	.*" nil (:color 0))
		   ("^削除	.*" nil (:color 1))
		   )
		 )
		)
  )

(defun parse-log (log-file-path)
  "
@brief	log の結果をパースする。
@param log-file	ログファイルのストリーム
@return	log-file をパースして得たコミットオブジェクトのリスト
"
  (with-open-file (file log-file-path :encoding :text)
	(setf *mode* (read-line file nil nil))
	(setf *exec-directory* (read-line file nil nil))
	(cond
	 ((string= "log" *mode*)
	  (setf *target-file-name* (read-line file nil nil))
	  )
	 )
	; シェルスクリプトによるパスの出力で、Linux 形式ではなく Windows 形式で
	; 吐き出される現象が確認されたので、Linux 形式の時のみ Windows 形式に
	; 変換するようにした。
	(when (char= #\/ (char *exec-directory* 0))
	  (setf *exec-directory* (concat (substring *exec-directory* 1 2)
									 ":"
									 (substring *exec-directory* 2)))
	  )

	(let (line skip-read
			   (commits (list))
			   )
	  (loop
		(let (splited description file-status
					  (files-status (list))
					  )
		  (if skip-read
			  (setf skip-read nil)
			; else
			(setf line (read-line file nil nil))
			)
		  (when (not line)
			(return)
			)
		  (setf splited (split-string line "|"))
		  ; body 解析
		  (loop
			(setf line (read-line file nil nil))
			(if (string-match "^`$" line)
				(return)
			  )
			(setf description (concat description line "\n"))
			)
		  ; 変更ファイル情報解析
		  (loop
			(setf line (read-line file nil nil))
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
			(push `(,(car file-status) ,(cadr file-status)) files-status)
			)
		  (push (make-commit
				 :splited-info splited
				 :description description
				 :files-status files-status
				 )
				commits)
		  )
		)
	  (setf *commits* (reverse commits))
	  )
	)
  )

(defun make-log-buffer-keymap ()
  "@brief	ログバッファのキーマップ作成"
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap #\SPC 'toggle-mark)
	(define-key keymap #\n 'forward-log)
	(define-key keymap #\C-n 'forward-log)
	(define-key keymap #\p 'back-log)
	(define-key keymap #\C-p 'back-log)
	(define-key keymap '(#\C-x #\r) 'rebase-interactively)
	(cond
	 ((string= "log" *mode*)
	  (define-key keymap #\RET 'compare-commits)
	  (define-key keymap #\c 'checkout)
	  (define-key keymap #\s 'retroversion-at-log-buffer)
	  )
	 ((string= "reflog" *mode*)
	  (define-key keymap #\r 'reset)
	  )
	 )
	keymap
	)
  )

(defun define-general-keymap (keymap)
  "各バッファ共通のキーバインド定義"
  (define-key keymap '#\q 'user::query-kill-xyzzy)
  (define-key keymap '(#\C-x #\o) 'forward-window)
  (define-key keymap '(#\C-x #\p) 'back-window)
  )

(defun define-modfile-buffer-key (keymap)
  "@brief	変更ファイルバッファのキーバインド定義"
  (define-key keymap #\s 'retroversion-at-modfile-buffer)
  )

(defun update-manual-pane ()
  "マニュアルペイン設定"
  (let ((active-window (selected-window))
		)
	(save-window-excursion
	  (set-window *manual-window*)
	  (setf buffer-read-only nil)
	  (erase-buffer (selected-buffer))
	  (cond
	   ((eq *log-window* active-window)
		(insert-file (merge-pathnames "git-util/general-operation.txt" *etc-path*))
		(goto-max)
		(cond
		 ((string= "log" *mode*)
		  (insert-file (merge-pathnames "git-util/log-mode-operation.txt" *etc-path*))
		  )
		 ((string= "reflog" *mode*)
		  (insert-file (merge-pathnames "git-util/reflog-mode-operation.txt" *etc-path*))
		  )
		 )
		)
	   ((eq *modfile-window* active-window)
		(insert-file (merge-pathnames "git-util/modfile-operation.txt" *etc-path*))
		)
	   )
	  (setf buffer-read-only t)
	  )
	)
  )

(defun toggle-mark ()
  "@brief	ログペインのコミットのマークをトグルする。"
  (interactive)
  (let ((commit (get-cursor-commit)))
	(setf (commit-checked commit) (if (commit-checked commit) nil t))
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
			  (dolist (commit *commits*)
				(when (commit-checked commit)
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
							*git-executable-path*
							(get-sha1-from-commit (nth 0 checkeds))
							(get-sha1-from-commit (nth 1 checkeds))
							*target-file-name*)
					)
			  (call-process command
							:exec-directory *exec-directory*
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

(defun make-log-text (commit)
  "
@brief	logモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param commit	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A~%"
		  (commit-checked commit)
		  (make-formated-date-from-commit commit)
		  (get-author-from-commit commit))
  )

(defun make-reflog-text (commit)
  "
@brief	reflogモードにおいて、
コミットオブジェクトからログペインに表示するテキストを作成する。
@param commit	コミットオブジェクト"
  (format nil "[~:[ ~;*~]] ~A	~A	~A ~A~%"
		  (commit-checked commit)
		  (get-reflog-selector-from-commit commit)
		  (make-formated-date-from-commit commit)
		  (get-author-from-commit commit)
		  (get-reflog-subject-from-commit commit)
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
	(dolist (commit commit-objects)
	  (insert (funcall text-maker commit))
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
  (let ((commit (get-cursor-commit)))
	(set-window-with-buffer-name "Git description")
	(setf buffer-read-only nil)
	(erase-buffer (selected-buffer))
	(insert (format
			 nil "SHA-1	~A~%Author	~A~%Mail	~A~%Date	~A~%~%~A~%"
			 (get-sha1-from-commit commit)
			 (get-author-from-commit commit)
			 (get-mail-from-commit commit)
			 (make-formated-date-from-commit commit)
			 (commit-description commit)
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
  (let* ((commit (get-cursor-commit))
		 (files-status (commit-files-status commit))
		 state state-text
		 )
	(dolist (file-status files-status)
	  (setf state (car file-status))
	  (cond
	   ((string= "A" state)
		(setf state-text "追加")
		)
	   ((string= "M" state)
		(setf state-text "変更")
		)
	   ((string= "D" state)
		(setf state-text "削除")
		)
	   (t
		(msgbox "予期せぬ状態が検出されました。[~A]" state)
		(setf state-text "Unknown")
		)
	   )
	  (insert (format nil "~A	~A~%"
					  state-text (cadr file-status)))
	  )
	(delete-backward-char)
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

(defun checkout()
  (interactive)
  "
@brief	カーソルがある行でチェックアウトする。　
ファイル名を指定していなくても
（ *target-file-name* が nil でも）使用できる。
その場合は管理対象全てがチェックアウト対象となる。"
  (let* ((commit (get-cursor-commit))
		 (sha1 (get-sha1-from-commit commit))
		 command
		 )
	(when (no-or-yes-p "チェックアウトします。よろしいですか？")
	  (setf command (format nil "~A checkout ~A ~A"
							*git-executable-path* sha1 *target-file-name*))
	  (call-process command)
	  (msgbox "リセットにより、現在表示しているログが古くなりました。~
\ngit-util-mode を終了します。")
	  (kill-xyzzy)
	  )
	)
  )

(defun reset ()
  "カーソルがある行が指すコミットでリセットする。"
  (interactive)
  (let* ((commit (get-cursor-commit))
		 (reflog-pointer (get-reflog-selector-from-commit commit))
		 answer mode command
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
	(setf command (format nil "~A reset --~A ~A"
						  *git-executable-path* mode reflog-pointer))
	(call-process command)
	)
  (msgbox "リセットにより、現在表示しているログが古くなりました。~
\ngit-util-mode を終了します。")
  (kill-xyzzy)
)

(defun retroversion (file-path)
  "
@brief	引数で指定したファイルの、特定のリビジョンの状態を取得する。
checkout で過去の状態に戻すと reflog に記録される。
ファイル名を改名しなければ reflog を汚さずに過去の状態が取得できる。
ただし同じ名前のファイルの内容が上書きされるので注意。
"
  (when (= 0 (length file-path))
	(msgbox "ファイル名が指定されていません。")
	(return-from retroversion)
	)
  (let* ((commit (get-cursor-commit))
		 (sha1 (get-sha1-from-commit commit))
		 (output-file-name file-path)
		 answer command
		 )
	(setf output-file-name (read-string "ファイル名を指定してください。>"
										:default output-file-name))
	(loop
	  (setf answer (read-string (format nil "~A のリビジョンの ~A の内容を~
~A というファイルに出力します。よろしいですか？ [y, n]?"
										sha1 file-path
										output-file-name)))
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
	(setf command (format nil "\"~A\" show ~A:~A"
						  *git-executable-path* sha1 file-path))
	(call-process command :output output-file-name)
	)
  )

(defun retroversion-at-log-buffer ()
  "@brief	ログバッファで retroversion を使用する。"
  (interactive)
  (retroversion *target-file-name*)
  )

(defun retroversion-at-modfile-buffer ()
  "@brief	変更ファイルバッファで retroversion を使用する。"
  (interactive)
  (let ((commit (get-cursor-commit))
		(cursor-line (- (current-line-number) 1))
		file-name
		)
	(when (=  0 (length (commit-files-status commit)))
	  (msgbox "このリビジョンには変更されたファイルがありません。")
	  (return-from retroversion-at-modfile-buffer)
	  )
	(setf file-name (cadr (nth cursor-line (commit-files-status commit))))
	(retroversion file-name)
	)
  )

(defun rebase-interactively ()
  "@brief	対話的リベースを行う。"
  (interactive)
  (let ((commit (get-cursor-commit))
		command
		)
	(if (no-or-yes-p "対話的リベースを行います。よろしいですか？")
		(progn
		  (msgbox "リベースが実行された場合ログが変わるため、~
git-util-mode を終了します。")
		  (setf command
				(format nil "\"~A\" rebase -i ~A^" *git-executable-path*
						(get-sha1-from-commit commit)))

		  (call-process command)
		  (kill-xyzzy)
		  )
	  (message "リベースを中止しました。")
	  )
	)
  )

(defun forward-window ()
  "
@brief	操作法ペインを適切に切り替えるための other-window のラッパ。"
  (interactive)
  (other-window)
  (update-manual-pane)
  )

(defun back-window ()
  "
@brief	操作法ペインを適切に切り替えるための move-previous-window のラッパ。"
  (interactive)
  (move-previous-window)
  (update-manual-pane)
  )

(defun git-util-mode (&optional log-file-name)
  "@param log-file-name	ログファイル名。フルパスである必要がある。
"
  (interactive)

  ; 開発用にログファイルを指定したい場合は以下をコメントアウトする。
;; 	(setf log-file-name "D:/myRoot/opt/xyzzy/site-lisp/siteinitlog.gitlog")

  ; log-file-name が sjis じゃない場合に、sjis に変換する。
  (convert-char-code log-file-name *encoding-sjis*)
  (parse-log log-file-name)

  (make-window-layout)
  (set-window *log-window*)
  (update-manual-pane)
  (update-log-pane *commits*)
  (update-description)
  (update-modfiles-pane)
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
