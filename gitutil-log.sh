GITLOG_FILE=D:/myRoot/home/.git/log.gitlog

echo "$1"

# 引数のファイルが存在するかチェック。
if [ -e "$1" ]; then
	
	# モード識別子挿入。
	echo "log" > ${GITLOG_FILE}

	# カレントディレクトリを GITLOG_FILE に追記。
	pwd >> ${GITLOG_FILE}
	
	# 引数（対象ファイル）を GITLOG_FILE に追記。
	echo "$1" >> ${GITLOG_FILE}
	
	# git log の実行結果を GITLOG_FILE に追記。
	git log "$1" >> ${GITLOG_FILE}
	
	# gitlog-mode を呼ぶ。
	D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/gitlog-mode.l ${GITLOG_FILE}
else
	echo -e "Error!\nSpecify file what you want to see log."
fi
