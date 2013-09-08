GITLOG_FILE=D:/myRoot/home/.git/reflog.gitlog

echo "$1"

# モード識別子挿入。
echo "reflog" > ${GITLOG_FILE}

# カレントディレクトリを GITLOG_FILE に追記。
pwd >> ${GITLOG_FILE}

# git log の実行結果を GITLOG_FILE に追記。
git log -g >> ${GITLOG_FILE}

# gitlog-mode を呼ぶ。
D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/gitlog-mode.l ${GITLOG_FILE}
