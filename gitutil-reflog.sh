GITLOG_FILE=D:/myRoot/home/.git/reflog.gitlog

# モード識別子挿入。
echo "reflog" > ${GITLOG_FILE}

# カレントディレクトリを GITLOG_FILE に追記。
pwd >> ${GITLOG_FILE}

# git log の実行結果を GITLOG_FILE に追記。
# git log -g >> ${GITLOG_FILE}
git log --name-status -g --pretty=format:"%at|%H|%gN|%gE|%gD|%gs%n%B\`" >> ${GITLOG_FILE}

# gitlog-mode を呼ぶ。
D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/git-util-mode.l ${GITLOG_FILE}
