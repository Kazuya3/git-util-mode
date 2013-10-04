GITLOG_FILE=D:/myRoot/home/.git/log.gitlog

# git から情報をファイルに出力し、gitlog-mode を起動するスクリプト。
# 引数で任意のファイルを指定することができる。

path=""

# ファイルの指定がある（引数がある）なら、
# そのファイルが存在するかチェックする。
if [ $# -eq 1 ]; then
	if [ -e "$1" ]; then
		path="$1"
	else
		echo -e "Error!\nReceived file does NOT exist."
		exit 1
	fi
fi
	
# モード識別子挿入。
echo "log" > ${GITLOG_FILE}

# カレントディレクトリを GITLOG_FILE に追記。
pwd >> ${GITLOG_FILE}

# 引数（対象ファイル）を GITLOG_FILE に追記。
echo "$1" >> ${GITLOG_FILE}

# git log の実行結果を GITLOG_FILE に追記。
git log --name-status --pretty=format:"%at|%H|%aN|%aE|%n%B\`" $path >> ${GITLOG_FILE}

# gitlog-mode を呼ぶ。
D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/git-util-mode.l ${GITLOG_FILE}
