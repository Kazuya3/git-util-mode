#!/bin/sh

# git から情報をファイルに出力し、git-util-mode を起動するスクリプト。
# 引数で任意のファイルを指定することができる。
# 第一引数：ログを出力するディレクトリ
# 第二引数：モード
# 第三引数：ファイルの指定（ログモード時のみ利用）


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#	変数定義
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ログディレクトリ。
# 引数のままだと分かりづらいので名前のついた変数に格納する。
LOG_DIRECTORY=$1

# モード名。
# 引数のままだと分かりづらいので名前のついた変数に格納する。
MODE=$2

# 特定のファイル名。
# logモード時に、特定のファイルのログのみを閲覧したい場合に指定する。
TARGET_FILE=$3

# ログファイルの絶対パス
LOG_FILE=$LOG_DIRECTORY"/log.gitlog"


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#	関数定義
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
#	処理
#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
# ログ出力用ディレクトリが正しく指定されているか確認する。
if [ ! -d $LOG_DIRECTORY ]; then
	echo -e "エラー：ログ用に指定されたディレクトリは存在しません。"
	exit 1
fi

#------------------------------------------------------------------------------
# モードが正しく指定されているか確認する。
case $MODE in
	"log" ) ;;
	"reflog" ) ;;
	* ) echo -e "エラー：モードの指定が正しくありません。
与えられたモード ["$MODE"]"
		exit 1 ;;
esac

#------------------------------------------------------------------------------
# ファイルの指定があるなら、そのファイルが存在するかチェックする。
if [ $# -eq 3 ]; then
	if [ ! -e $3 ]; then
		echo -e "Error!\nReceived file does NOT exist."
		exit 1
	fi
fi

# モードをログに書き込む。
echo $MODE > $LOG_FILE

# リポジトリのルートの絶対パスをログに書き込む。
git rev-parse --show-toplevel >> $LOG_FILE

#------------------------------------------------------------------------------
# モードに応じた gitコマンドを出力。
case "$MODE" in
	"log" )
	echo $TARGET_FILE >> $LOG_FILE
	git log --name-status --pretty=format:"%at|%H|%aN|%aE|%n%B\`" \
		$TARGET_FILE >> $LOG_FILE
	;;
	
	"reflog" ) 
	git log --name-status -g --pretty=format:"%at|%H|%gN|%gE|%gD|%gs%n%B\`" \
		>> $LOG_FILE
	;;
esac

#------------------------------------------------------------------------------
# xyzzy と git-util-mode 起動
$XYZZYHOME"\xyzzy.exe" -l $XYZZYHOME"/site-lisp/git-util-mode.l" $LOG_FILE
