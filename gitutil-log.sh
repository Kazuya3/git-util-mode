GITLOG_FILE=D:/myRoot/home/.git/log.gitlog

echo "$1"

# �����̃t�@�C�������݂��邩�`�F�b�N�B
if [ -e "$1" ]; then
	
	# ���[�h���ʎq�}���B
	echo "log" > ${GITLOG_FILE}

	# �J�����g�f�B���N�g���� GITLOG_FILE �ɒǋL�B
	pwd >> ${GITLOG_FILE}
	
	# �����i�Ώۃt�@�C���j�� GITLOG_FILE �ɒǋL�B
	echo "$1" >> ${GITLOG_FILE}
	
	# git log �̎��s���ʂ� GITLOG_FILE �ɒǋL�B
	git log "$1" >> ${GITLOG_FILE}
	
	# gitlog-mode ���ĂԁB
	D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/gitlog-mode.l ${GITLOG_FILE}
else
	echo -e "Error!\nSpecify file what you want to see log."
fi
