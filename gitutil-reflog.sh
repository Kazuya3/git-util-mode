GITLOG_FILE=D:/myRoot/home/.git/reflog.gitlog

echo "$1"

# ���[�h���ʎq�}���B
echo "reflog" > ${GITLOG_FILE}

# �J�����g�f�B���N�g���� GITLOG_FILE �ɒǋL�B
pwd >> ${GITLOG_FILE}

# git log �̎��s���ʂ� GITLOG_FILE �ɒǋL�B
# git log -g >> ${GITLOG_FILE}
git log -20 -g --pretty=format:"%at %H %gN %gE %gD %gs%n%B" >> ${GITLOG_FILE}

# gitlog-mode ���ĂԁB
D:/myRoot/opt/xyzzy/xyzzy.exe -l D:/myRoot/opt/xyzzy/site-lisp/gitlog-mode.l ${GITLOG_FILE}
