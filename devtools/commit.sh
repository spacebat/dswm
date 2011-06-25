#!/bin/sh
cd $(dirname $0)/../

COMMENT="$@"
branch=trunk
remotes="ws gh sf"
[ -z "$COMMENT" ] && COMMENT=$(date)

rm -f *\#*
autoconf
./configure && make clean
rm -rf ./configure

git add .
git commit -a -m "$COMMENT"
for i in $remotes; do
    git push --force $i $branch
done
