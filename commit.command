#!/bin/bash

Dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $Dir

echo "Enter commit message"
read Message
git add -A && git commit -m "$Message"

echo "Push? (y/n)"
read Push
case "$Push" in ([yY]) git push ;; esac

$SHELL