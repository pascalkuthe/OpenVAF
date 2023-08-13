#! /bin/bash
echo "foo"
mime=$(file -b --mime-type -L ".git/COMMIT_EDITMSG")
mime=${mime%;*}
echo "${mime}" 
case "${mime}" in
    application/*+xml) filetype="xml" ;;
    image/*+xml) filetype="xml" ;; #SVG
    message/rfc822) filetype="mail" ;;
    text/x-shellscript) filetype="sh" ;;
    text/x-script.*) filetype="${mime#text/x-script.}" ;;
    text/x-*) filetype="${mime#text/x-}" ;;
    text/plain) exit ;;
    text/*)   filetype="${mime#text/}" ;;
    application/x-shellscript) filetype="sh" ;;
    application/x-*) filetype="${mime#application/x-}" ;;
    application/*) filetype="${mime#application/}" ;;
    *) echo "hek" 
  exit ;;
esac
echo "xx ${filetype}"
