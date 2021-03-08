#!/bin/sh

set -e

if [ $# -gt 0 ]; then
    case "$1" in
        --post-commit)
            cat << EOF
begin post-commit
end
EOF
            exit 0
            ;;
        *)
            echo
            echo "Usage: $0 [--post-commit]"
            echo
            echo "  --post-commit Mandatory for post-commit scripts"
            exit 1
            ;;
    esac
else
    # echo "Redirect stdout to logfile"
    exec > /tmp/post_commit_show_diff.log 2>&1

    echo
    date
    echo $0
    echo "CONFD_MAAPI_USID=$CONFD_MAAPI_USID"
    echo "CONFD_MAAPI_THANDLE=$CONFD_MAAPI_THANDLE"
    echo
    echo "--- transaction diff ---"
    maapi --keypath-diff /
fi
