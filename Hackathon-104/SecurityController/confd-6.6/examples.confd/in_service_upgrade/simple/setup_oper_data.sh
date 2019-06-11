#! /bin/sh

PATH=$CONFD_DIR/bin:$PATH
export PATH

version=`ls -l pkg/current | awk '{print $NF}'`
hardware=`uname -m`
revision="0"

confd_cmd -o >/dev/null 2>&1 <<EOF
set /simple:stats/version $version
set /simple:stats/hardware $hardware
set /simple:stats/revision $revision
EOF

exit 0
