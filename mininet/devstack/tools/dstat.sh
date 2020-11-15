#!/bin/bash

# **tools/dstat.sh** - Execute instances of DStat to log system load info
#
# Multiple instances of DStat are executed in order to take advantage of
# incompatible features, particularly CSV output and the "top-cpu-adv" and
# "top-io-adv" flags.
#
# Assumes:
#  - dstat command is installed

# Retrieve log directory as argument from calling script.
LOGDIR=$1

DSTAT_TOP_OPTS="--top-cpu-adv --top-io-adv --top-mem"
if dstat --version | grep -q 'pcp-dstat' ; then
    # dstat is unmaintained, and moving to a plugin of performance
    # co-pilot.  Fedora 29 for example has rolled this out.  It's
    # mostly compatible, except for a few options which are not
    # implemented (yet?)
    DSTAT_TOP_OPTS=""
fi

# Command line arguments for primary DStat process.
DSTAT_OPTS="-tcmndrylpg ${DSTAT_TOP_OPTS} --swap --tcp"

# Command-line arguments for secondary background DStat process.
DSTAT_CSV_OPTS="-tcmndrylpg --tcp --output $LOGDIR/dstat-csv.log"

# Execute and background the secondary dstat process and discard its output.
dstat $DSTAT_CSV_OPTS >& /dev/null &

# Execute and background the primary dstat process, but keep its output in this
# TTY.
dstat $DSTAT_OPTS &

# Catch any exit signals, making sure to also terminate any child processes.
trap "kill -- -$$" EXIT

# Keep this script running as long as child dstat processes are alive.
wait
