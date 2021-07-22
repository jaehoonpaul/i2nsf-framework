#! /bin/sh
#
# A sample application startup script for ConfD
#
# Copyright 2009 Tail-f Systems
#
# Permission to use this code as a starting point hereby granted
# 
if [ -z "${CONFD_DIR}" ]; then
    CONFD_DIR=/usr/local/confd
fi

CONFD=${CONFD_DIR}/bin/confd
CONFD_CMD=${CONFD_DIR}/bin/confd_cmd
CONFD_CONF=./confd.conf
mode=start

critical_err() { # <msg>
    echo "CRITICAL ERROR: $*"
    # Stop ConfD in case we are in a half-baked starting state
    ${CONFD} --stop > /dev/null 2>&1

    # Critical error = start from factory install
    # A good thing to do here would also be to save logs, db, etc to e.g.
    # a tar file which could be used for post-mortem debugging.
    rm -f cdb/*.cdb cdb/candidate.db

    # Raise an alarm, send sms, call someone etc etc......
    exit 1
}

run_upgrade_scripts() { # phase0 | phase1
    # Now is the time for all good upgrade scripts to come to the aid of...
    echo "Running upgrade scripts in $1"
    case "$1" in
	phase0)
	    # Do upgrade stuff for phase0, e.g. editing upgrade transaction
	    ;;
	phase1)
	    # Do upgrade stuff for phase1, e.g. restoring operational data
	    
	    # EXAMPLE: set /test/bar to 20 if we upgrade (okay if we fail,
	    # means we don't have /test/bar)
	    ${CONFD_CMD} -o -c 'set /test:test/bar 20' 2>/dev/null
	    ;;
    esac
}

run_init_scripts() {
    echo "Running init scripts in $1"
    case "$1" in
	phase0)
	    # Do init stuff for phase0, e.g. editing system tables
	    ;;
	phase1)
	    # Do init stuff for phase1
	    ;;
    esac
}

# Make sure critical files exist
if [ ! -e example.fxs ]; then
    critical_err "system isn't built"
fi
if [ ! -e ${CONFD_CONF} ]; then
    critical_err "something wrong with installation, can't find confd.conf"
fi

# Now bring ConfD up to phase0
${CONFD} -c ${CONFD_CONF} --addloadpath ${CONFD_DIR}/etc/confd --start-phase0
ecode=$?
if [ $ecode -ne 0 ]; then
    critical_err "failed to start ConfD in phase 0, exit code $ecode"
fi
echo "ConfD started in phase0"

phase=`${CONFD_CMD} -c get_phase`
case "$phase" in
    *INIT)
	mode=init		# we are starting without a database
	run_init_scripts phase0
	;;
    *UPGRADE)
	echo "Upgrade in progress..."
	mode=upgrade		# remember that we are upgrading
	run_upgrade_scripts phase0
	;;
    *)
	;;
esac

# Now start validation code and setup notification sockets
# ...


# That's it for phase 0, take us to phase 1
${CONFD} --start-phase1
ecode=$?
if [ $ecode -ne 0 ]; then
    critical_err "failed to start ConfD in phase 1, exit code $ecode"
fi
echo "ConfD started in phase1"

case $mode in
    init)
	run_init_scripts phase1
	;;
    upgrade)
	run_upgrade_scripts phase1
	;;
esac

# Register data providers, transformation callbacks, hooks etc
# Register CDB subscribers
# ...

# Start in foreground, client forks when ready to proceed
./cdb_client_A
./cdb_client_B

# All CDB clients started, trigger a synthetic subscription
# notification to get them started
./trigger_subscribers


# We are now ready to bring up north-bound interfaces by going to phase2
${CONFD} --start-phase2
ecode=$?
if [ $ecode -ne 0 ]; then
    critical_err "failed to start ConfD in phase 2, exit code $ecode"
fi
echo "ConfD started in phase2"

exit 0
