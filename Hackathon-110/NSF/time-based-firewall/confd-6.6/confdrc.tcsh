# tcsh/csh environment setup for ConfD confd-basic-6.6
setenv CONFD_DIR /home/ubuntu/confd-6.6
setenv PATH $CONFD_DIR/bin:${PATH}
if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH $CONFD_DIR/lib:${LD_LIBRARY_PATH}
else
    setenv LD_LIBRARY_PATH $CONFD_DIR/lib
endif
if ($?PYTHONPATH) then
    setenv PYTHONPATH $CONFD_DIR/src/confd/pyapi:${PYTHONPATH}
else
    setenv PYTHONPATH $CONFD_DIR/src/confd/pyapi
endif
if ($?MANPATH) then
    setenv MANPATH $CONFD_DIR/man:${MANPATH}
else
    setenv MANPATH $CONFD_DIR/man:/usr/share/man
endif
