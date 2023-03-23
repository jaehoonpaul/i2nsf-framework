"""
*********************************************************************
* ConfD Actions intro example                                       *
* Implements a couple of actions                                    *
*                                                                   *
* (C) 2015 Tail-f Systems                                           *
* Permission to use this code as a starting point hereby granted    *
*                                                                   *
* See the README file for more information                          *
*********************************************************************
"""
from __future__ import print_function
import socket
import select

from ietf_i2nsf_registration_interface_ns import ns
from ietf_i2nsf_capability_ns import ns as cap_ns
import database
#from config_ns import ns

import confd
import _confd
import _confd.dp as dp
import _confd.error as confd_errors
import re


def make_tag_value(ns_hash, tag, value):
    """
    Wrapper to create a _confd.TagValue
    """
    return _confd.TagValue(_confd.XmlTag(ns_hash, tag),
                           _confd.Value(value))

def value_as_string(value):
    if type(value) != int:
        valInt = re.search("[0-9]+",value.as_pyval()).group()
    else:
        valInt = value
    for k,v in cap_ns.__dict__.items():
        if v == int(valInt):
            result = k+"_"
            break
    return(getattr(cap_ns,result))

def xmlGen(nsfs):
    result = []
    xmltag = _confd.XmlTag
    value = _confd.Value
    tagvalue = _confd.TagValue
    for nsf in nsfs:
        result.append(tagvalue(xmltag(ns.hash,ns.i2nsfri_nsf),value((ns.i2nsfri_nsf, ns.hash),_confd.C_XMLBEGIN))) # <nsf>
        result.append(tagvalue(xmltag(ns.hash, ns.i2nsfri_nsf_name),value(nsf.nsf_name)))  #<name>
        result.append(tagvalue(xmltag(ns.hash, ns.i2nsfri_version),value(nsf.version))) #<version>
        
        if hasattr(nsf,"directional_capabilities"):
            for capVal in iter(nsf.directional_capabilities):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_directional_capabilities),
                        value(capVal)
                    ))

        if nsf.condition_capabilities != 0:
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_condition_capabilities),
                    value((ns.i2nsfri_condition_capabilities, ns.hash),_confd.C_XMLBEGIN)
                ))
            if nsf.generic_nsf_capabilities != 0:
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_generic_nsf_capabilities),
                        value((ns.i2nsfri_generic_nsf_capabilities, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"ipv4_capability"):
                    for capVal in iter(nsf.ipv4_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_ipv4_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"ipv6_capability"):
                    for capVal in iter(nsf.ipv6_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_ipv6_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"tcp_capability"):
                    for capVal in iter(nsf.tcp_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_tcp_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"udp_capability"):
                    for capVal in iter(nsf.udp_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_udp_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"sctp_capability"):
                    for capVal in iter(nsf.sctp_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_sctp_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"dccp_capability"):
                    for capVal in iter(nsf.dccp_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_dccp_capability),
                                value(capVal)
                            ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_generic_nsf_capabilities),
                        value((ns.i2nsfri_generic_nsf_capabilities, ns.hash),_confd.C_XMLEND)
                    ))
            if nsf.advanced_nsf_capabilities != 0:
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_advanced_nsf_capabilities),
                        value((ns.i2nsfri_advanced_nsf_capabilities, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"anti_ddos_capability"):
                    for capVal in iter(nsf.anti_ddos_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_anti_ddos_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"ips_capability"):
                    for capVal in iter(nsf.ips_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_ips_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"anti_virus_capability"):
                    for capVal in iter(nsf.anti_virus_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_anti_virus_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"url_filtering_capability"):
                    for capVal in iter(nsf.url_filtering_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_url_filtering_capability),
                                value(capVal)
                            ))
                if hasattr(nsf,"voip_vocn_filtering_capability"):
                    for capVal in iter(nsf.voip_vocn_filtering_capability):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_voip_vocn_filtering_capability),
                                value(capVal)
                            ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_advanced_nsf_capabilities),
                        value((ns.i2nsfri_advanced_nsf_capabilities, ns.hash),_confd.C_XMLEND)
                    ))
            if nsf.context_capabilities != 0:
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_context_capabilities),
                        value((ns.i2nsfri_context_capabilities, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"time_capabilities"):
                    for capVal in iter(nsf.time_capabilities):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_time_capabilities),
                                value(capVal)
                            ))
                if hasattr(nsf,"application_filter_capabilities"):
                    for capVal in iter(nsf.application_filter_capabilities):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_application_filter_capabilities),
                                value(capVal)
                            ))
                if hasattr(nsf,"device_type_capabilities"):
                    for capVal in iter(nsf.device_type_capabilities):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_device_type_capabilities),
                                value(capVal)
                            ))
                if hasattr(nsf,"user_condition_capabilities"):
                    for capVal in iter(nsf.user_condition_capabilities):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_user_condition_capabilities),
                                value(capVal)
                            ))
                if hasattr(nsf,"geographic_capabilities"):
                    for capVal in iter(nsf.geographic_capabilities):
                        result.append(
                            tagvalue(
                                xmltag(ns.hash, ns.i2nsfri_geographic_capabilities),
                                value(capVal)
                            ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_context_capabilities),
                        value((ns.i2nsfri_context_capabilities, ns.hash),_confd.C_XMLEND)
                    ))
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_condition_capabilities),
                    value((ns.i2nsfri_condition_capabilities, ns.hash),_confd.C_XMLEND)
                ))
        if nsf.action_capabilities != 0:
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_action_capabilities),
                    value((ns.i2nsfri_action_capabilities, ns.hash),_confd.C_XMLBEGIN)
                ))
            if hasattr(nsf,"ingress_action_capability"):
                for capVal in iter(nsf.ingress_action_capability):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_ingress_action_capability),
                            value(capVal)
                        ))
            if hasattr(nsf,"egress_action_capability"):
                for capVal in iter(nsf.egress_action_capability):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_egress_action_capability),
                            value(capVal)
                        ))
            if hasattr(nsf,"log_action_capability"):
                for capVal in iter(nsf.log_action_capability):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_log_action_capability),
                            value(capVal)
                        ))
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_action_capabilities),
                    value((ns.i2nsfri_action_capabilities, ns.hash),_confd.C_XMLEND)
                ))
        if hasattr(nsf,"resolution_strategy_capabilities"):
            for capVal in iter(nsf.resolution_strategy_capabilities):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_resolution_strategy_capabilities),
                        value(capVal)
                    ))
        if hasattr(nsf,"default_action_capabilities"):
            for capVal in iter(nsf.default_action_capabilities):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_default_action_capabilities),
                        value(capVal)
                    ))
        
        if nsf.nsf_specification != 0:
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf_specification),
                    value((ns.i2nsfri_nsf_specification, ns.hash),_confd.C_XMLBEGIN)
                ))

            if nsf.cpu !=0:
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_cpu),
                        value((ns.i2nsfri_cpu, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"model"):

                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_model),
                            value(nsf.model)
                        ))
                if hasattr(nsf,"clock_speed"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_clock_speed),
                            value(nsf.clock_speed,_confd.C_UINT16)
                        ))
                if hasattr(nsf,"cores"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_cores),
                            value(nsf.cores,_confd.C_UINT8)
                        ))
                if hasattr(nsf,"threads"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_threads),
                            value(nsf.threads,_confd.C_UINT16)
                        ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_cpu),
                        value((ns.i2nsfri_cpu, ns.hash),_confd.C_XMLEND)
                    ))
            if nsf.memory !=0:
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_memory),
                        value((ns.i2nsfri_memory, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"capacity"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_capacity),
                            value(nsf.capacity,_confd.C_UINT32)
                        ))
                if hasattr(nsf,"speed"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_speed),
                            value(nsf.speed,_confd.C_UINT32)
                        ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_memory),
                        value((ns.i2nsfri_memory, ns.hash),_confd.C_XMLEND)
                    ))
            if nsf.disk !=0:           
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_disk),
                        value((ns.i2nsfri_disk, ns.hash),_confd.C_XMLBEGIN)
                    ))
                if hasattr(nsf,"capacity"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_capacity),
                            value(nsf.capacity,_confd.C_UINT32)
                        ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_disk),
                        value((ns.i2nsfri_disk, ns.hash),_confd.C_XMLEND)
                    ))
            if nsf.bandwidth !=0: 
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_bandwidth),
                        value((ns.i2nsfri_bandwidth, ns.hash),_confd.C_XMLBEGIN)
                    )) 
                if hasattr(nsf,"outbound"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_outbound),
                            value(nsf.outbound,_confd.C_UINT64)
                        ))
                if hasattr(nsf,"inbound"):
                    result.append(
                        tagvalue(
                            xmltag(ns.hash, ns.i2nsfri_inbound),
                            value(nsf.inbound,_confd.C_UINT64)
                        ))
                result.append(
                    tagvalue(
                        xmltag(ns.hash,ns.i2nsfri_bandwidth),
                        value((ns.i2nsfri_bandwidth, ns.hash),_confd.C_XMLEND)
                    )) 
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf_specification),
                    value((ns.i2nsfri_nsf_specification, ns.hash),_confd.C_XMLEND)
                ))
        if nsf.nsf_access_info != 0:
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf_access_info),
                    value((ns.i2nsfri_nsf_access_info, ns.hash),_confd.C_XMLBEGIN)
                ))
            if hasattr(nsf,"ip"):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_ip),
                        value(nsf.ip)
                    ))
            if hasattr(nsf,"port"):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_port),
                        value(nsf.port,_confd.C_UINT16)
                    ))
            if hasattr(nsf,"management_protocol"):
                result.append(
                    tagvalue(
                        xmltag(ns.hash, ns.i2nsfri_management_protocol),
                        value(nsf.management_protocol)
                    ))
            result.append(
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf_access_info),
                    value((ns.i2nsfri_nsf_access_info, ns.hash),_confd.C_XMLEND)
                ))
        
        result.append(tagvalue(xmltag(ns.hash,ns.i2nsfri_nsf),value((ns.i2nsfri_nsf, ns.hash),_confd.C_XMLEND)))
    return result

class ActionCallbacks(object):

    """
    The action callbacks needed.
    """

    def __init__(self, worker_sock):
        self.wsock = worker_sock

    def cb_init(self, uinfo):
        print("init_action called")
        dp.action_set_fd(uinfo, self.wsock)

    def cb_command(self, uinfo, path,argv):
        print("command called")

    def cb_action(self, uinfo, name, keypath, params):
        print("action called")
        print(keypath)
        for i, param in enumerate(params):
            print("param", i, "tag", param.tag,"value", param.v)
        print("uinfo:",uinfo)
        print("name:",name)
        self.uinfo = uinfo
        self.name = name
        self.params = params
        fun = self.action_switch()
        action_result = fun()
        if action_result is not None:
            return action_result

    def cb_abort(self, uinfo):
        print("Aborting outstanding action")
        # We need to clean  up the worker socket by replying
        dp.action_delayed_reply_error(uinfo, "aborted")

    def action_switch(self):
        """
        Hacky python switch
        """
        return {
            # ns.config_reboot: lambda: print("reboot"),
            ns.i2nsfri_nsf_capability_registration: self.nsf_capability_registration,
            ns.i2nsfri_nsf_capability_update: self.nsf_capability_update,
            # ns.config_restart: self.restart,
            # ns.config_reset: self.reset,
            # ns.config_abort_test: lambda: _confd.DELAYED_RESPONSE
        }[self.name.tag]

    def nsf_capability_registration(self):
        print("Capability Registration")
        
        param = self.params
        res = []
        res2 = []
        for p in param:
            if p.v.confd_type_str() == "C_LIST":
                print(f"tag: {value_as_string(p.tag)}")
                for val in p.v.as_list():
                    print(f"val: {val}")
                    print(f"valType: {val.confd_type_str()}")
                    print(f"valString: {value_as_string(val)}")
                    nsfs = database.getNSF2(value_as_string(p.tag),value_as_string(val))
                    print(nsfs)
                    print()
                    for nsf in nsfs:
                        if nsf.nsf_name not in res2:
                            res.append(nsf)
                            res2.append(nsf.nsf_name)
        print(res)
        result = xmlGen(res)
            #print(result)
        if result:
            dp.action_reply_values(self.uinfo, result)
        else:
            dp.action_delayed_reply_error(self.uinfo,"EMPTY")

    def nsf_capability_update(self):
        xmltag = _confd.XmlTag
        value = _confd.Value
        tagvalue = _confd.TagValue
        print("Capability Update")
        param = self.params
        name = str(param[0].v)
        if name == "test":
            result = [
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf),
                    value((ns.i2nsfri_nsf, ns.hash),
                    _confd.C_XMLBEGIN)
                ),
                tagvalue(
                    xmltag(ns.hash, ns.i2nsfri_nsf_name),
                    value(name)
                ),
                tagvalue(
                    xmltag(ns.hash,ns.i2nsfri_nsf),
                    value((ns.i2nsfri_nsf, ns.hash),
                    _confd.C_XMLEND)
                ),
            ]
            print(result)
            dp.action_reply_values(self.uinfo, result)


def connect(dctx, csock, wsock):
    """
    Connect the sockets
    """
    # Create the first control socket, all requests to
    # create new transactions arrive here
    dp.connect(dx=dctx,
               sock=csock,
               type=dp.CONTROL_SOCKET,
               ip='127.0.0.1',
               port=_confd.PORT,
               path='/')

    # Also establish a workersocket, this is the most simple
    # case where we have just one ctlsock and one workersock
    dp.connect(dx=dctx,
               sock=wsock,
               type=dp.WORKER_SOCKET,
               ip='127.0.0.1',
               port=_confd.PORT,
               path='/')


def read_data(dctx, sock):
    try:
        dp.fd_ready(dctx, sock)
    except (confd_errors.Error) as e:
        # Callback error
        if e.confd_errno is _confd.ERR_EXTERNAL:
            print(str(e))
        else:
            raise e


def poll_loop(dctx, ctrl_sock, worker_sock):
    """
    Check for I/O
    """
    _r = [ctrl_sock, worker_sock]
    _w = []
    _e = []

    try:
        while True:
            r, w, e = select.select(_r, _w, _e)

            for rs in r:
                if rs == ctrl_sock:
                    read_data(dctx=dctx, sock=ctrl_sock)
                elif rs == worker_sock:
                    read_data(dctx=dctx, sock=worker_sock)

    except KeyboardInterrupt:
        print("\nCtrl-C pressed\n")


def action_main():
    ctrl_sock = socket.socket()
    worker_sock = socket.socket()

    dctx = dp.init_daemon("actions_daemon")

    connect(dctx=dctx,
            csock=ctrl_sock,
            wsock=worker_sock)

    # register the action handler callback object
    acb = ActionCallbacks(worker_sock=worker_sock)
    dp.register_action_cbs(dctx, 'registration', acb)

    dp.register_done(dctx)
    print("register_done called")

    try:
        poll_loop(dctx, ctrl_sock, worker_sock)
    finally:
        worker_sock.close()
        ctrl_sock.close()
        dp.release_daemon(dctx)

if __name__ == "__main__":
    action_main()
