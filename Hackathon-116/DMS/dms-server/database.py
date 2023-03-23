import mysql.connector
from itertools import islice
import _confd

class nsfCapability():
    def __init__(self, 
        nsf_name = None, 
        version = None, 
        # directional_capabilities        = None,
        # system_event_capability         = None,
        # system_alarm_capability         = None,
        # ethernet_capability             = None,
        # ipv4_capability                 = None,
        # ipv6_capability                 = None,
        # icmpv4_capability               = None,
        # icmpv6_capability               = None,
        # tcp_capability                  = None,
        # udp_capability                  = None,
        # sctp_capability                 = None,
        # dccp_capability                 = None,
        # anti_ddos_capability            = None,
        # ips_capability                  = None,
        # anti_virus_capability           = None,
        # url_filtering_capability        = None,
        # voip_vocn_filtering_capability  = None,
        # time_capabilities               = None,
        # application_filter_capabilities = None,
        # device_type_capabilities        = None,
        # user_condition_capabilities     = None,
        # geographic_capabilities         = None,
        # ingress_action_capability       = None,
        # egress_action_capability        = None,
        # log_action_capability           = None,
        # resolution_strategy_capabilties = None,
        # default_action_capabilities     = None,
        # model               = None,
        # clock_speed         = None,
        # cores               = None,
        # threads             = None,
        # memory_capacity     = None,
        # memory_speed        = None,
        # disk_capacity       = None,
        # outbound            = None,
        # inbound             = None,
        # ip                  = None,
        # port                = None,
        # management_protocol = None

    ):
        self.nsf_name = nsf_name
        self.version = version
        # self.directional_capabilities         = directional_capabilities       
        # self.system_event_capability          = system_event_capability           
        # self.system_alarm_capability          = system_alarm_capability        
        # self.ethernet_capability              = ethernet_capability            
        # self.ipv4_capability                  = ipv4_capability                
        # self.ipv6_capability                  = ipv6_capability                
        # self.icmpv4_capability                = icmpv4_capability              
        # self.icmpv6_capability                = icmpv6_capability              
        # self.tcp_capability                   = tcp_capability                 
        # self.udp_capability                   = udp_capability                 
        # self.sctp_capability                  = sctp_capability                
        # self.dccp_capability                  = dccp_capability                
        # self.anti_ddos_capability             = anti_ddos_capability           
        # self.ips_capability                   = ips_capability                 
        # self.anti_virus_capability            = anti_virus_capability          
        # self.url_filtering_capability         = url_filtering_capability       
        # self.voip_vocn_filtering_capability   = voip_vocn_filtering_capability 
        # self.time_capabilities                = time_capabilities              
        # self.application_filter_capabilities  = application_filter_capabilities
        # self.device_type_capabilities         = device_type_capabilities       
        # self.user_condition_capabilities      = user_condition_capabilities    
        # self.geographic_capabilities          = geographic_capabilities        
        # self.ingress_action_capability        = ingress_action_capability      
        # self.egress_action_capability         = egress_action_capability       
        # self.log_action_capability            = log_action_capability          
        # self.resolution_strategy_capabilties  = resolution_strategy_capabilties
        # self.default_action_capabilities      = default_action_capabilities
        # self.model               = model              
        # self.clock_speed         = clock_speed        
        # self.cores               = cores              
        # self.threads             = threads            
        # self.memory_capacity     = memory_capacity    
        # self.memory_speed        = memory_speed       
        # self.disk_capacity       = disk_capacity      
        # self.outbound            = outbound           
        # self.inbound             = inbound            
        # self.ip                  = ip                 
        # self.port                = port               
        # self.management_protocol = management_protocol

def getNSF(key,value):
    mydb = mysql.connector.connect(
        host="localhost",
        user="root",
        password="dmspassword",
        database="nsf"
    )

    mycursor = mydb.cursor()

    mycursor.execute(f"SELECT * FROM nsf where FIND_IN_SET('{value}',`{key}`)>0")
    num_fields = len(mycursor.description)
    field_names = [i[0] for i in mycursor.description]
    #print(field_names)
    myresult = mycursor.fetchall()
    nsf = []
    n = 0
    for x in myresult:
        
        i = 0
        nsf.append(nsfCapability())
        for y in x:
            if y is not None:
                setattr(nsf[n],field_names[i].replace("-","_"),y)
            i += 1
        n+=1
    return nsf

def getNSF2(key, value):
    mydb = mysql.connector.connect(
        host="localhost",
        user="root",
        password="dmspassword",
        database="nsf"
    )

    mycursor = mydb.cursor()

    mycursor.execute(f"""SELECT DISTINCT TABLE_NAME 
                         FROM INFORMATION_SCHEMA.COLUMNS
                         WHERE COLUMN_NAME IN ('{key}')
                            AND TABLE_SCHEMA='nsf';""")
    parent = mycursor.fetchone()
    #print(parent[0])

    mycursor.execute(f"SELECT id FROM `{parent[0]}` where FIND_IN_SET('{value}',`{key}`)>0;")
    child = mycursor.fetchall()
    #print(child)
    curParent = parent
    if child:
        while parent is not None:
            mycursor.execute(f"""SELECT DISTINCT TABLE_NAME 
                                FROM INFORMATION_SCHEMA.COLUMNS
                                WHERE COLUMN_NAME IN ('{parent[0]}')
                                    AND TABLE_SCHEMA='nsf';""")
            curParent = mycursor.fetchone()
            if not curParent:
                break
            childID = [c[0] for c in child]
            where = []
            for a in childID:
                where.append(f"`{parent[0]}` = {a}")
            mycursor.execute(f"SELECT * FROM `{curParent[0]}` where {' or '.join(x for x in where)};")
            child = mycursor.fetchall()
            #print(child)
            parent = curParent
    else:
        return []

    nsf = []
    n = 0

    for x in child:

        mycursor.execute(f"""
                            SELECT A.*, b.*, c.*, d.*, e.*, f.*, g.*, h.*, i.*, j.*, k.*, l.*, m.*
                            FROM nsf A
                            JOIN `event-capabilities` b ON A.`event-capabilities` = b.id
                            JOIN `condition-capabilities` c ON A.`condition-capabilities` = c.id
                            JOIN `advanced-nsf-capabilities` d ON c.`advanced-nsf-capabilities` = d.id
                            JOIN `generic-nsf-capabilities` e ON c.`generic-nsf-capabilities` = e.id
                            JOIN `context-capabilities` f ON c.`context-capabilities` = f.id
                            JOIN `action-capabilities` g ON A.`action-capabilities` = g.id
                            JOIN `nsf-specification` h ON A.`nsf-specification` = h.id
                            JOIN `cpu` j ON h.`cpu` = j.id
                            JOIN `memory` k ON h.`memory` = k.id
                            JOIN `disk` l ON h.`disk` = l.id
                            JOIN `bandwidth` m ON h.`bandwidth` = m.id
                            JOIN `nsf-access-info` i ON A.`nsf-access-info` = i.id
                            WHERE A.`nsf-name` = '{x[0]}'
                            GROUP BY A.`nsf-name`;
                        """)
        num_fields = len(mycursor.description)
        field_names = [i[0] for i in mycursor.description]
        myresult = mycursor.fetchall()
        for y in myresult:
            i = 0
            nsf.append(nsfCapability())
            for z in y:
                if z is not None:
                    setattr(nsf[n],field_names[i].replace("-","_"),z)
                i += 1
        n+=1
    return nsf

    # nsf = []
    # n=0
    # for x in child:
    #     i = 0
    #     nsf.append(nsfCapability())
    #     for y in x:
    #         if y is not None:
    #             setattr(nsf[n],field_names[i].replace("-","_"),y)
    #             mycursor.execute(f"SELECT * FROM information_schema.tables WHERE table_schema = 'nsf' AND table_name = '{field_names[i]}' LIMIT 1;")
    #             tableVerify = mycursor.fetchall()
    #             if tableVerify:
    #                 mycursor.execute(f"SELECT * FROM `{field_names[i]}` where `id` = {y};")
    #                 print(mycursor.fetchall())
    #         i += 1
    #     n+=1
    # print(dir(nsf[0]))
    
    # generic = mycursor.fetchall()
    # for x in generic:
    #     for y in x:
    #         mycursor.execute(f"SELECT id FROM `condition-capabilities` where `generic-nsf-capabilities` = {y}")
    #         condition = mycursor.fetchall()
    #         print(condition)

    # for x in condition:
    #     for y in x:
    #         mycursor.execute(f"SELECT * FROM `nsf` where `condition-capabilities` = {y}")
    #         result = mycursor.fetchall()
    #         print(result)
    

#print(getNSF2("ipv4-capability","source-address"))
#print(getNSF2("ips-capability","signature-set"))
#print(getNSF2("time-capabilities","absolute-time"))
# print(dir(getNSF2("ingress-action-capability","pass")[0]))
# print(getNSF2("ingress-action-capability","pass")[1].time_capabilities)
# nsf = getNSF("ipv4-capability","source-address")
# print(nsf[1].ipv4_capability)

# nsf = []
# for x in getNSF("ipv4-capability","source-address"):
#     nsf.append(nsfCapability(name = x[0], version=x[1], id = x[2]))
#     for y in x:
#         print(y)

# print(nsf[0].directional_capabilities)

