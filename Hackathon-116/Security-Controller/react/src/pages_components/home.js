import React from 'react'

export default function Home() {
  return (
    <div style={{marginRight:"200px",marginLeft:"200px",fontSize:"large"}}>
        <h3>
          A Network Security Function (NSF) is a function used to ensure integrity, confidentiality, or availability of network communications, to detect unwanted network activity, or to block or at least mitigate the effects of unwanted activity. NSFs are provided and consumed in increasingly diverse environments. Users could consume network security services enforced by NSFs hosted by one or more providers, which may be their own enterprise, service providers, or a combination of both. Similarly, service providers may offer their customers network security services that are enforced by multiple security products, functions from different vendors, or open source technologies. NSFs may be provided by physical and/or virtualized infrastructure. Without standard interfaces to control and monitor the behavior of NSFs, it has become virtually impossible for providers of security services to automate service offerings that utilize different security functions from multiple vendors.
        </h3>
        <h3>
          The goal of I2NSF is to define a set of software interfaces and data models for controlling and monitoring aspects of physical and virtual NSFs, enabling clients to specify rulesets. If the working group finds it necessary to work on an information model before the data models, to help provide guidance and derive the data models, it may do so. The working group will decide later whether the information model needs to be published as an RFC. Other aspects of NSFs, such as device or network provisioning and configuration, are out of scope. As there are many different security vendors or open source technologies supporting different features and functions on their devices, I2NSF will focus on flow-based NSFs that provide treatment to packets/flows, such as Intrusion Protection/Detection System (IPS/IDS), web filtering, flow filtering, deep packet inspection, or pattern matching and remediation.
        </h3>
        <h3>
          Controlling and monitoring aspects of NSFs include the ability to specify rules (by a single controller at the first phase), query, and monitor NSFs by one or more management entities. The initial phase of I2NSF will only consider one single controller that can specify or change rules to NSFs because multi-headed control requires the coordination to avoid potential conflict of rules. The NSFs can be monitored by multiple entities, but the database update and synchronization among multiple management entities are out of scope for I2NSF.
        </h3>
        <h3>
          I2NSF will specify interfaces at two functional levels for the control and monitoring of network security functions:
        </h3>
        <ul>
          <h3>
            The I2NSF Capability Layer specifies how to control and monitor NSFs at a functional implementation level. The term "Functional Implementation" is used to emphasize that the rules (for control and monitor) of NSFs have to be implementable by most NSFs. I2NSF will standardize a set of interfaces by which a security controller can invoke, operate, and monitor NSFs.
          </h3>
          <h3>
            The I2NSF Service Layer defines how clients' security policies may be expressed to a security controller. The controller implements its policies according to the various capabilities provided by the I2NSF Capability Layer. The I2NSF Service Layer also allows the client to monitor the client specific policies.
          </h3>
          <h3>
            A client may leverage the I2NSF Service Layer interface to express security policies to a security controller, which in turn interacts with one or more NSFs through the I2NSF Capability Layer interface. Alternatively, a client may interact with one or more NSFs directly through the I2NSF Capability Layer interface.
          </h3>
        </ul>
    </div>
  )
}
