---
title: "Distributed System"
author:
  - "Andrea Lunghi"
---

## Introduction

A distributed system is a collection of independent computer that appears to the users as a single computer.

Some key aspects of distributed systems are:

- **Heterogeneity**: Different hardware, operating systems, programming languages, and network technologies can be integrated into a single distributed system.
- **Openness**: The system should be designed to allow easy integration of new components, technologies, and standards.
- **Security**: each information should be: confidential (only authorized users can access it), integrity (data cannot be altered without detection), and available (data is accessible when needed).
- **Scalability**: The system should be able to handle increasing workloads and accommodate growth in the number of users and resources without significant performance degradation.
- **Failure Handling**: A failure in a distributed system should cause only a partial failure of the system, not a total one. The system should be able to detect, recover from, and tolerate failures.
- **Concurrency**: A distributed system is inherently concurrent, with multiple processes running simultaneously on different machines. Proper synchronization and coordination mechanisms are essential to ensure consistency and correctness, as there isn't a single clock.
- **Transparency**: The complexity of the distributed system should be hidden from users and applications. This includes:
  - Access Transparency: Users should not need to know how resources are accessed.
  - Location Transparency: Users should not need to know the physical location of resources.
  - Migration Transparency: Resources can move without affecting users.
  - Replication Transparency: Users should not know if a resource is shared between other users.
  - Failure Transparency: The system should continue to operate despite failures.

## Distributed System Architecture

### High-Level Architecture

A distributed system is composed of multiple machines connected through a network. Those machines can be:

- **Network-OS based**: Communication and resource location are explicitly managed by the application programmer, resulting in low transparency. Access is typically via standard network APIs (e.g., sockets, file sharing protocols).
- **Middleware-based**: A software layer sits between the application and the OS/network, providing an high-level API (e.g., RPC, RMI). It offers high transparency by hiding heterogeneity and distribution.

### Runtime Architecture

A distributed system can be classified based on the components, the connections, the data exchange, and the interaction between them.

#### Client-Server

This is the most common architecture.

It is based on two component:

- **Server**: A passive entity that is responsible to provide a services through some defined API;
- **Client**: An active component that request the services to the server.

In distributed system one server can use a service from other servers creating a **multi-tier** (N-tier) infrastructure.

The services that can be distributed are:

- User interface;
- Application logic;
- Data.

This architecture is easy to manage and scale, but the server is a single point of failure and a bottleneck for the system.

#### Service Oriented

The service oriented architecture is based on three components:

- **Service Provider**: is a component that publish a service that he provide;
- **Service Broker**: holds a description to all the service available;
- **Service Consumer**: search for a service in the broker and then interacts _directly_ with the provider.

The interface is described with a standard language (e.g. WSDL).

The communication is done through a standard protocol (e.g. SOAP).

#### REST

REpresentional State Transfer (REST) is an architectural style that define how web standards should be used.

The interaction happen between a client and a server. The interaction is stateless, meaning that each request from the client to the server must contain all the information needed to understand and process the request.

The response must be explicitly labeled as cacheable or non-cacheable.

The server interface must satisfy four constraints:

1. **Identification of resources**: Each resource is identified by a unique URI;
2. **Manipulation of resources through representations**: The communication is done through representations of the resource (e.g. JSON, XML, HTML), selected dynamically based on the client needs;
3. **Self-descriptive messages**: Each message contains metadata about how to process it (e.g. HTTP headers);
4. **Hypermedia as the engine of application state**: The server provides links to other resources dynamically, allowing clients to navigate the application state.

#### Peer-to-Peer

All the components in a peer-to-peer architecture are equal and can act as both client and server.

A server, in a client-server architecture, represents a single point of failure and a bottleneck for the system. In a peer-to-peer architecture, the failure of a single node does not affect the overall system, as other nodes can take over its responsibilities.

#### Object-Oriented

An object-oriented architecture is based on the concept of objects that encapsulate data and behavior. The objects provide an interface to interact with them.

#### Data-Centred

A data-centered architecture is based on a shared data space (**Repository** - tuple space or shared global state) that is accessible by all the components in the system.

Data can be added or read from the repository.

When performing a read operation, the component can either take from the repository (destructive read) or read the data without removing it (non-destructive read).

The components provide a _pattern_ (or **template**) to access the data. If there is no match the component will wait until there is one.

#### Event-Based

An event-based architecture is based on the concept of events that are generated by components in the system.

Other components can subscribe to events and be notified when an event occurs.

#### Mobile Code

The Mobile Code architectural style allows for the dynamic movement of code or an entire running process across the network at runtime. This capability enhances a system's flexibility, enabling it to receive new functionality without being halted or manually updated.

This style is often implemented in languages that run on a virtual machine (VM), such as Java or JavaScript, because the VM provides a crucial layer of abstraction and control over the executing environment.

The ability to receive and execute code from an external source is a major security risk.

##### Code on Demand

The client retrieves the executable code (or script) from a server and executes it locally.

Only the code itself moves (_Weak Mobility_).

> An example is a web browser that downloads and executes JavaScript code from a web server.

##### Remote Evaluation

The client sends a request that is executed as code on the server.

The server performs the computation and returns only the result to the client (_Weak Mobility_).

> An example is an SQL query that is sent to a database server for execution, and the results are returned to the client, or a cloud notebook like Google Colab where the code is executed on a remote server.

##### Mobile Agent

A process, including both its code and its complete execution state (data, program counter, stack), is suspended, migrated to a new machine, and then resumed (_Strong Mobility_).

### Interactions

The behavior of distributed system is determined by a **distributed algorithm**: an algorithm executed collaboratively across multiple, independent machines. Due to the distribution, the algorithm must handle _communication_, _synchronization_, and _fault tolerance_.

A critical component of the performance and behavior of a distributed algorithm is the **time**: speed of the processes, the performance of the communication channel, and the _clock drift_ (the time difference between the different machines) rates between machines.

There are two types of distributed systems based on time:

- **Asynchronous**: there are _no bounds_ for the time components. This is the most realistic model, but it's hard to design algorithms for it.
- **Synchronous**: each component has a _known bound_ for the time. This is an idealized model that is easier to design algorithms for.

> **Pepperland Example**
>
> There are two generals on top of two hills. They need to sync who will lead an assault and when to start it. They can communicate on a reliable channel.
>
> To choose the leader they could both choose a random number, the bigger on win.
>
> In a async system is impossible to choose a time to charge as there is no bound on the time. One general could send a message to the other, but there is no guarantee that the message will arrive in a specific time. The other general could wait for the message, but there is no guarantee that it will arrive. The only solution is to charge immediately, but this could lead to a failure if the other general doesn't charge at the same time.

### Failures

In a distributed system, the key advantage is that failures are partial (one component fails, not the whole system), and the goal is to mask these failures from the client, achieving fault tolerance.

There are different types of failures:

- **Omission Failure**: A component simply fails to perform an action it was supposed to:
  - _Process_: A process crash or halt and doesn't execute any operation and become _silent_;
  - _channel_: A message is lost and never received (or never sent).
- **Byzantine Failures** (Arbitrary failure): These are hard to detect, if there is an error it might be detected with an omission
  - _Process_: The process executes an incorrect or unintended program. (e.g. memory failure that change the stack pointer, or compromised by a virus).
  - _channel_: Messages are corrupted, duplicated, fabricated.
- **Timing Failure**: Are only relevant in synchronous systems and happens when the time bound are violated.

#### Failure Detection

To detect a failure, a process can send a _heartbeat_ message to another process. If the message is not received within a certain time, the process is considered failed.

In an asynchronous system, it's impossible to distinguish between a slow process and a failed process. To mitigate this, a _timeout_ can be used, but it can lead to false positives.

The same happens for unreliable channels, where a message can be lost or delayed, making hard to distinguish between a failed process and a lost message.

## Communication

Communication protocols define how processes in a distributed system exchange information.

These protocols are primarily classified along two dimensions:

- Time dependency: The client and server must be active at the same time (_Transient_) or not (_Persistent_);
- Synchronization: The sender waits for the message to be sent, received, or processed (_Synchronous_) or not (_Asynchronous_).

### Remote Procedure Call (RPC)

**Remote Procedure Call** (RPC) is a communication abstraction that allows a program to execute a procedure or function in a different machine as if it were a local call (_Location Transparency_).

The RPC is build upon a middleware layer that hides the network communication.

This is done using a proxy called **Stub** that is responsible to:

- Serialize structured data into a stream of bytes;
- Marshall the data into a specific representation for the network;
- Deserialize and unmarshall the data on the other side.

The stubs are defined with an **Interface Definition Language** (IDL) that is language independent, allowing to have different languages between the caller and the callee.

#### RPC Communication

The communication is done by passing parameters to the remote procedure.

The standard method is to pass parameters by value, as there is no shared memory between the two machines.

If there is a need to pass parameters by reference, it's possible to simulate it using the _copy/restore_ method: the value is passed by value, but when the function ends, the return value of the parameters is cloned in the caller.

The RPC communication is typically _synchronous_ (blocking), to mimic the local procedure call behavior, but it can be also:

- _Asynchronous_: for void procedures, where the client doesn't wait for the server to process the request;
- _Deferred_: the client receive a **Future** object and than continue execution and only wait for the result when it's needed.

The communication can be optimize with two strategies:

- **Batching**: wait for multiple requests to create a single package to improve bandwidth;
- **Queuing**: store requests by the middleware and wait to send them if the destination is unreachable, improving resilience and persistency.

#### RPC Middleware

The client needs to locate the server that provide the requested service.

This is done through two logics:

- **PortMap**: in system like _Sun RPC_, each server register the services it provide with a portmap daemon that store the mapping between the service UUID and the host/port;
- **Directory Service**: in system like _DCE RPC_, there is a directory service that store the mapping between the service name and the host/port.

To conserve resources the server can be **Dynamically Activated** the server once a request arrive.

#### Inter-Process communication (IPC)

When two distinct process are on the same machine, it would be possible to use RPC to communicate, but it would be inefficient.

Using a Lightweight RPC that use a shared memory accessible from the middleware.

### Remote Method Invocation (RMI)

**Remote Method Invocation** (RMI) is the object-oriented counterpart to RPC. It allows a process to invoke a method on a remote object residing in another process.

The communication is done through method calls on remote objects (**Stub**) that acts as if they were local. This is done because it's not possible to pass objects by value between two different machines that might use different programming languages.

In java RMI, as both the client and the server use the same language, it's possible to pass objects, but the code must be available or downloaded dynamically.

### Message Oriented Communication (MOC)

**Message Oriented Communication** is a foundational style in distributed systems, where interaction is based on the asynchronous exchange of discrete, self-contained messages. It is generally a one-way interaction, often based on events.

#### Basic Message Passing

The most fundamental form of MOC uses low-level **socket programming**, relying on the _Operating System_ primitives for communication.

- **Transmission Control Protocol** (TCP) Sockets: Provide a connection-oriented, reliable communication channel between two processes (point-to-point).
- **User Datagram Protocol** (UDP) Sockets: Provide a connectionless, unreliable connection. A single socket can receive messages from multiple clients. They support broadcast and multicast addressing.

#### Message Passing Interface (MPI)

**Message Passing Interface** (MPI) is a library that provides a rich set of primitives to manage communication in parallel and distributed systems.

It allows to control the _synchronization level_ of the communication and avoids explicit serialization of data.

Messages can be sent in broadcast or scattered between multiple processes to perform parallel computation.

#### Message Queueing

**Message Queueing** is a system that introduces a persistent storage between the client and the server (queue) to store messages.

This allows to **decouple** the client and the server in _time_ and _space_.

The client _push_ messages in the queue and the server _pop_ messages from the queue asynchronously and they act as peers.

Inside the system there could be multiple queues identified by a name that can be statically or dynamically created.

This decoupling allows to scale the system easily by adding more servers to process the incoming messages.

In the system there could be managers that act as relay between multiple queues to create complex topologies, increasing the fault tolerance.

#### Publish-Subscribe

The **publish-subscribe** model is an event-driven architecture where _publishers_ generate events/messages without knowing who the receivers are, and _subscribers_ express interest in events without knowing who published them.

This allows to decouple the event with the space but the communication is _transient_ as only online subscribers will receive the messages.

A component can subscribe based on:

- **subject-based** (topic-based): Subscribers express interest in a predefined category or topic (e.g., subscribing to the "Stock Market/Technology" topic). This is efficient for the dispatcher to process but less expressive.
- **content-based**: Subscribers provide a predicate (a condition) over the content of the message (e.g., subscribing to "Stock Market/Technology where Price $> 100$ AND Volume $> 1M$"). This is more expressive but significantly more expensive for the dispatcher to process.

The core component of this architecture is the **Event Dispatcher** (broker) that manage the subscriptions and the notifications.

A **Complex Event Processing** system is often layered on top of the dispatcher. It analyzes streams of incoming simple events to detect patterns, correlations, or anomalies, and then generates a single, higher-level complex event (e.g., detecting "Fire" from simple events like "Smoke detected" and "High Temperature reading").

#### Distributed Publish-Subscribe

To overcome the Event Dispatcher bottleneck, a distributed architecture is organized into a _network_ of message brokers and the message is forwarded across the network using different strategies.

For **acyclic** graphs:

- **Message Forwarding**: Each broker only knows its local subscribers. When an event arrives, the broker forwards it to all neighboring brokers and all local subscribers. With this strategy the subscription is cheap as the subscription is stored locally, but the message need to be flood.
- **Subscription Forwarding**: Subscriptions are forwarded up the network hierarchy or to neighbors. Each broker maintains a routing table showing which neighbors have expressed interest in a given subscription. In this way the message is sent only to the interested brokers, reducing the amount of message, but increasing the subscription cost.
- **Hierarchical Forwarding**: Brokers are organized into a hierarchy with a single Root Broker. Subscriptions flow up to the root. Messages flow up to the point where a common interested path is found, then travel down to the subscribers. Good locality and efficient event forwarding, but high load and centralization risk on the root broker.

Paths can be optimized if some are a subset of others.

**Cyclic** topologies are more _fault-tolerant_ but introduce the problem of _message loops_ (flooding) and uncertainty about delivery paths.

- **Distributed Hash Table** (DHT): Each broker is assigned an ID. Events are hashed to find the successor node (node with an ID greater or equal). The message is routed towards this successor, and routing information is collected along the way to guide the message to actual subscribers.
- **Content-Based Routing**: each broker store a routing table to forward the message based on the content of the message, creating a spanning tree.
  - **Per-Source Routing** (PSR): each broker store a routing table with $<source, \text{next hop}, \text{event type}>$.
  - **Improved Per-Source Forwarding** (iPSF): an optimized version of PSR that aggregate indistinguishable sources.
  - **Per-Receiver Forwarding** (PRF): each broker store all the events that a specific broker is interested in and in another table the next hop to reach that broker.

### Stream Oriented communication

**Stream-Oriented Communication** involves transmitting a continuous, _ordered_ sequence of data from a source to a sink. This model is essential for multimedia and real-time applications where the timing of the data arrival is often as critical as its content.

#### Timing Constraints

In streaming, the correctness of the communication is heavily impacted by time, leading to three classifications based on timing guarantees:

- **Asynchronous**: Data are transmitted without any timing constraint;
- **Synchronous**: there is a max delay between the sending and receiving of data;
- **Isochronous**: there is a min and a max time constraint between the sending and receiving of data, limiting the delay variation (_jitter_).

#### Quality of Service (QoS)

The network doesn't guarantee the _Quality of the service_. Some metrics that need management:

- Bit Rate: The guaranteed data rate.
- Latency/Delay: Time to set up the connection and receive data.
- Jitter: The variance in delay ($\Delta D$) between consecutive data units.

Streaming is implemented using UDP instead of TCP. This is because, for real-time data, a retransmitted packet is useless, as it arrives too late for playback. UDP's speed and lack of automatic retransmission make it suitable.

The QoS are managed client and server side using different techniques:

- **Buffering**: the client stores incoming data in a buffer and the playback starts only after is filled after a threshold, dynamically adjusted based on network conditions. This is done to smooth out jitter;
- **Forward Error Connection**: if the application enters in an invalid state, it will go to the next valid state, instead of asking back the missing one. Missing data are concealed using interpolation or extrapolation techniques;
- **Interleaving data**: data are not sent sequentially. A single network packet contains non-consecutive fragments of multiple frames. If the packet is lost, only some non-consecutive frames are lost, which can be concealed more easily;

#### Multiple Streams Synchronization

Is possible that multiple streams need to be synchronized (e.g., audio and video). This can be done:

- **Client-side**: The client uses timestamps to synchronize streams during playback. Each stream includes timing information, allowing the client to align data from different streams.
- **Server-side**: The server merges streams before transmission.

## Naming

In a distributed system, **naming** is the mechanism used to reference and locate system entities, which can range from physical hosts, files, and services to users and abstract processes.

Names can be _human-friendly_ (e.g., "www.example.com") or _machine-friendly_ (e.g., IP addresses).

A name can also be _global_ (unique across the entire system) or _local_ (unique within a specific context or domain).

The **address** is the actual location of the entity in the network (e.g., an IP address) and it can be mutable and change over time.

An entity must be identified by an immutable _Identifier_ and a using a **name resolution** to convert that name to an address.

Name resolution can be performed in different ways:

### Flat Naming

A name is **flat** if it is a simple sequence of characters that contains no structural or topological information about the entity's location.

#### Simple Solution

This method is suitable only for small-scale, local area environments where network traffic is manageable.

To locate an entity with a specific name, a request is broadcasted to all hosts on the local network segment. The host that recognizes the name responds with its address.

An example is ARP, that broadcasts a request to find the MAC address associated with an IP address.

#### Home Based

This strategy handle **mobility** by having a fixed home address for each entity.

When an entity moves to a new location, it registers its new address with its home server.

When another entity wants to communicate with it, it first contacts the home server that return a **Forwarding Pointer** (the current address) allowing direct communication.

This adds an extra step (the trip to the Home Host) to every connection setup, increasing latency

#### Distributed Hash Table

**DHTs** create a scalable, decentralized system for mapping flat names (keys) to addresses (values) across thousands of nodes.

Each node in the DHT is assigned a unique identifier (ID) from the same address space as the keys. Than the key space is partitioned among the nodes based on the hash of the key (the first node with the id greater than the key is responsible for that key).

The nodes form an _overlay network_, organized as a logical ring.

Finding the node responsible for a key can be done with different strategies:

- **Chord**: each node knows only its successor and predecessor. To find a key the request is forwarded to the successor until the node is found. This is inefficient as it requires $O(N)$ hops in the worst case.
- **Chord finger table**: each node maintains a finger table with $O(log N)$ entries, each pointing to a node at a distance of $2^i$ from itself. This allows to find the key in $O(log N)$ hops.

#### Hierarchical

Hierarchical distribution organizes names into a tree-like structure, where each subtree represents a directory and entities (such as files or services) are leaves.

In this model, each directory node maintains information about its children and parent. When resolving a name, the process follows these steps:

- If a node knows of a child that knows the address of the requested entity, it forwards the request directly to that child.
- If not, it forwards the request to its parent node, which repeats the process until the root or a node with the information is reached.

Once resolved, the address is returned along the path, and intermediate nodes can cache the result for future queries to improve performance.

This approach excels in local domains, where queries are confined to nearby branches, reducing global network traffic. However, it can suffer from bottlenecks at higher-level nodes if not balanced properly.
