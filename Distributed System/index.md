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
