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
