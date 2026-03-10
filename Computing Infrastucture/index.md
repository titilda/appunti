---
title: "Computing Infrastructure"
author:
  - "Andrea Lunghi"
---

## Data Centers

As internet adoption grew, computing shifted from local machines to centralized data centers, large facilities housing thousands of servers and providing computational power and storage for various applications.

Data centers are geographically distributed in areas with favorable cooling and power conditions, reducing user latency and improving fault tolerance.

### Benefits of Centralized Computing

**User Benefits:**

- Ease of management: The user doesn't need to worry about hardware maintenance, backups, software updates, or security patches.
- Ubiquity: Users can access their applications and data from any device with an internet connection, enabling remote work and global collaboration.
- Compute power: Some applications require more computational resources than a typical personal computer can provide. Cloud computing allows users to access powerful servers that can handle intensive workloads, such as data analysis, machine learning, and video rendering.

**Vendor Benefits:**

- Homogeneity: The vendor can optimize their code for a specific hardware configuration, instead of caring about the wide variety of user hardware.
- Change management: The vendor can update the code and deploy it to all users at once, without relying on users to update their local software.

**Infrastructure Benefits:**

- Scalability: Servers can be added or removed based on demand, enabling efficient resource allocation.
- Cost-effectiveness: Providers achieve economies of scale, reducing per-user costs.
- Multi-tenancy: Multiple customers share the same data center, maximizing server utilization instead of remaining idle.

### Warehouse-Scale Computing

**Warehouse-scale computing** (WSC) is a type of data center architecture that treats thousands of interconnected servers as a single unified system.

This enables running large-scale applications (search engines, social media platforms, online gaming services) that require significant computational resources to be efficiently managed and scaled.

Many such providers also offer cloud services, virtualizing their infrastructure for external customers, allowing a traditional data center to be built on top of a warehouse-scale computing infrastructure.

### Geographic Distribution

Global cloud infrastructure is hierarchically organized for redundancy and low latency:

- **Geographical Area (GA)**: Determines data residence requirements.
- **Computing Regions**: At least two per GA, separated by ≥100 miles to avoid common failures (earthquakes, natural disasters). Allows disaster recovery but too distant for synchronous replication.
- **Availability Zones**: Multiple zones (min. 3) within a region, isolated yet close enough for synchronous replication. Provides a finer-grained redundancy level for critical applications enabling faster recovery from failures.
- **Edge Locations**: Smaller data centers closer to users. Used for CDNs and caching to reduce latency and improve content delivery speed.
- **Local Zones**: Metropolitan-area data centers providing ultra-low latency for location-specific applications.

### Physical Architecture

Data center architecture is similar to that of personal computers but at a massive scale.

**Computing Components:**

- **Servers**: Standardized physical machines in rack format (U) providing computational power and storage. Functionally equivalent to regular computers.
- **Networking equipment**: Switches, routers, and firewalls connecting servers and providing internet access.
- **Storage systems**: Additional storage capacity for applications and data.

**Support Infrastructure:**

- **Power supply**: Continuous, reliable power delivery. Redundancy includes backup generators and uninterruptible power supplies (UPS) to handle outages.
- **Cooling systems**: Manage server heat through air conditioning, liquid cooling, or hybrid approaches to maintain optimal operating temperatures.
- **Failure recovery**: Ensures system availability via batteries, diesel generators, and other redundancy mechanisms.
