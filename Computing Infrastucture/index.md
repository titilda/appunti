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

- **Servers**: Standardized physical machines providing computational power and storage. Functionally equivalent to regular computers.
- **Networking equipment**: Switches, routers, and firewalls connecting servers and providing internet access.
- **Storage systems**: Additional storage capacity for applications and data.

**Support Infrastructure:**

- **Power supply**: Continuous, reliable power delivery. Redundancy includes backup generators and uninterruptible power supplies (UPS) to handle outages.
- **Cooling systems**: Manage server heat through air conditioning, liquid cooling, or hybrid approaches to maintain optimal operating temperatures.
- **Failure recovery**: Ensures system availability via batteries, diesel generators, and other redundancy mechanisms.

#### Server

Servers are fundamental computing units in data centers, designed for performance, reliability, and scalability.

##### Form Factors

Servers are made in different standard form factors such us:

- **Rack-mounted** (most common): Standardized units (1U = 44.45 mm height) fitting into vertical racks. Racks integrate power distribution, cooling, networking, and cable management, enabling high density and efficient operations. Excellent space efficiency but complex cable management at scale.
- **Blade servers**: Vertically oriented, ultra-compact form factor. Highest component density per unit of space but requires specialized cooling due to high power density. These are more expensive than rack-mounted.
- **Tower servers**: Standalone units resembling desktop computers. Low density with simple cooling/maintenance, and lower cost. Rarely used in modern data centers due to poor scalability and space inefficiency.

##### Components

- **Motherboard**: Central circuit board interconnecting all components.
- **CPUs**: 1 to 8 processors per server.
- **RAM**: 2 to 192 DIMM slots for main memory.
- **Storage**: Multiple hard drives or SSDs for persistent data.
- **Specialized Hardware** (optional):
  - **GPUs**: Accelerate parallel compute tasks (machine learning, scientific computing). Communicate via NVLink (high-speed interconnect) to minimize latency bottlenecks.
  - **TPUs**: Tensor Processing Units specialized for neural network training/inference thanks to optimized matrix operations.
  - **FPGAs**: Field-Programmable Gate Arrays. Customizable hardware programmed for specific low-latency, application-specific acceleration (real-time processing, network processing).

All components are standardized for quick replacement and maintenance, with hot-swappable parts to minimize downtime.

##### Thermal Management

Data centers uses **cold aisle/warm aisle** configuration to maximize the air cooling efficiency:

- **Cold aisle**: Center floor supplies cold air; flows through server intake ports.
- **Warm aisle**: Rear of servers; hot exhaust air expelled upward.
- **Containment**: Roof caps on racks prevent cold air bypass, forcing air through servers and maximizing cooling efficiency.

#### Storage

WIth time the data have been moved from local towards cloud providers. This is due:

- Ease of management, with automatic backups and data recovery;
- Low price;
- Ease of access everywhere there is an internet connection.

##### File System Abstractions

OS manages data through hierarchical abstractions:

- **Data Blocks**: Smallest units of storage, addressable with **logical block addresses** (LBA).
- **Clusters**: Groups of contiguous blocks, used to reduce overhead compared to block-level management, reducing the number of read/write operations.
Inside each cluster there are the actual data and the **Metadata** that consist in the file attributes (name, size, permissions, timestamps) enabling organization and access control.

During data deletion, the cluster is only flagged as deleted, allowing it to be overwritten.

##### Space Allocation

The storage unit is represented as a multiple of the cluster size:

$$\text{Disk Size} = \lceil \frac{\text{File Size}}{\text{Cluster Size}} \rceil \times \text{Cluster Size}$$

when a file is smaller than the cluster size, sime of its space is wasted leading to **internal fragmentation**:

$$\text{Wasted Space} = \text{Disk Size} - \text{File Size}$$
When a file's clusters are non-contiguous (fragmented), read/write operations require multiple seeks, degrading performance. In these cases it's useful to perform **defragmentation** to rearrange sectors into sequential blocks.

##### Hard Disk Drives

**Physical Structure:**

HDDs contain rotating magnetic _platters_ coated with ferromagnetic material. Data is stored as magnetic patterns organized into:

- **Tracks**: Concentric circles on each platter.
- **Sectors**: Divisions of tracks, the smallest atomic read/write unit.

The platters are mounted on a spindle and spin at high speeds (RPM). An actuator arm with a **read/write head** moves across the platters to access data.

The entire assembly is enclosed in a sealed case to protect against dust, scratches, and environmental contaminants, while also providing shock resistance.

###### Access Time Components

During the read/write process, several time components contribute to the total access time:

- **Seek Time**: Time for actuator arm to position head over target track. Heuristic: $T_\text{Seek} \approx \frac{T_\text{max}}{3}$
- **Rotation Delay**: Average time for target sector to rotate under head: $T_\text{Rotation} = \frac{1}{2} \times \frac{60}{\text{RPM}}$
- **Transfer Time**: Duration to read/write data at disk transfer rate, based on the amount of data and the disk's throughput.
- **Controller Overhead**: Command processing and disk preparation time.

The total access time is the sum of these components:
$$T_\text{Access} = T_\text{Seek} + T_\text{Rotation} + T_\text{Transfer} + T_\text{Controller}$$

The **Data locality**, the tendency for related data to be stored close together, can significantly reduce access time by minimizing seek and rotation delays. The locality factor is represented by $\alpha$. The adjusted access time considering locality is:

$$T_\text{Access} = (1-\alpha)(T_\text{Seek} + T_\text{Rotation}) + T_\text{Transfer} + T_\text{Controller}$$

To reduce access time the HDDs include buffer memory that exploits spatial locality by storing neighboring sectors.

Writes target cache first, then flush to platters. This reduces repeated disk access for frequently accessed data.

###### Scheduling

When multiple I/O requests are fired, the disk scheduler determines the order of processing. The goal is to minimize total access time and maximize throughput. This introduces a **Scheduling Delay** as the disk may need to wait for the current request to finish before processing the next one. Common scheduling algorithms include:

- **FCFS (First-Come, First-Served)**: Requests are processed in the order they arrive.
- **SSTF (Shortest Seek Time First)**: The request with the shortest seek time is processed next, might lead to starvation.
- **SCAN**: The read/write head moves in one direction, processing requests as it goes, and then reverses direction.
- **C-SCAN**: The read/write head moves in one direction, processing requests as it goes, and then jumps back to the beginning without reversing direction.
- **C-LOOK**: Similar to C-SCAN, but reverse at the last I/O request in one direction instead of reaching the extreme end of the disk.
