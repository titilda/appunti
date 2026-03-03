---
title: "Advanced Computer Architecture"
author:
  - "Andrea Lunghi"
---


Computer architecture is fundamentally about **trade-offs**: every design decision involves balancing competing goals.

## Performance Metrics

Performance cannot be reduced to a single number. Instead, different metrics capture different aspects:

**Time-Based Metrics:**

- **Latency**: Time for a single operation to complete (relevant for interactive applications)
- **Throughput**: Operations completed per unit time (relevant for batch processing and servers)

**Frequency and Instruction Execution:**

- **CPI (Cycles Per Instruction)**: The average number of clock cycles required to execute one instruction. This directly reflects how efficiently the architecture executes code.
- **Clock Speed**: The frequency at which the CPU operates, measured in GHz. Raw frequency says little about real-world performance.

$$\text{Performance} = 1 / (\text{Clock Speed} × \text{CPI})$$

**Throughput-Oriented Metrics:**

- **Throughput**: Instructions executed per unit time
- **MIPS (Million Instructions Per Second)**: Useful for comparing general-purpose performance
- **MFLOPS (Million Floating Point Operations Per Second)**: Useful for scientific and graphics workloads

**Power Consumption:**

Energy efficiency is critical because:

- Heat dissipation limits clock speed
- Battery life constrains mobile devices
- Data center operational costs are dominated by power

### Comparing Architectures

Two architectures can be compared using two main approaches:

#### Speedup

The **speedup** of a new architecture compared to a baseline is calculated as:

$$\text{Speedup} = \frac{\text{Execution Time}_{\text{baseline}}}{\text{Execution Time}_{\text{new}}} = 1 + \frac{n}{100}$$

If Architecture A is 20% faster than B: $\text{Speedup} = 1.2$

#### Testing

An architecture can be tested using two type of programs:

- **Benchmark**: Standardized, reproducible test suite (e.g., SPEC CPU)
- **Workload**: Real-world application mix representative of actual use

### Design Trade-offs by Use Case

Different devices prioritize different metrics:

| Use Case | Goal | Constraint |
| ---------- | ------------- | ------------ |
| Mobile | Power efficiency, Latency (responsiveness) | Heat dissipation |
| Desktop | Cost-performance ratio | Reasonable power |
| Server | Throughput, Reliability | Power density |
| Embedded | Latency, Power | Size/cost |

### Heterogeneous Architectures

Modern systems often combine multiple types of processors, each optimized for different workloads:

- **CPU**: General-purpose, low latency
- **GPU**: High throughput for parallel tasks (graphics, ML)
- **TPU**: Specialized for tensor operations in ML
- **FPGA**: Reconfigurable hardware for custom workloads
