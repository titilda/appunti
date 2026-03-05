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

An architecture can be tested using two types of programs:

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

## Processor Architecture and Execution

### Instruction Set Architecture (ISA)

The ISA defines the set of instructions that a processor can execute, along with the data types, registers, and memory addressing modes. It serves as the interface between software and hardware.

The most common ISAs include:

- **MIPS**: A simple, RISC architecture used in education and embedded systems
- **x86**: A complex, CISC architecture dominant in desktops and servers
- **ARM**: A RISC architecture widely used in mobile and embedded devices
- **RISC-V**: An open-source ISA gaining popularity for research and custom designs

### Computing Infrastructure

A cpu is composed of two main components:

- **Control Unit**: Decodes instructions and generates control signals to orchestrate instruction execution
- **Data Path**: Contains the registers, ALU, and memory interfaces that perform actual data processing

The **main memory** stores both instructions and data that the CPU needs to access during execution.

The CPU and main memory communicate via three key buses:

- **Address Bus**: Carries memory addresses from the CPU to memory
- **Data Bus**: Carries data between the CPU, memory, and registers
- **Control Bus**: Carries control signals from the control unit to coordinate all components

#### Instruction Stages

The execution of an instruction can be broken down into several stages:

1. **Fetch (F)**: Load instruction from memory into the Instruction Register (IR)
2. **Decode (D)**: Interpret the instruction — determine operation and identify operands
3. **Execute (E)**: Perform the operation (ALU computation, address calculation, etc.)
4. **Memory Access (M)**: Access memory if the instruction requires it (load/store)
5. **Write-back (W)**: Store the result back to the registers

#### Instructions

The ISA defines a set of instructions that the processor can execute, such as:

| Instruction Type | IF (2 ns) | ID (1 ns) | EX (2 ns) | MEM (2 ns) | WB (1 ns) | Total Time |
| ---------------- | -- | -- | -- | --- | -- | -- |
| ALU | fetch instruction from memory & increase PC to next instruction | read source registers (rs1, rs2) | perform the operation in the ALU | | write to the destination register (rd) | 6 ns |
| Memory Load | fetch instruction from memory & increase PC to next instruction | read base register (rs1) | compute effective address | read memory | load data into destination register (rd) | 8 ns |
| Memory Store | fetch instruction from memory & increase PC to next instruction | read base register (rs1) and source register (rs2) | compute effective address | write memory | | 7 ns |
| Conditional Jump | fetch instruction from memory & increase PC to next instruction | read source registers (rs1, rs2) | compare rs1 and rs2 & compute target address | Update PC | | 5 ns |

Each instruction type requires different execution time depending on the stages it uses.

**Single-cycle architecture**: All instructions complete in one cycle, but the cycle time is constrained by the longest instruction (8 ns in the example above).

**Multi-cycle architecture**: By dividing execution into separate stages with individual latches between them, each stage operates on a shorter cycle time (2 ns).

### Pipelining

**Pipelining** is a technique that overlaps the execution stages of multiple instructions, enabling them to be processed simultaneously. While a single instruction takes longer to complete, the overall system throughput improves dramatically: instead of issuing one instruction every 8ns, the pipeline issues one instruction every 2ns once it reaches steady state.

Latches between each pipeline stage store intermediate results and control signals. As one instruction moves to the next stage, the following instruction enters the current stage.

#### Hazards

Hazards are situations where the next instruction has a dependency on the previous instruction that has not yet completed.

There are three main types of hazards:

##### Structural Hazards

**Structural Hazards** occur when the hardware resources required by an instruction are not available at the time it needs them. This can happen when multiple instructions try to use the same resource simultaneously.

For example there is a WB and a ID stage where both need to access the register file, if the register file can only handle one access at a time, this creates a structural hazard.

This is solved by splitting the write and read between two different stages within the same cycle, so that the register file can handle both accesses without conflict. (ID happens during rising edge, WB happens during falling edge)

##### Data Hazards

**Data Hazards** occur when an instruction depends on data from a previous instruction that has not yet been written to the register file. This is the most critical hazard type because it can cause incorrect computation. There are three subtypes:

- **Read After Write (RAW)**: An instruction reads a register before a previous instruction has written to it. Example: Instr1 writes R1, Instr2 reads R1. This is the _only true data hazard_ that causes incorrect results if unhandled.
- **Write After Read (WAR)**: An instruction writes to a register before a previous instruction has finished reading from it. Example: Instr1 reads R1, Instr2 writes R1.
- **Write After Write (WAW)**: Two instructions write to the same register, and their order matters. Example: Instr1 writes R1, Instr2 writes R1.

**Solutions for RAW hazards**:

1. **Stalling** (inserting bubbles): The pipeline inserts **NOP** (no operation) instructions to delay dependent instructions until the result is available. This is simple but wasteful.
2. **Compiler rescheduling**: The compiler reorders instructions to place independent instructions between dependent ones, keeping the pipeline busy. Example: Load R1, [independent instruction], Use R1.
3. **Forwarding**: An hardware optimization that passes results directly from one pipeline stage to another, bypassing the register file. Common forwarding paths:
    - **EX→EX**: Result from Execute stage forwarded to the next instruction's Execute stage
    - **MEM→EX**: Result from Memory stage forwarded to the next instruction's Execute stage  
    - **MEM→ID**: Result from Memory stage forwarded to the next instruction's Decode stage

Forwarding eliminates many data hazards without stalling, but some load instructions still require one stall.

##### Control Hazards

**Control Hazards** occur when a branch instruction reaches the Execute stage, but the pipeline has already fetched instructions from the potentially wrong path. The CPU doesn't know the branch outcome until the condition is evaluated, so intervening instructions may need to be discarded.
