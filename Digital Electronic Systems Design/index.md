---
title: "Digital Electronic Systems Design"
author:
  - "Andrea Oggioni"
---

# Introduction

This document wants to collect in a schematic way all the notions provided during the **Digital Electronic Systems Design** lectures. Those lectures explores how **Programmable Logic Devices** (PLDs) can be used, how their performance can be avaluated w.r.t. multiple metrics and the various tradeoffs between those same metrics. In particular, this document, will cover **Field Programmable Gate Arrays** (FPGAs).

Modern PLDs are the result of a decades-long-evolution. The first examples of PLDs belong to the so-called **paleo electronics** cathegory (not meaning tht they have seen dinosaurs but that they are old and strictly worse compared to modern alternatives). Within this cathegory we can find **GALs**, **PALs**, **PLAs** and much more.

<!-- chip photo -->

We will now go into more details about more modern PLDs:

- **Complex PLD** (CPLD): consists of IO blocks, logic elements and an interconnect. The configuration is stored in some sort of non volatile memory (usually, flash or EEPROM).
- **Field Programmable Gate Arrays** (FPGA): consists of IO blocks, logic elements, an interconnect and, depending on the type, also some advanced blocks like ALUs, **Phase Locked Loops** (PLLs), **Block RAMs** (BRAMs), **Ultra RAMs** (URAMs), **Digital Signal Processors** (DSPs) and much more. The configuration is stored in a volatile memory (no reverse engineering is possible) and can be partially reconfigured at runtime. <!-- PLL photo -->
- **System on chip** (SoC): umbrella term, usually refers to some microprocessors which has an FPGA attached as a peripheral (meaning that it is possible to add arbitrary interfaces to the microprocessors without having to redesign the chip every time). An example od SoC that fully reflect this definition is the Xilinx Zynq chip. Technically speaking, also a Raspberry PI can be considered an SoC since it packs basically the entire processing power into a single chip (ok, the Pi 4 has an external ram chip, but you got the idea).
- **Adaptive Compute Acceleration Platform** (ACAP): another unbrella term, usually referring to SoC with lots of peripherals that use **Network on Chip** (NoC) to communicate. Peripherals are usually some sort of hardware accelerator like AI Engines, Tensor Processing Units, Vector Processing Units and so on.

# FPGA Overview

We have already seen that FPGAs are composed of a multitude of different components:

- IO blocks,
- logic elements,
- interconnect,
- ALUs,
- PLLs,
- BRAMs/URAMs,
- DSPs.

We will now briefly go over each one of them in order to give a complete picture of the main kind of chip we are going to analyze.

The **IO blocks** constitute the interface between the FPGA and the external world. They are a filter that essentially translates between the external and internal logic levels. They can be single-ended or differential, can be configured as input, output or bidirectional and they provide the required pullup/pulldown resistors and tri-state control.

IO blocks may also handle different protocols like PCIe et similia.

The **logic elements** are mainly contituted of **Lookup Tables** (LUTs), that encodes the truth table of some logic function, and **Flip-flops** (FFs) whose job is to store bits of information.

::: {.callout .callout-note title="Latches vs. flip-flops"}
By definition, a **latch** reads from an **enable** signal. When the signal is active, the latch is transparent and the output is a copy of the input; when the enable signal is low, the output is set to the value the inout was at when the enable went low.

A flip-flop, on the other side, is clocked and updated the output only on the specified edges of the clock signal.

<big>_NEVER. EVER. USE. LATCHES._</big>
:::

The **interconnect** (a.k.a. **routing resources**) is literally a configurable crossbar that can connect all the other blocks, nothing more, nothing less. They _route_ the information from one block to another.

::: {.callout .callout-note title="Routing vs NoC"}
The interconnect routes the information one bit at a time around all the blocks and may be slow. NoC can route information on longer distances at a higher rate.

Usually, we can use the interconnect to move information from an FPGA block to the NoC gateway in order to send it somewhere else, like to another FPGA where the information is picked up by its interconnect and sent to the logic blocks.
:::

<!-- TODO: other components -->
<!-- TODO: scaling -->

## Real logic circuits

Digital signals are immune to noise, assuming that the **noise margin** is big enough (almost always true). When an electric signal changes to one logic level to another, it happens that for a really short amount of time, the signal is neither high or low. In such a case, the CMOS logic behaves like a class A amplifier and everything is uncertain.

In a logic circuit, we define the **propagation delay** ($t_p$) as the time it takes for the output to fully reflect a change in the input. We also define the **output contamination delay** ($t_c$) as the time it takes for the output to start mutating in response to an input change. Let the input change at time $t$. Between $t + t_c$ and $t + t_p$ the output may **glitch** and change multiple times.

This is the reason why we should _never_ use latches: if the enable signal glitches (that is almost guranteed), the latch may end up memorizing the wrong value.

The maximum theoretical frequency at which that specific circuit can run is $1 / t_p$.

# Powering an FPGA

An FPGA is powered by multiples power lines:

- **VCCINIT**: powers the internal fabric
- **VCCO**: powers the IO logic; each IO bank may have its own **VCC0#** line with different characteristics
- **VCCAUX**: used as configuration (bitstream source, etc.)
- Other power levels used specified in the datasheed, used to power ADC, RAM, etc.

To provide the correct voltages and power-up sequence, we use a **programmable power supply** (i.e. a microcontroller that controls multiple DC-DC converters). If I wanted to use only DC-DC converters, I can use a chain of DC-DC with an enable signal in order to turn on the next one only when the current has reached the correct output level.

# FPGA fabric

_This chapter applies to "Xilinx Series 7 Artix" FPGAs. Other vendors and/or models may have different terminology and/or specifications._

The programmable fabric of an FPGA is a matrix of different kind of **Configurable Logic Block**s (CLBs). The number of cells in an FPGA chip is called the number of **logic elements**.

Each CLB spans over 2 **slices** (2 SLICEL or 1 SLICEL + 1 SLICEM, we will see later). Each slice contains 4 LUTs (called A, B, C and D), 8 lathes/FFs, a number of multiplexers and some carry logic. Each slice is also connected to a **switch matrix** in order to communicate with the interconnect.

Slices can be **SLICEM** if they can store data in distributed RAM or SLICEL if they support additional logic.

Each LUT has six independent inputs and two independent outputs. This means that ech LUT can implement either

- a single 6-inputs 1-output function;
- two 5-inputs 1-output functions;
- two 3-inputs 2-outputs functions.

Multiple LUTs can be _grouped_ using F7 and F8 multiplexers. In particular, F7A can be used to implement 7-inputs functions using A and B, F7B can do the same tich C and D while F8 combines all four LUTs to implement 8-inputs functions. Bigger functions require multiple slices.

The 8 other multiplexers are used to route information from/to the outside of the slice and between the LUTs, registers and the carry logic in the slice itself.

The storage elements are 4 FFs and 4 other FFs that can also act as latches. FFs are edge triggered while latches are level-sensitive.

SLICEM LUts are used to also implement distributed ram to store "large" amount of data. The same LUTs may also be configured to act as shift registers.

The carry logic is composed by CARRY4 components that are cascadingly placed in upward vertical fashion and it is used mainly for carry propagation.

In addiotion to all of that, there are also colums of **Block RAM** (BRAM), connected in the same cascading upward vertical fashion. Each one of those is composed of two independend 18kb controllers. BRAM can be used to implmenent "big" memory without constraining timing penalties.

## IO resources

An FPGA is connected through the ret of the circuit through **IO pins**, which are grouped in **IO banks**. Banks are mainly one of two cathegories: **High Range** (HR), which support a wide range of voltages, and **High Performance** (HP) wich possess better electrical characteristics than HR.

Each IO pin can be conficured to act as DDR, SERDES, etc. and can have a delay line.

Each IO bank is powered by a VCCO (to power the output pins), a VREF (for the differential signals) and a VCCAUX (to power the auxiliary IO logic).

IO ping can be configured to work as **single ended** (one line per signal + one common ground, no static power, no resistance to noise, slower) or **differential** (two lines per signal + one common ref, static power consumption, strong resistance against noise, faster). Single ended standards include LVCMOS, LVTTL, HSTL, PCI and SSTL while differential standards include LVDS, Mini_LVDS, RSDS, PPDS, BLVDS, dHSTL, dSSTL and PCIe.

Single ended IO are accessed using IBUFs and OBUFS while differential ones are accessed using IBUFDSs and OBUFDSs.

The IO pins can be configured for the impedance matching in order to reduce reflection.

The IO logic is comprised of DDR/edge triggered FFs, IDELAYs, ODELAYs, ISERDES and OSERDES.

## Clock resources

All the clocked components inside an FPGA should be synchronized, otherwise undefined behavior may emerge. Signal **jitter** and **skew** must be minimized.

Each FPGA chip includes multiple **Clock Management Tiles** (CMTs), that provide the clock generation and deskewing and jitter filtering functionalities, and the **clock routing resources**, that are used to propagate the clock signal with as less skew and jitter as possible.

Each CMT comprises a single **Phase Locked Loop** (PLL) and a single **Mixed-Mode Clock Manager** (MMCM).

The FPGA fabric is split in different **clock regions** (spanning from the left to the right of the chip and 50 CLBs tall).

Global clock lines can clock and provide control to all the resources on an FPGA chip.

There exists multiple buffers that can be used to access different types of clock:

- **BUFG**s are used to access global clock lines;
- **BUFH**s are used to access "horizontally" local clock lines;
- **BUFR**s are used to access "regionally" local clock lines;

It is possible to provide external clock signals to the FPGA connecting them to specific pins that can route said signal into the clock trees.