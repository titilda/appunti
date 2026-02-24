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

