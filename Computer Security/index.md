---
title: "Computer Security"
author:
  - "Andrea Lunghi"
---

## Introduction

A **secure system** defends against external threats, while a **safe system** does not cause harm.

### CIA Paradigm

A secure system must satisfy the **CIA paradigm**:

- **Confidentiality**: Only authorized entities can access information
- **Integrity**: Information can be modified only by authorized entities that are entitled to do so
- **Availability**: Information must be accessible to authorized entities with proper access rights

> Confidentiality and Integrity are in conflict with Availability. Security requires finding appropriate tradeoffs between these pillars.

### Risk Assessment Components

To assess risk, it's important to understand the following components:

- **Vulnerability**: A weakness that allows violation of at least one CIA constraint
- **Exploit**: A specific technique that uses one or more vulnerabilities to accomplish an objective that violates CIA constraints

> An exploit implies a vulnerability exists, but a vulnerability can exist without an available exploit.

- **Assets**: Resources valuable to the organization (hardware, software, data, reputation)
- **Threat**: Any potential violation of CIA constraints (different from an exploit, which is a specific technique to accomplish a violation)
- **Attack**: Intentional use of an exploit to deliberately violate CIA constraints
- **Threat Agent**: Any entity that could potentially become an attacker
- **Attacker**: An entity that performs an attack
  - *Black hats*: Malicious attackers
  - *White hats*: Ethical security professionals

### Security Levels

- **Security Level**: The degree of security appropriate to the threats facing the system
- **Protection Level**: The degree of security actually implemented through countermeasures

### Risk

Risk is a statistical and economic evaluation of exposure to damage due to the presence of vulnerabilities and threats:

$$\text{Risk} = \underbrace{\text{Asset} \times \text{Vulnerability}}_{\text{controllable factors}} \times \underbrace{\text{Threats}}_{\text{independent factors}}$$

Key observations:

- Threats cannot be controlled as they are external; they must be evaluated and monitored
- Asset value cannot be reduced without losing organizational value
- Only vulnerabilities are directly controllable

### Security Strategy

Security focuses on reducing vulnerabilities and containing damage at acceptable costs (involving tradeoffs between security and usability/performance).

- **Direct costs**: Management, operations, equipment (relatively easy to estimate)
- **Indirect costs**: Reduced usability, performance, privacy, or productivity impacts
