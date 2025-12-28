---
title: Software Engineering
author:
  - "Andrea Lunghi"
---

## Requirements Engineering (RE)

**Requirements Engineering (RE)** is the process responsible to discover and document the purpose of a software. This is done to avoid misunderstandings and build the right product.

RE is a _iterative_ and _collaborative_ process that needs continuous reviews.

The core activities of RE are:

- Stakeholder identification: Identify Stakeholders (users, customers, etc.) that will use or have interest in the product;
- Requirements elicitation: Discover and extract the needs and constraints from stakeholders;

### Requirements

Requirements can be broadly classified into three main categories.

#### Functional Requirements (FR)

The _functional requirements_ describe the services that the system should provide and the interactions between the system and the environment.

These requirements must be _implementation-independent_, meaning that they should not specify how the functionality will be implemented, but rather what the system should do.

#### Non-Functional Requirements (NFR)

The _non-functional requirements_ describe the quality and how well the system performs its functions. They doesn't describe specific behaviors or functions, but might model constraints on the system.

NFRs can be measured using specific metrics.

- **Performance**: How fast the system responds (time behavior, resource utilization, etc).
- **Usability**: How easy the system is to learn and use.
- **Reliability**: The system's ability to operate without failure (e.g., uptime, fault tolerance).
- **Security**: The system's ability to protect against unauthorized access or data breaches.
- **Maintainability**: How easy it is to modify or update the system.
- **Portability**: The system's ability to run on different platforms or environments.

#### Constraints

The _constraints_ are specific technical or business requirements that limit or restrict the solution.

> Examples of constraints include regulatory compliance, budget limitations, or specific technology choices.

### How to write requirements

Well-written requirements are essential for a successful project. They must be clear, precise, and unambiguous.

- **Single Concern**: Each statement should focus on a single, atomic requirement. Avoid combining multiple ideas into one sentence.
  - Bad: "The system shall allow users to log in and view their profile."
  - Good: "The system shall allow users to log in." and "The system shall allow users to view their profile."
- **Not Ambiguous**: The requirement should not be open to different interpretations. Use specific, technical language and avoid vague terms.
  - Bad: "The system shall be fast."
  - Good: "The system shall respond to user requests within 2 seconds."
- **Testable**: It must be possible to verify whether the requirement has been met. This requires quantifiable metrics.
  - Bad: "The system shall be user-friendly."
  - Good: "The system shall have a user satisfaction rating of at least 85% in user surveys."
- **Achievable**: The requirement must be realistic and within the scope of what the software can accomplish on its own. It shouldn't depend on external, uncontrollable factors.
  - Bad: "The system depends on Adobe Acrobat to function."

### Context

RE is responsible to to define the **phenomena** (the observable events) that are relevant to the project.

- **World**: This is the _real-world environment_ in which the machine operates. It includes events and properties that happen in the environment, not observable by the machine. Some phenomena could _goals_ (G) and _domains properties_ (D);
  - Example: user thoughts, weather conditions, etc.;
- **Machine**: This is the part of the system that is being developed. It's the software and hardware building. It includes events and properties that are observable by the machine;
  - Example: internal states, computations, etc.;
- **Shared Phenomena**: interaction between the world and the machine. Here reside the _requirements_ (R).
  - _Machine controlled_: the machine perform an action that the world can observe (e.g., display a message, interacting with external services);
  - _World controlled_: the world can perform an action that the machine can observe (e.g., user inputs a command).

A requirement is _complete_ iff it satisfy (logically entails) the goal in the context of the domain.

$$\text{R and D} \models G$$

## Alloy

Alloy is a logic-based formal notation used to specify models of a system. It allows developers to describe the structure and behavior of a system and perform automated analysis to check for consistency and correctness.

Alloy uses a **declarative** notation, meaning it describes _what_ the system should do (constraints and relationships) rather than _how_ to do it.

The analysis is performed by the **Alloy Analyzer**, which translates the model into a boolean formula and uses a SAT solver to find instances (examples) or counterexamples.

### Core Concepts

#### Signatures (`sig`)

Signatures define the types of objects (atoms) in the system. They are similar to classes in OOP but represent sets of atoms.

```alloy
sig Name, Addr {}
sig Book {
    addr: Name
}
```

- `abstract sig`: A signature that has no atoms of its own (must be extended).
- `one sig`: A signature that contains exactly one atom (singleton).
- `extends`: Creates a subset of another signature (disjoint by default).

#### Relations and Multiplicity

Fields in signatures define relations. Multiplicity keywords constrain the size of these relations:

- `set`: Any number (default).
- `one`: Exactly one.
- `some`: One or more (at least one).
- `lone`: Zero or one (optional).

#### Facts (`fact`)

Facts are constraints that are assumed to be always true in the model. They restrict the set of possible instances.

```alloy
fact NoSelfReference {
    all n: Node | n !in n.next
}
```

They can also be declared without a name or with the definition of a signature:

```alloy
sig Node {
    next: lone Node
} {
    next != this
}
```

#### Predicates (`pred`)

Predicates are parameterized constraints that can be reused. They are often used to describe operations or state transitions. They are not automatically enforced but can be invoked.

```alloy
pred add [b, b': Book, n: Name, a: Addr] {
    b'.addr = b.addr + (n -> a)
}
```

#### Functions (`fun`)

Functions are expressions that return a value (a set or relation) rather than a boolean.

```alloy
fun lookup [b: Book, n: Name]: set Addr {
    n.(b.addr)
}
```

#### Assertions (`assert`)

Assertions are properties that the system is expected to satisfy. The analyzer checks if these hold given the facts.

```alloy
assert AddIdempotent {
    all b, b': Book, n: Name, a: Addr |
        add[b, b', n, a] implies b'.addr = b.addr + (n -> a)
}
```

### Analysis Commands

- **`run`**: Asks the analyzer to find an _instance_ where a predicate is true. Used for simulation and validation (checking if a scenario is possible).

  ```alloy
  run add for 3 but 1 Book
  ```

- **`check`**: Asks the analyzer to find a _counterexample_ to an assertion. Used for verification.

  ```alloy
  check AddIdempotent for 3
  ```

### Operators

- **Set Operators**: `+` (union), `&` (intersection), `-` (difference), `in` (subset).
- **Relational Join (`.`)**: Navigates relations (similar to dereferencing). `a.r` joins atom `a` with relation `r`.
- **Quantifiers**:
  - `all x: S | ...` (For all)
  - `some x: S | ...` (There exists)
  - `no x: S | ...` (There exists none)
  - `one x: S | ...` (There exists exactly one)
  - `lone x: S | ...` (There exists at most one)

### Temporal Logic (Alloy 6)

Alloy 6 introduces support for **Linear Temporal Logic (LTL)**, allowing the modeling of dynamic systems where state changes over time.

#### Mutable Signatures and Fields (`var`)

To model changing state, signatures and fields can be marked as `var`.

```alloy
var sig State {}
sig System {
    var status: one State
}
```

#### Temporal Operators

These operators are used to express properties over execution traces.

Operators about the future:

- **`always`**: The formula must hold in the current state and all future states.
- **`eventually`**: The formula must hold in the current state or some future state.
- **`after`**: The formula must hold in the next state.

Operators about the past:

- **`historically`**: The formula must have held in all past states.
- **`once`**: The formula must have held in some past state.
- **`before`**: The formula must have held in the previous state.

#### Prime Operator (`'`)

The prime symbol (`'`) is used to refer to the value of a variable in the _next_ state.

```alloy
fact Transition {
    always (x' = x + 1)
}
```

## Software Design

Software Design is the phase where we decide **how** the system will be implemented. It bridges the gap between requirements and code by making high-level decisions about the system's structure.

Design is not about "perfection" but it's a negotiation between multiple tradeoffs (performance, maintainability, scalability, etc).

The workflow is:

```mermaid
stateDiagram
  state HighPhases {
    FeasibilityStudy: Feasibility Study
    RequirementsAnalysis: Requirements Analysis
    ArchitecturalDesign: Architectural Design
  }
  state LowPhases {
    CodingAndUnitTesting: Coding and Unit Testing
    IntegrationAndTesting: Integration and Testing
    Deployment
    Maintenance
  }

  [*] --> HighPhases
  FeasibilityStudy --> RequirementsAnalysis
  RequirementsAnalysis --> ArchitecturalDesign
  HighPhases --> LowPhases
  CodingAndUnitTesting --> IntegrationAndTesting
  IntegrationAndTesting --> Deployment
  Deployment --> Maintenance
```

To reduce the complexity the system is looked at different **views**:

### Module Structure (Static View)

The module structure describe how the system is decomposed into **Implementation Units** (modules, files, packages, libraries, etc) and how they relate to each other.

This view is used to evaluate:

- **Cohesion**: how closely related and focused the responsibilities of a single module are.
- **Coupling**: the degree of dependence between modules. Low coupling is desirable as it reduces the impact of changes in one module on others.
- Planning the implementation phase.

The module structure can be represented with:

- **Package Diagrams**: show the organization of the system into packages and their dependencies.
- **Class Diagrams**: show the classes within each package and their relationships.

### Component-and-Connector (C&C) Structure (Runtime View)

The C&C structure describe how the system behaves at runtime.

The view is separated between:

- **Components**: are the processing elements (modules, services, etc)
- **Connectors**: the mean of communication between components (APIs, message queues, etc).

This view is used to evaluate:

- Performance: identify bottleneck and scalability issues;
- Reliability: identify single point of failure;
- Security: identify access points and vulnerabilities.

The C&C structure can be represented with:

- **Component Diagrams**: show the components and their interactions.
- **Sequence Diagrams**: show the dynamic interactions between components over time.

### Deployment Structure (Physical View)

The deployment structure describe how the system is physically deployed on hardware and network infrastructure.

The components mapped are:

- **Hardware**: physical devices (servers, routers, etc);
- **Execution Environment**: software platforms (OS, containers, VMs, etc);
- **Networking**: network devices and configurations (Firewall, Load Balancer, etc).

This is crucial for non-functional requirements like performance, availability, and security.

The deployment structure can be represented with:

- **Deployment Diagrams**: show the physical nodes and their relationships.
