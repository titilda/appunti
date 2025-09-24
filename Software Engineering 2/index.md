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
