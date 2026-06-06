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

## Cryptography

Cryptography comprises techniques to enable secure communication and storage in the presence of attackers.

### Objectives

A cryptographic system must provide:

- **Confidentiality**: Only authorized entities can access information
- **Integrity**: Detect and prevent unauthorized modification of information
- **Authentication**: Verify the identity of entities
- **Non-repudiation**: Prevent entities from denying their actions
- **Proof of Knowledge**: Allow one entity to prove to another that it knows a secret without revealing it
- **Proof of Computation**: Allow one entity to prove to another that it performed a computation without revealing details

### Cipher Fundamentals

Encryption transforms plaintext into ciphertext using an algorithm (public) and a key (secret). Decryption is the process that reverses this transformation.

#### Mathematical Definition

- **Plaintext space** $P$: set of all possible messages of length $n$
- **Ciphertext space** $C$: set of all possible encrypted messages of length $m$ (where $m \geq n$)
- **Key space** $K$: set of all possible keys of length $\lambda$
- **Encryption function** $\mathbb{E}: P \times K \rightarrow C$ produces ciphertext from plaintext and key
- **Decryption function** $\mathbb{D}: C \times K \rightarrow P$ recovers plaintext from ciphertext and key

**Correctness property**: $\mathbb{D}(\mathbb{E}(p, k_e), k_d) = p$ for all $p \in P$, $k_e \in K$, $k_d \in K$ (decryption key may differ from encryption key)

#### Properties of Good Ciphers

From both usability and security perspectives, a cipher should:

- Be practically impossible to break (if not mathematically)
- Remain secure even if the algorithm is public
- Use keys that are easily communicable and changeable without written records
- Be applicable to communication scenarios
- Be portable and operable by individuals
- Be easy to use and understand without excessive mental effort

Randomness is critical for secure encryption:

- Should be inherent in the key generation process
- Must produce uniform distribution of outputs
- Essential for preventing patterns in ciphertexts

#### Attack Models

To provide confidentiality, systems must resist various threat levels:

- **Ciphertext-only attack**: Attacker has access only to ciphertexts and attempts to deduce plaintext or key
- **Known-plaintext attack**: Attacker knows some plaintext-ciphertext pairs but cannot choose them
- **Chosen-plaintext attack**: Attacker can select arbitrary plaintexts and obtain corresponding ciphertexts, attempting to deduce the key
- **Active attacks**: Attacker can modify ciphertexts or inject new ones

#### Perfect Ciphers

A cipher is **perfect** if ciphertext provides no information about plaintext:

$$P(p|c) = P(p)$$

**Shannon's Theorem**: A cipher is perfect if and only if:

- Key space is at least as large as plaintext space
- Keys are used uniformly at random
- Each key is used only once (never reused)

This would requires managing truly random keys as long as messages, used only once—infeasible at scale.

> **Example**: One-Time Pad, performing a XOR operation between plaintext and a random key of equal length, is a perfect cipher.

#### Computational Security

In practice, perfect ciphers are replaced by **computationally secure ciphers**, which:

- Are designed to resist all known attacks
- Are easy to decrypt with the correct key
- Brute-force is the best known attack, and it is computationally infeasible to break the cipher within a reasonable time frame

**Nash's Theorem**: A cipher is secure if the cost of breaking it exceeds the value of the protected information.

Computationally secure ciphers rely on the hardness of certain mathematical problems, that are easy to compute in one direction but hard to reverse without specific information:

- **Factoring Large Integers**: Computing $n = p \times q$ is easy; reversing (finding $p$ and $q$ given $n$) is computationally hard
- **Discrete Logarithm Problem**: Computing $g^x = y$ is easy; reversing (finding $x$ given $g$ and $y$) is computationally hard

Security is proven by showing that breaking the cipher would solve a known hard problem, believed infeasible with current technology.

### Symmetric Encryption

In symmetric encryption, the same key is used for both encryption and decryption. Both parties must have access to this secret key, creating challenges for key distribution:

- **Distribution requirement**: Key must be shared through a trusted channel—secure not against all attackers, but specifically against the threat agents present.
- **Scalability limitation**: For $n$ users, the number of required keys grows quadratically: $\frac{n(n-1)}{2}$ keys total.

Some examples of symmetric ciphers include:

- **Substitution ciphers**: Replace each plaintext element with another (e.g., Caesar cipher)
- **Transposition ciphers**: Rearrange plaintext elements (e.g., rail fence cipher)

**AES (Advanced Encryption Standard)** is the current standard, using 128-bit blocks with key sizes of 128, 192, or 256 bits. Needs $2^{128}$ operations to brute-force a 128-bit key.

#### Pseudorandom Number Generators (CSPRNGs)

A **Cryptographically Secure Pseudorandom Number Generator** is a deterministic function $G: \{0,1\}^\lambda \rightarrow \{0,1\}^{\lambda + l}$ whose output is indistinguishable from random by any efficient (polynomial-time) algorithm.

##### Stream Ciphers

**Stream ciphers** generate a pseudorandom keystream that is XORed with the plaintext to produce ciphertext.

They are efficient for encrypting data of arbitrary length but require careful management of keys and initialization vectors (IVs) to prevent vulnerabilities.

##### Block Ciphers

**Pseudo-Random Permutations (PRP)** are a type of function that is bijective, meaning each input maps to a unique output and vice versa. The function is identified by a key, and for each key, it behaves like a random permutation over the input space.

The simplest block cipher is **Electronic Codebook (ECB)**, which encrypts each block of plaintext independently. However, it is insecure because identical plaintext blocks produce identical ciphertext blocks, revealing patterns in the data.
A better approach is to use **Counter (CTR) Mode**, which generates a keystream of the length of the plaintext by encrypting a counter for each block and XORing it with the plaintext. The problem with CTR mode is that the counter is predictable.

```mermaid
graph TD
    A[Counter] --> B[Encryption Function]
    C[key] --> B
    B --> D[Keystream]
    E[Plaintext block] --> F[XOR]
    D --> F
    F --> G[Ciphertext block]
```

To prevent attacks when reusing keys:

- **Nonce (Number Used Once)**: Use a random value unique for each encryption, based on the ciphertext, making the starting point of the counter unpredictable. Allows the same key for multiple encryptions without producing identical ciphertexts for identical plaintexts.
- **Rekeying**: Generate a new key for each block by encrypting a random value with the original key. Uses symmetric ratcheting: split the key into two parts—one encrypts the block, the other generates the next key.

```mermaid
graph TD
    A[Random Value] --> B[Encryption Function]
    C[Seed] --> B
    B --> D[Block Key]
    B --> E[Next Seed]
```

---

### Integrity and Authenticity

A cipher is **malleable** if an attacker can modify the ciphertext to produce predictable changes in the decrypted plaintext without knowing the key. This enables:

- Data manipulation attacks
- Homomorphic encryption (computation on encrypted data with results matching operations on plaintext)

#### Message Authentication Codes (MAC)

The **Message Authentication Code (MAC)** is a short tag generated from a message and a secret key, that is attached to the message. It allows the receiver to verify both the integrity and authenticity of the message.

- `compute_tag(message, key)`: Produces a tag for integrity verification
- `verify_tag(message, tag, key)`: Returns true if the tag is valid for the message and key

As both sender and receiver share the same key, MACs do not provide non-repudiation (both parties can generate valid tags).

It is implemented using **CBC-MAC** (Cipher Block Chaining Message Authentication Code) that encrypt a block with the key, XOR the output with the next block, and use the final output as the tag.

#### Hash Functions

Hash functions map messages to fixed-size digests unique to the input. They are faster than MAC and provide integrity checks.

A secure hash function must resist:

- **Preimage attack**: Given hash value $h$, it should be infeasible to find input $m$ where $\text{hash}(m) = h$
- **Second preimage attack**: Given input $m$ and its hash, it should be infeasible to find different $m'$ where $\text{hash}(m') = \text{hash}(m)$
- **Collision attack**: It should be infeasible to find any two different inputs $m_1 \neq m_2$ where $\text{hash}(m_1) = \text{hash}(m_2)$

**Brute-force resistance**: Secure functions should not be breakable faster than brute-force ($2^{n-1}$ for preimage, $2^{n/2}$ for collisions).

**SHA-2 and SHA-3** are the current standards, producing hash values of 256, 384, or 512 bits.

#### HMAC (Keyed Hash)

Combines hash functions with a secret key by including the key as part of the hash input.

This provides integrity and authenticity, but not non-repudiation as the parties with the key can generate valid HMACs.

### Asymmetric Encryption

Symmetric encryption alone cannot provide:

- Authentication (sender cannot be verified)
- Key agreement over public channels
- Confidential communication without pre-shared secrets

This is done through **asymmetric encryption**, which uses a pair of keys: a public key (freely distributable) and a private key (kept secret).

The key generation uses one-way functions with trapdoors, where the public key can be easily derived from the private key, but the private key cannot be feasibly derived from the public key without specific information (the trapdoor).

The double keys allow for two main use cases:

- **Public-key encryption**: Sender encrypts with recipient's public key; only recipient (with private key) can decrypt. Provides confidentiality.
- **Digital signatures**: Sender signs with private key; recipient verifies with sender's public key. Provides authentication and non-repudiation.

The most common algorithm is **RSA**, based on the difficulty of factoring large integers. Another common algorithm is **Elgamal**, based on the discrete logarithm problem.

The problem with asymmetric encryption is that it is computationally expensive and requires larger key sizes for equivalent security compared to symmetric encryption (2048-bit RSA ≈ 256-bit AES). Therefore, it is often used in combination with symmetric encryption in a **hybrid approach**.

#### Hybrid Approach

Hybrid approach uses asymmetric encryption for secure key exchange and symmetric encryption for the actual message:

1. Generate a symmetric key for the actual message
2. Encrypt the message with symmetric key
3. Encrypt the symmetric key with recipient's public key
4. Send both encrypted message and encrypted key

##### Diffie-Hellman Key Exchange

Allows two parties to establish a shared secret over an insecure channel without pre-shared private keys.

1. Define a finite cyclic group $(G, \cdot)$, generator $g$, and numbers $a, b$ (where $\lambda = \text{len}(a) \sim \log_2(|G|)$)
2. Alice computes $A = g^a$ and sends to Bob
3. Bob computes $B = g^b$ and sends to Alice
4. Alice computes shared secret: $s = B^a = g^{ab}$
5. Bob computes shared secret: $s = A^b = g^{ab}$

This is resistant to passive eavesdropping (attacker would need to solve the discrete logarithm problem).

#### Digital Signatures

To provide both authentication and integrity:

1. Hash the message
2. Encrypt the hash with the sender's private key
3. Recipient decrypts with sender's public key and compares to hash of received message

#### Public Key Infrastructure (PKI)

To authenticate public keys, PKI uses a hierarchical trust model where trusted **Certificate Authorities** (CAs) issue digital certificates that bind public keys to identities.

The CA signs a certificate containing the sender's public key and identity information with its private key. Recipients can verify the certificate's authenticity using the CA's public key, which must be trusted.

Certificate revocation can be managed through:

- **Certificate Revocation List (CRL)**: Published list of revoked certificates; recipients check against when verifying
- **Online Certificate Status Protocol (OCSP)**: Real-time protocol to query CA for certificate status—more up-to-date than CRL

### Information Theory and Entropy

Acquiring information reduces uncertainty about a message. A message source can be modeled as a random variable. Greater variance means higher uncertainty and more information gain.

#### Shannon Entropy

The **entropy** measures the uncertainty or randomness of a random variable.

Defined as:

$$H(X) = -\sum_{x \in X} P(x) \log_2 P(x)$$

where $P(x)$ is the probability of random variable $X$ taking value $x$.

**Properties**:

- Measured in bits
- Higher entropy = more unpredictable (more information)
- Lower entropy = more predictable (less information)
- Example: Constant message has $H(X) = 0$; completely random message has high $H(X)$

A message's outcome can be encoded using approximately $H(X)$ bits, the minimum bits needed to represent information without loss.

#### Min-Entropy

The **Min-Entropy** Represents the difficulty of guessing the most likely outcome of a random variable. It is defined as:

$$H_{\infty}(X) = -\log_2 \max_{x \in X} P(x)$$
