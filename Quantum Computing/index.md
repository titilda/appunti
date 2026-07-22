---
title: "Quantum Computing"
author:
  - "Andrea Oggioni"
---

# Basics

In order to fully understand this document, the basics of quantum physics and dirac notation are required. A really quick overview is given. See [Quantum Physics](/Quantum%20Physics/index.html) for more details.

The **quantum state** of a **qubit** is a unitary vector denoted with $|v\rangle$.

A single qubit state can be expressed as a linear combination of an orthonormal basis, e.g.

$$
|v\rangle = a |0\rangle + b |1\rangle \mapsto \begin{pmatrix} a \\ b \end{pmatrix}
$$

where $\{ |0\rangle, |1\rangle \}$ is called the **standard basis**:

$$
|0\rangle = \begin{pmatrix} 1 \\ 0 \end{pmatrix} \in \mathbb{C}^2 \qquad |1\rangle = \begin{pmatrix} 1 \\ 0 \end{pmatrix} \in \mathbb{C}^2
$$

$a$ and $b$ are called **amplitudes** while their quare are called **probabilities**.

Another useful basys is the **Hadamard basis**:

$$
|+\rangle = \frac{1}{\sqrt{2}}\begin{pmatrix} 1 \\ 1 \end{pmatrix} \in \mathbb{C}^2 \qquad |-\rangle = \frac{1}{\sqrt{2}}\begin{pmatrix} 1 \\ -1 \end{pmatrix} \in \mathbb{C}^2
$$

A qubit-measurment device always returns the measurement result in it's own basis. After the measurement has been performed, the qubit state collapse to the measured state.

This means that quantum mechanics allows us to perform fun party tricks like [this one](https://chem.libretexts.org/Bookshelves/Physical_and_Theoretical_Chemistry_Textbook_Maps/Quantum_Tutorials_(Rioux)/07%3A_Quantum_Optics/7.13%3A_The_Three-Polarizer_Paradox).

As we saw earlier, a qubit can be described with two complex numbers (four different real numbers): with this property, it is possible to encode lots of information (a.k.a. **prepare the state**) into a qubit amplitudes. The problem lies in the fact that it is impossible to measure them and that we cannot clone a qubit.

Two qubits that only differs by a **global phase factor** carry the same information.

The **Bloch sphere** is the space where all possible qubits lies.

![By <a href="//commons.wikimedia.org/w/index.php?title=User:Notezik&amp;action=edit&amp;redlink=1" class="new" title="User:Notezik (page does not exist)">Notezik</a> - <span class="int-own-work" lang="en">Own work</span>, <a href="http://creativecommons.org/publicdomain/zero/1.0/deed.en" title="Creative Commons Zero, Public Domain Dedication">CC0</a>, <a href="https://commons.wikimedia.org/w/index.php?curid=179712998">Link</a>](assets/bloch.png)

On the Bloch sphere, two orthogonal qubits are aligned on the same direction with opposite orientations. On the $z$ axis there is the standard basis, on the $x$ axis there is the Hadamard basis and on the $y$ axis there is the $\{ |i\rangle, |-i\rangle }$ basis.

In **spherical coordinates**, a qubit can be rewritten as

$$
|v\rangle = \cos \left( \frac{\theta}{2} \right) |0\rangle + e^{i \varphi} \sin \left( \frac{\theta}{2} \right) |1\rangle
$$

# Qubit operations

State preserved by qubits can be modified with **gates**. All operations performed by gates are reversible and deterministic (except for the measurements, that are neither reversible nor deterministic).

Gates may operate either on a single qubit or on multiple ones.

Gates can be represented as matrices that left-multiplies the qubit state.

If $|v_{in}\rangle$ gets transformed into $|v_{out}\rangle$ by going through gates $G_1, G_2, \dots, G_n$, then the same exact result can be achieved by using a single gate $\hat G$ defined as

$$
\hat G = \prod_{i=n}^1 G_i
$$

## Single qubit gates

Single qubit gates, as the name implies, work on a single qubit.

::: {.callout .callout-note title="Note"}
In order to create superposition from base states, all elements of a gate must be non null.
:::

### Identity gate

The **identity gate** is the `NOP` of quantum computers: it may look useless but will become useful when building multi-qubit gates. It is represented by

$$
I = \begin{bmatrix}
  1 & 0 \\ 0 & 1
\end{bmatrix}
$$

### Pauli-X gate

The **Pauli-X gate** (a.k.a. **NOT gate**) performs a rotation around the $x$ axis by $\pi$ radians. Practically, the amplitudes of the qubit are flipped. It is represented by

$$
X = \begin{bmatrix}
  0 & 1 \\ 1 & 0
\end{bmatrix}
$$

### Pauli-Z gate

The **Pauli-Z gate** (a.k.a. **phase flip gate**) performs a rotation around the $z$ axisby $\pi$ radians. Practically, the phase $\varphi$ of the qubit is flipped. It is represented by

$$
Z = \begin{bmatrix}
  1 & 0 \\ 0 & -1
\end{bmatrix}
$$

### Pauli-Y gate

The **Pauli-Y gate** performs a rotation around the $y$ axis by $\pi$ radians. It is represented by

$$
Y = \begin{bmatrix}
  0 & -i \\ i & 0
\end{bmatrix}
$$

### Phase gate

The **phase gate** performs a rotation around the $z$ axis by $\pi / 2$ radians. It is represented by

$$
Y = \begin{bmatrix}
  1 & 0 \\ 0 & i
\end{bmatrix}
$$

### Hadamard gate

The **Hadamard gate** performs a rotation around the $y$ axis by $\pi / 2$ radians followed by a rotation around the $x$ axis by $\pi$ radians. It maps $|0\rangle \mapsto |+\rangle$ and $|1\rangle \mapsto |-\rangle$. It is represented by

$$
H = \frac{1}{\sqrt{2}}\begin{bmatrix}
  1 & 1 \\ 1 & -1
\end{bmatrix}
$$

## Eigenvectors of single qubit gates.

The eigenvectors of a gate are always aligned (in the Bloch sphere) to the direction around which the rotation is applied by the same gate.

_TODO_

## Generic rotation gates

_TODO_

## Qubit measurement

_TODO_

## Multiple qubits

The state describing a system composed by multiple qubits can be obtained by performing the tensor product of all the qubits' states. The amplitudes associated to each base system state are associated to the probabilities of an hypotetical join mesurement of the single quibits' states. In general, the state of an $n$-qubits system is a $2^n$-dimensional complex vector.

Assume that two distinct states go through two distinct gates. The same result can be achieved by running the tensor product of the states through the tensor product of the gates.

### CNOT gate

The **CNOT** gate acts like a controlled negation on a quibit based on the value of another qubit. It is represented by

$$
CNOT = \begin{bmatrix}
  1 & 0 & 0 & 0 \\
  0 & 1 & 0 & 0 \\
  0 & 0 & 0 & 1 \\
  0 & 0 & 1 & 0
\end{bmatrix}
$$

### Generic controlled gate

More in general, a generic controlled gate $C_U$ can be represented by the matrix

$$
C_U = \begin{bmatrix}
  I & 0 \\ 0 & U
\end{bmatrix}
$$

where $I$ is the identity matrix and $U$ is the gate we want to conditionally apply.

### SWAP gate

The swap gate swaps the amplitudes of the two qubits, in the sense that terms like $a_xb_y$ becames $a_yb_x$. It is represented by

$$
SWAP = \begin{bmatrix}
  1 & 0 & 0 & 0 \\
  0 & 0 & 1 & 0 \\
  0 & 1 & 0 & 0 \\
  0 & 0 & 0 & 1
\end{bmatrix}
$$

### CCNOT gate

The CCNOT gate, also known as the Toffoli gate, is a cnot gate that flips the target qubit only when the two control qubit are high. It is represented by

$$
CCNOT = \begin{bmatrix}
  1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 \\
  0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 \\
  0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 \\
  0 & 0 & 0 & 1 & 0 & 0 & 0 & 0 \\
  0 & 0 & 0 & 0 & 1 & 0 & 0 & 0 \\
  0 & 0 & 0 & 0 & 0 & 1 & 0 & 0 \\
  0 & 0 & 0 & 0 & 0 & 0 & 0 & 1 \\
  0 & 0 & 0 & 0 & 0 & 0 & 1 & 0
\end{bmatrix}
$$

## Universal quantum gates

A set of quantum gates is said to be **universal** if an arbitrarily complex quantum circuit can be decomposed in gates belonging to that set. The set of two-qubits gates is universal, the Toffoli + Hadamard set is also universal and so is the set composed by $R_x, R_y, R_z, CNOT$.

## Entanglement

A state is said to be **entangled** if it cannot be written as the tensor product of two or more other states (opposite of **separable states**). The number of possible separable states is negligible compared to the total number of possible quantum states.

An example of entangled states are the **Bell states** $\Phi^+, \Phi^-, \Psi^+, \Psi*-$ where

$$
|\Phi^+\rangle = \frac{|00\rangle + |11\rangle}{\sqrt{2}} \qquad |\Phi^-\rangle = \frac{|00\rangle - |11\rangle}{\sqrt{2}} \\
|\Psi^+\rangle = \frac{|01\rangle + |10\rangle}{\sqrt{2}} \qquad |\Psi^-\rangle = \frac{|01\rangle - |10\rangle}{\sqrt{2}} 
$$

While superposition is basis dependent, entanglement is NOT.

Measurement brings the quantum state back to a separable state, therefore measurements destroy entanglement. This means that a measurement may affect more than one qubit.

_To be continued_