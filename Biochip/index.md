---
title: "Biochip"
author:
    - "Niccolò Papini"
---
## Introduction

Hello reader this is a short summary of our notes about the course "Biochip". If you find an error or you think that one point isn't clear please tell me and I fix it (sorry for my bad english). -NP

## Chapter Zero: Bases

**Definition of Biosensor:** A (chemical) sensor based on a biological entity.

Biosensor work with an Analyte and Receptors:

- Analyte: target molecule;
- Receptors: Receive the target and bind with it.

Analyte $\xrightarrow{bind}$ Receptor $\xrightarrow{change}$ Electronic transformation

**Planar configuration:** Receptor is immobilized on the sensor.

**Technological issue:** how to attach the molecule to a solid substrate while presenting its function (in a liquid environment) $\implies$ chemical functionalization.

**Geometrical Parameters (for simplify analysis):**

- Contour Length $(L):$ length of the macro molecule backbone.
- Radius of gyration $(R_g):$ for a folder, globular macromolecule is the average between the extremes.

Motion of fluids $\to$ Navier-Stokes

We work with fluids + particles and simple fluids.

## Chapter One: Fluids Law

Main properties of fluid:

- Density $(\rho)$
- Viscosity $(\eta)$
- Surface tension $(\gamma)$

Simple fluids:

- Newtonian
- Non-Newtonian

Complex fluids:

- Electrolytes: simple fluids + ions in solution
- Suspensions: simple fluids + large particles
- Complex fluids: fluids + heterogeneity change the properties

### 1.1 Properties

Density: $\rho = \frac{\text{Mass}}{\text{Volume}}[\frac{kg}{m^3}] \implies$ important to set flowing and sedimentation time of particles $\to$ decrease with temperature (higher is the temperature lowest is the density)

Viscosity: $\eta = \frac{F}{A} * \frac{y}{u} [P_a * s]$ but $\frac{F}{A}$ is called **shear stress** $\tau \to \eta = \tau * \frac{y}{u} \implies$ Express the resistance to deformation by $\tau$

- $F:$ force applied to win the resistance and to maintain a constant velocity $u$ of top plate (Area $A$)
- $u:$ velocity

Viscosity increase with the temperature (low temperature, high viscosity)

![](assets/chapter_one/viscosity.jpg)

$\eta (T) = \eta_0 e^{-bT}$

Viscosity of blood estimates greater than water ($5,5$ vs $1$)

Non-Newtonian fluids

- Newtonian like water: $\eta$ independent by $\tau$
- Non-Newtonian like blood: $\eta$ dependent by $\tau$

Pseudo-Plastic: non-newtonian at some point the $\eta$ decrease and it's simplest to move (honey)

Dilatant: non-newtonian at some point the $\eta$ increase and it's hardest to move

### 1.2 Flow Regimes

**Laminar Flow**

![](assets/chapter_one/laminar.jpg)

- No turbulence
- Reversible
- Minimum dissipation
- Unique solution

**Turbulent Flow**

Chaotic flow with vortex

![](assets/chapter_one/turbulent.jpg)

Reynolds Number: $R_e = \frac{\rho * 2r * v}{\eta}$

- $v:$ fluid velocity $[\frac{m}{s}]$
- $r:$ channel radius $[m]$
- $\rho :$ density $[\frac{kg}{m^3}]$
- $\eta :$ viscosity $[P_a * s]$

$R_e < 2300$ laminar flow.

$R_e > 3000$ turbulent flow. 