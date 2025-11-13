---
title: "Foundation of Operational Research"
author:
- "Ortore Joele Andrea"
---

## Linear Programming 
A linear programming problem is an optimization problem where:

- **The objective function** $F : X \to R$  is linear , where X is the feasible region.
- **The feasible region**  has linear constraints.

:::{.callout .callout-definition title="Optimal solution"}
A solution $\underline{x}^* \in R^n$ is said to be **optimal** if  $f(\underline{x}^*)$ beats $f(\underline{x}), \forall \underline{x} \in X$.
:::
### Traditional representations:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>General form</h4>

$$ 
    \begin{align*}
     min \quad &z = c_1 x_1 + \dots + c_nx_n \\
    & a_{11} + \dots + a_{1n}x_n (\geq,=,\leq) b_1\\
    &\vdots\\
    & a_{m1}x_1+\dots + a_{mn}x_n (\geq,=,\leq) b_m\\
    &x_1,\dots,x_n \geq 0
    \end{align*}
$$

</div>

<div style="flex:1; padding-left:10px;">
<h4>Matrix form </h4>
<div style="text-align: center;">

$min \quad z = [c_1 \dots c_n] 
\begin{bmatrix}
x_1 \\
\vdots\\
x_n
\end{bmatrix}$

</div>

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">

$$
\begin{align*}
min \quad &z = \underline{c}^T \underline{x} \\
s.t.\quad  & A\underline{x} \geq \underline{b} \\
&\underline{x} \geq 0
\end{align*}
$$

</div>

<div style="flex:1; padding-left:10px;">
<div style="text-align: center;">


</div>

$$
s.t. \quad 
\begin{bmatrix}
a_{11} &  \dots & a_{1n} \\
\vdots & & \vdots\\
a_{m1} & \dots & a_{mn}
\end{bmatrix}
\begin{bmatrix}
x_1  \\
\vdots \\
x_n
\end{bmatrix}
\geq
\begin{bmatrix}
b_1  \\
\vdots \\
b_m
\end{bmatrix}
$$

</div>
</div>

$$
\begin{bmatrix}
x_1  \\
\vdots \\
x_n
\end{bmatrix}
\geq \underline{0}
$$
</div>
</div>

### General Assumptions
:::{.callout .callout-property title="Assumptions"}
1. Consequences of the linearity: 

   -  **Proportionality**: Contribution of each variable $=$ constant $\times$ variable , we often refer to these constants as parameters.
   -  **Additivity**: Contribution of all variable $=$ sum of single contributions : $f(x + y + z) = f(x) + f(y) + f(z)$


2. In these types of problems variables can take any rational values.


3. Parameters are constants.
:::
#### Example:
 Suppose that we want to maximize how much we gain from the selling of 5 products ( suppose that we sell and produce the products in grams) , the price/g of each one of them are respectively: 2, 3, 4, 5, 6; the cost of production of each one of them is: 3, 6, 7, 9, 10; no more than 400g must be produced and the total production cost must be lower than 3000.
We shall model this problem like this:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>Traditional modelling</h4>

$$  
 \begin{align*}
        max  \quad & 2\mathscr{x}_1 + 3\mathscr{x}_2 + 4\mathscr{x}_3 + 5\mathscr{x}_4 + 6\mathscr{x}_5 \\
        s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad (1)\\
        & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad(2)\\
        & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4,5}
\end{align*}
$$
1. Maximum production cost
2. Maximum quantity produced
</div>

<div style="flex:1; padding-left:10px;">
<h4>Using python's MIP library:</h4>

```python 
    import mip
    from mip import CONTINUOUS
    
    # these are our parameters
    I = [0, 1, 2, 3, 4] # Products
    P = [2, 3, 4, 5, 6] # Prices
    C = [3, 6, 7, 9, 10] # Costs
    Cmax = 3000 # Max total cost
    Qmax = 400 # Max total amount of product
    
    
    model=mip.Model()
    x = [model.add_var(name=f"x_{i+1}", lb=0 ,var_type=CONTINUOUS) for i in I]
    
    # Objective function
    model.objective = mip.maximize(mip.xsum(P[i] * x[i] for i in I))
    
    # Constraints
    
    # Maximum production cost
    model.add_constr(mip.xsum(x[i]*C[i] for i in I) <= 3000)
    
    # Maximum quantity produced
    model.add_constr(mip.xsum(x[i] for i in I)<= 400)
    
    model.optimize()
    for i in model.vars:
    print(i.name,i.x)
```
</div>
</div>


## Geometry of linear Programming
### A couple of definitions:
- A **hyperplane** defined as H = {$\,\underline{x} \in R^n : \underline{a}^T \underline{x} = b\,$} is a flat surface that generalize a two-dimensional plane (in our case where b $\ne$ 0 is said to be an **affine** hyperplane).
- An **affine half-space** defined as $H^-$ = {$\,\underline{x} \in R^n:\underline{a}^T \underline{x} \leq b \,$} is the region that lies "below" or "above" an **affine hyperplane**.
 
> Each inequality constraint defines an affine half-space in the variable space.

> The feasible region of a linear programming problem is the intersection of a finite number of half-spaces (constraints).
> Said feasible region is a **polyhedron**.

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h4>Hyperplane (affine) </h4>

![](assets/chapter3/hyperplane.png)

</div>

<div style="flex:1; padding-left:10px;">
<h4>Affine half-space</h4>

![](assets/chapter3/affine_half-space.png)

</div>
<div style="flex:1; padding-left:10px;">
<h4>Polyhedron</h4>

![](assets/chapter3/polyhedron.png)

</div>
</div>

### Convex subsets
:::{.callout .callout-definition title="Convex subsets" }
A subset $S \subseteq R^n$ is convex if for each pair of points $\underline{x}_1,\underline{x}_2 \in S$ the segment defined by them is the defined by all the **convex combinations** of the two points:
$$
 [\underline{x}_1,\underline{x}_2] = {\:\underline{p} \in R^n : \underline{x} = \alpha\underline{x}_1 + (1 - \alpha)\underline{x}_2 \, \land \alpha \in [0,1]\:}
$$
:::

Meaning that the subset contains the whole segment connecting the two points.

![](assets/chapter3/convexset.png)

[Convex set interactive graph](https://www.desmos.com/calculator/y3ays9vrh3?lang=it)

> A *polyhedron* is a convex set of $R^n$ $\implies $ The feasible region of a LP is a convex set of $R^n$

### Vertices

>A vertex of a polyhedron P is a point which cannot be expressed as a convex combination of two other **dinstinct** points of P.

Given a point $\underline{x}$ and a pair of points $(\underline{y}_1,\underline{y}_2)$ </br>

$\underline{x}$ is a vertex $\iff \:\underline{x} = \alpha\underline{y}_1 + (1 - \alpha)\underline{y}_2 \, \land \alpha \in [0,1]\: \land (\underline{x}= \underline{y}_1 \lor \underline{x}= \underline{y}_2)$ 

### Unbounded feasible direction of polyhedron
:::{.callout .callout-definition title="Unbounded feasible direction"}
An unbounded feasible direction d of a polyhedron P is a nonzero vector such that ,from a point $\underline{x} \in P$, all points of the form $\underline{x} + \lambda d \:$ with $\lambda > 0$ also belongs to P. The set of such points is often called the **ray** of d through $\underline{x}$.
:::
![](assets/chapter3/unbounded.png)

### Polytopes
:::{.callout .callout-definition title="Polytope"}
A polytope is a **bounded polyhedron** , hence the only unbounded feasible direction is d = 0 (so in a sense it does not have any)
:::

![A polytope](assets/chapter3/polytope.png)

### Representation of polyhedra - Weyl-Minkowsky Theorem
::: {.callout .callout-theorem title="Weyl-Minkowsky Theorem"}
Every point $\underline{x}$ of a polyhedron P can be expressed as a **convex combination** of its vertices $\underline{x}^1, \dots, \underline{x}^k$ plus (if needed) an **unbounded feasible direction** \underline{d} of P:
$$
 \underline{x} = \alpha_1\underline{x}_1 + \dots + \alpha_k \underline{x}_k + \underline{d}
$$
 where $\quad\sum_{i=1}^{k} \alpha_i = 1 ,\:\alpha_i \geq 0 , \forall i \in { 1 \dots k}$ <br>
 "The multipliers are positive and theirs sum must be equal to 1"
:::
## Standard form of LPs

### Transformations rules

- $max \quad \underline{c}^T \underline{x} \implies -min \:(-\underline{c}^T \underline{x})$
 

- $\underline{a}^T \underline{x} \leq b \implies \begin{cases} \underline{a}^T\underline{x} + s = b \\
  s \geq 0
  \end{cases}\quad$ where s is a **slack** variable.
  

- $\underline{a}^T \underline{x} \geq b \implies \begin{cases} \underline{a}^T\underline{x} - s = b \\
  s \geq 0
  \end{cases}\quad$ where s is a **surplus** variable.


- $x_j$ unrestricted sign \implies $\begin{cases} x_j = {x_j}^+\,-\,{x_j}^- \\
  {x_j}^+\, , \, x_j^- \geq 0
  \end{cases}\quad$ <br><br>
After substituting $x_j$ with ${x_j}^+\,-\,{x_j}^-$ , we delete $x_j$ from the problem.

### Standard form 
::: {.callout .callout-definition title="LP standard form"}
An LP is said to be in standard form if this is its appearance:
$$
\begin{align*}
min \quad & z=\underline{c}^T\underline{x} \\
s.t. \quad & A\underline{x} = \underline{b} \\
& \underline{x} \geq \underline{0}
\end{align*}
$$
:::

#### Example:
Let's put the following LP in standard form:

$$  
\begin{align*}
max  \quad & 2\mathscr{x}_1 + 3\mathscr{x}_2 + 4\mathscr{x}_3 + 5\mathscr{x}_4 + 6\mathscr{x}_5 \\
s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad \\
& \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad\\
& \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R
\end{align*}
$$
  
1. From maximization to minimization
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 \leq 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 \leq 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R
   \end{align*}
   $$
2. From inequality to equalities by adding slack variables
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 + S_1 = 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5 + S_2 = 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \: , \mathscr{x}_5 \in R \\
   & S_1, S_2 \geq 0
   \end{align*}
   $$
3. Substituting unrestricted sign variables
   $$  
   \begin{align*}
   min  \quad & -2\mathscr{x}_1 - 3\mathscr{x}_2 - 4\mathscr{x}_3 - 5\mathscr{x}_4 - 6\mathscr{x}_5 \\
   s.t. \quad & 3\mathscr{x}_1 + 6\mathscr{x}_2 + 7\mathscr{x}_3 + 9\mathscr{x}_4 + 10\mathscr{x}_5 + S_1 = 3000 \quad \\
   & \mathscr{x}_1 + \mathscr{x}_2 + \mathscr{x}_3 + \mathscr{x}_4 + \mathscr{x}_5^+ - \mathscr{x}_5^- + S_2 = 400\quad\\
   & \mathscr{x}_i \geq 0, \forall i \in {1,2,3,4} \:  \\
   & \mathscr{x}_5^+ , \mathscr{x}_5^- \geq 0 \\
   & S_1, S_2 \geq 0
   \end{align*}
   $$
    The problem is now in standard form.
### Fundamental Theorem of Linear Programming
:::{.callout .callout-theorem title="Fundamental Theorem of Linear Programming"}
Consider a minimization problem in standard form where the constraints define a non-empty feasible area (polyhedron) P.
Then either:
1. The value of **the objective function** is **unbounded below** on P.


2. Exists at least one **optimal** vertex.
:::
#### Proof
##### Case 1:
P has an unbounded feasible direction $\underline{d}$ such that $\underline{c}^T\underline{d}<0$ ,this means that proceeding in that direction will make the value smaller and smaller and the objective value tends to -$\infty$.
##### Case 2:
P has no unbounded feasible direction such that along that path the value keeps getting smaller.
As we saw any point of the feasible region can be expressed as a convex combination of its vertices plus the unbounded direction, so for any $\underline{x} \in P$ we have $\underline{d}$ or $\underline{c}^T \underline{d} \geq 0$ (either the value along that direction gets bigger or that direction is $\underline{0}$) so:

$$
    \underline{c}^T\underline{x} = \underline{c}^T ( \sum_{i=1}^{k}{\alpha_i\underline{x}^i + \underline{d}}) =\sum_{i=1}^{k}{\alpha_i\underline{c}^T\underline{x}^i +\underline{c}^T\underline{d}} \geq min_{i=1.\dots,k}{(\underline{c}^T\underline{x}^i)}
$$

Put in words this tells us that the objective value of any point inside the feasible region ($\underline{c}^T (\sum_{i=1}^{k}{\alpha_i\underline{x}^i + \underline{d}})$) will be bigger the minimum value that the objective function has in one of its vertices (the optimal vertex) : $min_{i=1.\dots,k}{(\underline{c}^T\underline{x}^i)}$.

>This also foreshadow that the solution of any minimization problem lies in one of its vertices ( if the solution is unique)

This would likely become more clear when we see how to solve LP problems graphically.

### Types of LPs
1. Unique optimal solution.
2. Multiple(infinite) optimal solutions.
3. Unbounded LP: Unbounded polyhedron and unlimited objective function value.
4. Empty polyhedron: no feasible solutions.

### Solving LPs Graphically

We would like to solve this problem:

$$
\begin{align*}
max &\quad x_1+3x_2 \\
    &\quad -x_1+x_2 \geq-2\\
    &\quad 2x_1+x_2 \leq 10 \\
    &\quad -x_1+3x_2 \leq 9 \\
    &\quad x_1,x_2 \geq 0
\end{align*}
$$

This is our feasible region (x = $x_1$ ; y  = $x_2$):


![feasible region](assets/chapter3/feasibleRegion.png)

Lets analyze the objective function:
$$
 z=x_1+3x_2
$$
Lets compute its gradient:
$$
\nabla z= [ 1 \quad 3]^T
$$

This tells us where to go if we want our value to increase, since we are solving an LP this direction will always be the same.

Let's take a level curve from our objective function by assigning z to some constant c:
$$
c = x_1+3x_2
$$
Starting from 0 we see that by increasing the value of c the level curve follows the direction of the gradient:
![](assets/chapter3/gradientincr.png)

If we follow that direction we would see that it leads us to the vertex $[3 \:,\: 4]^T$ since it the last meeting point it will give us the maximum value of the objective function:

$$
 z = 1 \times 3 + 3 \times 4 = 15
$$

This graphical method is pretty easy to use , but it is feasible only for problems with less than 2 variables.
To solve bigger problems we must find a better method.

#### MIP implementation:










