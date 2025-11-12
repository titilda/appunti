---
title: "Foundation of Operational Research"
author:
- "Ortore Joele Andrea"
---

## Linear Programming 
A linear programming problem is an optimization problem where:
- **The objective function** $\mathscr{F} : X \to R $  is linear , where X is the feasible region.
- **The feasible region**  has linear constraints.

> A solution $\underline{x}^* \in R^n  $ is said to be **optimal** if  $ f(\underline{x}^*) $ beats $f(\underline{x}), \forall \underline{x} \in X $.

### Traditional representations:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h3>General form</h3>

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
<h3>Matrix form </h3>
<div style="text-align: center;">

$ min \quad z = [c_1 \dots c_n] 
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
1. Consequences of the linearity:
   -  **Proportionality**: Contribution of each variable $=$ constant $\times$ variable , we often refer to these constants as parameters.
   -  **Additivity**: Contribution of all variable $=$ sum of single contributions : $f(x + y + z) = f(x) + f(y) + f(z)$
2. In these types of problems variables can take any rational values.
3. Parameters are constants.

### Example:
 Suppose that we want to maximize how much we gain from the selling of 5 products ( suppose that we sell and produce the products in grams) , the price/g of each one of them are respectively: 2, 3, 4, 5, 6; the cost of production of each one of them is: 3, 6, 7, 9, 10; no more than 400g must be produced and the total production cost must be lower than 3000.
We shall model this problem like this:

<div style="display:flex; justify-content:space-between; width:100%;">

<div style="flex:1; padding-right:10px;">
<h3>Traditional modelling</h3>

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
<h3>Using python's MIP library:</h3>

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
The feasible region is an :
- **Hyperplane** if it is defined as H = ${\underline{x} in R^n : \underline{a}

