---
title: "Onde Elettromagnetiche e Mezzi Trasmissivi"
author: 
- "Alessandro Modica"
date: "25 Novembre 2023"
---

# Segnali
Un **segnale** $s(t)$ è una grandezza fisica che varia nel tempo e nello spazio. 
Per trasmettere informazioni, un segnale non deve essere periodico.

$$s(t) = \begin{cases}
v(t) & \to \text{ tensione} & \left[V\right]\\
i(t) & \to \text{ corrente} & \left[A\right]\\
\vec{e}(t) & \to \text{ campo elettrico} & \left[\frac{V}{m}\right]\\
\vec{h}(t) & \to \text{ campo magnetico} & \left[\frac{A}{m}\right]\\
\end{cases}
$$

I segnali digitali sono sequenze di valori discreti nel tempo che prendono il nome di **simboli**.

## Perdite di trasmissione

- **attenuazione**: diminuzione dell'ampiezza del segnale
- **distorsione**: deformazione temporale dell'onda
  - **selettività in frequenza**: distorsione che dipende dalla frequenza
  - **dispersione**
    - **cromatica**: frequenze diverse si propagano a velocità diverse
    - **modale** (in fibra ottica): frequenze diverse si propagano in *modi* (traiettorie) diverse
- **rumore**: segnale indesiderato, casuale, che si somma al segnale utile (**NON trattato in questo corso**)

## Serie di Fourier

Ogni segnale periodico può essere scomposto in una sommatoria di sinusoidi con coefficienti complessi:

$$s(t) = \sum_{k=-\infty}^{+\infty} S_k e^{j2\pi k f_0t}$$

dove $f_0$ è la frequenza fondamentale e $S_k$ sono i coefficienti complessi:

$$S_K = \left|S_k\right| e^{j\phi_k}$$

### Formule di Eulero

$$\begin{cases}
\cos(2\pi f_0 t) = \displaystyle\frac{e^{j2\pi f_0 t} + e^{-j2\pi f_0 t}}{2}\\
\sin(2\pi f_0 t) = \displaystyle\frac{e^{j2\pi f_0 t} - e^{-j2\pi f_0 t}}{2j}\\
\end{cases}$$

ovvero:

|        | $S_1$                       | $S_{-1}$                     |
| ------ | --------------------------- | ---------------------------- |
| $\cos$ | $\displaystyle\frac{1}{2}$  | $\displaystyle\frac{1}{2}$   |
| $\sin$ | $\displaystyle\frac{1}{2j}$ | $-\displaystyle\frac{1}{2j}$ |

## Trasformata di Fourier

La trasformata di Fourier è una generalizzazione della serie di Fourier per segnali non periodici.

$$S(f) = \int_{-\infty}^{+\infty} s(t) e^{-j2\pi ft}\, dt$$

È possibile ottenere il segnale originale a partire dalla trasformata di Fourier:

$$s(t) = \int_{-\infty}^{+\infty} S(f) e^{j2\pi ft}\, df$$

### Traslazione nel tempo

$$\begin{align*}
s(t) &\to S(f)\\
s(t-\tau) &\to S(f) e^{-j2\pi f \tau}\\
\end{align*}$$

Traslare nel tempo un segnale ne modifica la fase, ma non la sua banda:

$$\begin{align*}
Z(f) &= S(f) e^{-j2\pi f \tau}\\
&\updownarrow\\
\measuredangle Z(f) &= \measuredangle S(f) - 2\pi f \tau\\
\end{align*}$$

### Traslazione in frequenza

$$\begin{align*}
s(t) &\to S(f)\\
s(t) e^{j2\pi f_0 t} &\to S(f-f_0)\\
\end{align*}$$

La banda del nuovo segnale ha la stessa ampiezza di quella originale, ma è centrata in una nuova **frequenza portante** incrementata di $f_0$.

## Risposta in frequenza di un mezzo di trasmissione

La **risposta in frequenza** di un mezzo di trasmissione $H(f)$ è la sua capacità di trasmettere segnali con frequenze diverse.

$$S_{out}(f) = S_{in}(f) \cdot H(f)$$

Il modulo di $H(f)$ descrive l'attenuazione del segnale, mentre la fase descrive la distorsione.

$$\begin{align*}
\left|H(f)\right| &= \left| \frac{S_{out}(f)}{S_{in}(f)} \right|\\
\measuredangle H(f) &= \measuredangle S_{out}(f) - \measuredangle S_{in}(f)\\
\end{align*}$$

Affinché esista $H(f)$, il mezzo di trasmissione deve essere **lineare** e **tempo-invariante** (in questo corso, tutti i mezzi di trasmissione sono lineari e tempo-invarianti).

In questo corso, tutti i mezzi di trasmissione sono **passivi** (il modulo di $H(f)$ è sempre minore di 1).

## Rappresentazione in scala logaritmica

### Decibel (base 10)
$$\begin{align*}
X &\to X_{\text{dB}} = 10 \log_{10} X\\
X_{\text{dB}} &\to X = 10^{\frac{X_{\text{dB}}}{10}}\\
\end{align*}$$

### Neper (base $e$)
$$\begin{align*}
X &\to X_{\text{Np}} = \ln X\\
X_{\text{Np}} &\to X = e^{X_{\text{Np}}}\\
\end{align*}$$

Per passare da una base all'altra, basta moltiplicare per il fattore di conversione:

$$\begin{align*}
X_{\text{dB}} &= 10 \log_{10} X = 10 \frac{\ln X}{\ln 10} = 10 \frac{X_{\text{Np}}}{\ln 10}\\
X_{\text{Np}} &= \ln X = \ln 10 \frac{X_{\text{dB}}}{10} = \ln 10 \frac{X_{\text{dB}}}{10}\\
\end{align*}$$

### Potenza

Per la potenza, si usa la scala logaritmica in base 10 con unità di misura $\text{dBm}$ (decibel-milliwatt):

$$P\ [\text{dBm}] = 10 \log_{10} \frac{P\ [\text{mW}]}{1\ \text{mW}}$$

esempio:
$$\begin{align*}P &= \mu\text{W} = 10^{-6} \text{ W} = 10^{-3} \text{ mW}\\
P_{\text{dBm}} &= 10 \log_{10} \frac{10^{-3}}{1} = -30\ \text{dBm}\\
\end{align*}$$