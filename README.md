# Pairs-Trading-Using-Copula-Mixture-Model

### Overview
This project implements a **Copula-Based Pairs Trading Strategy** that models non-linear dependencies between asset pairs, addressing the limitations of traditional pairs trading methods. The strategy utilizes **Gaussian, Gumbel, and Clayton copulas**, along with a **mixture model** optimized using the **Expectation-Maximization (EM) algorithm**.

The approach outperforms traditional Bollinger Bands strategies, achieving improved Sharpe Ratios, reduced volatility, and higher CAGR.

---

## Table of Contents
1. [Motivation](#motivation)
2. [Objectives](#objectives)
3. [Methodology](#methodology)
   - [Data Preprocessing](#data-preprocessing)
   - [Copula Fitting Methods](#copula-fitting-methods)
   - [Trading Strategy](#trading-strategy)
4. [Results](#results)
5. [Future Work](#future-work)
6. [Technologies Used](#technologies-used)
7. [References](#references)

---

## Motivation
Traditional pairs trading strategies rely on linear assumptions (e.g., correlation), which fail to capture complex, non-linear relationships between assets. This project explores **copulas**, which allow for modeling **non-linear dependencies**, including tail dependencies critical for risk management and pairs trading.

---

## Objectives
- Develop a **copula-based trading strategy** to capture non-linear dependencies between asset pairs.
- Compare two methods of fitting copulas:
  - Best-fit copula based on **log-likelihood**.
  - **Mixture copula model** optimized using the EM algorithm.
- Evaluate strategy performance using key metrics: **Sharpe Ratio**, **CAGR**, and **Annualized Volatility**.

---

## Methodology

### 1. Data Preprocessing
- Selected S&P 500 stock pairs with **correlation > 0.8** and confirmed cointegration using the **Johansen Test**.
- Used **log returns** instead of raw prices to ensure stationarity.
- Transformed log returns to uniform marginals using the **Empirical CDF Transformation**.

### 2. Copula Fitting Methods
#### **Best-Fit Copula**
- Fit three copulas (Gaussian, Gumbel, Clayton) to the data.
- Selected the copula with the **highest log-likelihood**.

  ![image](https://github.com/user-attachments/assets/cf2925de-046e-4462-845a-50b4d344b845)


#### **Mixture Copula Model**
- Defined a weighted mixture of the three copulas:

![image](https://github.com/user-attachments/assets/10ada73c-0e64-40cb-9a60-b4b8a8727610)

![image](https://github.com/user-attachments/assets/65cd6bcd-eec0-4f70-b005-644faab7e623)

  - Optimized weights using:
    - **Grid Search**: Exhaustive search over possible weights.
    - **Expectation-Maximization (EM)**: Iteratively optimized weights, achieving faster computation (20x improvement).

### 3. Trading Strategy
- **Conditional Probabilities**: Calculated using partial derivatives of the copulas.
- **Trading Rules**:
  - **Opening Position**:
    - Long Asset 1 and Short Asset 2 if:
      \[ P(U_1 < u_1 | U_2 = u_2) < b_a \text{ and } P(U_2 < u_2 | U_1 = u_1) > b_b \]
    - Short Asset 1 and Long Asset 2 if:
      \[ P(U_2 < u_2 | U_1 = u_1) < b_a \text{ and } P(U_1 < u_1 | U_2 = u_2) > b_b \]
  - **Exit Position**: Close trades when conditional probabilities cross 0.5.
- **Rolling Window**: Recomputed copula fits periodically to adapt to changing market dynamics.
- Included transaction costs: Fixed fee of $1 and a variable fee of 0.001% per trade.

---

## Results
### Performance Comparison
The copula-based strategy significantly outperformed traditional Bollinger Bands approaches:

| **Stock Pair**       | **Best-Fit Copula Log-Likelihood** | **Mixture Model Log-Likelihood** |
|----------------------|------------------------------------|----------------------------------|
| MSFT - ADBE          | 361.446                            | 392.1353                         |
| PYPL - ADBE          | 223.971                            | 250.1762                         |
| MSFT - ACN           | 274.902                            | 299.8242                         |
| AMZN - ADBE          | 260.357                            | 280.5166                         |
| ACN - LIN            | 205.099                            | 223.0498                         |

### Key Metrics
| **Stock Pair**       | **Sharpe Ratio** | **Volatility** | **CAGR**   |
|----------------------|-----------------|---------------|------------|
| AMZN - ADBE         | 9.81            | 29.82%        | 18.43%     |
| V - ACN             | 9.14            | 43.50%        | 25.04%     |
| MA - LIN            | 11.41           | 46.53%        | 33.45%     |
| V - PYPL            | 9.17            | 61.50%        | 35.41%     |

**Conclusion**: The mixture model consistently achieved higher log-likelihood values, Sharpe Ratios, and CAGR compared to traditional methods.

---

## Future Work
- **Time-Varying Copulas**: Adapt dependency structures dynamically over time.
- **Integration with Machine Learning**: Utilize **Reinforcement Learning** to optimize copula parameters and trading rules.
- **Broader Asset Classes**: Expand to commodities, bonds, and cryptocurrencies for further validation.

---

## Technologies Used
- **R**: quantmod, copula, urca
- **Python**: pandas, NumPy, SciPy, matplotlib
- **Statistical Modeling**: Johansen Test, Log-Likelihood Estimation, EM Algorithm
- **Trading Metrics**: Sharpe Ratio, Annualized Volatility, CAGR

---

## References
1. Liew, R. Q., & Wu, Y. (2013). *Pairs trading: A copula approach*. Journal of Derivatives & Hedge Funds.
2. Sklar, A. (1959). *Fonctions de répartition à n dimensions et leurs marges*.
3. Meng, X.-L., & Van Dyk, D. (2002). *The EM algorithm*. Journal of the Royal Statistical Society.
4. Gatev, E., Goetzmann, W. N., & Rouwenhorst, K. G. (2006). *Pairs trading: Performance of a relative-value arbitrage rule*. The Review of Financial Studies.

---
