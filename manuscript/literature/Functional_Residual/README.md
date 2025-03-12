# Reproducibility Materials
Code to reproduce simulation results, figures and real data analysis results from the paper "A unified framework for residual diagnostics in generalized linear models and beyond"

## Organization

# R Functions for Model Diagnostics

To provide code that actually implements the method, we have created the following R functions.

- **`fresiduals`** is a function for producing functional residuals. More specifically, it yields $\pi(y-1;\mathbf{x})$ and $\pi(y;\mathbf{x})$ that are used to define a functional residual in Equation (2) on page 6 of the manuscript. The input is a fitted model, which can be a `vglm`, `glm`, `tsglm`, or `zeroinfl` object. This function generates an $n \times 2$ matrix as its output, where $n$ rows correspond to the $n$ observations, and 2 columns correspond to $\pi(y - 1; \mathbf{x})$ and $\pi(y; \mathbf{x})$ for each observation. This output can be called by other functions to generate model diagnostics results.

  ```r
  # Example usage:
  bikedata <- read.csv("hour.csv")
  model <- glm(cnt~hr+workingday+weathersit+
              temp+hum+windspeed,family = "poisson",data = bikedata)
  FR <- fresiduals(model)
  ```

- **`fresplot`** is a function for generating functional-residual-vs-covariate plots, visualizing the density of functional residuals as a heatmap. These plots can be rendered on either uniform or normal scales. It requires three inputs:

  - **`fresiduals`**: the object generated from the R function `fresiduals`.
  - **`x`**: the covariate against which to plot functional residuals.
  - **`scale`**: the scale of functional residuals, which can be uniform or normal.

  The `fresplot` function provides optional inputs, such as `title`, which enables users to customize the plot title. It also offers `xl` and `xp` for setting the lower and upper limits of the x-axis, and `yl` and `yp` for setting the lower and upper limits of the y-axis.

  ```r
  fresplot(fresiduals = FR, 
           x = hr, 
           title = "Functional residual vs. hour",
           scale = "normal",
           xl = 0,
           xp = 24)
  ```

- **`ffplot`** is a function that generates an $F_n$- $F_n$ plot. It requires a single input:

  - **`fresiduals`**: the object generated from the `fresiduals` function.

  ```r
  ffplot(fresiduals = FR,
         title = "Fn-Fn Plot")
  ```

- **`ffplot.envelop`** generates an $F_n$- $F_n$ plot with a ``probable envelop".  The envelope is formed by creating confidence bounds around the $F_n$- $F_n$ curve. At any given point $t$, it reduces to a confidence interval for $\overline{Res}(t)$ in (4) of the manuscript (with a pre-specified error rate $\alpha$). Specifically, the envelop is the set <img src="https://latex.codecogs.com/svg.image?$\mathcal{E}_{1-\alpha}=\{(L^*_{\alpha/2}(t),U^*_{1-\alpha/2}(t)),t\in(0,1)\}$" />, where <img src="https://latex.codecogs.com/svg.image?$L^*_{\alpha/2}(t)$"/> and <img src="https://latex.codecogs.com/svg.image?$U^*_{1-\alpha/2}(t)$"/> are lower and upper bounds constructed using the bootstrap method (see Supplementary Materials G to the paper). If the 45-degree line falls outside <img src="https://latex.codecogs.com/svg.image?$\mathcal{E}_{1-\alpha}$"/>, this may be regarded as evidence against the null that the model is correct.

 - **`model`**: the object of the fitted model.
  - **`B`**: the number of bootstrap samples for the graphical test. The default is 2000.
  - **`alpha`**: the error rate of the envelop. The default is 0.05.
  ```r
  ffplot.envelop(model = model,
                 B = 2000,
                 alpha=0.05,
                 title = "Fn-Fn plot with test envelop") 
  ```
### R_code_simulation

#### Illustration Figures

- "Sim_Figure1-3.R" contains R code for Figure 1-3 in Section 2.3

#### Simulation for ordinal data

- "Sim_Ordinal.R" contains R code for 

   - Figure 4-5 in Section 3.1;
   - Figure S1-S4 in Supplementary Materials B;
   - Figure S21 and S22 in Supplementary Materials F.

#### Simulation for count data

- "Sim_Count.R" contains R code for 
   - Figure S5-S9 in Supplementary Materials B;
   - Figure S17-S19 in Supplementary Materials D.
   
#### Simulation Graphical Test

- "Sim_Graphical_Test.R" contains R code for Figure S23 in Supplementary Materials G;
- "Sim_Power_Comparison.R" contains R code for producing the results displayed in Table S4 in Supplementary Materials G.

### Simulation-results

Contains the results from running the code in the simulation-code folder as described above. 


### Datasets

* "winequality-white.csv" contains the real data set in Section 5.1. Detailed information about this dataset can be accessed [here](https://archive.ics.uci.edu/dataset/186/wine+quality). 

* "hour.csv" contains the real data set in Section 5.2. Detailed information about this dataset can be accessed [here](https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset). 

### R_code_real_data_analysis
- "Whitewine.R" contains the code to run real data analysis in Section 5.1. It produces 
   - Figure S10-S12 in Supplementary Materials B;
   - Table S1 in Supplementary Materials C.


- "Bike.R" contains the code to run real data analysis in Section 5.2. It produces    
   - Figure S13-S16 in Supplementary Materials B;
   - Table S3 in Supplementary Materials C.


- "Bike_TimeSeries.R" contains the code to run the temporal effects analysis and produces Figure S20 in Supplementary Materials E. 


- "Robustness.R" contains the code to run the robustness analysis and produces Figure S24 in Supplementary Materials E. 

