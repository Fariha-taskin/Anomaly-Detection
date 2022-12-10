This is just a custom project; only for FUN. 

-------------------------------

What do we mean by Anomaly? Something that is not usual. Here we used some custom function to detect anomaly using Time Series forecasting methods. 
Note that these functions only works for additive series only. If you have a multiplicative series, add a log transformation. If $Y_t$ is the original series, then transform using

$$L_t=\log(Y_t).$$

However, if $Y_t$ contains $0$ or negative values, usual log transformation might not work. Then transform using the following transformation

$$L_t=\log(Y_t+C),$$

where $C$ is a constant and $C> Max(Y_t)$.
