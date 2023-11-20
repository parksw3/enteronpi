sir_nonpi <- function(t, y, par) {
  with(as.list(c(y, par)), {
    beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma

    dS <- mu * N - (beta * (I+omega)) * S/N - mu * S
    dI <- (beta * (I+omega)) * S/N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    dC <- beta * (I+omega) * S/N

    list(c(dS, dI, dR, dC))
  })
}

simulate_sir_nonpi_deterministic <- function(R0=32,
                                             gamma=1,
                                             phi=0.35,
                                             theta=0.2,
                                             mu=0.012/52,
                                             S0=1/32,
                                             I0=1e-5,
                                             omega=1,
                                             N=1e6,
                                             tmax=20) {
  par <- c(R0=R0, gamma=gamma, phi=phi, theta=theta, mu=mu, N=N, omega=omega)

  yini <- c(S=N*S0, I=N*I0, R=N*(1-S0-I0), C=0)

  tvec <- 0:(tmax*52)

  out <- as.data.frame(rk(yini, tvec, sir_nonpi, par))

  out
}

sir_npi_beta <- function(t, y, par) {
  with(as.list(c(y, par)), {
    if (t < npistart) {
      beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma
    } else if ((t >= npistart & t < npiend)) {
      beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma * (1-reduction)
    } else {
      beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma * (1-reduction_post)
    }


    dS <- mu * N - (beta * (I+omega)) * S/N - mu * S
    dI <- (beta * (I+omega)) * S/N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    dC <- beta * (I+omega) * S/N

    list(c(dS, dI, dR, dC))
  })
}

sir_npi_beta2 <- function(t, y, par) {
  with(as.list(c(y, par)), {

    if (t < npistart) {
      mobility <- 0
    } else if ((t >= npistart & t < npiend)) {
      mobility <- -reduction * 100
    } else if (t < npiend2) {
      mobility <- -reduction_post * 100
    } else {
      mobility <- 0
    }

    beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma * (1+mobility/100)

    dS <- mu * N - (beta * (I+omega)) * S/N - mu * S
    dI <- (beta * (I+omega)) * S/N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    dC <- beta * (I+omega) * S/N

    list(c(dS, dI, dR, dC), mobility=mobility)
  })
}

simulate_sir_npi_beta_deterministic <- function(R0=32,
                                                gamma=1,
                                                phi=0.35,
                                                theta=0.2,
                                                mu=0.012/52,
                                                S0=1/32,
                                                I0=1e-5,
                                                omega=1,
                                                N=1e6,
                                                tmax=20,
                                                npistart=0,
                                                npiend=0,
                                                reduction=0,
                                                reduction_post=0) {
  par <- c(R0=R0, gamma=gamma, phi=phi, theta=theta, mu=mu, N=N, omega=omega, npistart=npistart, npiend=npiend, reduction=reduction,
           reduction_post=reduction_post)

  yini <- c(S=N*S0, I=N*I0, R=N*(1-S0-I0), C=0)

  tvec <- 0:(tmax*52)

  out <- as.data.frame(rk(yini, tvec, sir_npi_beta, par))

  out
}

simulate_sir_npi_beta2_deterministic <- function(R0=32,
                                                gamma=1,
                                                phi=0.35,
                                                theta=0.2,
                                                mu=0.012/52,
                                                S0=1/32,
                                                I0=1e-5,
                                                omega=1,
                                                N=1e6,
                                                tmax=20,
                                                npistart=0,
                                                npiend=0,
                                                npiend2=0,
                                                reduction=0,
                                                reduction_post=0) {
  par <- c(R0=R0, gamma=gamma, phi=phi, theta=theta, mu=mu, N=N, omega=omega, npistart=npistart, npiend=npiend, npiend2=npiend2, reduction=reduction,
           reduction_post=reduction_post)

  yini <- c(S=N*S0, I=N*I0, R=N*(1-S0-I0), C=0)

  tvec <- 0:(tmax*52)

  out <- as.data.frame(rk(yini, tvec, sir_npi_beta2, par))

  out
}

sir_npi_mobility <- function(t, y, par, mobilityfun) {
  with(as.list(c(y, par)), {
    beta <- R0 * (1 + theta * cos(2 * pi * (t+phi*52)/52)) * gamma * (1+mobilityfun(t)/100 * mobility_scale)


    dS <- mu * N - (beta * (I+omega)) * S/N - mu * S
    dI <- (beta * (I+omega)) * S/N - gamma * I - mu * I
    dR <- gamma * I - mu * R
    dC <- beta * (I+omega) * S/N

    list(c(dS, dI, dR, dC), beta=beta,
         mobility=mobilityfun(t))
  })
}

simulate_sir_npi_mobility_deterministic <- function(R0=32,
                                                    gamma=1,
                                                    phi=0.35,
                                                    theta=0.2,
                                                    mu=0.012/52,
                                                    S0=1/32,
                                                    I0=1e-5,
                                                    omega=1,
                                                    N=1e6,
                                                    tmax=20,
                                                    mobilityfun,
                                                    mobility_scale=1) {
  par <- c(R0=R0, gamma=gamma, phi=phi, theta=theta, mu=mu, N=N, omega=omega, mobility_scale=mobility_scale)

  yini <- c(S=N*S0, I=N*I0, R=N*(1-S0-I0), C=0)

  tvec <- 0:(tmax*52)

  out <- as.data.frame(rk(yini, tvec, sir_npi_mobility, par, mobilityfun=mobilityfun))

  out
}
