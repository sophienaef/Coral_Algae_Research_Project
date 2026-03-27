#' @title Bayesian Blocks for Time Series Analysis
#'
#' @description Dynamic programming algorithm for solving a piecewise-constant model for
#' various datasets. This is based on the algorithm presented in Scargle
#' et al 2013.
#'
#' Applications include:
#'
#' - finding an optimal histogram with adaptive bin widths
#' - finding optimal segmentation of time series data
#' - detecting inflection points in the rate of event data
#'
#' The primary interface to these routines is the :func:`bayesian_blocks`
#' function. This module provides fitness functions suitable for three types
#' of data:
#'
#' - Irregularly-spaced event data via the `Events` data_type
#' - Regularly-spaced event data via the `RegularEvents` data_type
#' - Irregularly-spaced point measurements via the `PointMeasures` data_type
#'
#' For more fine-tuned control over the fitness functions used, it is possible
#' to define custom :class:`FitnessFunc` classes directly and use them with
#' the :func:`bayesian_blocks` routine.
#'
#' One common application of the Bayesian Blocks algorithm is the determination
#' of optimal adaptive-width histogram bins. This uses the same fitness function
#' as for irregularly-spaced time series events. The easiest interface for
#' creating Bayesian Blocks histograms is the :func:`astropy.stats.histogram`
#' function.

# Define the Bayesian Blocks function
bayesian_blocks <- function(t, x=NULL, p0=0.05, sigma=NULL, dt=NULL, gamma=NULL, ncp_prior=NULL, data_type='Events'){
    # Validate the input data
    validation <- validate_input(t, x, sigma, data_type)

    # Extract the validated data
    t <- validation$t
    x <- validation$x
    sigma <- validation$sigma

    # Get the number of data points
    N <- length(t)

    # Define the edges of the blocks
    edges <- c(t[1], 0.5 * (t[2:N] + t[1:(N - 1)]), t[N])

    # Calculate the length of each block
    block_length <- t[N] - edges

    # Initialize the best fitness and last change point arrays
    best <- rep(0, times = N)
    last <- rep(1, times = N)

    # Calculate a_k and b_k
    ak_raw <- rep(1, times=length(x)) / sigma^2
    bk_raw <- x / sigma^2

    #-----------------------------------------------------------------
    # Start with first data cell; add one cell at each iteration
    #-----------------------------------------------------------------
    for (K in seq(1:N)) {
        kwds <- list()

        # If the data type is 'Events' or 'RegularEvents'        
        # Calculate T_k and N_k
        kwds$T_k <- block_length[1:K] - block_length[K + 1]
        kwds$N_k <- rev(cumsum(rev(x[1:K])))

        kwds$a_k = NULL
        kwds$b_k = NULL

        # If ncp_prior is NULL, compute it
        if (is.null(ncp_prior)) {
            ncp_prior <- compute_ncp_prior(N, p0, gamma)
        } else {
            ncp_prior <- ncp_prior
        }

        # If the data type is 'PointMeasures'
        if (data_type !='Events'){
            kwds$a_k = 0.5*rev(cumsum(rev(ak_raw[1:K])))
            kwds$b_k = -1 * rev(cumsum(rev(bk_raw[1:K])))
        }

        # Calculate the fitness vector
        fit_vec <- fitness_func(N_k=kwds$N_k, T_k=kwds$T_k, dt=dt, a_k=kwds$a_k, b_k=kwds$b_k, data_type)

        # Calculate A_R
        A_R <- fit_vec - ncp_prior
        A_R[2:length(A_R)] <- A_R[2:length(A_R)] + best[1:(K-1)]

        # Find the index of the maximum value in A_R
        i_max <- which.max(A_R)
        # Update the last change point and the best fitness
        last[K] <- i_max
        best[K] <- A_R[i_max]
    }

    #-----------------------------------------------------------------
    # Recover changepoints by iteratively peeling off the last block
    #-----------------------------------------------------------------
    change_points <- rep(1, times = N)
    i_cp <- N
    ind <- N+1
    while (i_cp > 1) {
        change_points[i_cp] <- ind
        if (ind == 1) { break }
        i_cp <- i_cp - 1
        ind <- last[ind-1]
    }

    # If i_cp is 1, set the first change point to 1
    if (i_cp == 1) {
        change_points[i_cp] <- 1
    }

    # Get the change points
    change_points <- change_points[i_cp:length(change_points)]

    # Return the edges of the change points
    return(edges[change_points])
}

validate_input <- function(t, x=NULL, sigma, data_type='Events'){
    
    # Convert 't' to numeric
    t <- as.numeric(t)

    # Get the order of 't' in ascending order
    order_t <- order(t)

    # Sort 't' in ascending order
    t <- t[order_t]

    # If 'x' and 'sigma' are not NULL, sort them according to 't'
    if (!is.null(x)) {
        x <- x[order_t]
    }

    if (!is.null(sigma)) {
        sigma <- sigma[order_t]
    }

    # Get unique values of 't'
    unq_t <- unique(t)

    # Get the indices of the unique values in the original 't'
    unq_ind <- match(unq_t, t)

    # Get the indices of the original values in the unique 't'
    unq_inv <- match(t, unq_t)

    # If 'x' is NULL
    if (is.null(x)) {
        
        # If 'sigma' is not NULL, stop the function and show an error message
        if (!is.null(sigma)) {
            stop("If sigma is specified, x must be specified")
        } else {
            # If 'sigma' is NULL, set it to 1
            sigma <- 1
        }

        # If all values in 't' are unique
        if (length(unq_t) == length(t)) {
            # Set 'x' to a vector of 1s with the same length as 't'
            x <- rep(1, times = length(t))
        } else {
            # If there are duplicate values in 't', count the number of each unique value
            x <- tabulate(unq_inv)
        }

        # Set 't' to the unique values
        t <- unq_t
    } else {
        # If 'x' is not NULL, convert it to numeric
        x <- as.numeric(x)

        # Add 0 to 'x' to ensure it has the same length as 't'
        x <- x + rep(0, times = length(t))

        # Set 't' to the unique values and 'x' to the values at the indices of the unique values
        t <- unq_t
        x <- x[unq_ind]
    }

    # If 'sigma' is NULL, set it to 1
    if (is.null(sigma)) {
        sigma <- 1
    } else {
        # If 'sigma' is not NULL, convert it to numeric
        sigma <- as.numeric(sigma)

        # If the length of 'sigma' is not 0, 1, or the same as 't', stop the function and show an error message
        if (!(length(sigma) %in% c(0, 1, length(t)))) {
            stop("sigma does not match the length of x")
        }
    }

    # If the data type is 'Events' or 'RegularEvents'
    if (data_type=='Events' || data_type=='RegularEvents'){
        # If 'x' is not NULL and any value in 'x' is not an integer, stop the function and show an error message
        if (!is.null(x) && any(x %% 1 > 0)) {
                stop("x must be integer counts for fitness='Events'")
        }
        # Return a list with 't', 'x', and 'sigma'
        return(list(t = t, x = x, sigma = sigma))

    # If the data type is 'PointMeasures'
    } else if (data_type=='PointMeasures'){
        # If 'x' is NULL, stop the function and show an error message
        if (is.null(x)){
            stop("x must be specified for fitness=`PointMeasures`")
        }
        # Return a list with 't', 'x', and 'sigma'
        return(list(t = t, x = x, sigma = sigma))
    }
}

compute_p0_prior <- function(N, p0=0.05){
            return(4 - log(73.53 * p0 * (N^-0.478)))
}

compute_ncp_prior <- function(N, p0=0.05, gamma=NULL) {
            if (!is.null(gamma)) {
                return(-log(gamma))
            } else if (is.null(gamma) & !is.null(p0)) {
                return(compute_p0_prior(N))
            } else {
                stop("ncp_prior cannot be computed as neither gamma nor p0 is defined")
            }
}

# Define the fitness function
fitness_func <- function(N_k=NULL, T_k=NULL, dt=NULL, a_k=NULL, b_k=NULL, data_type='Events'){

    # If the data type is 'Events'
    if (data_type=='Events'){
        # Return the fitness value calculated as N_k * log(N_k / T_k)
        return(N_k * (log(N_k / T_k)))

    # If the data type is 'RegularEvents'
    } else if (data_type=='RegularEvents'){
        # Calculate M_k as T_k / dt
        M_k <- T_k / dt

        # Calculate N_over_M as N_k / M_k
        N_over_M <- N_k / M_k

        # Define a small number to avoid division by zero
        eps <- 1e-8

        # If any value in N_over_M is greater than 1 + eps, show a warning
        if (any(N_over_M > 1 + eps)) {
            warning("Regular Events: N/M > 1.  Is the time step correct?")
        }

        # Calculate one_m_NM as 1 - N_over_M
        one_m_NM <- 1 - N_over_M

        # If any value in N_over_M is less than or equal to 0, set it to 1
        N_over_M[N_over_M <= 0] <- 1

        # If any value in one_m_NM is less than or equal to 0, set it to 1
        one_m_NM[one_m_NM <= 0] <- 1

        # Return the fitness value calculated as N_k * log(N_over_M) + (M_k - N_k) * log(one_m_NM)
        return(N_k * log(N_over_M) + (M_k - N_k) * log(one_m_NM))

    # If the data type is 'PointMeasures'
    } else if (data_type=='PointMeasures'){
        # Return the fitness value calculated as (b_k * b_k) / (4 * a_k)
        return((b_k * b_k) / (4 * a_k))
    }
}