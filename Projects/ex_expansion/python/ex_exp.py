import math, numpy as np, mpmath as mp, pandas as pd, matplotlib.pyplot as plt, plotly.express as px, plotly.graph_objects as go

def taylor_series_expansion(x, max_terms=1000, dps=20):
    """
    Calculates the Taylor series expansion of e^x adaptively 
    with given decimal precision (dps) and up to max_terms.
    
    :param x: The value of x in e^x.
    :param max_terms: Maximum number of terms allowed in the series
    :param dps: Desired decimal precision (sets both mp.dps and epsilon = 1e-dps)
    :return: The approximation of e^x
    """
    mp.mp.dps = dps
    epsilon = mp.mpf(f"1e-{dps}")  # epsilon = 1e-dps
    
    result = mp.mpf(1)  # T0 = 1
    term = mp.mpf(1)
    k = 1
    
    while abs(term) > epsilon * abs(result) and k < max_terms:
        term = term * mp.mpf(x) / k
        result += term
        k += 1
    
    return result

def calculate_ex(x, n : int, precision : int = 20):
    """
    Calculates e^x using the Taylor series expansion and computes the error.
    
    :param x: The value of x in e^x.
    :param n: The number of terms in the Taylor series.
    :param precision: The precision for mpmath calculations (default is 20).
    :return: A tuple containing:
             straight - The direct approximation of e^x.
             inverted - The inverse approximation (1/e^-x) if x is negative, otherwise None.
             real_value - The real value of e^x.
             directError - The error in the direct approximation.
             invertedError - The error in the inverse approximation if applicable, otherwise None.
    """
    mp.mp.dps = precision
    
    real_value = mp.exp(x)
    straight = taylor_series_expansion(x, n)
    directError = mp.mpf(abs(real_value - straight))

    if x < 0:
        inverted = mp.mpf(1) / taylor_series_expansion(-x, n)
        invertedError = abs(real_value - inverted)
        return straight, inverted, directError, invertedError, real_value
    else:
        return straight, None, directError, None, real_value
    
def show_ex(x, n : int, precision : int = 20):
    """
    Displays the value of e^x using the Taylor series expansion.
    
    :param x: The value of x in e^x.
    :param n: The number of terms in the Taylor series.
    :param precision: The precision for mpmath calculations (default is 20).
    """
    mp.mp.dps = precision

    straight, inverted, directError, inverseError, real_value = calculate_ex(x, n,precision)

    print(f"\n--------------------------------------------\n")
    print(f"e^x for x = {x} with {n} terms and precision {precision}:\n")
    print(f"\nReal value:                  {real_value} \n")
    print(f"Direct apx:                  {straight}")
    print(f"Direct error:                {directError}")
    if inverted is not None:
        print(f"Inverse apx: (1/e^-x):       {inverted}")
        print(f"Inverse error:               {inverseError}")
        
    print(f"\n--------------------------------------------\n")

show_ex(1, 5, 2)
show_ex(-1, 5, 200)