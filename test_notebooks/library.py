# Copyright (C) 2026 Mathieu Renzo
# SPDX-License-Identifier: GPL-3.0-or-later
import numpy as np


def example_function(x):
    """ test function for M-. """
    return np.asarray(x+0.5*x*x+0.33*x*x*x)
