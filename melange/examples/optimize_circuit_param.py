# MOCKUP currently melange is not complete
# This file represents a goal API for melange but some details may change
from melange import Circuit, CircuitInstance

import numpy as np
from matplotlib import pyplot as plt
# This file demonstrates optimizing the FET length and width for maximum power gain

# simulation parameters 
time = np.linspace(0,1e-9, 100)

# create circuit
circ = Circuit("test_circuit")
# load PDK from spectre netlist
circ.load_netlist("spectre", "pdk/include.scs", "example_corner")

fet = CircuitInstance(circ, "test_fet", "example_fet", ports=["drain", "gate" ,"gnd"])
fet.set_param("L", "length")
fet.set_param("W", "width")
vdd = CircuitInstance(circ, "vdd", "vsource", ports=["vdd", "gnd"])
vdd.set_param("dc", 2)
rf_block = CircuitInstance(circ, "rf_block", "inductor", ports=["vdd", "drain"])
rf_block.set_param("l", 1e-3)
vin = CircuitInstance(circ, "vin", "vsource", ports=["gate", "gnd"])
vin.set_param("type", "sin")
vin.set_param("sindc", 0.3)
vin.set_param("amp", 0.5)
vin.set_param("freq", 10e9)

def amplifier_gain(length, width):
    # set circuit parameters and setup simulation
    simulation = circ.prepare_sim(temp=300, length=length, width = width)

    simulation.tran(time_steps=time)
    id = simulation.tran_lead_current("inductor", "drain")
    vd = simulation.tran_voltage("drain")
    pout = 10*np.log10(np.average(id*vd))
    ig = simulation.tran_lead_current("vin", "gate")
    vg = simulation.tran_voltage("gate")
    pin = 10*np.log10(np.average(ig*vg))
    gain = pout-pin
    return gain

# use standard mathematical package like scipy (or machine learning) to opimzie the amplifier_gain function
