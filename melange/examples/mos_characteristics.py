# MOCKUP currently melange is not complete
# This file represents a goal API of melange but some details may change
from melange import Circuit, CircuitInstance

import numpy as np
from matplotlib import pyplot as plt
# This file demonstrates plotting the transfer characteristic of a MOS transistor from scratch
# any values (like length) could be replaced by parameters to allow sweeping


# simulation parameters 
vg = np.linspace(-2,2,100)
freq = 10e9
temp = 300

# create circuit
circ = Circuit("test_circuit")
circ.load_veriloga_file("bsimbulk.va")
fet = CircuitInstance(circ, "test_fet", "bsimbulk", ports=["drain", "gate" ,"gnd", "gnd"])
fet.set_param("RSH", 1e-3) # real model has many more parameter
vdd = CircuitInstance(circ, "vdd", "vsource", ports=["drain", "gnd"])
vdd.set_param("dc", 2)
vdd.set_param("ac", 0)
vin = CircuitInstance(circ, "vin", "vsource", ports=["gate", "gnd"])
vin.set_param("dc", "gate_bias")
vin.set_param("ac", 1)

# set circuit parameters and setup simulation for each parameter
simulation = circ.prepare_sim(temp=temp, gate_bias=vg)

# obain drain current from dc simulations
simulation.dc_op()
id = simulation.lead_current("vdd", "anode");
# plot transfer characteristic Id(Vg)
plt.plot(vg, id)

# deterime ft from ac simulations (using the previously simulated operating points)
simulation.ac(freq=freq)
y21 = simulation.ac_lead_current("vdd", "anode")
y11 = simulation.ac_lead_current("vin", "anode")
ft = freq/np.imag(y11/y21)
# plot ft characteristic ft(Vg)
plt.plot(vg, ft)
plt.show()
