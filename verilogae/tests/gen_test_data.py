from pathlib import Path
import json
import numpy as np
import verilogae

mc = Path("mcard.json").read_text(encoding="utf-8")
args = json.loads(mc)

vbe = np.linspace(0.55,0.8,100)
vbc = np.array([-1.4,0.0,0.1])
temp = np.array([300.0,320.0,400])

temp_ = np.repeat(temp, 300)
vbe_ = np.tile(vbe,9)
vbc_ = np.tile(np.repeat(vbc, 100), 3)
args['temperature'] = temp_
args['voltages'] = {'br_biei': vbe_,'br_bei': vbe_,'br_bpei': vbe_, 'br_bie': vbe_,'br_be': vbe_,'br_bpe': vbe_, 'br_bici': vbc_,'br_bpci': vbc_, 'br_bci': vbc_,'br_sici': vbc_ - vbe_, 'br_sc': vbc_ - vbe_ }

hl2 = verilogae.load("hicumL2V2p4p0_vae.va")

itf = hl2.functions["itf"].eval(**args)
args["itf"] = itf

data = {}

for fun in hl2.functions.values():
    res = fun.eval(**args)
    data[fun.name] = res


np.savez('test_data.npz', **data)

