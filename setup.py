from setuptools import setup
from setuptools_rust import RustExtension, Binding
import os

from setuptools_rust.extension import Strip


extension = RustExtension(
    "verilogae.verilogae", 
    path = "crates/verilogae_py/Cargo.toml",
    rust_version=">=1.57",
    binding=Binding.NoBinding,
    strip=Strip.All,
)



setup(
    name="verilogae",
    version="0.9-beta-5",
    author="DSPOM",
    author_email="dspom@protonmail.com",
    url="https://man.sr.ht/~dspom/openvaf_doc/verilogae/",
    description="Compile Verilog-A files to obtain equations of compact models",
    license="GPL-3",
    python_requires=">=3.7",
    packages=["verilogae"],
    # package_data={'': ['libverilogae.so','verilogae.so'],},
    # rust extensions are not zip safe, just like C-extensions.
    zip_safe=False,
    rust_extensions=[
        extension,
        # Extension(
        #     name='verilogae.marker',
        #     sources=[]
        # )
    ]
)
