from setuptools import setup
from setuptools_rust import RustExtension, Binding
import os


extension = RustExtension(
    "verilogae.verilogae",
    path="verilogae/verilogae_py/Cargo.toml",
    rust_version=">=1.64",
    binding=Binding.NoBinding,
    debug=False,
)


setup(
    name="verilogae",
    version="1.0.0",
    author="DSPOM",
    author_email="dspom@protonmail.com",
    url="https://openvaf.semimod.de/docs/verilogae",
    description="Compile Verilog-A files to obtain equations of compact models",
    license="GPL-3",
    python_requires=">=3.8",
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
    ],
)
