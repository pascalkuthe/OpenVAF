//! Exposes a structued textual [description] of a [circuit] that is independent
//! of netlist format and can be [elaborated] to an actual [circuit].
//!
//! [elaborated]: crate::elaboration::CircuitDescription::elaborate
//! [description]: crate::elaboration::CircuitDescription
//! [circuit]: crate::circuit::Circuit

use anyhow::{bail, Context, Result};
use camino::Utf8PathBuf;
use typed_index_collections::TiVec;

use crate::circuit::{Circuit, DeviceId, InstanceId, ModelId, NameSpaceEntry, Node};
use crate::{veriloga, Arena, Expr};

/// A textual description of a circuit from which a circuit can be built.
/// This serves primarly as an intermediate step for the netlist parser.
/// By providing a netlist format independent representation, multiple netlist format can be easily
/// supterminaled withcout code duplication.
/// Furthermore downstream users might prefer the textual format over the builder API provided by
/// [`Circuit`]
///
/// A Circuit Description can be converted to a circuit [`Circuit`] with the
/// [`CircuitDescription::elaborate`] function
pub struct CircuitDescription {
    /// The name of the described circuit
    pub name: String,
    /// A listing of all instance within the described circuit
    pub instances: TiVec<InstanceId, CircuitInstanceDescription>,
    /// A listing of all models within the described circuit
    pub models: TiVec<ModelId, CircuitModelDescription>,
    /// A list of Verilog-A files that need to be compiled
    pub va_files: Vec<Utf8PathBuf>,

    /// Arena allocator for expressions
    pub earena: Arena,
}

/// A device instance inside a [`CircuitDescription`](create::circuit::CircuitDescription).
pub struct CircuitInstanceDescription {
    /// The name of this instance
    pub name: String,

    /// Refers to an entity by name that determines the device type:
    ///
    /// * a model
    /// * a device
    /// * a subcircuit
    ///
    /// If a device is specified an implicit model is created for this instance during elaboration.
    /// This anoynomous model recives all [`parameters`]  as model parameters.
    pub master: String,

    /// Parameter names and values specified by the user.
    ///
    /// These parameters are interpreted differently during elaboration depending
    /// on what the master field resolved to:
    ///
    /// * a model: parameters are interpreted as instance parameters
    /// * a device: parameters are interpreted as model parameters
    /// * a subcircuit: parameters are interpreted as subcircuit parameters
    ///
    /// For the [`CircuitDescription`] to be valid, all parameter names must be valid for the
    /// choosen interpretation.
    pub parameters: ParamDescription,

    /// Names of nodes connected to the terminals of the device
    ///
    /// The number of connected terminals can be smaller then the number of nodes of the devics (but
    /// never larger). The device can decide how to handle this case.
    /// While linear devices (vsource, resistor, isource) will emit an error, Verilog-A devices
    /// usually handle this case using the `$terminal_connected` function.
    pub terminal_connections: Vec<String>,
}

pub struct CircuitModelDescription {
    pub name: String,
    pub device: String,
    pub parameters: ParamDescription,
}

/// A list of `<param>=<value>` pairs specified by the user
pub type ParamDescription = Vec<(String, Expr)>;

impl CircuitDescription {
    /// Creates a circuit descriptor by elaborating the information in the descriptor.
    /// During elaboration the following tasks are performed:
    ///
    /// * compile any Verilog-A models
    /// * resolve any model/subcircuit/device references to their definition
    /// * create implicit models for instances without seperate model definition
    /// * match model/instance parameters to parameter ids provided by device
    /// * for each node name connected to a device terminal create a node
    ///
    /// All these tasks can fail if the user provided an invalid circuit descriptor.
    /// Currently only the first error is retruned using anyhow. In the future all errors should be
    /// reterminaled similar to OpenVAF.
    ///
    /// # Returns
    ///
    /// The information obtained during elaboration inside a [`Circuit`](crate::circuit::Circuit)
    /// if the descriptor is valid.
    ///
    /// If any of the following conditions occurs, an error is retruned instead:
    /// * Verilog-A compilation fails
    /// * A model/subcircuit/device is not found
    pub fn elaborate(self, earena: &mut Arena, opts: &veriloga::Opts) -> Result<Circuit> {
        let mut res = Circuit::new(self.name, earena);
        for va_file in self.va_files {
            res.load_veriloga_file(va_file, opts)?;
        }

        for model in self.models {
            let name = model.name.clone();
            res.elaborate_model(model)
                .with_context(|| format!("while elaborating model '{name}'"))?;
        }

        for inst in self.instances {
            let name = inst.name.clone();
            res.elaborate_instance(inst)
                .with_context(|| format!("while elaborating instance '{name}'"))?;
        }

        Ok(res)
    }
}

impl Circuit {
    /// Creates a circuit model from a [`CircuitDescription`]
    pub fn elaborate_model(&mut self, descr: CircuitModelDescription) -> Result<ModelId> {
        let device = match self.lookup_device(&descr.device) {
            Some(dev) => dev,
            None => bail!("device '{}' not found", descr.device),
        };

        let model = self.new_model(descr.name, device)?;
        for (param_name, val) in descr.parameters {
            self.set_model_param(model, &*param_name, val)?;
        }
        Ok(model)
    }

    fn elaborate_dev_terminals(
        &mut self,
        dev: DeviceId,
        connected_terminals: Vec<String>,
    ) -> Result<Vec<Node>> {
        let dev_info = &self[dev];
        if connected_terminals.len() > dev_info.terminals.len() {
            bail!(
                "device has terminals {} {:?} but {} terminals were connected",
                dev_info.terminals.len(),
                dev_info.terminals,
                connected_terminals.len(),
            );
        }

        Ok(self.elaborate_terminals(connected_terminals))
    }

    fn elaborate_terminals(&mut self, connected_terminals: Vec<String>) -> Vec<Node> {
        connected_terminals.into_iter().map(|terminal| self.node(terminal)).collect()
    }

    pub fn elaborate_instance(
        &mut self,
        instance: CircuitInstanceDescription,
    ) -> Result<InstanceId> {
        let inst = match self.namespace.get(&instance.master).copied() {
            Some(NameSpaceEntry::Model(model)) => {
                let dev = self[model].device;
                let terminals = self.elaborate_dev_terminals(dev, instance.terminal_connections)?;
                let inst = self.new_model_instance(instance.name, model, terminals)?;
                for (param_name, val) in instance.parameters {
                    self.set_instance_param(inst, &*param_name, val)?;
                }
                inst
            }

            Some(NameSpaceEntry::Device(dev)) => {
                let terminals = self.elaborate_dev_terminals(dev, instance.terminal_connections)?;
                let (inst, model) = self.new_device_instance(instance.name, dev, terminals)?;
                for (param_name, val) in instance.parameters {
                    self.set_model_param(model, &*param_name, val)?;
                }
                inst
            }

            Some(NameSpaceEntry::Instance(_)) => {
                bail!(
                    "expected a model, an isntance or a subcircuit but found instance '{}'",
                    instance.master
                )
            }

            None => {
                bail!("'{}' not found", instance.name);
            }
        };

        Ok(inst)
    }
}
