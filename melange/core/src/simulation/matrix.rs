use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;
use std::slice;

use klu_rs::{FixedKluMatrix, KluMatrixBuilder, KluMatrixSpec, KluSettings};
use num_complex::Complex64;
use typed_index_collections::TiSlice;

use crate::circuit::{InstanceId, Node};
use crate::devices::MatrixEntry;
use crate::Circuit;

pub type RealMatrix = FixedKluMatrix<i32, f64>;
pub type ComplexMatrix = FixedKluMatrix<i32, Complex64>;
pub type MatrixSpec = Rc<KluMatrixSpec<i32>>;

pub(super) struct SimulationMatrix {
    spec: MatrixSpec,
    pub nonlinear_matrix: RealMatrix,
    pub ac_matrix: ComplexMatrix,
}

impl SimulationMatrix {
    pub fn new_or_reset(
        cache: Option<SimulationMatrix>,
        builder: &MatrixBuilder,
    ) -> SimulationMatrix {
        match cache {
            Some(matrix) => matrix.reset(builder),
            None => SimulationMatrix::new(builder),
        }
    }

    fn new(builder: &MatrixBuilder) -> SimulationMatrix {
        let spec = builder.inner.finish(KluSettings::new());
        let nonlinear_matrix = RealMatrix::new(spec.clone()).expect("non empty matrix");
        let ac_matrix = ComplexMatrix::new(spec.clone()).expect("non empty matrix");
        SimulationMatrix { spec, nonlinear_matrix, ac_matrix }
    }

    fn reset(mut self, builder: &MatrixBuilder) -> SimulationMatrix {
        let nonlinear_matrix_alloc = self.nonlinear_matrix.into_alloc();
        let ac_matrix_alloc = self.ac_matrix.into_alloc();
        let spec = Rc::get_mut(&mut self.spec).expect("matrix spec is only borrowed by matricies");
        builder.inner.reinit(spec);
        let nonlinear_matrix =
            RealMatrix::new_with_alloc(self.spec.clone(), nonlinear_matrix_alloc)
                .expect("matrix is not empty");
        let ac_matrix = ComplexMatrix::new_with_alloc(self.spec.clone(), ac_matrix_alloc)
            .expect("matrix is not empty");
        SimulationMatrix { spec: self.spec, nonlinear_matrix, ac_matrix }
    }
}

pub(crate) struct MatrixBuilder {
    inner: KluMatrixBuilder<i32>,
    pub instance_entries: Box<TiSlice<InstanceId, Vec<(Node, Node)>>>,

    /// gnd is fixed to zero volt (otherwise the system is over specified).
    /// All gnd matrix entries are therefore not needed.
    /// To not insert conditions everywhere they are written into a dump value
    dump: NonNull<Cell<f64>>,
}

impl Drop for MatrixBuilder {
    fn drop(&mut self) {
        unsafe { drop(Box::from_raw(self.dump.as_ptr())) }
    }
}

impl MatrixBuilder {
    pub fn new(circ: &Circuit) -> MatrixBuilder {
        let instance_entries =
            vec![Vec::with_capacity(16); circ.num_instances() as usize].into_boxed_slice().into();
        MatrixBuilder {
            inner: KluMatrixBuilder::new(circ.num_unkowns() as i32),
            instance_entries,
            dump: Box::leak(Box::new(Cell::new(0f64))).into(),
        }
    }

    pub fn insert(&mut self, instance: InstanceId, column: Node, row: Node) {
        if column == Node::GROUND || row == Node::GROUND {
            self.instance_entries[instance].push((Node::GROUND, Node::GROUND))
        } else {
            self.instance_entries[instance].push((column, row));
            self.inner.add_entry(row.matrix_idx(), column.matrix_idx());
        }
    }

    pub fn reset(&mut self, circ: &Circuit) {
        self.inner.reset(circ.num_unkowns() as i32);
        for instance_entries in &mut *self.instance_entries {
            instance_entries.clear()
        }
    }

    pub fn clear_instance(&mut self, instance: InstanceId) {
        self.instance_entries[instance].clear()
    }
}

pub struct MatrixEntryIter<'a> {
    iter: slice::Iter<'a, (Node, Node)>,
    nonlinear_matrix: &'a RealMatrix,
    ac_matrix: &'a ComplexMatrix,
    dump: &'a Cell<f64>,
}

impl<'a> MatrixEntryIter<'a> {
    pub(super) fn new(
        matrix: &'a SimulationMatrix,
        builder: &'a MatrixBuilder,
        inst: InstanceId,
    ) -> MatrixEntryIter<'a> {
        MatrixEntryIter {
            iter: builder.instance_entries[inst].iter(),
            nonlinear_matrix: &matrix.nonlinear_matrix,
            ac_matrix: &matrix.ac_matrix,
            dump: unsafe { builder.dump.as_ref() },
        }
    }
}

impl<'a> Iterator for MatrixEntryIter<'a> {
    type Item = MatrixEntry<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|&(col, row)| {
            if row == Node::GROUND {
                return MatrixEntry { resist: self.dump, react: self.dump };
            }
            let resist = &self.nonlinear_matrix[(row.matrix_idx(), col.matrix_idx())];
            let complex = &self.ac_matrix[(row.matrix_idx(), col.matrix_idx())]
                as *const Cell<Complex64> as *const Cell<[f64; 2]>;

            // this is save because the original cell is still valid and because Complex64 is
            // garunteed to be layout compatible with [f64;2]
            // FIXME us as_array_of_cells instead when it becomes stable
            // SAFETY: `Cell<T>` has the same memory layout as `T`.
            let complex = unsafe { &*(complex as *const [Cell<f64>; 2]) };
            let react = &complex[1];
            MatrixEntry { resist, react }
        })
    }
}
