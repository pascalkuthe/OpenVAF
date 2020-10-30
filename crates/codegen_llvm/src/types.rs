/*
 * ******************************************************************************************
 * Copyright (c) 2020 Pascal Kuthe. This file is part of the frontend project.
 * It is subject to the license terms in the LICENSE file found in the top-level directory
 *  of this distribution and at  https://gitlab.com/DSPOM/OpenVAF/blob/master/LICENSE.
 *  No part of frontend, including this file, may be copied, modified, propagated, or
 *  distributed except according to the terms contained in the LICENSE file.
 * *****************************************************************************************
 */

use crate::{CallTypeCodeGen, CfgCodegen, LlvmCodegen};
use inkwell::types::{ArrayType, BasicType, BasicTypeEnum, FloatType, IntType, PointerType};
use inkwell::values::{ArrayValue, BasicValue, BasicValueEnum, FloatValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use openvaf_middle::osdi_types::ConstVal::{Array, Scalar};
use openvaf_middle::osdi_types::SimpleConstVal::{Bool, Integer, Real, String};
use openvaf_middle::SimpleConstVal;
use openvaf_middle::{CallType, ConstVal, Local, SimpleType, Type};

pub trait ArrayCreation<'ctx>: Copy {
    type Value: 'ctx;
    fn const_array(self, values: &[Self::Value]) -> ArrayValue<'ctx>;
}

impl<'ctx> ArrayCreation<'ctx> for IntType<'ctx> {
    type Value = IntValue<'ctx>;

    fn const_array(self, values: &[Self::Value]) -> ArrayValue<'ctx> {
        IntType::const_array(self, values)
    }
}

impl<'ctx> ArrayCreation<'ctx> for FloatType<'ctx> {
    type Value = FloatValue<'ctx>;

    fn const_array(self, values: &[Self::Value]) -> ArrayValue<'ctx> {
        FloatType::const_array(self, values)
    }
}

impl<'ctx> ArrayCreation<'ctx> for PointerType<'ctx> {
    type Value = PointerValue<'ctx>;

    fn const_array(self, values: &[Self::Value]) -> ArrayValue<'ctx> {
        PointerType::const_array(self, values)
    }
}

impl<'ctx> ArrayCreation<'ctx> for ArrayType<'ctx> {
    type Value = ArrayValue<'ctx>;

    fn const_array(self, values: &[Self::Value]) -> ArrayValue<'ctx> {
        ArrayType::const_array(self, values)
    }
}

pub fn array<'c, T: ArrayCreation<'c> + BasicType<'c>>(
    elements: &[T::Value],
    ty: T,
    dimensions: &[u32],
) -> ArrayValue<'c> {
    let mut dimensions = dimensions.iter().rev().copied();
    let base_len = dimensions.next().unwrap();
    let data: Vec<_> = elements
        .chunks(base_len as usize)
        .map(|x| ty.const_array(x))
        .collect();
    let ty = ty.array_type(base_len);
    let (data, _) = dimensions.fold((data, ty), |(data, ty), len| {
        let data = data
            .chunks(len as usize)
            .map(|x| ty.const_array(x))
            .collect();
        let ty = ty.array_type(len);
        (data, ty)
    });
    debug_assert_eq!(data.len(), 1);
    data[0]
}

impl<'a, 'c, A: CallType> LlvmCodegen<'a, 'c, A> {
    pub fn ty(&self, ty: Type) -> BasicTypeEnum<'c> {
        ty.with_info(|info| {
            let element = match info.element {
                SimpleType::Integer => self.integer_ty().as_basic_type_enum(),
                SimpleType::Real => self.real_ty().as_basic_type_enum(),
                SimpleType::String => self.string_ty().as_basic_type_enum(),
                SimpleType::Bool => self.bool_ty().as_basic_type_enum(),
            };
            if info.dimensions.is_empty() {
                element
            } else {
                self.array_ty(element, &info.dimensions)
                    .as_basic_type_enum()
            }
        })
    }

    pub fn integer_ty(&self) -> IntType<'c> {
        self.context.i64_type()
    }

    pub fn bool_ty(&self) -> IntType<'c> {
        self.context.bool_type()
    }

    pub fn real_ty(&self) -> FloatType<'c> {
        self.context.f64_type()
    }

    pub fn string_ty(&self) -> PointerType<'c> {
        self.context.i8_type().ptr_type(AddressSpace::Generic)
    }

    pub fn ptr_sized_int_ty(&self, address_space: Option<AddressSpace>) -> IntType<'c> {
        self.context.ptr_sized_int_type(&self.target, address_space)
    }

    pub fn array_ty(&self, element: BasicTypeEnum<'c>, dimensions: &[u32]) -> ArrayType<'c> {
        let mut iter = dimensions.iter().copied();
        let mut ty = element.array_type(iter.next().unwrap());
        for len in dimensions {
            ty = ty.array_type(*len)
        }
        ty
    }

    pub fn constant(&mut self, val: &ConstVal) -> BasicValueEnum<'c> {
        match val {
            Scalar(val) => self.simple_constant(*val),
            Array(data, ty) => {
                let array = ty.with_info(|info| {
                    debug_assert_eq!(data.len(), info.size() as usize);

                    match info.element {
                        SimpleType::Integer => {
                            let data: Vec<_> = data
                                .iter()
                                .map(|val| {
                                    if let SimpleConstVal::Integer(val) = val {
                                        self.integer_ty().const_int(*val as u64, true)
                                    } else {
                                        unreachable!("Malformed constant")
                                    }
                                })
                                .collect();
                            array(&data, self.integer_ty(), &info.dimensions)
                        }

                        SimpleType::Real => {
                            let data: Vec<_> = data
                                .iter()
                                .map(|val| {
                                    if let SimpleConstVal::Real(val) = val {
                                        self.real_ty().const_float(*val)
                                    } else {
                                        unreachable!("Malformed constant")
                                    }
                                })
                                .collect();
                            array(&data, self.real_ty(), &info.dimensions)
                        }

                        SimpleType::String => {
                            let data: Vec<_> = data
                                .iter()
                                .map(|val| {
                                    if let SimpleConstVal::String(val) = val {
                                        self.str_literal(*val)
                                    } else {
                                        unreachable!("Malformed constant")
                                    }
                                })
                                .collect();
                            array(&data, self.string_ty(), &info.dimensions)
                        }

                        SimpleType::Bool => {
                            let data: Vec<_> = data
                                .iter()
                                .map(|val| {
                                    if let SimpleConstVal::Bool(val) = val {
                                        self.bool_ty().const_int(*val as u64, false)
                                    } else {
                                        unreachable!("Malformed constant")
                                    }
                                })
                                .collect();
                            array(&data, self.bool_ty(), &info.dimensions)
                        }
                    }
                });

                array.as_basic_value_enum()
            }
        }
    }

    pub fn simple_constant(&mut self, val: SimpleConstVal) -> BasicValueEnum<'c> {
        match val {
            Integer(val) => self
                .integer_ty()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
            Real(val) => self.real_ty().const_float(val).as_basic_value_enum(),
            String(val) => self.str_literal(val).as_basic_value_enum(),
            Bool(val) => self
                .bool_ty()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
        }
    }
}

impl<'lt, 'a, 'c, D, A: CallType, C: CallTypeCodeGen<'lt, 'c>> CfgCodegen<'lt, 'a, 'c, D, A, C> {
    pub fn local_ty(&self, local: Local) -> BasicTypeEnum<'c> {
        let ty = self.cfg.locals[local].ty;
        self.ctx.ty(ty)
    }
}
