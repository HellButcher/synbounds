//! Helper methods and macros for analyzing and manipulating generic bounds in Rust `proc-macro` crates.
//!
//! This crate provides utilities built on top of the [`syn`] crate to track which generic parameters
//! are referenced in a syntax tree, extract bounded generics, and manipulate lifetime parameters.
//!
//! # Overview
//!
//! The primary type is [`BoundGenerics`], which implements the [`syn::visit::Visit`] trait to track
//! which generic parameters (types, lifetimes, and consts) are referenced in an AST subtree. This is
//! particularly useful when writing derive macros or other procedural macros that need to:
//!
//! - Determine which generic parameters are actually used in a struct or enum
//! - Extract only the relevant where clauses for used parameters
//! - Create minimal generic bounds for generated implementations
//!
//! # Examples
//!
//! ```
//! use syn::{parse_quote, DeriveInput};
//! use syn::visit::Visit;
//! use synbounds::BoundGenerics;
//!
//! // Parse a struct definition
//! let input: DeriveInput = parse_quote! {
//!     struct MyStruct<'a, T, U> {
//!         field: &'a T,
//!     }
//! };
//!
//! // Track which generics are used in the fields
//! let mut bounds = BoundGenerics::new(&input.generics);
//! if let syn::Data::Struct(data) = &input.data {
//!     bounds.visit_fields(&data.fields);
//! }
//!
//! // Only T and 'a are used, U is not
//! let bound_generics = bounds.to_bound_generics();
//! # assert_eq!(bound_generics.params.len(), 2); // 'a and T
//! ```
//!
//! ## Analyzing Impl Blocks
//!
//! ```
//! use syn::{parse_quote, ItemImpl};
//! use syn::visit::Visit;
//! use synbounds::BoundGenerics;
//!
//! // Parse an impl block
//! let item: ItemImpl = parse_quote! {
//!     impl<T, U, V> MyStruct<T> {
//!         fn process(&self, value: U) -> String {
//!             unimplemented!()
//!         }
//!     }
//! };
//!
//! // Track which generics are used in method signatures
//! let mut bounds = BoundGenerics::new(&item.generics);
//! bounds.visit_type(&item.self_ty); // Visit self type
//! for impl_item in &item.items {
//!     if let syn::ImplItem::Fn(method) = impl_item {
//!         bounds.visit_signature(&method.sig);
//!     }
//! }
//!
//! // T and U are used, V is not
//! let bound_generics = bounds.to_bound_generics();
//! # assert_eq!(bound_generics.params.len(), 2);
//! ```
//!
//! ## Substituting Lifetimes
//!
//! The crate also provides utilities for replacing lifetimes in syntax trees (requires the
//! `substitute` feature, enabled by default):
//!
//! ```
//! use syn::{parse_quote, Type};
//! use syn::visit_mut::VisitMut;
//! use synbounds::substitute_with_static_lifetime;
//!
//! // Replace all lifetimes with 'static
//! let mut ty: Type = parse_quote! { &'a Vec<&'b str> };
//! let mut visitor = substitute_with_static_lifetime::<()>();
//! visitor.visit_type_mut(&mut ty);
//! // ty is now: &'static Vec<&'static str>
//! ```
//!
//! You can also substitute with a specific lifetime using [`substitute_with_lifetime`]:
//!
//! ```
//! use syn::{parse_quote, Type, Lifetime};
//! use syn::visit_mut::VisitMut;
//! use synbounds::substitute_with_lifetime;
//!
//! let target_lifetime: Lifetime = parse_quote! { 'target };
//! let mut ty: Type = parse_quote! { &'a String };
//! let mut visitor = substitute_with_lifetime::<()>(&target_lifetime);
//! visitor.visit_type_mut(&mut ty);
//! // ty is now: &'target String
//! ```
//!
//! Or create custom substitution logic with [`SubstituteLifetimes`]:
//!
//! ```
//! use syn::{parse_quote, Type, Lifetime};
//! use syn::visit_mut::VisitMut;
//! use synbounds::SubstituteLifetimes;
//!
//! let mut ty: Type = parse_quote! { &'a String };
//! let mut visitor = SubstituteLifetimes(|lifetime: &mut Lifetime| {
//!     if lifetime.ident == "a" {
//!         *lifetime = Lifetime::new("'long", lifetime.span());
//!     }
//! });
//! visitor.visit_type_mut(&mut ty);
//! // ty is now: &'long String
//! ```
//!
//! ## Substituting `Self` Types
//!
//! When generating wrapper types or helper functions, you often need to replace `Self` with
//! the concrete type. The [`SubstituteSelfType`] visitor handles this:
//!
//! ```
//! use syn::{parse_quote, Type};
//! use syn::visit_mut::VisitMut;
//! use synbounds::SubstituteSelfType;
//!
//! let concrete_type: Type = parse_quote! { MyStruct<T, U> };
//! let mut return_type: Type = parse_quote! { Result<Self, Error> };
//!
//! let mut visitor = SubstituteSelfType::new(&concrete_type);
//! visitor.visit_type_mut(&mut return_type);
//! // return_type is now: Result<MyStruct<T, U>, Error>
//! ```
//!
//! This is especially useful when defining private wrapper objects for functions that use `Self`:
//!
//! ```
//! use syn::{parse_quote, Signature, Type};
//! use syn::visit_mut::VisitMut;
//! use synbounds::SubstituteSelfType;
//!
//! // Original method signature with Self
//! let mut sig: Signature = parse_quote! {
//!     fn process(input: &Self) -> Option<Self>
//! };
//!
//! let concrete: Type = parse_quote! { MyStruct<'a, T> };
//! let mut visitor = SubstituteSelfType::new(&concrete);
//! visitor.visit_signature_mut(&mut sig);
//! // Signature now uses: MyStruct<'a, T> instead of Self
//! ```
//!
//! # Features
//!
//! - `proc-macro` (default): Enable proc-macro support
//! - `full` (optional): Enable full syn feature for additional syntax support
//! - `substitute` (default): Enable lifetime substitution utilities
//!
//! [`syn`]: https://docs.rs/syn/

#![cfg_attr(docsrs, feature(doc_cfg))]

use std::cell::Cell;

use syn::{
    ConstParam, GenericParam, Generics, Ident, Lifetime, LifetimeParam, TypeParam,
    punctuated::Punctuated, visit::Visit,
};

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

/// A [`syn::visit::Visit`] that tracks which generic parameters are bound.
pub struct BoundGenerics<'a> {
    generics: &'a Generics,
    bound: Box<[bool]>,
    where_predicates_dirty: Cell<bool>,
    where_predicates: Box<[Cell<bool>]>,
}

impl<'a> BoundGenerics<'a> {
    /// Create a new `BoundGenerics` for the given `Generics`.
    pub fn new(generics: &'a Generics) -> Self {
        let mut bound = Vec::new();
        bound.resize(generics.params.len(), false);
        let mut where_predicates = Vec::new();
        if let Some(where_clause) = &generics.where_clause {
            where_predicates.resize(where_clause.predicates.len(), Cell::new(false));
        }
        BoundGenerics {
            generics,
            bound: bound.into_boxed_slice(),
            where_predicates_dirty: Cell::new(true),
            where_predicates: where_predicates.into_boxed_slice(),
        }
    }

    fn find_index(&self, param_ref: GenericParamRef<'_>) -> Option<usize> {
        self.generics
            .params
            .iter()
            .position(|p| param_ref.is_param(p))
    }

    fn find_non_lifetime_index_by_ident(&self, ident: &Ident) -> Option<usize> {
        self.generics.params.iter().position(|p| match p {
            GenericParam::Type(type_param) => &type_param.ident == ident,
            GenericParam::Const(const_param) => &const_param.ident == ident,
            GenericParam::Lifetime(_) => false,
        })
    }

    /// Check if the given generic parameter is bound.
    pub fn is_bound(&self, param_ref: GenericParamRef<'_>) -> Option<bool> {
        let index = self.find_index(param_ref)?;
        Some(self.bound[index])
    }

    /// Bind the given generic parameter. Returns `Some(true)` if the parameter was
    /// unbound and is now bound, `Some(false)` if it was already bound,
    /// or `None` if the parameter was not found.
    pub fn bind(&mut self, param_ref: GenericParamRef<'_>) -> Option<bool> {
        if let Some(index) = self.find_index(param_ref) {
            if self.bound[index] {
                Some(false)
            } else {
                self.bound[index] = true;
                self.where_predicates_dirty.set(true);
                match &self.generics.params[index] {
                    GenericParam::Type(param) => {
                        for bound in &param.bounds {
                            self.visit_type_param_bound(bound);
                        }
                    }
                    GenericParam::Lifetime(param) => {
                        for bound in &param.bounds {
                            self.visit_lifetime(bound);
                        }
                    }
                    GenericParam::Const(param) => self.visit_type(&param.ty),
                }
                Some(true)
            }
        } else {
            None
        }
    }

    /// Bind a generic type parameter by its identifier.
    /// For more details, see [`Self::bind`].
    #[inline]
    pub fn bind_type(&mut self, ident: &Ident) -> Option<bool> {
        self.bind(GenericParamRef::Type(ident))
    }

    /// Bind a generic const parameter by its identifier.
    /// For more details, see [`Self::bind`].
    #[inline]
    pub fn bind_const(&mut self, ident: &Ident) -> Option<bool> {
        self.bind(GenericParamRef::Const(ident))
    }

    /// Bind a generic lifetime parameter by its lifetime.
    /// For more details, see [`Self::bind`].
    #[inline]
    pub fn bind_lifetime(&mut self, lifetime: &Lifetime) -> Option<bool> {
        self.bind(GenericParamRef::Lifetime(&lifetime.ident))
    }

    /// Iterate over all bound generic parameters.
    pub fn bound_params(&self) -> impl Iterator<Item = &GenericParam> {
        self.generics
            .params
            .iter()
            .zip(self.bound.iter().copied())
            .filter_map(
                |(param, is_bound)| {
                    if is_bound { Some(param) } else { None }
                },
            )
    }

    /// Iterate over all bound type parameters.
    pub fn bound_type_params(&self) -> impl Iterator<Item = &TypeParam> {
        self.bound_params().filter_map(|param| {
            if let GenericParam::Type(type_param) = param {
                Some(type_param)
            } else {
                None
            }
        })
    }

    /// Iterate over all bound lifetime parameters.
    pub fn bound_lifetimes(&self) -> impl Iterator<Item = &LifetimeParam> {
        self.bound_params().filter_map(|param| {
            if let GenericParam::Lifetime(lifetime_param) = param {
                Some(lifetime_param)
            } else {
                None
            }
        })
    }

    /// Iterate over all bound const parameters.
    pub fn bound_const_params(&self) -> impl Iterator<Item = &ConstParam> {
        self.bound_params().filter_map(|param| {
            if let GenericParam::Const(const_param) = param {
                Some(const_param)
            } else {
                None
            }
        })
    }

    fn update_where_predicates(&self) {
        if !self.where_predicates_dirty.get() {
            return;
        }
        if let Some(where_clause) = &self.generics.where_clause {
            for (predicate, is_included) in where_clause
                .predicates
                .iter()
                .zip(self.where_predicates.iter())
            {
                if is_included.get() {
                    continue;
                } else {
                    let mut test = self.test_contains();
                    test.visit_where_predicate(predicate);
                    if !test.contains_unbound() {
                        is_included.set(true);
                    }
                }
            }
        }
        self.where_predicates_dirty.set(false);
    }

    /// Iterate over all "bound" where predicates.
    ///
    /// A "bound" where predicate is a predicate that doesn't refers to unbound generic parameters.
    pub fn bound_where_predicates(&self) -> impl Iterator<Item = &syn::WherePredicate> {
        self.update_where_predicates();
        self.generics
            .where_clause
            .iter()
            .flat_map(|where_clause| where_clause.predicates.iter())
            .zip(self.where_predicates.iter())
            .filter_map(|(predicate, is_included)| {
                if is_included.get() {
                    Some(predicate)
                } else {
                    None
                }
            })
    }

    /// Create a `Generics` containing only the bound generic parameters and
    /// where predicates.
    pub fn to_bound_generics(&self) -> Generics {
        let params: Punctuated<syn::GenericParam, _> = self.bound_params().cloned().collect();
        let where_clause = self
            .generics
            .where_clause
            .as_ref()
            .map(|clause| syn::WhereClause {
                where_token: clause.where_token,
                predicates: self.bound_where_predicates().cloned().collect(),
            });
        if params.is_empty() {
            Generics {
                lt_token: None,
                params,
                gt_token: None,
                where_clause,
            }
        } else {
            Generics {
                lt_token: self.generics.lt_token,
                params,
                gt_token: self.generics.gt_token,
                where_clause,
            }
        }
    }

    /// Create a `ContainsBoundGenerics` visitor to test whether any of the given generic parameters are contained and whether they are bound or unbound.
    pub fn test_contains(&self) -> ContainsBoundGenerics<'_> {
        ContainsBoundGenerics {
            bounds: self,
            contains_bound: false,
            contains_unbound: false,
        }
    }
}

fn visit_path_arguments<'a, V: ?Sized + Visit<'a>>(visitor: &mut V, path: &'a syn::Path) {
    for segment in &path.segments {
        visitor.visit_path_arguments(&segment.arguments);
    }
}

impl syn::visit::Visit<'_> for BoundGenerics<'_> {
    fn visit_lifetime(&mut self, node: &syn::Lifetime) {
        self.bind_lifetime(node);
    }

    fn visit_ident(&mut self, node: &Ident) {
        // bind type or const generic param
        let Some(index) = self.find_non_lifetime_index_by_ident(node) else {
            return;
        };
        match &self.generics.params[index] {
            GenericParam::Type(_) => {
                self.bind_type(node);
            }
            GenericParam::Const(_) => {
                self.bind_const(node);
            }
            GenericParam::Lifetime(_) => {} // do nothing, lifetimes are handled in visit_lifetime
        }
    }

    fn visit_type_path(&mut self, node: &syn::TypePath) {
        if let Some(qself) = &node.qself {
            // can not be a generic param (only walk down the QSelf)
            self.visit_qself(qself);
        } else if let Some(ident) = node.path.get_ident() {
            // maybe a generic param
            self.bind_type(ident);
            return;
        }
        // only look at the path arguments
        visit_path_arguments(self, &node.path);
    }

    fn visit_expr_path(&mut self, node: &syn::ExprPath) {
        if let Some(qself) = &node.qself {
            // can not be a generic param (only walk down the QSelf)
            self.visit_qself(qself);
        } else if let Some(ident) = node.path.get_ident() {
            // maybe a generic param
            self.bind_const(ident);
            return;
        }
        // only look at the path arguments
        visit_path_arguments(self, &node.path);
    }

    fn visit_generic_param(&mut self, _node: &'_ syn::GenericParam) {
        // don't walk down the generic parameters
    }

    fn visit_type_macro(&mut self, x: &syn::TypeMacro) {
        // bail out: we don't know what is inside the macro
        // assume all generic params are bound
        for r in &mut self.bound {
            *r = true;
        }
        syn::visit::visit_type_macro(self, x);
    }
}

/// A [`syn::visit::Visit`] that tests whether any of the given generic parameters are contained and whether they are bound or unbound.
pub struct ContainsBoundGenerics<'a> {
    bounds: &'a BoundGenerics<'a>,
    contains_bound: bool,
    contains_unbound: bool,
}

impl ContainsBoundGenerics<'_> {
    /// Result that indicates whether any of the given generic parameters were found.
    #[inline]
    pub fn contains(&self) -> bool {
        self.contains_bound || self.contains_unbound
    }

    /// Result that indicates whether any of the given generic parameters were bound.
    #[inline]
    pub fn contains_bound(&self) -> bool {
        self.contains_bound
    }

    /// Result that indicates whether any of the given generic parameters were unbound.
    #[inline]
    pub fn contains_unbound(&self) -> bool {
        self.contains_unbound
    }
}

impl syn::visit::Visit<'_> for ContainsBoundGenerics<'_> {
    fn visit_lifetime(&mut self, node: &syn::Lifetime) {
        if let Some(is_bound) = self.bounds.is_bound(GenericParamRef::Lifetime(&node.ident)) {
            if is_bound {
                self.contains_bound = true;
            } else {
                self.contains_unbound = true;
            }
        }
    }

    fn visit_ident(&mut self, node: &Ident) {
        // bind type or const generic param
        let Some(index) = self.bounds.find_non_lifetime_index_by_ident(node) else {
            return;
        };
        if self.bounds.bound[index] {
            self.contains_bound = true;
        } else {
            self.contains_unbound = true;
        }
    }

    fn visit_type_path(&mut self, node: &syn::TypePath) {
        if let Some(qself) = &node.qself {
            // can not be a generic param (only walk down the QSelf)
            self.visit_qself(qself);
        } else if let Some(ident) = node.path.get_ident() {
            // maybe a generic param
            if let Some(is_bound) = self.bounds.is_bound(GenericParamRef::Type(ident)) {
                if is_bound {
                    self.contains_bound = true;
                } else {
                    self.contains_unbound = true;
                }
            }
            return;
        }
        // only look at the path arguments
        visit_path_arguments(self, &node.path);
    }

    fn visit_expr_path(&mut self, node: &syn::ExprPath) {
        if let Some(qself) = &node.qself {
            // can not be a generic param (only walk down the QSelf)
            self.visit_qself(qself);
        } else if let Some(ident) = node.path.get_ident() {
            // maybe a generic param
            if let Some(is_bound) = self.bounds.is_bound(GenericParamRef::Const(ident)) {
                if is_bound {
                    self.contains_bound = true;
                } else {
                    self.contains_unbound = true;
                }
            }
            return;
        }
        // only look at the path arguments
        visit_path_arguments(self, &node.path);
    }
}

/// A [`syn::visit_mut::VisitMut`] that substitutes lifetimes according to the provided function.
#[cfg(feature = "substitute")]
#[cfg_attr(docsrs, doc(cfg(feature = "substitute")))]
#[derive(Copy, Clone)]
pub struct SubstituteLifetimes<F: ?Sized>(pub F);

#[cfg(feature = "substitute")]
impl<F: Fn(&mut Lifetime) + ?Sized> syn::visit_mut::VisitMut for SubstituteLifetimes<F> {
    fn visit_lifetime_mut(&mut self, node: &mut Lifetime) {
        (self.0)(node);
    }
}

/// Creates a [`SubstituteLifetimes`] visitor that substitutes all non-`'static` lifetimes with `'static`.
#[cfg(feature = "substitute")]
#[cfg_attr(docsrs, doc(cfg(feature = "substitute")))]
pub const fn substitute_with_static_lifetime<T>() -> SubstituteLifetimes<impl Fn(&mut Lifetime)> {
    SubstituteLifetimes(|lifetime: &mut Lifetime| {
        if lifetime.ident != "static" {
            *lifetime = Lifetime::new("'static", lifetime.span());
        }
    })
}

/// Creates a [`SubstituteLifetimes`] visitor that substitutes all non-`'static` lifetimes with the provided lifetime.
#[cfg(feature = "substitute")]
#[cfg_attr(docsrs, doc(cfg(feature = "substitute")))]
pub const fn substitute_with_lifetime<T>(
    new_lifetime: &Lifetime,
) -> SubstituteLifetimes<impl Fn(&mut Lifetime)> {
    SubstituteLifetimes(|lifetime: &mut Lifetime| {
        if lifetime.ident != "static" && lifetime.ident != new_lifetime.ident {
            *lifetime = new_lifetime.clone();
        }
    })
}

/// A [`syn::visit_mut::VisitMut`] that substitutes the `Self` type with a concrete type.
///
/// This is particularly useful when generating wrapper types or helper functions that need
/// to use the concrete type instead of `Self`.
///
/// # Example
///
/// ```
/// use syn::{parse_quote, Type};
/// use syn::visit_mut::VisitMut;
/// use synbounds::SubstituteSelfType;
///
/// let concrete_type: Type = parse_quote! { MyStruct<T> };
/// let mut return_type: Type = parse_quote! { Option<Self> };
///
/// let mut visitor = SubstituteSelfType::new(&concrete_type);
/// visitor.visit_type_mut(&mut return_type);
/// // return_type is now: Option<MyStruct<T>>
/// ```
#[cfg(feature = "substitute")]
#[cfg_attr(docsrs, doc(cfg(feature = "substitute")))]
#[derive(Clone)]
pub struct SubstituteSelfType<'a> {
    concrete_type: &'a syn::Type,
}

#[cfg(feature = "substitute")]
impl<'a> SubstituteSelfType<'a> {
    /// Create a new `SubstituteSelfType` visitor that will replace `Self` with the given concrete type.
    pub const fn new(concrete_type: &'a syn::Type) -> Self {
        SubstituteSelfType { concrete_type }
    }
}

#[cfg(feature = "substitute")]
impl syn::visit_mut::VisitMut for SubstituteSelfType<'_> {
    fn visit_type_mut(&mut self, node: &mut syn::Type) {
        // Check if this is a Self type
        if let syn::Type::Path(type_path) = node
            && type_path.qself.is_none()
            && type_path.path.is_ident("Self")
        {
            *node = self.concrete_type.clone();
            return;
        }
        // Continue visiting nested types
        syn::visit_mut::visit_type_mut(self, node);
    }
}

/// A reference to a generic parameter: type, lifetime, or const.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenericParamRef<'a> {
    Type(&'a Ident),
    Lifetime(&'a Ident),
    Const(&'a Ident),
}

impl GenericParamRef<'_> {
    /// Get the identifier of the generic parameter.
    pub fn ident(&self) -> &Ident {
        match self {
            GenericParamRef::Type(ident) => ident,
            GenericParamRef::Const(ident) => ident,
            GenericParamRef::Lifetime(ident) => ident,
        }
    }

    /// Check if this generic parameter reference is a type parameter with the given identifier.
    pub fn is_type(&self, ident: &Ident) -> bool {
        matches!(self, GenericParamRef::Type(id) if *id == ident)
    }

    /// Check if this generic parameter reference is a const parameter with the given identifier.
    pub fn is_const(&self, ident: &Ident) -> bool {
        matches!(self, GenericParamRef::Const(id) if *id == ident)
    }

    /// Check if this generic parameter reference is a lifetime parameter with the given lifetime.
    pub fn is_lifetime(&self, lifetime: &Lifetime) -> bool {
        matches!(self, GenericParamRef::Lifetime(id) if *id == &lifetime.ident)
    }

    /// Check if this generic parameter reference matches the given generic parameter.
    pub fn is_param(&self, param: &syn::GenericParam) -> bool {
        match (self, param) {
            (GenericParamRef::Type(ident), GenericParam::Type(ty_param)) => {
                *ident == &ty_param.ident
            }
            (GenericParamRef::Const(ident), GenericParam::Const(const_param)) => {
                *ident == &const_param.ident
            }
            (GenericParamRef::Lifetime(ident), GenericParam::Lifetime(lifetime_param)) => {
                *ident == &lifetime_param.lifetime.ident
            }
            _ => false,
        }
    }

    /// Check if this generic parameter reference matches the given captured parameter.
    #[cfg(feature = "full")]
    #[cfg_attr(docsrs, doc(cfg(feature = "full")))]
    pub fn is_captured(&self, param: &syn::CapturedParam) -> bool {
        match (self, param) {
            (GenericParamRef::Type(ident), syn::CapturedParam::Ident(captured_ident)) => {
                *ident == captured_ident
            }
            (GenericParamRef::Const(ident), syn::CapturedParam::Ident(captured_ident)) => {
                *ident == captured_ident
            }
            (GenericParamRef::Lifetime(ident), syn::CapturedParam::Lifetime(captured_lifetime)) => {
                *ident == &captured_lifetime.ident
            }
            _ => false,
        }
    }
}

impl<'a> From<&'a Ident> for GenericParamRef<'a> {
    #[inline]
    fn from(ident: &'a Ident) -> Self {
        GenericParamRef::Type(ident)
    }
}

impl<'a> From<&'a Lifetime> for GenericParamRef<'a> {
    #[inline]
    fn from(lifetime: &'a Lifetime) -> Self {
        GenericParamRef::Lifetime(&lifetime.ident)
    }
}

impl<'a> From<&'a TypeParam> for GenericParamRef<'a> {
    #[inline]
    fn from(param: &'a TypeParam) -> Self {
        GenericParamRef::Type(&param.ident)
    }
}

impl<'a> From<&'a ConstParam> for GenericParamRef<'a> {
    #[inline]
    fn from(param: &'a ConstParam) -> Self {
        GenericParamRef::Const(&param.ident)
    }
}

impl<'a> From<&'a LifetimeParam> for GenericParamRef<'a> {
    #[inline]
    fn from(param: &'a LifetimeParam) -> Self {
        GenericParamRef::Lifetime(&param.lifetime.ident)
    }
}

impl PartialEq<syn::Ident> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::Ident) -> bool {
        self.ident() == other
    }
}

impl PartialEq<syn::Lifetime> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::Lifetime) -> bool {
        self.is_lifetime(other)
    }
}

impl PartialEq<syn::GenericParam> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::GenericParam) -> bool {
        self.is_param(other)
    }
}

impl PartialEq<syn::TypeParam> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::TypeParam) -> bool {
        self.is_type(&other.ident)
    }
}

impl PartialEq<syn::ConstParam> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::ConstParam) -> bool {
        self.is_const(&other.ident)
    }
}

impl PartialEq<syn::LifetimeParam> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::LifetimeParam) -> bool {
        self.is_lifetime(&other.lifetime)
    }
}

#[cfg(feature = "full")]
impl PartialEq<syn::CapturedParam> for GenericParamRef<'_> {
    #[inline]
    fn eq(&self, other: &syn::CapturedParam) -> bool {
        self.is_captured(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::{DeriveInput, Fields, parse_quote};

    #[test]
    fn test_bound_generics_simple_struct() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T> {
                field: T,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // T should be bound
        let type_param = &input.generics.params[0];
        if let GenericParam::Type(tp) = type_param {
            assert!(bounds.is_bound(GenericParamRef::Type(&tp.ident)).unwrap());
        }
    }

    #[test]
    fn test_bound_generics_unused_param() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U> {
                field: T,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // T should be bound, U should not
        assert_eq!(bounds.bound_params().count(), 1);
        let bound_gen = bounds.to_bound_generics();
        assert_eq!(bound_gen.params.len(), 1);
    }

    #[test]
    fn test_bound_generics_with_lifetime() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<'a, T> {
                field: &'a T,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // Both 'a and T should be bound
        assert_eq!(bounds.bound_params().count(), 2);
        assert_eq!(bounds.bound_lifetimes().count(), 1);
        assert_eq!(bounds.bound_type_params().count(), 1);
    }

    #[test]
    fn test_bound_generics_const_param() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<const N: usize> {
                field: [u8; N],
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // N should be bound
        assert_eq!(bounds.bound_const_params().count(), 1);
    }

    #[test]
    fn test_bound_generics_nested_types() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U> {
                field: Vec<T>,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // Only T should be bound
        assert_eq!(bounds.bound_type_params().count(), 1);
    }

    #[test]
    fn test_bound_generics_with_where_clause() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U>
            where
                T: Clone,
                U: Default,
            {
                field: T,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // Only T's where predicate should be included
        let where_preds: Vec<_> = bounds.bound_where_predicates().collect();
        assert_eq!(where_preds.len(), 1);
    }

    #[test]
    fn test_bind_type() {
        let generics: Generics = parse_quote! { <T, U> };
        let mut bounds = BoundGenerics::new(&generics);

        let t_ident: Ident = parse_quote! { T };

        // First bind returns true
        assert_eq!(bounds.bind_type(&t_ident), Some(true));
        // Second bind returns false (already bound)
        assert_eq!(bounds.bind_type(&t_ident), Some(false));

        // Non-existent param returns None
        let x_ident: Ident = parse_quote! { X };
        assert_eq!(bounds.bind_type(&x_ident), None);
    }

    #[test]
    fn test_bind_lifetime() {
        let generics: Generics = parse_quote! { <'a, 'b> };
        let mut bounds = BoundGenerics::new(&generics);

        let a_lifetime: Lifetime = parse_quote! { 'a };

        // First bind returns true
        assert_eq!(bounds.bind_lifetime(&a_lifetime), Some(true));
        // Second bind returns false (already bound)
        assert_eq!(bounds.bind_lifetime(&a_lifetime), Some(false));
    }

    #[test]
    fn test_bind_const() {
        let generics: Generics = parse_quote! { <const N: usize> };
        let mut bounds = BoundGenerics::new(&generics);

        let n_ident: Ident = parse_quote! { N };

        assert_eq!(bounds.bind_const(&n_ident), Some(true));
        assert_eq!(bounds.bind_const(&n_ident), Some(false));
    }

    #[test]
    fn test_contains_bound_generics() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U> {
                field1: T,
                field2: String,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data
            && let Fields::Named(fields) = &data.fields
        {
            // Visit field1
            let field1 = &fields.named[0];
            bounds.visit_field(field1);
        }

        // Now test contains
        let ty: syn::Type = parse_quote! { T };
        let mut test = bounds.test_contains();
        test.visit_type(&ty);
        assert!(test.contains());
        assert!(test.contains_bound());
        assert!(!test.contains_unbound());

        // Test unbound
        let ty_u: syn::Type = parse_quote! { U };
        let mut test_u = bounds.test_contains();
        test_u.visit_type(&ty_u);
        assert!(test_u.contains());
        assert!(!test_u.contains_bound());
        assert!(test_u.contains_unbound());
    }

    #[test]
    fn test_generic_param_ref_equality() {
        let ident_t: Ident = parse_quote! { T };
        let ident_u: Ident = parse_quote! { U };
        let lifetime_a: Lifetime = parse_quote! { 'a };

        let type_ref = GenericParamRef::Type(&ident_t);
        let const_ref = GenericParamRef::Const(&ident_t);
        let lifetime_ref = GenericParamRef::Lifetime(&lifetime_a.ident);

        assert!(type_ref.is_type(&ident_t));
        assert!(!type_ref.is_type(&ident_u));
        assert!(const_ref.is_const(&ident_t));
        assert!(lifetime_ref.is_lifetime(&lifetime_a));

        assert_eq!(type_ref.ident(), &ident_t);
        assert_eq!(const_ref.ident(), &ident_t);
    }

    #[test]
    fn test_generic_param_ref_from_conversions() {
        let type_param: TypeParam = parse_quote! { T };
        let const_param: ConstParam = parse_quote! { const N: usize };
        let lifetime_param: LifetimeParam = parse_quote! { 'a };

        let type_ref = GenericParamRef::from(&type_param);
        let const_ref = GenericParamRef::from(&const_param);
        let lifetime_ref = GenericParamRef::from(&lifetime_param);

        assert!(matches!(type_ref, GenericParamRef::Type(_)));
        assert!(matches!(const_ref, GenericParamRef::Const(_)));
        assert!(matches!(lifetime_ref, GenericParamRef::Lifetime(_)));
    }

    #[test]
    fn test_bound_generics_with_generic_param_bounds() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T: Clone, U> {
                field: T,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // T should be bound, and its Clone bound should trigger any nested generics
        assert_eq!(bounds.bound_type_params().count(), 1);
    }

    #[test]
    fn test_bound_generics_type_macro() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U> {
                field: some_macro!(T, U, X),
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // With type macros, we conservatively assume all params are bound
        assert_eq!(bounds.bound_params().count(), 2);
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_with_static_lifetime() {
        use syn::visit_mut::VisitMut;

        let mut ty: syn::Type = parse_quote! { &'a String };
        let mut visitor = substitute_with_static_lifetime::<()>();
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { &'static String };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_with_lifetime() {
        use syn::visit_mut::VisitMut;

        let new_lifetime: Lifetime = parse_quote! { 'b };
        let mut ty: syn::Type = parse_quote! { &'a String };
        let mut visitor = substitute_with_lifetime::<()>(&new_lifetime);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { &'b String };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_preserves_static() {
        use syn::visit_mut::VisitMut;

        let new_lifetime: Lifetime = parse_quote! { 'a };
        let mut ty: syn::Type = parse_quote! { &'static String };
        let mut visitor = substitute_with_lifetime::<()>(&new_lifetime);
        visitor.visit_type_mut(&mut ty);

        // 'static should be preserved
        let expected: syn::Type = parse_quote! { &'static String };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_simple() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct };
        let mut ty: syn::Type = parse_quote! { Self };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { MyStruct };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_in_generic() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct<T> };
        let mut ty: syn::Type = parse_quote! { Option<Self> };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { Option<MyStruct<T>> };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_in_reference() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct };
        let mut ty: syn::Type = parse_quote! { &'a Self };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { &'a MyStruct };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_nested() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct<T, U> };
        let mut ty: syn::Type = parse_quote! { Vec<Box<Self>> };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { Vec<Box<MyStruct<T, U>>> };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_in_tuple() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct };
        let mut ty: syn::Type = parse_quote! { (Self, String, Self) };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { (MyStruct, String, MyStruct) };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_in_fn_signature() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct<T> };
        let mut sig: syn::Signature = parse_quote! {
            fn process(input: Self) -> Result<Self, Error>
        };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_signature_mut(&mut sig);

        let expected: syn::Signature = parse_quote! {
            fn process(input: MyStruct<T>) -> Result<MyStruct<T>, Error>
        };
        assert_eq!(
            quote::quote!(#sig).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_preserves_other_types() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct };
        let mut ty: syn::Type = parse_quote! { HashMap<String, Vec<i32>> };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        // Should remain unchanged
        let expected: syn::Type = parse_quote! { HashMap<String, Vec<i32>> };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[cfg(feature = "substitute")]
    #[test]
    fn test_substitute_self_with_lifetime_in_concrete() {
        use syn::visit_mut::VisitMut;

        let concrete: syn::Type = parse_quote! { MyStruct<'a, T> };
        let mut ty: syn::Type = parse_quote! { &Self };
        let mut visitor = SubstituteSelfType::new(&concrete);
        visitor.visit_type_mut(&mut ty);

        let expected: syn::Type = parse_quote! { &MyStruct<'a, T> };
        assert_eq!(
            quote::quote!(#ty).to_string(),
            quote::quote!(#expected).to_string()
        );
    }

    #[test]
    fn test_complex_generic_scenario() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<'a, 'b, T, U, const N: usize>
            where
                T: Clone + 'a,
                U: Default,
            {
                field1: &'a T,
                field2: [u8; N],
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // 'a, T, and N should be bound; 'b and U should not
        assert_eq!(bounds.bound_lifetimes().count(), 1);
        assert_eq!(bounds.bound_type_params().count(), 1);
        assert_eq!(bounds.bound_const_params().count(), 1);
        assert_eq!(bounds.bound_params().count(), 3);

        // Only T's where predicate should be included
        let where_preds: Vec<_> = bounds.bound_where_predicates().collect();
        assert_eq!(where_preds.len(), 1);

        let result_generics = bounds.to_bound_generics();
        assert_eq!(result_generics.params.len(), 3);
    }

    #[test]
    fn test_qself_handling() {
        let input: DeriveInput = parse_quote! {
            struct MyStruct<T, U> {
                field: <T as SomeTrait>::AssocType,
            }
        };

        let mut bounds = BoundGenerics::new(&input.generics);
        if let syn::Data::Struct(data) = &input.data {
            bounds.visit_fields(&data.fields);
        }

        // T should be bound (from QSelf)
        assert_eq!(bounds.bound_type_params().count(), 1);
    }

    #[test]
    fn test_empty_generics() {
        let generics: Generics = parse_quote! {};
        let bounds = BoundGenerics::new(&generics);

        assert_eq!(bounds.bound_params().count(), 0);
        let result = bounds.to_bound_generics();
        assert!(result.params.is_empty());
        assert!(result.lt_token.is_none());
        assert!(result.gt_token.is_none());
    }

    #[test]
    fn test_impl_block_method_return_type() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyStruct<T, U> {
                fn get_t(&self) -> T {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit just the method signature
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            bounds.visit_signature(&method.sig);
        }

        // Only T should be bound (used in return type)
        assert_eq!(bounds.bound_type_params().count(), 1);
        let bound_gen = bounds.to_bound_generics();
        assert_eq!(bound_gen.params.len(), 1);
    }

    #[test]
    fn test_impl_block_method_parameters() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U, V> MyStruct<T, U, V> {
                fn process(&self, value: U) -> String {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            bounds.visit_signature(&method.sig);
        }

        // Only U should be bound (used in parameter)
        assert_eq!(bounds.bound_type_params().count(), 1);
        let bound_params: Vec<_> = bounds.bound_type_params().collect();
        assert_eq!(bound_params[0].ident, "U");
    }

    #[test]
    fn test_impl_block_multiple_methods() {
        let item: syn::ItemImpl = parse_quote! {
            impl<'a, T, U, V> MyStruct<'a, T, U, V> {
                fn get_t(&self) -> &'a T {
                    unimplemented!()
                }

                fn set_u(&mut self, value: U) {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit all method signatures
        for impl_item in &item.items {
            if let syn::ImplItem::Fn(method) = impl_item {
                bounds.visit_signature(&method.sig);
            }
        }

        // 'a, T, and U should be bound; V should not
        assert_eq!(bounds.bound_params().count(), 3);
        assert_eq!(bounds.bound_lifetimes().count(), 1);
        assert_eq!(bounds.bound_type_params().count(), 2);
    }

    #[test]
    fn test_impl_block_method_with_where_clause() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyStruct<T, U>
            where
                T: Clone,
                U: Default,
            {
                fn clone_t(&self) -> T
                where
                    T: Clone,
                {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            bounds.visit_signature(&method.sig);
        }

        // Only T should be bound
        assert_eq!(bounds.bound_type_params().count(), 1);

        // Only T's where predicate should be included
        let where_preds: Vec<_> = bounds.bound_where_predicates().collect();
        assert_eq!(where_preds.len(), 1);
    }

    #[test]
    fn test_impl_block_method_body() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyStruct<T, U> {
                fn create_default() -> U
                where
                    U: Default,
                {
                    U::default()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            // Visit signature and body
            bounds.visit_impl_item_fn(method);
        }

        // U should be bound (return type and used in body)
        assert_eq!(bounds.bound_type_params().count(), 1);
        let bound_params: Vec<_> = bounds.bound_type_params().collect();
        assert_eq!(bound_params[0].ident, "U");
    }

    #[test]
    fn test_impl_block_with_const_generics() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, const N: usize> MyStruct<T, N> {
                fn get_array(&self) -> [T; N] {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            bounds.visit_signature(&method.sig);
        }

        // Both T and N should be bound
        assert_eq!(bounds.bound_type_params().count(), 1);
        assert_eq!(bounds.bound_const_params().count(), 1);
    }

    #[test]
    fn test_impl_block_generic_method() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T> MyStruct<T> {
                fn convert<U>(&self, value: U) -> T {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        if let Some(syn::ImplItem::Fn(method)) = item.items.first() {
            // Visit only the signature (not method's own generics)
            bounds.visit_signature(&method.sig);
        }

        // T should be bound (return type), but not U (that's method-local)
        assert_eq!(bounds.bound_type_params().count(), 1);
        let bound_params: Vec<_> = bounds.bound_type_params().collect();
        assert_eq!(bound_params[0].ident, "T");
    }

    #[test]
    fn test_impl_block_self_type() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyTrait for MyStruct<T> {
                fn process(&self) -> Self {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit the self type
        bounds.visit_type(&item.self_ty);

        // Only T should be bound (U is unused)
        assert_eq!(bounds.bound_type_params().count(), 1);
    }

    #[test]
    fn test_impl_block_trait_bounds() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyTrait<U> for MyStruct<T> {
                fn get(&self) -> U {
                    unimplemented!()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit trait and self type
        if let Some((_, ref trait_path, _)) = item.trait_ {
            bounds.visit_path(trait_path);
        }
        bounds.visit_type(&item.self_ty);

        // Both T and U should be bound
        assert_eq!(bounds.bound_type_params().count(), 2);
    }

    #[test]
    fn test_impl_block_associated_type() {
        let item: syn::ItemImpl = parse_quote! {
            impl<T, U> MyTrait for MyStruct<T> {
                type Output = U;
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit self type and impl items
        bounds.visit_type(&item.self_ty);
        for impl_item in &item.items {
            bounds.visit_impl_item(impl_item);
        }

        // Both T (from self type) and U (from associated type) should be bound
        assert_eq!(bounds.bound_type_params().count(), 2);
    }

    #[test]
    fn test_impl_block_complex_scenario() {
        let item: syn::ItemImpl = parse_quote! {
            impl<'a, 'b, T, U, V, const N: usize> MyStruct<'a, T, N>
            where
                T: Clone + 'a,
                U: Default,
                V: Debug,
            {
                fn get_data(&self) -> &'a [T; N] {
                    unimplemented!()
                }

                fn create_u() -> U
                where
                    U: Default,
                {
                    U::default()
                }
            }
        };

        let mut bounds = BoundGenerics::new(&item.generics);
        // Visit self type and all methods
        bounds.visit_type(&item.self_ty);
        for impl_item in &item.items {
            if let syn::ImplItem::Fn(method) = impl_item {
                bounds.visit_signature(&method.sig);
            }
        }

        // 'a, T, U, and N should be bound; 'b and V should not
        assert_eq!(bounds.bound_lifetimes().count(), 1);
        assert_eq!(bounds.bound_type_params().count(), 2);
        assert_eq!(bounds.bound_const_params().count(), 1);
        assert_eq!(bounds.bound_params().count(), 4);

        // Only T and U where predicates should be included
        let where_preds: Vec<_> = bounds.bound_where_predicates().collect();
        assert_eq!(where_preds.len(), 2);
    }
}
