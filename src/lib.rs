use std::cell::Cell;

use syn::{
    ConstParam, GenericParam, Generics, Ident, Lifetime, LifetimeParam, TypeParam,
    punctuated::Punctuated, visit::Visit,
};

#[cfg(feature = "proc-macro")]
extern crate proc_macro;

/// A [`syn::Visit`] that tracks which generic parameters are bound.
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
        let params: Punctuated<syn::GenericParam, _> = self
            .generics
            .params
            .iter()
            .zip(self.bound.iter().copied())
            .filter_map(|(param, is_bound)| if is_bound { Some(param.clone()) } else { None })
            .collect();
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
        }
        // don't walk down further (cant be a generic param)
    }

    fn visit_expr_path(&mut self, node: &syn::ExprPath) {
        if let Some(qself) = &node.qself {
            // can not be a generic param (only walk down the QSelf)
            self.visit_qself(qself);
        } else if let Some(ident) = node.path.get_ident() {
            // maybe a generic param
            self.bind_const(ident);
        }
        // don't walk down further (cant be a generic param)
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

/// A [`syn::Visit`] that tests whether any of the given generic parameters are contained and whether they are bound or unbound.
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
        }
        // don't walk down further (cant be a generic param)
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
        }
        // don't walk down further (cant be a generic param)
    }
}

/// A [`syn::VisitMut`] that substitutes lifetimes according to the provided function.
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
