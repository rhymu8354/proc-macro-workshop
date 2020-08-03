extern crate proc_macro;

use proc_macro::TokenStream;
use syn::*;
use quote::*;

fn is_wrapper<'a>(ty: &'a Type, wrapper: &'static str) -> Option<&'a Type> {
    if let Type::Path(type_path) = ty {
        if let Some(type_path_first_segment) = type_path.path.segments.first() {
            if type_path_first_segment.ident == wrapper {
                if let PathArguments::AngleBracketed(angle_bracketed_generic_args) = &type_path_first_segment.arguments {
                    if let Some(GenericArgument::Type(ty)) = angle_bracketed_generic_args.args.first() {
                        return Some(ty);
                    }
                }
            }
        }
    }
    None
}

fn is_option(ty: &Type) -> Option<&Type> {
    is_wrapper(ty, "Option")
}

fn is_vec(ty: &Type) -> Option<&Type> {
    is_wrapper(ty, "Vec")
}

fn has_each_attribute(field: &Field) -> Option<(String, &Type)> {
    for attr in &field.attrs {
        if let Meta::List(meta) = attr.parse_meta().unwrap() {
            if let Some(segment) = meta.path.segments.first() {
                if segment.ident == "builder" {
                    if let NestedMeta::Meta(Meta::NameValue(nested)) = meta.nested.first().unwrap() {
                        if &nested.path.segments.first().unwrap().ident.to_string() != "each" {
                            panic!("expected `builder(each = \"...\")`");
                        }
                        if let Some(inner_type) = is_vec(&field.ty) {
                            if let Lit::Str(each_setter_name) = &nested.lit {
                                return Some((each_setter_name.value(), inner_type));
                            } else {
                                panic!("String literal expected for 'each setter' name");
                            }
                        } else {
                            panic!("An 'each setter' requires the field to be a Vec<T>");
                        }
                    } else {
                        panic!("expected argument for builder attribute");
                    }
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = parse(input).unwrap();

    let name: &Ident = &ast.ident;

    let fields = match ast.data {
        Data::Struct(data_struct) => {
            match data_struct.fields {
                Fields::Named(FieldsNamed{named, .. }) => {
                    named
                },
                _ => panic!("Must have named fields!"),
            }
        },
        _ => panic!("Not a structure!"),
    };

    let builder_name = format_ident!("{}Builder", name);

    let values = fields.iter().map(|field| {
        let field_name = format_ident!("{}", field.ident.as_ref().unwrap());
        let field_type = &field.ty;
        if is_option(field_type).is_some() {
            quote! {
                #field_name: #field_type,
            }
        } else {
            quote! {
                #field_name: Option<#field_type>,
            }
        }
    });

    let setters = fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;
        if let Some(optional_type) = is_option(field_type) {
            quote! {
                fn #field_name(&mut self, value: #optional_type) -> &mut Self {
                    self.#field_name = Some(value);
                    self
                }
            }
        } else {
            let mut include_full_setter = true;

            let each_setter = if let Some((setter_name, item_type)) = has_each_attribute(&field) {
                let setter_name = format_ident!("{}", setter_name);
                // TODO: Look at the following line deeper to understand it better.
                if *field_name.to_string() == *setter_name.to_string() {
                    include_full_setter = false;
                }
                quote! {
                    fn #setter_name(&mut self, value: #item_type) -> &mut Self {
                        if self.#field_name.is_none() {
                            self.#field_name = Some(Vec::new());
                        }
                        self.#field_name.as_mut().unwrap().push(value);
                        self
                    }
                }
            } else {
                quote! {}
            };

            let full_setter = if include_full_setter {
                quote! {
                    fn #field_name(&mut self, value: #field_type) -> &mut Self {
                        self.#field_name = Some(value);
                        self
                    }
                }
            } else {
                quote! {}
            };

            quote! {
                #each_setter
                #full_setter
            }
        }
    });

    let builders = fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let field_type = &field.ty;
        if is_option(field_type).is_some() {
            quote! {
                #field_name: self.#field_name.take(),
            }
        } else {
            quote! {
                #field_name: self.#field_name.take().ok_or_else(|| {
                    format!("{} is not set!", stringify!(#field_name))
                })?,
            }
        }
    });

    let value_initializers = fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        if let Some((_, _)) = has_each_attribute(&field) {
            quote! {
                #field_name: Some(Vec::new()),
            }
        } else {
            quote! {
                #field_name: None,
            }
        }
    });

    let gen = quote! {
        struct #builder_name {
            #( #values )*
        }

        impl #builder_name {
            #( #setters )*

            fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name {
                    #( #builders )*
                })
            }
        }

        impl #name {
            fn builder() -> #builder_name {
                #builder_name {
                    #( #value_initializers )*
                }
            }
        }
    };
    gen.into()
}
