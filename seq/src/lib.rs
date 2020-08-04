extern crate proc_macro;

use syn::*;
use syn::parse::Parse;
use quote::*;

use proc_macro::TokenStream;

#[derive(Debug)]
struct BodyWithRepetition {
    prefix: proc_macro2::TokenStream,
    repetition: proc_macro2::TokenStream,
    suffix: proc_macro2::TokenStream,
}

#[derive(Debug)]
enum Body {
    WithoutRepetition(proc_macro2::TokenStream),
    WithRepetition(BodyWithRepetition),
}

fn parse_body(content: &parse::ParseBuffer) -> Result<Body> {
    enum State {
        Initial,
        FoundHash,
        FoundGroup,
    }
    let state = State::Initial;
    let mut body_with_repetition: Option<BodyWithRepetition> = None;
    let content = proc_macro2::TokenStream::parse(&content)?;
    content.clone().into_iter().for_each(|token| {
        match &token {
            proc_macro2::TokenTree::Group(group) => {
            },

            _ => {},
        }
    });
    if let Some(body_with_repetition) = body_with_repetition {
        Ok(Body::WithRepetition(body_with_repetition))
    } else {
        Ok(Body::WithoutRepetition(content))
    }
}

#[derive(Debug)]
struct Sequence {
    ident: Ident,
    body: Body,
    range_start: i32,
    range_end: i32,
}

impl Sequence {
    fn expand(
        &self,
        input: proc_macro2::TokenStream,
        loop_var: i32,
    ) -> proc_macro2::TokenStream {
        let mut prefix: Option<proc_macro2::Ident> = None;
        let mut compound_ident = false;
        let mut tokens = proc_macro2::TokenStream::new();
        input.into_iter().for_each(|token| {
            match &token {
                proc_macro2::TokenTree::Group(group) => {
                    if let Some(ident) = prefix.take() {
                        tokens.append(proc_macro2::TokenTree::Ident(ident));
                    };
                    let mut expansion = proc_macro2::Group::new(
                        group.delimiter(),
                        self.expand(group.stream(), loop_var)
                    );
                    expansion.set_span(group.span());
                    tokens.append(proc_macro2::TokenTree::Group(expansion));
                },

                proc_macro2::TokenTree::Ident(ident)
                    if (*ident == self.ident) && (prefix.is_none())
                => {
                    tokens.append(proc_macro2::TokenTree::Literal(
                        proc_macro2::Literal::i32_unsuffixed(loop_var)
                    ));
                },

                proc_macro2::TokenTree::Ident(ident)
                    if (*ident == self.ident) && compound_ident
                => {
                    let token = format_ident!(
                        "{}{}",
                        prefix.take().unwrap().to_string(),
                        loop_var.to_string()
                    );
                    tokens.append(proc_macro2::TokenTree::Ident(token));
                },

                proc_macro2::TokenTree::Ident(ident) => {
                    if let Some(ident) = prefix.take() {
                        tokens.append(proc_macro2::TokenTree::Ident(ident));
                    };
                    prefix = Some(ident.clone());
                    compound_ident = false;
                },

                proc_macro2::TokenTree::Punct(punct)
                    if (!compound_ident) && (punct.as_char() == '#')
                => {
                    compound_ident = true;
                },

                _ if prefix.is_some() => {
                    if compound_ident {
                        panic!("Expected loop variable after '#' in this context");
                    }
                    if let Some(ident) = prefix.take() {
                        tokens.append(proc_macro2::TokenTree::Ident(ident));
                    };
                    tokens.append(token);
                },

                _ => tokens.append(token),
            }
        });
        quote! {
            #tokens
        }
    }
}

impl Parse for Sequence {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let ident = Ident::parse(input)?;
        let _in_token = <Token![in]>::parse(input)?;
        let range_start = LitInt::parse(input)?.base10_parse::<i32>()?;
        let _range_delimiter = <Token![..]>::parse(input)?;
        let range_end = LitInt::parse(input)?.base10_parse::<i32>()?;
        let content;
        let _braces = braced!(content in input);
        Ok(Sequence{
            ident,
            body: parse_body(&content)?,
            range_start,
            range_end,
        })
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    // let input = dbg!(input);
    let sequence = parse_macro_input!(input as Sequence);
    match &sequence.body {
        Body::WithoutRepetition(body) => {
            let expansion = (sequence.range_start..sequence.range_end)
                .map(|loop_var| {
                    sequence.expand(body.clone(), loop_var)
                });
            quote!{ #( #expansion )* }
        },

        Body::WithRepetition(body) => {
            let prefix = &body.prefix;
            let suffix = &body.suffix;
            let expansion = (sequence.range_start..sequence.range_end)
                .map(|loop_var| {
                    sequence.expand(body.repetition.clone(), loop_var)
                });
            quote!{
                #prefix
                #( #expansion )*
                #suffix
            }
        },
    }.into()
}
