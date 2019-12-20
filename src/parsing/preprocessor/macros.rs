use pest::iterators::Pairs;

use super::*;
use crate::error::{error, error_message};

impl<'lt> Preprocessor<'lt> {
    pub(super) fn process_declaration(&mut self, node: ParseTreeNode<'lt>) -> PreprocessorResult {
        trace!("Processing macro declaration from: {:?}", node);
        let span = node.as_span();
        let mut description: Pairs<'lt, Rule> = node.into_inner();
        let name = identifier_string(description.next().unwrap());
        let mut args: Vec<String> = Vec::new();
        //If args are declared process teh,
        if_rule!(let Some(identifier_list) = description.next() where Rule::IDENTIFIER_LIST => {
            for identifier in identifier_list.into_inner() {
                args.push(identifier_string(identifier));
            }
        });
        let text = description.next();

        debug!("Macro {} with args {:?} declared", name, args);
        trace! {"With body: {:?}", text}
        if self.macros.insert(name, MACRO { args, text }).is_some() {
            warn!(
                "{}",
                error_message::<Rule>("Maro is being overwritten here", span)
            )
        }
        Ok(())
    }

    pub(super) fn process_reference(&mut self, node: ParseTreeNode<'lt>) -> PreprocessorResult {
        trace!("Processing macro reference from: {:?}", node);
        let span = node.as_span();
        let mut description: Pairs<Rule> = node.into_inner();

        let name = identifier_string(description.next().unwrap());
        if self.calling_macros.contains(&name) {
            return error(
                &format!(
                    "MACRO {} WAS CALLED FROM INSIDE ITSELF; MACRO RECURSION IS FORBIDDEN!",
                    name
                ),
                span,
            );
        }
        self.calling_macros.push(name.clone()); //macro is being processed it may not be called now

        let macro_definition = if let Some(found) = self.macros.get(&name) {
            found.clone()
        } else {
            return error(&format!("Macro {} not defined here!", name), span);
        };

        debug!(
            "Macro {} with args {:?} referenced",
            name, macro_definition.args
        );

        //Bind macro arguments
        let mut arg_bindings: HashMap<String, String> = HashMap::new();
        if let Some(args_list) = description.next() {
            let mut arg_definitions = macro_definition.args.iter();

            //Check that the right amount of args was specified
            let arguments_found = args_list.clone().into_inner().count();
            let arguments_expected = macro_definition.args.len();
            if arguments_expected != arguments_found {
                return error(
                    &format!(
                        "Expected {} arguments for the Macro {} found: {}",
                        arguments_expected, name, arguments_found
                    ),
                    args_list.as_span(),
                );
            }
            //Bind  args
            for arg in args_list.into_inner() {
                let arg_name = as_string!(arg_definitions.next().unwrap());
                let mut arg_content = String::from("");
                //Each macro argument is treated like a line in a referenced macro (macros may also be called and current arguments also have to be replaced=
                mem::swap(&mut self.preprocessed_source, &mut arg_content);
                self.process_macro_body_line(arg)?;
                mem::swap(&mut self.preprocessed_source, &mut arg_content);
                arg_bindings.insert(arg_name, arg_content);
            }

            debug!("Arg Bindings {:?}", arg_bindings);

            if macro_definition.text.is_none() {
                debug!("Empty macro {}", name);
                return Ok(());
            }
        } else if !macro_definition.args.is_empty() {
            return error(
                &format!(
                    "Found no arguments for the Macro {} expected: {} ",
                    name,
                    macro_definition.args.len()
                ),
                span,
            );
        }

        //according to standard the macro body needs to be processed when its referenced and not when its declared
        //to ensure the right references are used. Thats done here
        mem::swap(&mut self.calling_macro_arguments, &mut arg_bindings); //arg bindings will be empty if function expects nothing
        let macro_body = macro_definition.text.clone().unwrap();
        for line in macro_body.into_inner() {
            self.process_macro_body_line(line)?;
            self.preprocessed_source.push('\n');
        }
        self.preprocessed_source.pop(); //avoid adding unnecessary newlines
        mem::swap(&mut self.calling_macro_arguments, &mut arg_bindings);
        self.calling_macros.pop(); //this macro is done now it may be called again
        Ok(())
    }

    fn process_macro_body_line(&mut self, arg: ParseTreeNode<'lt>) -> PreprocessorResult {
        for arg_token in arg.into_inner() {
            match arg_token.as_rule() {
                Rule::SIMPLE_IDENTIFIER => {
                    if self
                        .calling_macro_arguments
                        .contains_key(arg_token.as_str())
                    {
                        self.preprocessed_source.push_str(
                            self.calling_macro_arguments
                                .get(arg_token.as_str())
                                .unwrap(),
                        );
                    } else {
                        self.preprocessed_source.push_str(arg_token.as_str());
                    }
                }
                Rule::COMPILER_DIRECTIVE => self.process_compiler_directive(arg_token)?,
                Rule::MEANINGFUL_CHAR => self.preprocessed_source.push_str(arg_token.as_str()),
                Rule::MACRO_ARGUMENT => self.process_macro_body_line(arg_token)?,
                _ => unexpected_rule!(arg_token),
            }
        }
        Ok(())
    }

    pub(super) fn process_macro_condition(
        &mut self,
        node: ParseTreeNode<'lt>,
    ) -> PreprocessorResult {
        trace!("Processing macro_condition from: {:?}", node);
        let mut description = node.into_inner();
        let condition_type = description.next().unwrap();
        let if_ndef = match condition_type.as_rule() {
            Rule::TOK_IFDEF => false,
            Rule::TOK_IFNDEF => true,
            _ => unexpected_rule!(condition_type),
        };
        debug!("Evaluating {}", if if_ndef { "if_ndef" } else { "if_def" });
        if self.evaluate_condition(
            if_ndef,
            description.next().unwrap(),
            description.next().unwrap(),
        )? {
            return Ok(());
        }
        while let Some(current) = description.next() {
            match current.as_rule() {
                Rule::IDENTIFIER => {
                    debug!("Processing ELSEIF");
                    if self.evaluate_condition(if_ndef, current, description.next().unwrap())? {
                        return Ok(());
                    }
                }
                Rule::BODY => {
                    debug!("PROCESSING ELSE");
                    return self.process_parsed_source(current);
                }
                _ => unexpected_rule!(current),
            }
        }
        debug!("no condition was satisfied and no else block was defined");
        Ok(())
    }

    fn evaluate_condition(
        &mut self,
        if_ndef: bool,
        ident_node: ParseTreeNode<'lt>,
        body: ParseTreeNode<'lt>,
    ) -> PreprocessorResult<bool> {
        let identifier = identifier_string(ident_node);
        debug!("for {}", identifier);
        if self.macros.contains_key(&identifier) && !if_ndef
            || !self.macros.contains_key(&identifier) && if_ndef
        {
            debug!("Its true!");
            self.process_parsed_source(body)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }
}
