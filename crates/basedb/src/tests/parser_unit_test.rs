use stdx::iter::zip;
use syntax::ast::{BranchKind, Item, ModuleItem, ModulePortKind};

use crate::tests::TestDataBase;

pub const BRANCH: &str = r#"
module test (output a,input b);
    branch (a,b) ab1,ab2;
    branch (a) a_gnd;
    branch (<$root.test.b>) port_flow_b;
endmodule
"#;

#[test]
fn branches() {
    let parse = TestDataBase::new("/branch.va", BRANCH).parse_and_check();
    let tree = parse.tree();
    let module = if let Some(Item::ModuleDecl(module)) = tree.items().next() {
        module
    } else {
        unreachable!()
    };
    let branches: Vec<_> = module
        .module_items()
        .map(
            |branch| {
                if let ModuleItem::BranchDecl(branch) = branch {
                    branch
                } else {
                    unreachable!()
                }
            },
        )
        .collect();

    assert_eq!(branches[0].names().map(|it| it.text().to_owned()).collect::<Vec<_>>(), &["ab1", "ab2"]);

    if let BranchKind::Nodes(hi, lo) =
        branches[0].branch_kind().expect("decl 1 was not parsed correctly")
    {
        assert_eq!(hi.as_raw_ident().expect("decl 1 node 1 is not an identifier").text(), "a");
        assert_eq!(lo.as_raw_ident().expect("decl 1 node 2 is not an identifier").text(), "b");
    } else {
        unreachable!(
            "assert_failed! expected BranchKind::Nodes(_,_) {:?}",
            branches[0].branch_kind().unwrap()
        )
    }

    assert_eq!(branches[1].names().map(|it| it.text().to_owned()).collect::<Vec<_>>(), &["a_gnd"]);

    if let BranchKind::NodeGnd(node) =
        branches[1].branch_kind().expect("decl 1 was not parsed correctly")
    {
        assert_eq!(node.as_raw_ident().expect("decl 2 node is not an identifier").text(), "a");
    } else {
        unreachable!(
            "assert_failed! expected BranchKind::NodeGnd(_) {:?}",
            branches[0].branch_kind().unwrap()
        )
    }

    assert_eq!(branches[2].names().map(|it| it.text().to_owned()).collect::<Vec<_>>(), &["port_flow_b"]);

    if let BranchKind::PortFlow(node) =
        branches[2].branch_kind().expect("decl 1 was not parsed correctly")
    {
        assert_eq!(
            node.port()
                .expect("port is missing from port flow")
                .segments()
                .map(|seg| seg.syntax.text().to_owned())
                .collect::<Vec<_>>(),
            &["$root", "test", "b"]
        );
    } else {
        unreachable!(
            "assert_failed! expected BranchKind::PortFlow {:?}",
            branches[0].branch_kind().unwrap()
        )
    }
}

pub const CONTRIUBTE: &str = r#"
module test;
    analog begin
        I(x)<+(V(x)-V0)**2/(R1R2);
    end
endmodule
"#;

#[test]
fn contribute() {
    TestDataBase::new("/contribute.va", CONTRIUBTE).parse_and_check();
}

pub const MODULE_HEADER: &str = r#"
module test1;
endmodule

module test2 (a,b,c,d,e,f,g);
    output a;
    input b;
    inout wire c;
    inout electrical d;
    output electrical e,f;
    inout electrical wire g;
endmodule

module test3 (output a,b, input electrical c,d,e, (*foo*) inout electrical wire f);
endmodule
"#;

#[test]
fn header() {
    let parse = TestDataBase::new("/net_declarations.va", MODULE_HEADER).parse_and_check();
    let tree = parse.tree();

    let modules: Vec<_> = tree
        .items()
        .map(|item| if let Item::ModuleDecl(m) = item { m } else { unreachable!("other item") })
        .collect();

    assert_eq!(modules.len(), 3);

    for (found, expected) in zip(modules[1].ports(), "abcdefg".chars()) {
        if let ModulePortKind::Name(name) = found.kind() {
            assert_eq!(name.text(), format!("{}", expected));
        } else {
            unreachable!(
                "all head ports in the second module are just names but {:?} was found",
                found
            );
        }
    }

    let mut body_ports = Vec::new();

    for (found, expected) in zip(modules[1].module_items(), &["a", "b", "c", "d", "ef", "g"]) {
        if let ModuleItem::BodyPortDecl(decl) = found {
            let decl = decl.port_decl().unwrap();
            let names: Vec<_> = decl.names().map(|n| n.text().to_owned()).collect();
            let expected: Vec<String> = expected.chars().map(|c| c.into()).collect();
            assert_eq!(names, expected);
            body_ports.push(decl);
        } else {
            unreachable!(
                "all module items second module are port declarations but {:?} was found",
                found
            );
        }
    }

    assert_eq!(
        body_ports[2].net_type_token().map(|t| t.text().to_owned()),
        Some("wire".to_owned())
    );
    assert_eq!(
        body_ports[5].net_type_token().map(|t| t.text().to_owned()),
        Some("wire".to_owned())
    );

    assert_eq!(
        body_ports[3].discipline().map(|t| t.text().to_owned()),
        Some("electrical".to_owned())
    );
    assert_eq!(
        body_ports[4].discipline().map(|t| t.text().to_owned()),
        Some("electrical".to_owned())
    );

    // TODO also check lest module
}

pub const NET_DECLARATION: &str = r#"
module test ();
    wire x,y;
    electrical z;
    wire electrical l;

endmodule
"#;

#[test]
fn net_decl() {
    TestDataBase::new("/contribute.va", NET_DECLARATION).parse_and_check();
}

pub const STATEMENTS: &str = r#"
`include "disciplines.va"


`define branches\
    branch (A,x) ax;\
    branch (A,y) ay;\
    branch (x,B) xb;\
    branch (y,B) yb;\
    branch (x,y) xy;

`define calculate(a,b)\
    b**a/2;\
    C = C**2;


module schaltung (A,B);
    inout electrical A,B;
    electrical x,y;
    `branches
    real C = 30;

    analog begin
        if (V(ax) > 100) begin
            if (V(ax) > 100) begin
                C=42*31+1;
            end
                        if (V(ax) > 100) begin
                            C=42*31+1;
                        end else             if (V(ax) > 100) begin
                                                 C=42*31+1;
                                             end else begin
                                             end
        end
        I(xy) <+ (C*V(ax)+I(xy))/30;
        // I(ax) <+ `calculate(31,V(ax))
        I(ay) <+ V(ay)/20;
        if (V(ax) > 100) begin
            C=42*31+1;
        end
        I(ax) <+ C == 0 ? V(ax)/40 : 42;
        $stprobe("foo",c,"bar");
        ; // Empty statements are allowed by the standard
        ;
        I(ay) <+ C*V(ay)/50;
    end
endmodule
"#;

#[test]
fn statements() {
    TestDataBase::new("/statements.va", STATEMENTS).parse_and_check();
}

pub const VARIABLE_DECLARATION: &str = r#"
module test ();
    real x=1.0;
    integer y=0,z;
    time t;
    realtime rt;
    analog x = 2==y;
endmodule
"#;

#[test]
fn var_declarations() {
    TestDataBase::new("/var_declarations.va", VARIABLE_DECLARATION).parse_and_check();
}
