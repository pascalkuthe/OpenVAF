//! Generated by `generate_integration_tests`, do not edit by hand.

use std::fs::read_to_string;

use expect_test::expect_file;
use sourcegen::{project_root, skip_slow_tests};

use crate::db::HirDefDB;
use crate::tests::TestDataBase;

#[test]
fn bsim3() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim3.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIM3").join("bsim3.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsim3.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIM3").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsim4() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim4.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIM4").join("bsim4.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsim4.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIM4").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsim6() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim6.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIM6").join("bsim6.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsim6.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIM6").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsimbulk() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimbulk.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIMBULK").join("bsimbulk.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimbulk.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIMBULK").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsimcmg() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimcmg.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg.va", &file_contents);
    }
    {
        let path = project_root()
            .join("integration_tests")
            .join("BSIMCMG")
            .join("bsimcmg_binning_parameters.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg_binning_parameters.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_body.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg_body.include", &file_contents);
    }
    {
        let path = project_root()
            .join("integration_tests")
            .join("BSIMCMG")
            .join("bsimcmg_cfringe.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg_cfringe.include", &file_contents);
    }
    {
        let path = project_root()
            .join("integration_tests")
            .join("BSIMCMG")
            .join("bsimcmg_quasi_static_cv.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg_quasi_static_cv.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_rdsmod.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimcmg_rdsmod.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMCMG").join("common_defs.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/common_defs.include", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIMCMG").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsimimg() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimimg.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIMIMG").join("bsimimg.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimimg.va", &file_contents);
    }
    {
        let path = project_root()
            .join("integration_tests")
            .join("BSIMIMG")
            .join("bsimimg_binning.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimimg_binning.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMIMG").join("bsimimg_body.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimimg_body.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMIMG").join("bsimimg_sp.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimimg_sp.include", &file_contents);
    }
    {
        let path =
            project_root().join("integration_tests").join("BSIMIMG").join("common_defs.include");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/common_defs.include", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIMIMG").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn bsimsoi() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimsoi.va", "");
    {
        let path = project_root().join("integration_tests").join("BSIMSOI").join("bsimsoi.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/bsimsoi.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("BSIMSOI").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn diode() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/diode.va", "");
    {
        let path = project_root().join("integration_tests").join("DIODE").join("diode.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/diode.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("DIODE").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn ekv() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/ekv.va", "");
    {
        let path = project_root().join("integration_tests").join("EKV").join("ekv.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/ekv.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("EKV").join("def_map.txt")]
        .assert_eq(&actual);
}
#[test]
fn hicuml2() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/hicuml2.va", "");
    {
        let path = project_root().join("integration_tests").join("HICUML2").join("hicuml2.va");
        let file_contents = read_to_string(path).unwrap();
        db.vfs().write().add_virt_file("/hicuml2.va", &file_contents);
    }
    let diagnostics = db.lower_and_check();
    assert_eq!(&diagnostics, "");
    let def_map = db.def_map(db.root_file());
    let actual = def_map.dump(&db);
    expect_file![project_root().join("integration_tests").join("HICUML2").join("def_map.txt")]
        .assert_eq(&actual);
}
