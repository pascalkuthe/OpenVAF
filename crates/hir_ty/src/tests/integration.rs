//! Generated by `generate_integration_tests`, do not edit by hand.

use std::fs::read;

use expect_test::expect_file;
use sourcegen::{project_root, skip_slow_tests};

use crate::tests::TestDataBase;

#[test]
fn asmhemt() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/asmhemt.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("ASMHEMT").join("asmhemt.va");
    vfs.add_virt_file("/asmhemt.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("ASMHEMT")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsim3() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim3.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIM3").join("bsim3.va");
    vfs.add_virt_file("/bsim3.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIM3")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsim4() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim4.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIM4").join("bsim4.va");
    vfs.add_virt_file("/bsim4.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIM4")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsim6() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsim6.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIM6").join("bsim6.va");
    vfs.add_virt_file("/bsim6.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIM6")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsimbulk() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimbulk.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIMBULK").join("bsimbulk.va");
    vfs.add_virt_file("/bsimbulk.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIMBULK")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsimcmg() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimcmg.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg.va");
    vfs.add_virt_file("/bsimcmg.va", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_body.include");
    vfs.add_virt_file("/bsimcmg_body.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_checking.include");
    vfs.add_virt_file("/bsimcmg_checking.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("BSIMCMG")
        .join("bsimcmg_initialization.include");
    vfs.add_virt_file("/bsimcmg_initialization.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_macros.include");
    vfs.add_virt_file("/bsimcmg_macros.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_noise.include");
    vfs.add_virt_file("/bsimcmg_noise.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_parameters.include");
    vfs.add_virt_file("/bsimcmg_parameters.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimcmg_variables.include");
    vfs.add_virt_file("/bsimcmg_variables.include", read(path).into());
    let path = project_root().join("integration_tests").join("BSIMCMG").join("bsimimg.va");
    vfs.add_virt_file("/bsimimg.va", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimimg_binning.include");
    vfs.add_virt_file("/bsimimg_binning.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimimg_body.include");
    vfs.add_virt_file("/bsimimg_body.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMCMG").join("bsimimg_sp_new.include");
    vfs.add_virt_file("/bsimimg_sp_new.include", read(path).into());
    let path = project_root().join("integration_tests").join("BSIMCMG").join("bsimsoi.va");
    vfs.add_virt_file("/bsimsoi.va", read(path).into());
    let path = project_root().join("integration_tests").join("BSIMCMG").join("common_defs.include");
    vfs.add_virt_file("/common_defs.include", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIMCMG")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsimimg() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimimg.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIMIMG").join("bsimimg.va");
    vfs.add_virt_file("/bsimimg.va", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMIMG").join("bsimimg_binning.include");
    vfs.add_virt_file("/bsimimg_binning.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMIMG").join("bsimimg_body.include");
    vfs.add_virt_file("/bsimimg_body.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("BSIMIMG").join("bsimimg_sp_new.include");
    vfs.add_virt_file("/bsimimg_sp_new.include", read(path).into());
    let path = project_root().join("integration_tests").join("BSIMIMG").join("common_defs.include");
    vfs.add_virt_file("/common_defs.include", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIMIMG")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn bsimsoi() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/bsimsoi.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("BSIMSOI").join("bsimsoi.va");
    vfs.add_virt_file("/bsimsoi.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("BSIMSOI")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn diode() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/diode.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("DIODE").join("diode.va");
    vfs.add_virt_file("/diode.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("DIODE")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn diode_cmc() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/diode_cmc.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_InitModel.include");
    vfs.add_virt_file("/DIODE_CMC_InitModel.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_SIMKIT_macrodefs.include");
    vfs.add_virt_file("/DIODE_CMC_SIMKIT_macrodefs.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_macrodefs.include");
    vfs.add_virt_file("/DIODE_CMC_macrodefs.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_parlist.include");
    vfs.add_virt_file("/DIODE_CMC_parlist.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_varlist1.include");
    vfs.add_virt_file("/DIODE_CMC_varlist1.include", read(path).into());
    let path = project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("DIODE_CMC_varlist2.include");
    vfs.add_virt_file("/DIODE_CMC_varlist2.include", read(path).into());
    let path = project_root().join("integration_tests").join("DIODE_CMC").join("diode_cmc.va");
    vfs.add_virt_file("/diode_cmc.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("DIODE_CMC")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn ekv() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/ekv.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("EKV").join("ekv.va");
    vfs.add_virt_file("/ekv.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("EKV")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn hicuml2() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/hicuml2.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("HICUML2").join("hicuml2.va");
    vfs.add_virt_file("/hicuml2.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("HICUML2")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn hisim2() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/hisim2.va", "");
    let mut vfs = db.vfs().write();
    let path =
        project_root().join("integration_tests").join("HiSIM2").join("HSM2_analyticalPs0.inc");
    vfs.add_virt_file("/HSM2_analyticalPs0.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("HSM2_depmos.inc");
    vfs.add_virt_file("/HSM2_depmos.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("HSM2_depmos2.inc");
    vfs.add_virt_file("/HSM2_depmos2.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("HSM2_depmos3.inc");
    vfs.add_virt_file("/HSM2_depmos3.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("HSM2_eval_aging.inc");
    vfs.add_virt_file("/HSM2_eval_aging.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("HSM2_eval_newPT.inc");
    vfs.add_virt_file("/HSM2_eval_newPT.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIM2").join("HSM2_iterativePs0.inc");
    vfs.add_virt_file("/HSM2_iterativePs0.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIM2").join("hisim2.va");
    vfs.add_virt_file("/hisim2.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("HiSIM2")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn hisimhv() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/hisimhv.va", "");
    let mut vfs = db.vfs().write();
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_analyticalPs0.inc");
    vfs.add_virt_file("/HSMHV_analyticalPs0.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_depmos.inc");
    vfs.add_virt_file("/HSMHV_depmos.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_depmos2.inc");
    vfs.add_virt_file("/HSMHV_depmos2.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_depmos3.inc");
    vfs.add_virt_file("/HSMHV_depmos3.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_Overlap.inc");
    vfs.add_virt_file("/HSMHV_eval_Overlap.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_aging.inc");
    vfs.add_virt_file("/HSMHV_eval_aging.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_dio.inc");
    vfs.add_virt_file("/HSMHV_eval_dio.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_loverLim.inc");
    vfs.add_virt_file("/HSMHV_eval_loverLim.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_newPT.inc");
    vfs.add_virt_file("/HSMHV_eval_newPT.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_qover.inc");
    vfs.add_virt_file("/HSMHV_eval_qover.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_rdrift.inc");
    vfs.add_virt_file("/HSMHV_eval_rdrift.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_eval_rdrifts.inc");
    vfs.add_virt_file("/HSMHV_eval_rdrifts.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_iterativePs0.inc");
    vfs.add_virt_file("/HSMHV_iterativePs0.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_macrosAndDefs.inc");
    vfs.add_virt_file("/HSMHV_macrosAndDefs.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_module.inc");
    vfs.add_virt_file("/HSMHV_module.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_temp_eval.inc");
    vfs.add_virt_file("/HSMHV_temp_eval.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_temp_eval_dio.inc");
    vfs.add_virt_file("/HSMHV_temp_eval_dio.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_temp_eval_rdri.inc");
    vfs.add_virt_file("/HSMHV_temp_eval_rdri.inc", read(path).into());
    let path =
        project_root().join("integration_tests").join("HiSIMHV").join("HSMHV_temp_eval_rdris.inc");
    vfs.add_virt_file("/HSMHV_temp_eval_rdris.inc", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("hisimhv.va");
    vfs.add_virt_file("/hisimhv.va", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("hisimhv_n4.va");
    vfs.add_virt_file("/hisimhv_n4.va", read(path).into());
    let path = project_root().join("integration_tests").join("HiSIMHV").join("hisimhv_n5.va");
    vfs.add_virt_file("/hisimhv_n5.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("HiSIMHV")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn hisimsotb() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/hisimsotb.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("HiSIMSOTB").join("hisimsotb.va");
    vfs.add_virt_file("/hisimsotb.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("HiSIMSOTB")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn mvsg_cmc() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/mvsg_cmc.va", "");
    let mut vfs = db.vfs().write();
    let path = project_root().join("integration_tests").join("MVSG_CMC").join("mvsg_cmc.va");
    vfs.add_virt_file("/mvsg_cmc.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("MVSG_CMC")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
#[test]
fn psp() {
    if skip_slow_tests() {
        return;
    }
    let db = TestDataBase::new("/psp.va", "");
    let mut vfs = db.vfs().write();
    let path =
        project_root().join("integration_tests").join("PSP").join("Common102_macrodefs.include");
    vfs.add_virt_file("/Common102_macrodefs.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("JUNCAP200_InitModel.include");
    vfs.add_virt_file("/JUNCAP200_InitModel.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("JUNCAP200_macrodefs.include");
    vfs.add_virt_file("/JUNCAP200_macrodefs.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("JUNCAP200_parlist.include");
    vfs.add_virt_file("/JUNCAP200_parlist.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("JUNCAP200_varlist1.include");
    vfs.add_virt_file("/JUNCAP200_varlist1.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("JUNCAP200_varlist2.include");
    vfs.add_virt_file("/JUNCAP200_varlist2.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("PSP102_ChargesNQS.include");
    vfs.add_virt_file("/PSP102_ChargesNQS.include", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("PSP102_InitNQS.include");
    vfs.add_virt_file("/PSP102_InitNQS.include", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("PSP102_binning.include");
    vfs.add_virt_file("/PSP102_binning.include", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("PSP102_binpars.include");
    vfs.add_virt_file("/PSP102_binpars.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("PSP102_macrodefs.include");
    vfs.add_virt_file("/PSP102_macrodefs.include", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("PSP102_module.include");
    vfs.add_virt_file("/PSP102_module.include", read(path).into());
    let path =
        project_root().join("integration_tests").join("PSP").join("PSP102_nqs_macrodefs.include");
    vfs.add_virt_file("/PSP102_nqs_macrodefs.include", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp.va");
    vfs.add_virt_file("/psp.va", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp102_nqs.va");
    vfs.add_virt_file("/psp102_nqs.va", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp102b.va");
    vfs.add_virt_file("/psp102b.va", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp102b_nqs.va");
    vfs.add_virt_file("/psp102b_nqs.va", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp102e.va");
    vfs.add_virt_file("/psp102e.va", read(path).into());
    let path = project_root().join("integration_tests").join("PSP").join("psp102e_nqs.va");
    vfs.add_virt_file("/psp102e_nqs.va", read(path).into());
    drop(vfs);
    let actual = db.lower_and_check();
    expect_file![project_root()
        .join("integration_tests")
        .join("PSP")
        .join("inference_diagnostics.log")]
    .assert_eq(&actual);
}
